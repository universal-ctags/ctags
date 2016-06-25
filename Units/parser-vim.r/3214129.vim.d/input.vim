" vim600: set foldmethod=marker:
"
" Vim plugin to assist in working with HG-controlled files.
"
" Last Change:   2006/02/22
" Version:       1.76
" Maintainer:    Mathieu Clabaut <mathieu.clabaut@gmail.com>
" License:       This file is placed in the public domain.
" Credits:
"                Bob Hiestand <bob.hiestand@gmail.com> for the fabulous
"                cvscommand.vim from which this script was directly created by
"                means of sed commands and minor tweaks.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Section: Documentation 
"----------------------------
"
" Documentation should be available by ":help hgcommand" command, once the
" script has been copied in you .vim/plugin directory.
"
" You still can read the documentation at the end of this file. Locate it by
" searching the "hgcommand-contents" string (and set ft=help to have
" appropriate syntaxic coloration). 

" Section: Plugin header {{{1

" loaded_hgcommand is set to 1 when the initialization begins, and 2 when it
" completes.  This allows various actions to only be taken by functions after
" system initialization.

if exists("loaded_hgcommand")
   finish
endif
let loaded_hgcommand = 1

if v:version < 602
  echohl WarningMsg|echomsg "HGCommand 1.69 or later requires VIM 6.2 or later"|echohl None
  finish
endif

" Section: Event group setup {{{1

augroup HGCommand
augroup END

" Section: Plugin initialization {{{1
silent do HGCommand User HGPluginInit

" Section: Script variable initialization {{{1

let s:HGCommandEditFileRunning = 0
unlet! s:vimDiffRestoreCmd
unlet! s:vimDiffSourceBuffer
unlet! s:vimDiffBufferCount
unlet! s:vimDiffScratchList

" Section: Utility functions {{{1

" Function: s:HGResolveLink() {{{2
" Fully resolve the given file name to remove shortcuts or symbolic links.

function! s:HGResolveLink(fileName)
  let resolved = resolve(a:fileName)
  if resolved != a:fileName
    let resolved = s:HGResolveLink(resolved)
  endif
  return resolved
endfunction

" Function: s:HGChangeToCurrentFileDir() {{{2
" Go to the directory in which the current HG-controlled file is located.
" If this is a HG command buffer, first switch to the original file.

function! s:HGChangeToCurrentFileDir(fileName)
  let oldCwd=getcwd()
  let fileName=s:HGResolveLink(a:fileName)
  let newCwd=fnamemodify(fileName, ':h')
  if strlen(newCwd) > 0
    execute 'cd' escape(newCwd, ' ')
  endif
  return oldCwd
endfunction

" Function: s:HGGetOption(name, default) {{{2
" Grab a user-specified option to override the default provided.  Options are
" searched in the window, buffer, then global spaces.

function! s:HGGetOption(name, default)
  if exists("s:" . a:name . "Override")
    execute "return s:".a:name."Override"
  elseif exists("w:" . a:name)
    execute "return w:".a:name
  elseif exists("b:" . a:name)
    execute "return b:".a:name
  elseif exists("g:" . a:name)
    execute "return g:".a:name
  else
    return a:default
  endif
endfunction

" Function: s:HGEditFile(name, origBuffNR) {{{2
" Wrapper around the 'edit' command to provide some helpful error text if the
" current buffer can't be abandoned.  If name is provided, it is used;
" otherwise, a nameless scratch buffer is used.
" Returns: 0 if successful, -1 if an error occurs.

function! s:HGEditFile(name, origBuffNR)
  "Name parameter will be pasted into expression.
  let name = escape(a:name, ' *?\')

  let editCommand = s:HGGetOption('HGCommandEdit', 'edit')
  if editCommand != 'edit'
    if s:HGGetOption('HGCommandSplit', 'horizontal') == 'horizontal'
      if name == ""
        let editCommand = 'rightbelow new'
      else
        let editCommand = 'rightbelow split ' . name
      endif
    else
      if name == ""
        let editCommand = 'vert rightbelow new'
      else
        let editCommand = 'vert rightbelow split ' . name
      endif
    endif
  else
    if name == ""
      let editCommand = 'enew'
    else
      let editCommand = 'edit ' . name
    endif
  endif

  " Protect against useless buffer set-up
  let s:HGCommandEditFileRunning = s:HGCommandEditFileRunning + 1
  try
    execute editCommand
  finally
    let s:HGCommandEditFileRunning = s:HGCommandEditFileRunning - 1
  endtry

  let b:HGOrigBuffNR=a:origBuffNR
  let b:HGCommandEdit='split'
endfunction

" Function: s:HGCreateCommandBuffer(cmd, cmdName, statusText, filename) {{{2
" Creates a new scratch buffer and captures the output from execution of the
" given command.  The name of the scratch buffer is returned.

function! s:HGCreateCommandBuffer(cmd, cmdName, statusText, origBuffNR)
  let fileName=bufname(a:origBuffNR)

  let resultBufferName=''

  if s:HGGetOption("HGCommandNameResultBuffers", 0)
    let nameMarker = s:HGGetOption("HGCommandNameMarker", '_')
    if strlen(a:statusText) > 0
      let bufName=a:cmdName . ' -- ' . a:statusText
    else
      let bufName=a:cmdName
    endif
    let bufName=fileName . ' ' . nameMarker . bufName . nameMarker
    let counter=0
    let resultBufferName = bufName
    while buflisted(resultBufferName)
      let counter=counter + 1
      let resultBufferName=bufName . ' (' . counter . ')'
    endwhile
  endif

  let hgCommand = s:HGGetOption("HGCommandHGExec", "hg") . " " . a:cmd
  echomsg "DBG :".hgCommand
  let hgOut = system(hgCommand)
  " HACK:  diff command does not return proper error codes
  if v:shell_error && a:cmdName != 'hgdiff'
    if strlen(hgOut) == 0
      echoerr "HG command failed"
    else
      echoerr "HG command failed:  " . hgOut
    endif
    return -1
  endif
  if strlen(hgOut) == 0
    " Handle case of no output.  In this case, it is important to check the
    " file status, especially since hg edit/unedit may change the attributes
    " of the file with no visible output.

    echomsg "No output from HG command"
    checktime
    return -1
  endif

  if s:HGEditFile(resultBufferName, a:origBuffNR) == -1
    return -1
  endif

  set buftype=nofile
  set noswapfile
  set filetype=

  if s:HGGetOption("HGCommandDeleteOnHide", 0)
    set bufhidden=delete
  endif

  silent 0put=hgOut

  " The last command left a blank line at the end of the buffer.  If the
  " last line is folded (a side effect of the 'put') then the attempt to
  " remove the blank line will kill the last fold.
  "
  " This could be fixed by explicitly detecting whether the last line is
  " within a fold, but I prefer to simply unfold the result buffer altogether.

  if has('folding')
    normal zR
  endif

  $d
  1

  " Define the environment and execute user-defined hooks.

  let b:HGSourceFile=fileName
  let b:HGCommand=a:cmdName
  if a:statusText != ""
    let b:HGStatusText=a:statusText
  endif

  silent do HGCommand User HGBufferCreated
  return bufnr("%")
endfunction

" Function: s:HGBufferCheck(hgBuffer) {{{2
" Attempts to locate the original file to which HG operations were applied
" for a given buffer.

function! s:HGBufferCheck(hgBuffer)
  let origBuffer = getbufvar(a:hgBuffer, "HGOrigBuffNR")
  if origBuffer
    if bufexists(origBuffer)
      return origBuffer
    else
      " Original buffer no longer exists.
      return -1 
    endif
  else
    " No original buffer
    return a:hgBuffer
  endif
endfunction

" Function: s:HGCurrentBufferCheck() {{{2
" Attempts to locate the original file to which HG operations were applied
" for the current buffer.

function! s:HGCurrentBufferCheck()
  return s:HGBufferCheck(bufnr("%"))
endfunction

" Function: s:HGToggleDeleteOnHide() {{{2
" Toggles on and off the delete-on-hide behavior of HG buffers

function! s:HGToggleDeleteOnHide()
  if exists("g:HGCommandDeleteOnHide")
    unlet g:HGCommandDeleteOnHide
  else
    let g:HGCommandDeleteOnHide=1
  endif
endfunction

" Function: s:HGDoCommand(hgcmd, cmdName, statusText) {{{2
" General skeleton for HG function execution.
" Returns: name of the new command buffer containing the command results

function! s:HGDoCommand(cmd, cmdName, statusText)
  let hgBufferCheck=s:HGCurrentBufferCheck()
  if hgBufferCheck == -1 
    echo "Original buffer no longer exists, aborting."
    return -1
  endif

  let fileName=bufname(hgBufferCheck)
  if isdirectory(fileName)
    let fileName=fileName . "/" . getline(".")
  endif
  let realFileName = fnamemodify(s:HGResolveLink(fileName), ':t')
  let oldCwd=s:HGChangeToCurrentFileDir(fileName)
  try
     " TODO
    "if !filereadable('HG/Root')
      "throw fileName . ' is not a HG-controlled file.'
    "endif
    let fullCmd = a:cmd . ' "' . realFileName . '"'
    "echomsg "DEBUG".fullCmd
    let resultBuffer=s:HGCreateCommandBuffer(fullCmd, a:cmdName, a:statusText, hgBufferCheck)
    return resultBuffer
  catch
    echoerr v:exception
    return -1
  finally
    execute 'cd' escape(oldCwd, ' ')
  endtry
endfunction


" Function: s:HGGetStatusVars(revision, branch, repository) {{{2
"
" Obtains a HG revision number and branch name.  The 'revisionVar',
" 'branchVar'and 'repositoryVar' arguments, if non-empty, contain the names of variables to hold
" the corresponding results.
"
" Returns: string to be exec'd that sets the multiple return values.

function! s:HGGetStatusVars(revisionVar, branchVar, repositoryVar)
  let hgBufferCheck=s:HGCurrentBufferCheck()
  if hgBufferCheck == -1 
    return ""
  endif
  let fileName=bufname(hgBufferCheck)
  let realFileName = fnamemodify(s:HGResolveLink(fileName), ':t')
  let oldCwd=s:HGChangeToCurrentFileDir(fileName)
  try
     ""TODO
    "if !filereadable('HG/Root')
      "return ""
    "endif
    let hgCommand = s:HGGetOption("HGCommandHGExec", "hg") . " status -mardui " . fileName
    let statustext=system(hgCommand)
    if(v:shell_error)
      return ""
    endif
    if match(statustext, '^[?I]') >= 0 
      let revision="NEW"
    elseif match(statustext, '^[R]') >= 0 
      let revision="REMOVED"
    elseif match(statustext, '^[D]') >= 0 
      let revision="DELETED"
    elseif match(statustext, '^[A]') >= 0 
      let revision="ADDED"
    endif

    let hgCommand = s:HGGetOption("HGCommandHGExec", "hg") . " parents -b  " 
    let statustext=system(hgCommand)
    if(v:shell_error)
        return ""
    endif
    if exists('revision')
      let returnExpression = "let " . a:revisionVar . "='" . revision . "'"
    else
      let revision=substitute(statustext, '^changeset:\s*\(\d\+\):.*\_$\_.*$', '\1', "")
      let returnExpression = "let " . a:revisionVar . "='" . revision . "'"
    endif

    if a:branchVar != "" && match(statustext, '^\_.*\_^branch:') >= 0
      let branch=substitute(statustext, '^\_.*\_^branch:\s*\(\S\+\)\n\_.*$', '\1', "")
      let returnExpression=returnExpression . " | let " . a:branchVar . "='" . branch . "'"
    endif
    if a:repositoryVar != ""
      let hgCommand = s:HGGetOption("HGCommandHGExec", "hg") . " root  " 
      let roottext=system(hgCommand)
      let repository=substitute(roottext,'^.*/\([^/\n\r]*\)\n\_.*$','\1','')
      let returnExpression=returnExpression . " | let " . a:repositoryVar . "='" . repository . "'"
    endif



    return returnExpression
  finally
    execute 'cd' escape(oldCwd, ' ')
  endtry
endfunction

" Function: s:HGSetupBuffer() {{{2
" Attempts to set the b:HGBranch, b:HGRevision and b:HGRepository variables.

function! s:HGSetupBuffer()
  if (exists("b:HGBufferSetup") && b:HGBufferSetup)
    " This buffer is already set up.
    return
  endif

  if !s:HGGetOption("HGCommandEnableBufferSetup", 0)
        \ || @% == ""
        \ || s:HGCommandEditFileRunning > 0
        \ || exists("b:HGOrigBuffNR")
    unlet! b:HGRevision
    unlet! b:HGBranch
    unlet! b:HGRepository
    return
  endif

  if !filereadable(expand("%"))
    return -1
  endif

  let revision=""
  let branch=""
  let repository=""

  exec s:HGGetStatusVars('revision', 'branch', 'repository')
  "echomsg "DBG ".revision."#".branch."#".repository
  if revision != ""
    let b:HGRevision=revision
  else
    unlet! b:HGRevision
  endif
  if branch != ""
    let b:HGBranch=branch
  else
    unlet! b:HGBranch
  endif
  if repository != ""
     let b:HGRepository=repository
  else
     unlet! b:HGRepository
  endif
  silent do HGCommand User HGBufferSetup
  let b:HGBufferSetup=1
endfunction

" Function: s:HGMarkOrigBufferForSetup(hgbuffer) {{{2
" Resets the buffer setup state of the original buffer for a given HG buffer.
" Returns:  The HG buffer number in a passthrough mode.

function! s:HGMarkOrigBufferForSetup(hgBuffer)
  checktime
  if a:hgBuffer != -1
    let origBuffer = s:HGBufferCheck(a:hgBuffer)
    "This should never not work, but I'm paranoid
    if origBuffer != a:hgBuffer
      call setbufvar(origBuffer, "HGBufferSetup", 0)
    endif
  endif
  return a:hgBuffer
endfunction

" Function: s:HGOverrideOption(option, [value]) {{{2
" Provides a temporary override for the given HG option.  If no value is
" passed, the override is disabled.

function! s:HGOverrideOption(option, ...)
  if a:0 == 0
    unlet! s:{a:option}Override
  else
    let s:{a:option}Override = a:1
  endif
endfunction

" Function: s:HGWipeoutCommandBuffers() {{{2
" Clears all current HG buffers of the specified type for a given source.

function! s:HGWipeoutCommandBuffers(originalBuffer, hgCommand)
  let buffer = 1
  while buffer <= bufnr('$')
    if getbufvar(buffer, 'HGOrigBuffNR') == a:originalBuffer
      if getbufvar(buffer, 'HGCommand') == a:hgCommand
        execute 'bw' buffer
      endif
    endif
    let buffer = buffer + 1
  endwhile
endfunction

" Function: s:HGInstallDocumentation(full_name, revision)              {{{2
"   Install help documentation.
" Arguments:
"   full_name: Full name of this vim plugin script, including path name.
"   revision:  Revision of the vim script. #version# mark in the document file
"              will be replaced with this string with 'v' prefix.
" Return:
"   1 if new document installed, 0 otherwise.
" Note: Cleaned and generalized by guo-peng Wen
"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function! s:HGInstallDocumentation(full_name, revision)
    " Name of the document path based on the system we use:
    if (has("unix"))
        " On UNIX like system, using forward slash:
        let l:slash_char = '/'
        let l:mkdir_cmd  = ':silent !mkdir -p '
    else
        " On M$ system, use backslash. Also mkdir syntax is different.
        " This should only work on W2K and up.
        let l:slash_char = '\'
        let l:mkdir_cmd  = ':silent !mkdir '
    endif

    let l:doc_path = l:slash_char . 'doc'
    let l:doc_home = l:slash_char . '.vim' . l:slash_char . 'doc'

    " Figure out document path based on full name of this script:
    let l:vim_plugin_path = fnamemodify(a:full_name, ':h')
    let l:vim_doc_path    = fnamemodify(a:full_name, ':h:h') . l:doc_path
    if (!(filewritable(l:vim_doc_path) == 2))
        echomsg "Doc path: " . l:vim_doc_path
        execute l:mkdir_cmd . '"' . l:vim_doc_path . '"'
        if (!(filewritable(l:vim_doc_path) == 2))
            " Try a default configuration in user home:
            let l:vim_doc_path = expand("~") . l:doc_home
            if (!(filewritable(l:vim_doc_path) == 2))
                execute l:mkdir_cmd . '"' . l:vim_doc_path . '"'
                if (!(filewritable(l:vim_doc_path) == 2))
                    " Put a warning:
                    echomsg "Unable to open documentation directory"
                    echomsg " type :help add-local-help for more informations."
                    return 0
                endif
            endif
        endif
    endif

    " Exit if we have problem to access the document directory:
    if (!isdirectory(l:vim_plugin_path)
        \ || !isdirectory(l:vim_doc_path)
        \ || filewritable(l:vim_doc_path) != 2)
        return 0
    endif

    " Full name of script and documentation file:
    let l:script_name = fnamemodify(a:full_name, ':t')
    let l:doc_name    = fnamemodify(a:full_name, ':t:r') . '.txt'
    let l:plugin_file = l:vim_plugin_path . l:slash_char . l:script_name
    let l:doc_file    = l:vim_doc_path    . l:slash_char . l:doc_name

    " Bail out if document file is still up to date:
    if (filereadable(l:doc_file)  &&
        \ getftime(l:plugin_file) < getftime(l:doc_file))
        return 0
    endif

    " Prepare window position restoring command:
    if (strlen(@%))
        let l:go_back = 'b ' . bufnr("%")
    else
        let l:go_back = 'enew!'
    endif

    " Create a new buffer & read in the plugin file (me):
    setl nomodeline
    exe 'enew!'
    exe 'r ' . l:plugin_file

    setl modeline
    let l:buf = bufnr("%")
    setl noswapfile modifiable

    norm zR
    norm gg

    " Delete from first line to a line starts with
    " === START_DOC
    1,/^=\{3,}\s\+START_DOC\C/ d

    " Delete from a line starts with
    " === END_DOC
    " to the end of the documents:
    /^=\{3,}\s\+END_DOC\C/,$ d

    " Remove fold marks:
    %s/{\{3}[1-9]/    /

    " Add modeline for help doc: the modeline string is mangled intentionally
    " to avoid it be recognized by VIM:
    call append(line('$'), '')
    call append(line('$'), ' v' . 'im:tw=78:ts=8:ft=help:norl:')

    " Replace revision:
    exe "normal :1s/#version#/ v" . a:revision . "/\<CR>"

    " Save the help document:
    exe 'w! ' . l:doc_file
    exe l:go_back
    exe 'bw ' . l:buf

    " Build help tags:
    exe 'helptags ' . l:vim_doc_path

    return 1
endfunction

" Section: Public functions {{{1

" Function: HGGetRevision() {{{2
" Global function for retrieving the current buffer's HG revision number.
" Returns: Revision number or an empty string if an error occurs.

function! HGGetRevision()
  let revision=""
  exec s:HGGetStatusVars('revision', '', '')
  return revision
endfunction

" Function: HGDisableBufferSetup() {{{2
" Global function for deactivating the buffer autovariables.

function! HGDisableBufferSetup()
  let g:HGCommandEnableBufferSetup=0
  silent! augroup! HGCommandPlugin
endfunction

" Function: HGEnableBufferSetup() {{{2
" Global function for activating the buffer autovariables.

function! HGEnableBufferSetup()
  let g:HGCommandEnableBufferSetup=1
  augroup HGCommandPlugin
    au!
    au BufEnter * call s:HGSetupBuffer()
  augroup END

  " Only auto-load if the plugin is fully loaded.  This gives other plugins a
  " chance to run.
  if g:loaded_hgcommand == 2
    call s:HGSetupBuffer()
  endif
endfunction

" Function: HGGetStatusLine() {{{2
" Default (sample) status line entry for HG files.  This is only useful if
" HG-managed buffer mode is on (see the HGCommandEnableBufferSetup variable
" for how to do this).

function! HGGetStatusLine()
  if exists('b:HGSourceFile')
    " This is a result buffer
    let value='[' . b:HGCommand . ' ' . b:HGSourceFile
    if exists('b:HGStatusText')
      let value=value . ' ' . b:HGStatusText
    endif
    let value = value . ']'
    return value
  endif

  if exists('b:HGRevision')
        \ && b:HGRevision != ''
        \ && exists('b:HGBranch')
        \ && b:HGBranch != ''
        \ && exists('b:HGRepository')
        \ && b:HGRepository != ''
        \ && exists('g:HGCommandEnableBufferSetup')
        \ && g:HGCommandEnableBufferSetup
   return '[HG ' . b:HGRepository . '/' . b:HGBranch .'/' . b:HGRevision . ']'
  else
    return ''
  endif
endfunction

" Section: HG command functions {{{1

" Function: s:HGAdd() {{{2
function! s:HGAdd()
  return s:HGMarkOrigBufferForSetup(s:HGDoCommand('add', 'hgadd', ''))
endfunction

" Function: s:HGAnnotate(...) {{{2
function! s:HGAnnotate(...)
  if a:0 == 0
    if &filetype == "HGAnnotate"
      " This is a HGAnnotate buffer.  Perform annotation of the version
      " indicated by the current line.
      let revision = substitute(getline("."),'\(^[0-9]*\):.*','\1','')
      if s:HGGetOption('HGCommandAnnotateParent', 0) != 0 && revision > 0
        let revision = revision - 1
      endif
    else
      let revision=HGGetRevision()
      if revision == ""
        echoerr "Unable to obtain HG version information."
        return -1
      endif
    endif
  else
    let revision=a:1
  endif

  if revision == "NEW"
    echo "No annotatation available for new file."
    return -1
  endif

  let resultBuffer=s:HGDoCommand('annotate -ndu -r ' . revision, 'hgannotate', revision) 
  echomsg "DBG: ".resultBuffer
  if resultBuffer !=  -1
    set filetype=HGAnnotate
  endif

  return resultBuffer
endfunction

" Function: s:HGCommit() {{{2
function! s:HGCommit(...)
  " Handle the commit message being specified.  If a message is supplied, it
  " is used; if bang is supplied, an empty message is used; otherwise, the
  " user is provided a buffer from which to edit the commit message.
  if a:2 != "" || a:1 == "!"
    return s:HGMarkOrigBufferForSetup(s:HGDoCommand('commit -m "' . a:2 . '"', 'hgcommit', ''))
  endif

  let hgBufferCheck=s:HGCurrentBufferCheck()
  if hgBufferCheck ==  -1
    echo "Original buffer no longer exists, aborting."
    return -1
  endif

  " Protect against windows' backslashes in paths.  They confuse exec'd
  " commands.

  let shellSlashBak = &shellslash
  try
    set shellslash

    let messageFileName = tempname()

    let fileName=bufname(hgBufferCheck)
    let realFilePath=s:HGResolveLink(fileName)
    let newCwd=fnamemodify(realFilePath, ':h')
    if strlen(newCwd) == 0
      " Account for autochdir being in effect, which will make this blank, but
      " we know we'll be in the current directory for the original file.
      let newCwd = getcwd()
    endif

    let realFileName=fnamemodify(realFilePath, ':t')

    if s:HGEditFile(messageFileName, hgBufferCheck) == -1
      return
    endif

    " Protect against case and backslash issues in Windows.
    let autoPattern = '\c' . messageFileName

    " Ensure existence of group
    augroup HGCommit
    augroup END

    execute 'au HGCommit BufDelete' autoPattern 'call delete("' . messageFileName . '")'
    execute 'au HGCommit BufDelete' autoPattern 'au! HGCommit * ' autoPattern

    " Create a commit mapping.  The mapping must clear all autocommands in case
    " it is invoked when HGCommandCommitOnWrite is active, as well as to not
    " invoke the buffer deletion autocommand.

    execute 'nnoremap <silent> <buffer> <Plug>HGCommit '.
          \ ':au! HGCommit * ' . autoPattern . '<CR>'.
          \ ':g/^HG:/d<CR>'.
          \ ':update<CR>'.
          \ ':call <SID>HGFinishCommit("' . messageFileName . '",' .
          \                             '"' . newCwd . '",' .
          \                             '"' . realFileName . '",' .
          \                             hgBufferCheck . ')<CR>'

    silent 0put ='HG: ----------------------------------------------------------------------'
    silent put =\"HG: Enter Log.  Lines beginning with `HG:' are removed automatically\"
    silent put ='HG: Type <leader>cc (or your own <Plug>HGCommit mapping)'

    if s:HGGetOption('HGCommandCommitOnWrite', 1) == 1
      execute 'au HGCommit BufWritePre' autoPattern 'g/^HG:/d'
      execute 'au HGCommit BufWritePost' autoPattern 'call s:HGFinishCommit("' . messageFileName . '", "' . newCwd . '", "' . realFileName . '", ' . hgBufferCheck . ') | au! * ' autoPattern
      silent put ='HG: or write this buffer'
    endif

    silent put ='HG: to finish this commit operation'
    silent put ='HG: ----------------------------------------------------------------------'
    $
    let b:HGSourceFile=fileName
    let b:HGCommand='HGCommit'
    set filetype=hg
  finally
    let &shellslash = shellSlashBak
  endtry

endfunction

" Function: s:HGDiff(...) {{{2
function! s:HGDiff(...)
  if a:0 == 1
    let revOptions = '-r' . a:1
    let caption = a:1 . ' -> current'
  elseif a:0 == 2
    let revOptions = '-r' . a:1 . ' -r' . a:2
    let caption = a:1 . ' -> ' . a:2
  else
    let revOptions = ''
    let caption = ''
  endif

  let hgdiffopt=s:HGGetOption('HGCommandDiffOpt', 'w')

  if hgdiffopt == ""
    let diffoptionstring=""
  else
    let diffoptionstring=" -" . hgdiffopt . " "
  endif

  let resultBuffer = s:HGDoCommand('diff ' . diffoptionstring . revOptions , 'hgdiff', caption)
  if resultBuffer != -1 
    set filetype=diff
  endif
  return resultBuffer
endfunction


" Function: s:HGGotoOriginal(["!]) {{{2
function! s:HGGotoOriginal(...)
  let origBuffNR = s:HGCurrentBufferCheck()
  if origBuffNR > 0
    let origWinNR = bufwinnr(origBuffNR)
    if origWinNR == -1
      execute 'buffer' origBuffNR
    else
      execute origWinNR . 'wincmd w'
    endif
    if a:0 == 1
      if a:1 == "!"
        let buffnr = 1
        let buffmaxnr = bufnr("$")
        while buffnr <= buffmaxnr
          if getbufvar(buffnr, "HGOrigBuffNR") == origBuffNR
            execute "bw" buffnr
          endif
          let buffnr = buffnr + 1
        endwhile
      endif
    endif
  endif
endfunction

" Function: s:HGFinishCommit(messageFile, targetDir, targetFile) {{{2
function! s:HGFinishCommit(messageFile, targetDir, targetFile, origBuffNR)
  if filereadable(a:messageFile)
    let oldCwd=getcwd()
    if strlen(a:targetDir) > 0
      execute 'cd' escape(a:targetDir, ' ')
    endif
    let resultBuffer=s:HGCreateCommandBuffer('commit -F "' . a:messageFile . '" "'. a:targetFile . '"', 'hgcommit', '', a:origBuffNR)
    execute 'cd' escape(oldCwd, ' ')
    execute 'bw' escape(a:messageFile, ' *?\')
    silent execute 'call delete("' . a:messageFile . '")'
    return s:HGMarkOrigBufferForSetup(resultBuffer)
  else
    echoerr "Can't read message file; no commit is possible."
    return -1
  endif
endfunction

" Function: s:HGLog() {{{2
function! s:HGLog(...)
  if a:0 == 0
    let versionOption = ""
    let caption = ''
  else
    let versionOption=" -r" . a:1
    let caption = a:1
  endif

  let resultBuffer=s:HGDoCommand('log' . versionOption, 'hglog', caption)
  if resultBuffer != ""
    set filetype=rcslog
  endif
  return resultBuffer
endfunction

" Function: s:HGRevert() {{{2
function! s:HGRevert()
  return s:HGMarkOrigBufferForSetup(s:HGDoCommand('revert', 'hgrevert', ''))
endfunction

" Function: s:HGReview(...) {{{2
function! s:HGReview(...)
  if a:0 == 0
    let versiontag=""
    if s:HGGetOption('HGCommandInteractive', 0)
      let versiontag=input('Revision:  ')
    endif
    if versiontag == ""
      let versiontag="(current)"
      let versionOption=""
    else
      let versionOption=" -r " . versiontag . " "
    endif
  else
    let versiontag=a:1
    let versionOption=" -r " . versiontag . " "
  endif

  let resultBuffer = s:HGDoCommand('cat' . versionOption, 'hgreview', versiontag)
  if resultBuffer > 0
    let &filetype=getbufvar(b:HGOrigBuffNR, '&filetype')
  endif

  return resultBuffer
endfunction

" Function: s:HGStatus() {{{2
function! s:HGStatus()
  return s:HGDoCommand('status', 'hgstatus', '')
endfunction


" Function: s:HGUpdate() {{{2
function! s:HGUpdate()
  return s:HGMarkOrigBufferForSetup(s:HGDoCommand('update', 'update', ''))
endfunction

" Function: s:HGVimDiff(...) {{{2
function! s:HGVimDiff(...)
  let originalBuffer = s:HGCurrentBufferCheck()
  let s:HGCommandEditFileRunning = s:HGCommandEditFileRunning + 1
  try
    " If there's already a VimDiff'ed window, restore it.
    " There may only be one HGVimDiff original window at a time.

    if exists("s:vimDiffSourceBuffer") && s:vimDiffSourceBuffer != originalBuffer
      " Clear the existing vimdiff setup by removing the result buffers.
      call s:HGWipeoutCommandBuffers(s:vimDiffSourceBuffer, 'vimdiff')
    endif

    " Split and diff
    if(a:0 == 2)
      " Reset the vimdiff system, as 2 explicit versions were provided.
      if exists('s:vimDiffSourceBuffer')
        call s:HGWipeoutCommandBuffers(s:vimDiffSourceBuffer, 'vimdiff')
      endif
      let resultBuffer = s:HGReview(a:1)
      if resultBuffer < 0
        echomsg "Can't open HG revision " . a:1
        return resultBuffer
      endif
      let b:HGCommand = 'vimdiff'
      diffthis
      let s:vimDiffBufferCount = 1
      let s:vimDiffScratchList = '{'. resultBuffer . '}'
      " If no split method is defined, cheat, and set it to vertical.
      try
        call s:HGOverrideOption('HGCommandSplit', s:HGGetOption('HGCommandDiffSplit', s:HGGetOption('HGCommandSplit', 'vertical')))
        let resultBuffer=s:HGReview(a:2)
      finally
        call s:HGOverrideOption('HGCommandSplit')
      endtry
      if resultBuffer < 0
        echomsg "Can't open HG revision " . a:1
        return resultBuffer
      endif
      let b:HGCommand = 'vimdiff'
      diffthis
      let s:vimDiffBufferCount = 2
      let s:vimDiffScratchList = s:vimDiffScratchList . '{'. resultBuffer . '}'
    else
      " Add new buffer
      try
        " Force splitting behavior, otherwise why use vimdiff?
        call s:HGOverrideOption("HGCommandEdit", "split")
        call s:HGOverrideOption("HGCommandSplit", s:HGGetOption('HGCommandDiffSplit', s:HGGetOption('HGCommandSplit', 'vertical')))
        if(a:0 == 0)
          let resultBuffer=s:HGReview()
        else
          let resultBuffer=s:HGReview(a:1)
        endif
      finally
        call s:HGOverrideOption("HGCommandEdit")
        call s:HGOverrideOption("HGCommandSplit")
      endtry
      if resultBuffer < 0
        echomsg "Can't open current HG revision"
        return resultBuffer
      endif
      let b:HGCommand = 'vimdiff'
      diffthis

      if !exists('s:vimDiffBufferCount')
        " New instance of vimdiff.
        let s:vimDiffBufferCount = 2
        let s:vimDiffScratchList = '{' . resultBuffer . '}'

        " This could have been invoked on a HG result buffer, not the
        " original buffer.
        wincmd W
        execute 'buffer' originalBuffer
        " Store info for later original buffer restore
        let s:vimDiffRestoreCmd = 
              \    "call setbufvar(".originalBuffer.", \"&diff\", ".getbufvar(originalBuffer, '&diff').")"
              \ . "|call setbufvar(".originalBuffer.", \"&foldcolumn\", ".getbufvar(originalBuffer, '&foldcolumn').")"
              \ . "|call setbufvar(".originalBuffer.", \"&foldenable\", ".getbufvar(originalBuffer, '&foldenable').")"
              \ . "|call setbufvar(".originalBuffer.", \"&foldmethod\", '".getbufvar(originalBuffer, '&foldmethod')."')"
              \ . "|call setbufvar(".originalBuffer.", \"&scrollbind\", ".getbufvar(originalBuffer, '&scrollbind').")"
              \ . "|call setbufvar(".originalBuffer.", \"&wrap\", ".getbufvar(originalBuffer, '&wrap').")"
              \ . "|if &foldmethod=='manual'|execute 'normal zE'|endif"
        diffthis
        wincmd w
      else
        " Adding a window to an existing vimdiff
        let s:vimDiffBufferCount = s:vimDiffBufferCount + 1
        let s:vimDiffScratchList = s:vimDiffScratchList . '{' . resultBuffer . '}'
      endif
    endif

    let s:vimDiffSourceBuffer = originalBuffer

    " Avoid executing the modeline in the current buffer after the autocommand.

    let currentBuffer = bufnr('%')
    let saveModeline = getbufvar(currentBuffer, '&modeline')
    try
      call setbufvar(currentBuffer, '&modeline', 0)
      silent do HGCommand User HGVimDiffFinish
    finally
      call setbufvar(currentBuffer, '&modeline', saveModeline)
    endtry
    return resultBuffer
  finally
    let s:HGCommandEditFileRunning = s:HGCommandEditFileRunning - 1
  endtry
endfunction

" Section: Command definitions {{{1
" Section: Primary commands {{{2
com! HGAdd call s:HGAdd()
com! -nargs=? HGAnnotate call s:HGAnnotate(<f-args>)
com! -bang -nargs=? HGCommit call s:HGCommit(<q-bang>, <q-args>)
com! -nargs=* HGDiff call s:HGDiff(<f-args>)
com! -bang HGGotoOriginal call s:HGGotoOriginal(<q-bang>)
com! -nargs=? HGLog call s:HGLog(<f-args>)
com! HGRevert call s:HGRevert()
com! -nargs=? HGReview call s:HGReview(<f-args>)
com! HGStatus call s:HGStatus()
com! HGUpdate call s:HGUpdate()
com! -nargs=* HGVimDiff call s:HGVimDiff(<f-args>)

" Section: HG buffer management commands {{{2
com! HGDisableBufferSetup call HGDisableBufferSetup()
com! HGEnableBufferSetup call HGEnableBufferSetup()

" Allow reloading hgcommand.vim
com! HGReload unlet! loaded_hgcommand | runtime plugin/hgcommand.vim

" Section: Plugin command mappings {{{1
nnoremap <silent> <Plug>HGAdd :HGAdd<CR>
nnoremap <silent> <Plug>HGAnnotate :HGAnnotate<CR>
nnoremap <silent> <Plug>HGCommit :HGCommit<CR>
nnoremap <silent> <Plug>HGDiff :HGDiff<CR>
nnoremap <silent> <Plug>HGGotoOriginal :HGGotoOriginal<CR>
nnoremap <silent> <Plug>HGClearAndGotoOriginal :HGGotoOriginal!<CR>
nnoremap <silent> <Plug>HGLog :HGLog<CR>
nnoremap <silent> <Plug>HGRevert :HGRevert<CR>
nnoremap <silent> <Plug>HGReview :HGReview<CR>
nnoremap <silent> <Plug>HGStatus :HGStatus<CR>
nnoremap <silent> <Plug>HGUpdate :HGUpdate<CR>
nnoremap <silent> <Plug>HGVimDiff :HGVimDiff<CR>
nnoremap <silent> <Plug>HGWatchers :HGWatchers<CR>
nnoremap <silent> <Plug>HGWatchAdd :HGWatchAdd<CR>
nnoremap <silent> <Plug>HGWatchOn :HGWatchOn<CR>
nnoremap <silent> <Plug>HGWatchOff :HGWatchOff<CR>
nnoremap <silent> <Plug>HGWatchRemove :HGWatchRemove<CR>

" Section: Default mappings {{{1
if !hasmapto('<Plug>HGAdd')
  nmap <unique> <Leader>hga <Plug>HGAdd
endif
if !hasmapto('<Plug>HGAnnotate')
  nmap <unique> <Leader>hgn <Plug>HGAnnotate
endif
if !hasmapto('<Plug>HGClearAndGotoOriginal')
  nmap <unique> <Leader>hgG <Plug>HGClearAndGotoOriginal
endif
if !hasmapto('<Plug>HGCommit')
  nmap <unique> <Leader>hgc <Plug>HGCommit
endif
if !hasmapto('<Plug>HGDiff')
  nmap <unique> <Leader>hgd <Plug>HGDiff
endif
if !hasmapto('<Plug>HGGotoOriginal')
  nmap <unique> <Leader>hgg <Plug>HGGotoOriginal
endif
if !hasmapto('<Plug>HGLog')
  nmap <unique> <Leader>hgl <Plug>HGLog
endif
if !hasmapto('<Plug>HGRevert')
  nmap <unique> <Leader>hgq <Plug>HGRevert
endif
if !hasmapto('<Plug>HGReview')
  nmap <unique> <Leader>hgr <Plug>HGReview
endif
if !hasmapto('<Plug>HGStatus')
  nmap <unique> <Leader>hgs <Plug>HGStatus
endif
if !hasmapto('<Plug>HGUpdate')
  nmap <unique> <Leader>hgu <Plug>HGUpdate
endif
if !hasmapto('<Plug>HGVimDiff')
  nmap <unique> <Leader>hgv <Plug>HGVimDiff
endif

" Section: Menu items {{{1
silent! aunmenu Plugin.HG
amenu <silent> &Plugin.HG.&Add        <Plug>HGAdd
amenu <silent> &Plugin.HG.A&nnotate   <Plug>HGAnnotate
amenu <silent> &Plugin.HG.&Commit     <Plug>HGCommit
amenu <silent> &Plugin.HG.&Diff       <Plug>HGDiff
amenu <silent> &Plugin.HG.&Log        <Plug>HGLog
amenu <silent> &Plugin.HG.Revert      <Plug>HGRevert
amenu <silent> &Plugin.HG.&Review     <Plug>HGReview
amenu <silent> &Plugin.HG.&Status     <Plug>HGStatus
amenu <silent> &Plugin.HG.&Update     <Plug>HGUpdate
amenu <silent> &Plugin.HG.&VimDiff    <Plug>HGVimDiff
amenu <silent> &Plugin.HG.&Watchers   <Plug>HGWatchers
amenu <silent> &Plugin.HG.WatchAdd    <Plug>HGWatchAdd
amenu <silent> &Plugin.HG.WatchOn     <Plug>HGWatchOn
amenu <silent> &Plugin.HG.WatchOff    <Plug>HGWatchOff
amenu <silent> &Plugin.HG.WatchRemove <Plug>HGWatchRemove

" Section: Autocommands to restore vimdiff state {{{1
function! s:HGVimDiffRestore(vimDiffBuff)
  let s:HGCommandEditFileRunning = s:HGCommandEditFileRunning + 1
  try
    if exists("s:vimDiffSourceBuffer")
      if a:vimDiffBuff == s:vimDiffSourceBuffer
        " Original file is being removed.
        unlet! s:vimDiffSourceBuffer
        unlet! s:vimDiffBufferCount
        unlet! s:vimDiffRestoreCmd
        unlet! s:vimDiffScratchList
      elseif match(s:vimDiffScratchList, '{' . a:vimDiffBuff . '}') >= 0
        let s:vimDiffScratchList = substitute(s:vimDiffScratchList, '{' . a:vimDiffBuff . '}', '', '')
        let s:vimDiffBufferCount = s:vimDiffBufferCount - 1
        if s:vimDiffBufferCount == 1 && exists('s:vimDiffRestoreCmd')
          " All scratch buffers are gone, reset the original.
          " Only restore if the source buffer is still in Diff mode

          let sourceWinNR=bufwinnr(s:vimDiffSourceBuffer)
          if sourceWinNR != -1
            " The buffer is visible in at least one window
            let currentWinNR = winnr()
            while winbufnr(sourceWinNR) != -1
              if winbufnr(sourceWinNR) == s:vimDiffSourceBuffer
                execute sourceWinNR . 'wincmd w'
                if getwinvar('', "&diff")
                  execute s:vimDiffRestoreCmd
                endif
              endif
              let sourceWinNR = sourceWinNR + 1
            endwhile
            execute currentWinNR . 'wincmd w'
          else
            " The buffer is hidden.  It must be visible in order to set the
            " diff option.
            let currentBufNR = bufnr('')
            execute "hide buffer" s:vimDiffSourceBuffer
            if getwinvar('', "&diff")
              execute s:vimDiffRestoreCmd
            endif
            execute "hide buffer" currentBufNR
          endif

          unlet s:vimDiffRestoreCmd
          unlet s:vimDiffSourceBuffer
          unlet s:vimDiffBufferCount
          unlet s:vimDiffScratchList
        elseif s:vimDiffBufferCount == 0
          " All buffers are gone.
          unlet s:vimDiffSourceBuffer
          unlet s:vimDiffBufferCount
          unlet s:vimDiffScratchList
        endif
      endif
    endif
  finally
    let s:HGCommandEditFileRunning = s:HGCommandEditFileRunning - 1
  endtry
endfunction

augroup HGVimDiffRestore
  au!
  au BufUnload * call s:HGVimDiffRestore(expand("<abuf>"))
augroup END

" Section: Optional activation of buffer management {{{1

if s:HGGetOption('HGCommandEnableBufferSetup', 0)
  call HGEnableBufferSetup()
endif

" Section: Doc installation {{{1
"
  let s:revision="0.1"
  silent! let s:install_status =
      \ s:HGInstallDocumentation(expand('<sfile>:p'), s:revision)
  if (s:install_status == 1)
      echom expand("<sfile>:t:r") . ' v' . s:revision .
		\ ': Help-documentation installed.'
  endif


" Section: Plugin completion {{{1

let loaded_hgcommand=2
silent do HGCommand User HGPluginFinish
" vim:se expandtab sts=2 sw=2:
finish

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Documentation content                                          {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
=== START_DOC
*hgcommand.txt*	  Mercurial vim integration		             #version#


			 HGCOMMAND REFERENCE MANUAL~


Author:  Mathieu Clabaut <mathieu.clabaut@gmail.com>
Credits:  Bob Hiestand <bob.hiestand@gmail.com>
Mercurial: http://www.selenic.com/mercurial
   Mercurial (noted Hg) is a fast, lightweight Source Control Management
   system designed for efficient handling of very large distributed projects.

==============================================================================
1. Contents						  *hgcommand-contents*

	Installation		: |hgcommand-install|
        HGCommand Intro	        : |hgcommand|
	HGCommand Manual	: |hgcommand-manual|
	Customization		: |hgcommand-customize|
	Bugs			: |hgcommand-bugs|

==============================================================================
2. HGCommand Installation				   *hgcommand-install*

   In order to install the plugin, place the hgcommand.vim file into a plugin' 
   directory in your runtime path (please see |add-global-plugin| and 
   |'runtimepath'|.

   HGCommand may be customized by setting variables, creating maps, and 
   specifying event handlers.  Please see |hgcommand-customize| for more
   details.

                                                         *hgcommand-auto-help*
   The help file is automagically generated when the |hgcommand| script is 
   loaded for the first time.

==============================================================================

3. HGCommand Intro					           *hgcommand*
                                                             *hgcommand-intro*

   The HGCommand plugin provides global ex commands for manipulating 
   HG-controlled source files.  In general, each command operates on the 
   current buffer and accomplishes a separate hg function, such as update, 
   commit, log, and others (please see |hgcommand-commands| for a list of all
   available commands).  The results of each operation are displayed in a 
   scratch buffer.  Several buffer variables are defined for those scratch 
   buffers (please see |hgcommand-buffer-variables|).

   The notion of "current file" means either the current buffer, or, in the 
   case of a directory buffer, the file on the current line within the buffer.

   For convenience, any HGCommand invoked on a HGCommand scratch buffer acts 
   as though it was invoked on the original file and splits the screen so that 
   the output appears in a new window.

   Many of the commands accept revisions as arguments.  By default, most 
   operate on the most recent revision on the current branch if no revision is 
   specified (though see |HGCommandInteractive| to prompt instead).

   Each HGCommand is mapped to a key sequence starting with the <Leader> 
   keystroke.  The default mappings may be overridden by supplying different 
   mappings before the plugin is loaded, such as in the vimrc, in the standard 
   fashion for plugin mappings.  For examples, please see 
   |hgcommand-mappings-override|.

   The HGCommand plugin may be configured in several ways.  For more details, 
   please see |hgcommand-customize|.

==============================================================================
4. HGCommand Manual					    *hgcommand-manual*

4.1 HGCommand commands					  *hgcommand-commands*

   HGCommand defines the following commands:

      |:HGAdd|
      |:HGAnnotate|
      |:HGCommit|
      |:HGDiff|
      |:HGGotoOriginal|
      |:HGLog|
      |:HGRevert|
      |:HGReview|
      |:HGStatus|
      |:HGUpdate|
      |:HGVimDiff|

:HGAdd							              *:HGAdd*

   This command performs "hg add" on the current file.  Please note, this does 
   not commit the newly-added file.

:HGAnnotate						         *:HGAnnotate*

   This command performs "hg annotate" on the current file.  If an argument is 
   given, the argument is used as a revision number to display.  If not given 
   an argument, it uses the most recent version of the file on the current 
   branch.  Additionally, if the current buffer is a HGAnnotate buffer 
   already, the version number on the current line is used.

   If the |HGCommandAnnotateParent| variable is set to a non-zero value, the 
   version previous to the one on the current line is used instead.  This 
   allows one to navigate back to examine the previous version of a line.

   The filetype of the HGCommand scratch buffer is set to 'HGAnnotate', to 
   take advantage of the bundled syntax file.


:HGCommit[!]						           *:HGCommit*

   If called with arguments, this performs "hg commit" using the arguments as 
   the log message.

   If '!' is used with no arguments, an empty log message is committed.

   If called with no arguments, this is a two-step command.  The first step 
   opens a buffer to accept a log message.  When that buffer is written, it is 
   automatically closed and the file is committed using the information from 
   that log message.  The commit can be abandoned if the log message buffer is 
   deleted or wiped before being written.

   Alternatively, the mapping that is used to invoke :HGCommit (by default 
   <Leader>hgc) can be used in the log message buffer to immediately commit.  
   This is useful if the |HGCommandCommitOnWrite| variable is set to 0 to 
   disable the normal commit-on-write behavior.

:HGDiff						                     *:HGDiff*

   With no arguments, this performs "hg diff" on the current file against the 
   current repository version.

   With one argument, "hg diff" is performed on the current file against the 
   specified revision.

   With two arguments, hg diff is performed between the specified revisions of 
   the current file.

   This command uses the 'HGCommandDiffOpt' variable to specify diff options.  
   If that variable does not exist, then 'wbBc' is assumed.  If you wish to 
   have no options, then set it to the empty string.


:HGGotoOriginal					             *:HGGotoOriginal*

   This command returns the current window to the source buffer, if the 
   current buffer is a HG command output buffer.

:HGGotoOriginal!

   Like ":HGGotoOriginal" but also executes :bufwipeout on all HG command 
   output buffers for the source buffer.

:HGLog							              *:HGLog*

   Performs "hg log" on the current file.

   If an argument is given, it is passed as an argument to the "-r" option of 
   "hg log".

:HGRevert						           *:HGRevert*

   Replaces the current file with the most recent version from the repository 
   in order to wipe out any undesired changes.
 
:HGReview						           *:HGReview*

   Retrieves a particular version of the current file.  If no argument is 
   given, the most recent version of the file on the current branch is 
   retrieved.  Otherwise, the specified version is retrieved.

:HGStatus					 	           *:HGStatus*

   Performs "hg status" on the current file.

:HGUpdate						           *:HGUpdate*

   Performs "hg update" on the current file.  This intentionally does not 
   automatically reload the current buffer, though vim should prompt the user 
   to do so if the underlying file is altered by this command.

:HGVimDiff						          *:HGVimDiff*

   With no arguments, this prompts the user for a revision and then uses 
   vimdiff to display the differences between the current file and the 
   specified revision.  If no revision is specified, the most recent version 
   of the file on the current branch is used.

   With one argument, that argument is used as the revision as above.  With 
   two arguments, the differences between the two revisions is displayed using 
   vimdiff.

   With either zero or one argument, the original buffer is used to perform 
   the vimdiff.  When the other buffer is closed, the original buffer will be 
   returned to normal mode.

   Once vimdiff mode is started using the above methods, additional vimdiff 
   buffers may be added by passing a single version argument to the command.  
   There may be up to 4 vimdiff buffers total.

   Using the 2-argument form of the command resets the vimdiff to only those 2 
   versions.  Additionally, invoking the command on a different file will 
   close the previous vimdiff buffers.


4.2 Mappings						  *hgcommand-mappings*

   By default, a mapping is defined for each command.  These mappings execute 
   the default (no-argument) form of each command.

      <Leader>hga HGAdd
      <Leader>hgn HGAnnotate
      <Leader>hgc HGCommit
      <Leader>hgd HGDiff
      <Leader>hgg HGGotoOriginal
      <Leader>hgG HGGotoOriginal!
      <Leader>hgl HGLog
      <Leader>hgr HGReview
      <Leader>hgs HGStatus
      <Leader>hgu HGUpdate
      <Leader>hgv HGVimDiff

                                                 *hgcommand-mappings-override*

   The default mappings can be overriden by user-provided instead by mapping 
   to <Plug>CommandName.  This is especially useful when these mappings 
   collide with other existing mappings (vim will warn of this during plugin 
   initialization, but will not clobber the existing mappings).

   For instance, to override the default mapping for :HGAdd to set it to 
   '\add', add the following to the vimrc: >

      nmap \add <Plug>HGAdd
<
4.3 Automatic buffer variables			  *hgcommand-buffer-variables*

   Several buffer variables are defined in each HGCommand result buffer.	
   These may be useful for additional customization in callbacks defined in 
   the event handlers (please see |hgcommand-events|).

   The following variables are automatically defined:

b:hgOrigBuffNR						      *b:hgOrigBuffNR*

   This variable is set to the buffer number of the source file.

b:hgcmd						                     *b:hgcmd*

   This variable is set to the name of the hg command that created the result 
   buffer.
==============================================================================

5. Configuration and customization			 *hgcommand-customize*
                                                            *hgcommand-config*

   The HGCommand plugin can be configured in two ways:  by setting 
   configuration variables (see |hgcommand-options|) or by defining HGCommand 
   event handlers (see |hgcommand-events|).  Additionally, the HGCommand 
   plugin provides several option for naming the HG result buffers (see 
   |hgcommand-naming|) and supported a customized status line (see 
   |hgcommand-statusline| and |hgcommand-buffer-management|).

5.1 HGCommand configuration variables			   *hgcommand-options*

   Several variables affect the plugin's behavior.  These variables are 
   checked at time of execution, and may be defined at the window, buffer, or 
   global level and are checked in that order of precedence.


   The following variables are available:

      |HGCommandAnnotateParent|
      |HGCommandCommitOnWrite|
      |HGCommandHGExec|
      |HGCommandDeleteOnHide|
      |HGCommandDiffOpt|
      |HGCommandDiffSplit|
      |HGCommandEdit|
      |HGCommandEnableBufferSetup|
      |HGCommandInteractive|
      |HGCommandNameMarker|
      |HGCommandNameResultBuffers|
      |HGCommandSplit|

HGCommandAnnotateParent			             *HGCommandAnnotateParent*

   This variable, if set to a non-zero value, causes the zero-argument form of 
   HGAnnotate when invoked on a HGAnnotate buffer to go to the version 
   previous to that displayed on the current line. If not set, it defaults to 
   0.

HGCommandCommitOnWrite				      *HGCommandCommitOnWrite*

   This variable, if set to a non-zero value, causes the pending hg commit to 
   take place immediately as soon as the log message buffer is written.  If 
   set to zero, only the HGCommit mapping will cause the pending commit to 
   occur.  If not set, it defaults to 1.

HGCommandHGExec				                     *HGCommandHGExec*

   This variable controls the executable used for all HG commands.  If not 
   set, it defaults to "hg".

HGCommandDeleteOnHide				       *HGCommandDeleteOnHide*

   This variable, if set to a non-zero value, causes the temporary HG result 
   buffers to automatically delete themselves when hidden.

HGCommandDiffOpt				            *HGCommandDiffOpt*

   This variable, if set, determines the options passed to the diff command of 
   HG.  If not set, it defaults to 'w'.

HGCommandDiffSplit				          *HGCommandDiffSplit*

   This variable overrides the |HGCommandSplit| variable, but only for buffers 
   created with |:HGVimDiff|.

HGCommandEdit					               *HGCommandEdit*

   This variable controls whether the original buffer is replaced ('edit') or 
   split ('split').  If not set, it defaults to 'edit'.

HGCommandEnableBufferSetup			  *HGCommandEnableBufferSetup*

   This variable, if set to a non-zero value, activates HG buffer management 
   mode see (|hgcommand-buffer-management|).  This mode means that three 
   buffer variables, 'HGRepository', 'HGRevision' and 'HGBranch', are set if 
   the file is HG-controlled.  This is useful for displaying version 
   information in the status bar.

HGCommandInteractive				        *HGCommandInteractive*

   This variable, if set to a non-zero value, causes appropriate commands (for 
   the moment, only |:HGReview|) to query the user for a revision to use 
   instead of the current revision if none is specified.

HGCommandNameMarker				         *HGCommandNameMarker*

   This variable, if set, configures the special attention-getting characters 
   that appear on either side of the hg buffer type in the buffer name.  This 
   has no effect unless |HGCommandNameResultBuffers| is set to a true value.  
   If not set, it defaults to '_'.  

HGCommandNameResultBuffers			  *HGCommandNameResultBuffers*

   This variable, if set to a true value, causes the hg result buffers to be 
   named in the old way ('<source file name> _<hg command>_').  If not set or 
   set to a false value, the result buffer is nameless.

HGCommandSplit					              *HGCommandSplit*

   This variable controls the orientation of the various window splits that 
   may occur (such as with HGVimDiff, when using a HG command on a HG command 
   buffer, or when the |HGCommandEdit| variable is set to 'split'.  If set to 
   'horizontal', the resulting windows will be on stacked on top of one 
   another.  If set to 'vertical', the resulting windows will be side-by-side.  
   If not set, it defaults to 'horizontal' for all but HGVimDiff windows.

5.2 HGCommand events				            *hgcommand-events*

   For additional customization, HGCommand can trigger user-defined events.  
   Event handlers are provided by defining User event autocommands (see 
   |autocommand|, |User|) in the HGCommand group with patterns matching the 
   event name.

   For instance, the following could be added to the vimrc to provide a 'q' 
   mapping to quit a HGCommand scratch buffer: >

      augroup HGCommand
         au HGCommand User HGBufferCreated silent! nmap <unique> <buffer> q:
         bwipeout<cr>
      augroup END
<

   The following hooks are available:

HGBufferCreated		This event is fired just after a hg command result
                        buffer is created and filled with the result of a hg 
                        command.  It is executed within the context of the HG 
                        command buffer.  The HGCommand buffer variables may be 
                        useful for handlers of this event (please see 
                        |hgcommand-buffer-variables|).

HGBufferSetup		This event is fired just after HG buffer setup occurs,
                        if enabled.

HGPluginInit		This event is fired when the HGCommand plugin first
                        loads.

HGPluginFinish		This event is fired just after the HGCommand plugin
                        loads.

HGVimDiffFinish		This event is fired just after the HGVimDiff command
                        executes to allow customization of, for instance, 
                        window placement and focus.

5.3 HGCommand buffer naming				    *hgcommand-naming*

   By default, the buffers containing the result of HG commands are nameless 
   scratch buffers.  It is intended that buffer variables of those buffers be 
   used to customize the statusline option so that the user may fully control 
   the display of result buffers.

   If the old-style naming is desired, please enable the 
   |HGCommandNameResultBuffers| variable.  Then, each result buffer will 
   receive a unique name that includes the source file name, the HG command, 
   and any extra data (such as revision numbers) that were part of the 
   command.

5.4 HGCommand status line support			*hgcommand-statusline*

   It is intended that the user will customize the |'statusline'| option to 
   include HG result buffer attributes.  A sample function that may be used in 
   the |'statusline'| option is provided by the plugin, HGGetStatusLine().  In 
   order to use that function in the status line, do something like the 
   following: >

      set statusline=%<%f\ %{HGGetStatusLine()}\ %h%m%r%=%l,%c%V\ %P
<
   of which %{HGGetStatusLine()} is the relevant portion.

   The sample HGGetStatusLine() function handles both HG result buffers and 
   HG-managed files if HGCommand buffer management is enabled (please see 
   |hgcommand-buffer-management|).

5.5 HGCommand buffer management		         *hgcommand-buffer-management*

   The HGCommand plugin can operate in buffer management mode, which means 
   that it attempts to set two buffer variables ('HGRevision' and 'HGBranch') 
   upon entry into a buffer.  This is rather slow because it means that 'hg 
   status' will be invoked at each entry into a buffer (during the |BufEnter| 
   autocommand).

   This mode is disabled by default.  In order to enable it, set the 
   |HGCommandEnableBufferSetup| variable to a true (non-zero) value.  Enabling 
   this mode simply provides the buffer variables mentioned above.  The user 
   must explicitly include those in the |'statusline'| option if they are to 
   appear in the status line (but see |hgcommand-statusline| for a simple way
   to do that).

==============================================================================
9. Tips							      *hgcommand-tips*

9.1 Split window annotation, by Michael Anderson >

   :nmap <Leader>hgN :vs<CR><C-w>h<Leader>hgn:vertical res 40<CR>
                 \ggdddd:set scb<CR>:set nowrap<CR><C-w>lgg:set scb<CR>
                 \:set nowrap<CR>
<

   This splits the buffer vertically, puts an annotation on the left (minus 
   the header) with the width set to 40. An editable/normal copy is placed on 
   the right.  The two versions are scroll locked so they  move as one. and 
   wrapping is turned off so that the lines line up correctly. The advantages 
   are...

   1) You get a versioning on the right.
   2) You can still edit your own code.
   3) Your own code still has syntax highlighting.

==============================================================================

8. Known bugs						      *hgcommand-bugs*

   Please let me know if you run across any.

   HGVimDiff, when using the original (real) source buffer as one of the diff 
   buffers, uses some hacks to try to restore the state of the original buffer 
   when the scratch buffer containing the other version is destroyed.  There 
   may still be bugs in here, depending on many configuration details.

==============================================================================
=== END_DOC
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" v im:tw=78:ts=8:ft=help:norl:
" vim600: set foldmethod=marker  tabstop=8 shiftwidth=2 softtabstop=2 smartindent smarttab  :
"fileencoding=iso-8859-15 
