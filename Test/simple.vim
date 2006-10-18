
" These variables should be tagged
let g:var_global_scope = 1
let s:var_script_scope = 1
let var_script_default_scope = 1
let forms#FT_TEXTFIELD = 'textfield'
let forms#form = {
      \ 'title': 'Address Entry Form',
      \ 'fields': [],
      \ 'defaultbutton': 'ok',
      \ 'fieldMap': {},
      \ 'hotkeyMap': {},
      \ }
let $env_var = 'something'


" These lets should be ignored
let &errorformat = "%A%.%#rror reported by parser: %m" 
let @a = 'set register a'
let [a, b; rest] = ["aval", "bval", 3, 4]

let 

" These lets should be ignored Vim variables are readonly
let v:version = 'something'

" autogroups
augroup uncompress
  au!
  au BufEnter *.gz	%!gunzip
augroup END

" This deletes the autogroup and should be ignored
augroup! uncompress

" Ensure line is ignored
augroup    

" commands
command! -nargs=+ SelectCmd         :call s:DB_execSql("select " . <q-args>)
command! -nargs=* -complete=customlist,<SID>DB_settingsComplete DBSetOption :call s:DB_setMultipleOptions(<q-args>)
comma -bang 
            \NoSpaceBeforeContinue edit<bang> <args>
com -nargs=1 -bang -complete=customlist,EditFileComplete
            \ SpaceBeforeContinue edit<bang> <args>
com -nargs=1 -bang 
            \-complete=customlist,EditFileComplete
            \ -complete=file
            \ ExtraLines edit<bang> <args>

" functions
fu Bar(arg)
    let var_inside_func = []
endf

fu invalidFuncLowerCaseInitialLetter(arg)
endf

function! Foo(arg)
endfunction

    fu IndentedFunction(arg)
    endf

function! <SID>Foo_SID(arg)
endfu

function! s:Foo_scolon(arg)
endfunction

function! s:validFuncLowerCaseInitialLetter(arg)
endfunction

" New to Vim7
let mydict = {'data': [0, 1, 2, 3]}
function mydict.len() dict
    let var_in_func = 2
    let s:script_var_in_func = 2
   return len(self.data)
endfunction
echo mydict.len()

function autoloadFunc#subdirname#Funcname()
   echo "Done!"
endfunction

function! forms#form.addTextField(fname, flabel, fvalue, hotkey)
  let field = s:field.new(self, g:forms#FT_TEXTFIELD, a:fname, a:flabel,
        \ a:fvalue, a:hotkey)
  return field
endfunction

" Invalid function, must start with an UPPER case letter
function e
endfunction

" Shortest possible function name
function E
endfunction

" Ensure it ingores the invalid function declaration
function s:
endfunction

" Tags should be created for
" <F8>
" <Leader>scdt
" ,,,
" (
nnoremap <silent> <F8> :Tlist<CR>
map <unique> <Leader>scdt <Plug>GetColumnDataType
inoremap ,,, <esc>diwi<<esc>pa><cr></<esc>pa><esc>kA
inoremap <buffer> ( <C-R>=PreviewFunctionSignature()<LF> 

