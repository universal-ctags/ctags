let &errorformat = "%A%.%#rror reported by parser: %m" 
let g:variable = 1

fu Bar(arg)
endf

fu invalidFuncLowerCaseInitialLetter(arg)
endf

function! Foo(arg)
endfunction

    fu IndentedFunction(arg)
    endf

function! <SID>Foo_SID(arg)
endfunction

function! s:Foo_scolon(arg)
endfunction

function! s:validFuncLowerCaseInitialLetter(arg)
endfunction

augroup uncompress
  au!
  au BufEnter *.gz	%!gunzip
augroup END

" New to Vim7
let mydict = {'data': [0, 1, 2, 3]}
function mydict.len() dict
   return len(self.data)
endfunction
echo mydict.len()

function autoloadFunc#subdirname#Funcname()
   echo "Done!"
endfunction

