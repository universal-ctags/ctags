let &errorformat="%A%.%#rror reported by parser: %m" 
let g:variable=1

fu Bar(arg)
endf

function! Foo(arg)
endfunction

    fu IndentedFunction(arg)
    endf

augroup uncompress
  au!
  au BufEnter *.gz	%!gunzip
augroup END
