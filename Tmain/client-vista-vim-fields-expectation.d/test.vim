let s:cur_dir = fnamemodify(resolve(expand('<sfile>:p')), ':h')

function! vista#FindItemsUnderDirectory(dir) abort
  return map(split(globpath(a:dir, '*'), '\n'), 'fnamemodify(v:val, '':t:r'')')
endfunction
