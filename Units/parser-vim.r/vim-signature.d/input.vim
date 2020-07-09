" Taken from vim82/autoload/netrw.vim
" ---------------------------------------------------------------------
" s:ShellEscape: shellescape(), or special windows handling {{{2
fun! s:ShellEscape(s, ...)
  if (has('win32') || has('win64')) && $SHELL == '' && &shellslash
    return printf('"%s"', substitute(a:s, '"', '""', 'g'))
  endif 
  let f = a:0 > 0 ? a:1 : 0
  return shellescape(a:s, f)
endfun

fun! s:Foo()
     return []
endfun

function! s:Broken (arg
  return []
endfun

" Taken from vim82/indent/erlang.vim
function! s:GetTokensFromLine(line, string_continuation, atom_continuation,
                             \tabstop)

  let linelen = strlen(a:line) " The length of the line
  let i = 0 " The index of the current character in the line
  let vcol = 0 " The virtual column of the current character
  let indtokens = []

  if a:string_continuation
    let i = matchend(a:line, '^\%([^"\\]\|\\.\)*"', 0)
    if i ==# -1
      call s:Log('    Whole line is string continuation -> ignore')
      return []
    else
      let vcol = s:CalcVCol(a:line, 0, i - 1, 0, a:tabstop)
      call add(indtokens, ['<string_end>', vcol, i])
    endif
  elseif a:atom_continuation
    let i = matchend(a:line, "^\\%([^'\\\\]\\|\\\\.\\)*'", 0)
    if i ==# -1
      call s:Log('    Whole line is quoted atom continuation -> ignore')
      return []
    else
      let vcol = s:CalcVCol(a:line, 0, i - 1, 0, a:tabstop)
      call add(indtokens, ['<quoted_atom_end>', vcol, i])
    endif
  endif

  while 0 <= i && i < linelen

    let next_vcol = ''

    " Spaces
    if a:line[i] ==# ' '
      let next_i = matchend(a:line, ' *', i + 1)

    " Tabs
    elseif a:line[i] ==# "\t"
      let next_i = matchend(a:line, '\t*', i + 1)

      " See example in s:CalcVCol
      let next_vcol = (vcol / a:tabstop + (next_i - i)) * a:tabstop

    " Comment
    elseif a:line[i] ==# '%'
      let next_i = linelen

    " String token: "..."
    elseif a:line[i] ==# '"'
      let next_i = matchend(a:line, '\%([^"\\]\|\\.\)*"', i + 1)
      if next_i ==# -1
        call add(indtokens, ['<string_start>', vcol, i])
      else
        let next_vcol = s:CalcVCol(a:line, i, next_i - 1, vcol, a:tabstop)
        call add(indtokens, ['<string>', vcol, i])
      endif

    " Quoted atom token: '...'
    elseif a:line[i] ==# "'"
      let next_i = matchend(a:line, "\\%([^'\\\\]\\|\\\\.\\)*'", i + 1)
      if next_i ==# -1
        call add(indtokens, ['<quoted_atom_start>', vcol, i])
      else
        let next_vcol = s:CalcVCol(a:line, i, next_i - 1, vcol, a:tabstop)
        call add(indtokens, ['<quoted_atom>', vcol, i])
      endif

    " Keyword or atom or variable token or number
    elseif a:line[i] =~# '[a-zA-Z_@0-9]'
      let next_i = matchend(a:line,
                           \'[[:alnum:]_@:]*\%(\s*#\s*[[:alnum:]_@:]*\)\=',
                           \i + 1)
      call add(indtokens, [a:line[(i):(next_i - 1)], vcol, i])

    " Character token: $<char> (as in: $a)
    elseif a:line[i] ==# '$'
      call add(indtokens, ['$.', vcol, i])
      let next_i = i + 2

    " Dot token: .
    elseif a:line[i] ==# '.'

      let next_i = i + 1

      if i + 1 ==# linelen || a:line[i + 1] =~# '[[:blank:]%]'
        " End of clause token: . (as in: f() -> ok.)
        call add(indtokens, ['<end_of_clause>', vcol, i])

      else
        " Possibilities:
        " - Dot token in float: . (as in: 3.14)
        " - Dot token in record: . (as in: #myrec.myfield)
        call add(indtokens, ['.', vcol, i])
      endif

    " Equal sign
    elseif a:line[i] ==# '='
      " This is handled separately so that "=<<" will be parsed as
      " ['=', '<<'] instead of ['=<', '<']. Although Erlang parses it
      " currently in the latter way, that may be fixed some day.
      call add(indtokens, [a:line[i], vcol, i])
      let next_i = i + 1

    " Three-character tokens
    elseif i + 1 < linelen &&
         \ index(['=:=', '=/='], a:line[i : i + 1]) != -1
      call add(indtokens, [a:line[i : i + 1], vcol, i])
      let next_i = i + 2

    " Two-character tokens
    elseif i + 1 < linelen &&
         \ index(['->', '<<', '>>', '||', '==', '/=', '=<', '>=', '++', '--',
         \        '::'],
         \       a:line[i : i + 1]) != -1
      call add(indtokens, [a:line[i : i + 1], vcol, i])
      let next_i = i + 2

    " Other character: , ; < > ( ) [ ] { } # + - * / : ? = ! |
    else
      call add(indtokens, [a:line[i], vcol, i])
      let next_i = i + 1

    endif

    if next_vcol ==# ''
      let vcol += next_i - i
    else
      let vcol = next_vcol
    endif

    let i = next_i

  endwhile

  return indtokens

endfunction
