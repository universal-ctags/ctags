/* Convert multibyte character to wide character.
   Copyright (C) 2011-2021 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2011.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* We don't need a static internal state, because the encoding is not state
   dependent, and when mbrtowc returns (size_t)(-2). we throw the result
   away. */

int
mbtowc (wchar_t *pwc, const char *s, size_t n)
{
  if (s == NULL)
    return 0;
  else
    {
      mbstate_t state;
      wchar_t wc;
      size_t result;

      memset (&state, 0, sizeof (mbstate_t));
      result = mbrtowc (&wc, s, n, &state);
      if (result == (size_t)-1 || result == (size_t)-2)
        {
          errno = EILSEQ;
          return -1;
        }
      if (pwc != NULL)
        *pwc = wc;
      return (wc == 0 ? 0 : result);
    }
}
