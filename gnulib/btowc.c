/* Convert unibyte character to wide character.
   Copyright (C) 2008, 2010-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2008.

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

#include <config.h>

/* Specification.  */
#include <wchar.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

wint_t
btowc (int c)
{
  if (c != EOF)
    {
      char buf[1];
      wchar_t wc;

      buf[0] = c;
#if HAVE_MBRTOWC
      mbstate_t state;
      mbszero (&state);
      size_t ret = mbrtowc (&wc, buf, 1, &state);
      if (!(ret == (size_t)(-1) || ret == (size_t)(-2)))
#else
      if (mbtowc (&wc, buf, 1) >= 0)
#endif
        return wc;
    }
  return WEOF;
}
