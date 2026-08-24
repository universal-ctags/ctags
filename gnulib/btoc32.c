/* Convert unibyte character to 32-bit wide character.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2020.  */

#include <config.h>

#define IN_BTOC32
/* Specification.  */
#include <uchar.h>

#include <stdio.h>
#include <string.h>
#include <wchar.h>

#if GL_CHAR32_T_IS_UNICODE
# include "lc-charset-unicode.h"
#endif

#if _GL_WCHAR_T_IS_UCS4
_GL_EXTERN_INLINE
#endif
wint_t
btoc32 (int c)
{
#if HAVE_WORKING_MBRTOC32 && HAVE_WORKING_C32RTOMB && !_GL_WCHAR_T_IS_UCS4
  /* The char32_t encoding of a multibyte character may be different than its
     wchar_t encoding.  */
  if (c != EOF)
    {
      mbstate_t state;
      mbszero (&state);
      char s[1];
      s[0] = (unsigned char) c;
      char32_t wc;
      if (mbrtoc32 (&wc, s, 1, &state) <= 1)
        return wc;
    }
  return WEOF;
#else
  /* In all known locale encodings, unibyte characters correspond only to
     characters in the BMP.  */
  wint_t wc = btowc (c);
# if GL_CHAR32_T_IS_UNICODE && GL_CHAR32_T_VS_WCHAR_T_NEEDS_CONVERSION
  if (wc != WEOF && wc != 0)
    {
      wc = locale_encoding_to_unicode (wc);
      if (wc == 0)
        return WEOF;
    }
# endif
  return wc;
#endif
}
