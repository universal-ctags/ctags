/* Convert wide character to multibyte character.
   Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

#include <errno.h>
#include <stdlib.h>


size_t
wcrtomb (char *s, wchar_t wc, mbstate_t *ps)
#undef wcrtomb
{
  /* This implementation of wcrtomb supports only stateless encodings.
     ps must be in the initial state.  */
  if (ps != NULL && !mbsinit (ps))
    {
      errno = EINVAL;
      return (size_t)(-1);
    }

#if !HAVE_WCRTOMB                       /* IRIX 6.5 */ \
    || WCRTOMB_RETVAL_BUG               /* Solaris 11.3, MSVC */ \
    || WCRTOMB_C_LOCALE_BUG             /* Android */
  if (s == NULL)
    /* We know the NUL wide character corresponds to the NUL character.  */
    return 1;
  else
#endif
    {
#if HAVE_WCRTOMB
# if WCRTOMB_C_LOCALE_BUG               /* Android */
      /* Implement consistently with mbrtowc(): through a 1:1 correspondence,
         as in ISO-8859-1.  */
      if (wc >= 0 && wc <= 0xff)
        {
          *s = (unsigned char) wc;
          return 1;
        }
      else
        {
          errno = EILSEQ;
          return (size_t)(-1);
        }
# else
      return wcrtomb (s, wc, ps);
# endif
#else                                   /* IRIX 6.5 */
      /* Fallback for platforms that don't have wcrtomb().
         Implement on top of wctomb().
         This code is not multithread-safe.  */
      int ret = wctomb (s, wc);

      if (ret >= 0)
        return ret;
      else
        {
          errno = EILSEQ;
          return (size_t)(-1);
        }
#endif
    }
}
