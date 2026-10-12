/* Case mapping of a 32-bit wide character.
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

/* Written by Bruno Haible <bruno@clisp.org>, 2023.  */

#include <wchar.h>
#include <wctype.h>

#if GNULIB_defined_mbstate_t
# include "localcharset.h"
# include "streq-opt.h"
#endif

#if GL_CHAR32_T_IS_UNICODE
# include "lc-charset-unicode.h"
#endif

#include "unicase.h"

#if _GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t
_GL_EXTERN_INLINE
#endif
wint_t
FUNC (wint_t wc)
{
  /* The char32_t encoding of a multibyte character is defined by the way
     mbrtoc32() is defined.  */

#if GNULIB_defined_mbstate_t            /* AIX */
  /* mbrtoc32() is defined on top of mbtowc() for the non-UTF-8 locales
     and directly for the UTF-8 locales.  */
  if (wc != WEOF)
    {
      const char *encoding = locale_charset ();
      if (STREQ_OPT (encoding, "UTF-8", 'U', 'T', 'F', '-', '8', 0, 0, 0, 0))
        return UCS_FUNC (wc);
      else
        return WCHAR_FUNC (wc);
    }
  else
    return wc;

#elif _GL_SMALL_WCHAR_T                 /* Cygwin, mingw, MSVC */
  /* The wchar_t encoding is UTF-16.
     The char32_t encoding is UCS-4.  */

# if defined _WIN32 && !defined __CYGWIN__
  /* On native Windows, in the UTF-8 locale, towlower and towupper are
     lacking (at least) the mappings for ISO-8859-1 characters, such as
     0x00C9 <-> 0x00E9.  Since it is expensive to test whether the locale
     encoding is UTF-8, ignore the system's WCHAR_FUNC altogether.  */
  if (wc != WEOF)
    return UCS_FUNC (wc);
  else
    return wc;
# else
  if (wc == WEOF || wc == (wchar_t) wc)
    /* wc is in the range for the tow* functions.  */
    return WCHAR_FUNC (wc);
  else
    return UCS_FUNC (wc);
# endif

#elif HAVE_WORKING_MBRTOC32 && HAVE_WORKING_C32RTOMB /* glibc, Android */
  /* mbrtoc32() is essentially defined by the system libc.  */

# if _GL_WCHAR_T_IS_UCS4
  /* The char32_t encoding of a multibyte character is known to be the same as
     the wchar_t encoding.  */
  return WCHAR_FUNC (wc);
# else
  /* The char32_t encoding of a multibyte character is known to be UCS-4,
     different from the wchar_t encoding.  */
  if (wc != WEOF)
    return UCS_FUNC (wc);
  else
    return wc;
# endif

#else /* macOS, FreeBSD, NetBSD, OpenBSD, HP-UX, Solaris, Minix, Android */
  /* char32_t and wchar_t are equivalent.  */
  static_assert (sizeof (char32_t) == sizeof (wchar_t));

# if GL_CHAR32_T_IS_UNICODE && GL_CHAR32_T_VS_WCHAR_T_NEEDS_CONVERSION
  return UCS_FUNC (wc);
# else
  return WCHAR_FUNC (wc);
# endif
#endif
}
