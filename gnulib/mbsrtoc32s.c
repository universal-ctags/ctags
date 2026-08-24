/* Convert string to 32-bit wide string.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2020.

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

#define IN_MBSRTOC32S
/* Specification.  */
#include <uchar.h>

#include <wchar.h>

#if (HAVE_WORKING_MBRTOC32 && HAVE_WORKING_C32RTOMB && !_GL_WCHAR_T_IS_UCS4) || (GL_CHAR32_T_IS_UNICODE && GL_CHAR32_T_VS_WCHAR_T_NEEDS_CONVERSION) || _GL_SMALL_WCHAR_T
/* The char32_t encoding of a multibyte character may be different than its
   wchar_t encoding, or char32_t is wider than wchar_t.  */

# include <errno.h>
# include <limits.h>
# include <stdlib.h>

# include "strnlen1.h"

extern mbstate_t _gl_mbsrtoc32s_state;

# define FUNC mbsrtoc32s
# define DCHAR_T char32_t
# define INTERNAL_STATE _gl_mbsrtoc32s_state
# define MBRTOWC mbrtoc32
# if GNULIB_MBRTOC32_REGULAR
   /* If the 'mbrtoc32-regular' module is in use, we don't need to handle
      a (size_t)(-3) return value from mbrtoc32.  */
#  define USES_C32 0
# else
#  define USES_C32 1
# endif
# include "mbsrtowcs-impl.h"

#else
/* char32_t and wchar_t are equivalent.  */

static_assert (sizeof (char32_t) == sizeof (wchar_t));

# if _GL_WCHAR_T_IS_UCS4
_GL_EXTERN_INLINE
# endif
size_t
mbsrtoc32s (char32_t *dest, const char **srcp, size_t len, mbstate_t *ps)
{
  return mbsrtowcs ((wchar_t *) dest, srcp, len, ps);
}

#endif
