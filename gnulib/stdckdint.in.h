/* stdckdint.h -- checked integer arithmetic

   Copyright 2022-2026 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

#ifndef _@GUARD_PREFIX@_STDCKDINT_H

/* The include_next requires a split double-inclusion guard.  */
#if defined __cplusplus ? @HAVE_CXX_STDCKDINT_H@ : @HAVE_C_STDCKDINT_H@
# @INCLUDE_NEXT@ @NEXT_STDCKDINT_H@
#endif

#ifndef _@GUARD_PREFIX@_STDCKDINT_H
#define _@GUARD_PREFIX@_STDCKDINT_H

/* Do nothing but include the system header if it works properly.  */
# if defined __cplusplus ? !@HAVE_WORKING_CXX_STDCKDINT_H@ : !@HAVE_WORKING_C_STDCKDINT_H@

/* Avoid redefining macros.  */
#  undef ckd_add
#  undef ckd_sub
#  undef ckd_mul

#  include "intprops-internal.h"

/* Store into *R the low-order bits of A + B, A - B, A * B, respectively.
   Return 1 if the result overflows, 0 otherwise.
   A, B, and *R can have any integer type other than char, bool, a
   bit-precise integer type, or an enumeration type.

   These are like the standard macros introduced in C23, except that
   arguments should not have side effects.  The C++26 standard is
   expected to add this header and its macros.  */

#  define ckd_add(r, a, b) ((bool) _GL_INT_ADD_WRAPV (a, b, r))
#  define ckd_sub(r, a, b) ((bool) _GL_INT_SUBTRACT_WRAPV (a, b, r))
#  define ckd_mul(r, a, b) ((bool) _GL_INT_MULTIPLY_WRAPV (a, b, r))

# endif /* defined __cplusplus ? @HAVE_WORKING_CXX_STDCKDINT_H@ : @HAVE_WORKING_C_STDCKDINT_H@ */
#endif /* _@GUARD_PREFIX@_STDCKDINT_H */
#endif /* _@GUARD_PREFIX@_STDCKDINT_H */
