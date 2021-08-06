/* A substitute for ISO C99 <ctype.h>, for platforms on which it is incomplete.

   Copyright (C) 2009-2021 Free Software Foundation, Inc.

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

/* Written by Bruno Haible.  */

/*
 * ISO C 99 <ctype.h> for platforms on which it is incomplete.
 * <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/ctype.h.html>
 */

#ifndef _@GUARD_PREFIX@_CTYPE_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* Include the original <ctype.h>.  */
/* The include_next requires a split double-inclusion guard.  */
#@INCLUDE_NEXT@ @NEXT_CTYPE_H@

#ifndef _@GUARD_PREFIX@_CTYPE_H
#define _@GUARD_PREFIX@_CTYPE_H

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

/* Return non-zero if c is a blank, i.e. a space or tab character.  */
#if @GNULIB_ISBLANK@
# if !@HAVE_ISBLANK@
_GL_EXTERN_C int isblank (int c);
# endif
#elif defined GNULIB_POSIXCHECK
# undef isblank
# if HAVE_RAW_DECL_ISBLANK
_GL_WARN_ON_USE (isblank, "isblank is unportable - "
                 "use gnulib module isblank for portability");
# endif
#endif

#endif /* _@GUARD_PREFIX@_CTYPE_H */
#endif /* _@GUARD_PREFIX@_CTYPE_H */
