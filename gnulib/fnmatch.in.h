/* Substitute for and wrapper around <fnmatch.h>.
   Copyright (C) 1991-1993, 1996-1999, 2001-2003, 2005, 2007, 2009-2021 Free
   Software Foundation, Inc.

   This file is part of the GNU C Library.

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

#ifndef _@GUARD_PREFIX@_FNMATCH_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* The include_next requires a split double-inclusion guard.  */
#if @HAVE_FNMATCH_H@ && !@REPLACE_FNMATCH@
# @INCLUDE_NEXT@ @NEXT_FNMATCH_H@
#endif

#ifndef _@GUARD_PREFIX@_FNMATCH_H
#define _@GUARD_PREFIX@_FNMATCH_H

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_ARG_NONNULL is copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

#if !@HAVE_FNMATCH_H@ || @REPLACE_FNMATCH@

/* We #undef these before defining them because some losing systems
   (HP-UX A.08.07 for example) define these in <unistd.h>.  */
#undef  FNM_PATHNAME
#undef  FNM_NOESCAPE
#undef  FNM_PERIOD

/* Bits set in the FLAGS argument to 'fnmatch'.  */
#define FNM_PATHNAME    (1 << 0) /* No wildcard can ever match '/'.  */
#define FNM_NOESCAPE    (1 << 1) /* Backslashes don't quote special chars.  */
#define FNM_PERIOD      (1 << 2) /* Leading '.' is matched only explicitly.  */

#if !defined _POSIX_C_SOURCE || _POSIX_C_SOURCE < 2 || defined _GNU_SOURCE
# define FNM_FILE_NAME   FNM_PATHNAME   /* Preferred GNU name.  */
# define FNM_LEADING_DIR (1 << 3)       /* Ignore '/...' after a match.  */
# define FNM_CASEFOLD    (1 << 4)       /* Compare without regard to case.  */
# define FNM_EXTMATCH    (1 << 5)       /* Use ksh-like extended matching. */
#endif

/* Value returned by 'fnmatch' if STRING does not match PATTERN.  */
#define FNM_NOMATCH     1

/* This value is returned if the implementation does not support
   'fnmatch'.  Since this is not the case here it will never be
   returned but the conformance test suites still require the symbol
   to be defined.  */
#ifdef _XOPEN_SOURCE
# define FNM_NOSYS      (-1)
#endif

#endif


#if @GNULIB_FNMATCH@
/* Match NAME against the file name pattern PATTERN,
   returning zero if it matches, FNM_NOMATCH if not.  */
# if @REPLACE_FNMATCH@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   define fnmatch rpl_fnmatch
#  endif
_GL_FUNCDECL_RPL (fnmatch, int,
                  (const char *pattern, const char *name, int flags)
                  _GL_ARG_NONNULL ((1, 2)));
_GL_CXXALIAS_RPL (fnmatch, int,
                  (const char *pattern, const char *name, int flags));
# else
#  if !@HAVE_FNMATCH@
_GL_FUNCDECL_SYS (fnmatch, int,
                  (const char *pattern, const char *name, int flags)
                  _GL_ARG_NONNULL ((1, 2)));
#  endif
_GL_CXXALIAS_SYS (fnmatch, int,
                  (const char *pattern, const char *name, int flags));
# endif
# if !GNULIB_FNMATCH_GNU && __GLIBC__ >= 2
_GL_CXXALIASWARN (fnmatch);
# endif
#elif defined GNULIB_POSIXCHECK
# undef fnmatch
# if HAVE_RAW_DECL_FNMATCH
_GL_WARN_ON_USE (fnmatch,
                 "fnmatch does not portably work - "
                 "use gnulib module fnmatch for portability or gnulib module fnmatch-gnu for a glibc compatible implementation");
# endif
#endif


#endif /* _@GUARD_PREFIX@_FNMATCH_H */
#endif /* _@GUARD_PREFIX@_FNMATCH_H */
