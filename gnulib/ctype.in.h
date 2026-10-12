/* A substitute for ISO C99 <ctype.h>, for platforms on which it is incomplete.

   Copyright (C) 2009-2026 Free Software Foundation, Inc.

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

/* On Solaris 11 OmniOS, we cannot include <locale.h> until after <wchar.h> has
   been entirely included.  That is because
     - <locale.h> includes <xlocale.h>, which makes use of the mbstate_t type.
     - <wchar.h> includes <iso/wchar_iso.h>, which includes <ctype.h> *before*
       defining mbstate_t, WEOF, etc.  */
#if defined __sun && defined _ISO_WCHAR_ISO_H && !defined WEOF
/* We're in the middle of including <iso/wchar_iso.h>.
   Include just the original <ctype.h>.  */

#@INCLUDE_NEXT@ @NEXT_CTYPE_H@

#else

/* Include the original <ctype.h>.  */
/* The include_next requires a split double-inclusion guard.  */
#@INCLUDE_NEXT@ @NEXT_CTYPE_H@

#ifndef _@GUARD_PREFIX@_CTYPE_H
#define _@GUARD_PREFIX@_CTYPE_H

/* This file uses _GL_ARG_NONNULL, GNULIB_POSIXCHECK, HAVE_RAW_DECL_*.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

#if (@GNULIB_ISALNUM_L@ || @GNULIB_ISALPHA_L@ || @GNULIB_ISBLANK_L@ \
     || @GNULIB_ISCNTRL_L@ || @GNULIB_ISDIGIT_L@ || @GNULIB_ISGRAPH_L@ \
     || @GNULIB_ISLOWER_L@ || @GNULIB_ISPRINT_L@ || @GNULIB_ISPUNCT_L@ \
     || @GNULIB_ISSPACE_L@ || @GNULIB_ISUPPER_L@ || @GNULIB_ISXDIGIT_L@ \
     || @GNULIB_TOLOWER_L@ || @GNULIB_TOUPPER_L@)
/* Get locale_t.  */
# include <locale.h>
#endif

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_ARG_NONNULL is copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

/* Return non-zero if c is alphanumeric.  */
#if @GNULIB_ISALNUM_L@
# if !@HAVE_ISALNUM_L@
_GL_FUNCDECL_SYS (isalnum_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isalnum_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isalnum_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISALNUM_L
_GL_WARN_ON_USE (isalnum_l, "isalnum_l is unportable - "
                 "use gnulib module isalnum_l for portability");
# endif
#endif

/* Return non-zero if c is alphabetic.  */
#if @GNULIB_ISALPHA_L@
# if !@HAVE_ISALPHA_L@
_GL_FUNCDECL_SYS (isalpha_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isalpha_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isalpha_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISALPHA_L
_GL_WARN_ON_USE (isalpha_l, "isalpha_l is unportable - "
                 "use gnulib module isalpha_l for portability");
# endif
#endif

/* Return non-zero if c is a blank, i.e. a space or tab character.  */
#if @GNULIB_ISBLANK@
# if !@HAVE_ISBLANK@
_GL_EXTERN_C int isblank (int c);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISBLANK
_GL_WARN_ON_USE (isblank, "isblank is unportable - "
                 "use gnulib module isblank for portability");
# endif
#endif

/* Return non-zero if c is a blank, i.e. a space or tab character.  */
#if @GNULIB_ISBLANK_L@
# if !@HAVE_ISBLANK_L@
_GL_FUNCDECL_SYS (isblank_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isblank_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isblank_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISBLANK_L
_GL_WARN_ON_USE (isblank_l, "isblank_l is unportable - "
                 "use gnulib module isblank_l for portability");
# endif
#endif

/* Return non-zero if c is a control character.  */
#if @GNULIB_ISCNTRL_L@
# if !@HAVE_ISCNTRL_L@
_GL_FUNCDECL_SYS (iscntrl_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (iscntrl_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (iscntrl_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISCNTRL_L
_GL_WARN_ON_USE (iscntrl_l, "iscntrl_l is unportable - "
                 "use gnulib module iscntrl_l for portability");
# endif
#endif

/* Return non-zero if c is a digit.  */
#if @GNULIB_ISDIGIT_L@
# if !@HAVE_ISDIGIT_L@
_GL_FUNCDECL_SYS (isdigit_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isdigit_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isdigit_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISDIGIT_L
_GL_WARN_ON_USE (isdigit_l, "isdigit_l is unportable - "
                 "use gnulib module isdigit_l for portability");
# endif
#endif

/* Return non-zero if c is graphic.  */
#if @GNULIB_ISGRAPH_L@
# if !@HAVE_ISGRAPH_L@
_GL_FUNCDECL_SYS (isgraph_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isgraph_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isgraph_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISGRAPH_L
_GL_WARN_ON_USE (isgraph_l, "isgraph_l is unportable - "
                 "use gnulib module isgraph_l for portability");
# endif
#endif

/* Return non-zero if c is lowercase.  */
#if @GNULIB_ISLOWER_L@
# if !@HAVE_ISLOWER_L@
_GL_FUNCDECL_SYS (islower_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (islower_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (islower_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISLOWER_L
_GL_WARN_ON_USE (islower_l, "islower_l is unportable - "
                 "use gnulib module islower_l for portability");
# endif
#endif

/* Return non-zero if c is printable.  */
#if @GNULIB_ISPRINT_L@
# if !@HAVE_ISPRINT_L@
_GL_FUNCDECL_SYS (isprint_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isprint_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isprint_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISPRINT_L
_GL_WARN_ON_USE (isprint_l, "isprint_l is unportable - "
                 "use gnulib module isprint_l for portability");
# endif
#endif

/* Return non-zero if c is a punctuation or symbol character.  */
#if @GNULIB_ISPUNCT_L@
# if !@HAVE_ISPUNCT_L@
_GL_FUNCDECL_SYS (ispunct_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (ispunct_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (ispunct_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISPUNCT_L
_GL_WARN_ON_USE (ispunct_l, "ispunct_l is unportable - "
                 "use gnulib module ispunct_l for portability");
# endif
#endif

/* Return non-zero if c is white-space.  */
#if @GNULIB_ISSPACE_L@
# if !@HAVE_ISSPACE_L@
_GL_FUNCDECL_SYS (isspace_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isspace_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isspace_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISSPACE_L
_GL_WARN_ON_USE (isspace_l, "isspace_l is unportable - "
                 "use gnulib module isspace_l for portability");
# endif
#endif

/* Return non-zero if c is uppercase.  */
#if @GNULIB_ISUPPER_L@
# if !@HAVE_ISUPPER_L@
_GL_FUNCDECL_SYS (isupper_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isupper_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isupper_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISUPPER_L
_GL_WARN_ON_USE (isupper_l, "isupper_l is unportable - "
                 "use gnulib module isupper_l for portability");
# endif
#endif

/* Return non-zero if c is a hexadecimal digit.  */
#if @GNULIB_ISXDIGIT_L@
# if !@HAVE_ISXDIGIT_L@
_GL_FUNCDECL_SYS (isxdigit_l, int, (int c, locale_t locale),
                                   _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (isxdigit_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (isxdigit_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_ISXDIGIT_L
_GL_WARN_ON_USE (isxdigit_l, "isxdigit_l is unportable - "
                 "use gnulib module isxdigit_l for portability");
# endif
#endif

/* Map c to lowercase.  */
#if @GNULIB_TOLOWER_L@
# if !@HAVE_TOLOWER_L@
_GL_FUNCDECL_SYS (tolower_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (tolower_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (tolower_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_TOLOWER_L
_GL_WARN_ON_USE (tolower_l, "tolower_l is unportable - "
                 "use gnulib module tolower_l for portability");
# endif
#endif

/* Map c to uppercase.  */
#if @GNULIB_TOUPPER_L@
# if !@HAVE_TOUPPER_L@
_GL_FUNCDECL_SYS (toupper_l, int, (int c, locale_t locale),
                                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (toupper_l, int, (int c, locale_t locale));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (toupper_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_TOUPPER_L
_GL_WARN_ON_USE (toupper_l, "toupper_l is unportable - "
                 "use gnulib module toupper_l for portability");
# endif
#endif

#endif /* _@GUARD_PREFIX@_CTYPE_H */
#endif
#endif /* _@GUARD_PREFIX@_CTYPE_H */
