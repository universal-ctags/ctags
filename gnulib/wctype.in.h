/* A substitute for ISO C99 <wctype.h>, for platforms that lack it.

   Copyright (C) 2006-2021 Free Software Foundation, Inc.

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

/* Written by Bruno Haible and Paul Eggert.  */

/*
 * ISO C 99 <wctype.h> for platforms that lack it.
 * <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/wctype.h.html>
 *
 * iswctype, towctrans, towlower, towupper, wctrans, wctype,
 * wctrans_t, and wctype_t are not yet implemented.
 */

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

#if (defined __MINGW32__ && defined __CTYPE_H_SOURCED__)

/* Special invocation convention:
   - With MinGW 3.22, when <ctype.h> includes <wctype.h>, only some part of
     <wctype.h> is being processed, which doesn't include the idempotency
     guard.   */

#@INCLUDE_NEXT@ @NEXT_WCTYPE_H@

#else
/* Normal invocation convention.  */

#ifndef _@GUARD_PREFIX@_WCTYPE_H

#if @HAVE_WINT_T@
/* Solaris 2.5 has a bug: <wchar.h> must be included before <wctype.h>.  */
# include <wchar.h>
#endif

/* Native Windows (mingw, MSVC) have declarations of towupper, towlower, and
   isw* functions in <ctype.h>, <wchar.h> as well as in <wctype.h>.  Include
   <ctype.h>, <wchar.h> in advance to avoid rpl_ prefix being added to the
   declarations.  */
#if defined _WIN32 && ! defined __CYGWIN__
# include <ctype.h>
# include <wchar.h>
#endif

/* Include the original <wctype.h> if it exists.
   BeOS 5 has the functions but no <wctype.h>.  */
/* The include_next requires a split double-inclusion guard.  */
#if @HAVE_WCTYPE_H@
# @INCLUDE_NEXT@ @NEXT_WCTYPE_H@
#endif

#ifndef _@GUARD_PREFIX@_WCTYPE_H
#define _@GUARD_PREFIX@_WCTYPE_H

#ifndef _GL_INLINE_HEADER_BEGIN
 #error "Please include config.h first."
#endif
_GL_INLINE_HEADER_BEGIN
#ifndef _GL_WCTYPE_INLINE
# define _GL_WCTYPE_INLINE _GL_INLINE
#endif

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

/* Solaris 2.6 <wctype.h> includes <widec.h> which includes <euc.h> which
   #defines a number of identifiers in the application namespace.  Revert
   these #defines.  */
#ifdef __sun
# undef multibyte
# undef eucw1
# undef eucw2
# undef eucw3
# undef scrw1
# undef scrw2
# undef scrw3
#endif

/* Define wint_t and WEOF.  (Also done in wchar.in.h.)  */
#if !@HAVE_WINT_T@ && !defined wint_t
# define wint_t int
# ifndef WEOF
#  define WEOF -1
# endif
#else
/* mingw and MSVC define wint_t as 'unsigned short' in <crtdefs.h> or
   <stddef.h>.  This is too small: ISO C 99 section 7.24.1.(2) says that
   wint_t must be "unchanged by default argument promotions".  Override it.  */
# if @GNULIBHEADERS_OVERRIDE_WINT_T@
#  if !GNULIB_defined_wint_t
#   if @HAVE_CRTDEFS_H@
#    include <crtdefs.h>
#   else
#    include <stddef.h>
#   endif
typedef unsigned int rpl_wint_t;
#   undef wint_t
#   define wint_t rpl_wint_t
#   define GNULIB_defined_wint_t 1
#  endif
# endif
# ifndef WEOF
#  define WEOF ((wint_t) -1)
# endif
#endif


#if !GNULIB_defined_wctype_functions

/* FreeBSD 4.4 to 4.11 has <wctype.h> but lacks the functions.
   Linux libc5 has <wctype.h> and the functions but they are broken.
   mingw and MSVC have <wctype.h> and the functions but they take a wchar_t
   as argument, not an rpl_wint_t.
   Assume all 11 functions (all isw* except iswblank) are implemented the
   same way, or not at all.  */
# if ! @HAVE_ISWCNTRL@ || @REPLACE_ISWCNTRL@

#  if @GNULIBHEADERS_OVERRIDE_WINT_T@ /* implies @REPLACE_ISWCNTRL@ */

_GL_WCTYPE_INLINE int
rpl_iswalnum (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswalnum ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswalpha (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswalpha ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswblank (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswblank ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswcntrl (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswcntrl ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswdigit (wint_t wc)
{
  return ((wchar_t) wc == wc ? wc >= '0' && wc <= '9' : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswgraph (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswgraph ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswlower (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswlower ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswprint (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswprint ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswpunct (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswpunct ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswspace (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswspace ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswupper (wint_t wc)
{
  return ((wchar_t) wc == wc ? iswupper ((wchar_t) wc) : 0);
}

_GL_WCTYPE_INLINE int
rpl_iswxdigit (wint_t wc)
{
  return ((wchar_t) wc == wc
          ? (wc >= '0' && wc <= '9')
            || ((wc & ~0x20) >= 'A' && (wc & ~0x20) <= 'F')
          : 0);
}

_GL_WCTYPE_INLINE wint_t
rpl_towlower (wint_t wc)
{
  return ((wchar_t) wc == wc ? (wchar_t) towlower ((wchar_t) wc) : wc);
}

_GL_WCTYPE_INLINE wint_t
rpl_towupper (wint_t wc)
{
  return ((wchar_t) wc == wc ? (wchar_t) towupper ((wchar_t) wc) : wc);
}

#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef iswalnum
#    undef iswalpha
#    undef iswblank
#    undef iswcntrl
#    undef iswdigit
#    undef iswgraph
#    undef iswlower
#    undef iswprint
#    undef iswpunct
#    undef iswspace
#    undef iswupper
#    undef iswxdigit
#    undef towlower
#    undef towupper
#    define iswalnum rpl_iswalnum
#    define iswalpha rpl_iswalpha
#    define iswblank rpl_iswblank
#    define iswcntrl rpl_iswcntrl
#    define iswdigit rpl_iswdigit
#    define iswgraph rpl_iswgraph
#    define iswlower rpl_iswlower
#    define iswprint rpl_iswprint
#    define iswpunct rpl_iswpunct
#    define iswspace rpl_iswspace
#    define iswupper rpl_iswupper
#    define iswxdigit rpl_iswxdigit
#    define towlower rpl_towlower
#    define towupper rpl_towupper
#   endif

#  else

/* IRIX 5.3 has macros but no functions, its isw* macros refer to an
   undefined variable _ctmp_ and to <ctype.h> macros like _P, and they
   refer to system functions like _iswctype that are not in the
   standard C library.  Rather than try to get ancient buggy
   implementations like this to work, just disable them.  */
#   undef iswalnum
#   undef iswalpha
#   undef iswblank
#   undef iswcntrl
#   undef iswdigit
#   undef iswgraph
#   undef iswlower
#   undef iswprint
#   undef iswpunct
#   undef iswspace
#   undef iswupper
#   undef iswxdigit
#   undef towlower
#   undef towupper

/* Linux libc5 has <wctype.h> and the functions but they are broken.  */
#   if @REPLACE_ISWCNTRL@
#    if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#     define iswalnum rpl_iswalnum
#     define iswalpha rpl_iswalpha
#     define iswblank rpl_iswblank
#     define iswcntrl rpl_iswcntrl
#     define iswdigit rpl_iswdigit
#     define iswgraph rpl_iswgraph
#     define iswlower rpl_iswlower
#     define iswprint rpl_iswprint
#     define iswpunct rpl_iswpunct
#     define iswspace rpl_iswspace
#     define iswupper rpl_iswupper
#     define iswxdigit rpl_iswxdigit
#    endif
#   endif
#   if @REPLACE_TOWLOWER@
#    if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#     define towlower rpl_towlower
#     define towupper rpl_towupper
#    endif
#   endif

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswalnum
#   else
iswalnum
#   endif
         (wint_t wc)
{
  return ((wc >= '0' && wc <= '9')
          || ((wc & ~0x20) >= 'A' && (wc & ~0x20) <= 'Z'));
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswalpha
#   else
iswalpha
#   endif
         (wint_t wc)
{
  return (wc & ~0x20) >= 'A' && (wc & ~0x20) <= 'Z';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswblank
#   else
iswblank
#   endif
         (wint_t wc)
{
  return wc == ' ' || wc == '\t';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswcntrl
#   else
iswcntrl
#   endif
        (wint_t wc)
{
  return (wc & ~0x1f) == 0 || wc == 0x7f;
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWDIGIT@
rpl_iswdigit
#   else
iswdigit
#   endif
         (wint_t wc)
{
  return wc >= '0' && wc <= '9';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswgraph
#   else
iswgraph
#   endif
         (wint_t wc)
{
  return wc >= '!' && wc <= '~';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswlower
#   else
iswlower
#   endif
         (wint_t wc)
{
  return wc >= 'a' && wc <= 'z';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswprint
#   else
iswprint
#   endif
         (wint_t wc)
{
  return wc >= ' ' && wc <= '~';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswpunct
#   else
iswpunct
#   endif
         (wint_t wc)
{
  return (wc >= '!' && wc <= '~'
          && !((wc >= '0' && wc <= '9')
               || ((wc & ~0x20) >= 'A' && (wc & ~0x20) <= 'Z')));
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswspace
#   else
iswspace
#   endif
         (wint_t wc)
{
  return (wc == ' ' || wc == '\t'
          || wc == '\n' || wc == '\v' || wc == '\f' || wc == '\r');
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWCNTRL@
rpl_iswupper
#   else
iswupper
#   endif
         (wint_t wc)
{
  return wc >= 'A' && wc <= 'Z';
}

_GL_WCTYPE_INLINE int
#   if @REPLACE_ISWXDIGIT@
rpl_iswxdigit
#   else
iswxdigit
#   endif
          (wint_t wc)
{
  return ((wc >= '0' && wc <= '9')
          || ((wc & ~0x20) >= 'A' && (wc & ~0x20) <= 'F'));
}

_GL_WCTYPE_INLINE wint_t
#   if @REPLACE_TOWLOWER@
rpl_towlower
#   else
towlower
#   endif
         (wint_t wc)
{
  return (wc >= 'A' && wc <= 'Z' ? wc - 'A' + 'a' : wc);
}

_GL_WCTYPE_INLINE wint_t
#   if @REPLACE_TOWLOWER@
rpl_towupper
#   else
towupper
#   endif
         (wint_t wc)
{
  return (wc >= 'a' && wc <= 'z' ? wc - 'a' + 'A' : wc);
}

#  endif

# else
/* Only some of the functions are missing or broken.  */

#  if @GNULIB_ISWBLANK@ && (! @HAVE_ISWBLANK@ || @REPLACE_ISWBLANK@)
/* Only the iswblank function is missing.  */
#   if @REPLACE_ISWBLANK@
#    if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#     define iswblank rpl_iswblank
#    endif
_GL_FUNCDECL_RPL (iswblank, int, (wint_t wc));
#   else
_GL_FUNCDECL_SYS (iswblank, int, (wint_t wc));
#   endif
#  endif

#  if @GNULIB_ISWDIGIT@
#   if @REPLACE_ISWDIGIT@
#    if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#     undef iswdigit
#     define iswdigit rpl_iswdigit
#    endif
_GL_FUNCDECL_RPL (iswdigit, int, (wint_t wc));
#   endif
#  endif

#  if @GNULIB_ISWXDIGIT@
#   if @REPLACE_ISWXDIGIT@
#    if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#     undef iswxdigit
#     define iswxdigit rpl_iswxdigit
#    endif
_GL_FUNCDECL_RPL (iswxdigit, int, (wint_t wc));
#   endif
#  endif

# endif

# if defined __MINGW32__ && !@GNULIBHEADERS_OVERRIDE_WINT_T@

/* On native Windows, wchar_t is uint16_t, and wint_t is uint32_t.
   The functions towlower and towupper are implemented in the MSVCRT library
   to take a wchar_t argument and return a wchar_t result.  mingw declares
   these functions to take a wint_t argument and return a wint_t result.
   This means that:
   1. When the user passes an argument outside the range 0x0000..0xFFFF, the
      function will look only at the lower 16 bits.  This is allowed according
      to POSIX.
   2. The return value is returned in the lower 16 bits of the result register.
      The upper 16 bits are random: whatever happened to be in that part of the
      result register.  We need to fix this by adding a zero-extend from
      wchar_t to wint_t after the call.  */

_GL_WCTYPE_INLINE wint_t
rpl_towlower (wint_t wc)
{
  return (wint_t) (wchar_t) towlower (wc);
}
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   define towlower rpl_towlower
#  endif

_GL_WCTYPE_INLINE wint_t
rpl_towupper (wint_t wc)
{
  return (wint_t) (wchar_t) towupper (wc);
}
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   define towupper rpl_towupper
#  endif

# endif /* __MINGW32__ && !@GNULIBHEADERS_OVERRIDE_WINT_T@ */

# define GNULIB_defined_wctype_functions 1
#endif

#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswalnum, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswalnum, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswalpha, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswalpha, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswcntrl, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswcntrl, int, (wint_t wc));
#endif
#if @GNULIB_ISWDIGIT@
# if @REPLACE_ISWDIGIT@
_GL_CXXALIAS_RPL (iswdigit, int, (wint_t wc));
# else
_GL_CXXALIAS_SYS (iswdigit, int, (wint_t wc));
# endif
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswgraph, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswgraph, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswlower, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswlower, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswprint, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswprint, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswpunct, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswpunct, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswspace, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswspace, int, (wint_t wc));
#endif
#if @REPLACE_ISWCNTRL@
_GL_CXXALIAS_RPL (iswupper, int, (wint_t wc));
#else
_GL_CXXALIAS_SYS (iswupper, int, (wint_t wc));
#endif
#if @GNULIB_ISWXDIGIT@
# if @REPLACE_ISWXDIGIT@
_GL_CXXALIAS_RPL (iswxdigit, int, (wint_t wc));
# else
_GL_CXXALIAS_SYS (iswxdigit, int, (wint_t wc));
# endif
#endif
#if __GLIBC__ >= 2
_GL_CXXALIASWARN (iswalnum);
_GL_CXXALIASWARN (iswalpha);
_GL_CXXALIASWARN (iswcntrl);
_GL_CXXALIASWARN (iswdigit);
_GL_CXXALIASWARN (iswgraph);
_GL_CXXALIASWARN (iswlower);
_GL_CXXALIASWARN (iswprint);
_GL_CXXALIASWARN (iswpunct);
_GL_CXXALIASWARN (iswspace);
_GL_CXXALIASWARN (iswupper);
_GL_CXXALIASWARN (iswxdigit);
#endif

#if @GNULIB_ISWBLANK@
# if @REPLACE_ISWCNTRL@ || @REPLACE_ISWBLANK@
_GL_CXXALIAS_RPL (iswblank, int, (wint_t wc));
# else
_GL_CXXALIAS_SYS (iswblank, int, (wint_t wc));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (iswblank);
# endif
#endif

#if !@HAVE_WCTYPE_T@
# if !GNULIB_defined_wctype_t
typedef void * wctype_t;
#  define GNULIB_defined_wctype_t 1
# endif
#endif

/* Get a descriptor for a wide character property.  */
#if @GNULIB_WCTYPE@
# if !@HAVE_WCTYPE_T@
_GL_FUNCDECL_SYS (wctype, wctype_t, (const char *name));
# endif
_GL_CXXALIAS_SYS (wctype, wctype_t, (const char *name));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wctype);
# endif
#elif defined GNULIB_POSIXCHECK
# undef wctype
# if HAVE_RAW_DECL_WCTYPE
_GL_WARN_ON_USE (wctype, "wctype is unportable - "
                 "use gnulib module wctype for portability");
# endif
#endif

/* Test whether a wide character has a given property.
   The argument WC must be either a wchar_t value or WEOF.
   The argument DESC must have been returned by the wctype() function.  */
#if @GNULIB_ISWCTYPE@
# if @GNULIBHEADERS_OVERRIDE_WINT_T@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef iswctype
#   define iswctype rpl_iswctype
#  endif
_GL_FUNCDECL_RPL (iswctype, int, (wint_t wc, wctype_t desc));
_GL_CXXALIAS_RPL (iswctype, int, (wint_t wc, wctype_t desc));
# else
#  if !@HAVE_WCTYPE_T@
_GL_FUNCDECL_SYS (iswctype, int, (wint_t wc, wctype_t desc));
#  endif
_GL_CXXALIAS_SYS (iswctype, int, (wint_t wc, wctype_t desc));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (iswctype);
# endif
#elif defined GNULIB_POSIXCHECK
# undef iswctype
# if HAVE_RAW_DECL_ISWCTYPE
_GL_WARN_ON_USE (iswctype, "iswctype is unportable - "
                 "use gnulib module iswctype for portability");
# endif
#endif

#if @REPLACE_TOWLOWER@ || defined __MINGW32__
_GL_CXXALIAS_RPL (towlower, wint_t, (wint_t wc));
_GL_CXXALIAS_RPL (towupper, wint_t, (wint_t wc));
#else
_GL_CXXALIAS_SYS (towlower, wint_t, (wint_t wc));
_GL_CXXALIAS_SYS (towupper, wint_t, (wint_t wc));
#endif
#if __GLIBC__ >= 2
_GL_CXXALIASWARN (towlower);
_GL_CXXALIASWARN (towupper);
#endif

#if !@HAVE_WCTRANS_T@
# if !GNULIB_defined_wctrans_t
typedef void * wctrans_t;
#  define GNULIB_defined_wctrans_t 1
# endif
#endif

/* Get a descriptor for a wide character case conversion.  */
#if @GNULIB_WCTRANS@
# if !@HAVE_WCTRANS_T@
_GL_FUNCDECL_SYS (wctrans, wctrans_t, (const char *name));
# endif
_GL_CXXALIAS_SYS (wctrans, wctrans_t, (const char *name));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wctrans);
# endif
#elif defined GNULIB_POSIXCHECK
# undef wctrans
# if HAVE_RAW_DECL_WCTRANS
_GL_WARN_ON_USE (wctrans, "wctrans is unportable - "
                 "use gnulib module wctrans for portability");
# endif
#endif

/* Perform a given case conversion on a wide character.
   The argument WC must be either a wchar_t value or WEOF.
   The argument DESC must have been returned by the wctrans() function.  */
#if @GNULIB_TOWCTRANS@
# if !@HAVE_WCTRANS_T@
_GL_FUNCDECL_SYS (towctrans, wint_t, (wint_t wc, wctrans_t desc));
# endif
_GL_CXXALIAS_SYS (towctrans, wint_t, (wint_t wc, wctrans_t desc));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (towctrans);
# endif
#elif defined GNULIB_POSIXCHECK
# undef towctrans
# if HAVE_RAW_DECL_TOWCTRANS
_GL_WARN_ON_USE (towctrans, "towctrans is unportable - "
                 "use gnulib module towctrans for portability");
# endif
#endif

_GL_INLINE_HEADER_END

#endif /* _@GUARD_PREFIX@_WCTYPE_H */
#endif /* _@GUARD_PREFIX@_WCTYPE_H */
#endif
