/* <uchar.h> substitute - 16-bit and 32-bit wide character types.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2019.  */

/*
 * ISO C 23 <uchar.h> for platforms that lack it.
 */

#ifndef _@GUARD_PREFIX@_UCHAR_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* The include_next requires a split double-inclusion guard.  */
#if (defined __cplusplus ? @CXX_HAVE_UCHAR_H@ : @HAVE_UCHAR_H@)
# if defined __HAIKU__
/* Work around <https://dev.haiku-os.org/ticket/17040>.  */
#  include <stdint.h>
# endif
/* On AIX 7.2 with xlclang++, /usr/include/uchar.h produces compilation errors
   because it contains typedef definitions of char16_t and char32_t, however
   char16_t and char32_t are keywords in this situation.  To work around it,
   define char16_t and char32_t as macros.  */
# if defined __cplusplus && defined _AIX && defined __ibmxl__ && defined __clang__
#  define char16_t gl_char16_t
#  define char32_t gl_char32_t
# endif
# @INCLUDE_NEXT@ @NEXT_UCHAR_H@
#endif

#ifndef _@GUARD_PREFIX@_UCHAR_H
#define _@GUARD_PREFIX@_UCHAR_H

/* This file uses _GL_INLINE_HEADER_BEGIN, _GL_INLINE, _GL_BEGIN_C_LINKAGE,
   _GL_ATTRIBUTE_PURE, GNULIB_POSIXCHECK, HAVE_RAW_DECL_*.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* If needed, get uint_least16_t, uint_least32_t.
   Although POSIX allows <uchar.h> to make all symbols visible from <stdint.h>,
   our includers should not rely on this.  */
#if (! (defined __cplusplus \
        ? @CXX_HAVE_UCHAR_H@ || @CXX_HAS_UCHAR_TYPES@ \
        : @HAVE_UCHAR_H@) \
     || @GNULIBHEADERS_OVERRIDE_CHAR16_T@ || @GNULIBHEADERS_OVERRIDE_CHAR32_T@)
# include <stdint.h>
#endif

/* If needed, get btowc, mbstate_t, mbszero, size_t, wctob, wint_t, WEOF.
   Although POSIX allows <uchar.h> to make all symbols visible from <wchar.h>,
   our includers should not rely on this, except they can rely on wint_t and WEOF
   when part of the API of a Gnulib module extending <uchar.h> that needs
   these two symbols.  */
#if (! (/* The underlying <uchar.h> defines mbstate_t, size_t.  */ \
        defined __cplusplus ? @CXX_HAVE_UCHAR_H@ : @HAVE_UCHAR_H@) \
     || (/* These need wint_t and maybe WEOF and a <wchar.h> function.  */ \
         @GNULIB_BTOC32@ || @GNULIB_C32TOB@) \
     || (/* These need a <wchar.h> function.  */ \
         @GNULIB_C32WIDTH@ || @GNULIB_C32SNRTOMBS@ || @GNULIB_C32SRTOMBS@ \
         || @GNULIB_C32SWIDTH@ || @GNULIB_MBSNRTOC32S@ || @GNULIB_MBSRTOC32S@) \
     || (/* These need mbszero.  */ \
         (@GNULIB_C32STOMBS@ || @GNULIB_MBSTOC32S@)))
# include <wchar.h>
#endif

/* If needed, get iswalnum, iswalpha, iswblank, iswcntrl, iswctype,
   iswdigit, iswgraph, iswlower, iswprint, iswpunct, iswspace,
   iswupper, iswxdigit, towctrans, towlower, towupper, wctrans,
   wctrans_t, wctype, wctype_t, wint_t, WEOF.
   Our includers should not rely on this, except they can rely on wint_t and WEOF
   when part of the API of a Gnulib module extending <uchar.h> that needs
   these two symbols.  */
#if (/* These need wint_t and maybe wctrans_t, wctype_t, WEOF, \
        and a <wchar.h> function.  */ 0 \
     || @GNULIB_C32ISALNUM@ || @GNULIB_C32ISALPHA@ || @GNULIB_C32ISBLANK@ \
     || @GNULIB_C32ISCNTRL@ || @GNULIB_C32ISDIGIT@ || @GNULIB_C32ISGRAPH@ \
     || @GNULIB_C32ISLOWER@ || @GNULIB_C32ISPRINT@ || @GNULIB_C32ISPUNCT@ \
     || @GNULIB_C32ISSPACE@ || @GNULIB_C32ISUPPER@ || @GNULIB_C32ISXDIGIT@ \
     || @GNULIB_C32TOLOWER@ || @GNULIB_C32TOUPPER@ \
     || @GNULIB_C32_GET_TYPE_TEST@ || @GNULIB_C32_APPLY_TYPE_TEST@ \
     || @GNULIB_C32_GET_MAPPING@ || @GNULIB_C32_APPLY_MAPPING@)
# include <wctype.h>
#endif

/* The __attribute__ feature is available in gcc versions 2.5 and later.
   The attribute __pure__ was added in gcc 2.96.  */
#ifndef _GL_ATTRIBUTE_PURE
# if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 96) || defined __clang__
#  define _GL_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define _GL_ATTRIBUTE_PURE /* empty */
# endif
#endif

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_ARG_NONNULL is copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */


_GL_INLINE_HEADER_BEGIN


#if !(defined __cplusplus ? @CXX_HAVE_UCHAR_H@ || @CXX_HAS_CHAR8_TYPE@ : @HAVE_UCHAR_H@)

/* An 8-bit variant of wchar_t.
   Note: This type is mandated by ISO C 23 or newer, and denotes UTF-8 units.  */
typedef unsigned char char8_t;

#elif @GNULIBHEADERS_OVERRIDE_CHAR8_T@

typedef unsigned char gl_char8_t;
# define char8_t gl_char8_t

#endif

#if !(defined __cplusplus ? @CXX_HAVE_UCHAR_H@ || @CXX_HAS_UCHAR_TYPES@ : @HAVE_UCHAR_H@)

/* A 16-bit variant of wchar_t.
   Note: This type is mandated by ISO C 11 or newer.  In ISO C 23
   and newer, it denotes UTF-16 units; in older versions of ISO C it did
   so on platforms on which __STDC_UTF_16__ was defined.  */
typedef uint_least16_t char16_t;

#elif @GNULIBHEADERS_OVERRIDE_CHAR16_T@

typedef uint_least16_t gl_char16_t;
# define char16_t gl_char16_t

#endif

#if !(defined __cplusplus ? @CXX_HAVE_UCHAR_H@ || @CXX_HAS_UCHAR_TYPES@ : @HAVE_UCHAR_H@)

/* A 32-bit variant of wchar_t.
   Note: This type is mandated by ISO C 11 or newer.  In ISO C 23
   and newer, it denotes UTF-32 code points; in older versions of ISO C
   it did so on platforms on which __STDC_UTF_32__ was defined.
   In gnulib, we guarantee that it denotes UTF-32 code points if and
   only if the module 'uchar-h-c23' is in use.  */
typedef uint_least32_t char32_t;

#elif @GNULIBHEADERS_OVERRIDE_CHAR32_T@

typedef uint_least32_t gl_char32_t;
# define char32_t gl_char32_t

#endif

/* Define if a 'char32_t' can hold more characters than a 'wchar_t'.  */
#if @SMALL_WCHAR_T@                    /* 32-bit AIX, Cygwin, native Windows */
# define _GL_SMALL_WCHAR_T 1
#endif

/* Define if 'wchar_t', like 'char32_t',
     - is a 32-bit type, and
     - represents Unicode code points.
   For this test, we can use __STDC_ISO_10646__ (defined by glibc, musl libc,
   Cygwin) but need to consider _GL_SMALL_WCHAR_T, so as to exclude Cygwin.
   We cannot use __STDC_UTF_16__ or __STDC_UTF_32__
     - because these macros provide info about char16_t and char32_t (not
       wchar_t!), and
     - because GCC >= 4.9 defines these macros on all platforms, even on
       FreeBSD and Solaris.
   We should better not use __STD_UTF_16__, __STD_UTF_32__ either, because
   these macros are misspellings, defined only by Android's <uchar.h>.  */
#if defined __STDC_ISO_10646__ && !_GL_SMALL_WCHAR_T
/* glibc, musl libc */
# define _GL_WCHAR_T_IS_UCS4 1
#endif


/* Convert a single-byte character C to a 32-bit wide character,
   or to WEOF if C is invalid.  */
#if @GNULIB_BTOC32@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_BTOC32
#  if !GNULIB_defined_btoc32
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ATTRIBUTE_PURE wint_t
btoc32 (int c)
{
  return
#   if @GNULIB_BTOWC@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         btowc (c);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_btoc32 1
#  endif
# else
_GL_FUNCDECL_SYS (btoc32, wint_t, (int c), _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (btoc32, wint_t, (int c));
_GL_CXXALIASWARN (btoc32);
#endif


/* Test a specific property of a 32-bit wide character.  */
#if @GNULIB_C32ISALNUM@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISALNUM
#  if !GNULIB_defined_c32isalnum
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isalnum (wint_t wc)
{
  return
#  if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#  endif
         iswalnum (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isalnum 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isalnum, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isalnum, int, (wint_t wc));
_GL_CXXALIASWARN (c32isalnum);
#endif
#if @GNULIB_C32ISALPHA@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISALPHA
#  if !GNULIB_defined_c32isalpha
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isalpha (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswalpha (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isalpha 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isalpha, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isalpha, int, (wint_t wc));
_GL_CXXALIASWARN (c32isalpha);
#endif
#if @GNULIB_C32ISBLANK@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISBLANK
#  if !GNULIB_defined_c32isblank
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isblank (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswblank (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isblank 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isblank, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isblank, int, (wint_t wc));
_GL_CXXALIASWARN (c32isblank);
#endif
#if @GNULIB_C32ISCNTRL@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISCNTRL
#  if !GNULIB_defined_c32iscntrl
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32iscntrl (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswcntrl (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32iscntrl 1
#  endif
# else
_GL_FUNCDECL_SYS (c32iscntrl, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32iscntrl, int, (wint_t wc));
_GL_CXXALIASWARN (c32iscntrl);
#endif
#if @GNULIB_C32ISDIGIT@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISDIGIT
#  if !GNULIB_defined_c32isdigit
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isdigit (wint_t wc)
{
  return
#   if @GNULIB_ISWDIGIT@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswdigit (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isdigit 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isdigit, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isdigit, int, (wint_t wc));
_GL_CXXALIASWARN (c32isdigit);
#endif
#if @GNULIB_C32ISGRAPH@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISGRAPH
#  if !GNULIB_defined_c32isgraph
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isgraph (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswgraph (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isgraph 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isgraph, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isgraph, int, (wint_t wc));
_GL_CXXALIASWARN (c32isgraph);
#endif
#if @GNULIB_C32ISLOWER@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISLOWER
#  if !GNULIB_defined_c32islower
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32islower (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswlower (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32islower 1
#  endif
# else
_GL_FUNCDECL_SYS (c32islower, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32islower, int, (wint_t wc));
_GL_CXXALIASWARN (c32islower);
#endif
#if @GNULIB_C32ISPRINT@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISPRINT
#  if !GNULIB_defined_c32isprint
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isprint (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswprint (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isprint 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isprint, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isprint, int, (wint_t wc));
_GL_CXXALIASWARN (c32isprint);
#endif
#if @GNULIB_C32ISPUNCT@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISPUNCT
#  if !GNULIB_defined_c32ispunct
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32ispunct (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswpunct (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32ispunct 1
#  endif
# else
_GL_FUNCDECL_SYS (c32ispunct, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32ispunct, int, (wint_t wc));
_GL_CXXALIASWARN (c32ispunct);
#endif
#if @GNULIB_C32ISSPACE@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISSPACE
#  if !GNULIB_defined_c32isspace
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isspace (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswspace (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isspace 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isspace, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isspace, int, (wint_t wc));
_GL_CXXALIASWARN (c32isspace);
#endif
#if @GNULIB_C32ISUPPER@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISUPPER
#  if !GNULIB_defined_c32isupper
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isupper (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswupper (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isupper 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isupper, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isupper, int, (wint_t wc));
_GL_CXXALIASWARN (c32isupper);
#endif
#if @GNULIB_C32ISXDIGIT@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32ISXDIGIT
#  if !GNULIB_defined_c32isxdigit
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32isxdigit (wint_t wc)
{
  return
#   if @GNULIB_ISWXDIGIT@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         iswxdigit (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32isxdigit 1
#  endif
# else
_GL_FUNCDECL_SYS (c32isxdigit, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32isxdigit, int, (wint_t wc));
_GL_CXXALIASWARN (c32isxdigit);
#endif


/* Case mapping of a 32-bit wide character.  */
#if @GNULIB_C32TOLOWER@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32TOLOWER
#  if !GNULIB_defined_c32tolower
_GL_BEGIN_C_LINKAGE
_GL_INLINE wint_t
c32tolower (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         towlower (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32tolower 1
#  endif
# else
_GL_FUNCDECL_SYS (c32tolower, wint_t, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32tolower, wint_t, (wint_t wc));
_GL_CXXALIASWARN (c32tolower);
#endif
#if @GNULIB_C32TOUPPER@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32TOUPPER
#  if !GNULIB_defined_c32toupper
_GL_BEGIN_C_LINKAGE
_GL_INLINE wint_t
c32toupper (wint_t wc)
{
  return
#   if defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         towupper (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32toupper 1
#  endif
# else
_GL_FUNCDECL_SYS (c32toupper, wint_t, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32toupper, wint_t, (wint_t wc));
_GL_CXXALIASWARN (c32toupper);
#endif


/* Number of screen columns needed for a 32-bit wide character.  */
#if @GNULIB_C32WIDTH@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32WIDTH
#  if !GNULIB_defined_c32width
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32width (char32_t wc)
{
  return
#   if @GNULIB_WCWIDTH@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wcwidth (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32width 1
#  endif
# else
_GL_FUNCDECL_SYS (c32width, int, (char32_t wc), );
# endif
_GL_CXXALIAS_SYS (c32width, int, (char32_t wc));
_GL_CXXALIASWARN (c32width);
#endif


/* Convert a 32-bit wide character to a multibyte character.  */
#if @GNULIB_C32RTOMB@
# if @REPLACE_C32RTOMB@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef c32rtomb
#   define c32rtomb rpl_c32rtomb
#  endif
_GL_FUNCDECL_RPL (c32rtomb, size_t, (char *s, char32_t wc, mbstate_t *ps), );
_GL_CXXALIAS_RPL (c32rtomb, size_t, (char *s, char32_t wc, mbstate_t *ps));
# else
#  if !@HAVE_C32RTOMB@
_GL_FUNCDECL_SYS (c32rtomb, size_t, (char *s, char32_t wc, mbstate_t *ps), );
#  endif
_GL_CXXALIAS_SYS (c32rtomb, size_t, (char *s, char32_t wc, mbstate_t *ps));
# endif
# if __GLIBC__ + (__GLIBC_MINOR__ >= 16) > 2
_GL_CXXALIASWARN (c32rtomb);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_C32RTOMB
_GL_WARN_ON_USE (c32rtomb, "c32rtomb is not portable - "
                 "use gnulib module c32rtomb for portability");
# endif
#endif


/* Convert a 32-bit wide string to a string.  */
#if @GNULIB_C32SNRTOMBS@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32SNRTOMBS
#  if !GNULIB_defined_c32snrtombs
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) size_t
c32snrtombs (char *dest, const char32_t **srcp, size_t srclen, size_t len,
             mbstate_t *ps)
{
  return
#   if @GNULIB_WCSNRTOMBS@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wcsnrtombs (dest, (const wchar_t **) srcp, srclen, len, ps);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32snrtombs 1
#  endif
# else
_GL_FUNCDECL_SYS (c32snrtombs, size_t,
                  (char *dest, const char32_t **srcp, size_t srclen, size_t len,
                   mbstate_t *ps),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (c32snrtombs, size_t,
                  (char *dest, const char32_t **srcp, size_t srclen, size_t len,
                   mbstate_t *ps));
_GL_CXXALIASWARN (c32snrtombs);
#endif


/* Convert a 32-bit wide string to a string.  */
#if @GNULIB_C32SRTOMBS@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32SRTOMBS
#  if !GNULIB_defined_c32srtombs
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) size_t
c32srtombs (char *dest, const char32_t **srcp, size_t len, mbstate_t *ps)
{
  return
#   if @GNULIB_WCSRTOMBS@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wcsrtombs (dest, (const wchar_t **) srcp, len, ps);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32srtombs 1
#  endif
# else
_GL_FUNCDECL_SYS (c32srtombs, size_t,
                  (char *dest, const char32_t **srcp, size_t len,
                   mbstate_t *ps),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (c32srtombs, size_t,
                  (char *dest, const char32_t **srcp, size_t len,
                   mbstate_t *ps));
_GL_CXXALIASWARN (c32srtombs);
#endif


/* Convert a 32-bit wide string to a string.  */
#if @GNULIB_C32STOMBS@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32STOMBS
#  if !GNULIB_defined_c32stombs
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) size_t
c32stombs (char *dest, const char32_t *src, size_t len)
{
  mbstate_t state;

  mbszero (&state);
  return c32srtombs (dest, &src, len, &state);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32stombs 1
#  endif
# else
_GL_FUNCDECL_SYS (c32stombs, size_t,
                  (char *dest, const char32_t *src, size_t len),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (c32stombs, size_t,
                  (char *dest, const char32_t *src, size_t len));
_GL_CXXALIASWARN (c32stombs);
#endif


/* Number of screen columns needed for a size-bounded 32-bit wide string.  */
#if @GNULIB_C32SWIDTH@
# if (_GL_WCHAR_T_IS_UCS4 && !GNULIB_defined_mbstate_t) && !defined IN_C32SWIDTH
#  if !GNULIB_defined_c32swidth
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((1)) int
c32swidth (const char32_t *s, size_t n)
{
  return
#   if @GNULIB_WCSWIDTH@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wcswidth ((const wchar_t *) s, n);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32swidth 1
#  endif
# else
_GL_FUNCDECL_SYS (c32swidth, int, (const char32_t *s, size_t n),
                                  _GL_ARG_NONNULL ((1)));
# endif
_GL_CXXALIAS_SYS (c32swidth, int, (const char32_t *s, size_t n));
_GL_CXXALIASWARN (c32swidth);
#endif


/* Convert a 32-bit wide character to unibyte character.
   Return the single-byte representation of WC if it exists,
   or EOF otherwise.  */
#if @GNULIB_C32TOB@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32TOB
#  if !GNULIB_defined_c32tob
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32tob (wint_t wc)
{
  return
#   if @GNULIB_WCTOB@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wctob (wc);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32tob 1
#  endif
# else
_GL_FUNCDECL_SYS (c32tob, int, (wint_t wc), );
# endif
_GL_CXXALIAS_SYS (c32tob, int, (wint_t wc));
_GL_CXXALIASWARN (c32tob);
#endif


/* Convert a multibyte character to a 32-bit wide character.  */
#if @GNULIB_MBRTOC32@
# if @REPLACE_MBRTOC32@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbrtoc32
#   define mbrtoc32 rpl_mbrtoc32
#  endif
_GL_FUNCDECL_RPL (mbrtoc32, size_t,
                  (char32_t *pc, const char *s, size_t n, mbstate_t *ps), );
_GL_CXXALIAS_RPL (mbrtoc32, size_t,
                  (char32_t *pc, const char *s, size_t n, mbstate_t *ps));
# else
#  if !@HAVE_MBRTOC32@
_GL_FUNCDECL_SYS (mbrtoc32, size_t,
                  (char32_t *pc, const char *s, size_t n, mbstate_t *ps), );
#  endif
_GL_CXXALIAS_SYS (mbrtoc32, size_t,
                  (char32_t *pc, const char *s, size_t n, mbstate_t *ps));
# endif
# if __GLIBC__ + (__GLIBC_MINOR__ >= 16) > 2
_GL_CXXALIASWARN (mbrtoc32);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBRTOC32
_GL_WARN_ON_USE (mbrtoc32, "mbrtoc32 is not portable - "
                 "use gnulib module mbrtoc32 for portability");
# endif
#endif


/* Convert a multibyte character and returns the next 16-bit wide
   character.  */
#if @GNULIB_MBRTOC16@
# if @REPLACE_MBRTOC16@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbrtoc16
#   define mbrtoc16 rpl_mbrtoc16
#  endif
_GL_FUNCDECL_RPL (mbrtoc16, size_t,
                  (char16_t *pc, const char *s, size_t n, mbstate_t *ps), );
_GL_CXXALIAS_RPL (mbrtoc16, size_t,
                  (char16_t *pc, const char *s, size_t n, mbstate_t *ps));
# else
#  if !@HAVE_MBRTOC16@
_GL_FUNCDECL_SYS (mbrtoc16, size_t,
                  (char16_t *pc, const char *s, size_t n, mbstate_t *ps), );
#  endif
_GL_CXXALIAS_SYS (mbrtoc16, size_t,
                  (char16_t *pc, const char *s, size_t n, mbstate_t *ps));
# endif
# if __GLIBC__ + (__GLIBC_MINOR__ >= 16) > 2
_GL_CXXALIASWARN (mbrtoc16);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBRTOC16
_GL_WARN_ON_USE (mbrtoc16, "mbrtoc16 is not portable - "
                 "use gnulib module mbrtoc16 for portability");
# endif
#endif


/* Convert a string to a 32-bit wide string.  */
#if @GNULIB_MBSNRTOC32S@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_MBSNRTOC32S
#  if !GNULIB_defined_mbsnrtoc32s
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) size_t
mbsnrtoc32s (char32_t *dest, const char **srcp, size_t srclen, size_t len,
             mbstate_t *ps)
{
  return
#   if @GNULIB_MBSNRTOWCS@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         mbsnrtowcs ((wchar_t *) dest, srcp, srclen, len, ps);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_mbsnrtoc32s 1
#  endif
# else
_GL_FUNCDECL_SYS (mbsnrtoc32s, size_t,
                  (char32_t *dest, const char **srcp, size_t srclen, size_t len,
                   mbstate_t *ps),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (mbsnrtoc32s, size_t,
                  (char32_t *dest, const char **srcp, size_t srclen, size_t len,
                   mbstate_t *ps));
_GL_CXXALIASWARN (mbsnrtoc32s);
#endif


/* Convert a string to a 32-bit wide string.  */
#if @GNULIB_MBSRTOC32S@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_MBSRTOC32S
#  if !GNULIB_defined_mbsrtoc32s
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) size_t
mbsrtoc32s (char32_t *dest, const char **srcp, size_t len, mbstate_t *ps)
{
  return
#   if @GNULIB_MBSRTOWCS@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         mbsrtowcs ((wchar_t *) dest, srcp, len, ps);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_mbsrtoc32s 1
#  endif
# else
_GL_FUNCDECL_SYS (mbsrtoc32s, size_t,
                  (char32_t *dest, const char **srcp, size_t len,
                   mbstate_t *ps),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (mbsrtoc32s, size_t,
                  (char32_t *dest, const char **srcp, size_t len,
                   mbstate_t *ps));
_GL_CXXALIASWARN (mbsrtoc32s);
#endif


/* Convert a string to a 32-bit wide string.  */
#if @GNULIB_MBSTOC32S@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_MBSTOC32S
#  if !GNULIB_defined_mbstoc32s
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) size_t
mbstoc32s (char32_t *dest, const char *src, size_t len)
{
  mbstate_t state;

  mbszero (&state);
  return mbsrtoc32s (dest, &src, len, &state);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_mbstoc32s 1
#  endif
# else
_GL_FUNCDECL_SYS (mbstoc32s, size_t,
                  (char32_t *dest, const char *src, size_t len),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (mbstoc32s, size_t,
                  (char32_t *dest, const char *src, size_t len));
_GL_CXXALIASWARN (mbstoc32s);
#endif


#if @GNULIB_C32_GET_TYPE_TEST@ || @GNULIB_C32_APPLY_TYPE_TEST@
/* A scalar type.  Instances of this type, other than (c32_type_test_t) 0,
   represent a character property, sometimes also viewed as a "character class".
   It can be applied to 32-bit wide characters.  It is the counterpart of
   type 'wctype_t' for wide characters.
   To test whether a given character has a certain property, use the function
   'c32_apply_type_test'.  */
# if _GL_WCHAR_T_IS_UCS4
typedef wctype_t c32_type_test_t;
# else
typedef /*bool*/int (*c32_type_test_t) (wint_t wc);
# endif
#endif

/* Return a character property with the given name, or (c32_type_test_t) 0
   if the designated property does not exist.
   This function is the counterpart of function 'wctype' for wide characters.
 */
#if @GNULIB_C32_GET_TYPE_TEST@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32_GET_TYPE_TEST
#  if !GNULIB_defined_c32_get_type_test
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((1)) c32_type_test_t
c32_get_type_test (const char *name)
{
  return
#   if @GNULIB_WCTYPE@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wctype (name);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32_get_type_test 1
#  endif
# else
_GL_FUNCDECL_SYS (c32_get_type_test, c32_type_test_t, (const char *name),
                                                      _GL_ARG_NONNULL ((1)));
# endif
_GL_CXXALIAS_SYS (c32_get_type_test, c32_type_test_t, (const char *name));
_GL_CXXALIASWARN (c32_get_type_test);
#endif

/* Test whether a given 32-bit wide character has the specified character
   property.
   Return non-zero if true, zero if false or if the argument is WEOF.
   This function is the counterpart of function 'iswctype' for wide characters.
 */
#if @GNULIB_C32_APPLY_TYPE_TEST@
# if _GL_WCHAR_T_IS_UCS4
#  if !defined IN_C32_APPLY_TYPE_TEST
#   if !GNULIB_defined_c32_apply_type_test
_GL_BEGIN_C_LINKAGE
_GL_INLINE int
c32_apply_type_test (wint_t wc, c32_type_test_t property)
{
  return
#    if @GNULIB_ISWCTYPE@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#    endif
         iswctype (wc, property);
}
_GL_END_C_LINKAGE
#    define GNULIB_defined_c32_apply_type_test 1
#   endif
#  else
_GL_FUNCDECL_SYS (c32_apply_type_test, int,
                  (wint_t wc, c32_type_test_t property), );
#  endif
# else
_GL_FUNCDECL_SYS (c32_apply_type_test, int,
                  (wint_t wc, c32_type_test_t property),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (c32_apply_type_test, int,
                  (wint_t wc, c32_type_test_t property));
_GL_CXXALIASWARN (c32_apply_type_test);
#endif


#if @GNULIB_C32_GET_MAPPING@ || @GNULIB_C32_APPLY_MAPPING@
/* A scalar type.  Instances of this type, other than (c32_mapping_t) 0,
   represent a character mapping.  It can be applied to 32-bit wide characters.
   It is the counterpart of type 'wctrans_t' for wide characters.
   To apply a certain mapping to a given character, use the function
   'c32_apply_mapping'.  */
# if _GL_WCHAR_T_IS_UCS4
typedef wctrans_t c32_mapping_t;
# else
typedef wint_t (*c32_mapping_t) (wint_t wc);
# endif
#endif

/* Return a character mapping with the given name, or (c32_mapping_t) 0
   if the designated mapping does not exist.
   This function is the counterpart of function 'wctrans' for wide characters.
 */
#if @GNULIB_C32_GET_MAPPING@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32_GET_MAPPING
#  if !GNULIB_defined_c32_get_mapping
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((1)) c32_mapping_t
c32_get_mapping (const char *name)
{
  return
#   if @GNULIB_WCTRANS@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         wctrans (name);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32_get_mapping 1
#  endif
# else
_GL_FUNCDECL_SYS (c32_get_mapping, c32_mapping_t, (const char *name),
                                                  _GL_ARG_NONNULL ((1)));
# endif
_GL_CXXALIAS_SYS (c32_get_mapping, c32_mapping_t, (const char *name));
_GL_CXXALIASWARN (c32_get_mapping);
#endif

/* Apply the specified character mapping to a given 32-bit wide character.
   Return the result of this mapping.  Return the WC argument unchanged if it is
   WEOF.
   This function is the counterpart of function 'towctrans' for wide characters.
 */
#if @GNULIB_C32_APPLY_MAPPING@
# if _GL_WCHAR_T_IS_UCS4 && !defined IN_C32_APPLY_MAPPING
#  if !GNULIB_defined_c32_apply_mapping
_GL_BEGIN_C_LINKAGE
_GL_INLINE _GL_ARG_NONNULL ((2)) wint_t
c32_apply_mapping (wint_t wc, c32_mapping_t mapping)
{
  return
#   if @GNULIB_TOWCTRANS@ && defined __cplusplus && defined GNULIB_NAMESPACE
         GNULIB_NAMESPACE::
#   endif
         towctrans (wc, mapping);
}
_GL_END_C_LINKAGE
#   define GNULIB_defined_c32_apply_mapping 1
#  endif
# else
_GL_FUNCDECL_SYS (c32_apply_mapping, wint_t,
                  (wint_t wc, c32_mapping_t mapping),
                  _GL_ARG_NONNULL ((2)));
# endif
_GL_CXXALIAS_SYS (c32_apply_mapping, wint_t,
                  (wint_t wc, c32_mapping_t mapping));
_GL_CXXALIASWARN (c32_apply_mapping);
#endif


_GL_INLINE_HEADER_END

#endif /* _@GUARD_PREFIX@_UCHAR_H */
#endif /* _@GUARD_PREFIX@_UCHAR_H */
