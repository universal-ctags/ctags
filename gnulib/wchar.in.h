/* A substitute for ISO C99 <wchar.h>, for platforms that have issues.

   Copyright (C) 2007-2026 Free Software Foundation, Inc.

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

/* Written by Eric Blake.  */

/*
 * ISO C 99 <wchar.h> for platforms that have issues.
 * <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/wchar.h.html>
 *
 * For now, this just ensures proper prerequisite inclusion order and
 * the declaration of wcwidth().
 */

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

#if (((defined __need_mbstate_t || defined __need_wint_t)               \
      && !defined __MINGW32__)                                          \
     || (defined __hpux                                                 \
         && ((defined _INTTYPES_INCLUDED                                \
              && !defined _GL_FINISHED_INCLUDING_SYSTEM_INTTYPES_H)     \
             || defined _GL_JUST_INCLUDE_SYSTEM_WCHAR_H))               \
     || (defined __MINGW32__ && defined __STRING_H_SOURCED__))
/* Special invocation convention:
   - Inside glibc and uClibc header files, but not MinGW.
   - On HP-UX 11.00 we have a sequence of nested includes
     <wchar.h> -> <stdlib.h> -> <stdint.h>, and the latter includes <wchar.h>,
     once indirectly <stdint.h> -> <sys/types.h> -> <inttypes.h> -> <wchar.h>
     and once directly.  In both situations 'wint_t' is not yet defined,
     therefore we cannot provide the function overrides; instead include only
     the system's <wchar.h>.
   - With MinGW 3.22, when <string.h> includes <wchar.h>, only some part of
     <wchar.h> is actually processed, and that doesn't include 'mbstate_t'.  */

#@INCLUDE_NEXT@ @NEXT_WCHAR_H@
/* The glibc 2.5 /usr/include/wchar.h defines __need_wint_t but never undefines
   it.  We need to do that here.  */
#undef __need_wint_t

#else
/* Normal invocation convention.  */

#ifndef _@GUARD_PREFIX@_WCHAR_H

#if @HAVE_FEATURES_H@
# include <features.h> /* for __GLIBC__ */
#endif

/* In some builds of uClibc, <wchar.h> is nonexistent and wchar_t is defined
   by <stddef.h>.
   But avoid namespace pollution on glibc systems.  */
#if !(defined __GLIBC__ && !defined __UCLIBC__)
# include <stddef.h>
#endif

/* Include the original <wchar.h> if it exists.
   Some builds of uClibc lack it.  */
/* The include_next requires a split double-inclusion guard.  */
#if @HAVE_WCHAR_H@
# @INCLUDE_NEXT@ @NEXT_WCHAR_H@
#endif

#ifndef _@GUARD_PREFIX@_WCHAR_H
#define _@GUARD_PREFIX@_WCHAR_H

/* This file uses _GL_ATTRIBUTE_DEALLOC, _GL_ATTRIBUTE_MALLOC,
   _GL_ATTRIBUTE_NOTHROW, _GL_ATTRIBUTE_PURE, GNULIB_POSIXCHECK,
   HAVE_RAW_DECL_*.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* _GL_ATTRIBUTE_DEALLOC (F, I) declares that the function returns pointers
   that can be freed by passing them as the Ith argument to the
   function F.  */
#ifndef _GL_ATTRIBUTE_DEALLOC
# if __GNUC__ >= 11 && !defined __clang__
#  define _GL_ATTRIBUTE_DEALLOC(f, i) __attribute__ ((__malloc__ (f, i)))
# else
#  define _GL_ATTRIBUTE_DEALLOC(f, i)
# endif
#endif

/* _GL_ATTRIBUTE_DEALLOC_FREE declares that the function returns pointers that
   can be freed via 'free'; it can be used only after declaring 'free'.  */
/* Applies to: functions.  Cannot be used on inline functions.  */
#ifndef _GL_ATTRIBUTE_DEALLOC_FREE
# if defined __cplusplus && defined __GNUC__ && !defined __clang__
/* Work around GCC bug <https://gcc.gnu.org/PR108231> */
#  define _GL_ATTRIBUTE_DEALLOC_FREE \
     _GL_ATTRIBUTE_DEALLOC ((void (*) (void *)) free, 1)
# else
#  define _GL_ATTRIBUTE_DEALLOC_FREE \
     _GL_ATTRIBUTE_DEALLOC (free, 1)
# endif
#endif

/* _GL_ATTRIBUTE_MALLOC declares that the function returns a pointer to freshly
   allocated memory.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_MALLOC
# if __GNUC__ >= 3 || defined __clang__
#  define _GL_ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
# else
#  define _GL_ATTRIBUTE_MALLOC
# endif
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

/* _GL_ATTRIBUTE_NONNULL_IF_NONZERO (NP, NI) declares that the argument NP
   (a pointer) must not be NULL if the argument NI (an integer) is != 0.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_NONNULL_IF_NONZERO
# if __GNUC__ >= 15 && !defined __clang__
#  define _GL_ATTRIBUTE_NONNULL_IF_NONZERO(np, ni) \
     __attribute__ ((__nonnull_if_nonzero__ (np, ni)))
# else
#  define _GL_ATTRIBUTE_NONNULL_IF_NONZERO(np, ni)
# endif
#endif

/* _GL_ATTRIBUTE_NOTHROW declares that the function does not throw exceptions.
 */
#ifndef _GL_ATTRIBUTE_NOTHROW
# if defined __cplusplus
#  if (__GNUC__ + (__GNUC_MINOR__ >= 8) > 2) || __clang_major__ >= 4
#   if __cplusplus >= 201103L
#    define _GL_ATTRIBUTE_NOTHROW noexcept (true)
#   else
#    define _GL_ATTRIBUTE_NOTHROW throw ()
#   endif
#  else
#   define _GL_ATTRIBUTE_NOTHROW
#  endif
# else
#  if (__GNUC__ + (__GNUC_MINOR__ >= 3) > 3) || defined __clang__
#   define _GL_ATTRIBUTE_NOTHROW __attribute__ ((__nothrow__))
#  else
#   define _GL_ATTRIBUTE_NOTHROW
#  endif
# endif
#endif

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_ARG_NONNULL is copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */


/* Define wint_t and WEOF.  (Also done in wctype.in.h.)  */
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


/* Override mbstate_t if it is too small.
   On AIX, MSVC, and OpenBSD 6.0, mbrtowc needs to be overridden, but
   mbstate_t exists and is large enough and overriding it would cause problems
   in C++ mode.  */
#if !(((defined _WIN32 && !defined __CYGWIN__) || @HAVE_MBSINIT@) && @HAVE_MBRTOWC@) || @REPLACE_MBSTATE_T@
# if !GNULIB_defined_mbstate_t
#  define GNULIB_defined_mbstate_t 1
# endif
#endif

/* Make _GL_ATTRIBUTE_DEALLOC_FREE work, even though <stdlib.h> may not have
   been included yet.  */
#if @GNULIB_FREE_POSIX@
# if (@REPLACE_FREE@ && !defined free \
      && !(defined __cplusplus && defined GNULIB_NAMESPACE))
/* We can't do '#define free rpl_free' here.  */
#  if defined __cplusplus && (__GLIBC__ + (__GLIBC_MINOR__ >= 14) > 2)
_GL_EXTERN_C void rpl_free (void *) _GL_ATTRIBUTE_NOTHROW;
#  else
_GL_EXTERN_C void rpl_free (void *);
#  endif
#  undef _GL_ATTRIBUTE_DEALLOC_FREE
#  define _GL_ATTRIBUTE_DEALLOC_FREE _GL_ATTRIBUTE_DEALLOC (rpl_free, 1)
# else
#  if defined _MSC_VER && !defined free
_GL_EXTERN_C
#   if defined _DLL
     __declspec (dllimport)
#   endif
     void __cdecl free (void *);
#  else
#   if defined __cplusplus && (__GLIBC__ + (__GLIBC_MINOR__ >= 14) > 2)
_GL_EXTERN_C void free (void *) _GL_ATTRIBUTE_NOTHROW;
#   else
_GL_EXTERN_C void free (void *);
#   endif
#  endif
# endif
#else
# if defined _MSC_VER && !defined free
_GL_EXTERN_C
#   if defined _DLL
     __declspec (dllimport)
#   endif
     void __cdecl free (void *);
# else
#  if defined __cplusplus && (__GLIBC__ + (__GLIBC_MINOR__ >= 14) > 2)
_GL_EXTERN_C void free (void *) _GL_ATTRIBUTE_NOTHROW;
#  else
_GL_EXTERN_C void free (void *);
#  endif
# endif
#endif


#if @GNULIB_MBSZERO@
# ifdef __has_builtin
#  if __has_builtin (__builtin_memset)
#   define _GL_WCHAR_MEMSET __builtin_memset
#  endif
# endif
# ifndef _GL_WCHAR_MEMSET
#  include <string.h>
#  define _GL_WCHAR_MEMSET memset
# endif
#endif


/* Declarations for ISO C N3322.  */
#if defined __GNUC__ && __GNUC__ >= 15 && !defined __clang__
_GL_EXTERN_C wchar_t *wmemcpy (wchar_t *__dest, const wchar_t *__src, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3);
_GL_EXTERN_C wchar_t *wmemmove (wchar_t *__dest, const wchar_t *__src, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3);
_GL_EXTERN_C wchar_t *wcsncpy (wchar_t *__dest, const wchar_t *__src, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3);
_GL_EXTERN_C wchar_t *wcsncat (wchar_t *__dest, const wchar_t *__src, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ARG_NONNULL ((1)) _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3);
_GL_EXTERN_C int wmemcmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3);
_GL_EXTERN_C int wcsncmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3);
# ifndef __cplusplus
_GL_EXTERN_C wchar_t *(wmemchr) (const wchar_t *__s, wchar_t __wc, size_t __n)
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3);
# endif
_GL_EXTERN_C wchar_t *wmemset (wchar_t *__s, wchar_t __wc, size_t __n)
# if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
  _GL_ATTRIBUTE_NOTHROW
# endif
  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3);
#endif


/* Convert a single-byte character to a wide character.  */
#if @GNULIB_BTOWC@
# if @REPLACE_BTOWC@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef btowc
#   define btowc rpl_btowc
#  endif
_GL_FUNCDECL_RPL (btowc, wint_t, (int c), _GL_ATTRIBUTE_PURE);
_GL_CXXALIAS_RPL (btowc, wint_t, (int c));
# else
#  if !@HAVE_BTOWC@
_GL_FUNCDECL_SYS (btowc, wint_t, (int c), _GL_ATTRIBUTE_PURE);
#  endif
/* Need to cast, because on mingw, the return type is 'unsigned short'.  */
_GL_CXXALIAS_SYS_CAST (btowc, wint_t, (int c));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (btowc);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_BTOWC
_GL_WARN_ON_USE (btowc, "btowc is unportable - "
                 "use gnulib module btowc for portability");
# endif
#endif


/* Convert a wide character to a single-byte character.  */
#if @GNULIB_WCTOB@
# if @REPLACE_WCTOB@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wctob
#   define wctob rpl_wctob
#  endif
_GL_FUNCDECL_RPL (wctob, int, (wint_t wc), _GL_ATTRIBUTE_PURE);
_GL_CXXALIAS_RPL (wctob, int, (wint_t wc));
# else
#  if !defined wctob && !@HAVE_WCTOB@
/* wctob is provided by gnulib.  */
_GL_FUNCDECL_SYS (wctob, int, (wint_t wc), _GL_ATTRIBUTE_PURE);
#  endif
_GL_CXXALIAS_SYS (wctob, int, (wint_t wc));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wctob);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCTOB
_GL_WARN_ON_USE (wctob, "wctob is unportable - "
                 "use gnulib module wctob for portability");
# endif
#endif


/* Test whether *PS is in an initial state.  */
#if @GNULIB_MBSINIT@
# if @REPLACE_MBSINIT@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbsinit
#   define mbsinit rpl_mbsinit
#  endif
_GL_FUNCDECL_RPL (mbsinit, int, (const mbstate_t *ps), );
_GL_CXXALIAS_RPL (mbsinit, int, (const mbstate_t *ps));
# else
#  if !@HAVE_MBSINIT@
_GL_FUNCDECL_SYS (mbsinit, int, (const mbstate_t *ps), );
#  endif
_GL_CXXALIAS_SYS (mbsinit, int, (const mbstate_t *ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (mbsinit);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBSINIT
_GL_WARN_ON_USE (mbsinit, "mbsinit is unportable - "
                 "use gnulib module mbsinit for portability");
# endif
#endif


/* Put *PS into an initial state.  */
#if @GNULIB_MBSZERO@
/* ISO C 23 § 7.31.6.(3) says that zeroing an mbstate_t is a way to put the
   mbstate_t into an initial state.  However, on many platforms an mbstate_t
   is large, and it is possible - as an optimization - to get away with zeroing
   only part of it.  So, instead of

        mbstate_t state = { 0 };

   or

        mbstate_t state;
        memset (&state, 0, sizeof (mbstate_t));

   we can write this faster code:

        mbstate_t state;
        mbszero (&state);
 */
/* _GL_MBSTATE_INIT_SIZE describes how mbsinit() behaves: It is the number of
   bytes at the beginning of an mbstate_t that need to be zero, for mbsinit()
   to return true.
   _GL_MBSTATE_ZERO_SIZE is the number of bytes at the beginning of an mbstate_t
   that need to be zero,
     - for mbsinit() to return true, and
     - for all other multibyte-aware functions to operate properly.
   0 < _GL_MBSTATE_INIT_SIZE <= _GL_MBSTATE_ZERO_SIZE <= sizeof (mbstate_t).
   These values are determined by source code inspection, where possible, and
   by running the gnulib unit tests.
   We need _GL_MBSTATE_INIT_SIZE because if we define _GL_MBSTATE_ZERO_SIZE
   without considering what mbsinit() does, we get test failures such as
     assertion "mbsinit (&iter->state)" failed
 */
# if GNULIB_defined_mbstate_t                             /* AIX */
/* mbstate_t has at least 4 bytes.  They are used as coded in
   gnulib/lib/mbrtowc.c.  */
#  define _GL_MBSTATE_INIT_SIZE 1
/* define _GL_MBSTATE_ZERO_SIZE 4
   does not work: it causes test failures.
   So, use the safe fallback value, below.  */
# elif __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2             /* glibc */
/* mbstate_t is defined in <bits/types/__mbstate_t.h>.
   For more details, see glibc/iconv/skeleton.c.  */
#  define _GL_MBSTATE_INIT_SIZE 4 /* sizeof (((mbstate_t) {0}).__count) */
#  define _GL_MBSTATE_ZERO_SIZE /* 8 */ sizeof (mbstate_t)
# elif defined MUSL_LIBC                                  /* musl libc */
/* mbstate_t is defined in <bits/alltypes.h>.
   It is an opaque aligned 8-byte struct, of which at most the first
   4 bytes are used.
   For more details, see src/multibyte/mbrtowc.c.  */
#  define _GL_MBSTATE_INIT_SIZE 4 /* sizeof (unsigned) */
#  define _GL_MBSTATE_ZERO_SIZE 4
# elif defined __APPLE__ && defined __MACH__              /* macOS */
/* On macOS, mbstate_t is defined in <machine/_types.h>.
   It is an opaque aligned 128-byte struct, of which at most the first
   12 bytes are used.
   For more details, see the __mbsinit implementations in
   Libc-<version>/locale/FreeBSD/
   {ascii,none,euc,mskanji,big5,gb2312,gbk,gb18030,utf8,utf2}.c.  */
/* File       INIT_SIZE  ZERO_SIZE
   ascii.c        0          0
   none.c         0          0
   euc.c         12         12
   mskanji.c      4          4
   big5.c         4          4
   gb2312.c       4          6
   gbk.c          4          4
   gb18030.c      4          8
   utf8.c         8         10
   utf2.c         8         12 */
#  define _GL_MBSTATE_INIT_SIZE 12
#  define _GL_MBSTATE_ZERO_SIZE 12
# elif defined __FreeBSD__                                /* FreeBSD */
/* On FreeBSD, mbstate_t is defined in src/sys/sys/_types.h.
   It is an opaque aligned 128-byte struct, of which at most the first
   12 bytes are used.
   For more details, see the __mbsinit implementations in
   src/lib/libc/locale/
   {ascii,none,euc,mskanji,big5,gb2312,gbk,gb18030,utf8}.c.  */
/* File       INIT_SIZE  ZERO_SIZE
   ascii.c        0          0
   none.c         0          0
   euc.c         12         12
   mskanji.c      4          4
   big5.c         4          4
   gb2312.c       4          6
   gbk.c          4          4
   gb18030.c      4          8
   utf8.c         8         12 */
#  define _GL_MBSTATE_INIT_SIZE 12
#  define _GL_MBSTATE_ZERO_SIZE 12
# elif defined __NetBSD__                                 /* NetBSD */
/* On NetBSD, mbstate_t is defined in src/sys/sys/ansi.h.
   It is an opaque aligned 128-byte struct, of which at most the first
   28 bytes are used.
   For more details, see the *State types in
   src/lib/libc/citrus/modules/citrus_*.c
   (ignoring citrus_{hz,iso2022,utf7,viqr,zw}.c, since these implement
   stateful encodings, not usable as locale encodings).  */
/* File                                ZERO_SIZE
   citrus/citrus_none.c                    0
   citrus/modules/citrus_euc.c             8
   citrus/modules/citrus_euctw.c           8
   citrus/modules/citrus_mskanji.c         8
   citrus/modules/citrus_big5.c            8
   citrus/modules/citrus_gbk2k.c           8
   citrus/modules/citrus_dechanyu.c        8
   citrus/modules/citrus_johab.c           6
   citrus/modules/citrus_utf8.c           12 */
/* But 12 is not the correct value for _GL_MBSTATE_ZERO_SIZE: we get test
   failures for values < 28.  */
#  define _GL_MBSTATE_ZERO_SIZE 28
# elif defined __OpenBSD__                                /* OpenBSD */
/* On OpenBSD, mbstate_t is defined in src/sys/sys/_types.h.
   It is an opaque aligned 128-byte struct, of which at most the first
   12 bytes are used.
   For more details, see src/lib/libc/citrus/citrus_*.c.  */
/* File           INIT_SIZE  ZERO_SIZE
   citrus_none.c      0          0
   citrus_utf8.c     12         12 */
#  define _GL_MBSTATE_INIT_SIZE 12
#  define _GL_MBSTATE_ZERO_SIZE 12
# elif defined __minix                                    /* Minix */
/* On Minix, mbstate_t is defined in sys/sys/ansi.h.
   It is an opaque aligned 128-byte struct.
   For more details, see the *State types in
   lib/libc/citrus/citrus_*.c.  */
/* File           INIT_SIZE  ZERO_SIZE
   citrus_none.c      0          0 */
/* But 1 is not the correct value for _GL_MBSTATE_ZERO_SIZE: we get test
   failures for values < 4.  */
#  define _GL_MBSTATE_ZERO_SIZE 4
# elif defined __sun                                      /* Solaris */
/* On Solaris, mbstate_t is defined in <wchar_impl.h>.
   It is an opaque aligned 24-byte or 32-byte struct, of which at most the first
   20 or 28 bytes are used.
   For more details on OpenSolaris derivatives, see the *State types in
   illumos-gate/usr/src/lib/libc/port/locale/
   {none,euc,mskanji,big5,gb2312,gbk,gb18030,utf8}.c.  */
/* File       INIT_SIZE  ZERO_SIZE
   none.c         0          0
   euc.c         12         12
   mskanji.c      4          4
   big5.c         4          4
   gb2312.c       4          6
   gbk.c          4          4
   gb18030.c      4          8
   utf8.c        12         12 */
/* But 12 is not the correct value for _GL_MBSTATE_ZERO_SIZE: we get test
   failures
     - in OpenIndiana and OmniOS: for values < 16,
     - in Solaris 10 and 11: for values < 20 (in 32-bit mode)
       or < 28 (in 64-bit mode).
   Since we don't have a good way to distinguish the OpenSolaris derivatives
   from the proprietary Solaris versions, and can't inspect the Solaris source
   code, use the safe fallback values, below.  */
# elif defined __CYGWIN__                                 /* Cygwin */
/* On Cygwin, mbstate_t is defined in <sys/_types.h>.
   For more details, see newlib/libc/stdlib/mbtowc_r.c and
   winsup/cygwin/strfuncs.cc.  */
#  define _GL_MBSTATE_INIT_SIZE 4 /* sizeof (int) */
#  define _GL_MBSTATE_ZERO_SIZE 8
# elif defined _WIN32 && !defined __CYGWIN__              /* Native Windows.  */
/* MSVC defines 'mbstate_t' as an aligned 8-byte struct.
   On mingw, 'mbstate_t' is sometimes defined as 'int', sometimes defined
   as an aligned 8-byte struct, of which the first 4 bytes matter.
   Use the safe values, below.  */
# elif defined __ANDROID__                                /* Android */
/* Android defines 'mbstate_t' in <bits/mbstate_t.h>.
   It is an opaque 4-byte or 8-byte struct.
   For more details, see
   bionic/libc/private/bionic_mbstate.h
   bionic/libc/bionic/mbrtoc32.cpp
   bionic/libc/bionic/mbrtoc16.cpp
 */
#  define _GL_MBSTATE_INIT_SIZE 4
#  define _GL_MBSTATE_ZERO_SIZE 4
# endif
/* Use safe values as defaults.  */
# ifndef _GL_MBSTATE_INIT_SIZE
#  define _GL_MBSTATE_INIT_SIZE sizeof (mbstate_t)
# endif
# ifndef _GL_MBSTATE_ZERO_SIZE
#  define _GL_MBSTATE_ZERO_SIZE sizeof (mbstate_t)
# endif
_GL_BEGIN_C_LINKAGE
# if !GNULIB_defined_mbszero
#  if defined IN_MBSZERO
_GL_EXTERN_INLINE
#  else
_GL_INLINE
#  endif
_GL_ARG_NONNULL ((1)) void
mbszero (mbstate_t *ps)
{
  _GL_WCHAR_MEMSET (ps, 0, _GL_MBSTATE_ZERO_SIZE);
}
#  define GNULIB_defined_mbszero 1
# endif
_GL_END_C_LINKAGE
_GL_CXXALIAS_SYS (mbszero, void, (mbstate_t *ps));
_GL_CXXALIASWARN (mbszero);
#endif


/* Convert a multibyte character to a wide character.  */
#if @GNULIB_MBRTOWC@
# if @REPLACE_MBRTOWC@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbrtowc
#   define mbrtowc rpl_mbrtowc
#  endif
_GL_FUNCDECL_RPL (mbrtowc, size_t,
                  (wchar_t *restrict pwc, const char *restrict s, size_t n,
                   mbstate_t *restrict ps), );
_GL_CXXALIAS_RPL (mbrtowc, size_t,
                  (wchar_t *restrict pwc, const char *restrict s, size_t n,
                   mbstate_t *restrict ps));
# else
#  if !@HAVE_MBRTOWC@
_GL_FUNCDECL_SYS (mbrtowc, size_t,
                  (wchar_t *restrict pwc, const char *restrict s, size_t n,
                   mbstate_t *restrict ps), );
#  endif
_GL_CXXALIAS_SYS (mbrtowc, size_t,
                  (wchar_t *restrict pwc, const char *restrict s, size_t n,
                   mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (mbrtowc);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBRTOWC
_GL_WARN_ON_USE (mbrtowc, "mbrtowc is unportable - "
                 "use gnulib module mbrtowc for portability");
# endif
#endif


/* Recognize a multibyte character.  */
#if @GNULIB_MBRLEN@
# if @REPLACE_MBRLEN@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbrlen
#   define mbrlen rpl_mbrlen
#  endif
_GL_FUNCDECL_RPL (mbrlen, size_t,
                  (const char *restrict s, size_t n, mbstate_t *restrict ps), );
_GL_CXXALIAS_RPL (mbrlen, size_t,
                  (const char *restrict s, size_t n, mbstate_t *restrict ps));
# else
#  if !@HAVE_MBRLEN@
_GL_FUNCDECL_SYS (mbrlen, size_t,
                  (const char *restrict s, size_t n, mbstate_t *restrict ps), );
#  endif
_GL_CXXALIAS_SYS (mbrlen, size_t,
                  (const char *restrict s, size_t n, mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (mbrlen);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBRLEN
_GL_WARN_ON_USE (mbrlen, "mbrlen is unportable - "
                 "use gnulib module mbrlen for portability");
# endif
#endif


/* Convert a string to a wide string.  */
#if @GNULIB_MBSRTOWCS@
# if @REPLACE_MBSRTOWCS@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbsrtowcs
#   define mbsrtowcs rpl_mbsrtowcs
#  endif
_GL_FUNCDECL_RPL (mbsrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (mbsrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t len,
                   mbstate_t *restrict ps));
# else
#  if !@HAVE_MBSRTOWCS@
_GL_FUNCDECL_SYS (mbsrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
#  endif
_GL_CXXALIAS_SYS (mbsrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t len,
                   mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (mbsrtowcs);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBSRTOWCS
_GL_WARN_ON_USE (mbsrtowcs, "mbsrtowcs is unportable - "
                 "use gnulib module mbsrtowcs for portability");
# endif
#endif


/* Convert a string to a wide string.  */
#if @GNULIB_MBSNRTOWCS@
# if @REPLACE_MBSNRTOWCS@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef mbsnrtowcs
#   define mbsnrtowcs rpl_mbsnrtowcs
#  endif
_GL_FUNCDECL_RPL (mbsnrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t srclen, size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (mbsnrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t srclen, size_t len,
                   mbstate_t *restrict ps));
# else
#  if !@HAVE_MBSNRTOWCS@
_GL_FUNCDECL_SYS (mbsnrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t srclen, size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
#  endif
_GL_CXXALIAS_SYS (mbsnrtowcs, size_t,
                  (wchar_t *restrict dest,
                   const char **restrict srcp, size_t srclen, size_t len,
                   mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (mbsnrtowcs);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_MBSNRTOWCS
_GL_WARN_ON_USE (mbsnrtowcs, "mbsnrtowcs is unportable - "
                 "use gnulib module mbsnrtowcs for portability");
# endif
#endif


/* Convert a wide character to a multibyte character.  */
#if @GNULIB_WCRTOMB@
# if @REPLACE_WCRTOMB@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcrtomb
#   define wcrtomb rpl_wcrtomb
#  endif
_GL_FUNCDECL_RPL (wcrtomb, size_t,
                  (char *restrict s, wchar_t wc, mbstate_t *restrict ps), );
_GL_CXXALIAS_RPL (wcrtomb, size_t,
                  (char *restrict s, wchar_t wc, mbstate_t *restrict ps));
# else
#  if !@HAVE_WCRTOMB@
_GL_FUNCDECL_SYS (wcrtomb, size_t,
                  (char *restrict s, wchar_t wc, mbstate_t *restrict ps), );
#  endif
_GL_CXXALIAS_SYS (wcrtomb, size_t,
                  (char *restrict s, wchar_t wc, mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcrtomb);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCRTOMB
_GL_WARN_ON_USE (wcrtomb, "wcrtomb is unportable - "
                 "use gnulib module wcrtomb for portability");
# endif
#endif


/* Convert a wide string to a string.  */
#if @GNULIB_WCSRTOMBS@
# if @REPLACE_WCSRTOMBS@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsrtombs
#   define wcsrtombs rpl_wcsrtombs
#  endif
_GL_FUNCDECL_RPL (wcsrtombs, size_t,
                  (char *restrict dest, const wchar_t **restrict srcp,
                   size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (wcsrtombs, size_t,
                  (char *restrict dest, const wchar_t **restrict srcp,
                   size_t len,
                   mbstate_t *restrict ps));
# else
#  if !@HAVE_WCSRTOMBS@
_GL_FUNCDECL_SYS (wcsrtombs, size_t,
                  (char *restrict dest, const wchar_t **restrict srcp,
                   size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
#  endif
_GL_CXXALIAS_SYS (wcsrtombs, size_t,
                  (char *restrict dest, const wchar_t **restrict srcp,
                   size_t len,
                   mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsrtombs);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSRTOMBS
_GL_WARN_ON_USE (wcsrtombs, "wcsrtombs is unportable - "
                 "use gnulib module wcsrtombs for portability");
# endif
#endif


/* Convert a wide string to a string.  */
#if @GNULIB_WCSNRTOMBS@
# if @REPLACE_WCSNRTOMBS@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsnrtombs
#   define wcsnrtombs rpl_wcsnrtombs
#  endif
_GL_FUNCDECL_RPL (wcsnrtombs, size_t,
                  (char *restrict dest,
                   const wchar_t **restrict srcp, size_t srclen,
                   size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (wcsnrtombs, size_t,
                  (char *restrict dest,
                   const wchar_t **restrict srcp, size_t srclen,
                   size_t len,
                   mbstate_t *restrict ps));
# else
#  if !@HAVE_WCSNRTOMBS@ || (defined __cplusplus && defined __sun)
_GL_FUNCDECL_SYS (wcsnrtombs, size_t,
                  (char *restrict dest,
                   const wchar_t **restrict srcp, size_t srclen,
                   size_t len,
                   mbstate_t *restrict ps),
                  _GL_ARG_NONNULL ((2)));
#  endif
_GL_CXXALIAS_SYS (wcsnrtombs, size_t,
                  (char *restrict dest,
                   const wchar_t **restrict srcp, size_t srclen,
                   size_t len,
                   mbstate_t *restrict ps));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsnrtombs);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSNRTOMBS
_GL_WARN_ON_USE (wcsnrtombs, "wcsnrtombs is unportable - "
                 "use gnulib module wcsnrtombs for portability");
# endif
#endif


/* Return the number of screen columns needed for WC.  */
#if @GNULIB_WCWIDTH@
# if @REPLACE_WCWIDTH@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcwidth
#   define wcwidth rpl_wcwidth
#  endif
_GL_FUNCDECL_RPL (wcwidth, int, (wchar_t), _GL_ATTRIBUTE_PURE);
_GL_CXXALIAS_RPL (wcwidth, int, (wchar_t));
# else
#  if !@HAVE_DECL_WCWIDTH@
/* wcwidth exists but is not declared.  */
_GL_FUNCDECL_SYS (wcwidth, int, (wchar_t), _GL_ATTRIBUTE_PURE);
#  endif
_GL_CXXALIAS_SYS (wcwidth, int, (wchar_t));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcwidth);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCWIDTH
_GL_WARN_ON_USE (wcwidth, "wcwidth is unportable - "
                 "use gnulib module wcwidth for portability");
# endif
#endif


/* Search N wide characters of S for C.  */
#if @GNULIB_WMEMCHR@
# if !@HAVE_WMEMCHR@
_GL_FUNCDECL_SYS (wmemchr, wchar_t *,
                  (const wchar_t *s, wchar_t c, size_t n),
                  _GL_ATTRIBUTE_PURE _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3));
# endif
  /* On some systems, this function is defined as an overloaded function:
       extern "C++" {
         const wchar_t * std::wmemchr (const wchar_t *, wchar_t, size_t);
         wchar_t * std::wmemchr (wchar_t *, wchar_t, size_t);
       }  */
_GL_CXXALIAS_SYS_CAST2 (wmemchr,
                        wchar_t *, (const wchar_t *, wchar_t, size_t),
                        const wchar_t *, (const wchar_t *, wchar_t, size_t));
# if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 10) && !defined __UCLIBC__) \
     && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)) \
     && !defined __clang__
_GL_CXXALIASWARN1 (wmemchr, wchar_t *, (wchar_t *s, wchar_t c, size_t n));
_GL_CXXALIASWARN1 (wmemchr, const wchar_t *,
                   (const wchar_t *s, wchar_t c, size_t n));
# elif __GLIBC__ >= 2 && !defined __CORRECT_ISO_CPP_WCHAR_H_PROTO
_GL_CXXALIASWARN (wmemchr);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WMEMCHR
_GL_WARN_ON_USE (wmemchr, "wmemchr is unportable - "
                 "use gnulib module wmemchr for portability");
# endif
#endif


/* Compare N wide characters of S1 and S2.  */
#if @GNULIB_WMEMCMP@
# if @REPLACE_WMEMCMP@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wmemcmp
#   define wmemcmp rpl_wmemcmp
#  endif
_GL_FUNCDECL_RPL (wmemcmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n),
                  _GL_ATTRIBUTE_PURE
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
_GL_CXXALIAS_RPL (wmemcmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n));
# else
#  if !@HAVE_WMEMCMP@
_GL_FUNCDECL_SYS (wmemcmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n),
                  _GL_ATTRIBUTE_PURE
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
#  endif
_GL_CXXALIAS_SYS (wmemcmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wmemcmp);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WMEMCMP
_GL_WARN_ON_USE (wmemcmp, "wmemcmp is unportable - "
                 "use gnulib module wmemcmp for portability");
# endif
#endif


/* Copy N wide characters of SRC to DEST.  */
#if @GNULIB_WMEMCPY@
# if !@HAVE_WMEMCPY@
_GL_FUNCDECL_SYS (wmemcpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n),
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
# endif
_GL_CXXALIAS_SYS (wmemcpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wmemcpy);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WMEMCPY
_GL_WARN_ON_USE (wmemcpy, "wmemcpy is unportable - "
                 "use gnulib module wmemcpy for portability");
# endif
#endif


/* Copy N wide characters of SRC to DEST, guaranteeing correct behavior for
   overlapping memory areas.  */
#if @GNULIB_WMEMMOVE@
# if !@HAVE_WMEMMOVE@
_GL_FUNCDECL_SYS (wmemmove, wchar_t *,
                  (wchar_t *dest, const wchar_t *src, size_t n),
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
# endif
_GL_CXXALIAS_SYS (wmemmove, wchar_t *,
                  (wchar_t *dest, const wchar_t *src, size_t n));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wmemmove);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WMEMMOVE
_GL_WARN_ON_USE (wmemmove, "wmemmove is unportable - "
                 "use gnulib module wmemmove for portability");
# endif
#endif


/* Copy N wide characters of SRC to DEST.
   Return pointer to wide characters after the last written wide character.  */
#if @GNULIB_WMEMPCPY@
# if @REPLACE_WMEMPCPY@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wmempcpy
#   define wmempcpy rpl_wmempcpy
#  endif
_GL_FUNCDECL_RPL (wmempcpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n), );
_GL_CXXALIAS_RPL (wmempcpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n));
# else
#  if !@HAVE_WMEMPCPY@
_GL_FUNCDECL_SYS (wmempcpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n), );
#  endif
_GL_CXXALIAS_SYS (wmempcpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wmempcpy);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WMEMPCPY
_GL_WARN_ON_USE (wmempcpy, "wmempcpy is unportable - "
                 "use gnulib module wmempcpy for portability");
# endif
#endif


/* Set N wide characters of S to C.  */
#if @GNULIB_WMEMSET@
# if !@HAVE_WMEMSET@
_GL_FUNCDECL_SYS (wmemset, wchar_t *, (wchar_t *s, wchar_t c, size_t n),
                                      _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3));
# endif
_GL_CXXALIAS_SYS (wmemset, wchar_t *, (wchar_t *s, wchar_t c, size_t n));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wmemset);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WMEMSET
_GL_WARN_ON_USE (wmemset, "wmemset is unportable - "
                 "use gnulib module wmemset for portability");
# endif
#endif


/* Return the number of wide characters in S.  */
#if @GNULIB_WCSLEN@
# if !@HAVE_WCSLEN@
_GL_FUNCDECL_SYS (wcslen, size_t, (const wchar_t *s), _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (wcslen, size_t, (const wchar_t *s));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcslen);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSLEN
_GL_WARN_ON_USE (wcslen, "wcslen is unportable - "
                 "use gnulib module wcslen for portability");
# endif
#endif


/* Return the number of wide characters in S, but at most MAXLEN.  */
#if @GNULIB_WCSNLEN@
/* On Solaris 11.3, the header files declare the function in the std::
   namespace, not in the global namespace.  So, force a declaration in
   the global namespace.  */
# if !@HAVE_WCSNLEN@ || (defined __sun && defined __cplusplus)
_GL_FUNCDECL_SYS (wcsnlen, size_t, (const wchar_t *s, size_t maxlen),
                                   _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (wcsnlen, size_t, (const wchar_t *s, size_t maxlen));
_GL_CXXALIASWARN (wcsnlen);
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSNLEN
_GL_WARN_ON_USE (wcsnlen, "wcsnlen is unportable - "
                 "use gnulib module wcsnlen for portability");
# endif
#endif


/* Copy SRC to DEST.  */
#if @GNULIB_WCSCPY@
# if !@HAVE_WCSCPY@
_GL_FUNCDECL_SYS (wcscpy, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src), );
# endif
_GL_CXXALIAS_SYS (wcscpy, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcscpy);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCPY
_GL_WARN_ON_USE (wcscpy, "wcscpy is unportable - "
                 "use gnulib module wcscpy for portability");
# endif
#endif


/* Copy SRC to DEST, returning the address of the terminating L'\0' in DEST.  */
#if @GNULIB_WCPCPY@
/* On Solaris 11.3, the header files declare the function in the std::
   namespace, not in the global namespace.  So, force a declaration in
   the global namespace.  */
# if !@HAVE_WCPCPY@ || (defined __sun && defined __cplusplus)
_GL_FUNCDECL_SYS (wcpcpy, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src), );
# endif
_GL_CXXALIAS_SYS (wcpcpy, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src));
_GL_CXXALIASWARN (wcpcpy);
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCPCPY
_GL_WARN_ON_USE (wcpcpy, "wcpcpy is unportable - "
                 "use gnulib module wcpcpy for portability");
# endif
#endif


/* Copy no more than N wide characters of SRC to DEST.  */
#if @GNULIB_WCSNCPY@
# if !@HAVE_WCSNCPY@
_GL_FUNCDECL_SYS (wcsncpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n),
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
# endif
_GL_CXXALIAS_SYS (wcsncpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsncpy);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSNCPY
_GL_WARN_ON_USE (wcsncpy, "wcsncpy is unportable - "
                 "use gnulib module wcsncpy for portability");
# endif
#endif


/* Copy no more than N characters of SRC to DEST, returning the address of
   the last character written into DEST.  */
#if @GNULIB_WCPNCPY@
/* On Solaris 11.3, the header files declare the function in the std::
   namespace, not in the global namespace.  So, force a declaration in
   the global namespace.  */
# if !@HAVE_WCPNCPY@ || (defined __sun && defined __cplusplus)
_GL_FUNCDECL_SYS (wcpncpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n), );
# endif
_GL_CXXALIAS_SYS (wcpncpy, wchar_t *,
                  (wchar_t *restrict dest,
                   const wchar_t *restrict src, size_t n));
_GL_CXXALIASWARN (wcpncpy);
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCPNCPY
_GL_WARN_ON_USE (wcpncpy, "wcpncpy is unportable - "
                 "use gnulib module wcpncpy for portability");
# endif
#endif


/* Append SRC onto DEST.  */
#if @GNULIB_WCSCAT@
# if !@HAVE_WCSCAT@
_GL_FUNCDECL_SYS (wcscat, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src), );
# endif
_GL_CXXALIAS_SYS (wcscat, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcscat);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCAT
_GL_WARN_ON_USE (wcscat, "wcscat is unportable - "
                 "use gnulib module wcscat for portability");
# endif
#endif


/* Append no more than N wide characters of SRC onto DEST.  */
#if @GNULIB_WCSNCAT@
# if @REPLACE_WCSNCAT@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsncat
#   define wcsncat rpl_wcsncat
#  endif
_GL_FUNCDECL_RPL (wcsncat, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src,
                   size_t n),
                  _GL_ARG_NONNULL ((1))
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
_GL_CXXALIAS_RPL (wcsncat, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src,
                   size_t n));
# else
#  if !@HAVE_WCSNCAT@
_GL_FUNCDECL_SYS (wcsncat, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src,
                   size_t n),
                  _GL_ARG_NONNULL ((1))
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
#  endif
_GL_CXXALIAS_SYS (wcsncat, wchar_t *,
                  (wchar_t *restrict dest, const wchar_t *restrict src,
                   size_t n));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsncat);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSNCAT
_GL_WARN_ON_USE (wcsncat, "wcsncat is unportable - "
                 "use gnulib module wcsncat for portability");
# endif
#endif


/* Compare S1 and S2.  */
#if @GNULIB_WCSCMP@
# if @REPLACE_WCSCMP@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcscmp
#   define wcscmp rpl_wcscmp
#  endif
_GL_FUNCDECL_RPL (wcscmp, int, (const wchar_t *s1, const wchar_t *s2),
                               _GL_ATTRIBUTE_PURE);
_GL_CXXALIAS_RPL (wcscmp, int, (const wchar_t *s1, const wchar_t *s2));
# else
#  if !@HAVE_WCSCMP@
_GL_FUNCDECL_SYS (wcscmp, int, (const wchar_t *s1, const wchar_t *s2),
                               _GL_ATTRIBUTE_PURE);
#  endif
_GL_CXXALIAS_SYS (wcscmp, int, (const wchar_t *s1, const wchar_t *s2));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcscmp);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCMP
_GL_WARN_ON_USE (wcscmp, "wcscmp is unportable - "
                 "use gnulib module wcscmp for portability");
# endif
#endif


/* Compare no more than N wide characters of S1 and S2.  */
#if @GNULIB_WCSNCMP@
# if @REPLACE_WCSNCMP@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsncmp
#   define wcsncmp rpl_wcsncmp
#  endif
_GL_FUNCDECL_RPL (wcsncmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n),
                  _GL_ATTRIBUTE_PURE
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
_GL_CXXALIAS_RPL (wcsncmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n));
# else
#  if !@HAVE_WCSNCMP@
_GL_FUNCDECL_SYS (wcsncmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n),
                  _GL_ATTRIBUTE_PURE
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (1, 3)
                  _GL_ATTRIBUTE_NONNULL_IF_NONZERO (2, 3));
#  endif
_GL_CXXALIAS_SYS (wcsncmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsncmp);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSNCMP
_GL_WARN_ON_USE (wcsncmp, "wcsncmp is unportable - "
                 "use gnulib module wcsncmp for portability");
# endif
#endif


/* Compare S1 and S2, ignoring case.  */
#if @GNULIB_WCSCASECMP@
/* On Solaris 11.3, the header files declare the function in the std::
   namespace, not in the global namespace.  So, force a declaration in
   the global namespace.  */
# if !@HAVE_WCSCASECMP@ || (defined __sun && defined __cplusplus)
_GL_FUNCDECL_SYS (wcscasecmp, int, (const wchar_t *s1, const wchar_t *s2),
                                   _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (wcscasecmp, int, (const wchar_t *s1, const wchar_t *s2));
_GL_CXXALIASWARN (wcscasecmp);
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCASECMP
_GL_WARN_ON_USE (wcscasecmp, "wcscasecmp is unportable - "
                 "use gnulib module wcscasecmp for portability");
# endif
#endif


/* Compare no more than N chars of S1 and S2, ignoring case.  */
#if @GNULIB_WCSNCASECMP@
/* On Solaris 11.3, the header files declare the function in the std::
   namespace, not in the global namespace.  So, force a declaration in
   the global namespace.  */
# if !@HAVE_WCSNCASECMP@ || (defined __sun && defined __cplusplus)
_GL_FUNCDECL_SYS (wcsncasecmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n),
                  _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (wcsncasecmp, int,
                  (const wchar_t *s1, const wchar_t *s2, size_t n));
_GL_CXXALIASWARN (wcsncasecmp);
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSNCASECMP
_GL_WARN_ON_USE (wcsncasecmp, "wcsncasecmp is unportable - "
                 "use gnulib module wcsncasecmp for portability");
# endif
#endif


/* Compare S1 and S2, both interpreted as appropriate to the LC_COLLATE
   category of the current locale.  */
#if @GNULIB_WCSCOLL@
# if !@HAVE_WCSCOLL@
_GL_FUNCDECL_SYS (wcscoll, int, (const wchar_t *s1, const wchar_t *s2), );
# endif
_GL_CXXALIAS_SYS (wcscoll, int, (const wchar_t *s1, const wchar_t *s2));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcscoll);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCOLL
_GL_WARN_ON_USE (wcscoll, "wcscoll is unportable - "
                 "use gnulib module wcscoll for portability");
# endif
#endif


/* Transform S2 into array pointed to by S1 such that if wcscmp is applied
   to two transformed strings the result is the as applying 'wcscoll' to the
   original strings.  */
#if @GNULIB_WCSXFRM@
# if !@HAVE_WCSXFRM@
_GL_FUNCDECL_SYS (wcsxfrm, size_t,
                  (wchar_t *restrict s1, const wchar_t *restrict s2, size_t n), );
# endif
_GL_CXXALIAS_SYS (wcsxfrm, size_t,
                  (wchar_t *restrict s1, const wchar_t *restrict s2, size_t n));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsxfrm);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSXFRM
_GL_WARN_ON_USE (wcsxfrm, "wcsxfrm is unportable - "
                 "use gnulib module wcsxfrm for portability");
# endif
#endif


/* Duplicate S, returning an identical malloc'd string.  */
#if @GNULIB_WCSDUP@
# if defined _WIN32 && !defined __CYGWIN__
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsdup
#   define wcsdup _wcsdup
#  endif
_GL_CXXALIAS_MDA (wcsdup, wchar_t *, (const wchar_t *s));
# else
/* On Solaris 11.3, the header files declare the function in the std::
   namespace, not in the global namespace.  So, force a declaration in
   the global namespace.  */
#  if !@HAVE_WCSDUP@ || (defined __sun && defined __cplusplus) \
      || (__GNUC__ >= 11 && !defined __clang__)
#   if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
_GL_FUNCDECL_SYS (wcsdup, wchar_t *,
                  (const wchar_t *s),
                  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE)
                  _GL_ATTRIBUTE_NOTHROW;
#   else
_GL_FUNCDECL_SYS (wcsdup, wchar_t *,
                  (const wchar_t *s),
                  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE);
#   endif
#  endif
_GL_CXXALIAS_SYS (wcsdup, wchar_t *, (const wchar_t *s));
# endif
_GL_CXXALIASWARN (wcsdup);
#else
# if (__GNUC__ >= 11 && !defined __clang__) && !defined wcsdup
/* For -Wmismatched-dealloc: Associate wcsdup with free or rpl_free.  */
#  if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
_GL_FUNCDECL_SYS (wcsdup, wchar_t *,
                  (const wchar_t *s),
                  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE)
                  _GL_ATTRIBUTE_NOTHROW;
#  else
_GL_FUNCDECL_SYS (wcsdup, wchar_t *,
                  (const wchar_t *s),
                  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE);
#  endif
# endif
# if defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_WCSDUP
_GL_WARN_ON_USE (wcsdup, "wcsdup is unportable - "
                 "use gnulib module wcsdup for portability");
#  endif
# elif @GNULIB_MDA_WCSDUP@
/* On native Windows, map 'wcsdup' to '_wcsdup', so that -loldnames is not
   required.  In C++ with GNULIB_NAMESPACE, avoid differences between
   platforms by defining GNULIB_NAMESPACE::wcsdup always.  */
#  if defined _WIN32 && !defined __CYGWIN__
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef wcsdup
#    define wcsdup _wcsdup
#   endif
_GL_CXXALIAS_MDA (wcsdup, wchar_t *, (const wchar_t *s));
#  else
#   if __GLIBC__ + (__GLIBC_MINOR__ >= 2) > 2
_GL_FUNCDECL_SYS (wcsdup, wchar_t *,
                  (const wchar_t *s),
                  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE)
                  _GL_ATTRIBUTE_NOTHROW;
#   else
_GL_FUNCDECL_SYS (wcsdup, wchar_t *,
                  (const wchar_t *s),
                  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE);
#   endif
#   if @HAVE_DECL_WCSDUP@
_GL_CXXALIAS_SYS (wcsdup, wchar_t *, (const wchar_t *s));
#   endif
#  endif
#  if (defined _WIN32 && !defined __CYGWIN__) || @HAVE_DECL_WCSDUP@
_GL_CXXALIASWARN (wcsdup);
#  endif
# endif
#endif


/* Find the first occurrence of WC in WCS.  */
#if @GNULIB_WCSCHR@
# if !@HAVE_WCSCHR@
_GL_FUNCDECL_SYS (wcschr, wchar_t *, (const wchar_t *wcs, wchar_t wc),
                                     _GL_ATTRIBUTE_PURE);
# endif
  /* On some systems, this function is defined as an overloaded function:
       extern "C++" {
         const wchar_t * std::wcschr (const wchar_t *, wchar_t);
         wchar_t * std::wcschr (wchar_t *, wchar_t);
       }  */
_GL_CXXALIAS_SYS_CAST2 (wcschr,
                        wchar_t *, (const wchar_t *, wchar_t),
                        const wchar_t *, (const wchar_t *, wchar_t));
# if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 10) && !defined __UCLIBC__) \
     && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)) \
     && !defined __clang__
_GL_CXXALIASWARN1 (wcschr, wchar_t *, (wchar_t *wcs, wchar_t wc));
_GL_CXXALIASWARN1 (wcschr, const wchar_t *, (const wchar_t *wcs, wchar_t wc));
# elif __GLIBC__ >= 2 && !defined __CORRECT_ISO_CPP_WCHAR_H_PROTO
_GL_CXXALIASWARN (wcschr);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCHR
_GL_WARN_ON_USE (wcschr, "wcschr is unportable - "
                 "use gnulib module wcschr for portability");
# endif
#endif


/* Find the last occurrence of WC in WCS.  */
#if @GNULIB_WCSRCHR@
# if !@HAVE_WCSRCHR@
_GL_FUNCDECL_SYS (wcsrchr, wchar_t *, (const wchar_t *wcs, wchar_t wc),
                                      _GL_ATTRIBUTE_PURE);
# endif
  /* On some systems, this function is defined as an overloaded function:
       extern "C++" {
         const wchar_t * std::wcsrchr (const wchar_t *, wchar_t);
         wchar_t * std::wcsrchr (wchar_t *, wchar_t);
       }  */
_GL_CXXALIAS_SYS_CAST2 (wcsrchr,
                        wchar_t *, (const wchar_t *, wchar_t),
                        const wchar_t *, (const wchar_t *, wchar_t));
# if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 10) && !defined __UCLIBC__) \
     && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)) \
     && !defined __clang__
_GL_CXXALIASWARN1 (wcsrchr, wchar_t *, (wchar_t *wcs, wchar_t wc));
_GL_CXXALIASWARN1 (wcsrchr, const wchar_t *, (const wchar_t *wcs, wchar_t wc));
# elif __GLIBC__ >= 2 && !defined __CORRECT_ISO_CPP_WCHAR_H_PROTO
_GL_CXXALIASWARN (wcsrchr);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSRCHR
_GL_WARN_ON_USE (wcsrchr, "wcsrchr is unportable - "
                 "use gnulib module wcsrchr for portability");
# endif
#endif


/* Return the length of the initial segment of WCS which consists entirely
   of wide characters not in REJECT.  */
#if @GNULIB_WCSCSPN@
# if !@HAVE_WCSCSPN@
_GL_FUNCDECL_SYS (wcscspn, size_t, (const wchar_t *wcs, const wchar_t *reject),
                                   _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (wcscspn, size_t, (const wchar_t *wcs, const wchar_t *reject));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcscspn);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSCSPN
_GL_WARN_ON_USE (wcscspn, "wcscspn is unportable - "
                 "use gnulib module wcscspn for portability");
# endif
#endif


/* Return the length of the initial segment of WCS which consists entirely
   of wide characters in ACCEPT.  */
#if @GNULIB_WCSSPN@
# if !@HAVE_WCSSPN@
_GL_FUNCDECL_SYS (wcsspn, size_t, (const wchar_t *wcs, const wchar_t *accept),
                                  _GL_ATTRIBUTE_PURE);
# endif
_GL_CXXALIAS_SYS (wcsspn, size_t, (const wchar_t *wcs, const wchar_t *accept));
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsspn);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSSPN
_GL_WARN_ON_USE (wcsspn, "wcsspn is unportable - "
                 "use gnulib module wcsspn for portability");
# endif
#endif


/* Find the first occurrence in WCS of any character in ACCEPT.  */
#if @GNULIB_WCSPBRK@
# if !@HAVE_WCSPBRK@
_GL_FUNCDECL_SYS (wcspbrk, wchar_t *,
                  (const wchar_t *wcs, const wchar_t *accept),
                  _GL_ATTRIBUTE_PURE);
# endif
  /* On some systems, this function is defined as an overloaded function:
       extern "C++" {
         const wchar_t * std::wcspbrk (const wchar_t *, const wchar_t *);
         wchar_t * std::wcspbrk (wchar_t *, const wchar_t *);
       }  */
_GL_CXXALIAS_SYS_CAST2 (wcspbrk,
                        wchar_t *, (const wchar_t *, const wchar_t *),
                        const wchar_t *, (const wchar_t *, const wchar_t *));
# if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 10) && !defined __UCLIBC__) \
     && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)) \
     && !defined __clang__
_GL_CXXALIASWARN1 (wcspbrk, wchar_t *,
                   (wchar_t *wcs, const wchar_t *accept));
_GL_CXXALIASWARN1 (wcspbrk, const wchar_t *,
                   (const wchar_t *wcs, const wchar_t *accept));
# elif __GLIBC__ >= 2 && !defined __CORRECT_ISO_CPP_WCHAR_H_PROTO
_GL_CXXALIASWARN (wcspbrk);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSPBRK
_GL_WARN_ON_USE (wcspbrk, "wcspbrk is unportable - "
                 "use gnulib module wcspbrk for portability");
# endif
#endif


/* Find the first occurrence of NEEDLE in HAYSTACK.  */
#if @GNULIB_WCSSTR@
# if @REPLACE_WCSSTR@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsstr
#   define wcsstr rpl_wcsstr
#  endif
_GL_FUNCDECL_RPL (wcsstr, wchar_t *,
                  (const wchar_t *restrict haystack,
                   const wchar_t *restrict needle),
                  _GL_ATTRIBUTE_PURE);
_GL_CXXALIAS_RPL (wcsstr, wchar_t *,
                  (const wchar_t *restrict haystack,
                   const wchar_t *restrict needle));
# else
#  if !@HAVE_WCSSTR@
_GL_FUNCDECL_SYS (wcsstr, wchar_t *,
                  (const wchar_t *restrict haystack,
                   const wchar_t *restrict needle),
                  _GL_ATTRIBUTE_PURE);
#  endif
  /* On some systems, this function is defined as an overloaded function:
       extern "C++" {
         const wchar_t * std::wcsstr (const wchar_t *, const wchar_t *);
         wchar_t * std::wcsstr (wchar_t *, const wchar_t *);
       }  */
_GL_CXXALIAS_SYS_CAST2 (wcsstr,
                        wchar_t *,
                        (const wchar_t *restrict, const wchar_t *restrict),
                        const wchar_t *,
                        (const wchar_t *restrict, const wchar_t *restrict));
# endif
# if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 10) && !defined __UCLIBC__) \
     && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)) \
     && !defined __clang__
_GL_CXXALIASWARN1 (wcsstr, wchar_t *,
                   (wchar_t *restrict haystack,
                    const wchar_t *restrict needle));
_GL_CXXALIASWARN1 (wcsstr, const wchar_t *,
                   (const wchar_t *restrict haystack,
                    const wchar_t *restrict needle));
# elif __GLIBC__ >= 2 && !defined __CORRECT_ISO_CPP_WCHAR_H_PROTO
_GL_CXXALIASWARN (wcsstr);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSSTR
_GL_WARN_ON_USE (wcsstr, "wcsstr is unportable - "
                 "use gnulib module wcsstr for portability");
# endif
#endif


/* Divide WCS into tokens separated by characters in DELIM.  */
#if @GNULIB_WCSTOK@
# if @REPLACE_WCSTOK@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcstok
#   define wcstok rpl_wcstok
#  endif
_GL_FUNCDECL_RPL (wcstok, wchar_t *,
                  (wchar_t *restrict wcs, const wchar_t *restrict delim,
                   wchar_t **restrict ptr), );
_GL_CXXALIAS_RPL (wcstok, wchar_t *,
                  (wchar_t *restrict wcs, const wchar_t *restrict delim,
                   wchar_t **restrict ptr));
# else
#  if !@HAVE_WCSTOK@
_GL_FUNCDECL_SYS (wcstok, wchar_t *,
                  (wchar_t *restrict wcs, const wchar_t *restrict delim,
                   wchar_t **restrict ptr), );
#  endif
_GL_CXXALIAS_SYS (wcstok, wchar_t *,
                  (wchar_t *restrict wcs, const wchar_t *restrict delim,
                   wchar_t **restrict ptr));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcstok);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSTOK
_GL_WARN_ON_USE (wcstok, "wcstok is unportable - "
                 "use gnulib module wcstok for portability");
# endif
#endif


/* Determine number of column positions required for first N wide
   characters (or fewer if S ends before this) in S.  */
#if @GNULIB_WCSWIDTH@
# if @REPLACE_WCSWIDTH@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcswidth
#   define wcswidth rpl_wcswidth
#  endif
_GL_FUNCDECL_RPL (wcswidth, int, (const wchar_t *s, size_t n),
                                 _GL_ATTRIBUTE_PURE);
_GL_CXXALIAS_RPL (wcswidth, int, (const wchar_t *s, size_t n));
# else
#  if !@HAVE_WCSWIDTH@
_GL_FUNCDECL_SYS (wcswidth, int, (const wchar_t *s, size_t n),
                                 _GL_ATTRIBUTE_PURE);
#  endif
_GL_CXXALIAS_SYS (wcswidth, int, (const wchar_t *s, size_t n));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcswidth);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSWIDTH
_GL_WARN_ON_USE (wcswidth, "wcswidth is unportable - "
                 "use gnulib module wcswidth for portability");
# endif
#endif


/* Convert *TP to a date and time wide string.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/wcsftime.html>.  */
#if @GNULIB_WCSFTIME@
# if @REPLACE_WCSFTIME@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef wcsftime
#   define wcsftime rpl_wcsftime
#  endif
_GL_FUNCDECL_RPL (wcsftime, size_t,
                  (wchar_t *restrict __buf, size_t __bufsize,
                   const wchar_t *restrict __fmt,
                   const struct tm *restrict __tp),
                  _GL_ARG_NONNULL ((1, 3, 4)));
_GL_CXXALIAS_RPL (wcsftime, size_t,
                  (wchar_t *restrict __buf, size_t __bufsize,
                   const wchar_t *restrict __fmt,
                   const struct tm *restrict __tp));
# else
#  if !@HAVE_WCSFTIME@
_GL_FUNCDECL_SYS (wcsftime, size_t,
                  (wchar_t *restrict __buf, size_t __bufsize,
                   const wchar_t *restrict __fmt,
                   const struct tm *restrict __tp),
                  _GL_ARG_NONNULL ((1, 3, 4)));
#  endif
_GL_CXXALIAS_SYS (wcsftime, size_t,
                  (wchar_t *restrict __buf, size_t __bufsize,
                   const wchar_t *restrict __fmt,
                   const struct tm *restrict __tp));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (wcsftime);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_WCSFTIME
_GL_WARN_ON_USE (wcsftime, "wcsftime is unportable - "
                 "use gnulib module wcsftime for portability");
# endif
#endif


#if @GNULIB_WGETCWD@ && (defined _WIN32 && !defined __CYGWIN__)
/* Gets the name of the current working directory.
   (a) If BUF is non-NULL, it is assumed to have room for SIZE wide characters.
       This function stores the working directory (NUL-terminated) in BUF and
       returns BUF.
   (b) If BUF is NULL, an array is allocated with 'malloc'.  The array is SIZE
       wide characters long, unless SIZE == 0, in which case it is as big as
       necessary.
   If the directory couldn't be determined or SIZE was too small, this function
   returns NULL and sets errno.  For a directory of length LEN, SIZE should be
   >= LEN + 3 in case (a) or >= LEN + 1 in case (b).
   Possible errno values include:
     - ERANGE if SIZE is too small.
     - ENOMEM if the memory could no be allocated.  */
_GL_FUNCDECL_SYS (wgetcwd, wchar_t *, (wchar_t *buf, size_t size), );
#endif


#endif /* _@GUARD_PREFIX@_WCHAR_H */
#endif /* _@GUARD_PREFIX@_WCHAR_H */
#endif
