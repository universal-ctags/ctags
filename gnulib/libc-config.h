/* System definitions for code taken from the GNU C Library

   Copyright 2017-2021 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this program; if not, see
   <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

/* This is intended to be a good-enough substitute for glibc system
   macros like those defined in <sys/cdefs.h>, so that Gnulib code
   shared with glibc can do this as the first #include:

     #ifndef _LIBC
     # include <libc-config.h>
     #endif

   When compiled as part of glibc this is a no-op; when compiled as
   part of Gnulib this includes Gnulib's <config.h> and defines macros
   that glibc library code would normally assume.

   Note: This header file MUST NOT be included by public header files
   of Gnulib.  */

#include <config.h>

/* On glibc this includes <features.h> and <sys/cdefs.h> and #defines
   _FEATURES_H, __WORDSIZE, and __set_errno.  On FreeBSD 11 and
   DragonFlyBSD 5.9 it includes <sys/cdefs.h> which defines __nonnull.
   Elsewhere it is harmless.  */
#include <errno.h>

/* From glibc <errno.h>.  */
#ifndef __set_errno
# define __set_errno(val) (errno = (val))
#endif

/* From glibc <features.h>.  */

#ifndef __GNUC_PREREQ
# if defined __GNUC__ && defined __GNUC_MINOR__
#  define __GNUC_PREREQ(maj, min) ((maj) < __GNUC__ + ((min) <= __GNUC_MINOR__))
# else
#  define __GNUC_PREREQ(maj, min) 0
# endif
#endif

#ifndef __glibc_clang_prereq
# if defined __clang_major__ && defined __clang_minor__
#  ifdef __apple_build_version__
/* Apple for some reason renumbers __clang_major__ and __clang_minor__.
   Gnulib code uses only __glibc_clang_prereq (3, 5); map it to
   6000000 <= __apple_build_version__.  Support for other calls to
   __glibc_clang_prereq can be added here as needed.  */
#   define __glibc_clang_prereq(maj, min) \
      ((maj) == 3 && (min) == 5 ? 6000000 <= __apple_build_version__ : 0)
#  else
#   define __glibc_clang_prereq(maj, min) \
      ((maj) < __clang_major__ + ((min) <= __clang_minor__))
#  endif
# else
#  define __glibc_clang_prereq(maj, min) 0
# endif
#endif

#ifndef __attribute_nonnull__
/* <sys/cdefs.h> either does not exist, or is too old for Gnulib.
   Prepare to include <cdefs.h>, which is Gnulib's version of a
   more-recent glibc <sys/cdefs.h>.  */

/* Define _FEATURES_H so that <cdefs.h> does not include <features.h>.  */
# ifndef _FEATURES_H
#  define _FEATURES_H 1
# endif
/* Define __GNULIB_CDEFS so that <cdefs.h> does not attempt to include
   nonexistent files.  */
# define __GNULIB_CDEFS
/* Undef the macros unconditionally defined by our copy of glibc
   <sys/cdefs.h>, so that they do not clash with any system-defined
   versions.  */
# undef _SYS_CDEFS_H
# undef __ASMNAME
# undef __ASMNAME2
# undef __BEGIN_DECLS
# undef __CONCAT
# undef __END_DECLS
# undef __HAVE_GENERIC_SELECTION
# undef __LDBL_COMPAT
# undef __LDBL_REDIR
# undef __LDBL_REDIR1
# undef __LDBL_REDIR1_DECL
# undef __LDBL_REDIR1_NTH
# undef __LDBL_REDIR2_DECL
# undef __LDBL_REDIR_DECL
# undef __LDBL_REDIR_NTH
# undef __LEAF
# undef __LEAF_ATTR
# undef __NTH
# undef __NTHNL
# undef __REDIRECT
# undef __REDIRECT_LDBL
# undef __REDIRECT_NTH
# undef __REDIRECT_NTHNL
# undef __REDIRECT_NTH_LDBL
# undef __STRING
# undef __THROW
# undef __THROWNL
# undef __attr_access
# undef __attribute__
# undef __attribute_alloc_size__
# undef __attribute_artificial__
# undef __attribute_const__
# undef __attribute_deprecated__
# undef __attribute_deprecated_msg__
# undef __attribute_format_arg__
# undef __attribute_format_strfmon__
# undef __attribute_malloc__
# undef __attribute_noinline__
# undef __attribute_nonstring__
# undef __attribute_pure__
# undef __attribute_returns_twice__
# undef __attribute_used__
# undef __attribute_warn_unused_result__
# undef __bos
# undef __bos0
# undef __errordecl
# undef __extension__
# undef __extern_always_inline
# undef __extern_inline
# undef __flexarr
# undef __fortify_function
# undef __glibc_c99_flexarr_available
# undef __glibc_has_attribute
# undef __glibc_has_builtin
# undef __glibc_has_extension
# undef __glibc_macro_warning
# undef __glibc_macro_warning1
# undef __glibc_objsize
# undef __glibc_objsize0
# undef __glibc_unlikely
# undef __inline
# undef __ptr_t
# undef __restrict
# undef __restrict_arr
# undef __va_arg_pack
# undef __va_arg_pack_len
# undef __warnattr

/* Include our copy of glibc <sys/cdefs.h>.  */
# include <cdefs.h>

/* <cdefs.h> __inline is too pessimistic for non-GCC.  */
# undef __inline
# ifndef HAVE___INLINE
#  if 199901 <= __STDC_VERSION__ || defined inline
#   define __inline inline
#  else
#   define __inline
#  endif
# endif

#endif /* defined __glibc_likely */


/* A substitute for glibc <libc-symbols.h>, good enough for Gnulib.  */
#define attribute_hidden
#define libc_hidden_proto(name)
#define libc_hidden_def(name)
#define libc_hidden_weak(name)
#define libc_hidden_ver(local, name)
#define strong_alias(name, aliasname)
#define weak_alias(name, aliasname)

/* A substitute for glibc <shlib-compat.h>, good enough for Gnulib.  */
#define SHLIB_COMPAT(lib, introduced, obsoleted) 0
#define compat_symbol(lib, local, symbol, version) extern int dummy
#define versioned_symbol(lib, local, symbol, version) extern int dummy
