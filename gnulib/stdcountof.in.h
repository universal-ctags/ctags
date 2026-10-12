/* Copyright 2025-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2025.  */

#ifndef _@GUARD_PREFIX@_STDCOUNTOF_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* The include_next requires a split double-inclusion guard.  */
#if (defined __cplusplus ? @CXX_HAVE_STDCOUNTOF_H@ : @HAVE_STDCOUNTOF_H@)
# @INCLUDE_NEXT@ @NEXT_STDCOUNTOF_H@
#else

#ifndef _@GUARD_PREFIX@_STDCOUNTOF_H
#define _@GUARD_PREFIX@_STDCOUNTOF_H

/* This file uses _GL_GNUC_PREREQ.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* Get size_t.  */
#include <stddef.h>

/* Returns the number of elements of the array A, as a value of type size_t.

   Example declarations of arrays:
     extern int a[];
     extern int a[10];
     static int a[10][20];
     void func () { int a[10]; ... }
   It works for arrays that are declared outside functions and for local
   variables of array type.  It does *not* work for function parameters
   of array type, because they are actually parameters of pointer type.
   In this case, i.e. if A is a pointer, e.g. in
     void func (int a[10]) { ... }
   this macro attempts to produce an error.
 */
#define countof(...) \
  ((size_t) (sizeof (__VA_ARGS__) / sizeof (__VA_ARGS__)[0] \
             + 0 * _gl_verify_is_array (__VA_ARGS__)))

/* Attempts to verify that A is an array.  */
#if defined __cplusplus
/* Borrowed from verify.h.  */
# if !GNULIB_defined_struct__gl_verify_type
template <int w>
  struct _gl_verify_type {
    unsigned int _gl_verify_error_if_negative: w;
  };
#  define GNULIB_defined_struct__gl_verify_type 1
# endif
# if __cplusplus >= 201103L
#  if 1
  /* Use decltype.  */
/* Default case.  */
template <typename T>
  struct _gl_array_type_test { static const int is_array = -1; };
/* Unbounded arrays.  */
template <typename T>
  struct _gl_array_type_test<T[]> { static const int is_array = 1; };
/* Bounded arrays.  */
template <typename T, size_t N>
  struct _gl_array_type_test<T[N]> { static const int is_array = 1; };
/* String literals.  */
template <typename T, size_t N>
  struct _gl_array_type_test<T const (&)[N]> { static const int is_array = 1; };
#   define _gl_verify_is_array(...) \
     sizeof (_gl_verify_type<_gl_array_type_test<decltype(__VA_ARGS__)>::is_array>)
#  else
  /* Use template argument deduction.
     Use sizeof to get a constant expression from an unknown type.
     Note: This approach does not work for countof (((int[]) { a, b, c })).  */
/* Default case.  */
template <typename T>
  struct _gl_array_type_test { double large; };
/* Unbounded arrays.  */
template <typename T>
  struct _gl_array_type_test<T[]> { char small; };
/* Bounded arrays.  */
template <typename T, size_t N>
  struct _gl_array_type_test<T[N]> { char small; };
/* The T& parameter is essential here: it prevents decay (array-to-pointer
   conversion).  */
template <typename T> _gl_array_type_test<T> _gl_array_type_test_helper(T&);
#   define _gl_verify_is_array(...) \
     sizeof (_gl_verify_type<(sizeof (_gl_array_type_test_helper(__VA_ARGS__)) < sizeof (double) ? 1 : -1)>)
#  endif
# else
/* The compiler does not have the necessary functionality.  */
#  define _gl_verify_is_array(...) 0
# endif
#else
/* In C, we can use typeof and __builtin_types_compatible_p.  */
/* Work around clang bug <https://github.com/llvm/llvm-project/issues/143284>.  */
# if (_GL_GNUC_PREREQ (3, 1) && ! defined __clang__ /* || defined __clang__ */) \
     && !(defined __STRICT_ANSI__ && __STDC_VERSION__ < 202311L) /* but not with -std=c99 or -std=c11 */
#  define _gl_verify_is_array(...) \
    sizeof (struct { unsigned int _gl_verify_error_if_negative : __builtin_types_compatible_p (typeof (__VA_ARGS__), typeof (&*(__VA_ARGS__))) ? -1 : 1; })
# else
/* The compiler does not have the necessary built-ins.  */
#  define _gl_verify_is_array(...) 0
# endif
#endif

#endif /* _@GUARD_PREFIX@_STDCOUNTOF_H */
#endif
#endif /* _@GUARD_PREFIX@_STDCOUNTOF_H */
