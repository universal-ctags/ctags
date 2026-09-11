# assert_h.m4
# serial 6
dnl Copyright (C) 2011-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Paul Eggert.

AC_DEFUN([gl_ASSERT_H],
[
  AC_CACHE_CHECK([for static_assert], [gl_cv_static_assert],
    [gl_saved_CFLAGS=$CFLAGS
     for gl_working in "yes, a keyword" "yes, an <assert.h> macro"; do
       AS_CASE([$gl_working],
         [*assert.h*], [CFLAGS="$gl_saved_CFLAGS -DINCLUDE_ASSERT_H"])
       AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#if defined __clang__ && __STDC_VERSION__ < 202311
               #pragma clang diagnostic error "-Wc2x-extensions"
               #pragma clang diagnostic error "-Wc++1z-extensions"
              #endif
              #ifdef INCLUDE_ASSERT_H
               #include <assert.h>
              #endif
              static_assert (2 + 2 == 4, "arithmetic does not work");
              static_assert (2 + 2 == 4);
            ]],
            [[
              static_assert (sizeof (char) == 1, "sizeof does not work");
              static_assert (sizeof (char) == 1);
            ]])
         ],
         [gl_cv_static_assert=$gl_working],
         [gl_cv_static_assert=no])
       CFLAGS=$gl_saved_CFLAGS
       test "$gl_cv_static_assert" != no && break
     done
    ])

  GL_GENERATE_ASSERT_H=false
  AS_CASE([$gl_cv_static_assert],
    [yes*keyword*],
      [AC_DEFINE([HAVE_C_STATIC_ASSERT], [1],
         [Define to 1 if the static_assert keyword works.])],
    [no],
      [GL_GENERATE_ASSERT_H=true
       gl_NEXT_HEADERS([assert.h])])

  dnl The "zz" puts this toward config.h's end, to avoid potential
  dnl collisions with other definitions.
  dnl Hardcode the known configuration results for GCC and clang, so that
  dnl a configuration made with the C compiler works also with the C++ compiler
  dnl and vice versa.
  dnl The seemingly redundant parentheses are necessary for MSVC 14.
  dnl #undef assert so that programs are not tempted to use it without
  dnl specifically including assert.h.
  dnl Break the #undef_s apart with a comment so that 'configure' does
  dnl not comment them out.
  AH_VERBATIM([zzstatic_assert],
[#if (!(defined __clang__ \
       ? (defined __cplusplus \
          ? __cplusplus >= 201703L \
          : __STDC_VERSION__ >= 202000L && __clang_major__ >= 16 \
            && !defined __sun) \
       : (defined __GNUC__ \
          ? (defined __cplusplus \
             ? __cplusplus >= 201103L && __GNUG__ >= 6 \
             : __STDC_VERSION__ >= 202000L && __GNUC__ >= 13 \
               && !defined __sun) \
          : defined HAVE_C_STATIC_ASSERT)) \
     && !defined assert \
     && (!defined __cplusplus \
         || (__cpp_static_assert < 201411 \
             && __GNUG__ < 6 && __clang_major__ < 6)))
 #include <assert.h>
 #undef/**/assert
 /* Solaris 11.4 <assert.h> defines static_assert as a macro with 2 arguments.
    We need it also to be invocable with a single argument.
    Haiku 2022 <assert.h> does not define static_assert at all.  */
 #if (__STDC_VERSION__ - 0 >= 201112L) && !defined __cplusplus
  #undef/**/static_assert
  #define static_assert _Static_assert
 #endif
#endif])
])
