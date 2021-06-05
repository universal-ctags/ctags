# Test for __inline keyword
dnl Copyright 2017-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl___INLINE],
[
  AC_CACHE_CHECK([whether the compiler supports the __inline keyword],
    [gl_cv_c___inline],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
         [[typedef int foo_t;
           static __inline foo_t foo (void) { return 0; }]],
         [[return foo ();]])],
       [gl_cv_c___inline=yes],
       [gl_cv_c___inline=no])])
  if test $gl_cv_c___inline = yes; then
    AC_DEFINE([HAVE___INLINE], [1],
      [Define to 1 if the compiler supports the keyword '__inline'.])
  fi
])
