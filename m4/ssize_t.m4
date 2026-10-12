# ssize_t.m4
# serial 6
dnl Copyright (C) 2001-2003, 2006, 2010-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Bruno Haible.
dnl Define ssize_t if it does not already exist.

AC_DEFUN([gt_TYPE_SSIZE_T],
[
  AC_CACHE_CHECK([for ssize_t], [gl_cv_ssize_t],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <sys/types.h>]],
          [[int x = sizeof (ssize_t *) + sizeof (ssize_t);
            return !x;]])],
       [gl_cv_ssize_t=yes], [gl_cv_ssize_t=no])])
  if test $gl_cv_ssize_t = no; then
    dnl On 64-bit native Windows, ssize_t needs to be defined as 'long long',
    dnl for consistency with the 64-bit size_t.
    AC_CACHE_CHECK([whether size_t is wider than 'long'], [gl_cv_size_t_large],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <sys/types.h>
              typedef int array [2 * (sizeof (size_t) > sizeof (long)) - 1];
            ]])],
         [gl_cv_size_t_large=yes], [gl_cv_size_t_large=no])])
    if test $gl_cv_size_t_large = yes; then
      gl_def_ssize_t='long long'
    else
      gl_def_ssize_t='long'
    fi
    AC_DEFINE_UNQUOTED([ssize_t], [$gl_def_ssize_t],
                       [Define as a signed type of the same size as size_t.])
  fi
])
