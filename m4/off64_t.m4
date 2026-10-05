# off64_t.m4
# serial 1
dnl Copyright (C) 2024-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl Check whether <sys/types.h> defines the 'off64_t' type.
dnl Set HAVE_OFF64_T.

AC_DEFUN([gl_TYPE_OFF64_T],
[
  dnl Persuade glibc <sys/types.h>, <stdio.h>, <fcntl.h>, <unistd.h>, <aio.h>
  dnl to define off64_t.
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_CACHE_CHECK([for off64_t], [gl_cv_off64_t],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <sys/types.h>]],
          [[int x = sizeof (off64_t *) + sizeof (off64_t);
            return !x;]])],
       [gl_cv_off64_t=yes], [gl_cv_off64_t=no])])

  if test $gl_cv_off64_t != no; then
    HAVE_OFF64_T=1
  else
    HAVE_OFF64_T=0
  fi
  AC_SUBST([HAVE_OFF64_T])
])
