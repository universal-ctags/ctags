# sys_cdefs_h.m4 - Is <sys/cdefs.h> compatible enough with glibc?
# serial 2
dnl Copyright 2024-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl Written by Paul Eggert.

AC_DEFUN_ONCE([gl_CHECK_HEADER_SYS_CDEFS_H],
  [AC_CACHE_CHECK([for glibc-compatible sys/cdefs.h],
     [gl_cv_header_sys_cdefs_h],
     [AC_COMPILE_IFELSE(
        [AC_LANG_DEFINES_PROVIDED
         [#include <sys/cdefs.h>
          enum { foo = __GNUC_PREREQ (14, 1) } bar;
        ]],
        [gl_cv_header_sys_cdefs_h=yes],
        [gl_cv_header_sys_cdefs_h=no])])
   if test "$gl_cv_header_sys_cdefs_h" = yes; then
     HAVE_SYS_CDEFS_H=1
   else
     HAVE_SYS_CDEFS_H=0
   fi
   AC_SUBST([HAVE_SYS_CDEFS_H])])
