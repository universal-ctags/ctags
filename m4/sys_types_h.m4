# sys_types_h.m4 serial 13
dnl Copyright (C) 2011-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN_ONCE([gl_SYS_TYPES_H],
[
  AC_REQUIRE([gl_SYS_TYPES_H_DEFAULTS])

  dnl Use sane struct stat types in OpenVMS 8.2 and later.
  AC_DEFINE([_USE_STD_STAT], 1, [For standard stat data types on VMS.])

  gl_NEXT_HEADERS([sys/types.h])

  dnl Ensure the type pid_t gets defined.
  AC_REQUIRE([AC_TYPE_PID_T])

  dnl Ensure the type mode_t gets defined.
  AC_REQUIRE([AC_TYPE_MODE_T])

  dnl Whether to override the 'off_t' type.
  AC_REQUIRE([gl_TYPE_OFF_T])

  dnl Whether to override the 'dev_t' and 'ino_t' types.
  m4_ifdef([gl_WINDOWS_STAT_INODES], [
    AC_REQUIRE([gl_WINDOWS_STAT_INODES])
  ], [
    WINDOWS_STAT_INODES=0
  ])
  AC_SUBST([WINDOWS_STAT_INODES])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_SYS_TYPES_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_SYS_TYPE_H_MODULE_INDICATOR_DEFAULTS], [
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_SYS_TYPE_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_SYS_TYPES_H_DEFAULTS])
])

AC_DEFUN([gl_SYS_TYPES_H_DEFAULTS],
[
])

# This works around a buggy version in autoconf <= 2.69.
# See <https://lists.gnu.org/r/autoconf/2016-08/msg00014.html>
# The 2.70 version isn't quoted properly, so override it too.

m4_version_prereq([2.70.1], [], [

m4_undefine([AC_HEADER_MAJOR])
AC_DEFUN([AC_HEADER_MAJOR],
[AC_CHECK_HEADERS_ONCE([sys/types.h])
AC_CHECK_HEADER([sys/mkdev.h],
                [AC_DEFINE([MAJOR_IN_MKDEV], [1],
                           [Define to 1 if `major', `minor', and `makedev' are
                            declared in <mkdev.h>.])])
if test $ac_cv_header_sys_mkdev_h = no; then
  AC_CHECK_HEADER([sys/sysmacros.h],
                  [AC_DEFINE([MAJOR_IN_SYSMACROS], [1],
                             [Define to 1 if `major', `minor', and `makedev'
                              are declared in <sysmacros.h>.])])
fi
])# AC_HEADER_MAJOR

])
