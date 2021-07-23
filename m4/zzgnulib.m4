# zzgnulib.m4 serial 1
dnl Copyright (C) 2020-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This file must be named something that sorts after all other
dnl package- or gnulib-provided .m4 files - at least for those packages
dnl that redefine AC_PROG_CC.

dnl Redefine AC_PROG_CC so that it ends with invocations of gl_COMPILER_CLANG
dnl and gl_COMPILER_PREPARE_CHECK_DECL.
m4_define([AC_PROG_CC],
  m4_defn([AC_PROG_CC])[
gl_COMPILER_CLANG
gl_COMPILER_PREPARE_CHECK_DECL
])

# gl_ZZGNULIB
# -----------
# Witness macro that this file has been included.  Needed to force
# Automake to include this file after all other gnulib .m4 files.
AC_DEFUN([gl_ZZGNULIB])
