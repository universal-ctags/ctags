# uninorm_h.m4
# serial 1
dnl Copyright (C) 2023-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_UNINORM_H],
[
  dnl Ensure to expand the default settings once only, before all statements
  dnl that occur in other macros.
  AC_REQUIRE([gl_UNINORM_H_DEFAULTS])
])

# gl_UNINORM_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_UNINORM_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_UNINORM_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_UNINORM_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_UNINORM_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_UNINORM_NFD_DLL_VARIABLE], ['LIBUNISTRING_DLL_VARIABLE'])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_UNINORM_NFC_DLL_VARIABLE], ['LIBUNISTRING_DLL_VARIABLE'])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_UNINORM_NFKD_DLL_VARIABLE], ['LIBUNISTRING_DLL_VARIABLE'])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_UNINORM_NFKC_DLL_VARIABLE], ['LIBUNISTRING_DLL_VARIABLE'])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_UNINORM_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_UNINORM_H_DEFAULTS])
])

AC_DEFUN([gl_UNINORM_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
])
