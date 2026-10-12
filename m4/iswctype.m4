# iswctype.m4
# serial 3
dnl Copyright (C) 2011-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_ISWCTYPE],
[
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
  AC_REQUIRE([gl_WCTYPE_H])
  dnl Determine REPLACE_WCTYPE.
  AC_REQUIRE([gl_FUNC_WCTYPE])
])
