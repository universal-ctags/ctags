# musl.m4
# serial 4
dnl Copyright (C) 2019-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Test for musl libc, despite the musl libc authors don't like it
# <https://wiki.musl-libc.org/faq.html>
# <https://lists.gnu.org/archive/html/bug-gnulib/2018-02/msg00079.html>.
# From Bruno Haible.

AC_DEFUN_ONCE([gl_MUSL_LIBC],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  case "$host_os" in
    *-musl* | midipix*)
      AC_DEFINE([MUSL_LIBC], [1], [Define to 1 on musl libc.])
      ;;
  esac
])
