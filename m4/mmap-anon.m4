# mmap-anon.m4
# serial 15
dnl Copyright (C) 2005, 2007, 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Detect how mmap can be used to create anonymous (not file-backed) memory
# mappings.
# - On Linux, AIX, Solaris, Cygwin, Interix, Haiku, both MAP_ANONYMOUS and
#   MAP_ANON exist and have the same value.
# - On HP-UX, only MAP_ANONYMOUS exists.
# - On Mac OS X, FreeBSD, NetBSD, OpenBSD, Minix, only MAP_ANON exists.

AC_DEFUN_ONCE([gl_FUNC_MMAP_ANON],
[
  dnl Persuade glibc <sys/mman.h> to define MAP_ANONYMOUS.
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  # Check for mmap(). Don't use AC_FUNC_MMAP, because it checks too much: it
  # fails on HP-UX 11, because MAP_FIXED mappings do not work. But this is
  # irrelevant for anonymous mappings.
  # Instead, assume that mmap() exists if and only if <sys/mman.h> exists.
  # Code needs to tests HAVE_SYS_MMAN_H, not HAVE_MMAP.
  AC_CHECK_HEADERS_ONCE([sys/mman.h])

  # Try to allow MAP_ANONYMOUS.
  gl_have_mmap_anonymous=no
  if test $ac_cv_header_sys_mman_h = yes; then
    AC_MSG_CHECKING([for MAP_ANONYMOUS])
    AC_EGREP_CPP([I cannot identify this map], [
#include <sys/mman.h>
#ifdef MAP_ANONYMOUS
    I cannot identify this map
#endif
],
      [gl_have_mmap_anonymous=yes])
    if test $gl_have_mmap_anonymous != yes; then
      AC_EGREP_CPP([I cannot identify this map], [
#include <sys/mman.h>
#ifdef MAP_ANON
    I cannot identify this map
#endif
],
        [AC_DEFINE([MAP_ANONYMOUS], [MAP_ANON],
          [Define to a substitute value for mmap()'s MAP_ANONYMOUS flag.])
         gl_have_mmap_anonymous=yes])
    fi
    AC_MSG_RESULT([$gl_have_mmap_anonymous])
  fi
])
