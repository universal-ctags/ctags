# strncpy.m4
# serial 1
dnl Copyright (C) 2002-2004, 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_FUNC_STRNCPY],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  dnl Check for prerequisites for memory fence checks.
  gl_FUNC_MMAP_ANON
  AC_CHECK_HEADERS_ONCE([sys/mman.h])
  AC_CHECK_FUNCS_ONCE([mprotect])

  dnl Detect bug in FreeBSD 15.0 on x86_64:
  dnl strncpy should not dereference more than n bytes, but always dereferences
  dnl n+1 bytes if the first n bytes don't contain a NUL byte.
  dnl Assume that strncpy works on platforms that lack mprotect.
  AC_CACHE_CHECK([whether strncpy works], [gl_cv_func_strncpy_works],
    [AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <string.h>
#if HAVE_SYS_MMAN_H
# include <fcntl.h>
# include <unistd.h>
# include <sys/types.h>
# include <sys/mman.h>
#endif
]GL_MDA_DEFINES],
[[
  char *fence = NULL;
#if HAVE_SYS_MMAN_H && HAVE_MPROTECT
  {
    long int pagesize = sysconf (_SC_PAGESIZE);
    char *two_pages =
      (char *) mmap (NULL, 2 * pagesize, PROT_READ | PROT_WRITE,
                     MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (two_pages != (char *)(-1)
        && mprotect (two_pages + pagesize, pagesize, PROT_NONE) == 0)
      fence = two_pages + pagesize;
  }
#endif
  if (fence)
    {
      char dest[8];

      dest[0] = 'a';
      dest[1] = 'b';
      dest[2] = 'c';
      dest[3] = 'd';
      dest[4] = 'e';
      dest[5] = 'f';
      dest[6] = 'g';

      *(fence - 3) = '7';
      *(fence - 2) = '2';
      *(fence - 1) = '9';

      if (strncpy (dest + 1, fence - 3, 3) != dest + 1)
        return 1;
      if (dest[0] != 'a')
        return 2;
      if (dest[1] != '7' || dest[2] != '2' || dest[3] != '9')
        return 3;
      if (dest[4] != 'e')
        return 4;
    }
  return 0;
]])], [gl_cv_func_strncpy_works=yes], [gl_cv_func_strncpy_works=no],
       [
        case "$host_os" in
                                 # Guess no on FreeBSD.
          freebsd* | dragonfly*) gl_cv_func_strncpy_works="guessing no" ;;
                                 # Guess yes on native Windows.
          mingw* | windows*)     gl_cv_func_strncpy_works="guessing yes" ;;
                                 # Guess yes otherwise.
          *)                     gl_cv_func_strncpy_works="guessing yes" ;;
        esac
       ])
    ])
  case "$gl_cv_func_strncpy_works" in
    *yes) ;;
    *) REPLACE_STRNCPY=1 ;;
  esac
])

# Prerequisites of lib/strncpy.c.
AC_DEFUN([gl_PREREQ_STRNCPY], [
  :
])
