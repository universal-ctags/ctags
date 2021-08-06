# memchr.m4 serial 18
dnl Copyright (C) 2002-2004, 2009-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN_ONCE([gl_FUNC_MEMCHR],
[
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  dnl Check for prerequisites for memory fence checks.
  gl_FUNC_MMAP_ANON
  AC_CHECK_HEADERS_ONCE([sys/mman.h])
  AC_CHECK_FUNCS_ONCE([mprotect])

  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  # Detect platform-specific bugs in some versions of glibc:
  # memchr should not dereference anything with length 0
  #   https://bugzilla.redhat.com/show_bug.cgi?id=499689
  # memchr should not dereference overestimated length after a match
  #   https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=521737
  #   https://sourceware.org/bugzilla/show_bug.cgi?id=10162
  # memchr should cast the second argument to 'unsigned char'.
  #   This bug exists in Android 4.3.
  # Assume that memchr works on platforms that lack mprotect.
  AC_CACHE_CHECK([whether memchr works], [gl_cv_func_memchr_works],
    [AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <string.h>
#if HAVE_SYS_MMAN_H
# include <fcntl.h>
# include <unistd.h>
# include <sys/types.h>
# include <sys/mman.h>
# ifndef MAP_FILE
#  define MAP_FILE 0
# endif
#endif
]], [[
  int result = 0;
  char *fence = NULL;
#if HAVE_SYS_MMAN_H && HAVE_MPROTECT
# if HAVE_MAP_ANONYMOUS
  const int flags = MAP_ANONYMOUS | MAP_PRIVATE;
  const int fd = -1;
# else /* !HAVE_MAP_ANONYMOUS */
  const int flags = MAP_FILE | MAP_PRIVATE;
  int fd = open ("/dev/zero", O_RDONLY, 0666);
  if (fd >= 0)
# endif
    {
      int pagesize = getpagesize ();
      char *two_pages =
        (char *) mmap (NULL, 2 * pagesize, PROT_READ | PROT_WRITE,
                       flags, fd, 0);
      if (two_pages != (char *)(-1)
          && mprotect (two_pages + pagesize, pagesize, PROT_NONE) == 0)
        fence = two_pages + pagesize;
    }
#endif
  if (fence)
    {
      /* Test against bugs on glibc systems.  */
      if (memchr (fence, 0, 0))
        result |= 1;
      strcpy (fence - 9, "12345678");
      if (memchr (fence - 9, 0, 79) != fence - 1)
        result |= 2;
      if (memchr (fence - 1, 0, 3) != fence - 1)
        result |= 4;
      /* Test against bug on AIX 7.2.  */
      if (memchr (fence - 4, '6', 16) != fence - 4)
        result |= 8;
    }
  /* Test against bug on Android 4.3.  */
  {
    char input[3];
    input[0] = 'a';
    input[1] = 'b';
    input[2] = 'c';
    if (memchr (input, 0x789abc00 | 'b', 3) != input + 1)
      result |= 16;
  }
  return result;
]])],
       [gl_cv_func_memchr_works=yes],
       [gl_cv_func_memchr_works=no],
       [case "$host_os" in
                           # Guess no on Android.
          linux*-android*) gl_cv_func_memchr_works="guessing no" ;;
                           # Guess yes on native Windows.
          mingw*)          gl_cv_func_memchr_works="guessing yes" ;;
                           # If we don't know, obey --enable-cross-guesses.
          *)               gl_cv_func_memchr_works="$gl_cross_guess_normal" ;;
        esac
       ])
    ])
  case "$gl_cv_func_memchr_works" in
    *yes) ;;
    *) REPLACE_MEMCHR=1 ;;
  esac
])

# Prerequisites of lib/memchr.c.
AC_DEFUN([gl_PREREQ_MEMCHR], [
  AC_CHECK_HEADERS([bp-sym.h])
])
