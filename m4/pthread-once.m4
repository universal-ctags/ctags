# pthread-once.m4
# serial 6
dnl Copyright (C) 2019-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_PTHREAD_ONCE],
[
  AC_REQUIRE([gl_PTHREAD_H])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_PTHREADLIB])

  if { case "$host_os" in mingw* | windows*) true;; *) false;; esac; } \
     && test $gl_threads_api = windows; then
    dnl Choose function names that don't conflict with the mingw-w64 winpthreads
    dnl library.
    REPLACE_PTHREAD_ONCE=1
    PTHREAD_ONCE_LIB=
  else
    if test $HAVE_PTHREAD_H = 0; then
      HAVE_PTHREAD_ONCE=0
      PTHREAD_ONCE_LIB=
    else
      dnl Work around Cygwin 3.5.3 bug.
      AC_CACHE_CHECK([whether pthread_once works],
        [gl_cv_func_pthread_once_works],
        [case "$host_os" in
           cygwin*) gl_cv_func_pthread_once_works="guessing no" ;;
           *)       gl_cv_func_pthread_once_works="yes" ;;
         esac
        ])
      case "$gl_cv_func_pthread_once_works" in
        *yes) ;;
        *) REPLACE_PTHREAD_ONCE=1 ;;
      esac
      dnl Determine whether linking requires $(LIBPMULTITHREAD) or only
      dnl $(LIBPTHREAD).
      if test -z "$LIBPTHREAD" && test -n "$LIBPMULTITHREAD"; then
        AC_CACHE_CHECK([whether pthread_once can be used without linking with libpthread],
          [gl_cv_func_pthread_once_no_lib],
          [AC_RUN_IFELSE(
             [AC_LANG_PROGRAM(
                [[#include <pthread.h>
                  static pthread_once_t a_once = PTHREAD_ONCE_INIT;
                  static int a;
                  static void a_init (void) { a = 8647; }
                ]],
                [[if (pthread_once (&a_once, a_init)) return 1;
                  if (a != 8647) return 2;
                  return 0;
                ]])],
             [gl_cv_func_pthread_once_no_lib=yes],
             [gl_cv_func_pthread_once_no_lib=no],
             [case "$host_os" in
                # Guess no on glibc.
                *-gnu* | gnu*)
                  gl_cv_func_pthread_once_no_lib="guessing no" ;;
                # Guess no on FreeBSD.
                freebsd* | dragonfly* | midnightbsd*)
                  gl_cv_func_pthread_once_no_lib="guessing no" ;;
                # Guess yes otherwise.
                *)
                  gl_cv_func_pthread_once_no_lib="guessing yes" ;;
              esac
             ])
          ])
        case "$gl_cv_func_pthread_once_no_lib" in
          *yes) PTHREAD_ONCE_LIB="$LIBPTHREAD" ;;
          *)    PTHREAD_ONCE_LIB="$LIBPMULTITHREAD" ;;
        esac
        dnl Expected result:
        dnl PTHREAD_ONCE_LIB is $(LIBPMULTITHREAD) on glibc < 2.34, FreeBSD.
        dnl PTHREAD_ONCE_LIB is $(LIBPTHREAD) in particular on
        dnl   musl libc, macOS, NetBSD, Solaris, Cygwin, Haiku, Android.
      else
        PTHREAD_ONCE_LIB="$LIBPTHREAD"
      fi
    fi
  fi
  AC_SUBST([PTHREAD_ONCE_LIB])
])
