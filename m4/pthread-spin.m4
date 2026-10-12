# pthread-spin.m4
# serial 8
dnl Copyright (C) 2019-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_PTHREAD_SPIN],
[
  AC_REQUIRE([gl_PTHREAD_H_PART1])
  AC_REQUIRE([AC_CANONICAL_HOST])

  if { case "$host_os" in mingw* | windows*) true;; *) false;; esac; } \
     && test $gl_threads_api = windows; then
    dnl Choose function names that don't conflict with the mingw-w64 winpthreads
    dnl library.
    REPLACE_PTHREAD_SPIN_INIT=1
    REPLACE_PTHREAD_SPIN_LOCK=1
    REPLACE_PTHREAD_SPIN_TRYLOCK=1
    REPLACE_PTHREAD_SPIN_UNLOCK=1
    REPLACE_PTHREAD_SPIN_DESTROY=1
  else
    if test $HAVE_PTHREAD_H = 0 || test $HAVE_PTHREAD_SPINLOCK_T = 0; then
      HAVE_PTHREAD_SPIN_INIT=0
      HAVE_PTHREAD_SPIN_LOCK=0
      HAVE_PTHREAD_SPIN_TRYLOCK=0
      HAVE_PTHREAD_SPIN_UNLOCK=0
      HAVE_PTHREAD_SPIN_DESTROY=0
    else
      dnl Test whether the gnulib module 'threadlib' is in use.
      dnl Some packages like Emacs use --avoid=threadlib.
      dnl Write the symbol in such a way that it does not cause 'aclocal' to pick
      dnl the threadlib.m4 file that is installed in $PREFIX/share/aclocal/.
      m4_ifdef([gl_][THREADLIB], [
        AC_REQUIRE([gl_][THREADLIB])
        dnl Test whether the functions actually exist.
        dnl FreeBSD 5.2.1 declares them but does not define them.
        AC_CACHE_CHECK([for pthread_spin_init],
          [gl_cv_func_pthread_spin_init_in_LIBMULTITHREAD],
          [gl_saved_LIBS="$LIBS"
           LIBS="$LIBS $LIBMULTITHREAD"
           AC_LINK_IFELSE(
             [AC_LANG_PROGRAM(
                [[#include <pthread.h>
                ]],
                [[pthread_spinlock_t lock;
                  return pthread_spin_init (&lock, 0);
                ]])
             ],
             [gl_cv_func_pthread_spin_init_in_LIBMULTITHREAD=yes],
             [gl_cv_func_pthread_spin_init_in_LIBMULTITHREAD=no])
           LIBS="$gl_saved_LIBS"
          ])
        if test $gl_cv_func_pthread_spin_init_in_LIBMULTITHREAD != yes; then
          HAVE_PTHREAD_SPIN_INIT=0
          REPLACE_PTHREAD_SPIN_INIT=1
          HAVE_PTHREAD_SPIN_LOCK=0
          REPLACE_PTHREAD_SPIN_LOCK=1
          HAVE_PTHREAD_SPIN_TRYLOCK=0
          REPLACE_PTHREAD_SPIN_TRYLOCK=1
          HAVE_PTHREAD_SPIN_UNLOCK=0
          REPLACE_PTHREAD_SPIN_UNLOCK=1
          HAVE_PTHREAD_SPIN_DESTROY=0
          REPLACE_PTHREAD_SPIN_DESTROY=1
        fi
      ], [
        :
      ])
    fi
  fi
])
