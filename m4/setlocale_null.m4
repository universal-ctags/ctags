# setlocale_null.m4 serial 5
dnl Copyright (C) 2019-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_SETLOCALE_NULL],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_PTHREADLIB])
  AC_CHECK_HEADERS_ONCE([threads.h])

  AC_CACHE_CHECK([whether setlocale (LC_ALL, NULL) is multithread-safe],
    [gl_cv_func_setlocale_null_all_mtsafe],
    [case "$host_os" in
       # Guess no on musl libc, macOS, FreeBSD, NetBSD, OpenBSD, AIX, Haiku, Cygwin.
       *-musl* | darwin* | freebsd* | midnightbsd* | netbsd* | openbsd* | aix* | haiku* | cygwin*)
         gl_cv_func_setlocale_null_all_mtsafe=no ;;
       # Guess yes on glibc, HP-UX, IRIX, Solaris, native Windows.
       *-gnu* | gnu* | hpux* | irix* | solaris* | mingw*)
         gl_cv_func_setlocale_null_all_mtsafe=yes ;;
       # If we don't know, obey --enable-cross-guesses.
       *)
         gl_cv_func_setlocale_null_all_mtsafe="$gl_cross_guess_normal" ;;
     esac
    ])
  dnl On platforms without multithreading, there is no issue.
  case "$host_os" in
    mingw*) ;;
    *)
      if test $gl_pthread_api = no && test $ac_cv_header_threads_h = no; then
        gl_cv_func_setlocale_null_all_mtsafe="trivially yes"
      fi
      ;;
  esac
  case "$gl_cv_func_setlocale_null_all_mtsafe" in
    *yes) SETLOCALE_NULL_ALL_MTSAFE=1 ;;
    *)    SETLOCALE_NULL_ALL_MTSAFE=0 ;;
  esac
  AC_DEFINE_UNQUOTED([SETLOCALE_NULL_ALL_MTSAFE], [$SETLOCALE_NULL_ALL_MTSAFE],
    [Define to 1 if setlocale (LC_ALL, NULL) is multithread-safe.])

  dnl This is about a single category (not LC_ALL).
  AC_CACHE_CHECK([whether setlocale (category, NULL) is multithread-safe],
    [gl_cv_func_setlocale_null_one_mtsafe],
    [case "$host_os" in
       # Guess no on OpenBSD, AIX.
       openbsd* | aix*)
         gl_cv_func_setlocale_null_one_mtsafe=no ;;
       # Guess yes on glibc, musl libc, macOS, FreeBSD, NetBSD, HP-UX, IRIX, Solaris, Haiku, Cygwin, native Windows.
       *-gnu* | gnu* | *-musl* | darwin* | freebsd* | midnightbsd* | netbsd* | hpux* | irix* | solaris* | haiku* | cygwin* | mingw*)
         gl_cv_func_setlocale_null_one_mtsafe=yes ;;
       # If we don't know, obey --enable-cross-guesses.
       *)
         gl_cv_func_setlocale_null_one_mtsafe="$gl_cross_guess_normal" ;;
     esac
    ])
  dnl On platforms without multithreading, there is no issue.
  case "$host_os" in
    mingw*) ;;
    *)
      if test $gl_pthread_api = no && test $ac_cv_header_threads_h = no; then
        gl_cv_func_setlocale_null_one_mtsafe="trivially yes"
      fi
      ;;
  esac
  case "$gl_cv_func_setlocale_null_one_mtsafe" in
    *yes) SETLOCALE_NULL_ONE_MTSAFE=1 ;;
    *)    SETLOCALE_NULL_ONE_MTSAFE=0 ;;
  esac
  AC_DEFINE_UNQUOTED([SETLOCALE_NULL_ONE_MTSAFE], [$SETLOCALE_NULL_ONE_MTSAFE],
    [Define to 1 if setlocale (category, NULL) is multithread-safe.])

  dnl Determine link dependencies of lib/setlocale_null.c and lib/setlocale-lock.c.
  if test $SETLOCALE_NULL_ALL_MTSAFE = 0 || test $SETLOCALE_NULL_ONE_MTSAFE = 0; then
    case "$host_os" in
      mingw*) LIB_SETLOCALE_NULL= ;;
      *)
        gl_WEAK_SYMBOLS
        case "$gl_cv_have_weak" in
          *yes) LIB_SETLOCALE_NULL= ;;
          *)    LIB_SETLOCALE_NULL="$LIBPTHREAD" ;;
        esac
        ;;
    esac
  else
    LIB_SETLOCALE_NULL=
  fi
  dnl LIB_SETLOCALE_NULL is expected to be '-pthread' or '-lpthread' on AIX
  dnl with gcc or xlc, and empty otherwise.
  AC_SUBST([LIB_SETLOCALE_NULL])
])

# Prerequisites of lib/setlocale-lock.c.
AC_DEFUN([gl_PREREQ_SETLOCALE_LOCK],
[
  gl_VISIBILITY
])
