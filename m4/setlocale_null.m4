# setlocale_null.m4
# serial 11
dnl Copyright (C) 2019-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_FUNC_SETLOCALE_NULL],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_PTHREADLIB])
  AC_CHECK_HEADERS_ONCE([threads.h])

  AC_CACHE_CHECK([whether setlocale (LC_ALL, NULL) is thread-safe],
    [gl_cv_func_setlocale_null_all_mtsafe],
    [case "$host_os" in
       # Guess no on musl libc, macOS, FreeBSD, NetBSD, OpenBSD, AIX, Haiku.
       *-musl* | midipix* | darwin* | freebsd* | midnightbsd* | netbsd* | openbsd* | aix* | haiku*)
         gl_cv_func_setlocale_null_all_mtsafe=no ;;
       # Guess no on Cygwin < 3.4.6.
       cygwin*)
         AC_EGREP_CPP([Lucky user],
           [
#if defined __CYGWIN__
 #include <cygwin/version.h>
 #if CYGWIN_VERSION_DLL_COMBINED >= CYGWIN_VERSION_DLL_MAKE_COMBINED (3004, 6)
  Lucky user
 #endif
#endif
          ],
          [gl_cv_func_setlocale_null_all_mtsafe=yes],
          [gl_cv_func_setlocale_null_all_mtsafe=no])
        ;;
       # Guess yes on glibc, HP-UX, Solaris, native Windows.
       *-gnu* | gnu* | hpux* | solaris* | mingw* | windows*)
         gl_cv_func_setlocale_null_all_mtsafe=yes ;;
       # If we don't know, obey --enable-cross-guesses.
       *)
         gl_cv_func_setlocale_null_all_mtsafe="$gl_cross_guess_normal" ;;
     esac
    ])
  dnl On platforms without multithreading, there is no issue.
  case "$host_os" in
    mingw* | windows*) ;;
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
    [Define to 1 if setlocale (LC_ALL, NULL) is thread-safe.])

  dnl This is about a single category (not LC_ALL).
  AC_CACHE_CHECK([whether setlocale (category, NULL) is thread-safe],
    [gl_cv_func_setlocale_null_one_mtsafe],
    [case "$host_os" in
       # Guess no on OpenBSD, AIX.
       openbsd* | aix*)
         gl_cv_func_setlocale_null_one_mtsafe=no ;;
       # Guess yes on glibc, musl libc, macOS, FreeBSD, NetBSD, HP-UX, Solaris, Haiku, Cygwin, native Windows.
       *-gnu* | gnu* | *-musl* | midipix* | darwin* | freebsd* | midnightbsd* | netbsd* | hpux* | solaris* | haiku* | cygwin* | mingw* | windows*)
         gl_cv_func_setlocale_null_one_mtsafe=yes ;;
       # If we don't know, obey --enable-cross-guesses.
       *)
         gl_cv_func_setlocale_null_one_mtsafe="$gl_cross_guess_normal" ;;
     esac
    ])
  dnl On platforms without multithreading, there is no issue.
  case "$host_os" in
    mingw* | windows*) ;;
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
    [Define to 1 if setlocale (category, NULL) is thread-safe.])

  dnl Determine link dependencies of lib/setlocale_null.c and lib/setlocale-lock.c.
  if test $SETLOCALE_NULL_ALL_MTSAFE = 0 || test $SETLOCALE_NULL_ONE_MTSAFE = 0; then
    case "$host_os" in
      mingw* | windows*)
        SETLOCALE_NULL_LIB=
        ;;
      *)
        gl_WEAK_SYMBOLS
        case "$gl_cv_have_weak" in
          *yes) SETLOCALE_NULL_LIB= ;;
          *)    SETLOCALE_NULL_LIB="$LIBPTHREAD" ;;
        esac
        ;;
    esac
  else
    SETLOCALE_NULL_LIB=
  fi
  dnl SETLOCALE_NULL_LIB is expected to be '-pthread' or '-lpthread' on AIX
  dnl with gcc or xlc, and empty otherwise.
  AC_SUBST([SETLOCALE_NULL_LIB])
  dnl For backward compatibility.
  LIB_SETLOCALE_NULL="$SETLOCALE_NULL_LIB"
  AC_SUBST([LIB_SETLOCALE_NULL])
])

# Prerequisites of lib/setlocale-lock.c.
AC_DEFUN([gl_PREREQ_SETLOCALE_LOCK],
[
  gl_VISIBILITY
])
