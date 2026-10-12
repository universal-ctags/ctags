# time_h.m4
# serial 27
dnl Copyright (C) 2000-2001, 2003-2007, 2009-2026 Free Software Foundation,
dnl Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Configure a more-standard replacement for <time.h>.

# Written by Paul Eggert and Jim Meyering.

AC_DEFUN_ONCE([gl_TIME_H],
[
  dnl Ensure to expand the default settings once only, before all statements
  dnl that occur in other macros.
  AC_REQUIRE([gl_TIME_H_DEFAULTS])

  gl_NEXT_HEADERS([time.h])
  AC_REQUIRE([gl_CHECK_TYPE_STRUCT_TIMESPEC])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[
#include <time.h>
    ]], [
      asctime asctime_r ctime ctime_r gmtime_r localtime localtime_r mktime
      nanosleep strftime strptime time timegm timespec_get timespec_getres tzset
    ])

  AC_REQUIRE([AC_C_RESTRICT])

  AC_CACHE_CHECK([for TIME_UTC in <time.h>],
    [gl_cv_time_h_has_TIME_UTC],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <time.h>
          ]],
          [[static int x = TIME_UTC; x++;]])],
       [gl_cv_time_h_has_TIME_UTC=yes],
       [gl_cv_time_h_has_TIME_UTC=no])])
  if test $gl_cv_time_h_has_TIME_UTC = yes; then
    TIME_H_DEFINES_TIME_UTC=1
  else
    TIME_H_DEFINES_TIME_UTC=0
  fi
  AC_SUBST([TIME_H_DEFINES_TIME_UTC])
])

dnl Check whether 'struct timespec' is declared
dnl in time.h, sys/time.h, pthread.h, or unistd.h.

AC_DEFUN([gl_CHECK_TYPE_STRUCT_TIMESPEC],
[
  AC_CHECK_HEADERS_ONCE([sys/time.h])
  AC_CACHE_CHECK([for struct timespec in <time.h>],
    [gl_cv_sys_struct_timespec_in_time_h],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <time.h>
          ]],
          [[static struct timespec x; x.tv_sec = x.tv_nsec;]])],
       [gl_cv_sys_struct_timespec_in_time_h=yes],
       [gl_cv_sys_struct_timespec_in_time_h=no])])

  TIME_H_DEFINES_STRUCT_TIMESPEC=0
  SYS_TIME_H_DEFINES_STRUCT_TIMESPEC=0
  PTHREAD_H_DEFINES_STRUCT_TIMESPEC=0
  UNISTD_H_DEFINES_STRUCT_TIMESPEC=0
  if test $gl_cv_sys_struct_timespec_in_time_h = yes; then
    TIME_H_DEFINES_STRUCT_TIMESPEC=1
  else
    AC_CACHE_CHECK([for struct timespec in <sys/time.h>],
      [gl_cv_sys_struct_timespec_in_sys_time_h],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <sys/time.h>
            ]],
            [[static struct timespec x; x.tv_sec = x.tv_nsec;]])],
         [gl_cv_sys_struct_timespec_in_sys_time_h=yes],
         [gl_cv_sys_struct_timespec_in_sys_time_h=no])])
    if test $gl_cv_sys_struct_timespec_in_sys_time_h = yes; then
      SYS_TIME_H_DEFINES_STRUCT_TIMESPEC=1
    else
      AC_CACHE_CHECK([for struct timespec in <pthread.h>],
        [gl_cv_sys_struct_timespec_in_pthread_h],
        [AC_COMPILE_IFELSE(
           [AC_LANG_PROGRAM(
              [[#include <pthread.h>
              ]],
              [[static struct timespec x; x.tv_sec = x.tv_nsec;]])],
           [gl_cv_sys_struct_timespec_in_pthread_h=yes],
           [gl_cv_sys_struct_timespec_in_pthread_h=no])])
      if test $gl_cv_sys_struct_timespec_in_pthread_h = yes; then
        PTHREAD_H_DEFINES_STRUCT_TIMESPEC=1
      else
        AC_CACHE_CHECK([for struct timespec in <unistd.h>],
          [gl_cv_sys_struct_timespec_in_unistd_h],
          [AC_COMPILE_IFELSE(
             [AC_LANG_PROGRAM(
                [[#include <unistd.h>
                ]],
                [[static struct timespec x; x.tv_sec = x.tv_nsec;]])],
             [gl_cv_sys_struct_timespec_in_unistd_h=yes],
             [gl_cv_sys_struct_timespec_in_unistd_h=no])])
        if test $gl_cv_sys_struct_timespec_in_unistd_h = yes; then
          UNISTD_H_DEFINES_STRUCT_TIMESPEC=1
        fi
      fi
    fi
  fi
  AC_SUBST([TIME_H_DEFINES_STRUCT_TIMESPEC])
  AC_SUBST([SYS_TIME_H_DEFINES_STRUCT_TIMESPEC])
  AC_SUBST([PTHREAD_H_DEFINES_STRUCT_TIMESPEC])
  AC_SUBST([UNISTD_H_DEFINES_STRUCT_TIMESPEC])
])

# gl_TIME_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_TIME_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_TIME_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_TIME_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_TIME_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_CTIME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MKTIME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_LOCALTIME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_NANOSLEEP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STRFTIME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STRPTIME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TIME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TIMEGM])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TIMESPEC_GET])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TIMESPEC_GETRES])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TIME_R])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TIME_RZ])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TZNAME])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TZSET])
    dnl Support Microsoft deprecated alias function names by default.
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MDA_TZSET], [1])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_TIME_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_TIME_H_DEFAULTS])
])

AC_DEFUN([gl_TIME_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_DECL_LOCALTIME_R=1;               AC_SUBST([HAVE_DECL_LOCALTIME_R])
  HAVE_NANOSLEEP=1;                      AC_SUBST([HAVE_NANOSLEEP])
  HAVE_STRPTIME=1;                       AC_SUBST([HAVE_STRPTIME])
  HAVE_TIMEGM=1;                         AC_SUBST([HAVE_TIMEGM])
  HAVE_TIMESPEC_GET=1;                   AC_SUBST([HAVE_TIMESPEC_GET])
  HAVE_TIMESPEC_GETRES=1;                AC_SUBST([HAVE_TIMESPEC_GETRES])
  dnl Even GNU libc does not have timezone_t and tzalloc() yet.
  HAVE_TIMEZONE_T=0;                     AC_SUBST([HAVE_TIMEZONE_T])
  HAVE_TZALLOC=0;                        AC_SUBST([HAVE_TZALLOC])
  REPLACE_CTIME=0;                       AC_SUBST([REPLACE_CTIME])
  REPLACE_GMTIME=0;                      AC_SUBST([REPLACE_GMTIME])
  REPLACE_LOCALTIME=0;                   AC_SUBST([REPLACE_LOCALTIME])
  REPLACE_LOCALTIME_R=0;                 AC_SUBST([REPLACE_LOCALTIME_R])
  REPLACE_LOCALTIME_RZ=0;                AC_SUBST([REPLACE_LOCALTIME_RZ])
  REPLACE_MKTIME=0;                      AC_SUBST([REPLACE_MKTIME])
  REPLACE_MKTIME_Z=0;                    AC_SUBST([REPLACE_MKTIME_Z])
  REPLACE_NANOSLEEP=0;                   AC_SUBST([REPLACE_NANOSLEEP])
  REPLACE_STRFTIME=0;                    AC_SUBST([REPLACE_STRFTIME])
  REPLACE_TIME=0;                        AC_SUBST([REPLACE_TIME])
  REPLACE_TIMEGM=0;                      AC_SUBST([REPLACE_TIMEGM])
  REPLACE_TIMESPEC_GET=0;                AC_SUBST([REPLACE_TIMESPEC_GET])
  REPLACE_TIMESPEC_GETRES=0;             AC_SUBST([REPLACE_TIMESPEC_GETRES])
  REPLACE_TZSET=0;                       AC_SUBST([REPLACE_TZSET])
])
