# sched_h.m4
# serial 16
dnl Copyright (C) 2008-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl Written by Bruno Haible.

AC_DEFUN_ONCE([gl_SCHED_H],
[
  dnl Ensure to expand the default settings once only, before all statements
  dnl that occur in other macros.
  AC_REQUIRE([gl_SCHED_H_DEFAULTS])

  AC_REQUIRE([AC_CANONICAL_HOST])

  AC_REQUIRE([gl_CHECK_HEADER_SYS_CDEFS_H])

  AC_CHECK_HEADERS([sched.h], [], [],
    [[#if HAVE_SYS_CDEFS_H
       #include <sys/cdefs.h>
      #endif
    ]])
  gl_NEXT_HEADERS([sched.h])

  if test "$ac_cv_header_sched_h" = yes; then
    HAVE_SCHED_H=1
  else
    HAVE_SCHED_H=0
  fi
  AC_SUBST([HAVE_SCHED_H])

  if test "$HAVE_SCHED_H" = 1; then
    AC_CHECK_TYPE([struct sched_param],
      [HAVE_STRUCT_SCHED_PARAM=1], [HAVE_STRUCT_SCHED_PARAM=0],
      [[#if HAVE_SYS_CDEFS_H
         #include <sys/cdefs.h>
        #endif
        #include <sched.h>
      ]])
  else
    HAVE_STRUCT_SCHED_PARAM=0
    case "$host_os" in
      os2*)
        dnl On OS/2 kLIBC, struct sched_param is in spawn.h.
        AC_CHECK_TYPE([struct sched_param],
          [HAVE_STRUCT_SCHED_PARAM=1], [],
          [#include <spawn.h>])
        ;;
      vms)
        dnl On OpenVMS 7.2 or newer, struct sched_param is in pthread.h.
        AC_CHECK_TYPE([struct sched_param],
          [HAVE_STRUCT_SCHED_PARAM=1], [],
          [#include <pthread.h>])
        ;;
    esac
  fi
  AC_SUBST([HAVE_STRUCT_SCHED_PARAM])

  dnl Ensure the type pid_t gets defined.
  AC_REQUIRE([AC_TYPE_PID_T])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use, if it is not common
  dnl enough to be declared everywhere.
  gl_WARN_ON_USE_PREPARE([[#include <sched.h>
    ]], [sched_yield])
])

# gl_SCHED_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_SCHED_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_SCHED_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_SCHED_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_SCHED_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_SCHED_YIELD])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_SCHED_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_SCHED_H_DEFAULTS])
])

AC_DEFUN([gl_SCHED_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_SCHED_YIELD=1;    AC_SUBST([HAVE_SCHED_YIELD])
  REPLACE_SCHED_YIELD=0; AC_SUBST([REPLACE_SCHED_YIELD])
])
