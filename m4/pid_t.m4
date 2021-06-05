# pid_t.m4 serial 4
dnl Copyright (C) 2020-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# The following implementation works around a problem in autoconf <= 2.69.
m4_version_prereq([2.70], [], [

dnl Define pid_t if the headers don't define it.
AC_DEFUN([AC_TYPE_PID_T],
[
  AC_CHECK_TYPE([pid_t],
    [],
    [dnl On 64-bit native Windows, define it to the equivalent of 'intptr_t'
     dnl (= 'long long' = '__int64'), because that is the return type
     dnl of the _spawnv* functions
     dnl <https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/spawnvp-wspawnvp>
     dnl and the argument type of the _cwait function
     dnl <https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/cwait>.
     dnl Otherwise (on 32-bit Windows and on old Unix platforms), define it
     dnl to 'int'.
     AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
          #if defined _WIN64 && !defined __CYGWIN__
          LLP64
          #endif
          ]])
       ],
       [gl_pid_type='int'],
       [gl_pid_type='__int64'])
     AC_DEFINE_UNQUOTED([pid_t], [$gl_pid_type],
       [Define as a signed integer type capable of holding a process identifier.])
    ],
    [AC_INCLUDES_DEFAULT])
])

])# m4_version_prereq 2.70
