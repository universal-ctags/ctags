# strnlen.m4
# serial 15
dnl Copyright (C) 2002-2003, 2005-2007, 2009-2026 Free Software Foundation,
dnl Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

m4_version_prereq([2.73], [], [
# Replace AC_FUNC_STRNLEN from Autoconf 2.72 and earlier,
# which does not check for Android strnlen bugs.

AC_DEFUN([AC_FUNC_STRNLEN],
[AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])dnl
AC_CACHE_CHECK([for working strnlen], [ac_cv_func_strnlen_working],
[AC_RUN_IFELSE(
   [AC_LANG_PROGRAM(
      [AC_INCLUDES_DEFAULT
       [/* Use pstrnlen to test; 'volatile' prevents the compiler
           from optimizing the strnlen calls away.  */
        size_t (*volatile pstrnlen) (char const *, size_t) = strnlen;
        char const s[] = "foobar";
        int s_len = sizeof s - 1;
       ]],
      [[
        /* AIX 4.3 is buggy: strnlen (S, 1) == 3.  */
        int i;
        for (i = 0; i < s_len + 1; ++i)
          {
            int expected = i <= s_len ? i : s_len;
            if (pstrnlen (s, i) != expected)
              return 1;
          }

        /* Android 5.0 (API 21) strnlen ("", SIZE_MAX) incorrectly crashes.  */
        if (pstrnlen ("", -1) != 0)
          return 1;]])],
   [ac_cv_func_strnlen_working=yes],
   [ac_cv_func_strnlen_working=no],
   [AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT],
         [[#if defined _AIX && !defined _AIX51
            #error "AIX pre 5.1 is buggy"
           #endif
           #ifdef __ANDROID__
            #include <android/api-level.h>
            #if __ANDROID_API__ < 22
             #error "Android API < 22 is buggy"
            #endif
           #endif
         ]])],
      [ac_cv_func_strnlen_working=yes],
      [ac_cv_func_strnlen_working=no])])])
test $ac_cv_func_strnlen_working = no && AC_LIBOBJ([strnlen])
])# AC_FUNC_STRNLEN
])

AC_DEFUN([gl_FUNC_STRNLEN],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])

  dnl Persuade glibc <string.h> to declare strnlen().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_CHECK_DECLS_ONCE([strnlen])
  if test $ac_cv_have_decl_strnlen = no; then
    HAVE_DECL_STRNLEN=0
  else
    m4_pushdef([AC_LIBOBJ], [:])
    dnl Note: AC_FUNC_STRNLEN does AC_LIBOBJ([strnlen]).
    AC_FUNC_STRNLEN
    m4_popdef([AC_LIBOBJ])
    if test $ac_cv_func_strnlen_working = no; then
      REPLACE_STRNLEN=1
    fi
  fi
])

# Prerequisites of lib/strnlen.c.
AC_DEFUN([gl_PREREQ_STRNLEN], [:])
