dnl Check for __builtin_expect.

dnl Copyright 2016-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Paul Eggert.

AC_DEFUN([gl___BUILTIN_EXPECT],
[
  AC_CACHE_CHECK([for __builtin_expect],
    [gl_cv___builtin_expect],
    [AC_LINK_IFELSE(
       [AC_LANG_SOURCE([[
         int
         main (int argc, char **argv)
         {
           argc = __builtin_expect (argc, 100);
           return argv[argc != 100][0];
         }]])],
       [gl_cv___builtin_expect=yes],
       [AC_LINK_IFELSE(
          [AC_LANG_SOURCE([[
             #include <builtins.h>
             int
             main (int argc, char **argv)
             {
               argc = __builtin_expect (argc, 100);
               return argv[argc != 100][0];
             }]])],
          [gl_cv___builtin_expect="in <builtins.h>"],
          [gl_cv___builtin_expect=no])])])
  if test "$gl_cv___builtin_expect" = yes; then
    AC_DEFINE([HAVE___BUILTIN_EXPECT], [1])
  elif test "$gl_cv___builtin_expect" = "in <builtins.h>"; then
    AC_DEFINE([HAVE___BUILTIN_EXPECT], [2])
  fi
  AH_VERBATIM([HAVE___BUILTIN_EXPECT],
    [/* Define to 1 if the compiler supports __builtin_expect,
   and to 2 if <builtins.h> does.  */
#undef HAVE___BUILTIN_EXPECT
#ifndef HAVE___BUILTIN_EXPECT
# define __builtin_expect(e, c) (e)
#elif HAVE___BUILTIN_EXPECT == 2
# include <builtins.h>
#endif
    ])
])
