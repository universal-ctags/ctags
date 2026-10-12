# c-bool.m4
# serial 3
dnl Copyright 2022-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Check for bool that conforms to C2023.

AC_DEFUN([gl_C_BOOL],
[
  AC_CACHE_CHECK([for bool, true, false], [gl_cv_c_bool],
    [AC_COMPILE_IFELSE(
       [AC_LANG_SOURCE([[
          #if true == false
           #error "true == false"
          #endif
          extern bool b;
          bool b = true == false;]])],
       [gl_cv_c_bool=yes],
       [gl_cv_c_bool=no])])
  if test "$gl_cv_c_bool" = yes; then
    AC_DEFINE([HAVE_C_BOOL], [1],
      [Define to 1 if bool, true and false work as per C2023.])
  fi

  AC_CHECK_HEADERS_ONCE([stdbool.h])

  dnl The "zz" puts this toward config.h's end, to avoid potential
  dnl collisions with other definitions.
  dnl If 'bool', 'true' and 'false' do not work, arrange for them to work.
  dnl Hardcode the known configuration results for GCC and clang, so that
  dnl a configuration made with the C compiler works also with the C++ compiler
  dnl and vice versa.
  dnl The seemingly redundant parentheses are necessary for MSVC 14.
  dnl "Arrange for them to work", in C, means including <stdbool.h> if it is
  dnl not already included.
  dnl However, if the preprocessor mistakenly treats 'true' as 0,
  dnl define it to a bool expression equal to 1; this is needed in
  dnl Sun C++ 5.11 (Oracle Solaris Studio 12.2, 2010) and older.
  AH_VERBATIM([zzbool],
[#if !(defined __cplusplus \
      ? 1 \
      : (defined __clang__ \
         ? __STDC_VERSION__ >= 202000L && __clang_major__ >= 15 \
         : (defined __GNUC__ \
            ? __STDC_VERSION__ >= 202000L && __GNUC__ >= 13 \
            : defined HAVE_C_BOOL)))
# if !defined __cplusplus && !defined __bool_true_false_are_defined
#  if HAVE_STDBOOL_H
#   include <stdbool.h>
#  else
#   if defined __SUNPRO_C
#    error "<stdbool.h> is not usable with this configuration. To make it usable, add -D_STDC_C99= to $CC."
#   else
#    error "<stdbool.h> does not exist on this platform. Use gnulib module 'stdbool-c99' instead of gnulib module 'stdbool'."
#   endif
#  endif
# endif
# if !true
#  define true (!false)
# endif
#endif])
])
