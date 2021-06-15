# wctype_h.m4 serial 30

dnl A placeholder for ISO C99 <wctype.h>, for platforms that lack it.

dnl Copyright (C) 2006-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Paul Eggert.

AC_DEFUN_ONCE([gl_WCTYPE_H],
[
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_CHECK_FUNCS_ONCE([iswcntrl])
  if test $ac_cv_func_iswcntrl = yes; then
    HAVE_ISWCNTRL=1
  else
    HAVE_ISWCNTRL=0
  fi
  AC_SUBST([HAVE_ISWCNTRL])

  AC_REQUIRE([gt_TYPE_WINT_T])
  if test $gt_cv_c_wint_t = yes; then
    HAVE_WINT_T=1
  else
    HAVE_WINT_T=0
  fi
  AC_SUBST([HAVE_WINT_T])

  AC_REQUIRE([gl_TYPE_WINT_T_PREREQ])

  gl_CHECK_NEXT_HEADERS([wctype.h])
  if test $ac_cv_header_wctype_h = yes; then
    if test $ac_cv_func_iswcntrl = yes; then
      dnl Linux libc5 has an iswprint function that returns 0 for all arguments.
      dnl The other functions are likely broken in the same way.
      AC_CACHE_CHECK([whether iswcntrl works], [gl_cv_func_iswcntrl_works],
        [
          AC_RUN_IFELSE(
            [AC_LANG_SOURCE([[
               #include <wchar.h>
               #include <wctype.h>
               int main () { return iswprint ('x') == 0; }
            ]])],
            [gl_cv_func_iswcntrl_works=yes], [gl_cv_func_iswcntrl_works=no],
            [dnl Guess no on Linux libc5, yes otherwise.
             AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdlib.h>
                          #if __GNU_LIBRARY__ == 1
                          Linux libc5 i18n is broken.
                          #endif]], [[]])],
              [gl_cv_func_iswcntrl_works="guessing yes"],
              [gl_cv_func_iswcntrl_works="guessing no"])
            ])
        ])
    fi
    HAVE_WCTYPE_H=1
  else
    HAVE_WCTYPE_H=0
  fi
  AC_SUBST([HAVE_WCTYPE_H])

  if test $GNULIBHEADERS_OVERRIDE_WINT_T = 1; then
    REPLACE_ISWCNTRL=1
  else
    case "$gl_cv_func_iswcntrl_works" in
      *yes) REPLACE_ISWCNTRL=0 ;;
      *)    REPLACE_ISWCNTRL=1 ;;
    esac
  fi
  AC_SUBST([REPLACE_ISWCNTRL])

  if test $HAVE_ISWCNTRL = 0 || test $REPLACE_ISWCNTRL = 1; then
    dnl Redefine all of iswcntrl, ..., iswxdigit in <wctype.h>.
    :
  fi

  if test $REPLACE_ISWCNTRL = 1; then
    REPLACE_TOWLOWER=1
  else
    AC_CHECK_FUNCS([towlower])
    if test $ac_cv_func_towlower = yes; then
      REPLACE_TOWLOWER=0
    else
      AC_CHECK_DECLS([towlower],,,
        [[#include <wchar.h>
          #if HAVE_WCTYPE_H
          # include <wctype.h>
          #endif
        ]])
      if test $ac_cv_have_decl_towlower = yes; then
        dnl On Minix 3.1.8, the system's <wctype.h> declares towlower() and
        dnl towupper() although it does not have the functions. Avoid a
        dnl collision with gnulib's replacement.
        REPLACE_TOWLOWER=1
      else
        REPLACE_TOWLOWER=0
      fi
    fi
  fi
  AC_SUBST([REPLACE_TOWLOWER])

  if test $HAVE_ISWCNTRL = 0 || test $REPLACE_TOWLOWER = 1; then
    dnl Redefine towlower, towupper in <wctype.h>.
    :
  fi

  dnl We assume that the wctype() and iswctype() functions exist if and only
  dnl if the type wctype_t is defined in <wchar.h> or in <wctype.h> if that
  dnl exists.
  dnl HP-UX 11.00 declares all these in <wchar.h> and lacks <wctype.h>.
  AC_CACHE_CHECK([for wctype_t], [gl_cv_type_wctype_t],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <wchar.h>
            #if HAVE_WCTYPE_H
            # include <wctype.h>
            #endif
            wctype_t a;
          ]],
          [[]])],
       [gl_cv_type_wctype_t=yes],
       [gl_cv_type_wctype_t=no])
    ])
  if test $gl_cv_type_wctype_t = no; then
    HAVE_WCTYPE_T=0
  fi

  dnl We assume that the wctrans() and towctrans() functions exist if and only
  dnl if the type wctrans_t is defined in <wctype.h>.
  AC_CACHE_CHECK([for wctrans_t], [gl_cv_type_wctrans_t],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <wchar.h>
            #include <wctype.h>
            wctrans_t a;
          ]],
          [[]])],
       [gl_cv_type_wctrans_t=yes],
       [gl_cv_type_wctrans_t=no])
    ])
  if test $gl_cv_type_wctrans_t = no; then
    HAVE_WCTRANS_T=0
  fi

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[
#if !(defined __GLIBC__ && !defined __UCLIBC__)
# include <wchar.h>
#endif
#include <wctype.h>
    ]],
    [wctype iswctype wctrans towctrans
    ])
])

# gl_WCTYPE_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_WCTYPE_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_WCTYPE_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_WCTYPE_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_WCTYPE_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISWBLANK])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISWDIGIT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISWXDIGIT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCTYPE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISWCTYPE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCTRANS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TOWCTRANS])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_WCTYPE_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
])

AC_DEFUN([gl_WCTYPE_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_ISWBLANK=1;      AC_SUBST([HAVE_ISWBLANK])
  HAVE_WCTYPE_T=1;      AC_SUBST([HAVE_WCTYPE_T])
  HAVE_WCTRANS_T=1;     AC_SUBST([HAVE_WCTRANS_T])
  REPLACE_ISWBLANK=0;   AC_SUBST([REPLACE_ISWBLANK])
  REPLACE_ISWDIGIT=0;   AC_SUBST([REPLACE_ISWDIGIT])
  REPLACE_ISWXDIGIT=0;  AC_SUBST([REPLACE_ISWXDIGIT])
])
