# stdckdint_h.m4
# serial 2
dnl Copyright 2025-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl Written by Collin Funk.

AC_DEFUN_ONCE([gl_STDCKDINT_H],
[
  gl_CHECK_NEXT_HEADERS([stdckdint.h])
  if test $ac_cv_header_stdckdint_h = yes; then
    HAVE_STDCKDINT_H=1
  else
    HAVE_STDCKDINT_H=0
  fi
  AC_SUBST([HAVE_STDCKDINT_H])

  if test $HAVE_STDCKDINT_H = 1; then
    AC_CACHE_CHECK([whether stdckdint.h can be included in C],
      [gl_cv_header_c_stdckdint_h],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <stdckdint.h>
            ]])],
         [gl_cv_header_c_stdckdint_h=yes],
         [gl_cv_header_c_stdckdint_h=no])])
    if test $gl_cv_header_c_stdckdint_h = yes; then
      HAVE_C_STDCKDINT_H=1
      AC_CACHE_CHECK([checking for an ISO C23 compliant stdckdint.h in C],
        [gl_cv_header_c_stdckdint_h_works],
        [AC_COMPILE_IFELSE(
           [AC_LANG_PROGRAM(
              [[#include <stdckdint.h>
              ]],
              [[int r;
                int a = 1;
                int b = 1;
                return !!(ckd_add (&r, a, b) || ckd_sub (&r, a, b)
                          || ckd_mul (&r, a, b));
              ]])],
           [gl_cv_header_c_stdckdint_h_works=yes],
           [gl_cv_header_c_stdckdint_h_works=no])])
      if test $gl_cv_header_c_stdckdint_h_works = yes; then
        HAVE_WORKING_C_STDCKDINT_H=1
      else
        HAVE_WORKING_C_STDCKDINT_H=0
      fi
    else
      HAVE_C_STDCKDINT_H=0
      HAVE_WORKING_C_STDCKDINT_H=0
    fi
    if test -n "$CXX" && test "$CXX" != no; then
      AC_CACHE_CHECK([whether stdckdint.h can be included in C++],
        [gl_cv_header_cxx_stdckdint_h],
        [dnl We can't use AC_LANG_PUSH([C++]) and AC_LANG_POP([C++]) here, due to
         dnl an autoconf bug <https://savannah.gnu.org/support/?110294>.
         cat > conftest.cpp <<\EOF
#include <stdckdint.h>
EOF
         gl_command="$CXX $CXXFLAGS $CPPFLAGS -c conftest.cpp"
         if AC_TRY_EVAL([gl_command]); then
           gl_cv_header_cxx_stdckdint_h=yes
         else
           gl_cv_header_cxx_stdckdint_h=no
         fi
         rm -fr conftest*
        ])
      if test $gl_cv_header_cxx_stdckdint_h = yes; then
        HAVE_CXX_STDCKDINT_H=1
        AC_CACHE_CHECK([checking for an ISO C++26 compliant stdckdint.h in C++],
          [gl_cv_header_cxx_stdckdint_h_works],
          [dnl We can't use AC_LANG_PUSH([C++]) and AC_LANG_POP([C++]) here, due to
           dnl an autoconf bug <https://savannah.gnu.org/support/?110294>.
           cat > conftest.cpp <<\EOF
#include <stdckdint.h>
int
main (void)
{
  int r;
  int a = 1;
  int b = 1;
  return !!(ckd_add (&r, a, b) || ckd_sub (&r, a, b) || ckd_mul (&r, a, b));
}
EOF
           gl_command="$CXX $CXXFLAGS $CPPFLAGS -c conftest.cpp"
           if AC_TRY_EVAL([gl_command]); then
             gl_cv_header_cxx_stdckdint_h_works=yes
           else
             gl_cv_header_cxx_stdckdint_h_works=no
           fi
           rm -fr conftest*
          ])
        if test $gl_cv_header_cxx_stdckdint_h_works = yes; then
          HAVE_WORKING_CXX_STDCKDINT_H=1
        else
          HAVE_WORKING_CXX_STDCKDINT_H=0
        fi
      else
        HAVE_CXX_STDCKDINT_H=0
        HAVE_WORKING_CXX_STDCKDINT_H=0
      fi
    fi
  else
    HAVE_C_STDCKDINT_H=0
    HAVE_WORKING_C_STDCKDINT_H=0
    HAVE_CXX_STDCKDINT_H=0
    HAVE_WORKING_CXX_STDCKDINT_H=0
  fi
  AC_SUBST([HAVE_C_STDCKDINT_H])
  AC_SUBST([HAVE_WORKING_C_STDCKDINT_H])
  AC_SUBST([HAVE_CXX_STDCKDINT_H])
  AC_SUBST([HAVE_WORKING_CXX_STDCKDINT_H])

  if test -n "$CXX" && test "$CXX" != no; then
    dnl We might need the header for C or C++.
    if test $HAVE_C_STDCKDINT_H = 1 \
       && test $HAVE_WORKING_C_STDCKDINT_H = 1 \
       && test $HAVE_CXX_STDCKDINT_H = 1 \
       && test $HAVE_WORKING_CXX_STDCKDINT_H = 1; then
      GL_GENERATE_STDCKDINT_H=false
    else
      GL_GENERATE_STDCKDINT_H=true
    fi
  else
    dnl We don't care about C++ here.
    if test $HAVE_C_STDCKDINT_H = 1 \
       && test $HAVE_WORKING_C_STDCKDINT_H = 1; then
      GL_GENERATE_STDCKDINT_H=false
    else
      GL_GENERATE_STDCKDINT_H=true
    fi
  fi
])
