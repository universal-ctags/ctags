# stdcountof_h.m4
# serial 3
dnl Copyright 2025-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_STDCOUNTOF_H],
[
  AC_CHECK_HEADERS_ONCE([stdcountof.h])
  gl_CHECK_NEXT_HEADERS([stdcountof.h])
  if test $ac_cv_header_stdcountof_h = yes; then
    HAVE_STDCOUNTOF_H=1
  else
    HAVE_STDCOUNTOF_H=0
  fi
  AC_SUBST([HAVE_STDCOUNTOF_H])

  dnl In clang 21, <stdcountof.h> exists but does not work in C++ mode, because
  dnl it uses _Countof, which is not a compiler built-in in C++ mode.
  m4_ifdef([gl_ANSI_CXX], [AC_REQUIRE([gl_ANSI_CXX])])
  CXX_HAVE_STDCOUNTOF_H=1
  if test -n "$CXX" && test "$CXX" != no; then
    AC_CACHE_CHECK([whether the C++ compiler has <stdcountof.h>],
      [gl_cv_cxx_have_stdcountof_h],
      [dnl We can't use AC_LANG_PUSH([C++]) and AC_LANG_POP([C++]) here, due to
       dnl an autoconf bug <https://savannah.gnu.org/support/?110294>.
       cat > conftest.cpp <<\EOF
#include <stdcountof.h>
int a[] = { 86, 47 };
unsigned int a_n = countof (a);
EOF
       gl_command="$CXX $CXXFLAGS $CPPFLAGS -c conftest.cpp"
       if AC_TRY_EVAL([gl_command]); then
         gl_cv_cxx_have_stdcountof_h=yes
       else
         gl_cv_cxx_have_stdcountof_h=no
       fi
       rm -fr conftest*
      ])
    if test $gl_cv_cxx_have_stdcountof_h != yes; then
      CXX_HAVE_STDCOUNTOF_H=0
    fi
  fi
  AC_SUBST([CXX_HAVE_STDCOUNTOF_H])

  if test $HAVE_STDCOUNTOF_H = 1 && test $CXX_HAVE_STDCOUNTOF_H = 1; then
    GL_GENERATE_STDCOUNTOF_H=false
  else
    GL_GENERATE_STDCOUNTOF_H=true
  fi
])
