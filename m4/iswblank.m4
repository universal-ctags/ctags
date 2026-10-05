# iswblank.m4
# serial 7
dnl Copyright (C) 2011-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_ISWBLANK],
[
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
  AC_REQUIRE([gl_WCTYPE_H])
  dnl Persuade glibc <wctype.h> to declare iswblank().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  gl_CHECK_FUNCS_ANDROID([iswblank], [[#include <wctype.h>]])
  AC_CHECK_DECLS([iswblank], , , [[
    #include <wchar.h>
    #include <wctype.h>
  ]])
  if test $ac_cv_func_iswblank = no; then
    HAVE_ISWBLANK=0
    if test $ac_cv_have_decl_iswblank = yes \
       || case "$gl_cv_onwards_func_iswblank" in \
            future*) true ;; \
            *) false ;; \
          esac; then
      REPLACE_ISWBLANK=1
    fi
  fi
  if test $HAVE_ISWCNTRL = 0 || test $REPLACE_ISWCNTRL = 1; then
    dnl Redefine all of iswcntrl, ..., towupper in <wctype.h>.
    :
  else
    if test $HAVE_ISWBLANK = 0 || test $REPLACE_ISWBLANK = 1; then
      dnl Redefine only iswblank.
      :
    fi
  fi

])
