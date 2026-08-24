# flexmember.m4
# serial 5
dnl Copyright (C) 2006, 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Check for flexible array member support.

# Written by Paul Eggert.

AC_DEFUN([AC_C_FLEXIBLE_ARRAY_MEMBER],
[
  AC_CACHE_CHECK([for flexible array member],
    ac_cv_c_flexmember,
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <stdlib.h>
            #include <stdio.h>
            #include <stddef.h>
            struct m { struct m *next, **list; char name[]; };
            struct s { struct s *p; struct m *m; int n; double d[]; };]],
          [[int m = getchar ();
            size_t nbytes = offsetof (struct s, d) + m * sizeof (double);
            nbytes += sizeof (struct s) - 1;
            nbytes -= nbytes % sizeof (struct s);
            struct s *p = malloc (nbytes);
            p->p = p;
            p->m = NULL;
            p->d[0] = 0.0;
            return p->d != (double *) NULL;]])],
       [ac_cv_c_flexmember=yes],
       [ac_cv_c_flexmember=no])])
  if test $ac_cv_c_flexmember = yes; then
    AC_DEFINE([FLEXIBLE_ARRAY_MEMBER], [],
      [Define to nothing if C supports flexible array members, and to
       1 if it does not.  That way, with a declaration like 'struct s
       { int n; short d@<:@FLEXIBLE_ARRAY_MEMBER@:>@; };', the struct hack
       can be used with pre-C99 compilers.
       Use 'FLEXSIZEOF (struct s, d, N * sizeof (short))' to calculate
       the size in bytes of such a struct containing an N-element array.])
  else
    AC_DEFINE([FLEXIBLE_ARRAY_MEMBER], [1])
  fi
])
