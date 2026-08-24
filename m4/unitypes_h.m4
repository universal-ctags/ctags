# unitypes_h.m4
# serial 1
dnl Copyright (C) 2021-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_UNITYPES_H],
[
  AH_VERBATIM([unitypes_restrict], [
/* This definition is a duplicate of the one in unitypes.h.
   It is here so that we can cope with an older version of unitypes.h
   that does not contain this definition and that is pre-installed among
   the public header files.  */
# if defined __restrict \
     || 2 < __GNUC__ + (95 <= __GNUC_MINOR__) \
     || __clang_major__ >= 3
#  define _UC_RESTRICT __restrict
# elif 199901L <= __STDC_VERSION__ || defined restrict
#  define _UC_RESTRICT restrict
# else
#  define _UC_RESTRICT
# endif
])
])
