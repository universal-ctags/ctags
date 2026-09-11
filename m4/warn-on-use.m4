# warn-on-use.m4
# serial 11
dnl Copyright (C) 2010-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# gl_WARN_ON_USE_PREPARE(INCLUDES, NAMES)
# ---------------------------------------
# If the module 'posixcheck' is in use:
#
# For each whitespace-separated element in the list of NAMES, define
# HAVE_RAW_DECL_name if the function has a declaration among INCLUDES
# even after being undefined as a macro.
#
# See warn-on-use.h for some hints on how to poison function names, as
# well as ideas on poisoning global variables and macros.  NAMES may
# include global variables, but remember that only functions work with
# _GL_WARN_ON_USE.  Typically, INCLUDES only needs to list a single
# header, but if the replacement header pulls in other headers because
# some systems declare functions in the wrong header, then INCLUDES
# should do likewise.
#
# It is generally safe to assume declarations for functions declared
# in the intersection of C89 and C11 (such as printf) without
# needing gl_WARN_ON_USE_PREPARE.
AC_DEFUN([gl_WARN_ON_USE_PREPARE],
[
  m4_ifdef([gl_POSIXCHECK],
    [m4_foreach_w([gl_decl], [$2],
       [AH_TEMPLATE([HAVE_RAW_DECL_]AS_TR_CPP(m4_defn([gl_decl])),
         [Define to 1 if ]m4_defn([gl_decl])[ is declared even after
          undefining macros.])])dnl
     for gl_func in m4_flatten([$2]); do
       AS_VAR_PUSHDEF([gl_Symbol], [gl_cv_have_raw_decl_$gl_func])dnl
       dnl As a workaround to implicit built-in function declarations in
       dnl clang (e.g. strndup), reference ac_compile_for_check_decl instead
       dnl of ac_compile.  If, for whatever reason, the override of AC_PROG_CC
       dnl in zzgnulib.m4 is inactive, use the original ac_compile.
       ac_saved_ac_compile="$ac_compile"
       if test -n "$ac_compile_for_check_decl"; then
         ac_compile="$ac_compile_for_check_decl"
       fi
       AC_CACHE_CHECK([whether $gl_func is declared without a macro],
         [gl_Symbol],
         [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$1],
[[#undef $gl_func
  (void) $gl_func;]])],
           [AS_VAR_SET([gl_Symbol], [yes])], [AS_VAR_SET([gl_Symbol], [no])])])
       ac_compile="$ac_saved_ac_compile"
       AS_VAR_IF([gl_Symbol], [yes],
         [AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_RAW_DECL_$gl_func]), [1])
          dnl Shortcut for an AC_CHECK_DECL invocation that may come later:
          dnl If the raw declaration exists with the given includes, then
          dnl AC_CHECK_DECL with its many includes would see it as well.
          dnl So, set a cache variable to allow skipping any later
          dnl AC_CHECK_DECL invocation for $gl_func.
          eval "ac_cv_have_decl_$gl_func=yes"
         ])
       AS_VAR_POPDEF([gl_Symbol])dnl
     done
    ])
])
