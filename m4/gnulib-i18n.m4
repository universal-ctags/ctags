# gnulib-i18n.m4
# serial 1
dnl Copyright (C) 2005-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Bruno Haible.

dnl Support for internationalization of Gnulib code.

dnl GNULIB_I18N
dnl Sets GNULIB_LOCALEDIR to indicate where to find the gnulib.mo files.
dnl Also it defines GNULIB_LOCALEDIR as macro in config.h, that expands to
dnl the corresponding C string.
AC_DEFUN([GNULIB_I18N],
[
  dnl It is best to not test "$USE_NLS" here, because: It would be empty
  dnl in case the package is internationalized but this macro is used before
  dnl AM_GNU_GETTEXT. We would need to warn about this situation. But since
  dnl this module is used as a dependency of many packages, such a warning is
  dnl not welcome.

  dnl Determine gnulib's localedir.
  dnl Generally, accept an option --with-gnulib-prefix=PREFIX to indicate
  dnl where to find gnulib's runtime data.
  dnl Usually ${prefix}/share/locale, but can be influenced by the configure
  dnl options --datarootdir and --localedir.
  GNULIB_LOCALEDIR="${localedir}"
  AC_ARG_WITH([gnulib-prefix],
    [[  --with-gnulib-prefix=DIR  search for gnulib's runtime data in DIR/share]],
    [if test "X$withval" != "X" && test "X$withval" != "Xno"; then
       GNULIB_LOCALEDIR="$withval/share/locale"
     fi
    ])
  AC_SUBST([GNULIB_LOCALEDIR])

  dnl Define GNULIB_LOCALEDIR_c and GNULIB_LOCALEDIR_c_make.
  dnl Find the final value of GNULIB_LOCALEDIR.
  gl_saved_prefix="${prefix}"
  gl_saved_datarootdir="${datarootdir}"
  gl_saved_localedir="${localedir}"
  gl_saved_gnuliblocaledir="${GNULIB_LOCALEDIR}"
  dnl Unfortunately, prefix gets only finally determined at the end of
  dnl configure.
  if test "X$prefix" = "XNONE"; then
    prefix="$ac_default_prefix"
  fi
  eval datarootdir="$datarootdir"
  eval localedir="$localedir"
  eval GNULIB_LOCALEDIR="$GNULIB_LOCALEDIR"
  gl_BUILD_TO_HOST([GNULIB_LOCALEDIR])
  GNULIB_LOCALEDIR="${gl_saved_gnuliblocaledir}"
  localedir="${gl_saved_localedir}"
  datarootdir="${gl_saved_datarootdir}"
  prefix="${gl_saved_prefix}"

  AC_DEFINE_UNQUOTED([GNULIB_LOCALEDIR], [${GNULIB_LOCALEDIR_c}],
    [Define to the directory where to find the localizations of the translation domain 'gnulib', as a C string.])
])
