changequote([`],['])
define(`x', 1)
changequote(`[',`]')
AC_DEFUN([PKG_PREREQ],
[m4_define([PKG_MACROS_VERSION], [0.29])
m4_if(m4_version_compare(PKG_MACROS_VERSION, [$1]), -1,
    [m4_fatal([pkg.m4 version $1 or higher is required but ]PKG_MACROS_VERSION[ found])])
])dnl PKG_PREREQ
m4_define([y], 2)
m4_changequote([`],['])
define(`z', 3)

