# ===========================================================================
#   https://www.gnu.org/software/autoconf-archive/ax_prog_cc_for_build.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_PROG_CC_FOR_BUILD
#
# DESCRIPTION
#
#   This macro searches for a C compiler that generates native executables,
#   that is a C compiler that surely is not a cross-compiler. This can be
#   useful if you have to generate source code at compile-time like for
#   example GCC does.
#
#   The macro sets the CC_FOR_BUILD and CPP_FOR_BUILD macros to anything
#   needed to compile or link (CC_FOR_BUILD) and preprocess (CPP_FOR_BUILD).
#   The value of these variables can be overridden by the user by specifying
#   a compiler with an environment variable (like you do for standard CC).
#
#   It also sets BUILD_EXEEXT and BUILD_OBJEXT to the executable and object
#   file extensions for the build platform, and GCC_FOR_BUILD to `yes' if
#   the compiler we found is GCC. All these variables but GCC_FOR_BUILD are
#   substituted in the Makefile.
#
# LICENSE
#
#   Copyright (c) 2008 Paolo Bonzini <bonzini@gnu.org>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 18


dnl This fix is taken from alsa-firmware.
dnl
dnl https://git.alsa-project.org/?p=alsa-firmware.git;a=patch;h=39da20beb3165ef759161b9467c9c98fd314bc08;hp=f08b6476d0db2f34a256514533f2f86d7a23ef7f
dnl
dnl From 39da20beb3165ef759161b9467c9c98fd314bc08 Mon Sep 17 00:00:00 2001
dnl From: Jaroslav Kysela <perex@perex.cz>
dnl Date: Tue, 20 Oct 2020 13:27:16 +0200
dnl Subject: [PATCH 1/1] add _AC_LANG_COMPILER_GNU workaround to
dnl  m4/ax_prog_cc_for_build.m4
dnl
dnl Signed-off-by: Jaroslav Kysela <perex@perex.cz>
dnl
dnl _AC_LANG_COMPILER_GNU
dnl ---------------------
dnl This is hacked version to bypass the cache. --jk
m4_define([_AC_LANG_COMPILER_GNU],
[AC_MSG_CHECKING([whether we are using the GNU _AC_LANG compiler])
 _AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[#ifndef __GNUC__
       choke me
#endif
]])],
               [ac_compiler_gnu=yes],
               [ac_compiler_gnu=no])
ac_cv_[]_AC_LANG_ABBREV[]_compiler_gnu=$ac_compiler_gnu
AC_MSG_RESULT([$ac_compiler_gnu])
])# _AC_LANG_COMPILER_GNU

AU_ALIAS([AC_PROG_CC_FOR_BUILD], [AX_PROG_CC_FOR_BUILD])
AC_DEFUN([AX_PROG_CC_FOR_BUILD], [dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_CC_C99])dnl
AC_REQUIRE([AC_PROG_CPP])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl

dnl Use the standard macros, but make them use other variable names
dnl
pushdef([ac_cv_prog_CPP], ac_cv_build_prog_CPP)dnl
pushdef([ac_cv_prog_cc_c89], ac_cv_build_prog_cc_c89)dnl
pushdef([ac_cv_prog_cc_c99], ac_cv_build_prog_cc_c99)dnl
pushdef([ac_cv_prog_gcc], ac_cv_build_prog_gcc)dnl
pushdef([ac_cv_prog_cc_works], ac_cv_build_prog_cc_works)dnl
pushdef([ac_cv_prog_cc_cross], ac_cv_build_prog_cc_cross)dnl
pushdef([ac_cv_prog_cc_g], ac_cv_build_prog_cc_g)dnl
pushdef([ac_cv_c_compiler_gnu], ac_cv_build_c_compiler_gnu)dnl
pushdef([ac_cv_exeext], ac_cv_build_exeext)dnl
pushdef([ac_cv_objext], ac_cv_build_objext)dnl
pushdef([ac_exeext], ac_build_exeext)dnl
pushdef([ac_objext], ac_build_objext)dnl
pushdef([CC], CC_FOR_BUILD)dnl
pushdef([CPP], CPP_FOR_BUILD)dnl
pushdef([GCC], GCC_FOR_BUILD)dnl
pushdef([CFLAGS], CFLAGS_FOR_BUILD)dnl
pushdef([CPPFLAGS], CPPFLAGS_FOR_BUILD)dnl
pushdef([EXEEXT], BUILD_EXEEXT)dnl
pushdef([LDFLAGS], LDFLAGS_FOR_BUILD)dnl
pushdef([OBJEXT], BUILD_OBJEXT)dnl
pushdef([host], build)dnl
pushdef([host_alias], build_alias)dnl
pushdef([host_cpu], build_cpu)dnl
pushdef([host_vendor], build_vendor)dnl
pushdef([host_os], build_os)dnl
pushdef([ac_cv_host], ac_cv_build)dnl
pushdef([ac_cv_host_alias], ac_cv_build_alias)dnl
pushdef([ac_cv_host_cpu], ac_cv_build_cpu)dnl
pushdef([ac_cv_host_vendor], ac_cv_build_vendor)dnl
pushdef([ac_cv_host_os], ac_cv_build_os)dnl
pushdef([ac_tool_prefix], ac_build_tool_prefix)dnl
pushdef([am_cv_CC_dependencies_compiler_type], am_cv_build_CC_dependencies_compiler_type)dnl
pushdef([am_cv_prog_cc_c_o], am_cv_build_prog_cc_c_o)dnl
pushdef([cross_compiling], cross_compiling_build)dnl

cross_compiling_build=no

ac_build_tool_prefix=
AS_IF([test -n "$build"],      [ac_build_tool_prefix="$build-"],
      [test -n "$build_alias"],[ac_build_tool_prefix="$build_alias-"])

AC_LANG_PUSH([C])
dnl
dnl AC_PROG_CC is expanded twice in this AX_PROG_CC_FOR_BUILD:
dnl
dnl * the preparation stage (the 2nd time), and
dnl * the main stage (the 3rd time).
dnl
dnl "The 1st time" is at AC_PROG_CC put directly at ./configure.ac.
dnl
dnl Expanding twice is needed to build packcc in CentOS6 with -std=c99
dnl option. See https://github.com/universal-ctags/ctags/issues/2776.
dnl
dnl Why AC_PROG_CC must be expanded twice?
dnl
dnl --------------------------------------------------------------------
dnl AC_PROG_CC
dnl    ...
dnl    m4_expand_once (_AC_COMPILER_OBJEXT)
dnl    _AC_LANG_COMPILER_GNU
dnl    ...
dnl --------------------------------------------------------------------
dnl
dnl This is internal structure of AC_PROG_CC.
dnl
dnl _AC_LANG_COMPILER_GNU of CentOS6 refers to ac_objext and needs
dnl a value for ac_objext when a shell executes the generated code.
dnl The code generated by _AC_COMPILER_OBJEXT provides the value.
dnl
dnl --------------------------------------------------------------------
dnl configre.ac:
dnl    ...
dnl    AC_PROG_CC # the 1st time
dnl        ...
dnl >>>    m4_expand_once (_AC_COMPILER_OBJEXT)
dnl        _AC_LANG_COMPILER_GNU
dnl        ...
dnl    ...
dnl    AX_PROG_CC_FOR_BUILD
dnl    ...
dnl        AC_PROG_CC # the 2nd time
dnl            ...
dnl >>>        m4_expand_once (_AC_COMPILER_OBJEXT)
dnl            _AC_LANG_COMPILER_GNU
dnl            ...
dnl        ...
dnl    ...
dnl --------------------------------------------------------------------
dnl
dnl When you use this AX_PROG_CC_FOR_BUILD macro, _AC_COMPILER_OBJEXT
dnl in AC_PROG_CC in AX_PROG_CC_FOR_BUILD is not expanded because
dnl m4_expand_once prevents _AC_COMPILER_OBJEXT being expanded.
dnl See the marker '>>>'.
dnl As result of NOT being expanded, a shell process running
dnl configure doesn't set a value to ac_build_objext.
dnl The value is needed in _AC_LANG_COMPILER_GNU.
dnl You will see the following message in output of configure
dnl
dnl --------------------------------------------------------------------
dnl checking whether we are using the GNU C compiler... no
dnl --------------------------------------------------------------------
dnl
dnl though gcc is available.
dnl
dnl twice, in configure.ac directly (the 1st time), and from
dnl AX_PROG_CC_FOR_BUILD indirectly (the 2nd time).
dnl
dnl We have a trouble in the 2nd time. _AC_COMPILER_OBJEXT is not
dnl expanded in the 2nd time because m4_expand_once is used.
dnl
dnl Reordering the order of macro expansion as following
dnl doesn't help:
dnl
dnl --------------------------------------------------------------------
dnl AX_PROG_CC_FOR_BUILD
dnl    ...
dnl    _AC_COMPILER_OBJEXT
dnl    AC_PROG_CC
dnl --------------------------------------------------------------------
dnl
dnl because _AC_COMPILER_OBJEXT depends on a value of CC_BUILD
dnl set by the code generated from AC_PROG_CC.
dnl
dnl Here a hack comes in.
dnl For setting a value to ac_build_objext that _AC_LANG_COMPILER_GNU
dnl needs, expanding the macros in the following order
dnl
dnl --------------------------------------------------------------------
dnl AC_PROG_CC
dnl _AC_COMPILER_OBJEXT
dnl --------------------------------------------------------------------
dnl
dnl This is the preparation stage (the 2nd stage) of AC_PROG_CC expansion.
dnl The code generated by AC_PROG_CC may not be executed successful because
dnl the code generated by _AC_LANG_COMPILER_GNU may fail because a value
dnl for ac_build_objext is not set. However, the code generated by the
dnl above macros sets a value to ac_build_objext at the end of the code:
dnl
dnl --------------------------------------------------------------------
dnl AC_PROG_CC
dnl # ac_build_objext is NOT set. CC_BUILD is set.
dnl _AC_COMPILER_OBJEXT
dnl # ac_build_objext is set.
dnl --------------------------------------------------------------------
dnl
dnl At the end of line, it is ready to run the code generated by
dnl _AC_LANG_COMPILER_GNU. The code is run in AC_PROG_CC in the main
dnl stage (the 3rd time).

dnl the preparation stage (the 2nd time).
AC_PROG_CC
_AC_COMPILER_OBJEXT
_AC_COMPILER_EXEEXT
unset ac_cv_build_prog_cc_g
unset ac_cv_build_prog_cc_c89
unset am_cv_CC_FOR_BUILD_dependencies_compiler_type

dnl the main stage (the 3rd time).
AC_PROG_CC
AC_PROG_CC_C99
AC_PROG_CPP

dnl Restore the old definitions
dnl
popdef([cross_compiling])dnl
popdef([am_cv_prog_cc_c_o])dnl
popdef([am_cv_CC_dependencies_compiler_type])dnl
popdef([ac_tool_prefix])dnl
popdef([ac_cv_host_os])dnl
popdef([ac_cv_host_vendor])dnl
popdef([ac_cv_host_cpu])dnl
popdef([ac_cv_host_alias])dnl
popdef([ac_cv_host])dnl
popdef([host_os])dnl
popdef([host_vendor])dnl
popdef([host_cpu])dnl
popdef([host_alias])dnl
popdef([host])dnl
popdef([OBJEXT])dnl
popdef([LDFLAGS])dnl
popdef([EXEEXT])dnl
popdef([CPPFLAGS])dnl
popdef([CFLAGS])dnl
popdef([GCC])dnl
popdef([CPP])dnl
popdef([CC])dnl
popdef([ac_objext])dnl
popdef([ac_exeext])dnl
popdef([ac_cv_objext])dnl
popdef([ac_cv_exeext])dnl
popdef([ac_cv_c_compiler_gnu])dnl
popdef([ac_cv_prog_cc_g])dnl
popdef([ac_cv_prog_cc_cross])dnl
popdef([ac_cv_prog_cc_works])dnl
popdef([ac_cv_prog_cc_c99])dnl
popdef([ac_cv_prog_cc_c89])dnl
popdef([ac_cv_prog_gcc])dnl
popdef([ac_cv_prog_CPP])dnl

dnl restore global variables ac_ext, ac_cpp, ac_compile,
dnl ac_link, ac_compiler_gnu (dependant on the current
dnl language after popping):
AC_LANG_POP([C])

dnl Finally, set Makefile variables
dnl
AC_SUBST(BUILD_EXEEXT)dnl
AC_SUBST(BUILD_OBJEXT)dnl
AC_SUBST([CFLAGS_FOR_BUILD])dnl
AC_SUBST([CPPFLAGS_FOR_BUILD])dnl
AC_SUBST([LDFLAGS_FOR_BUILD])dnl
])
