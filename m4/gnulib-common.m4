# gnulib-common.m4 serial 65
dnl Copyright (C) 2007-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_PREREQ([2.62])

# gl_COMMON
# is expanded unconditionally through gnulib-tool magic.
AC_DEFUN([gl_COMMON], [
  dnl Use AC_REQUIRE here, so that the code is expanded once only.
  AC_REQUIRE([gl_00GNULIB])
  AC_REQUIRE([gl_COMMON_BODY])
  AC_REQUIRE([gl_ZZGNULIB])
])
AC_DEFUN([gl_COMMON_BODY], [
  AH_VERBATIM([_GL_GNUC_PREREQ],
[/* True if the compiler says it groks GNU C version MAJOR.MINOR.  */
#if defined __GNUC__ && defined __GNUC_MINOR__
# define _GL_GNUC_PREREQ(major, minor) \
    ((major) < __GNUC__ + ((minor) <= __GNUC_MINOR__))
#else
# define _GL_GNUC_PREREQ(major, minor) 0
#endif
])
  AH_VERBATIM([_Noreturn],
[/* The _Noreturn keyword of C11.  */
#ifndef _Noreturn
# if (defined __cplusplus \
      && ((201103 <= __cplusplus && !(__GNUC__ == 4 && __GNUC_MINOR__ == 7)) \
          || (defined _MSC_VER && 1900 <= _MSC_VER)) \
      && 0)
    /* [[noreturn]] is not practically usable, because with it the syntax
         extern _Noreturn void func (...);
       would not be valid; such a declaration would only be valid with 'extern'
       and '_Noreturn' swapped, or without the 'extern' keyword.  However, some
       AIX system header files and several gnulib header files use precisely
       this syntax with 'extern'.  */
#  define _Noreturn [[noreturn]]
# elif ((!defined __cplusplus || defined __clang__) \
        && (201112 <= (defined __STDC_VERSION__ ? __STDC_VERSION__ : 0) \
            || (!defined __STRICT_ANSI__ \
                && (_GL_GNUC_PREREQ (4, 7) \
                    || (defined __apple_build_version__ \
                        ? 6000000 <= __apple_build_version__ \
                        : 3 < __clang_major__ + (5 <= __clang_minor__))))))
   /* _Noreturn works as-is.  */
# elif _GL_GNUC_PREREQ (2, 8) || defined __clang__ || 0x5110 <= __SUNPRO_C
#  define _Noreturn __attribute__ ((__noreturn__))
# elif 1200 <= (defined _MSC_VER ? _MSC_VER : 0)
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn
# endif
#endif
])
  AH_VERBATIM([isoc99_inline],
[/* Work around a bug in Apple GCC 4.0.1 build 5465: In C99 mode, it supports
   the ISO C 99 semantics of 'extern inline' (unlike the GNU C semantics of
   earlier versions), but does not display it by setting __GNUC_STDC_INLINE__.
   __APPLE__ && __MACH__ test for Mac OS X.
   __APPLE_CC__ tests for the Apple compiler and its version.
   __STDC_VERSION__ tests for the C99 mode.  */
#if defined __APPLE__ && defined __MACH__ && __APPLE_CC__ >= 5465 && !defined __cplusplus && __STDC_VERSION__ >= 199901L && !defined __GNUC_STDC_INLINE__
# define __GNUC_STDC_INLINE__ 1
#endif])
  AH_VERBATIM([attribute],
[/* Attributes.  */
#if (defined __has_attribute \
     && (!defined __clang_minor__ \
         || 3 < __clang_major__ + (5 <= __clang_minor__)))
# define _GL_HAS_ATTRIBUTE(attr) __has_attribute (__##attr##__)
#else
# define _GL_HAS_ATTRIBUTE(attr) _GL_ATTR_##attr
# define _GL_ATTR_alloc_size _GL_GNUC_PREREQ (4, 3)
# define _GL_ATTR_always_inline _GL_GNUC_PREREQ (3, 2)
# define _GL_ATTR_artificial _GL_GNUC_PREREQ (4, 3)
# define _GL_ATTR_cold _GL_GNUC_PREREQ (4, 3)
# define _GL_ATTR_const _GL_GNUC_PREREQ (2, 95)
# define _GL_ATTR_deprecated _GL_GNUC_PREREQ (3, 1)
# define _GL_ATTR_diagnose_if 0
# define _GL_ATTR_error _GL_GNUC_PREREQ (4, 3)
# define _GL_ATTR_externally_visible _GL_GNUC_PREREQ (4, 1)
# define _GL_ATTR_fallthrough _GL_GNUC_PREREQ (7, 0)
# define _GL_ATTR_format _GL_GNUC_PREREQ (2, 7)
# define _GL_ATTR_leaf _GL_GNUC_PREREQ (4, 6)
# ifdef _ICC
#  define _GL_ATTR_may_alias 0
# else
#  define _GL_ATTR_may_alias _GL_GNUC_PREREQ (3, 3)
# endif
# define _GL_ATTR_malloc _GL_GNUC_PREREQ (3, 0)
# define _GL_ATTR_noinline _GL_GNUC_PREREQ (3, 1)
# define _GL_ATTR_nonnull _GL_GNUC_PREREQ (3, 3)
# define _GL_ATTR_nonstring _GL_GNUC_PREREQ (8, 0)
# define _GL_ATTR_nothrow _GL_GNUC_PREREQ (3, 3)
# define _GL_ATTR_packed _GL_GNUC_PREREQ (2, 7)
# define _GL_ATTR_pure _GL_GNUC_PREREQ (2, 96)
# define _GL_ATTR_returns_nonnull _GL_GNUC_PREREQ (4, 9)
# define _GL_ATTR_sentinel _GL_GNUC_PREREQ (4, 0)
# define _GL_ATTR_unused _GL_GNUC_PREREQ (2, 7)
# define _GL_ATTR_warn_unused_result _GL_GNUC_PREREQ (3, 4)
#endif

]dnl There is no _GL_ATTRIBUTE_ALIGNED; use stdalign's _Alignas instead.
[
#if _GL_HAS_ATTRIBUTE (alloc_size)
# define _GL_ATTRIBUTE_ALLOC_SIZE(args) __attribute__ ((__alloc_size__ args))
#else
# define _GL_ATTRIBUTE_ALLOC_SIZE(args)
#endif

#if _GL_HAS_ATTRIBUTE (always_inline)
# define _GL_ATTRIBUTE_ALWAYS_INLINE __attribute__ ((__always_inline__))
#else
# define _GL_ATTRIBUTE_ALWAYS_INLINE
#endif

#if _GL_HAS_ATTRIBUTE (artificial)
# define _GL_ATTRIBUTE_ARTIFICIAL __attribute__ ((__artificial__))
#else
# define _GL_ATTRIBUTE_ARTIFICIAL
#endif

/* Avoid __attribute__ ((cold)) on MinGW; see thread starting at
   <https://lists.gnu.org/r/emacs-devel/2019-04/msg01152.html>.
   Also, Oracle Studio 12.6 requires 'cold' not '__cold__'.  */
#if _GL_HAS_ATTRIBUTE (cold) && !defined __MINGW32__
# ifndef __SUNPRO_C
#  define _GL_ATTRIBUTE_COLD __attribute__ ((__cold__))
# else
#  define _GL_ATTRIBUTE_COLD __attribute__ ((cold))
# endif
#else
# define _GL_ATTRIBUTE_COLD
#endif

#if _GL_HAS_ATTRIBUTE (const)
# define _GL_ATTRIBUTE_CONST __attribute__ ((__const__))
#else
# define _GL_ATTRIBUTE_CONST
#endif

#if 201710L < __STDC_VERSION__
# define _GL_ATTRIBUTE_DEPRECATED [[__deprecated__]]
#elif _GL_HAS_ATTRIBUTE (deprecated)
# define _GL_ATTRIBUTE_DEPRECATED __attribute__ ((__deprecated__))
#else
# define _GL_ATTRIBUTE_DEPRECATED
#endif

#if _GL_HAS_ATTRIBUTE (error)
# define _GL_ATTRIBUTE_ERROR(msg) __attribute__ ((__error__ (msg)))
# define _GL_ATTRIBUTE_WARNING(msg) __attribute__ ((__warning__ (msg)))
#elif _GL_HAS_ATTRIBUTE (diagnose_if)
# define _GL_ATTRIBUTE_ERROR(msg) __attribute__ ((__diagnose_if__ (1, msg, "error")))
# define _GL_ATTRIBUTE_WARNING(msg) __attribute__ ((__diagnose_if__ (1, msg, "warning")))
#else
# define _GL_ATTRIBUTE_ERROR(msg)
# define _GL_ATTRIBUTE_WARNING(msg)
#endif

#if _GL_HAS_ATTRIBUTE (externally_visible)
# define _GL_ATTRIBUTE_EXTERNALLY_VISIBLE __attribute__ ((externally_visible))
#else
# define _GL_ATTRIBUTE_EXTERNALLY_VISIBLE
#endif

/* FALLTHROUGH is special, because it always expands to something.  */
#if 201710L < __STDC_VERSION__
# define _GL_ATTRIBUTE_FALLTHROUGH [[__fallthrough__]]
#elif _GL_HAS_ATTRIBUTE (fallthrough)
# define _GL_ATTRIBUTE_FALLTHROUGH __attribute__ ((__fallthrough__))
#else
# define _GL_ATTRIBUTE_FALLTHROUGH ((void) 0)
#endif

#if _GL_HAS_ATTRIBUTE (format)
# define _GL_ATTRIBUTE_FORMAT(spec) __attribute__ ((__format__ spec))
#else
# define _GL_ATTRIBUTE_FORMAT(spec)
#endif

#if _GL_HAS_ATTRIBUTE (leaf)
# define _GL_ATTRIBUTE_LEAF __attribute__ ((__leaf__))
#else
# define _GL_ATTRIBUTE_LEAF
#endif

/* Oracle Studio 12.6 mishandles may_alias despite __has_attribute OK.  */
#if _GL_HAS_ATTRIBUTE (may_alias) && !defined __SUNPRO_C
# define _GL_ATTRIBUTE_MAY_ALIAS __attribute__ ((__may_alias__))
#else
# define _GL_ATTRIBUTE_MAY_ALIAS
#endif

#if 201710L < __STDC_VERSION__
# define _GL_ATTRIBUTE_MAYBE_UNUSED [[__maybe_unused__]]
#elif _GL_HAS_ATTRIBUTE (unused)
# define _GL_ATTRIBUTE_MAYBE_UNUSED __attribute__ ((__unused__))
#else
# define _GL_ATTRIBUTE_MAYBE_UNUSED
#endif
/* Earlier spellings of this macro.  */
#define _GL_UNUSED _GL_ATTRIBUTE_MAYBE_UNUSED
#define _UNUSED_PARAMETER_ _GL_ATTRIBUTE_MAYBE_UNUSED

#if _GL_HAS_ATTRIBUTE (malloc)
# define _GL_ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
#else
# define _GL_ATTRIBUTE_MALLOC
#endif

#if 201710L < __STDC_VERSION__
# define _GL_ATTRIBUTE_NODISCARD [[__nodiscard__]]
#elif _GL_HAS_ATTRIBUTE (warn_unused_result)
# define _GL_ATTRIBUTE_NODISCARD __attribute__ ((__warn_unused_result__))
#else
# define _GL_ATTRIBUTE_NODISCARD
#endif

#if _GL_HAS_ATTRIBUTE (noinline)
# define _GL_ATTRIBUTE_NOINLINE __attribute__ ((__noinline__))
#else
# define _GL_ATTRIBUTE_NOINLINE
#endif

#if _GL_HAS_ATTRIBUTE (nonnull)
# define _GL_ATTRIBUTE_NONNULL(args) __attribute__ ((__nonnull__ args))
#else
# define _GL_ATTRIBUTE_NONNULL(args)
#endif

#if _GL_HAS_ATTRIBUTE (nonstring)
# define _GL_ATTRIBUTE_NONSTRING __attribute__ ((__nonstring__))
#else
# define _GL_ATTRIBUTE_NONSTRING
#endif

/* There is no _GL_ATTRIBUTE_NORETURN; use _Noreturn instead.  */

#if _GL_HAS_ATTRIBUTE (nothrow) && !defined __cplusplus
# define _GL_ATTRIBUTE_NOTHROW __attribute__ ((__nothrow__))
#else
# define _GL_ATTRIBUTE_NOTHROW
#endif

#if _GL_HAS_ATTRIBUTE (packed)
# define _GL_ATTRIBUTE_PACKED __attribute__ ((__packed__))
#else
# define _GL_ATTRIBUTE_PACKED
#endif

#if _GL_HAS_ATTRIBUTE (pure)
# define _GL_ATTRIBUTE_PURE __attribute__ ((__pure__))
#else
# define _GL_ATTRIBUTE_PURE
#endif

#if _GL_HAS_ATTRIBUTE (returns_nonnull)
# define _GL_ATTRIBUTE_RETURNS_NONNULL __attribute__ ((__returns_nonnull__))
#else
# define _GL_ATTRIBUTE_RETURNS_NONNULL
#endif

#if _GL_HAS_ATTRIBUTE (sentinel)
# define _GL_ATTRIBUTE_SENTINEL(pos) __attribute__ ((__sentinel__ pos))
#else
# define _GL_ATTRIBUTE_SENTINEL(pos)
#endif

]dnl There is no _GL_ATTRIBUTE_VISIBILITY; see m4/visibility.m4 instead.
[
/* To support C++ as well as C, use _GL_UNUSED_LABEL with trailing ';'.  */
#if !defined __cplusplus || _GL_GNUC_PREREQ (4, 5)
# define _GL_UNUSED_LABEL _GL_ATTRIBUTE_MAYBE_UNUSED
#else
# define _GL_UNUSED_LABEL
#endif
])
  AH_VERBATIM([async_safe],
[/* The _GL_ASYNC_SAFE marker should be attached to functions that are
   signal handlers (for signals other than SIGABRT, SIGPIPE) or can be
   invoked from such signal handlers.  Such functions have some restrictions:
     * All functions that it calls should be marked _GL_ASYNC_SAFE as well,
       or should be listed as async-signal-safe in POSIX
       <https://pubs.opengroup.org/onlinepubs/9699919799/functions/V2_chap02.html#tag_15_04>
       section 2.4.3.  Note that malloc(), sprintf(), and fwrite(), in
       particular, are NOT async-signal-safe.
     * All memory locations (variables and struct fields) that these functions
       access must be marked 'volatile'.  This holds for both read and write
       accesses.  Otherwise the compiler might optimize away stores to and
       reads from such locations that occur in the program, depending on its
       data flow analysis.  For example, when the program contains a loop
       that is intended to inspect a variable set from within a signal handler
           while (!signal_occurred)
             ;
       the compiler is allowed to transform this into an endless loop if the
       variable 'signal_occurred' is not declared 'volatile'.
   Additionally, recall that:
     * A signal handler should not modify errno (except if it is a handler
       for a fatal signal and ends by raising the same signal again, thus
       provoking the termination of the process).  If it invokes a function
       that may clobber errno, it needs to save and restore the value of
       errno.  */
#define _GL_ASYNC_SAFE
])
  AH_VERBATIM([micro_optimizations],
[/* _GL_CMP (n1, n2) performs a three-valued comparison on n1 vs. n2, where
   n1 and n2 are expressions without side effects, that evaluate to real
   numbers (excluding NaN).
   It returns
     1  if n1 > n2
     0  if n1 == n2
     -1 if n1 < n2
   The naïve code   (n1 > n2 ? 1 : n1 < n2 ? -1 : 0)  produces a conditional
   jump with nearly all GCC versions up to GCC 10.
   This variant     (n1 < n2 ? -1 : n1 > n2)  produces a conditional with many
   GCC versions up to GCC 9.
   The better code  (n1 > n2) - (n1 < n2)  from Hacker's Delight § 2-9
   avoids conditional jumps in all GCC versions >= 3.4.  */
#define _GL_CMP(n1, n2) (((n1) > (n2)) - ((n1) < (n2)))
])
  dnl Hint which direction to take regarding cross-compilation guesses:
  dnl When a user installs a program on a platform they are not intimately
  dnl familiar with, --enable-cross-guesses=conservative is the appropriate
  dnl choice.  It implements the "If we don't know, assume the worst" principle.
  dnl However, when an operating system developer (on a platform which is not
  dnl yet known to gnulib) builds packages for their platform, they want to
  dnl expose, not hide, possible platform bugs; in this case,
  dnl --enable-cross-guesses=risky is the appropriate choice.
  dnl Sets the variables
  dnl gl_cross_guess_normal    (to be used when 'yes' is good and 'no' is bad),
  dnl gl_cross_guess_inverted  (to be used when 'no' is good and 'yes' is bad).
  AC_ARG_ENABLE([cross-guesses],
    [AS_HELP_STRING([--enable-cross-guesses={conservative|risky}],
       [specify policy for cross-compilation guesses])],
    [if test "x$enableval" != xconservative && test "x$enableval" != xrisky; then
       AC_MSG_WARN([invalid argument supplied to --enable-cross-guesses])
       enableval=conservative
     fi
     gl_cross_guesses="$enableval"],
    [gl_cross_guesses=conservative])
  if test $gl_cross_guesses = risky; then
    gl_cross_guess_normal="guessing yes"
    gl_cross_guess_inverted="guessing no"
  else
    gl_cross_guess_normal="guessing no"
    gl_cross_guess_inverted="guessing yes"
  fi
  dnl Preparation for running test programs:
  dnl Tell glibc to write diagnostics from -D_FORTIFY_SOURCE=2 to stderr, not
  dnl to /dev/tty, so they can be redirected to log files.  Such diagnostics
  dnl arise e.g., in the macros gl_PRINTF_DIRECTIVE_N, gl_SNPRINTF_DIRECTIVE_N.
  LIBC_FATAL_STDERR_=1
  export LIBC_FATAL_STDERR_
])

# gl_MODULE_INDICATOR_INIT_VARIABLE([variablename])
# gl_MODULE_INDICATOR_INIT_VARIABLE([variablename], [initialvalue])
# initializes the shell variable that indicates the presence of the given module
# as a C preprocessor expression.
AC_DEFUN([gl_MODULE_INDICATOR_INIT_VARIABLE],
[
  GL_MODULE_INDICATOR_PREFIX[]_[$1]=m4_if([$2], , [0], [$2])
  AC_SUBST(GL_MODULE_INDICATOR_PREFIX[]_[$1])
])

# gl_MODULE_INDICATOR_CONDITION
# expands to a C preprocessor expression that evaluates to 1 or 0, depending
# whether a gnulib module that has been requested shall be considered present
# or not.
m4_define([gl_MODULE_INDICATOR_CONDITION], [1])

# gl_MODULE_INDICATOR_SET_VARIABLE([modulename])
# sets the shell variable that indicates the presence of the given module to
# a C preprocessor expression that will evaluate to 1.
AC_DEFUN([gl_MODULE_INDICATOR_SET_VARIABLE],
[
  gl_MODULE_INDICATOR_SET_VARIABLE_AUX(
    [GL_MODULE_INDICATOR_PREFIX[]_GNULIB_[]m4_translit([[$1]],
                                                       [abcdefghijklmnopqrstuvwxyz./-],
                                                       [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])],
    [gl_MODULE_INDICATOR_CONDITION])
])

# gl_MODULE_INDICATOR_SET_VARIABLE_AUX([variable])
# modifies the shell variable to include the gl_MODULE_INDICATOR_CONDITION.
# The shell variable's value is a C preprocessor expression that evaluates
# to 0 or 1.
AC_DEFUN([gl_MODULE_INDICATOR_SET_VARIABLE_AUX],
[
  m4_if(m4_defn([gl_MODULE_INDICATOR_CONDITION]), [1],
    [
     dnl Simplify the expression VALUE || 1 to 1.
     $1=1
    ],
    [gl_MODULE_INDICATOR_SET_VARIABLE_AUX_OR([$1],
                                             [gl_MODULE_INDICATOR_CONDITION])])
])

# gl_MODULE_INDICATOR_SET_VARIABLE_AUX_OR([variable], [condition])
# modifies the shell variable to include the given condition.  The shell
# variable's value is a C preprocessor expression that evaluates to 0 or 1.
AC_DEFUN([gl_MODULE_INDICATOR_SET_VARIABLE_AUX_OR],
[
  dnl Simplify the expression 1 || CONDITION to 1.
  if test "$[]$1" != 1; then
    dnl Simplify the expression 0 || CONDITION to CONDITION.
    if test "$[]$1" = 0; then
      $1=$2
    else
      $1="($[]$1 || $2)"
    fi
  fi
])

# gl_MODULE_INDICATOR([modulename])
# defines a C macro indicating the presence of the given module
# in a location where it can be used.
#                                             |  Value  |   Value   |
#                                             | in lib/ | in tests/ |
# --------------------------------------------+---------+-----------+
# Module present among main modules:          |    1    |     1     |
# --------------------------------------------+---------+-----------+
# Module present among tests-related modules: |    0    |     1     |
# --------------------------------------------+---------+-----------+
# Module not present at all:                  |    0    |     0     |
# --------------------------------------------+---------+-----------+
AC_DEFUN([gl_MODULE_INDICATOR],
[
  AC_DEFINE_UNQUOTED([GNULIB_]m4_translit([[$1]],
      [abcdefghijklmnopqrstuvwxyz./-],
      [ABCDEFGHIJKLMNOPQRSTUVWXYZ___]),
    [gl_MODULE_INDICATOR_CONDITION],
    [Define to a C preprocessor expression that evaluates to 1 or 0,
     depending whether the gnulib module $1 shall be considered present.])
])

# gl_MODULE_INDICATOR_FOR_TESTS([modulename])
# defines a C macro indicating the presence of the given module
# in lib or tests. This is useful to determine whether the module
# should be tested.
#                                             |  Value  |   Value   |
#                                             | in lib/ | in tests/ |
# --------------------------------------------+---------+-----------+
# Module present among main modules:          |    1    |     1     |
# --------------------------------------------+---------+-----------+
# Module present among tests-related modules: |    1    |     1     |
# --------------------------------------------+---------+-----------+
# Module not present at all:                  |    0    |     0     |
# --------------------------------------------+---------+-----------+
AC_DEFUN([gl_MODULE_INDICATOR_FOR_TESTS],
[
  AC_DEFINE([GNULIB_TEST_]m4_translit([[$1]],
      [abcdefghijklmnopqrstuvwxyz./-],
      [ABCDEFGHIJKLMNOPQRSTUVWXYZ___]), [1],
    [Define to 1 when the gnulib module $1 should be tested.])
])

# gl_ASSERT_NO_GNULIB_POSIXCHECK
# asserts that there will never be a need to #define GNULIB_POSIXCHECK.
# and thereby enables an optimization of configure and config.h.
# Used by Emacs.
AC_DEFUN([gl_ASSERT_NO_GNULIB_POSIXCHECK],
[
  dnl Override gl_WARN_ON_USE_PREPARE.
  dnl But hide this definition from 'aclocal'.
  AC_DEFUN([gl_W][ARN_ON_USE_PREPARE], [])
])

# gl_ASSERT_NO_GNULIB_TESTS
# asserts that there will be no gnulib tests in the scope of the configure.ac
# and thereby enables an optimization of config.h.
# Used by Emacs.
AC_DEFUN([gl_ASSERT_NO_GNULIB_TESTS],
[
  dnl Override gl_MODULE_INDICATOR_FOR_TESTS.
  AC_DEFUN([gl_MODULE_INDICATOR_FOR_TESTS], [])
])

# Test whether <features.h> exists.
# Set HAVE_FEATURES_H.
AC_DEFUN([gl_FEATURES_H],
[
  AC_CHECK_HEADERS_ONCE([features.h])
  if test $ac_cv_header_features_h = yes; then
    HAVE_FEATURES_H=1
  else
    HAVE_FEATURES_H=0
  fi
  AC_SUBST([HAVE_FEATURES_H])
])

# gl_PROG_CC_C99
# Modifies the value of the shell variable CC in an attempt to make $CC
# understand ISO C99 source code.
AC_DEFUN([gl_PROG_CC_C99],
[
  dnl Just use AC_PROG_CC_C99.
  dnl When AC_PROG_CC_C99 and AC_PROG_CC_STDC are used together, the substituted
  dnl value of CC will contain the C99 enabling options twice. But this is only
  dnl a cosmetic problem.
  dnl With Autoconf >= 2.70, use AC_PROG_CC since it implies AC_PROG_CC_C99;
  dnl this avoids a "warning: The macro `AC_PROG_CC_C99' is obsolete."
  m4_version_prereq([2.70],
    [AC_REQUIRE([AC_PROG_CC])],
    [AC_REQUIRE([AC_PROG_CC_C99])])
])

# gl_PROG_AR_RANLIB
# Determines the values for AR, ARFLAGS, RANLIB that fit with the compiler.
# The user can set the variables AR, ARFLAGS, RANLIB if he wants to override
# the values.
AC_DEFUN([gl_PROG_AR_RANLIB],
[
  dnl Minix 3 comes with two toolchains: The Amsterdam Compiler Kit compiler
  dnl as "cc", and GCC as "gcc". They have different object file formats and
  dnl library formats. In particular, the GNU binutils programs ar and ranlib
  dnl produce libraries that work only with gcc, not with cc.
  AC_REQUIRE([AC_PROG_CC])
  dnl The '][' hides this use from 'aclocal'.
  AC_BEFORE([$0], [A][M_PROG_AR])
  AC_CACHE_CHECK([for Minix Amsterdam compiler], [gl_cv_c_amsterdam_compiler],
    [
      AC_EGREP_CPP([Amsterdam],
        [
#ifdef __ACK__
Amsterdam
#endif
        ],
        [gl_cv_c_amsterdam_compiler=yes],
        [gl_cv_c_amsterdam_compiler=no])
    ])

  dnl Don't compete with AM_PROG_AR's decision about AR/ARFLAGS if we are not
  dnl building with __ACK__.
  if test $gl_cv_c_amsterdam_compiler = yes; then
    if test -z "$AR"; then
      AR='cc -c.a'
    fi
    if test -z "$ARFLAGS"; then
      ARFLAGS='-o'
    fi
  else
    dnl AM_PROG_AR was added in automake v1.11.2.  AM_PROG_AR does not AC_SUBST
    dnl ARFLAGS variable (it is filed into Makefile.in directly by automake
    dnl script on-demand, if not specified by ./configure of course).
    dnl Don't AC_REQUIRE the AM_PROG_AR otherwise the code for __ACK__ above
    dnl will be ignored.  Also, pay attention to call AM_PROG_AR in else block
    dnl because AM_PROG_AR is written so it could re-set AR variable even for
    dnl __ACK__.  It may seem like its easier to avoid calling the macro here,
    dnl but we need to AC_SUBST both AR/ARFLAGS (thus those must have some good
    dnl default value and automake should usually know them).
    dnl
    dnl The '][' hides this use from 'aclocal'.
    m4_ifdef([A][M_PROG_AR], [A][M_PROG_AR], [:])
  fi

  dnl In case the code above has not helped with setting AR/ARFLAGS, use
  dnl Automake-documented default values for AR and ARFLAGS, but prefer
  dnl ${host}-ar over ar (useful for cross-compiling).
  AC_CHECK_TOOL([AR], [ar], [ar])
  if test -z "$ARFLAGS"; then
    ARFLAGS='cr'
  fi

  AC_SUBST([AR])
  AC_SUBST([ARFLAGS])
  if test -z "$RANLIB"; then
    if test $gl_cv_c_amsterdam_compiler = yes; then
      RANLIB=':'
    else
      dnl Use the ranlib program if it is available.
      AC_PROG_RANLIB
    fi
  fi
  AC_SUBST([RANLIB])
])

# AC_C_RESTRICT
# This definition is copied from post-2.70 Autoconf and overrides the
# AC_C_RESTRICT macro from autoconf 2.60..2.70.
m4_version_prereq([2.70.1], [], [
AC_DEFUN([AC_C_RESTRICT],
[AC_CACHE_CHECK([for C/C++ restrict keyword], [ac_cv_c_restrict],
  [ac_cv_c_restrict=no
   # Put '__restrict__' first, to avoid problems with glibc and non-GCC; see:
   # https://lists.gnu.org/archive/html/bug-autoconf/2016-02/msg00006.html
   # Put 'restrict' last, because C++ lacks it.
   for ac_kw in __restrict__ __restrict _Restrict restrict; do
     AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM(
         [[typedef int *int_ptr;
           int foo (int_ptr $ac_kw ip) { return ip[0]; }
           int bar (int [$ac_kw]); /* Catch GCC bug 14050.  */
           int bar (int ip[$ac_kw]) { return ip[0]; }
         ]],
         [[int s[1];
           int *$ac_kw t = s;
           t[0] = 0;
           return foo (t) + bar (t);
         ]])],
      [ac_cv_c_restrict=$ac_kw])
     test "$ac_cv_c_restrict" != no && break
   done
  ])
 AH_VERBATIM([restrict],
[/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported only directly.  */
#undef restrict
/* Work around a bug in older versions of Sun C++, which did not
   #define __restrict__ or support _Restrict or __restrict__
   even though the corresponding Sun C compiler ended up with
   "#define restrict _Restrict" or "#define restrict __restrict__"
   in the previous line.  This workaround can be removed once
   we assume Oracle Developer Studio 12.5 (2016) or later.  */
#if defined __SUNPRO_CC && !defined __RESTRICT && !defined __restrict__
# define _Restrict
# define __restrict__
#endif])
 case $ac_cv_c_restrict in
   restrict) ;;
   no) AC_DEFINE([restrict], []) ;;
   *)  AC_DEFINE_UNQUOTED([restrict], [$ac_cv_c_restrict]) ;;
 esac
])# AC_C_RESTRICT
])

# gl_BIGENDIAN
# is like AC_C_BIGENDIAN, except that it can be AC_REQUIREd.
# Note that AC_REQUIRE([AC_C_BIGENDIAN]) does not work reliably because some
# macros invoke AC_C_BIGENDIAN with arguments.
AC_DEFUN([gl_BIGENDIAN],
[
  AC_C_BIGENDIAN
])

# A temporary file descriptor.
# Must be less than 10, because dash 0.5.8 does not support redirections
# with multi-digit file descriptors.
m4_define([GL_TMP_FD], 9)

# gl_SILENT(command)
# executes command, but without the normal configure output.
# This is useful when you want to invoke AC_CACHE_CHECK (or AC_CHECK_FUNC etc.)
# inside another AC_CACHE_CHECK.
AC_DEFUN([gl_SILENT],
[
  exec GL_TMP_FD>&AS_MESSAGE_FD AS_MESSAGE_FD>/dev/null
  $1
  exec AS_MESSAGE_FD>&GL_TMP_FD GL_TMP_FD>&-
])

# gl_CACHE_VAL_SILENT(cache-id, command-to-set-it)
# is like AC_CACHE_VAL(cache-id, command-to-set-it), except that it does not
# output a spurious "(cached)" mark in the midst of other configure output.
# This macro should be used instead of AC_CACHE_VAL when it is not surrounded
# by an AC_MSG_CHECKING/AC_MSG_RESULT pair.
AC_DEFUN([gl_CACHE_VAL_SILENT],
[
  gl_SILENT([
    AC_CACHE_VAL([$1], [$2])
  ])
])

dnl Expands to some code for use in .c programs that, on native Windows, defines
dnl the Microsoft deprecated alias function names to the underscore-prefixed
dnl actual function names. With this macro, these function names are available
dnl without linking with '-loldnames' and without generating warnings.
dnl Usage: Use it after all system header files are included.
dnl          #include <...>
dnl          #include <...>
dnl          ]GL_MDA_DEFINES[
dnl          ...
AC_DEFUN([GL_MDA_DEFINES],[
AC_REQUIRE([_GL_MDA_DEFINES])
[$gl_mda_defines]
])
AC_DEFUN([_GL_MDA_DEFINES],
[gl_mda_defines='
#if defined _WIN32 && !defined __CYGWIN__
#define access    _access
#define chdir     _chdir
#define chmod     _chmod
#define close     _close
#define creat     _creat
#define dup       _dup
#define dup2      _dup2
#define ecvt      _ecvt
#define execl     _execl
#define execle    _execle
#define execlp    _execlp
#define execv     _execv
#define execve    _execve
#define execvp    _execvp
#define execvpe   _execvpe
#define fcloseall _fcloseall
#define fcvt      _fcvt
#define fdopen    _fdopen
#define fileno    _fileno
#define gcvt      _gcvt
#define getcwd    _getcwd
#define getpid    _getpid
#define getw      _getw
#define isatty    _isatty
#define j0        _j0
#define j1        _j1
#define jn        _jn
#define lfind     _lfind
#define lsearch   _lsearch
#define lseek     _lseek
#define memccpy   _memccpy
#define mkdir     _mkdir
#define mktemp    _mktemp
#define open      _open
#define putenv    _putenv
#define putw      _putw
#define read      _read
#define rmdir     _rmdir
#define strdup    _strdup
#define swab      _swab
#define tempnam   _tempnam
#define tzset     _tzset
#define umask     _umask
#define unlink    _unlink
#define utime     _utime
#define wcsdup    _wcsdup
#define write     _write
#define y0        _y0
#define y1        _y1
#define yn        _yn
#endif
'
])
