# uchar_h.m4
# serial 33
dnl Copyright (C) 2019-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Bruno Haible.
dnl Prepare the overridden <uchar.h>.

AC_DEFUN_ONCE([gl_UCHAR_H],
[
  AC_REQUIRE([gl_UCHAR_H_DEFAULTS])

  gl_CHECK_NEXT_HEADERS([uchar.h])
  if test $ac_cv_header_uchar_h = yes; then
    HAVE_UCHAR_H=1
  else
    HAVE_UCHAR_H=0
  fi
  AC_SUBST([HAVE_UCHAR_H])

  dnl On macOS 15, in C mode, <uchar.h> does not exist. But in C++ mode,
  dnl it exists, and we need to #include_next it, otherwise we get an error
  dnl "<cuchar> tried including <uchar.h> but didn't find libc++'s <uchar.h>
  dnl  header."
  m4_ifdef([gl_ANSI_CXX], [AC_REQUIRE([gl_ANSI_CXX])])
  CXX_HAVE_UCHAR_H=0
  if test -n "$CXX" && test "$CXX" != no; then
    AC_CACHE_CHECK([whether the C++ compiler has <uchar.h>],
      [gl_cv_cxx_have_uchar_h],
      [dnl We can't use AC_LANG_PUSH([C++]) and AC_LANG_POP([C++]) here, due to
       dnl an autoconf bug <https://savannah.gnu.org/support/?110294>.
       cat > conftest.cpp <<\EOF
#include <uchar.h>
EOF
       gl_command="$CXX $CXXFLAGS $CPPFLAGS -c conftest.cpp"
       if AC_TRY_EVAL([gl_command]); then
         gl_cv_cxx_have_uchar_h=yes
       else
         gl_cv_cxx_have_uchar_h=no
       fi
       rm -fr conftest*
      ])
    if test $gl_cv_cxx_have_uchar_h = yes; then
      CXX_HAVE_UCHAR_H=1
    fi
  fi
  AC_SUBST([CXX_HAVE_UCHAR_H])

  gl_TYPE_CHAR8_T
  gl_TYPE_CHAR16_T
  gl_TYPE_CHAR32_T

  dnl In C++ mode, clang defines 'char16_t' and 'char32_t' as built-in types
  dnl on some platforms (e.g. OpenBSD 6.7), and as types defined by many
  dnl header files (<limits.h>, <stddef.h>, <stdint.h>, <stdio.h>, <stdlib.h>
  dnl and others) on some platforms (e.g. Mac OS X 10.13).
  dnl The same thing may also happen for 'char8_t'; so, be prepared for it.
  m4_ifdef([gl_ANSI_CXX], [AC_REQUIRE([gl_ANSI_CXX])])
  CXX_HAS_UCHAR_TYPES=0
  if test $HAVE_UCHAR_H = 0; then
    if test -n "$CXX" && test "$CXX" != no; then
      AC_CACHE_CHECK([whether the C++ compiler predefines the <uchar.h> types],
        [gl_cv_cxx_has_uchar_types],
        [dnl We can't use AC_LANG_PUSH([C++]) and AC_LANG_POP([C++]) here, due to
         dnl an autoconf bug <https://savannah.gnu.org/support/?110294>.
         cat > conftest.cpp <<\EOF
#include <stddef.h>
char16_t a;
char32_t b;
EOF
         gl_command="$CXX $CXXFLAGS $CPPFLAGS -c conftest.cpp"
         if AC_TRY_EVAL([gl_command]); then
           gl_cv_cxx_has_uchar_types=yes
         else
           gl_cv_cxx_has_uchar_types=no
         fi
         rm -fr conftest*
        ])
      if test $gl_cv_cxx_has_uchar_types = yes; then
        CXX_HAS_UCHAR_TYPES=1
      fi
    fi
  fi
  AC_SUBST([CXX_HAS_UCHAR_TYPES])
  CXX_HAS_CHAR8_TYPE=0
  if test $HAVE_UCHAR_H = 0; then
    if test -n "$CXX" && test "$CXX" != no; then
      AC_CACHE_CHECK([whether the C++ compiler predefines the char8_t type],
        [gl_cv_cxx_has_char8_type],
        [dnl We can't use AC_LANG_PUSH([C++]) and AC_LANG_POP([C++]) here, due to
         dnl an autoconf bug <https://savannah.gnu.org/support/?110294>.
         cat > conftest.cpp <<\EOF
#include <stddef.h>
char8_t a;
EOF
         gl_command="$CXX $CXXFLAGS $CPPFLAGS -c conftest.cpp"
         if AC_TRY_EVAL([gl_command]); then
           gl_cv_cxx_has_char8_type=yes
         else
           gl_cv_cxx_has_char8_type=no
         fi
         rm -fr conftest*
        ])
      if test $gl_cv_cxx_has_char8_type = yes; then
        CXX_HAS_CHAR8_TYPE=1
      fi
    fi
  fi
  AC_SUBST([CXX_HAS_CHAR8_TYPE])

  dnl Test whether a 'char32_t' can hold more characters than a 'wchar_t'.
  gl_STDINT_BITSIZEOF([wchar_t], [gl_STDINT_INCLUDES])
  if test $BITSIZEOF_WCHAR_T -lt 32; then
    SMALL_WCHAR_T=1
  else
    SMALL_WCHAR_T=0
  fi
  dnl SMALL_WCHAR_T is expected to be 1 on 32-bit AIX, Cygwin, native Windows.
  AC_SUBST([SMALL_WCHAR_T])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use, and which is not
  dnl guaranteed by C11.
  gl_WARN_ON_USE_PREPARE([[
      #ifdef __HAIKU__
       #include <stdint.h>
      #endif
      #include <uchar.h>
    ]], [c32rtomb mbrtoc16 mbrtoc32])
])

AC_DEFUN_ONCE([gl_TYPE_CHAR8_T],
[
  dnl Determine whether gnulib's <uchar.h> would, if present, override char8_t.
  AC_CACHE_CHECK([whether char8_t is correctly defined],
    [gl_cv_type_char8_t_works],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
          #ifdef __HAIKU__
           #include <stdint.h>
          #endif
          #include <uchar.h>
          int verify[(char8_t)(-1) >= 0 && sizeof (char8_t) == sizeof (unsigned char) ? 1 : -1];
          ]])
       ],
       [gl_cv_type_char8_t_works=yes],
       [gl_cv_type_char8_t_works=no])
    ])
  if test $gl_cv_type_char8_t_works = no; then
    GNULIBHEADERS_OVERRIDE_CHAR8_T=1
  else
    GNULIBHEADERS_OVERRIDE_CHAR8_T=0
  fi
  AC_SUBST([GNULIBHEADERS_OVERRIDE_CHAR8_T])
])

dnl On Haiku 2020, char16_t and char32_t are incorrectly defined.
dnl See <https://dev.haiku-os.org/ticket/15990>.
AC_DEFUN_ONCE([gl_TYPE_CHAR16_T],
[
  dnl Determine whether gnulib's <uchar.h> would, if present, override char16_t.
  AC_CACHE_CHECK([whether char16_t is correctly defined],
    [gl_cv_type_char16_t_works],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
          #ifdef __HAIKU__
           #include <stdint.h>
          #endif
          #include <uchar.h>
          /* For simplicity, assume that uint16_least_t is equivalent to
             'unsigned short'.  */
          int verify[(char16_t)(-1) >= 0 && sizeof (char16_t) == sizeof (unsigned short) ? 1 : -1];
          ]])
       ],
       [gl_cv_type_char16_t_works=yes],
       [gl_cv_type_char16_t_works=no])
    ])
  if test $gl_cv_type_char16_t_works = no; then
    GNULIBHEADERS_OVERRIDE_CHAR16_T=1
  else
    GNULIBHEADERS_OVERRIDE_CHAR16_T=0
  fi
  AC_SUBST([GNULIBHEADERS_OVERRIDE_CHAR16_T])
])
AC_DEFUN_ONCE([gl_TYPE_CHAR32_T],
[
  dnl Determine whether gnulib's <uchar.h> would, if present, override char32_t.
  AC_CACHE_CHECK([whether char32_t is correctly defined],
    [gl_cv_type_char32_t_works],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
          #ifdef __HAIKU__
           #include <stdint.h>
          #endif
          #include <uchar.h>
          /* For simplicity, assume that uint32_least_t is equivalent to
             'unsigned int'.  */
          int verify[(char32_t)(-1) >= 0 && sizeof (char32_t) == sizeof (unsigned int) ? 1 : -1];
          ]])
       ],
       [gl_cv_type_char32_t_works=yes],
       [gl_cv_type_char32_t_works=no])
    ])
  if test $gl_cv_type_char32_t_works = no; then
    GNULIBHEADERS_OVERRIDE_CHAR32_T=1
  else
    GNULIBHEADERS_OVERRIDE_CHAR32_T=0
  fi
  AC_SUBST([GNULIBHEADERS_OVERRIDE_CHAR32_T])
])

# gl_UCHAR_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_UCHAR_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_UCHAR_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_UCHAR_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_UCHAR_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_BTOC32])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISALNUM])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISALPHA])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISBLANK])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISCNTRL])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISDIGIT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISGRAPH])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISLOWER])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISPRINT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISPUNCT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISSPACE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISUPPER])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32ISXDIGIT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32TOLOWER])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32TOUPPER])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32WIDTH])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32RTOMB])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32SNRTOMBS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32SRTOMBS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32STOMBS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32SWIDTH])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32TOB])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32_APPLY_MAPPING])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32_APPLY_TYPE_TEST])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32_GET_MAPPING])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_C32_GET_TYPE_TEST])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBRTOC16])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBRTOC32])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBSNRTOC32S])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBSRTOC32S])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBSTOC32S])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_UCHAR_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_UCHAR_H_DEFAULTS])
])

AC_DEFUN([gl_UCHAR_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_C32RTOMB=1;             AC_SUBST([HAVE_C32RTOMB])
  HAVE_MBRTOC16=1;             AC_SUBST([HAVE_MBRTOC16])
  HAVE_MBRTOC32=1;             AC_SUBST([HAVE_MBRTOC32])
  REPLACE_C32RTOMB=0;          AC_SUBST([REPLACE_C32RTOMB])
  REPLACE_MBRTOC16=0;          AC_SUBST([REPLACE_MBRTOC16])
  REPLACE_MBRTOC32=0;          AC_SUBST([REPLACE_MBRTOC32])
])
