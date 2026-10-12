# libunistring-base.m4
# serial 10
dnl Copyright (C) 2010-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Paolo Bonzini and Bruno Haible.

dnl gl_LIBUNISTRING_MODULE([VERSION], [Module])
dnl Declares that the source files of Module should be compiled, unless we
dnl are linking with libunistring and its version is >= the given VERSION.
dnl Defines an automake conditional LIBUNISTRING_COMPILE_$MODULE that is
dnl true if the source files of Module should be compiled.
dnl This macro is to be used for public libunistring API, not for
dnl undocumented API.
dnl
dnl You have to bump the VERSION argument to the next projected version
dnl number each time you make a change that affects the behaviour of the
dnl functions defined in Module (even if the sources of Module itself do not
dnl change).
dnl
dnl This macro invocation must not occur in macros that are AC_REQUIREd.

AC_DEFUN([gl_LIBUNISTRING_MODULE],
[
  AC_REQUIRE([gl_LIBUNISTRING_LIB_PREPARE])
  dnl Use the variables HAVE_LIBUNISTRING, LIBUNISTRING_VERSION from
  dnl gl_LIBUNISTRING_CORE if that macro has been run.
  gl_CONDITIONAL(AS_TR_CPP([LIBUNISTRING_COMPILE_$2]),
    [gl_LIBUNISTRING_VERSION_CMP([$1])])
])

dnl gl_LIBUNISTRING_MODULE_WITH_VARIABLE([VERSION], [Module])
dnl is like gl_LIBUNISTRING_MODULE([VERSION], [Module]), except that it also
dnl defines an AC_SUBSTed autoconf variable GNULIB_$MODULE_DLL_VARIABLE.
dnl What's the expansion of this autoconf variable?
dnl   - When building libunistring, it expands to LIBUNISTRING_DLL_VARIABLE.
dnl     (This is necessary because this token must be present in the .h files
dnl     when the .h files get installed.)
dnl   - When building gnulib or application code it expands to
dnl       - LIBUNISTRING_DLL_VARIABLE by default,
dnl       - if the automake conditional LIBUNISTRING_COMPILE_$MODULE evaluates
dnl         to true: the value of
dnl         ${module_indicator_prefix}_GNULIB_LIBUNISTRING_DLL_VARIABLE_NAME
dnl         (which usually is empty, unless explicitly set in configure.ac).
dnl     (This is necessary because when the conditional evaluates to false,
dnl     the application code expects to use the declared variable from the
dnl     installed libunistring; it's in this case that the
dnl     LIBUNISTRING_DLL_VARIABLE macro from the installed
dnl     <unistring/woe32dll.h> must be used.)
dnl
dnl This macro invocation must not occur in macros that are AC_REQUIREd.

AC_DEFUN([gl_LIBUNISTRING_MODULE_WITH_VARIABLE],
[
  gl_LIBUNISTRING_MODULE([$1], [$2])
  m4_ifndef([gl_IN_LIBUNISTRING],
    [if test -z "${AS_TR_CPP([LIBUNISTRING_COMPILE_$2])_TRUE}"; then
       GL_MODULE_INDICATOR_PREFIX[]_GNULIB_[]AS_TR_CPP([$2_DLL_VARIABLE])=$GL_MODULE_INDICATOR_PREFIX[]_GNULIB_LIBUNISTRING_DLL_VARIABLE_NAME
     fi
    ])
])

dnl gl_LIBUNISTRING_LIBHEADER([VERSION], [HeaderFile])
dnl Declares that HeaderFile should be created, unless we are linking
dnl with libunistring and its version is >= the given VERSION.
dnl HeaderFile should be relative to the lib directory and end in '.h'.
dnl Prepares for substituting LIBUNISTRING_HEADERFILE (to HeaderFile or empty).
dnl
dnl When we are linking with the already installed libunistring and its version
dnl is < VERSION, we create HeaderFile here, because we may compile functions
dnl (via gl_LIBUNISTRING_MODULE above) that are not contained in the installed
dnl version.
dnl When we are linking with the already installed libunistring and its version
dnl is > VERSION, we don't create HeaderFile here: it could cause compilation
dnl errors in other libunistring header files if some types are missing.
dnl
dnl You have to bump the VERSION argument to the next projected version
dnl number each time you make a non-comment change to the HeaderFile.

AC_DEFUN([gl_LIBUNISTRING_LIBHEADER],
[
  AC_REQUIRE([gl_LIBUNISTRING_LIB_PREPARE])
  dnl Use the variables HAVE_LIBUNISTRING, LIBUNISTRING_VERSION from
  dnl gl_LIBUNISTRING_CORE if that macro has been run.
  if gl_LIBUNISTRING_VERSION_CMP([$1]); then
    dnl It is OK to use a .h file in lib/ from within tests/, but not vice
    dnl versa.
    if test -z "$LIBUNISTRING_[]AS_TR_CPP([$2])"; then
      LIBUNISTRING_[]AS_TR_CPP([$2])="${gl_source_base_prefix}$2"
    fi
  else
    LIBUNISTRING_[]AS_TR_CPP([$2])=
  fi
  AC_SUBST([LIBUNISTRING_]AS_TR_CPP([$2]))
])

dnl Miscellaneous preparations/initializations.

AC_DEFUN([gl_LIBUNISTRING_LIB_PREPARE],
[
  dnl Ensure that HAVE_LIBUNISTRING is fully determined at this point.
  m4_ifdef([gl_LIBUNISTRING], [AC_REQUIRE([gl_LIBUNISTRING])])

  AC_REQUIRE([AC_PROG_AWK])

dnl Sed expressions to extract the parts of a version number.
changequote(,)
gl_libunistring_sed_extract_major='/^[0-9]/{s/^\([0-9]*\).*/\1/p;q;}
i\
0
q
'
gl_libunistring_sed_extract_minor='/^[0-9][0-9]*[.][0-9]/{s/^[0-9]*[.]\([0-9]*\).*/\1/p;q;}
i\
0
q
'
gl_libunistring_sed_extract_subminor='/^[0-9][0-9]*[.][0-9][0-9]*[.][0-9]/{s/^[0-9]*[.][0-9]*[.]\([0-9]*\).*/\1/p;q;}
i\
0
q
'
changequote([,])

  if test "$HAVE_LIBUNISTRING" = yes; then
    LIBUNISTRING_VERSION_MAJOR=`echo "$LIBUNISTRING_VERSION" | sed -n -e "$gl_libunistring_sed_extract_major"`
    LIBUNISTRING_VERSION_MINOR=`echo "$LIBUNISTRING_VERSION" | sed -n -e "$gl_libunistring_sed_extract_minor"`
    LIBUNISTRING_VERSION_SUBMINOR=`echo "$LIBUNISTRING_VERSION" | sed -n -e "$gl_libunistring_sed_extract_subminor"`
  fi

  dnl Determine whether <unistring/woe32dll.h> from an installed libunistring
  dnl is available.
  m4_ifdef([gl_IN_LIBUNISTRING],
    [dnl In libunistring, all .h files that declare variables need to
     dnl #include <unistring/woe32dll.h>.  This references the file
     dnl unistring/woe32dll.h in libunistring.
     HAVE_UNISTRING_WOE32DLL_H=1
    ],
    [dnl In gnulib or in applications, we need a #include <unistring/woe32dll.h>
     dnl if and only if an installed libunistring is available.
     if test "$HAVE_LIBUNISTRING" = yes; then
       AC_CHECK_HEADERS([unistring/woe32dll.h],
         [HAVE_UNISTRING_WOE32DLL_H=1],
         [HAVE_UNISTRING_WOE32DLL_H=0])
     else
       HAVE_UNISTRING_WOE32DLL_H=0
     fi
    ])
  AC_SUBST([HAVE_UNISTRING_WOE32DLL_H])
])

dnl gl_LIBUNISTRING_VERSION_CMP([VERSION])
dnl Expands to a shell statement that evaluates to true if LIBUNISTRING_VERSION
dnl is less than the VERSION argument.
AC_DEFUN([gl_LIBUNISTRING_VERSION_CMP],
[dnl VERSION = 999.9 means to evaluates to true always, i.e. to ignore
dnl the installed libunistring regardless of its version.
m4_if([$1], [999.9],
[true],
[ { test "$HAVE_LIBUNISTRING" != yes \
    || {
         dnl AS_LITERAL_IF exists and works fine since autoconf-2.59 at least.
         AS_LITERAL_IF([$1],
           [dnl This is the optimized variant, that assumes the argument is a literal:
            m4_pushdef([requested_version_major],
              [gl_LIBUNISTRING_ARG_OR_ZERO(m4_bpatsubst([$1], [^\([0-9]*\).*], [\1]), [])])
            m4_pushdef([requested_version_minor],
              [gl_LIBUNISTRING_ARG_OR_ZERO(m4_bpatsubst([$1], [^[0-9]*[.]\([0-9]*\).*], [\1]), [$1])])
            m4_pushdef([requested_version_subminor],
              [gl_LIBUNISTRING_ARG_OR_ZERO(m4_bpatsubst([$1], [^[0-9]*[.][0-9]*[.]\([0-9]*\).*], [\1]), [$1])])
            test $LIBUNISTRING_VERSION_MAJOR -lt requested_version_major \
            || { test $LIBUNISTRING_VERSION_MAJOR -eq requested_version_major \
                 && { test $LIBUNISTRING_VERSION_MINOR -lt requested_version_minor \
                      || { test $LIBUNISTRING_VERSION_MINOR -eq requested_version_minor \
                           && test $LIBUNISTRING_VERSION_SUBMINOR -lt requested_version_subminor
                         }
                    }
               }
            m4_popdef([requested_version_subminor])
            m4_popdef([requested_version_minor])
            m4_popdef([requested_version_major])
           ],
           [dnl This is the unoptimized variant:
            requested_version_major=`echo '$1' | sed -n -e "$gl_libunistring_sed_extract_major"`
            requested_version_minor=`echo '$1' | sed -n -e "$gl_libunistring_sed_extract_minor"`
            requested_version_subminor=`echo '$1' | sed -n -e "$gl_libunistring_sed_extract_subminor"`
            test $LIBUNISTRING_VERSION_MAJOR -lt $requested_version_major \
            || { test $LIBUNISTRING_VERSION_MAJOR -eq $requested_version_major \
                 && { test $LIBUNISTRING_VERSION_MINOR -lt $requested_version_minor \
                      || { test $LIBUNISTRING_VERSION_MINOR -eq $requested_version_minor \
                           && test $LIBUNISTRING_VERSION_SUBMINOR -lt $requested_version_subminor
                         }
                    }
               }
           ])
       }
  }])])

dnl gl_LIBUNISTRING_ARG_OR_ZERO([ARG], [ORIG]) expands to ARG if it is not the
dnl same as ORIG, otherwise to 0.
m4_define([gl_LIBUNISTRING_ARG_OR_ZERO], [m4_if([$1], [$2], [0], [$1])])
