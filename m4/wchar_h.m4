dnl A placeholder for ISO C99 <wchar.h>, for platforms that have issues.

dnl Copyright (C) 2007-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Eric Blake.

# wchar_h.m4 serial 53

AC_DEFUN_ONCE([gl_WCHAR_H],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  AC_REQUIRE([gl_WCHAR_H_INLINE_OK])
  dnl Prepare for creating substitute <wchar.h>.
  dnl Check for <wchar.h> (missing in Linux uClibc when built without wide
  dnl character support).
  dnl <wchar.h> is always overridden, because of GNULIB_POSIXCHECK.
  gl_CHECK_NEXT_HEADERS([wchar.h])
  if test $ac_cv_header_wchar_h = yes; then
    HAVE_WCHAR_H=1
  else
    HAVE_WCHAR_H=0
  fi
  AC_SUBST([HAVE_WCHAR_H])

  AC_REQUIRE([gl_FEATURES_H])

  AC_REQUIRE([gt_TYPE_WINT_T])
  if test $gt_cv_c_wint_t = yes; then
    HAVE_WINT_T=1
  else
    HAVE_WINT_T=0
  fi
  AC_SUBST([HAVE_WINT_T])

  AC_REQUIRE([gl_TYPE_WINT_T_PREREQ])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[
      #include <wchar.h>
    ]],
    [btowc wctob mbsinit mbrtowc mbrlen mbsrtowcs mbsnrtowcs wcrtomb
     wcsrtombs wcsnrtombs wcwidth
     wmemchr wmemcmp wmemcpy wmemmove wmempcpy wmemset
     wcslen wcsnlen wcscpy wcpcpy wcsncpy wcpncpy wcscat wcsncat wcscmp
     wcsncmp wcscasecmp wcsncasecmp wcscoll wcsxfrm wcsdup wcschr wcsrchr
     wcscspn wcsspn wcspbrk wcsstr wcstok wcswidth wcsftime
    ])

  AC_REQUIRE([AC_C_RESTRICT])

  AC_CHECK_DECLS([wcsdup], [], [], [[
      #include <wchar.h>
    ]])
  if test $ac_cv_have_decl_wcsdup = no; then
    HAVE_DECL_WCSDUP=0
  fi
])

dnl Check whether <wchar.h> is usable at all.
AC_DEFUN([gl_WCHAR_H_INLINE_OK],
[
  dnl Test whether <wchar.h> suffers due to the transition from '__inline' to
  dnl 'gnu_inline'. See <https://sourceware.org/bugzilla/show_bug.cgi?id=4022>
  dnl and <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=42440>. In summary,
  dnl glibc version 2.5 or older, together with gcc version 4.3 or newer and
  dnl the option -std=c99 or -std=gnu99, leads to a broken <wchar.h>.
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_CACHE_CHECK([whether <wchar.h> uses 'inline' correctly],
    [gl_cv_header_wchar_h_correct_inline],
    [gl_cv_header_wchar_h_correct_inline=yes
     case "$host_os" in
       *-gnu* | gnu*)
         AC_LANG_CONFTEST([
           AC_LANG_SOURCE([[
             #define wcstod renamed_wcstod
             #include <wchar.h>
             extern int zero (void);
             int main () { return zero(); }
           ]])])
         dnl Do not rename the object file from conftest.$ac_objext to
         dnl conftest1.$ac_objext, as this will cause the link to fail on
         dnl z/OS when using the XPLINK object format (due to duplicate
         dnl CSECT names). Instead, temporarily redefine $ac_compile so
         dnl that the object file has the latter name from the start.
         save_ac_compile="$ac_compile"
         ac_compile=`echo "$save_ac_compile" | sed s/conftest/conftest1/`
         if echo '#include "conftest.c"' >conftest1.c \
            && AC_TRY_EVAL([ac_compile]); then
           AC_LANG_CONFTEST([
             AC_LANG_SOURCE([[
               #define wcstod renamed_wcstod
               #include <wchar.h>
               int zero (void) { return 0; }
             ]])])
           dnl See note above about renaming object files.
           ac_compile=`echo "$save_ac_compile" | sed s/conftest/conftest2/`
           if echo '#include "conftest.c"' >conftest2.c \
              && AC_TRY_EVAL([ac_compile]); then
             if $CC -o conftest$ac_exeext $CFLAGS $LDFLAGS conftest1.$ac_objext conftest2.$ac_objext $LIBS >&AS_MESSAGE_LOG_FD 2>&1; then
               :
             else
               gl_cv_header_wchar_h_correct_inline=no
             fi
           fi
         fi
         ac_compile="$save_ac_compile"
         rm -f conftest[12].c conftest[12].$ac_objext conftest$ac_exeext
         ;;
     esac
    ])
  if test $gl_cv_header_wchar_h_correct_inline = no; then
    AC_MSG_ERROR([<wchar.h> cannot be used with this compiler ($CC $CFLAGS $CPPFLAGS).
This is a known interoperability problem of glibc <= 2.5 with gcc >= 4.3 in
C99 mode. You have four options:
  - Add the flag -fgnu89-inline to CC and reconfigure, or
  - Fix your include files, using parts of
    <https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=b037a293a48718af30d706c2e18c929d0e69a621>, or
  - Use a gcc version older than 4.3, or
  - Don't use the flags -std=c99 or -std=gnu99.
Configuration aborted.])
  fi
])

# gl_WCHAR_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_WCHAR_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_WCHAR_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_WCHAR_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_WCHAR_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_BTOWC])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCTOB])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBSINIT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBRTOWC])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBRLEN])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBSRTOWCS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MBSNRTOWCS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCRTOMB])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSRTOMBS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSNRTOMBS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCWIDTH])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WMEMCHR])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WMEMCMP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WMEMCPY])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WMEMMOVE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WMEMPCPY])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WMEMSET])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSLEN])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSNLEN])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCPY])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCPCPY])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSNCPY])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCPNCPY])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCAT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSNCAT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCMP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSNCMP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCASECMP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSNCASECMP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCOLL])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSXFRM])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSDUP])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCHR])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSRCHR])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSCSPN])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSSPN])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSPBRK])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSSTR])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSTOK])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSWIDTH])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_WCSFTIME])
    dnl Support Microsoft deprecated alias function names by default.
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MDA_WCSDUP], [1])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_WCHAR_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
])

AC_DEFUN([gl_WCHAR_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_BTOWC=1;         AC_SUBST([HAVE_BTOWC])
  HAVE_MBSINIT=1;       AC_SUBST([HAVE_MBSINIT])
  HAVE_MBRTOWC=1;       AC_SUBST([HAVE_MBRTOWC])
  HAVE_MBRLEN=1;        AC_SUBST([HAVE_MBRLEN])
  HAVE_MBSRTOWCS=1;     AC_SUBST([HAVE_MBSRTOWCS])
  HAVE_MBSNRTOWCS=1;    AC_SUBST([HAVE_MBSNRTOWCS])
  HAVE_WCRTOMB=1;       AC_SUBST([HAVE_WCRTOMB])
  HAVE_WCSRTOMBS=1;     AC_SUBST([HAVE_WCSRTOMBS])
  HAVE_WCSNRTOMBS=1;    AC_SUBST([HAVE_WCSNRTOMBS])
  HAVE_WMEMCHR=1;       AC_SUBST([HAVE_WMEMCHR])
  HAVE_WMEMCMP=1;       AC_SUBST([HAVE_WMEMCMP])
  HAVE_WMEMCPY=1;       AC_SUBST([HAVE_WMEMCPY])
  HAVE_WMEMMOVE=1;      AC_SUBST([HAVE_WMEMMOVE])
  HAVE_WMEMPCPY=1;      AC_SUBST([HAVE_WMEMPCPY])
  HAVE_WMEMSET=1;       AC_SUBST([HAVE_WMEMSET])
  HAVE_WCSLEN=1;        AC_SUBST([HAVE_WCSLEN])
  HAVE_WCSNLEN=1;       AC_SUBST([HAVE_WCSNLEN])
  HAVE_WCSCPY=1;        AC_SUBST([HAVE_WCSCPY])
  HAVE_WCPCPY=1;        AC_SUBST([HAVE_WCPCPY])
  HAVE_WCSNCPY=1;       AC_SUBST([HAVE_WCSNCPY])
  HAVE_WCPNCPY=1;       AC_SUBST([HAVE_WCPNCPY])
  HAVE_WCSCAT=1;        AC_SUBST([HAVE_WCSCAT])
  HAVE_WCSNCAT=1;       AC_SUBST([HAVE_WCSNCAT])
  HAVE_WCSCMP=1;        AC_SUBST([HAVE_WCSCMP])
  HAVE_WCSNCMP=1;       AC_SUBST([HAVE_WCSNCMP])
  HAVE_WCSCASECMP=1;    AC_SUBST([HAVE_WCSCASECMP])
  HAVE_WCSNCASECMP=1;   AC_SUBST([HAVE_WCSNCASECMP])
  HAVE_WCSCOLL=1;       AC_SUBST([HAVE_WCSCOLL])
  HAVE_WCSXFRM=1;       AC_SUBST([HAVE_WCSXFRM])
  HAVE_WCSDUP=1;        AC_SUBST([HAVE_WCSDUP])
  HAVE_WCSCHR=1;        AC_SUBST([HAVE_WCSCHR])
  HAVE_WCSRCHR=1;       AC_SUBST([HAVE_WCSRCHR])
  HAVE_WCSCSPN=1;       AC_SUBST([HAVE_WCSCSPN])
  HAVE_WCSSPN=1;        AC_SUBST([HAVE_WCSSPN])
  HAVE_WCSPBRK=1;       AC_SUBST([HAVE_WCSPBRK])
  HAVE_WCSSTR=1;        AC_SUBST([HAVE_WCSSTR])
  HAVE_WCSTOK=1;        AC_SUBST([HAVE_WCSTOK])
  HAVE_WCSWIDTH=1;      AC_SUBST([HAVE_WCSWIDTH])
  HAVE_WCSFTIME=1;      AC_SUBST([HAVE_WCSFTIME])
  HAVE_DECL_WCTOB=1;    AC_SUBST([HAVE_DECL_WCTOB])
  HAVE_DECL_WCSDUP=1;   AC_SUBST([HAVE_DECL_WCSDUP])
  HAVE_DECL_WCWIDTH=1;  AC_SUBST([HAVE_DECL_WCWIDTH])
  REPLACE_MBSTATE_T=0;  AC_SUBST([REPLACE_MBSTATE_T])
  REPLACE_BTOWC=0;      AC_SUBST([REPLACE_BTOWC])
  REPLACE_WCTOB=0;      AC_SUBST([REPLACE_WCTOB])
  REPLACE_MBSINIT=0;    AC_SUBST([REPLACE_MBSINIT])
  REPLACE_MBRTOWC=0;    AC_SUBST([REPLACE_MBRTOWC])
  REPLACE_MBRLEN=0;     AC_SUBST([REPLACE_MBRLEN])
  REPLACE_MBSRTOWCS=0;  AC_SUBST([REPLACE_MBSRTOWCS])
  REPLACE_MBSNRTOWCS=0; AC_SUBST([REPLACE_MBSNRTOWCS])
  REPLACE_WCRTOMB=0;    AC_SUBST([REPLACE_WCRTOMB])
  REPLACE_WCSRTOMBS=0;  AC_SUBST([REPLACE_WCSRTOMBS])
  REPLACE_WCSNRTOMBS=0; AC_SUBST([REPLACE_WCSNRTOMBS])
  REPLACE_WCWIDTH=0;    AC_SUBST([REPLACE_WCWIDTH])
  REPLACE_WCSWIDTH=0;   AC_SUBST([REPLACE_WCSWIDTH])
  REPLACE_WCSFTIME=0;   AC_SUBST([REPLACE_WCSFTIME])
  REPLACE_WCSTOK=0;     AC_SUBST([REPLACE_WCSTOK])
])
