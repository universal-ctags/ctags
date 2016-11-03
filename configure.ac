dnl Process this file with autoconf to produce a configure script.
AC_INIT(onigmo, 6.0.0)

AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE(foreign)
AC_CONFIG_HEADER(config.h)


dnl default value for RUBYDIR
RUBYDIR=".."
AC_ARG_WITH(rubydir,
	[  --with-rubydir=RUBYDIR  specify value for RUBYDIR (default ..)],
	[ RUBYDIR=$withval ])
AC_SUBST(RUBYDIR)

dnl default value for STATISTICS
STATISTICS=""
AC_ARG_WITH(statistics,
	[  --with-statistics       take matching time statistical data],
	[ STATISTICS=-DONIG_DEBUG_STATISTICS ])
AC_SUBST(STATISTICS)

dnl check for COMBINATION_EXPLOSION
AC_ARG_ENABLE(combination-explosion-check,
	[  --enable-combination-explosion-check   enable combination explosion check],
	[comb_expl_check=$enableval])
if test "${comb_expl_check}" = yes; then
  AC_DEFINE(USE_COMBINATION_EXPLOSION_CHECK,1,[Define if combination explosion check])
fi

dnl check for CRNL_AS_LINE_TERMINATOR
AC_ARG_ENABLE(crnl-as-line-terminator,
	[  --enable-crnl-as-line-terminator   enable CR+NL as line terminator],
	[crnl_as_line_terminator=$enableval])
if test "${crnl_as_line_terminator}" = yes; then
  AC_DEFINE(USE_CRNL_AS_LINE_TERMINATOR,1,[Define if enable CR+NL as line terminator])
fi


dnl Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
AM_PROG_CC_C_O
LTVERSION="6:0:0"
AC_SUBST(LTVERSION)

AC_PROG_INSTALL
AC_PROG_MAKE_SET

AC_CHECK_PROGS(python_prog, python3 python python2)

dnl Checks for libraries.

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(stdlib.h string.h strings.h sys/time.h unistd.h sys/times.h stdint.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_SIZEOF(int, 4)
AC_CHECK_SIZEOF(short, 2)
AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF([void*])
AC_C_CONST
AC_HEADER_TIME

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MEMCMP

dnl check for multithread
AC_ARG_ENABLE(multithread,
	[  --enable-multithread    enable multithread support],
	[multithread=$enableval])
if test "${multithread}" = yes; then
  case $host_os in
  mingw*)
    AC_DEFINE(USE_MULTI_THREAD_SYSTEM,1,[Define if enbale multithread support])
    AC_DEFINE(USE_DEFAULT_MULTI_THREAD_SYSTEM,1,[Define if enable the default multithread system])
    ;;
  *)
    dnl This check must be after AC_PROG_CC.
    AC_CHECK_HEADERS(pthread.h,
	  [AC_DEFINE(USE_MULTI_THREAD_SYSTEM,1,[Define if enbale multithread support])
	   AC_DEFINE(USE_DEFAULT_MULTI_THREAD_SYSTEM,1,[Define if enable the default multithread system])
    ])
    AC_CHECK_LIB(pthread, pthread_mutex_init)
    ;;
  esac
fi


AC_OUTPUT([Makefile onigmo-config sample/Makefile], [chmod +x onigmo-config])
