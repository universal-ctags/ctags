#	Copyright (c) 2009, Darren Hiebert
#
#	This source code is released for free distribution under the terms
#	of the GNU General Public License version 2 or (at your option) any
#	later version.

#	Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([universal-ctags],[0.0.0])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])
AC_CONFIG_HEADERS([config.h])
if ! test -e "${srcdir}/config.h.in"; then
   echo "---"
   echo "--- ${srcdir}/config.h.in WAS NOT FOUND." 1>&2
   echo "--- YOU MIGHT HAVE RUN autoconf ONLY" 1>&2
   echo "--- BUT YOU MAY HAVE TO RUN autogen.sh." 1>&2
   echo "---"
   exit 1
fi

AC_CANONICAL_BUILD
AC_CANONICAL_HOST

AH_TEMPLATE([PACKAGE], [Package name.])
AH_TEMPLATE([VERSION], [Package version.])
AH_TEMPLATE([clock_t],
	[Define to the appropriate type if <time.h> does not define this.])
AH_TEMPLATE([fpos_t],
	[Define to long if <stdio.h> does not define this.])
AH_TEMPLATE([L_tmpnam],
	[Define to the appropriate size for tmpnam() if <stdio.h> does not define
	this.])
AH_TEMPLATE([HAVE_STAT_ST_INO],
	[Define this macro if the field "st_ino" exists in struct stat in
	<sys/stat.h>.])
AH_TEMPLATE([remove],
	[Define remove to unlink if you have unlink(), but not remove().])
AH_TEMPLATE([INT_MAX],
	[Define as the maximum integer on your system if not defined <limits.h>.])
AH_TEMPLATE([CUSTOM_CONFIGURATION_FILE],
	[You can define this label to be a string containing the name of a
	site-specific configuration file containing site-wide default options. The
	files /etc/ctags.conf and /usr/local/etc/ctags.conf are already checked,
	so only define one here if you need a file somewhere else.])
AH_TEMPLATE([ETAGS],
	[Define the string to check (in executable name) for etags mode])
AH_TEMPLATE([MACROS_USE_PATTERNS],
	[Define this label if you want macro tags (defined lables) to use patterns
	in the EX command by default (original ctags behavior is to use line
	numbers).])
AH_VERBATIM([DEFAULT_FILE_FORMAT], [
/* Define this as desired.
 * 1:  Original ctags format
 * 2:  Extended ctags format with extension flags in EX-style comment.
 */
#define DEFAULT_FILE_FORMAT	2
])
AH_TEMPLATE([CASE_INSENSITIVE_FILENAMES],
	[Define this label if your system uses case-insensitive file names])
AH_VERBATIM([EXTERNAL_SORT], [
/* Define this label to use the system sort utility (which is probably more
*  efficient) over the internal sorting algorithm.
*/
#ifndef INTERNAL_SORT
# undef EXTERNAL_SORT
#endif
])
AH_TEMPLATE([HAVE_ICONV],
	[Define this value if support multibyte character encoding.])
AH_TEMPLATE([TMPDIR],
	[If you wish to change the directory in which temporary files are stored,
	define this label to the directory desired.])
AH_TEMPLATE([CHECK_REGCOMP],
	[Define this label if you wish to check the regcomp() function at run time
	for correct behavior. This function is currently broken on Cygwin.])
AH_TEMPLATE([NON_CONST_PUTENV_PROTOTYPE],
	[Define this is you have a prototype for putenv() in <stdlib.h>, but
	doesn't declare its argument as "const char *".])
AH_TEMPLATE([MSDOS_STYLE_PATH],
	[Define to 1 if your system uses MS-DOS style path.])


# Report system info
# ------------------
program_name=[`grep 'PROGRAM_NAME  *"' $srcdir/main/ctags.h | sed -e 's/.*"\([^"]*\)".*/\1/'`]
program_version=[`grep 'PROGRAM_VERSION  *"' $srcdir/main/ctags.h | sed -e 's/.*"\([^"]*\)".*/\1/'`]
echo "$program_name, version $program_version"
uname -mrsv 2>/dev/null

# Define convenience macros
# -------------------------
# CHECK_HEADER_DEFINE(LABEL, HEADER [,ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND ] ])
AC_DEFUN([CHECK_HEADER_DEFINE], [
	AC_MSG_CHECKING([if $1 is defined in $2])
	AC_EGREP_CPP([$2:$1],
[
#include <$2>
#ifdef $1
const char *result_yes = "$2:$1";
#endif
], [
	AC_MSG_RESULT(yes)
	[$3]
], [
	AC_MSG_RESULT(no)
	[$4]
]) ])

# CHECK_PROTO(FUNCTION, HEADER-FILE)
AC_DEFUN([CHECK_PROTO], [
	AC_EGREP_HEADER([[^A-Za-z0-9_]$1([ 	]+[A-Za-z0-9_]*)?[	 ]*\(],
	[$2],
	[],
	[
		AC_MSG_NOTICE([adding prototype for $1])
		AH_TEMPLATE(AS_TR_CPP([NEED_PROTO_$1]),
			    [If you receive error or warning messages indicating that you are missing a prototype for, or a type mismatch using, the following function, define this label and remake.])
		AC_DEFINE(AS_TR_CPP([NEED_PROTO_$1]))
	])
])

# PRETTY_ARG_VAR(VARIABLE, DESCRIPTION, DEFAULT)
# ----------------------------------------------
# Call AC_ARG_VAR with DEFAULT value.
AC_DEFUN([PRETTY_ARG_VAR], [
	AC_ARG_VAR([$1], [$2 [$3]])

	if test "${ac_env_$1_set}" = "set"; then
		$1="${ac_env_$1_value}"
	else
		$1="$3"
	fi
])

# PRETTY_VAR_EXPAND(VARIABLE)
# ---------------------------
# Expand nested variable.
# e.g. "\$\{prefix\}/bin" to "/usr/local/bin"
AC_DEFUN([PRETTY_VAR_EXPAND],
	 [$(eval "$as_echo_n" $(eval "$as_echo_n" "${$1}"))])

# Checks for configuration options
# --------------------------------

AC_ARG_WITH([sparse-cgcc],
	[AS_HELP_STRING([--with-sparse-cgcc],
		[use Sparse 'compiler wrapper' cgcc as C compiler [no]])])

AC_ARG_ENABLE([coverage-gcov],
	[AS_HELP_STRING([--enable-coverage-gcov],
		[enable 'gcov' coverage testing tool [no]])])

AC_ARG_ENABLE(readlib,
	[AS_HELP_STRING([--enable-readlib],
		[include readtags library object during install])])

AC_ARG_ENABLE(readcmd,
	[AS_HELP_STRING([--disable-readcmd],
		[do not include readtags command during install])],
	[], [enable_readcmd=yes])

# AC_ARG_WITH(perl-regex,
# 	[AS_HELP_STRING([--with-perl-regex],
# 	    [use Perl pcre interface, if available])])

AC_ARG_ENABLE(etags,
	[AS_HELP_STRING([--enable-etags],
		[enable the installation of links for etags])])

AC_ARG_ENABLE(extended-format,
	[AS_HELP_STRING([--disable-extended-format],
		[disable extension flags; use original ctags file format only])],
	AC_DEFINE(DEFAULT_FILE_FORMAT, 1), AC_DEFINE(DEFAULT_FILE_FORMAT, 2))

AC_ARG_ENABLE(external-sort,
	[AS_HELP_STRING([--disable-external-sort],
		[use internal sort algorithm instead of sort program])])

AC_ARG_ENABLE(iconv,
	[AS_HELP_STRING([--enable-iconv],
		[support multibyte character encoding])])

AC_ARG_ENABLE(custom-config,
	[AS_HELP_STRING([--enable-custom-config=FILE],
		[enable custom config file for site-wide defaults])])

AC_ARG_ENABLE(macro-patterns,
	[AS_HELP_STRING([--enable-macro-patterns],
		[use patterns as default method to locate macros instead of line numbers])])

AC_ARG_ENABLE(tmpdir,
	[AS_HELP_STRING([--enable-tmpdir=DIR],
		[default directory for temporary files [ARG=/tmp]])],
	tmpdir_specified=yes)

AC_ARG_WITH([ctags-sysconfdir],
	[AS_HELP_STRING([--with-ctags-sysconfdir=DIR],
		['universal-ctags'-specific read-only single-machine data [SYSCONFDIR/ctags]])])

AC_ARG_WITH([ctags-libexecdir],
	[AS_HELP_STRING([--with-ctags-libexecdir=DIR],
		['universal-ctags'-specific program executables [LIBEXECDIR/ctags]])])

AC_ARG_ENABLE([debugging],
	[AS_HELP_STRING([--enable-debugging],
		[enable debugging features])])

AC_ARG_PROGRAM

# Process configuration options
# -----------------------------

case "${with_ctags_sysconfdir+set}"-"$with_ctags_sysconfdir" in #(
"set"-"yes") ctags_sysconfdir="${sysconfdir}/ctags"	 ;; #(
"set"-"no")  ctags_sysconfdir="${sysconfdir}"		 ;; #(
"set"-*)     ctags_sysconfdir="${with_ctags_sysconfdir}" ;; #(
*)	     ctags_sysconfdir="${sysconfdir}/ctags"	 ;;
esac

case "${with_ctags_libexecdir+set}"-"$with_ctags_libexecdir" in #(
"set"-"yes") ctags_libexecdir="${libexecdir}/ctags"	 ;; #(
"set"-"no")  ctags_libexecdir="${libexecdir}"		 ;; #(
"set"-*)     ctags_libexecdir="${with_ctags_libexecdir}" ;; #(
*)	     ctags_libexecdir="${libexecdir}/ctags"	 ;;
esac

pkgsysconfdir="${ctags_sysconfdir}"
pkglibexecdir="${ctags_libexecdir}"

AC_SUBST([pkgsysconfdir])
AC_SUBST([pkglibexecdir])

AH_TEMPLATE([PKGSYSCONFDIR], [installation directory of 'universal-ctags'-specific read-only single-machine data.])
AH_TEMPLATE([PKGLIBEXECDIR], [installation directory of 'universal-ctags'-specific program executables.])

AC_CONFIG_COMMANDS_PRE([
	PKGSYSCONFDIR="PRETTY_VAR_EXPAND([pkgsysconfdir])"
	PKGLIBEXECDIR="PRETTY_VAR_EXPAND([pkglibexecdir])"

	AC_DEFINE_UNQUOTED([PKGSYSCONFDIR], ["${PKGSYSCONFDIR}"])
	AC_DEFINE_UNQUOTED([PKGLIBEXECDIR], ["${PKGLIBEXECDIR}"])
])

AM_CONDITIONAL(INSTALL_LIB, [test "x$enable_readlib" = "xyes"])
AM_CONDITIONAL(INSTALL_ETAGS, [test "x$enable_etags" = "xyes"])
AM_CONDITIONAL(USE_READCMD, [test "x$enable_readcmd" = "xyes"])

dnl AC_MSG_NOTICE(Change with $program_transform_name)
CTAGS_NAME_EXECUTABLE=`echo ctags | sed "$program_transform_name"`
AC_SUBST(CTAGS_NAME_EXECUTABLE)
ETAGS_NAME_EXECUTABLE=`echo etags | sed "$program_transform_name"`
AC_SUBST(ETAGS_NAME_EXECUTABLE)

AC_DEFINE_UNQUOTED(ETAGS, "$ETAGS_NAME_EXECUTABLE")

if test "$enable_custom_config" = no -o "$enable_custom_config" = yes ; then
	AC_MSG_NOTICE(no name supplied for custom configuration file)
elif test -n "$enable_custom_config" ; then
	AC_DEFINE_UNQUOTED(CUSTOM_CONFIGURATION_FILE, "$enable_custom_config")
	AC_MSG_NOTICE($enable_custom_config will be used as custom configuration file)
fi

if test "$enable_macro_patterns" = yes ; then
	AC_DEFINE(MACROS_USE_PATTERNS)
	AC_MSG_NOTICE(tag file will use patterns for macros by default)
fi

AM_CONDITIONAL(ENABLE_DEBUGGING, [test "x$enable_debugging" = "xyes"])


# Checks for programs
# -------------------

AC_PROG_CC
AC_PROG_CC_C99

if test "${with_sparse_cgcc}" = "yes"; then
	REAL_CC="${CC}"
	CC="cgcc"
	AC_SUBST([REAL_CC])

	PRETTY_ARG_VAR([CGCC_CFLAGS], [cgcc specific flags],
		       [-Wsparse-all])
else
	CGCC_CFLAGS=""
fi

if test "${enable_coverage_gcov}" = "yes"; then
	COVERAGE_CFLAGS="--coverage"
	COVERAGE_LDFLAGS="--coverage"
fi
AC_SUBST([COVERAGE_CFLAGS])
AC_SUBST([COVERAGE_LDFLAGS])

AC_PROG_LN_S
AC_CHECK_PROG(STRIP, strip, strip, :)
AC_SYS_LARGEFILE

AC_CHECK_PROGS([perl_found], [perl], [yes], [no])
AM_CONDITIONAL([RUN_OPTLIB2C], [test "${perl_found}" = "yes"])

AC_PATH_PROGS(RST2MAN, [rst2man rst2man.py], [no])
AM_CONDITIONAL([HAVE_RST2MAN], [test "x$RST2MAN" != "xno"])

# Checks for operating environment
# --------------------------------
in_git_repo=no
AC_MSG_CHECKING(building in a git repository)
if test -f "${srcdir}/.git/HEAD"; then
	in_git_repo=yes
fi
AC_MSG_RESULT(${in_git_repo})

AC_CHECK_PROGS([GIT], [git])
if ! test "${GIT+set}" = "set"; then
	in_git_repo=no
fi
AM_CONDITIONAL([BUILD_IN_GIT_REPO], [test "x${in_git_repo}" = "xyes"])


# Check for temporary directory
AC_MSG_CHECKING(directory to use for temporary files)
if test -n "$enable_tmpdir"; then
	tmpdir="$enable_tmpdir"
elif test -n "$TMPDIR"; then
	tmpdir="$TMPDIR"
elif test -n "$TMP"; then
	tmpdir="$TMP"
elif test -n "$TEMP"; then
	tmpdir="$TEMP"
elif test -d "c:/"; then
	tmpdir="c:/"
else
	tmpdir="/tmp"
fi
if test -d $tmpdir ; then
	AC_MSG_RESULT($tmpdir)
	AC_DEFINE_UNQUOTED(TMPDIR, "$tmpdir")
else
	AC_MSG_ERROR($tmpdir does not exist)
fi

# Test for case-insensitive filenames
AC_MSG_CHECKING(for case-insensitive filenames)
touch conftest.cif
if test -f CONFTEST.CIF; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(CASE_INSENSITIVE_FILENAMES)
else
	AC_MSG_RESULT(no)
fi
rm -f conftest.cif

AC_MSG_CHECKING(selected sort method)
if test no = "$enable_external_sort"; then
	AC_MSG_RESULT(simple internal algorithm)
else
	AC_MSG_RESULT(external sort utility)
	enable_external_sort=no
    AC_CHECK_PROG(sort_found, sort, yes, no)
	if test "$sort_found" = yes ; then
		AC_MSG_CHECKING(if sort accepts our command line)
		touch ${tmpdir}/sort.test
		sort -u -f -o ${tmpdir}/sort.test ${tmpdir}/sort.test 1>/dev/null 2>&1
		if test $? -ne 0 ; then
			AC_MSG_RESULT(no)
		else
			AC_MSG_RESULT(yes)
			AC_DEFINE(EXTERNAL_SORT)
			enable_external_sort=yes
		fi
		rm -f ${tmpdir}/sort.test
    fi
fi
if test "$enable_external_sort" != yes ; then
	AC_MSG_NOTICE(using internal sort algorithm as fallback)
fi


# Checks for header files
# -----------------------

AC_CHECK_HEADERS([dirent.h errno.h fcntl.h io.h limits.h stat.h stdlib.h string.h])
AC_CHECK_HEADERS([time.h types.h unistd.h])
AC_CHECK_HEADERS([sys/dir.h sys/stat.h sys/times.h sys/types.h sys/wait.h])


# Checks for header file macros
# -----------------------------

CHECK_HEADER_DEFINE(L_tmpnam, [stdio.h],, AC_DEFINE(L_tmpnam, 20))

CHECK_HEADER_DEFINE(INT_MAX, [limits.h],,
	CHECK_HEADER_DEFINE(MAXINT, [limits.h],
		AC_DEFINE(INT_MAX, MAXINT), AC_DEFINE(INT_MAX, 32767)))


# Checks for typedefs
# -------------------

AC_TYPE_SIZE_T
AC_TYPE_OFF_T

AC_MSG_CHECKING(for fpos_t)
AC_EGREP_HEADER(fpos_t, stdio.h, AC_MSG_RESULT(yes),
[
	AC_MSG_RESULT(no)
	AC_DEFINE(fpos_t, long)
])

AC_MSG_CHECKING(for clock_t)
AC_EGREP_HEADER(clock_t, time.h, AC_MSG_RESULT(yes),
[
	AC_MSG_RESULT(no)
	AC_DEFINE(clock_t, long)
])

AC_CHECK_HEADERS([stdbool.h],
[
	AH_TEMPLATE([USE_STDBOOL_H], [whether or not to use <stdbool.h>.])
	AC_DEFINE([USE_STDBOOL_H])
],
[
	AH_TEMPLATE([bool],  [type for 'bool' if <stdbool.h> is missing or broken.])
	AH_TEMPLATE([true],  [value of 'true' if <stdbool.h> is missing or broken.])
	AH_TEMPLATE([false], [value of 'false' if <stdbool.h> is missing or broken.])

	AC_CHECK_TYPES([_Bool],
		       [AC_DEFINE([bool], [_Bool])],
		       [AC_DEFINE([bool], [int])])
	AC_DEFINE([true],  [1])
	AC_DEFINE([false], [0])
])


# Checks for compiler characteristics
# -----------------------------------

# AC_CYGWIN
# AC_MINGW32
AC_C_CONST
AC_OBJEXT
AC_EXEEXT

# Check for host type
case "$host" in
  i?86-*-mingw* | x86_64-*-mingw*)
	host_mingw=yes
	AC_DEFINE(MSDOS_STYLE_PATH)
	;;
esac

# Check if struct stat contains st_ino.
# MinGW has st_ino, but it doesn't work.
if test yes != "$host_mingw"; then
	AC_MSG_CHECKING(if struct stat contains st_ino)
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
		#include <sys/stat.h>
		#include <stdlib.h>
		], [
		struct stat st;
		stat(".", &st);
		if (st.st_ino > 0)
			exit(0);
	])],[have_st_ino=yes],[have_st_ino=no])
	AC_MSG_RESULT($have_st_ino)
	if test yes = "$have_st_ino"; then
		AC_DEFINE(HAVE_STAT_ST_INO)
	fi
fi

PRETTY_ARG_VAR([EXTRA_CPPFLAGS], [extra (Objective) C/C++ preprocessor flags],
	       [-D_GNU_SOURCE -D__USE_GNU])
PRETTY_ARG_VAR([DEBUG_CPPFLAGS], [(Objective) C/C++ preprocessor debug flags],
	       [-DDEBUG])

PRETTY_ARG_VAR([EXTRA_CFLAGS], [extra C compiler flags],
	       [-std=gnu99])
PRETTY_ARG_VAR([WARNING_CFLAGS], [C compiler warning flags],
	       [-Wall])

# Checks for library functions
# ----------------------------
AC_CHECK_FUNCS(fnmatch,[have_fnmatch=yes],[
AC_LIBOBJ([fnmatch])
FNMATCH_CPPFLAGS="-I${srcdir}/fnmatch"
AC_DEFINE(HAVE_FNMATCH)
have_fnmatch=no])
AC_SUBST([FNMATCH_CPPFLAGS])
AC_CHECK_HEADERS(fnmatch.h)
AM_CONDITIONAL([HAVE_FNMATCH], [test "x$have_fnmatch" = "xyes"])

AC_CHECK_FUNCS(strstr)
AC_CHECK_FUNCS(strcasecmp stricmp, break)
AC_CHECK_FUNCS(strncasecmp strnicmp, break)
AC_CHECK_FUNCS(fgetpos, have_fgetpos=yes)

AC_CHECK_FUNCS(mkstemp, have_mkstemp=yes)
if test "$have_mkstemp" != yes ; then
	AC_CHECK_FUNCS(tempnam, have_tempnam=yes)
fi
if test "$have_mkstemp" != yes -a "$have_tempnam" != yes; then
	AC_CHECK_FUNCS(chmod)
	if test "$tmpdir_specified" = yes ; then
		AC_MSG_NOTICE(use of tmpnam overrides temporary directory selection)
	fi
fi

AC_CHECK_FUNCS(opendir findfirst _findfirst, break)
AC_CHECK_FUNCS(strerror)
AC_CHECK_FUNCS(clock times, break)
AC_CHECK_FUNCS(remove, have_remove=yes,
	CHECK_HEADER_DEFINE(remove, unistd.h,, AC_DEFINE(remove, unlink)))

AC_CHECK_FUNCS(truncate, have_truncate=yes)
# === Cannot nest AC_CHECK_FUNCS() calls
if test "$have_truncate" != yes  ; then
	AC_CHECK_FUNCS(ftruncate, have_ftruncate=yes)
	if test "$have_ftruncate" != yes ; then
		AC_CHECK_FUNCS(chsize)
	fi
fi

AC_CHECK_FUNCS(setenv, have_setenv=yes)
# === Cannot nest AC_CHECK_FUNCS() calls
if test "$have_setenv" != yes ; then
	AC_CHECK_FUNCS(putenv, have_putenv=yes)
	if test "$have_putenv" = yes ; then
		AC_EGREP_HEADER(putenv, stdlib.h, have_putenv_prototype=yes)
		if test "$have_putenv_prototype" = yes ; then
			AC_MSG_CHECKING(putenv prototype)
			AC_EGREP_HEADER([[^A-Za-zo-9_]putenv[ 	]*\(.*const.*\)[ 	]*;],
				stdlib.h, AC_MSG_RESULT(correct),
				[
					AC_MSG_RESULT(no const)
					AC_DEFINE(NON_CONST_PUTENV_PROTOTYPE)
				])
		fi
	fi
fi

dnl
dnl
dnl Check regcomp
dnl
dnl If it is found but broken, abort with a message.
dnl If it is found but cannot be test brokenness, define CHECK_REGCOMP.
dnl If it is not found, use bundled implementation.
dnl
dnl If CHECK_REGCOMP is defined, ctags itself check the brokenness at runtime
dnl (cross-compiling is assumed here).
dnl
dnl
dnl The final result can be check with ./ctags --list-features.
dnl
dnl
AC_CHECK_FUNCS(regcomp,[
AC_MSG_CHECKING(if regcomp works)
AC_RUN_IFELSE([AC_LANG_SOURCE([
#include <sys/types.h>
#include <regex.h>
int main(void) {
	regex_t patbuf;
	return (regcomp (&patbuf, "/hello/", 0) != 0);
}
])],[regcomp_works=yes],[regcomp_works=no],[AC_DEFINE(CHECK_REGCOMP)])
AC_MSG_RESULT($regcomp_works)
if test yes != "$regcomp_works"; then
	AC_MSG_ERROR([regcomp() on this system is broken.])
fi
have_regcomp=yes
],[
dnl Using bundled regex implementation
AC_LIBOBJ([regex])
REGCOMP_CPPFLAGS="-I${srcdir}/gnu_regex -D__USE_GNU"
AC_DEFINE(HAVE_REGCOMP)
have_regcomp=no])
AC_SUBST([REGCOMP_CPPFLAGS])
AM_CONDITIONAL([HAVE_REGCOMP], [test "xyes" = "x$have_regcomp"])

AC_CHECK_FUNCS(scandir)

AC_CHECK_FUNCS(fork waitpid execv pipe,[enable_xcmd=yes],[enable_xcmd=no])
AM_CONDITIONAL([ENABLE_XCMD], [test "xyes" = "x$enable_xcmd"])

AH_TEMPLATE([HAVE_LIBXML],
	[Define this value if libxml is available.])
PKG_CHECK_MODULES(LIBXML, libxml-2.0,
		       [have_libxml=yes
		       AC_DEFINE(HAVE_LIBXML)],
		       [have_libxml=no])
AM_CONDITIONAL(HAVE_LIBXML, test "x$have_libxml" = xyes)

AH_TEMPLATE([HAVE_JANSSON],
	[Define this value if jansson is available.])
PKG_CHECK_MODULES(JANSSON, jansson,
		       [have_jansson=yes
		       AC_DEFINE(HAVE_JANSSON)],
		       [have_jansson=no])
AM_CONDITIONAL(HAVE_JANSSON, test "x$have_jansson" = xyes)


# Checks for missing prototypes
# -----------------------------
AC_MSG_NOTICE(checking for new missing prototypes)

if test "$have_remove" = yes ; then
	CHECK_PROTO(remove, stdio.h)
else
	CHECK_PROTO(unlink,	unistd.h)
fi
CHECK_PROTO(malloc,	stdlib.h)
CHECK_PROTO(getenv,	stdlib.h)
CHECK_PROTO(stat,	sys/stat.h)
CHECK_PROTO(lstat,	sys/stat.h)
if test "$have_fgetpos" = yes ; then
	CHECK_PROTO(fgetpos, stdio.h)
fi
if test "$have_truncate" = yes ; then
	CHECK_PROTO(truncate, unistd.h)
fi
if test "$have_ftruncate" = yes ; then
	CHECK_PROTO(ftruncate, unistd.h)
fi

# Process library configuration options
# -------------------------------------

if test "$enable_iconv" = yes ; then
	save_LIBS="$LIBS"
	LIBS="$LIBS -liconv"
	AC_MSG_CHECKING(for iconv_open with -liconv)
	AC_LINK_IFELSE([AC_LANG_PROGRAM([
			#include <iconv.h>
		], [iconv_open("fr", "to")])], [
			AC_MSG_RESULT(yes)
			AC_DEFINE(HAVE_ICONV)
		], [LIBS="$save_LIBS"
			AC_MSG_RESULT(no)
			AC_CHECK_FUNC(iconv_open, [AC_DEFINE(HAVE_ICONV)],
				[AC_MSG_ERROR([Could not find libiconv. Please install libiconv and libiconv-devel.])])])
fi

# Checks for declaration
# ----------------------
AC_CHECK_DECLS([__environ], [], [], [
[#include <unistd.h>]
])
AC_CHECK_DECLS([_NSGetEnviron],[],[],[
[#include <crt_externs.h>]
])

if test "$ctags_name_executable" != ctags ; then
	AC_MSG_NOTICE(Changing name of 'ctags' for $ctags_name_executable)
fi

if test "$etags_name_executable" != etags ; then
	AC_MSG_NOTICE(Changing name of 'etags' for $etags_name_executable)
fi

# Output files
# ------------

AC_CHECK_FUNCS([mblen], [], [HAVE_MBLEN=1], [])

AC_CONFIG_FILES([Makefile
		 man/ctags.1.rst
		 ])

AC_OUTPUT

# vim:ts=4:sw=4:
