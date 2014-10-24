# $Id$

#	Copyright (c) 2009, Darren Hiebert
#
#	This source code is released for free distribution under the terms
#	of the GNU General Public License.

#	Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([ctags],[fishman])
AC_CONFIG_HEADERS([config.h])

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
AH_TEMPLATE([SEEK_SET],
	[Define this value used by fseek() appropriately if <stdio.h>
	(or <unistd.h> on SunOS 4.1.x) does not define them.])
AH_TEMPLATE([INT_MAX],
	[Define as the maximum integer on your system if not defined <limits.h>.])
AH_TEMPLATE([CUSTOM_CONFIGURATION_FILE],
	[You can define this label to be a string containing the name of a
	site-specific configuration file containing site-wide default options. The
	files /etc/ctags.conf and /usr/local/etc/ctags.conf are already checked,
	so only define one here if you need a file somewhere else.])
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
AH_TEMPLATE([TMPDIR],
	[If you wish to change the directory in which temporary files are stored,
	define this label to the directory desired.])
AH_TEMPLATE([REGCOMP_BROKEN],
	[Define this label if regcomp() is broken.])
AH_TEMPLATE([CHECK_REGCOMP],
	[Define this label if you wish to check the regcomp() function at run time
	for correct behavior. This function is currently broken on Cygwin.])
AH_TEMPLATE([__USE_FIXED_PROTOTYPES__],
	[This corrects the problem of missing prototypes for certain functions
	in some GNU installations (e.g. SunOS 4.1.x).])
AH_TEMPLATE([NON_CONST_PUTENV_PROTOTYPE],
	[Define this is you have a prototype for putenv() in <stdlib.h>, but
	doesn't declare its argument as "const char *".])
AH_TEMPLATE([NEED_PROTO_REMOVE],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_UNLINK],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_MALLOC],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_GETENV],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_FGETPOS],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_STAT],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_LSTAT],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_TRUNCATE],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])
AH_TEMPLATE([NEED_PROTO_FTRUNCATE],
	[If you receive error or warning messages indicating that you are missing
	a prototype for, or a type mismatch using, the following function, define
	this label and remake.])



# Report system info
# ------------------
program_name=[`grep 'PROGRAM_NAME  *"' $srcdir/ctags.h | sed -e 's/.*"\([^"]*\)".*/\1/'`]
program_version=[`grep 'PROGRAM_VERSION  *"' $srcdir/ctags.h | sed -e 's/.*"\([^"]*\)".*/\1/'`]
echo "$program_name, version $program_version"
uname -mrsv 2>/dev/null

# Define convenience macros
# -------------------------
# CHECK_HEADER_DEFINE(LABEL, HEADER [,ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND ] ])
AC_DEFUN([CHECK_HEADER_DEFINE], [
	AC_MSG_CHECKING([if $1 is defined in $2])
	AC_EGREP_CPP(yes,
[#include <$2>
#ifdef $1
  yes
#endif
], [
	AC_MSG_RESULT(yes)
	[$3]
], [
	AC_MSG_RESULT(no)
	[$4]
]) ])

# Checks for configuration options
# --------------------------------

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

AC_ARG_ENABLE(custom-config,
	[AS_HELP_STRING([--enable-custom-config=FILE],
		[enable custom config file for site-wide defaults])])

AC_ARG_ENABLE(macro-patterns,
	[AS_HELP_STRING([--enable-macro-patterns],
		[use patterns as default method to locate macros instead of line numbers])])

AC_ARG_ENABLE(maintainer-mode,
	[AS_HELP_STRING([--enable-maintainer-mode],
		[use maintainer makefile])])

AC_ARG_ENABLE(shell-globbing,
	[AS_HELP_STRING([--enable-shell-globbing=DIR],
		[does shell expand wildcards (yes|no)? [yes]])])

AC_ARG_ENABLE(tmpdir,
	[AS_HELP_STRING([--enable-tmpdir=DIR],
		[default directory for temporary files [ARG=/tmp]])],
	tmpdir_specified=yes)


# Process configuration options
# -----------------------------

if test "$enable_maintainer_mode" = yes ; then
	AC_MSG_RESULT(enabling maintainer mode)
fi

install_targets="install-ctags install-data install-libexec"
AC_MSG_CHECKING(whether to install link to etags)
if test yes = "$enable_etags"; then
	AC_MSG_RESULT(yes)
	install_targets="$install_targets install-etags"
else
	AC_MSG_RESULT(no)
fi
AC_MSG_CHECKING(whether to install readtags object file)
if test yes = "$enable_readlib"; then
	AC_MSG_RESULT(yes)
	install_targets="$install_targets install-lib"
else
	AC_MSG_RESULT(no)
fi
AC_MSG_CHECKING(whether to install readtags command)
if test no = "$enable_readcmd"; then
	AC_MSG_RESULT(no)
else
	AC_MSG_RESULT(yes)
	install_targets="$install_targets install-cmd"
fi
AC_SUBST(install_targets)

if test "$enable_custom_config" = no -o "$enable_custom_config" = yes ; then
	AC_MSG_RESULT(no name supplied for custom configuration file)
elif test -n "$enable_custom_config" ; then
	AC_DEFINE_UNQUOTED(CUSTOM_CONFIGURATION_FILE, "$enable_custom_config")
	AC_MSG_RESULT($enable_custom_config will be used as custom configuration file)
fi

if test "$enable_macro_patterns" = yes ; then
	AC_DEFINE(MACROS_USE_PATTERNS)
	AC_MSG_RESULT(tag file will use patterns for macros by default)
fi

# Checks for programs
# -------------------

AC_PROG_CC

case `uname` in
	HP-UX)
		AC_MSG_CHECKING(HP-UX native compiler)
		if test "$CC" = "cc"; then
			AC_MSG_RESULT(yes; adding compiler options for ANSI support)
			CFLAGS="$CFLAGS -Aa -D_HPUX_SOURCE"
		else
			AC_MSG_RESULT(no)
		fi
		;;
	SunOS)
		if uname -r | grep '5\.' >/dev/null 2>&1; then
			AC_MSG_CHECKING(Solaris native compiler)
			if test "$CC" = "cc" -a "`which cc`" = "/usr/ucb/cc"; then
				AC_MSG_RESULT(yes; adding compiler option for ANSI support)
				CC="$CC -Xa"
			else
				AC_MSG_RESULT(no)
			fi
		fi
		;;
esac

AC_PROG_LN_S
AC_CHECK_PROG(STRIP, strip, strip, :)
AC_SYS_LARGEFILE


# Checks for operating environment
# --------------------------------

# Check for temporary directory
AC_MSG_CHECKING(directory to use for temporary files)
if test -n "$enable_tmpdir"; then
	tmpdir="$enable_tmpdir"
elif test -n "$TMPDIR"; then
	tmpdir="$TMPDIR"
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
	AC_MSG_RESULT(using internal sort algorithm as fallback)
fi


# Checks for header files
# -----------------------

AC_CHECK_HEADERS_ONCE([dirent.h fcntl.h stat.h stdlib.h string.h])
AC_CHECK_HEADERS_ONCE([time.h types.h unistd.h])
AC_CHECK_HEADERS_ONCE([sys/dir.h sys/stat.h sys/times.h sys/types.h])


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


# Checks for compiler characteristics
# -----------------------------------

# AC_CYGWIN
# AC_MINGW32
AC_C_CONST
AC_OBJEXT
AC_EXEEXT

AC_MSG_CHECKING(if struct stat contains st_ino)
AC_TRY_COMPILE([#include <sys/stat.h>
				#include <stdlib.h>], [
	struct stat st;
	stat(".", &st);
	if (st.st_ino > 0)
		exit(0);
], have_st_ino=yes, have_st_ino=no)
AC_MSG_RESULT($have_st_ino)
if test yes = "$have_st_ino"; then
	AC_DEFINE(HAVE_STAT_ST_INO)
fi


# Checks for library functions
# ----------------------------
AC_CHECK_FUNCS(fnmatch,[],[
AC_LIBOBJ([fnmatch])
CPPFLAGS="$CPPFLAGS -I${srcdir}/fnmatch"
AC_DEFINE(HAVE_FNMATCH)
])
AC_CHECK_HEADERS(fnmatch.h)

AC_CHECK_FUNCS(strstr)
AC_CHECK_FUNCS(strcasecmp stricmp, break)
AC_CHECK_FUNCS(strncasecmp strnicmp, break)
AC_CHECK_FUNCS(fgetpos, have_fgetpos=yes)

# SEEK_SET should be in stdio.h, but may be in unistd.h on SunOS 4.1.x
if test "$have_fgetpos" != yes ; then
	CHECK_HEADER_DEFINE(SEEK_SET, stdio.h,,
		CHECK_HEADER_DEFINE(SEEK_SET, unistd.h,,
			AC_DEFINE(SEEK_SET, 0)))
fi

AC_CHECK_FUNCS(mkstemp, have_mkstemp=yes)
if test "$have_mkstemp" != yes ; then
	AC_CHECK_FUNCS(tempnam, have_tempnam=yes)
fi
if test "$have_mkstemp" != yes -a "$have_tempnam" != yes; then
	AC_CHECK_FUNCS(chmod)
	if test "$tmpdir_specified" = yes ; then
		AC_MSG_RESULT(use of tmpnam overrides temporary directory selection)
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
dnl If it is found but broken, define REGCOMP_BROKEN.
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
AC_TRY_RUN([
#include <sys/types.h>
#include <regex.h>
int main(void) {
	regex_t patbuf;
	return (regcomp (&patbuf, "/hello/", 0) != 0);
}],regcomp_works=yes,regcomp_works=no,AC_DEFINE(CHECK_REGCOMP))
AC_MSG_RESULT($regcomp_works)
if test yes != "$regcomp_works"; then
	AC_DEFINE(REGCOMP_BROKEN)
fi
],[
dnl Using bundled regex implementation
AC_LIBOBJ([regex])
CPPFLAGS="$CPPFLAGS -I${srcdir}/gnu_regex"
AC_DEFINE(HAVE_REGCOMP)
])
# if test yes = "$with_perl_regex"; then
# 	AC_MSG_CHECKING(for Perl regex library)
# 	pcre_candidates="$with_perl_regex $HOME/local/lib* /usr*/local/lib* /usr/lib*"
# 	for lib in $pcre_candidates; do
# 		if test -f $lib/libpcreposix.so; then
# 			pcre_lib="-L$lib -lpcreposix"
# 			break
# 		elif test -f $lib/libpcreposix.a; then
# 			pcre_lib="$lib/libpcreposix.a"
# 			break
# 		fi
# 	done
# 	if test -z "$pcre_lib"; then
# 		AC_MSG_RESULT(not found)
# 	else
# 		AC_MSG_RESULT($lib)
# 		AC_DEFINE(HAVE_REGCOMP)
# 		LDFLAGS="$LDFLAGS $pcre_lib"
# 		have_regex=yes
# 	fi
# fi

install_data_targets="install-data-corpora"
AC_CHECK_FUNCS(scandir,[install_data_targets="$install_data_targets install-data-optlib install-data-preload"])
AC_SUBST(install_data_targets)

install_libexec_targets=
AC_CHECK_FUNCS(fork waitpid execv pipe,[install_libexec_targets="$install_libexec_targets install-libexec-drivers"])
AC_SUBST(install_libexec_targets)

# Checks for missing prototypes
# -----------------------------
AC_CHECKING(for new missing prototypes)

AC_DEFUN([CHECK_PROTO], [
	AC_EGREP_HEADER([[^A-Za-z0-9_]$1([ 	]+[A-Za-z0-9_]*)?[	 ]*\(],
	$2,
	,
	[
		AC_MSG_RESULT([adding prototype for $1])
		AC_DEFINE(patsubst([NEED_PROTO_NAME], [NAME], translit([$1], [[a-z]], [[A-Z]])))
	])])

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

# Checks for declaration
# ----------------------
AC_CHECK_DECLS([__environ], [], [], [
[#include <unistd.h>]
])
AC_CHECK_DECLS([_NSGetEnviron],[],[],[
[#include <crt_externs.h>]
])

# Output files
# ------------


rm -f Makefile
if test "$enable_maintainer_mode" = yes ; then
	AC_MSG_RESULT(creating maintainer Makefile)
	ln -s maintainer.mak Makefile
	makefile_out=
else
	makefile_out=Makefile
fi
AC_OUTPUT($makefile_out)

# vim:ts=4:sw=4:
