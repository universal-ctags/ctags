# std-gnu11.m4
# serial 3

# Prefer GNU C11 and C++11 to earlier versions.  -*- coding: utf-8 -*-

# The std-gnu23 module, which defines _AC_C_C23_OPTIONS, supersedes us.
m4_ifndef([_AC_C_C23_OPTIONS], [

# This implementation is taken from GNU Autoconf lib/autoconf/c.m4
# commit 017d5ddd82854911f0119691d91ea8a1438824d6
# dated Sun Apr 3 13:57:17 2016 -0700
# with minor changes to commentary.
# This implementation will be obsolete once we can assume Autoconf 2.70
# or later is installed everywhere a Gnulib program might be developed.

m4_version_prereq([2.70], [], [


# Copyright (C) 2001-2026 Free Software Foundation, Inc.

# This file is part of Autoconf.  This program is free
# software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the Autoconf Configure Script Exception,
# version 3.0, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License
# and a copy of the Autoconf Configure Script Exception along with
# this program; see the files COPYINGv3 and COPYING.EXCEPTION
# respectively.  If not, see <https://www.gnu.org/licenses/> and
# <https://git.savannah.gnu.org/gitweb/?p=autoconf.git;a=blob_plain;f=COPYING.EXCEPTION>.

# Written by David MacKenzie, with help from
# Akim Demaille, Paul Eggert,
# François Pinard, Karl Berry, Richard Pixley, Ian Lance Taylor,
# Roland McGrath, Noah Friedman, david d zuhn, and many others.


# AC_PROG_CC([COMPILER ...])
# --------------------------
# COMPILER ... is a space separated list of C compilers to search for.
# This just gives the user an opportunity to specify an alternative
# search list for the C compiler.
AC_DEFUN([AC_PROG_CC],
[AC_LANG_PUSH(C)dnl
AC_ARG_VAR([CC],     [C compiler command])dnl
AC_ARG_VAR([CFLAGS], [C compiler flags])dnl
_AC_ARG_VAR_LDFLAGS()dnl
_AC_ARG_VAR_LIBS()dnl
_AC_ARG_VAR_CPPFLAGS()dnl
m4_ifval([$1],
      [AC_CHECK_TOOLS(CC, [$1])],
[AC_CHECK_TOOL(CC, gcc)
if test -z "$CC"; then
  dnl Here we want:
  dnl	AC_CHECK_TOOL(CC, cc)
  dnl but without the check for a tool without the prefix.
  dnl Until the check is removed from there, copy the code:
  if test -n "$ac_tool_prefix"; then
    AC_CHECK_PROG(CC, [${ac_tool_prefix}cc], [${ac_tool_prefix}cc])
  fi
fi
if test -z "$CC"; then
  AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
fi
if test -z "$CC"; then
  AC_CHECK_TOOLS(CC, cl.exe)
fi
if test -z "$CC"; then
  AC_CHECK_TOOL(CC, clang)
fi
])

test -z "$CC" && AC_MSG_FAILURE([no acceptable C compiler found in \$PATH])

# Provide some information about the compiler.
_AS_ECHO_LOG([checking for _AC_LANG compiler version])
set X $ac_compile
ac_compiler=$[2]
for ac_option in --version -v -V -qversion -version; do
  _AC_DO_LIMIT([$ac_compiler $ac_option >&AS_MESSAGE_LOG_FD])
done

m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
_AC_LANG_COMPILER_GNU
if test $ac_compiler_gnu = yes; then
  GCC=yes
else
  GCC=
fi
_AC_PROG_CC_G
dnl
dnl Set ac_prog_cc_stdc to the supported C version.
dnl Also set the documented variable ac_cv_prog_cc_stdc;
dnl its name was chosen when it was cached, but it is no longer cached.
_AC_PROG_CC_C11([ac_prog_cc_stdc=c11
		 ac_cv_prog_cc_stdc=$ac_cv_prog_cc_c11],
  [_AC_PROG_CC_C99([ac_prog_cc_stdc=c99
		    ac_cv_prog_cc_stdc=$ac_cv_prog_cc_c99],
     [_AC_PROG_CC_C89([ac_prog_cc_stdc=c89
		       ac_cv_prog_cc_stdc=$ac_cv_prog_cc_c89],
		      [ac_prog_cc_stdc=no
		       ac_cv_prog_cc_stdc=no])])])
dnl
AC_LANG_POP(C)dnl
])# AC_PROG_CC



# AC_PROG_CXX([LIST-OF-COMPILERS])
# --------------------------------
# LIST-OF-COMPILERS is a space separated list of C++ compilers to search
# for (if not specified, a default list is used).  This just gives the
# user an opportunity to specify an alternative search list for the C++
# compiler.
# aCC	HP-UX C++ compiler much better than `CC', so test before.
# FCC   Fujitsu C++ compiler
# KCC	KAI C++ compiler
# RCC	Rational C++
# xlC_r	AIX C Set++ (with support for reentrant code)
# xlC	AIX C Set++
AC_DEFUN([AC_PROG_CXX],
[AC_LANG_PUSH(C++)dnl
AC_ARG_VAR([CXX],      [C++ compiler command])dnl
AC_ARG_VAR([CXXFLAGS], [C++ compiler flags])dnl
_AC_ARG_VAR_LDFLAGS()dnl
_AC_ARG_VAR_LIBS()dnl
_AC_ARG_VAR_CPPFLAGS()dnl
_AC_ARG_VAR_PRECIOUS([CCC])dnl
if test -z "$CXX"; then
  if test -n "$CCC"; then
    CXX=$CCC
  else
    AC_CHECK_TOOLS(CXX,
		   [m4_default([$1],
			       [g++ c++ gpp aCC CC cxx cc++ cl.exe FCC KCC RCC xlC_r xlC clang++])],
		   g++)
  fi
fi
# Provide some information about the compiler.
_AS_ECHO_LOG([checking for _AC_LANG compiler version])
set X $ac_compile
ac_compiler=$[2]
for ac_option in --version -v -V -qversion; do
  _AC_DO_LIMIT([$ac_compiler $ac_option >&AS_MESSAGE_LOG_FD])
done

m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
_AC_LANG_COMPILER_GNU
if test $ac_compiler_gnu = yes; then
  GXX=yes
else
  GXX=
fi
_AC_PROG_CXX_G
_AC_PROG_CXX_CXX11([ac_prog_cxx_stdcxx=cxx11
		    ac_cv_prog_cxx_stdcxx=$ac_cv_prog_cxx_cxx11
		    ac_cv_prog_cxx_cxx98=$ac_cv_prog_cxx_cxx11],
   [_AC_PROG_CXX_CXX98([ac_prog_cxx_stdcxx=cxx98
		        ac_cv_prog_cxx_stdcxx=$ac_cv_prog_cxx_cxx98],
		       [ac_prog_cxx_stdcxx=no
		        ac_cv_prog_cxx_stdcxx=no])])
AC_LANG_POP(C++)dnl
])# AC_PROG_CXX


# _AC_C_STD_TRY(STANDARD, TEST-PROLOGUE, TEST-BODY, OPTION-LIST,
#		ACTION-IF-AVAILABLE, ACTION-IF-UNAVAILABLE)
# --------------------------------------------------------------
# Check whether the C compiler accepts features of STANDARD (e.g `c89', `c99')
# by trying to compile a program of TEST-PROLOGUE and TEST-BODY.  If this fails,
# try again with each compiler option in the space-separated OPTION-LIST; if one
# helps, append it to CC.  If eventually successful, run ACTION-IF-AVAILABLE,
# else ACTION-IF-UNAVAILABLE.
AC_DEFUN([_AC_C_STD_TRY],
[AC_MSG_CHECKING([for $CC option to enable ]m4_translit($1, [c], [C])[ features])
AC_CACHE_VAL(ac_cv_prog_cc_$1,
[ac_cv_prog_cc_$1=no
ac_save_CC=$CC
AC_LANG_CONFTEST([AC_LANG_PROGRAM([$2], [$3])])
for ac_arg in '' $4
do
  CC="$ac_save_CC $ac_arg"
  _AC_COMPILE_IFELSE([], [ac_cv_prog_cc_$1=$ac_arg])
  test "x$ac_cv_prog_cc_$1" != "xno" && break
done
rm -f conftest.$ac_ext
CC=$ac_save_CC
])# AC_CACHE_VAL
ac_prog_cc_stdc_options=
case "x$ac_cv_prog_cc_$1" in
  x)
    AC_MSG_RESULT([none needed]) ;;
  xno)
    AC_MSG_RESULT([unsupported]) ;;
  *)
    ac_prog_cc_stdc_options=" $ac_cv_prog_cc_$1"
    CC=$CC$ac_prog_cc_stdc_options
    AC_MSG_RESULT([$ac_cv_prog_cc_$1]) ;;
esac
AS_IF([test "x$ac_cv_prog_cc_$1" != xno], [$5], [$6])
])# _AC_C_STD_TRY

# _AC_C_C99_TEST_HEADER
# ---------------------
# A C header suitable for testing for C99.
AC_DEFUN([_AC_C_C99_TEST_HEADER],
[[#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <wchar.h>
#include <stdio.h>

// Check varargs macros.  These examples are taken from C99 6.10.3.5.
#define debug(...) fprintf (stderr, __VA_ARGS__)
#define showlist(...) puts (#__VA_ARGS__)
#define report(test,...) ((test) ? puts (#test) : printf (__VA_ARGS__))
static void
test_varargs_macros (void)
{
  int x = 1234;
  int y = 5678;
  debug ("Flag");
  debug ("X = %d\n", x);
  showlist (The first, second, and third items.);
  report (x>y, "x is %d but y is %d", x, y);
}

// Check long long types.
#define BIG64 18446744073709551615ull
#define BIG32 4294967295ul
#define BIG_OK (BIG64 / BIG32 == 4294967297ull && BIG64 % BIG32 == 0)
#if !BIG_OK
  your preprocessor is broken;
#endif
#if BIG_OK
#else
  your preprocessor is broken;
#endif
static long long int bignum = -9223372036854775807LL;
static unsigned long long int ubignum = BIG64;

struct incomplete_array
{
  int datasize;
  double data[];
};

struct named_init {
  int number;
  const wchar_t *name;
  double average;
};

typedef const char *ccp;

static inline int
test_restrict (ccp restrict text)
{
  // See if C++-style comments work.
  // Iterate through items via the restricted pointer.
  // Also check for declarations in for loops.
  for (unsigned int i = 0; *(text+i) != '\0'; ++i)
    continue;
  return 0;
}

// Check varargs and va_copy.
static bool
test_varargs (const char *format, ...)
{
  va_list args;
  va_start (args, format);
  va_list args_copy;
  va_copy (args_copy, args);

  const char *str = "";
  int number = 0;
  float fnumber = 0;

  while (*format)
    {
      switch (*format++)
	{
	case 's': // string
	  str = va_arg (args_copy, const char *);
	  break;
	case 'd': // int
	  number = va_arg (args_copy, int);
	  break;
	case 'f': // float
	  fnumber = va_arg (args_copy, double);
	  break;
	default:
	  break;
	}
    }
  va_end (args_copy);
  va_end (args);

  return *str && number && fnumber;
}]])# _AC_C_C99_TEST_HEADER

# _AC_C_C99_TEST_BODY
# -------------------
# A C body suitable for testing for C99, assuming the corresponding header.
AC_DEFUN([_AC_C_C99_TEST_BODY],
[[
  // Check bool.
  _Bool success = false;

  // Check restrict.
  if (test_restrict ("String literal") == 0)
    success = true;
  char *restrict newvar = "Another string";

  // Check varargs.
  success &= test_varargs ("s, d' f .", "string", 65, 34.234);
  test_varargs_macros ();

  // Check flexible array members.
  struct incomplete_array *ia =
    malloc (sizeof (struct incomplete_array) + (sizeof (double) * 10));
  ia->datasize = 10;
  for (int i = 0; i < ia->datasize; ++i)
    ia->data[i] = i * 1.234;

  // Check named initializers.
  struct named_init ni = {
    .number = 34,
    .name = L"Test wide string",
    .average = 543.34343,
  };

  ni.number = 58;

  int dynamic_array[ni.number];
  dynamic_array[ni.number - 1] = 543;

  // work around unused variable warnings
  return (!success || bignum == 0LL || ubignum == 0uLL || newvar[0] == 'x'
	  || dynamic_array[ni.number - 1] != 543);
]])

# _AC_PROG_CC_C99 ([ACTION-IF-AVAILABLE], [ACTION-IF-UNAVAILABLE])
# ----------------------------------------------------------------
# If the C compiler is not in ISO C99 mode by default, try to add an
# option to output variable CC to make it so.  This macro tries
# various options that select ISO C99 on some system or another.  It
# considers the compiler to be in ISO C99 mode if it handles _Bool,
# // comments, flexible array members, inline, long long int, mixed
# code and declarations, named initialization of structs, restrict,
# va_copy, varargs macros, variable declarations in for loops and
# variable length arrays.
AC_DEFUN([_AC_PROG_CC_C99],
[_AC_C_STD_TRY([c99],
[_AC_C_C99_TEST_HEADER],
[_AC_C_C99_TEST_BODY],
dnl Try
dnl GCC		-std=gnu99 (unused restrictive modes: -std=c99 -std=iso9899:1999)
dnl IBM XL C	-qlanglvl=extc1x (V12.1; does not pass C11 test)
dnl IBM XL C	-qlanglvl=extc99
dnl		(pre-V12.1; unused restrictive mode: -qlanglvl=stdc99)
dnl HP cc	-AC99
dnl Intel ICC	-std=c99, -c99 (deprecated)
dnl IRIX	-c99
dnl Solaris	-D_STDC_C99=
dnl		cc's -xc99 option uses linker magic to define the external
dnl		symbol __xpg4 as if by "int __xpg4 = 1;", which enables C99
dnl		behavior for C library functions.  This is not wanted here,
dnl		because it means that a single module compiled with -xc99
dnl		alters C runtime behavior for the entire program, not for
dnl		just the module.  Instead, define the (private) symbol
dnl		_STDC_C99, which suppresses a bogus failure in <stdbool.h>.
dnl		The resulting compiler passes the test case here, and that's
dnl		good enough.  For more, please see the thread starting at:
dnl            https://lists.gnu.org/r/autoconf/2010-12/msg00059.html
dnl Tru64	-c99
dnl with extended modes being tried first.
[[-std=gnu99 -std=c99 -c99 -AC99 -D_STDC_C99= -qlanglvl=extc1x -qlanglvl=extc99]], [$1], [$2])[]dnl
])# _AC_PROG_CC_C99


# _AC_PROG_CC_C11 ([ACTION-IF-AVAILABLE], [ACTION-IF-UNAVAILABLE])
# ----------------------------------------------------------------
# If the C compiler is not in ISO C11 mode by default, try to add an
# option to output variable CC to make it so.  This macro tries
# various options that select ISO C11 on some system or another.  It
# considers the compiler to be in ISO C11 mode if it handles _Alignas,
# _Alignof, _Noreturn, _Static_assert, UTF-8 string literals,
# duplicate typedefs, and anonymous structures and unions.
AC_DEFUN([_AC_PROG_CC_C11],
[_AC_C_STD_TRY([c11],
[_AC_C_C99_TEST_HEADER[
// Check _Alignas.
char _Alignas (double) aligned_as_double;
char _Alignas (0) no_special_alignment;
extern char aligned_as_int;
char _Alignas (0) _Alignas (int) aligned_as_int;

// Check _Alignof.
enum
{
  int_alignment = _Alignof (int),
  int_array_alignment = _Alignof (int[100]),
  char_alignment = _Alignof (char)
};
_Static_assert (0 < -_Alignof (int), "_Alignof is signed");

// Check _Noreturn.
int _Noreturn does_not_return (void) { for (;;) continue; }

// Check _Static_assert.
struct test_static_assert
{
  int x;
  _Static_assert (sizeof (int) <= sizeof (long int),
                  "_Static_assert does not work in struct");
  long int y;
};

// Check UTF-8 literals.
#define u8 syntax error!
char const utf8_literal[] = u8"happens to be ASCII" "another string";

// Check duplicate typedefs.
typedef long *long_ptr;
typedef long int *long_ptr;
typedef long_ptr long_ptr;

// Anonymous structures and unions -- taken from C11 6.7.2.1 Example 1.
struct anonymous
{
  union {
    struct { int i; int j; };
    struct { int k; long int l; } w;
  };
  int m;
} v1;
]],
[_AC_C_C99_TEST_BODY[
  v1.i = 2;
  v1.w.k = 5;
  _Static_assert ((offsetof (struct anonymous, i)
		   == offsetof (struct anonymous, w.k)),
		  "Anonymous union alignment botch");
]],
dnl Try
dnl GCC		-std=gnu11 (unused restrictive mode: -std=c11)
dnl with extended modes being tried first.
dnl
dnl Do not try -qlanglvl=extc1x, because IBM XL C V12.1 (the latest version as
dnl of September 2012) does not pass the C11 test.  For now, try extc1x when
dnl compiling the C99 test instead, since it enables _Static_assert and
dnl _Noreturn, which is a win.  If -qlanglvl=extc11 or -qlanglvl=extc1x passes
dnl the C11 test in some future version of IBM XL C, we'll add it here,
dnl preferably extc11.
[[-std=gnu11]], [$1], [$2])[]dnl
])# _AC_PROG_CC_C11


# AC_PROG_CC_C89
# --------------
# Do not use AU_ALIAS here and in AC_PROG_CC_C99 and AC_PROG_CC_STDC,
# as that'd be incompatible with how Automake redefines AC_PROG_CC.  See
# <https://lists.gnu.org/r/autoconf/2012-10/msg00048.html>.
AU_DEFUN([AC_PROG_CC_C89],
  [AC_REQUIRE([AC_PROG_CC])],
  [$0 is obsolete; use AC_PROG_CC]
)

# AC_PROG_CC_C99
# --------------
AU_DEFUN([AC_PROG_CC_C99],
  [AC_REQUIRE([AC_PROG_CC])],
  [$0 is obsolete; use AC_PROG_CC]
)

# AC_PROG_CC_STDC
# ---------------
AU_DEFUN([AC_PROG_CC_STDC],
  [AC_REQUIRE([AC_PROG_CC])],
  [$0 is obsolete; use AC_PROG_CC]
)


# AC_C_PROTOTYPES
# ---------------
# Check if the C compiler supports prototypes, included if it needs
# options.
AC_DEFUN([AC_C_PROTOTYPES],
[AC_REQUIRE([AC_PROG_CC])dnl
if test "$ac_prog_cc_stdc" != no; then
  AC_DEFINE(PROTOTYPES, 1,
	    [Define to 1 if the C compiler supports function prototypes.])
  AC_DEFINE(__PROTOTYPES, 1,
	    [Define like PROTOTYPES; this can be used by system headers.])
fi
])# AC_C_PROTOTYPES


# _AC_CXX_STD_TRY(STANDARD, TEST-PROLOGUE, TEST-BODY, OPTION-LIST,
#		  ACTION-IF-AVAILABLE, ACTION-IF-UNAVAILABLE)
# ----------------------------------------------------------------
# Check whether the C++ compiler accepts features of STANDARD (e.g
# `cxx98', `cxx11') by trying to compile a program of TEST-PROLOGUE
# and TEST-BODY.  If this fails, try again with each compiler option
# in the space-separated OPTION-LIST; if one helps, append it to CXX.
# If eventually successful, run ACTION-IF-AVAILABLE, else
# ACTION-IF-UNAVAILABLE.
AC_DEFUN([_AC_CXX_STD_TRY],
[AC_MSG_CHECKING([for $CXX option to enable ]m4_translit(m4_translit($1, [x], [+]), [a-z], [A-Z])[ features])
AC_LANG_PUSH(C++)dnl
AC_CACHE_VAL(ac_cv_prog_cxx_$1,
[ac_cv_prog_cxx_$1=no
ac_save_CXX=$CXX
AC_LANG_CONFTEST([AC_LANG_PROGRAM([$2], [$3])])
for ac_arg in '' $4
do
  CXX="$ac_save_CXX $ac_arg"
  _AC_COMPILE_IFELSE([], [ac_cv_prog_cxx_$1=$ac_arg])
  test "x$ac_cv_prog_cxx_$1" != "xno" && break
done
rm -f conftest.$ac_ext
CXX=$ac_save_CXX
])# AC_CACHE_VAL
ac_prog_cxx_stdcxx_options=
case "x$ac_cv_prog_cxx_$1" in
  x)
    AC_MSG_RESULT([none needed]) ;;
  xno)
    AC_MSG_RESULT([unsupported]) ;;
  *)
    ac_prog_cxx_stdcxx_options=" $ac_cv_prog_cxx_$1"
    CXX=$CXX$ac_prog_cxx_stdcxx_options
    AC_MSG_RESULT([$ac_cv_prog_cxx_$1]) ;;
esac
AC_LANG_POP(C++)dnl
AS_IF([test "x$ac_cv_prog_cxx_$1" != xno], [$5], [$6])
])# _AC_CXX_STD_TRY

# _AC_CXX_CXX98_TEST_HEADER
# -------------------------
# A C++ header suitable for testing for CXX98.
AC_DEFUN([_AC_CXX_CXX98_TEST_HEADER],
[[
#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace test {
  typedef std::vector<std::string> string_vec;
  typedef std::pair<int,bool> map_value;
  typedef std::map<std::string,map_value> map_type;
  typedef std::set<int> set_type;

  template<typename T>
  class printer {
  public:
    printer(std::ostringstream& os): os(os) {}
    void operator() (T elem) { os << elem << std::endl; }
  private:
    std::ostringstream& os;
  };
}
]])# _AC_CXX_CXX98_TEST_HEADER

# _AC_CXX_CXX98_TEST_BODY
# -----------------------
# A C++ body suitable for testing for CXX98, assuming the corresponding header.
AC_DEFUN([_AC_CXX_CXX98_TEST_BODY],
[[

try {
  // Basic string.
  std::string teststr("ASCII text");
  teststr += " string";

  // Simple vector.
  test::string_vec testvec;
  testvec.push_back(teststr);
  testvec.push_back("foo");
  testvec.push_back("bar");
  if (testvec.size() != 3) {
    throw std::runtime_error("vector size is not 1");
  }

  // Dump vector into stringstream and obtain string.
  std::ostringstream os;
  for (test::string_vec::const_iterator i = testvec.begin();
       i != testvec.end(); ++i) {
    if (i + 1 != testvec.end()) {
      os << teststr << '\n';
    }
  }
  // Check algorithms work.
  std::for_each(testvec.begin(), testvec.end(), test::printer<std::string>(os));
  std::string os_out = os.str();

  // Test pair and map.
  test::map_type testmap;
  testmap.insert(std::make_pair(std::string("key"),
                                std::make_pair(53,false)));

  // Test set.
  int values[] = {9, 7, 13, 15, 4, 18, 12, 10, 5, 3, 14, 19, 17, 8, 6, 20, 16, 2, 11, 1};
  test::set_type testset(values, values + sizeof(values)/sizeof(values[0]));
  std::list<int> testlist(testset.begin(), testset.end());
  std::copy(testset.begin(), testset.end(), std::back_inserter(testlist));
} catch (const std::exception& e) {
  std::cerr << "Caught exception: " << e.what() << std::endl;

  // Test fstream
  std::ofstream of("test.txt");
  of << "Test ASCII text\n" << std::flush;
  of << "N= " << std::hex << std::setw(8) << std::left << 534 << std::endl;
  of.close();
}
std::exit(0);
]])

# _AC_CXX_CXX11_TEST_HEADER
# -------------------------
# A C++ header suitable for testing for CXX11.
AC_DEFUN([_AC_CXX_CXX11_TEST_HEADER],
[[
#include <deque>
#include <functional>
#include <memory>
#include <tuple>
#include <array>
#include <regex>
#include <iostream>

namespace cxx11test
{
  typedef std::shared_ptr<std::string> sptr;
  typedef std::weak_ptr<std::string> wptr;

  typedef std::tuple<std::string,int,double> tp;
  typedef std::array<int, 20> int_array;

  constexpr int get_val() { return 20; }

  struct testinit
  {
    int i;
    double d;
  };

  class delegate  {
  public:
    delegate(int n) : n(n) {}
    delegate(): delegate(2354) {}

    virtual int getval() { return this->n; };
  protected:
    int n;
  };

  class overridden : public delegate {
  public:
    overridden(int n): delegate(n) {}
    virtual int getval() override final { return this->n * 2; }
  };

  class nocopy {
  public:
    nocopy(int i): i(i) {}
    nocopy() = default;
    nocopy(const nocopy&) = delete;
    nocopy & operator=(const nocopy&) = delete;
  private:
    int i;
  };
}
]])# _AC_CXX_CXX11_TEST_HEADER

# _AC_CXX_CXX11_TEST_BODY
# -----------------------
# A C++ body suitable for testing for CXX11, assuming the corresponding header.
AC_DEFUN([_AC_CXX_CXX11_TEST_BODY],
[[
{
  // Test auto and decltype
  std::deque<int> d;
  d.push_front(43);
  d.push_front(484);
  d.push_front(3);
  d.push_front(844);
  int total = 0;
  for (auto i = d.begin(); i != d.end(); ++i) { total += *i; }

  auto a1 = 6538;
  auto a2 = 48573953.4;
  auto a3 = "String literal";

  decltype(a2) a4 = 34895.034;
}
{
  // Test constexpr
  short sa[cxx11test::get_val()] = { 0 };
}
{
  // Test initializer lists
  cxx11test::testinit il = { 4323, 435234.23544 };
}
{
  // Test range-based for and lambda
  cxx11test::int_array array = {9, 7, 13, 15, 4, 18, 12, 10, 5, 3, 14, 19, 17, 8, 6, 20, 16, 2, 11, 1};
  for (int &x : array) { x += 23; }
  std::for_each(array.begin(), array.end(), [](int v1){ std::cout << v1; });
}
{
  using cxx11test::sptr;
  using cxx11test::wptr;

  sptr sp(new std::string("ASCII string"));
  wptr wp(sp);
  sptr sp2(wp);
}
{
  cxx11test::tp tuple("test", 54, 45.53434);
  double d = std::get<2>(tuple);
  std::string s;
  int i;
  std::tie(s,i,d) = tuple;
}
{
  static std::regex filename_regex("^_?([a-z0-9_.]+-)+[a-z0-9]+$");
  std::string testmatch("Test if this string matches");
  bool match = std::regex_search(testmatch, filename_regex);
}
{
  cxx11test::int_array array = {9, 7, 13, 15, 4, 18, 12, 10, 5, 3, 14, 19, 17, 8, 6, 20, 16, 2, 11, 1};
  cxx11test::int_array::size_type size = array.size();
}
{
  // Test constructor delegation
  cxx11test::delegate d1;
  cxx11test::delegate d2();
  cxx11test::delegate d3(45);
}
{
  // Test override and final
  cxx11test::overridden o1(55464);
}
{
  // Test nullptr
  char *c = nullptr;
}
{
  // Test template brackets
  std::vector<std::pair<int,char*>> v1;
}
{
  // Unicode literals
  auto const *utf8 = u8"UTF-8 string \u2500"; // This is portable to C++20.
  char16_t const *utf16 = u"UTF-8 string \u2500";
  char32_t const *utf32 = U"UTF-32 string \u2500";
}
]])

# _AC_PROG_CXX_CXX98 ([ACTION-IF-AVAILABLE], [ACTION-IF-UNAVAILABLE])
# -------------------------------------------------------------------

# If the C++ compiler is not in ISO C++98 mode by default, try to add
# an option to output variable CXX to make it so.  This macro tries
# various options that select ISO C++98 on some system or another.  It
# considers the compiler to be in ISO C++98 mode if it handles basic
# features of the std namespace including: string, containers (list,
# map, set, vector), streams (fstreams, iostreams, stringstreams,
# iomanip), pair, exceptions and algorithms.


AC_DEFUN([_AC_PROG_CXX_CXX98],
[_AC_CXX_STD_TRY([cxx98],
[_AC_CXX_CXX98_TEST_HEADER],
[_AC_CXX_CXX98_TEST_BODY],
dnl Try
dnl GCC		-std=gnu++98 (unused restrictive mode: -std=c++98)
dnl IBM XL C	-qlanglvl=extended
dnl HP aC++	-AA
dnl Intel ICC	-std=gnu++98
dnl Solaris	N/A (default)
dnl Tru64	N/A (default, but -std gnu could be used)
dnl with extended modes being tried first.
[[-std=gnu++98 -std=c++98 -qlanglvl=extended -AA]], [$1], [$2])[]dnl
])# _AC_PROG_CXX_CXX98

# _AC_PROG_CXX_CXX11 ([ACTION-IF-AVAILABLE], [ACTION-IF-UNAVAILABLE])
# -------------------------------------------------------------------
# If the C++ compiler is not in ISO CXX11 mode by default, try to add
# an option to output variable CXX to make it so.  This macro tries
# various options that select ISO C++11 on some system or another.  It
# considers the compiler to be in ISO C++11 mode if it handles all the
# tests from the C++98 checks, plus the following: Language features
# (auto, constexpr, decltype, default/deleted constructors, delegate
# constructors, final, initializer lists, lambda functions, nullptr,
# override, range-based for loops, template brackets without spaces,
# unicode literals) and library features (array, memory (shared_ptr,
# weak_ptr), regex and tuple types).
AC_DEFUN([_AC_PROG_CXX_CXX11],
[_AC_CXX_STD_TRY([cxx11],
[_AC_CXX_CXX11_TEST_HEADER
_AC_CXX_CXX98_TEST_HEADER],
[_AC_CXX_CXX11_TEST_BODY
_AC_CXX_CXX98_TEST_BODY],
dnl Try
dnl GCC		-std=gnu++11 (unused restrictive mode: -std=c++11) [and 0x variants]
dnl IBM XL C	-qlanglvl=extended0x
dnl		(pre-V12.1; unused restrictive mode: -qlanglvl=stdcxx11)
dnl HP aC++	-AA
dnl Intel ICC	-std=c++11 -std=c++0x
dnl Solaris	N/A (no support)
dnl Tru64	N/A (no support)
dnl with extended modes being tried first.
[[-std=gnu++11 -std=c++11 -std=gnu++0x -std=c++0x -qlanglvl=extended0x -AA]], [$1], [$2])[]dnl
])# _AC_PROG_CXX_CXX11


])# m4_version_prereq
])# !_AC_C_C23_OPTIONS
