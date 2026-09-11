/* Compile-time assert-like macros.

   Copyright (C) 2005-2006, 2009-2026 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert, Bruno Haible, and Jim Meyering.  */

#ifndef _GL_VERIFY_H
#define _GL_VERIFY_H


/* Define _GL_HAVE__STATIC_ASSERT to 1 if _Static_assert (R, DIAGNOSTIC)
   works as per C11.  This is supported by GCC 4.6.0+ and by clang 4+.
   Check for __clang_major__ >= 5 instead of >= 4 because clang on Mac OS 10.7.5
   sets __clang_major__ to 4 even though it was derived from clang 3.2.

   Define _GL_HAVE__STATIC_ASSERT1 to 1 if _Static_assert (R) works as
   per C23.  This is supported by GCC 9.1+.

   Support compilers claiming conformance to the relevant standard,
   and also support GCC when not pedantic.  If we were willing to slow
   'configure' down we could also use it with other compilers, but
   since this affects only the quality of diagnostics, why bother?  */
#ifndef __cplusplus
# if (201112 <= __STDC_VERSION__ \
      || (!defined __STRICT_ANSI__ \
          && ((4 < __GNUC__ + (6 <= __GNUC_MINOR__) && !defined __clang__) \
              || 5 <= __clang_major__)))
#  define _GL_HAVE__STATIC_ASSERT 1
# endif
# if (202311 <= __STDC_VERSION__ \
      || (!defined __STRICT_ANSI__ && 9 <= __GNUC__ && !defined __clang__))
#  define _GL_HAVE__STATIC_ASSERT1 1
# endif
#endif

/* <sys/cdefs.h> in glibc may define _Static_assert with two arguments if the
   compiler is in C89 or C99 mode.  This matters especially with Clang:
   (1) glibc < 2.34, before the commit c8ba52ab3350 ("misc: Sync cdefs.h
       with gnulib"), doesn't check for __clang_major__ >= 4.
   (2) The above condition for _GL_HAVE__STATIC_ASSERT checks for
       __clang_major__ >= 5, so it's not in sync with glibc.  */
#if defined __GLIBC__ \
    && !defined __cplusplus && __STDC_VERSION__ < 201112
# undef _Static_assert
#endif

/* FreeBSD 9.1 <sys/cdefs.h>, included by <stddef.h> and lots of other
   system headers, defines a conflicting _Static_assert that is no
   better than ours; override it unless Gnulib's replacement <assert.h>
   (included from config.h) already overrode it.  */
#if ! (defined _GL_HAVE__STATIC_ASSERT || defined _GL_STATIC_ASSERT)
# include <stddef.h>
# undef _Static_assert
#endif

/* Each of these macros verifies that its argument R is nonzero.  To
   be portable, R should be an integer constant expression.  Unlike
   assert (R), there is no run-time overhead.

   If _Static_assert works, verify (R) uses it directly.  Similarly,
   _GL_VERIFY_TRUE works by packaging a _Static_assert inside a struct
   that is an operand of sizeof.

   The code below uses several ideas for C++ compilers, and for C
   compilers that do not support _Static_assert:

   * The first step is ((R) ? 1 : -1).  Given an expression R, of
     integral or boolean or floating-point type, this yields an
     expression of integral type, whose value is later verified to be
     constant and nonnegative.

   * Next this expression W is wrapped in a type
     struct _gl_verify_type {
       unsigned int _gl_verify_error_if_negative: W;
     }.
     If W is negative, this yields a compile-time error.  No compiler can
     deal with a bit-field of negative size.

     One might think that an array size check would have the same
     effect, that is, that the type struct { unsigned int dummy[W]; }
     would work as well.  However, inside a function, some compilers
     (such as C++ compilers and GNU C) allow local parameters and
     variables inside array size expressions.  With these compilers,
     an array size check would not properly diagnose this misuse of
     the verify macro:

       void function (int n) { verify (n < 0); }

   * For the verify macro, the struct _gl_verify_type will need to
     somehow be embedded into a declaration.  To be portable, this
     declaration must declare an object, a constant, a function, or a
     typedef name.  If the declared entity uses the type directly,
     such as in

       struct dummy {...};
       typedef struct {...} dummy;
       extern struct {...} *dummy;
       extern void dummy (struct {...} *);
       extern struct {...} *dummy (void);

     two uses of the verify macro would yield colliding declarations
     if the entity names are not disambiguated.  A workaround is to
     attach the current line number to the entity name:

       #define _GL_CONCAT0(x, y) x##y
       #define _GL_CONCAT(x, y) _GL_CONCAT0 (x, y)
       extern struct {...} * _GL_CONCAT (dummy, __LINE__);

     But this has the problem that two invocations of verify from
     within the same macro would collide, since the __LINE__ value
     would be the same for both invocations.  (The GCC __COUNTER__
     macro solves this problem, but is not portable.)

     A solution is to use the sizeof operator.  It yields a number,
     getting rid of the identity of the type.  Declarations like

       extern int dummy [sizeof (struct {...})];
       extern void dummy (int [sizeof (struct {...})]);
       extern int (*dummy (void)) [sizeof (struct {...})];

     can be repeated.

   * Should the implementation use a named struct or an unnamed struct?
     Which of the following alternatives can be used?

       extern int dummy [sizeof (struct {...})];
       extern int dummy [sizeof (struct _gl_verify_type {...})];
       extern void dummy (int [sizeof (struct {...})]);
       extern void dummy (int [sizeof (struct _gl_verify_type {...})]);
       extern int (*dummy (void)) [sizeof (struct {...})];
       extern int (*dummy (void)) [sizeof (struct _gl_verify_type {...})];

     In the second and sixth case, the struct type is exported to the
     outer scope; two such declarations therefore collide.  GCC warns
     about the first, third, and fourth cases.  So the only remaining
     possibility is the fifth case:

       extern int (*dummy (void)) [sizeof (struct {...})];

   * GCC warns about duplicate declarations of the dummy function if
     -Wredundant-decls is used.  GCC 4.3 and later have a builtin
     __COUNTER__ macro that can let us generate unique identifiers for
     each dummy function, to suppress this warning.

   * This implementation exploits the fact that older versions of GCC,
     which do not support _Static_assert, also do not warn about the
     last declaration mentioned above.

   * GCC warns if -Wnested-externs is enabled and 'verify' is used
     within a function body; but inside a function, you can always
     arrange to use verify_expr instead.

   * In C++, any struct definition inside sizeof is invalid.
     Use a template type to work around the problem.  */

/* Concatenate two preprocessor tokens.  */
#define _GL_CONCAT(x, y) _GL_CONCAT0 (x, y)
#define _GL_CONCAT0(x, y) x##y

/* _GL_COUNTER is an integer, preferably one that changes each time we
   use it.  Use __COUNTER__ if it works (it does so with most compilers,
   see <https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3457.htm>),
   falling back on __LINE__ otherwise.  __LINE__ isn't perfect, but it's
   better than a constant.  */
#if defined __COUNTER__ && __COUNTER__ != __COUNTER__
# define _GL_COUNTER __COUNTER__
#else
# define _GL_COUNTER __LINE__
#endif

/* Generate a symbol with the given prefix, making it unique if
   possible.  */
#define _GL_GENSYM(prefix) _GL_CONCAT (prefix, _GL_COUNTER)

/* Verify requirement R at compile-time, as an integer constant expression
   that returns 1.  If R is false, fail at compile-time, preferably
   with a diagnostic that includes the string-literal DIAGNOSTIC.  */

#define _GL_VERIFY_TRUE(R, DIAGNOSTIC) \
   (!!sizeof (_GL_VERIFY_TYPE (R, DIAGNOSTIC)))

#ifdef __cplusplus
# if !GNULIB_defined_struct__gl_verify_type
template <int w>
  struct _gl_verify_type {
    unsigned int _gl_verify_error_if_negative: w;
  };
#  define GNULIB_defined_struct__gl_verify_type 1
# endif
# define _GL_VERIFY_TYPE(R, DIAGNOSTIC) \
    _gl_verify_type<(R) ? 1 : -1>
#elif defined _GL_HAVE__STATIC_ASSERT
# define _GL_VERIFY_TYPE(R, DIAGNOSTIC) \
    struct { \
      _Static_assert (R, DIAGNOSTIC); \
      int _gl_dummy; \
    }
#else
# define _GL_VERIFY_TYPE(R, DIAGNOSTIC) \
    struct { unsigned int _gl_verify_error_if_negative: (R) ? 1 : -1; }
#endif

/* Verify requirement R at compile-time, as a declaration without a
   trailing ';'.  If R is false, fail at compile-time.

   This macro requires three or more arguments but uses at most the first
   two, so that the _Static_assert macro optionally defined below supports
   both the C11 two-argument syntax and the C23 one-argument syntax.

   Unfortunately, unlike C11, this implementation must appear as an
   ordinary declaration, and cannot appear inside struct { ... }.  */

#if 202311 <= __STDC_VERSION__ || 200410 <= __cpp_static_assert
# define _GL_VERIFY(R, DIAGNOSTIC, ...) static_assert (R, DIAGNOSTIC)
#elif defined _GL_HAVE__STATIC_ASSERT
# define _GL_VERIFY(R, DIAGNOSTIC, ...) _Static_assert (R, DIAGNOSTIC)
#else
# define _GL_VERIFY(R, DIAGNOSTIC, ...) \
    extern int (*_GL_GENSYM (_gl_verify_function) (void)) \
      [_GL_VERIFY_TRUE (R, DIAGNOSTIC)]
# if 4 < __GNUC__ + (6 <= __GNUC_MINOR__) && !defined __clang__
#  pragma GCC diagnostic ignored "-Wnested-externs"
# endif
#endif

/* _GL_STATIC_ASSERT_H is defined if this code is copied into assert.h.  */
#ifdef _GL_STATIC_ASSERT_H
/* Define _Static_assert if needed.  */
/* With clang ≥ 3.8.0 in C++ mode, _Static_assert already works and accepts
   1 or 2 arguments.  We better don't override it, because clang's standard
   C++ library uses static_assert inside classes in several places, and our
   replacement via _GL_VERIFY does not work in these contexts.  */
# if (defined __cplusplus && defined __clang__ \
      && (4 <= __clang_major__ + (8 <= __clang_minor__)))
#  if 5 <= __clang_major__
/* Avoid "warning: 'static_assert' with no message is a C++17 extension".  */
#   pragma clang diagnostic ignored "-Wc++17-extensions"
#  else
/* Avoid "warning: static_assert with no message is a C++1z extension".  */
#   pragma clang diagnostic ignored "-Wc++1z-extensions"
#  endif
# elif !defined _GL_HAVE__STATIC_ASSERT1 && !defined _Static_assert
#  if !defined _MSC_VER || defined __clang__
#   define _Static_assert(...) \
      _GL_VERIFY (__VA_ARGS__, "static assertion failed", -)
#  else
#   if defined __cplusplus && _MSC_VER >= 1910
     /* In MSVC 14.1 or newer, static_assert accepts one or two arguments,
        but _Static_assert is not defined.  */
#    define _Static_assert static_assert
#   else
     /* Work around MSVC preprocessor incompatibility with ISO C; see
        <https://stackoverflow.com/questions/5134523/>.  */
#    define _Static_assert(R, ...) \
       _GL_VERIFY ((R), "static assertion failed", -)
#   endif
#  endif
# endif
/* Define static_assert if needed.  */
# if defined __cplusplus && defined __clang__ && __clang_major__ < 9
/* clang++ before commit 5c739665a8721228cf6143fd4ef95870a59f55ae had a
   two-arguments static_assert but not the one-argument static_assert.  */
#  undef static_assert
# endif
# if (!defined static_assert \
      && __STDC_VERSION__ < 202311 \
      && (!defined __cplusplus \
          || (__cpp_static_assert < 201411 \
              && __GNUG__ < 6 && __clang_major__ < 6 && _MSC_VER < 1910)))
#  if (defined __cplusplus && defined __GNUG__ && __GNUG__ < 6 \
       && __cplusplus == 201103L && !defined __clang__)
/* g++ >= 4.7, < 6 with option -std=c++11 or -std=gnu++11 supports the
   two-arguments static_assert but not the one-argument static_assert, and
   it does not support _Static_assert.
   We have to play preprocessor tricks to distinguish the two cases.  */
#   define _GL_SA1(a1) static_assert ((a1), "static assertion failed")
#   define _GL_SA2 static_assert
#   define _GL_SA3 static_assert
#   define _GL_SA_PICK(x1,x2,x3,x4,...) x4
#   define static_assert(...) _GL_SA_PICK(__VA_ARGS__,_GL_SA3,_GL_SA2,_GL_SA1) (__VA_ARGS__)
#  elif defined __cplusplus && _MSC_VER >= 1900 && !defined __clang__
/* MSVC 14 in C++ mode supports the two-arguments static_assert but not
   the one-argument static_assert, and it does not support _Static_assert.
   We have to play preprocessor tricks to distinguish the two cases.
   Since the MSVC preprocessor is not ISO C compliant (see above),
   the solution is specific to MSVC.  */
#   define _GL_EXPAND(x) x
#   define _GL_SA1(a1) static_assert ((a1), "static assertion failed")
#   define _GL_SA2 static_assert
#   define _GL_SA3 static_assert
#   define _GL_SA_PICK(x1,x2,x3,x4,...) x4
#   define static_assert(...) _GL_EXPAND(_GL_SA_PICK(__VA_ARGS__,_GL_SA3,_GL_SA2,_GL_SA1)) (__VA_ARGS__)
/* Avoid "fatal error C1189: #error:  The C++ Standard Library forbids macroizing keywords."  */
#   define _ALLOW_KEYWORD_MACROS 1
#  else
#   define static_assert _Static_assert /* C11 requires this #define. */
#  endif
# endif
#endif

/* @assert.h omit start@  */

#if defined __clang_major__ && __clang_major__ < 5
# define _GL_HAS_BUILTIN_TRAP 0
#elif 3 < __GNUC__ + (3 < __GNUC_MINOR__ + (4 <= __GNUC_PATCHLEVEL__))
# define _GL_HAS_BUILTIN_TRAP 1
#elif defined __has_builtin
# define _GL_HAS_BUILTIN_TRAP __has_builtin (__builtin_trap)
#else
# define _GL_HAS_BUILTIN_TRAP 0
#endif

#ifndef _GL_HAS_BUILTIN_UNREACHABLE
# if defined __clang_major__ && __clang_major__ < 5
#  define _GL_HAS_BUILTIN_UNREACHABLE 0
# elif 4 < __GNUC__ + (5 <= __GNUC_MINOR__) && !defined __clang__
#  define _GL_HAS_BUILTIN_UNREACHABLE 1
# elif defined __has_builtin
#  define _GL_HAS_BUILTIN_UNREACHABLE __has_builtin (__builtin_unreachable)
# else
#  define _GL_HAS_BUILTIN_UNREACHABLE 0
# endif
#endif

/* Each of these macros verifies that its argument R is nonzero.  To
   be portable, R should be an integer constant expression.  Unlike
   assert (R), there is no run-time overhead.

   There are two macros, since no single macro can be used in all
   contexts in C.  verify_expr (R, E) is for scalar contexts, including
   integer constant expression contexts.  verify (R) is for declaration
   contexts, e.g., the top level.  */

/* Verify requirement R at compile-time.  Return the value of the
   expression E.  */

#define verify_expr(R, E) \
   (_GL_VERIFY_TRUE (R, "verify_expr (" #R ", " #E ")") ? (E) : (E))

/* Verify requirement R at compile-time, as a declaration without a
   trailing ';'.  verify (R) acts like static_assert (R) except that
   it is portable to C11/C++14 and earlier, it can issue better
   diagnostics, and its name is shorter and may be more convenient.  */

#ifdef __PGI
/* PGI barfs if R is long.  */
# define verify(R) _GL_VERIFY (R, "verify (...)", -)
#else
# define verify(R) _GL_VERIFY (R, "verify (" #R ")", -)
#endif

/* Assume that R always holds.  Behavior is undefined if R is false,
   fails to evaluate, or has side effects.

   'assume (R)' is a directive from the programmer telling the
   compiler that R is true so the compiler needn't generate code to
   test R.  This is why 'assume' is in verify.h: it's related to
   static checking (in this case, static checking done by the
   programmer), not dynamic checking.

   'assume (R)' can affect compilation of all the code, not just code
   that happens to be executed after the assume (R) is "executed".
   For example, if the code mistakenly does 'assert (R); assume (R);'
   the compiler is entitled to optimize away the 'assert (R)'.

   Although assuming R can help a compiler generate better code or
   diagnostics, performance can suffer if R uses hard-to-optimize
   features such as function calls not inlined by the compiler.

   Avoid Clang's __builtin_assume, as it breaks GNU Emacs master
   as of 2020-08-23T21:09:49Z!eggert@cs.ucla.edu; see
   <https://bugs.gnu.org/43152#71>.  It's not known whether this breakage
   is a Clang bug or an Emacs bug; play it safe for now.  */

#if _GL_HAS_BUILTIN_UNREACHABLE
# define assume(R) ((R) ? (void) 0 : __builtin_unreachable ())
#elif 1200 <= _MSC_VER
# define assume(R) __assume (R)
#elif 202311 <= __STDC_VERSION__
# include <stddef.h>
# define assume(R) ((R) ? (void) 0 : unreachable ())
#elif (defined GCC_LINT || defined lint) && _GL_HAS_BUILTIN_TRAP
  /* Doing it this way helps various packages when configured with
     --enable-gcc-warnings, which compiles with -Dlint.  It's nicer
     if 'assume' silences warnings with GCC 3.4 through GCC 4.4.7 (2012).  */
# define assume(R) ((R) ? (void) 0 : __builtin_trap ())
#else
  /* Some older tools grok NOTREACHED, e.g., Oracle Studio 12.6 (2017).  */
# define assume(R) ((R) ? (void) 0 : /*NOTREACHED*/ (void) 0)
#endif

/* @assert.h omit end@  */

#endif
