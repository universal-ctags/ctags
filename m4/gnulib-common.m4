# gnulib-common.m4
# serial 122
dnl Copyright (C) 2007-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

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
  AH_VERBATIM([0witness],
[/* Witness that <config.h> has been included.  */
#define _GL_CONFIG_H_INCLUDED 1
])
  dnl Avoid warnings from gcc -Wtrailing-whitespace.
  dnl This is a temporary workaround until Autoconf fixes it.
  dnl Test case:
  dnl empty1=; empty2=; AC_DEFINE_UNQUOTED([FOO], [$empty1$empty2], [...])
  dnl should produce "#define FOO /**/", not "#define FOO ".
  AH_TOP([#if defined __GNUC__ && __GNUC__ >= 15 && !defined __clang__
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wtrailing-whitespace"
#endif
])
  AH_BOTTOM([#if defined __GNUC__ && __GNUC__ >= 15 && !defined __clang__
# pragma GCC diagnostic pop
#endif
])
  AH_VERBATIM([_GL_GNUC_PREREQ],
[/* True if the compiler says it groks GNU C version MAJOR.MINOR.
    Except that
      - clang groks GNU C 4.2, even on Windows, where it does not define
        __GNUC__.
      - The OpenMandriva-modified clang compiler pretends that it groks
        GNU C version 13.1, but it doesn't: It does not support
        __attribute__ ((__malloc__ (f, i))), nor does it support
        __attribute__ ((__warning__ (message))) on a function redeclaration.
      - Users can make clang lie as well, through the -fgnuc-version option.  */
#if defined __GNUC__ && defined __GNUC_MINOR__ && !defined __clang__
# define _GL_GNUC_PREREQ(major, minor) \
    ((major) < __GNUC__ + ((minor) <= __GNUC_MINOR__))
#elif defined __clang__
  /* clang really only groks GNU C 4.2.  */
# define _GL_GNUC_PREREQ(major, minor) \
    ((major) < 4 + ((minor) <= 2))
#else
# define _GL_GNUC_PREREQ(major, minor) 0
#endif
])
  AH_VERBATIM([_Noreturn],
[/* The _Noreturn keyword of C11.
   Do not use [[noreturn]], because with it the syntax
     extern _Noreturn void func (...);
   would not be valid; such a declaration would be valid only with 'extern'
   and '_Noreturn' swapped, or without the 'extern' keyword.  However, some
   AIX system header files and several gnulib header files use precisely
   this syntax with 'extern'.  So even though C23 deprecates _Noreturn,
   it is currently more portable to prefer it to [[noreturn]].

   Also, do not try to work around LLVM bug 59792 (clang 15 or earlier).
   This rare bug can be worked around by compiling with 'clang -D_Noreturn=',
   though the workaround may generate many false-alarm warnings.  */
#ifndef _Noreturn
# if ((!defined __cplusplus || defined __clang__) \
      && (201112 <= (defined __STDC_VERSION__ ? __STDC_VERSION__ : 0)))
   /* _Noreturn works as-is.  */
# elif _GL_GNUC_PREREQ (2, 8) || defined __clang__ || 0x5110 <= __SUNPRO_C
   /* Prefer __attribute__ ((__noreturn__)) to plain _Noreturn even if the
      latter works, as 'gcc -std=gnu99 -Wpedantic' warns about _Noreturn.  */
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
/* Define _GL_HAS_ATTRIBUTE only once, because on FreeBSD, with gcc < 5, if
   <config.h> gets included once again after <sys/cdefs.h>, __has_attribute(x)
   expands to 0 always, and redefining _GL_HAS_ATTRIBUTE would turn off all
   attributes.  */
#ifndef _GL_HAS_ATTRIBUTE
# if (defined __has_attribute \
      && (!defined __clang_minor__ \
          || (defined __apple_build_version__ \
              ? 7000000 <= __apple_build_version__ \
              : 5 <= __clang_major__)))
#  define _GL_HAS_ATTRIBUTE(attr) __has_attribute (__##attr##__)
# else
#  define _GL_HAS_ATTRIBUTE(attr) _GL_ATTR_##attr
/* The following lines list the first GCC version that supports the attribute.
   Although the lines are not used in GCC 5 and later (as GCC 5 introduced
   __has_attribute support), list GCC versions 5+ anyway for completeness.  */
#  define _GL_ATTR_alloc_size _GL_GNUC_PREREQ (4, 3)
#  define _GL_ATTR_always_inline _GL_GNUC_PREREQ (3, 2)
#  define _GL_ATTR_artificial _GL_GNUC_PREREQ (4, 3)
#  define _GL_ATTR_cold _GL_GNUC_PREREQ (4, 3)
#  define _GL_ATTR_const _GL_GNUC_PREREQ (2, 95)
#  define _GL_ATTR_deprecated _GL_GNUC_PREREQ (3, 1)
#  define _GL_ATTR_diagnose_if 0
#  define _GL_ATTR_error _GL_GNUC_PREREQ (4, 3)
#  define _GL_ATTR_externally_visible _GL_GNUC_PREREQ (4, 1)
#  define _GL_ATTR_fallthrough _GL_GNUC_PREREQ (7, 0)
#  define _GL_ATTR_format _GL_GNUC_PREREQ (2, 7)
#  define _GL_ATTR_leaf _GL_GNUC_PREREQ (4, 6)
#  define _GL_ATTR_malloc _GL_GNUC_PREREQ (3, 0)
#  ifdef _ICC
#   define _GL_ATTR_may_alias 0
#  else
#   define _GL_ATTR_may_alias _GL_GNUC_PREREQ (3, 3)
#  endif
#  define _GL_ATTR_noinline _GL_GNUC_PREREQ (3, 1)
#  define _GL_ATTR_nonnull _GL_GNUC_PREREQ (3, 3)
#  define _GL_ATTR_nonnull_if_nonzero _GL_GNUC_PREREQ (15, 1)
#  define _GL_ATTR_nonstring _GL_GNUC_PREREQ (8, 0)
#  define _GL_ATTR_nothrow _GL_GNUC_PREREQ (3, 3)
#  define _GL_ATTR_packed _GL_GNUC_PREREQ (2, 7)
#  define _GL_ATTR_pure _GL_GNUC_PREREQ (2, 96)
#  define _GL_ATTR_reproducible _GL_GNUC_PREREQ (15, 1)
#  define _GL_ATTR_returns_nonnull _GL_GNUC_PREREQ (4, 9)
#  define _GL_ATTR_sentinel _GL_GNUC_PREREQ (4, 0)
#  define _GL_ATTR_unsequenced _GL_GNUC_PREREQ (15, 1)
#  define _GL_ATTR_unused _GL_GNUC_PREREQ (2, 7)
#  define _GL_ATTR_warn_unused_result _GL_GNUC_PREREQ (3, 4)
# endif
#endif

/* Use __has_c_attribute if available.  However, do not use with
   pre-C23 GCC, which can issue false positives if -Wpedantic.  */
#if (defined __has_c_attribute \
     && ! (_GL_GNUC_PREREQ (4, 6) \
           && (defined __STDC_VERSION__ ? __STDC_VERSION__ : 0) <= 201710))
# define _GL_HAVE___HAS_C_ATTRIBUTE 1
#else
# define _GL_HAVE___HAS_C_ATTRIBUTE 0
#endif

/* Attributes in bracket syntax [[...]] vs. attributes in __attribute__((...))
   syntax, in function declarations.  There are two problems here.
   (Last tested with gcc/g++ 14 and clang/clang++ 18.)

   1) We want that the _GL_ATTRIBUTE_* can be cumulated on the same declaration
      in any order.
      =========================== foo.c = foo.cc ===========================
      __attribute__ ((__deprecated__)) [[__nodiscard__]] int bar1 (int);
      [[__nodiscard__]] __attribute__ ((__deprecated__)) int bar2 (int);
      ======================================================================
      This gives a syntax error
        - in C mode with gcc
          <https://gcc.gnu.org/PR108796>, and
        - in C++ mode with clang++ version < 16, and
        - in C++ mode, inside extern "C" {}, still in newer clang++ versions
          <https://github.com/llvm/llvm-project/issues/101990>.
 */
/* Define if, in a function declaration, the attributes in bracket syntax
   [[...]] must come before the attributes in __attribute__((...)) syntax.
   If this is defined, it is best to avoid the bracket syntax, so that the
   various _GL_ATTRIBUTE_* can be cumulated on the same declaration in any
   order.  */
#ifdef __cplusplus
# if defined __clang__
#  define _GL_BRACKET_BEFORE_ATTRIBUTE 1
# endif
#else
# if defined __GNUC__ && !defined __clang__
#  define _GL_BRACKET_BEFORE_ATTRIBUTE 1
# endif
#endif
/*
   2) We want that the _GL_ATTRIBUTE_* can be placed in a declaration
        - without 'extern', in C as well as in C++,
        - with 'extern', in C,
        - with 'extern "C"', in C++
      in the same position.  That is, we don't want to be forced to use a
      macro which arranges for the attribute to come before 'extern' in
      one case and after 'extern' in the other case, because such a macro
      would make the source code of .h files pretty ugly.
      =========================== foo.c = foo.cc ===========================
      #ifdef __cplusplus
      # define CC "C"
      #else
      # define CC
      #endif

      #define ND   [[__nodiscard__]]
      #define WUR  __attribute__((__warn_unused_result__))

      #ifdef __cplusplus
      extern "C" {
      #endif
                                        // gcc   clang  g++   clang++

      ND int foo (int);
      int ND foo (int);                 // warn  error  warn  error
      int foo ND (int);
      int foo (int) ND;                 // warn  error  warn  error

      WUR int foo (int);
      int WUR foo (int);
      int fo1 WUR (int);                // error error  error error
      int foo (int) WUR;

      #ifdef __cplusplus
      }
      #endif

                                        // gcc   clang  g++   clang++

      ND extern CC int foo (int);       //              error error
      extern CC ND int foo (int);       // error error
      extern CC int ND foo (int);       // warn  error  warn  error
      extern CC int foo ND (int);
      extern CC int foo (int) ND;       // warn  error  warn  error

      WUR extern CC int foo (int);      //              warn
      extern CC WUR int foo (int);
      extern CC int WUR foo (int);
      extern CC int foo WUR (int);      // error error  error error
      extern CC int foo (int) WUR;

      ND EXTERN_C_FUNC int foo (int);   //              error error
      EXTERN_C_FUNC ND int foo (int);
      EXTERN_C_FUNC int ND foo (int);   // warn  error  warn  error
      EXTERN_C_FUNC int foo ND (int);
      EXTERN_C_FUNC int foo (int) ND;   // warn  error  warn  error

      WUR EXTERN_C_FUNC int foo (int);  //              warn
      EXTERN_C_FUNC WUR int foo (int);
      EXTERN_C_FUNC int WUR foo (int);
      EXTERN_C_FUNC int fo2 WUR (int);  // error error  error error
      EXTERN_C_FUNC int foo (int) WUR;
      ======================================================================
      So, if we insist on using the 'extern' keyword ('extern CC' idiom):
        * If _GL_ATTRIBUTE_* expands to bracket syntax [[...]]
          in both C and C++, there is one available position:
            - between the function name and the parameter list.
        * If _GL_ATTRIBUTE_* expands to __attribute__((...)) syntax
          in both C and C++, there are several available positions:
            - before the return type,
            - between return type and function name,
            - at the end of the declaration.
        * If _GL_ATTRIBUTE_* expands to bracket syntax [[...]] in C and to
          __attribute__((...)) syntax in C++, there is no available position:
          it would need to come before 'extern' in C but after 'extern "C"'
          in C++.
        * If _GL_ATTRIBUTE_* expands to __attribute__((...)) syntax in C and
          to bracket syntax [[...]] in C++, there is one available position:
            - before the return type.
      Whereas, if we use the 'EXTERN_C_FUNC' idiom, which conditionally
      omits the 'extern' keyword:
        * If _GL_ATTRIBUTE_* expands to bracket syntax [[...]]
          in both C and C++, there are two available positions:
            - before the return type,
            - between the function name and the parameter list.
        * If _GL_ATTRIBUTE_* expands to __attribute__((...)) syntax
          in both C and C++, there are several available positions:
            - before the return type,
            - between return type and function name,
            - at the end of the declaration.
        * If _GL_ATTRIBUTE_* expands to bracket syntax [[...]] in C and to
          __attribute__((...)) syntax in C++, there is one available position:
            - before the return type.
        * If _GL_ATTRIBUTE_* expands to __attribute__((...)) syntax in C and
          to bracket syntax [[...]] in C++, there is one available position:
            - before the return type.
      The best choice is therefore to use the 'EXTERN_C_FUNC' idiom and
      put the attributes before the return type. This works regardless
      to what the _GL_ATTRIBUTE_* macros expand.
 */

/* Attributes in bracket syntax [[...]] vs. attributes in __attribute__((...))
   syntax, in static/inline function definitions.

   There are similar constraints as for function declarations.  However, here,
   we cannot omit the storage-class specifier.  Therefore, the following rule
   applies:
     * The macros
         _GL_ATTRIBUTE_DEPRECATED
         _GL_ATTRIBUTE_MAYBE_UNUSED
         _GL_ATTRIBUTE_NODISCARD
         _GL_ATTRIBUTE_REPRODUCIBLE
         _GL_ATTRIBUTE_UNSEQUENCED
       which may expand to bracket syntax [[...]], must come first, before the
       storage-class specifier.
     * Other _GL_ATTRIBUTE_* macros, that expand to __attribute__((...)) syntax,
       are better placed between the storage-class specifier and the return
       type.
 */

/* Attributes in bracket syntax [[...]] vs. attributes in __attribute__((...))
   syntax, in variable declarations.

   At which position can they be placed?
   (Last tested with gcc/g++ 14 and clang/clang++ 18.)

      =========================== foo.c = foo.cc ===========================
      #ifdef __cplusplus
      # define CC "C"
      #else
      # define CC
      #endif

      #define BD   [[__deprecated__]]
      #define AD   __attribute__ ((__deprecated__))

                              // gcc   clang  g++    clang++

      BD extern CC int var;   //              error  error
      extern CC BD int var;   // error error
      extern CC int BD var;   // warn  error  warn   error
      extern CC int var BD;

      AD extern CC int var;   //              warn
      extern CC AD int var;
      extern CC int AD var;
      extern CC int var AD;

      BD extern CC int z[];   //              error  error
      extern CC BD int z[];   // error error
      extern CC int BD z[];   // warn  error  warn   error
      extern CC int z1 BD [];
      extern CC int z[] BD;   // warn  error         error

      AD extern CC int z[];   //              warn
      extern CC AD int z[];
      extern CC int AD z[];
      extern CC int z2 AD []; // error error  error  error
      extern CC int z[] AD;
      ======================================================================

   * For non-array variables, the only good position is after the variable name,
     that is, at the end of the declaration.
   * For array variables, you will need to distinguish C and C++:
       - In C, before the 'extern' keyword.
       - In C++, between the 'extern "C"' and the variable's type.
 */
]dnl There is no _GL_ATTRIBUTE_ALIGNED; use stdalign's alignas instead.
[
/* _GL_ATTRIBUTE_ALLOC_SIZE ((N)) declares that the Nth argument of the function
   is the size of the returned memory block.
   _GL_ATTRIBUTE_ALLOC_SIZE ((M, N)) declares that the Mth argument multiplied
   by the Nth argument of the function is the size of the returned memory block.
 */
/* Applies to: functions, pointer to functions, function types.  */
#ifndef _GL_ATTRIBUTE_ALLOC_SIZE
# if _GL_HAS_ATTRIBUTE (alloc_size)
#  define _GL_ATTRIBUTE_ALLOC_SIZE(args) __attribute__ ((__alloc_size__ args))
# else
#  define _GL_ATTRIBUTE_ALLOC_SIZE(args)
# endif
#endif

/* _GL_ATTRIBUTE_ALWAYS_INLINE tells that the compiler should always inline the
   function and report an error if it cannot do so.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_ALWAYS_INLINE
# if _GL_HAS_ATTRIBUTE (always_inline)
#  define _GL_ATTRIBUTE_ALWAYS_INLINE __attribute__ ((__always_inline__))
# else
#  define _GL_ATTRIBUTE_ALWAYS_INLINE
# endif
#endif

/* _GL_ATTRIBUTE_ARTIFICIAL declares that the function is not important to show
    in stack traces when debugging.  The compiler should omit the function from
    stack traces.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_ARTIFICIAL
# if _GL_HAS_ATTRIBUTE (artificial)
#  define _GL_ATTRIBUTE_ARTIFICIAL __attribute__ ((__artificial__))
# else
#  define _GL_ATTRIBUTE_ARTIFICIAL
# endif
#endif

/* _GL_ATTRIBUTE_COLD declares that the function is rarely executed.  */
/* Applies to: functions.  */
/* Avoid __attribute__ ((cold)) on MinGW; see thread starting at
   <https://lists.gnu.org/r/emacs-devel/2019-04/msg01152.html>.
   Also, Oracle Studio 12.6 requires 'cold' not '__cold__'.  */
#ifndef _GL_ATTRIBUTE_COLD
# if _GL_HAS_ATTRIBUTE (cold) && !defined __MINGW32__
#  ifndef __SUNPRO_C
#   define _GL_ATTRIBUTE_COLD __attribute__ ((__cold__))
#  else
#   define _GL_ATTRIBUTE_COLD __attribute__ ((cold))
#  endif
# else
#  define _GL_ATTRIBUTE_COLD
# endif
#endif

/* _GL_ATTRIBUTE_CONST declares:
   It is OK for a compiler to move a call, or omit a duplicate call
   and reuse a cached return value, even if the state changes between calls.
   It is also OK to omit a call if the result is not used.
   This attribute is safe if the function does not change observable state,
   returns a value determined solely by its arguments' values
   without examining state, and always returns exactly once -
   e.g., does not raise an exception, call longjmp, or loop forever.
   (This attribute is stricter than _GL_ATTRIBUTE_PURE because the
   function cannot observe state.  Unlike _GL_ATTRIBUTE_UNSEQUENCED
   the function must return exactly once and cannot access state
   addressed by its pointer arguments or that happens to have the same
   value for all calls to the function, but the function is allowed to
   return a pointer to storage that can be modified later.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_CONST
# if _GL_HAS_ATTRIBUTE (const)
#  define _GL_ATTRIBUTE_CONST __attribute__ ((__const__))
# else
#  define _GL_ATTRIBUTE_CONST
# endif
#endif

/* _GL_ATTRIBUTE_COUNTED_BY (C) declares that the number of elements of
   the field is given by C, which must be another field in the same struct.
   The programmer is responsible for guaranteeing some invariants; see
   <https://gcc.gnu.org/onlinedocs/gcc/Common-Attributes.html> for details.  */
/* Applies to struct fields of type array or pointer (to data).  */
#ifndef _GL_ATTRIBUTE_COUNTED_BY
/* This attributes is supported
     - for fields of array type: by gcc >= 16, clang >= 18,
     - for fields of pointer type: by gcc when <https://gcc.gnu.org/PR125072>
       will be fixed, clang >= 19.  */
# if defined __clang__ && __clang_major__ >= 19
#  define _GL_ATTRIBUTE_COUNTED_BY(c) __attribute__ ((__counted_by__ (c)))
# else
#  define _GL_ATTRIBUTE_COUNTED_BY(c)
# endif
#endif

/* _GL_ATTRIBUTE_DEALLOC (F, I) declares that the function returns pointers
   that can be freed by passing them as the Ith argument to the
   function F.
   _GL_ATTRIBUTE_DEALLOC_FREE declares that the function returns pointers that
   can be freed via 'free'; it can be used only after declaring 'free'.  */
/* Applies to: functions.  Cannot be used on inline functions.  */
#ifndef _GL_ATTRIBUTE_DEALLOC
# if _GL_GNUC_PREREQ (11, 0)
#  define _GL_ATTRIBUTE_DEALLOC(f, i) __attribute__ ((__malloc__ (f, i)))
# else
#  define _GL_ATTRIBUTE_DEALLOC(f, i)
# endif
#endif
/* If gnulib's <string.h> or <wchar.h> has already defined this macro, continue
   to use this earlier definition, since <stdlib.h> may not have been included
   yet.  */
#ifndef _GL_ATTRIBUTE_DEALLOC_FREE
# if defined __cplusplus && defined __GNUC__ && !defined __clang__
/* Work around GCC bug <https://gcc.gnu.org/PR108231> */
#  define _GL_ATTRIBUTE_DEALLOC_FREE \
     _GL_ATTRIBUTE_DEALLOC ((void (*) (void *)) free, 1)
# else
#  define _GL_ATTRIBUTE_DEALLOC_FREE \
     _GL_ATTRIBUTE_DEALLOC (free, 1)
# endif
#endif

/* _GL_ATTRIBUTE_DEPRECATED: Declares that an entity is deprecated.
   The compiler may warn if the entity is used.  */
/* Applies to:
     - function, variable,
     - struct, union, struct/union member,
     - enumeration, enumeration item,
     - typedef,
   in C++ also: namespace, class, template specialization.  */
#ifndef _GL_ATTRIBUTE_DEPRECATED
# ifndef _GL_BRACKET_BEFORE_ATTRIBUTE
#  if _GL_HAVE___HAS_C_ATTRIBUTE
#   if __has_c_attribute (__deprecated__)
#    define _GL_ATTRIBUTE_DEPRECATED [[__deprecated__]]
#   endif
#  endif
# endif
# if !defined _GL_ATTRIBUTE_DEPRECATED && _GL_HAS_ATTRIBUTE (deprecated)
#  define _GL_ATTRIBUTE_DEPRECATED __attribute__ ((__deprecated__))
# endif
# ifndef _GL_ATTRIBUTE_DEPRECATED
#  define _GL_ATTRIBUTE_DEPRECATED
# endif
#endif

/* _GL_ATTRIBUTE_ERROR(msg) requests an error if a function is called and
   the function call is not optimized away.
   _GL_ATTRIBUTE_WARNING(msg) requests a warning if a function is called and
   the function call is not optimized away.  */
/* Applies to: functions.  */
#if !(defined _GL_ATTRIBUTE_ERROR && defined _GL_ATTRIBUTE_WARNING)
# if _GL_HAS_ATTRIBUTE (error)
#  define _GL_ATTRIBUTE_ERROR(msg) __attribute__ ((__error__ (msg)))
#  define _GL_ATTRIBUTE_WARNING(msg) __attribute__ ((__warning__ (msg)))
# elif _GL_HAS_ATTRIBUTE (diagnose_if)
#  define _GL_ATTRIBUTE_ERROR(msg) __attribute__ ((__diagnose_if__ (1, msg, "error")))
#  define _GL_ATTRIBUTE_WARNING(msg) __attribute__ ((__diagnose_if__ (1, msg, "warning")))
# else
#  define _GL_ATTRIBUTE_ERROR(msg)
#  define _GL_ATTRIBUTE_WARNING(msg)
# endif
#endif

/* _GL_ATTRIBUTE_EXTERNALLY_VISIBLE declares that the entity should remain
   visible to debuggers etc., even with '-fwhole-program'.  */
/* Applies to: functions, variables.  */
#ifndef _GL_ATTRIBUTE_EXTERNALLY_VISIBLE
# if _GL_HAS_ATTRIBUTE (externally_visible)
#  define _GL_ATTRIBUTE_EXTERNALLY_VISIBLE __attribute__ ((externally_visible))
# else
#  define _GL_ATTRIBUTE_EXTERNALLY_VISIBLE
# endif
#endif

/* _GL_ATTRIBUTE_FALLTHROUGH declares that it is not a programming mistake if
   the control flow falls through to the immediately following 'case' or
   'default' label.  The compiler should not warn in this case.  */
/* Applies to: Empty statement (;), inside a 'switch' statement.  */
/* Always expands to something.  */
#ifndef _GL_ATTRIBUTE_FALLTHROUGH
# if _GL_HAVE___HAS_C_ATTRIBUTE
#  if __has_c_attribute (__fallthrough__)
#   define _GL_ATTRIBUTE_FALLTHROUGH [[__fallthrough__]]
#  endif
# endif
# if !defined _GL_ATTRIBUTE_FALLTHROUGH && _GL_HAS_ATTRIBUTE (fallthrough)
#  define _GL_ATTRIBUTE_FALLTHROUGH __attribute__ ((__fallthrough__))
# endif
# ifndef _GL_ATTRIBUTE_FALLTHROUGH
#  define _GL_ATTRIBUTE_FALLTHROUGH ((void) 0)
# endif
#endif

/* _GL_ATTRIBUTE_FORMAT ((ARCHETYPE, STRING-INDEX, FIRST-TO-CHECK))
   declares that the STRING-INDEXth function argument is a format string of
   style ARCHETYPE, which is one of:
     printf, gnu_printf
     scanf, gnu_scanf,
     strftime, gnu_strftime,
     strfmon,
   or the same thing prefixed and suffixed with '__'.
   If FIRST-TO-CHECK is not 0, arguments starting at FIRST-TO_CHECK
   are suitable for the format string.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_FORMAT
# if _GL_HAS_ATTRIBUTE (format)
#  define _GL_ATTRIBUTE_FORMAT(spec) __attribute__ ((__format__ spec))
# else
#  define _GL_ATTRIBUTE_FORMAT(spec)
# endif
#endif

/* _GL_ATTRIBUTE_LEAF declares that if the function is called from some other
   compilation unit, it executes code from that unit only by return or by
   exception handling.  This declaration lets the compiler optimize that unit
   more aggressively.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_LEAF
# if _GL_HAS_ATTRIBUTE (leaf)
#  define _GL_ATTRIBUTE_LEAF __attribute__ ((__leaf__))
# else
#  define _GL_ATTRIBUTE_LEAF
# endif
#endif

/* _GL_ATTRIBUTE_MALLOC declares that the function returns a pointer to freshly
   allocated memory.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_MALLOC
# if _GL_HAS_ATTRIBUTE (malloc)
#  define _GL_ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
# else
#  define _GL_ATTRIBUTE_MALLOC
# endif
#endif

/* _GL_ATTRIBUTE_MAY_ALIAS declares that pointers to the type may point to the
   same storage as pointers to other types.  Thus this declaration disables
   strict aliasing optimization.  */
/* Applies to: types.  */
/* Oracle Studio 12.6 mishandles may_alias despite __has_attribute OK.  */
#ifndef _GL_ATTRIBUTE_MAY_ALIAS
# if _GL_HAS_ATTRIBUTE (may_alias) && !defined __SUNPRO_C
#  define _GL_ATTRIBUTE_MAY_ALIAS __attribute__ ((__may_alias__))
# else
#  define _GL_ATTRIBUTE_MAY_ALIAS
# endif
#endif

/* _GL_ATTRIBUTE_MAYBE_UNUSED declares that it is not a programming mistake if
   the entity is not used.  The compiler should not warn if the entity is not
   used.  However, 'int _GL_UNNAMED (i)' is preferable to
   '_GL_ATTRIBUTE_MAYBE_UNUSED int i' when parameter I is unused
   regardless of preprocessor macro settings.  */
/* Applies to:
     - function, variable,
     - struct, union, struct/union member,
     - enumeration, enumeration item,
     - typedef,
   in C++ also: class.  */
/* In C++ and C23, this is spelled [[__maybe_unused__]].
   GCC's syntax is __attribute__ ((__unused__)).
   clang supports both syntaxes.  Except that with clang ≥ 6, < 10, in C++ mode,
   __has_c_attribute (__maybe_unused__) yields true but the use of
   [[__maybe_unused__]] nevertheless produces a warning.  */
#ifndef _GL_ATTRIBUTE_MAYBE_UNUSED
# ifndef _GL_BRACKET_BEFORE_ATTRIBUTE
#  if defined __clang__ && defined __cplusplus
#   if !defined __apple_build_version__ && __clang_major__ >= 10
#    define _GL_ATTRIBUTE_MAYBE_UNUSED [[__maybe_unused__]]
#   endif
#  elif _GL_HAVE___HAS_C_ATTRIBUTE
#   if __has_c_attribute (__maybe_unused__)
#    define _GL_ATTRIBUTE_MAYBE_UNUSED [[__maybe_unused__]]
#   endif
#  endif
# endif
# ifndef _GL_ATTRIBUTE_MAYBE_UNUSED
#  define _GL_ATTRIBUTE_MAYBE_UNUSED _GL_ATTRIBUTE_UNUSED
# endif
#endif
/* Alternative spelling of this macro, for convenience and for
   compatibility with glibc/include/libc-symbols.h.  */
#define _GL_UNUSED _GL_ATTRIBUTE_MAYBE_UNUSED
/* Earlier spellings of this macro.  */
#define _UNUSED_PARAMETER_ _GL_ATTRIBUTE_MAYBE_UNUSED

/* _GL_ATTRIBUTE_NODISCARD declares that the caller of the function should not
   discard the return value.  The compiler may warn if the caller does not use
   the return value, unless the caller uses something like ignore_value.  */
/* Applies to: function, enumeration, class.  */
#ifndef _GL_ATTRIBUTE_NODISCARD
# ifndef _GL_BRACKET_BEFORE_ATTRIBUTE
#  if defined __clang__ && defined __cplusplus
  /* With clang up to 15.0.6 (at least), in C++ mode, [[__nodiscard__]] produces
     a warning.
     The 1000 below means a yet unknown threshold.  When clang++ version X
     starts supporting [[__nodiscard__]] without warning about it, you can
     replace the 1000 with X.  */
#   if __clang_major__ >= 1000
#    define _GL_ATTRIBUTE_NODISCARD [[__nodiscard__]]
#   endif
#  elif _GL_HAVE___HAS_C_ATTRIBUTE
#   if __has_c_attribute (__nodiscard__)
#    define _GL_ATTRIBUTE_NODISCARD [[__nodiscard__]]
#   endif
#  endif
# endif
# if !defined _GL_ATTRIBUTE_NODISCARD && _GL_HAS_ATTRIBUTE (warn_unused_result)
#  define _GL_ATTRIBUTE_NODISCARD __attribute__ ((__warn_unused_result__))
# endif
# ifndef _GL_ATTRIBUTE_NODISCARD
#  define _GL_ATTRIBUTE_NODISCARD
# endif
#endif

/* _GL_ATTRIBUTE_NOINLINE tells that the compiler should not inline the
   function.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_NOINLINE
# if _GL_HAS_ATTRIBUTE (noinline)
#  define _GL_ATTRIBUTE_NOINLINE __attribute__ ((__noinline__))
# else
#  define _GL_ATTRIBUTE_NOINLINE
# endif
#endif

/* _GL_ATTRIBUTE_NONNULL ((N1, N2,...)) declares that the arguments N1, N2,...
   must not be NULL.
   _GL_ATTRIBUTE_NONNULL () declares that all pointer arguments must not be
   null.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_NONNULL
# if _GL_HAS_ATTRIBUTE (nonnull)
#  define _GL_ATTRIBUTE_NONNULL(args) __attribute__ ((__nonnull__ args))
# else
#  define _GL_ATTRIBUTE_NONNULL(args)
# endif
#endif

/* _GL_ATTRIBUTE_NONNULL_IF_NONZERO (NP, NI) declares that the argument NP
   (a pointer) must not be NULL if the argument NI (an integer) is != 0.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_NONNULL_IF_NONZERO
# if _GL_HAS_ATTRIBUTE (nonnull_if_nonzero)
#  define _GL_ATTRIBUTE_NONNULL_IF_NONZERO(np, ni) __attribute__ ((__nonnull_if_nonzero__ (np, ni)))
# else
#  define _GL_ATTRIBUTE_NONNULL_IF_NONZERO(np, ni)
# endif
#endif

/* _GL_ATTRIBUTE_NONSTRING declares that the contents of a character array is
   not meant to be NUL-terminated.  */
/* Applies to: struct/union members and variables that are arrays of element
   type '[[un]signed] char'.  */
#ifndef _GL_ATTRIBUTE_NONSTRING
# if _GL_HAS_ATTRIBUTE (nonstring)
#  define _GL_ATTRIBUTE_NONSTRING __attribute__ ((__nonstring__))
# else
#  define _GL_ATTRIBUTE_NONSTRING
# endif
#endif

/* There is no _GL_ATTRIBUTE_NORETURN; use _Noreturn instead.  */

/* _GL_ATTRIBUTE_NOTHROW declares that the function does not throw exceptions.
 */
/* Applies to: functions.  */
/* After a function's parameter list, this attribute must come first, before
   other attributes.  */
#ifndef _GL_ATTRIBUTE_NOTHROW
# if defined __cplusplus
#  if _GL_GNUC_PREREQ (2, 8) || __clang_major__ >= 4
#   if __cplusplus >= 201103L
#    define _GL_ATTRIBUTE_NOTHROW noexcept (true)
#   else
#    define _GL_ATTRIBUTE_NOTHROW throw ()
#   endif
#  else
#   define _GL_ATTRIBUTE_NOTHROW
#  endif
# else
#  if _GL_HAS_ATTRIBUTE (nothrow)
#   define _GL_ATTRIBUTE_NOTHROW __attribute__ ((__nothrow__))
#  else
#   define _GL_ATTRIBUTE_NOTHROW
#  endif
# endif
#endif

/* _GL_ATTRIBUTE_PACKED declares:
   For struct members: The member has the smallest possible alignment.
   For struct, union, class: All members have the smallest possible alignment,
   minimizing the memory required.  */
/* Applies to: struct members, struct, union,
   in C++ also: class.  */
#ifndef _GL_ATTRIBUTE_PACKED
/* Oracle Studio 12.6 miscompiles code with __attribute__ ((__packed__)) despite
   __has_attribute OK.  */
# if _GL_HAS_ATTRIBUTE (packed) && !defined __SUNPRO_C
#  define _GL_ATTRIBUTE_PACKED __attribute__ ((__packed__))
# else
#  define _GL_ATTRIBUTE_PACKED
# endif
#endif

/* _GL_ATTRIBUTE_PURE declares:
   It is OK for a compiler to move a call, or omit a duplicate call
   and reuse a cached return value, if observable state is the same.
   It is also OK to omit a call if the return value is not used.
   This attribute is safe if the function does not change observable state,
   returns a value determined solely by its arguments's values
   together with observable state, and always returns exactly once.
   (This attribute is looser than _GL_ATTRIBUTE_CONST because the function
   can depend on observable state.
   Unlike _GL_ATTRIBUTE_REPRODUCIBLE the function must return exactly
   once and cannot change state addressed by its arguments, but the
   function can return a pointer to storage whose contents change later.)  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_PURE
# if _GL_HAS_ATTRIBUTE (pure)
#  define _GL_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define _GL_ATTRIBUTE_PURE
# endif
#endif

/* _GL_ATTRIBUTE_REPRODUCIBLE declares:
   It is OK for a compiler to move a call, or omit a duplicate call
   and reuse a cached value returned either directly or indirectly via
   a pointer, if other observable state is the same;
   however, pointer arguments cannot alias.
   This attribute is safe for a function that is effectless and idempotent;
   see ISO C 23 § 6.7.13.8 for a definition of these terms.
   (This attribute is looser than _GL_ATTRIBUTE_UNSEQUENCED because
   the function need not be stateless or independent.
   Unlike _GL_ATTRIBUTE_PURE the function need not return exactly once
   and can change state addressed by its pointer arguments, but the
   function cannot return a pointer to storage whose contents change later.)
   See also <https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2956.htm> and
   <https://stackoverflow.com/questions/76847905/>.  */
/* Applies to: functions, pointer to functions, function types.  */
#ifndef _GL_ATTRIBUTE_REPRODUCIBLE
/* This may be revisited when gcc and clang support [[reproducible]] or possibly
   __attribute__ ((__reproducible__)).  */
# ifndef _GL_BRACKET_BEFORE_ATTRIBUTE
#  if _GL_HAS_ATTRIBUTE (reproducible)
#   define _GL_ATTRIBUTE_REPRODUCIBLE [[reproducible]]
#  endif
# endif
# ifndef _GL_ATTRIBUTE_REPRODUCIBLE
#  define _GL_ATTRIBUTE_REPRODUCIBLE
# endif
#endif

/* _GL_ATTRIBUTE_RETURNS_NONNULL declares that the function's return value is
   a non-NULL pointer.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_RETURNS_NONNULL
# if _GL_HAS_ATTRIBUTE (returns_nonnull)
#  define _GL_ATTRIBUTE_RETURNS_NONNULL __attribute__ ((__returns_nonnull__))
# else
#  define _GL_ATTRIBUTE_RETURNS_NONNULL
# endif
#endif

/* _GL_ATTRIBUTE_SENTINEL(pos) declares that the variadic function expects a
   trailing NULL argument.
   _GL_ATTRIBUTE_SENTINEL () - The last argument is NULL (requires C99).
   _GL_ATTRIBUTE_SENTINEL ((N)) - The (N+1)st argument from the end is NULL.  */
/* Applies to: functions.  */
#ifndef _GL_ATTRIBUTE_SENTINEL
# if _GL_HAS_ATTRIBUTE (sentinel)
#  define _GL_ATTRIBUTE_SENTINEL(pos) __attribute__ ((__sentinel__ pos))
# else
#  define _GL_ATTRIBUTE_SENTINEL(pos)
# endif
#endif

/* _GL_ATTRIBUTE_UNSEQUENCED declares:
   It is OK for a compiler to move a call, or omit a duplicate call
   and reuse a cached value returned either directly or indirectly via
   a pointer, if the state addressed by its pointer arguments is the same;
   however, pointer arguments cannot alias.
   This attribute is safe for a function that is effectless, idempotent,
   stateless, and independent; see ISO C 23 § 6.7.13.8 for a definition of
   these terms.
   (This attribute is stricter than _GL_ATTRIBUTE_REPRODUCIBLE because
   the function must be stateless and independent.  Unlike
   _GL_ATTRIBUTE_CONST the function need not return exactly once, and
   can depend on state accessed via its pointer arguments or that
   happens to have the same value for all calls to the function, but
   the function cannot return a pointer to storage whose contents
   change later.)
   See also <https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2956.htm> and
   <https://stackoverflow.com/questions/76847905/>.  */
/* Applies to: functions, pointer to functions, function types.  */
#ifndef _GL_ATTRIBUTE_UNSEQUENCED
/* This may be revisited when gcc and clang support [[unsequenced]] or possibly
   __attribute__ ((__unsequenced__)).  */
# ifndef _GL_BRACKET_BEFORE_ATTRIBUTE
#  if _GL_HAS_ATTRIBUTE (unsequenced)
#   define _GL_ATTRIBUTE_UNSEQUENCED [[unsequenced]]
#  endif
# endif
# ifndef _GL_ATTRIBUTE_UNSEQUENCED
#  define _GL_ATTRIBUTE_UNSEQUENCED
# endif
#endif

/* A helper macro.  Don't use it directly.  */
#ifndef _GL_ATTRIBUTE_UNUSED
# if _GL_HAS_ATTRIBUTE (unused)
#  define _GL_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define _GL_ATTRIBUTE_UNUSED
# endif
#endif

]dnl There is no _GL_ATTRIBUTE_VISIBILITY; see m4/visibility.m4 instead.
[
/* _GL_UNUSED_LABEL; declares that it is not a programming mistake if the
   immediately preceding label is not used.  The compiler should not warn
   if the label is not used.  */
/* Applies to: label (both in C and C++).  */
/* Note that g++ < 4.5 does not support the '__attribute__ ((__unused__)) ;'
   syntax.  But clang does.  */
#ifndef _GL_UNUSED_LABEL
# if !(defined __cplusplus && !_GL_GNUC_PREREQ (4, 5)) || defined __clang__
#  define _GL_UNUSED_LABEL _GL_ATTRIBUTE_UNUSED
# else
#  define _GL_UNUSED_LABEL
# endif
#endif

/* _GL_UNNAMED (ID) is the "name" of an unnamed function parameter.
   Each of the function's unnamed parameters should have a unique "name".
   The "name" cannot be used.  This ports both to C17 and earlier, which
   lack unnamed parameters, and to C++ and later C, which have them.  */
/* Applies to:
     - function parameters.  */
#ifndef _GL_UNNAMED
# if ((defined __STDC_VERSION__ ? __STDC_VERSION__ : 0) < 202311 \
      && !defined __cplusplus)
#  define _GL_UNNAMED(id) unnamed_##id _GL_ATTRIBUTE_UNUSED
# else
#  define _GL_UNNAMED(id)
# endif
#endif

/* The following attributes enable detection of thread safety problems
   and resource leaks at compile-time, by clang ≥ 15, when the warning option
   -Wthread-safety is enabled.  For usage, see
   <https://clang.llvm.org/docs/ThreadSafetyAnalysis.html>.  */
#ifndef _GL_ATTRIBUTE_CAPABILITY_TYPE
# if __clang_major__ >= 15
#  define _GL_ATTRIBUTE_CAPABILITY_TYPE(concept) \
     __attribute__ ((__capability__ (concept)))
# else
#  define _GL_ATTRIBUTE_CAPABILITY_TYPE(concept)
# endif
#endif
#ifndef _GL_ATTRIBUTE_ACQUIRE_CAPABILITY
# if __clang_major__ >= 15
#  define _GL_ATTRIBUTE_ACQUIRE_CAPABILITY(resource) \
     __attribute__ ((__acquire_capability__ (resource)))
# else
#  define _GL_ATTRIBUTE_ACQUIRE_CAPABILITY(resource)
# endif
#endif
#ifndef _GL_ATTRIBUTE_RELEASE_CAPABILITY
# if __clang_major__ >= 15
#  define _GL_ATTRIBUTE_RELEASE_CAPABILITY(resource) \
     __attribute__ ((__release_capability__ (resource)))
# else
#  define _GL_ATTRIBUTE_RELEASE_CAPABILITY(resource)
# endif
#endif
])
  AH_VERBATIM([c_linkage],
[/* In C++, there is the concept of "language linkage", that encompasses
    name mangling and function calling conventions.
    The following macros start and end a block of "C" linkage.  */
#ifdef __cplusplus
# define _GL_BEGIN_C_LINKAGE extern "C" {
# define _GL_END_C_LINKAGE }
#else
# define _GL_BEGIN_C_LINKAGE
# define _GL_END_C_LINKAGE
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
   This variant     (n1 < n2 ? -1 : n1 > n2)  produces a conditional jump with
   many GCC versions up to GCC 9.
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
    [AS_HELP_STRING([[--enable-cross-guesses={conservative|risky}]],
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
# This definition is copied from post-2.73 Autoconf and overrides the
# AC_C_RESTRICT macro from autoconf 2.60..2.73.
m4_version_prereq([2.73.1], [], [
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
   nothing if this is not supported.  In particular it is not supported
   in MSVC 14.44 and in g++ 7 on Solaris 11, although these compilers
   define __STDC_VERSION__ to 199901L.
   Do not define if restrict is supported directly.  */
#if ! (defined __STDC_VERSION__ && 199901L <= __STDC_VERSION__ \
       && !defined _MSC_VER && !defined __cplusplus)
#undef restrict
#endif
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

# gl_CONDITIONAL(conditional, condition)
# is like AM_CONDITIONAL(conditional, condition), except that it does not
# produce an error
#   configure: error: conditional "..." was never defined.
#   Usually this means the macro was only invoked conditionally.
# when only invoked conditionally. Instead, in that case, both the _TRUE
# and the _FALSE case are disabled.
AC_DEFUN([gl_CONDITIONAL],
[
  pushdef([AC_CONFIG_COMMANDS_PRE], [:])dnl
  AM_CONDITIONAL([$1], [$2])
  popdef([AC_CONFIG_COMMANDS_PRE])dnl
  if test -z "${[$1]_TRUE}" && test -z "${[$1]_FALSE}"; then
    [$1]_TRUE='#'
    [$1]_FALSE='#'
  fi
])

# gl_CC_ALLOW_WARNINGS
# sets and substitutes a variable GL_CFLAG_ALLOW_WARNINGS, to a $(CC) option
# that reverts a preceding '-Werror' option, if available.
# This is expected to be '-Wno-error' on gcc, clang (except clang/MSVC), xlclang
# and empty otherwise.
AC_DEFUN([gl_CC_ALLOW_WARNINGS],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_CACHE_CHECK([for C compiler option to allow warnings],
    [gl_cv_cc_wallow],
    [rm -fr conftest*
     echo 'int dummy;' > conftest.c
     AC_TRY_COMMAND([${CC-cc} $CFLAGS $CPPFLAGS -c conftest.c 2>conftest1.err]) >/dev/null
     AC_TRY_COMMAND([${CC-cc} $CFLAGS $CPPFLAGS -Wno-error -c conftest.c 2>conftest2.err]) >/dev/null
     dnl Test the number of error output lines, because AIX xlc accepts the
     dnl option '-Wno-error', just to produce a warning
     dnl "Option -Wno-error was incorrectly specified. The option will be ignored."
     dnl afterwards.
     if test $? = 0 && test `wc -l < conftest1.err` = `wc -l < conftest2.err`; then
       gl_cv_cc_wallow='-Wno-error'
     else
       gl_cv_cc_wallow=none
     fi
     rm -fr conftest*
    ])
  case "$gl_cv_cc_wallow" in
    none) GL_CFLAG_ALLOW_WARNINGS='' ;;
    *)    GL_CFLAG_ALLOW_WARNINGS="$gl_cv_cc_wallow" ;;
  esac
  AC_SUBST([GL_CFLAG_ALLOW_WARNINGS])
])

# gl_CXX_ALLOW_WARNINGS
# sets and substitutes a variable GL_CXXFLAG_ALLOW_WARNINGS, to a $(CC) option
# that reverts a preceding '-Werror' option, if available.
AC_DEFUN([gl_CXX_ALLOW_WARNINGS],
[
  dnl Requires AC_PROG_CXX or gl_PROG_ANSI_CXX.
  if test -n "$CXX" && test "$CXX" != no; then
    AC_CACHE_CHECK([for C++ compiler option to allow warnings],
      [gl_cv_cxx_wallow],
      [rm -fr conftest*
       echo 'int dummy;' > conftest.cc
       AC_TRY_COMMAND([${CXX-c++} $CXXFLAGS $CPPFLAGS -c conftest.cc 2>conftest1.err]) >/dev/null
       AC_TRY_COMMAND([${CXX-c++} $CXXFLAGS $CPPFLAGS -Wno-error -c conftest.cc 2>conftest2.err]) >/dev/null
       dnl Test the number of error output lines, because AIX xlC accepts the
       dnl option '-Wno-error', just to produce a warning
       dnl "Option -Wno-error was incorrectly specified. The option will be ignored."
       dnl afterwards.
       if test $? = 0 && test `wc -l < conftest1.err` = `wc -l < conftest2.err`; then
         gl_cv_cxx_wallow='-Wno-error'
       else
         gl_cv_cxx_wallow=none
       fi
       rm -fr conftest*
      ])
    case "$gl_cv_cxx_wallow" in
      none) GL_CXXFLAG_ALLOW_WARNINGS='' ;;
      *)    GL_CXXFLAG_ALLOW_WARNINGS="$gl_cv_cxx_wallow" ;;
    esac
  else
    GL_CXXFLAG_ALLOW_WARNINGS=''
  fi
  AC_SUBST([GL_CXXFLAG_ALLOW_WARNINGS])
])

# gl_CC_GNULIB_WARNINGS
# sets and substitutes a variable GL_CFLAG_GNULIB_WARNINGS, to a $(CC) option
# set that enables or disables warnings as suitable for the Gnulib coding style.
AC_DEFUN([gl_CC_GNULIB_WARNINGS],
[
  AC_REQUIRE([gl_CC_ALLOW_WARNINGS])
  dnl Assume that the compiler supports -Wno-* options only if it also supports
  dnl -Wno-error.
  GL_CFLAG_GNULIB_WARNINGS=''
  if test -n "$GL_CFLAG_ALLOW_WARNINGS"; then
    dnl Enable these warning options:
    dnl
    dnl                                       GCC             clang
    dnl -Wno-cast-qual                        >= 3            >= 3.9
    dnl -Wno-conversion                       >= 3            >= 3.9
    dnl -Wno-float-conversion                 >= 4.9          >= 3.9
    dnl -Wno-float-equal                      >= 3            >= 3.9
    dnl -Wimplicit-fallthrough                >= 7            >= 3.9
    dnl -Wno-pedantic                         >= 4.8          >= 3.9
    dnl -Wno-sign-compare                     >= 3            >= 3.9
    dnl -Wno-sign-conversion                  >= 4.3          >= 3.9
    dnl -Wno-string-plus-int                  -               >= 3.9
    dnl -Wno-tautological-out-of-range-compare  -             >= 3.9
    dnl -Wno-type-limits                      >= 4.3          >= 3.9
    dnl -Wno-undef                            >= 3            >= 3.9
    dnl -Wno-unsuffixed-float-constants       >= 4.5
    dnl -Wno-unused-const-variable            >= 6.1          >= 3.9
    dnl -Wno-unused-function                  >= 3            >= 3.9
    dnl -Wno-unused-parameter                 >= 3            >= 3.9
    dnl
    cat > conftest.c <<\EOF
      #if (__GNUC__ >= 3 && !defined __clang__) || (__clang_major__ + (__clang_minor__ >= 9) > 3)
      -Wno-cast-qual
      -Wno-conversion
      -Wno-float-equal
      -Wno-sign-compare
      -Wno-undef
      -Wno-unused-function
      -Wno-unused-parameter
      #endif
      #if (__GNUC__ + (__GNUC_MINOR__ >= 9) > 4 && !defined __clang__) || (__clang_major__ + (__clang_minor__ >= 9) > 3)
      -Wno-float-conversion
      #endif
      #if (__GNUC__ >= 7 && !defined __clang__) || (__clang_major__ + (__clang_minor__ >= 9) > 3)
      -Wimplicit-fallthrough
      #endif
      #if (__GNUC__ + (__GNUC_MINOR__ >= 8) > 4 && !defined __clang__) || (__clang_major__ + (__clang_minor__ >= 9) > 3)
      -Wno-pedantic
      #endif
      #if 3 < __clang_major__ + (9 <= __clang_minor__)
      -Wno-string-plus-int
      -Wno-tautological-constant-out-of-range-compare
      #endif
      #if (__GNUC__ + (__GNUC_MINOR__ >= 3) > 4 && !defined __clang__) || (__clang_major__ + (__clang_minor__ >= 9) > 3)
      -Wno-sign-conversion
      -Wno-type-limits
      #endif
      #if (__GNUC__ + (__GNUC_MINOR__ >= 1) > 6 && !defined __clang__) || (__clang_major__ + (__clang_minor__ >= 9) > 3)
      -Wno-unused-const-variable
      #endif
      #if (__GNUC__ + (__GNUC_MINOR__ >= 5) > 4 && !defined __clang__)
      -Wno-unsuffixed-float-constants
      #endif
EOF
    gl_command="$CC $CFLAGS $CPPFLAGS -E conftest.c > conftest.out"
    if AC_TRY_EVAL([gl_command]); then
      gl_options=`grep -v '#' conftest.out`
      for word in $gl_options; do
        GL_CFLAG_GNULIB_WARNINGS="$GL_CFLAG_GNULIB_WARNINGS $word"
      done
    fi
    rm -f conftest.c conftest.out
  fi
  AC_SUBST([GL_CFLAG_GNULIB_WARNINGS])
])

dnl gl_CONDITIONAL_HEADER([foo.h])
dnl takes a shell variable GL_GENERATE_FOO_H (with value true or false) as input
dnl and produces
dnl   - an AC_SUBSTed variable FOO_H that is either a file name or empty, based
dnl     on whether GL_GENERATE_FOO_H is true or false,
dnl   - an Automake conditional GL_GENERATE_FOO_H that evaluates to the value of
dnl     the shell variable GL_GENERATE_FOO_H.
AC_DEFUN([gl_CONDITIONAL_HEADER],
[
  m4_pushdef([gl_header_name], AS_TR_SH(m4_toupper($1)))
  m4_pushdef([gl_generate_var], [GL_GENERATE_]AS_TR_SH(m4_toupper($1)))
  m4_pushdef([gl_generate_cond], [GL_GENERATE_]AS_TR_SH(m4_toupper($1)))
  case "$gl_generate_var" in
    false) gl_header_name='' ;;
    true)
      dnl It is OK to use a .h file in lib/ from within tests/, but not vice
      dnl versa.
      if test -z "$gl_header_name"; then
        gl_header_name="${gl_source_base_prefix}$1"
      fi
      ;;
    *) echo "*** gl_generate_var is not set correctly" 1>&2; exit 1 ;;
  esac
  AC_SUBST(gl_header_name)
  gl_CONDITIONAL(gl_generate_cond, [$gl_generate_var])
  m4_popdef([gl_generate_cond])
  m4_popdef([gl_generate_var])
  m4_popdef([gl_header_name])
])

dnl Preparations for gl_CHECK_FUNCS_MACOS.
AC_DEFUN([gl_PREPARE_CHECK_FUNCS_MACOS],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_COMPILER_CLANG])
  AC_CACHE_CHECK([for compiler option needed when checking for future declarations],
    [gl_cv_compiler_check_future_option],
    [case "$host_os" in
       dnl This is only needed on macOS.
       darwin*)
         if test $gl_cv_compiler_clang = yes; then
           dnl Test whether the compiler supports the option
           dnl '-Werror=unguarded-availability-new'.
           saved_ac_compile="$ac_compile"
           ac_compile="$ac_compile -Werror=unguarded-availability-new"
           AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],[[]])],
             [gl_cv_compiler_check_future_option='-Werror=unguarded-availability-new'],
             [gl_cv_compiler_check_future_option=none])
           ac_compile="$saved_ac_compile"
         else
           gl_cv_compiler_check_future_option=none
         fi
         ;;
       *) gl_cv_compiler_check_future_option=none ;;
     esac
    ])
])

dnl Pieces of the expansion of
dnl gl_CHECK_FUNCS_ANDROID
dnl gl_CHECK_FUNCS_MACOS
dnl gl_CHECK_FUNCS_ANDROID_MACOS

AC_DEFUN([gl_CHECK_FUNCS_DEFAULT_CASE],
[
         *)
           AC_CHECK_FUNC([$1])
           [gl_cv_onwards_func_][$1]=$[ac_cv_func_][$1]
           ;;
])

AC_DEFUN([gl_CHECK_FUNCS_CASE_FOR_ANDROID],
[
         linux*-android*)
           AC_CHECK_DECL([$1], , , [$2])
           if test $[ac_cv_have_decl_][$1] = yes; then
             AC_CHECK_FUNC([[$1]])
             if test $[ac_cv_func_][$1] = yes; then
               [gl_cv_onwards_func_][$1]=yes
             else
               dnl The function is declared but does not exist. This should not
               dnl happen normally. But anyway, we know that a future version
               dnl of Android will have the function.
               [gl_cv_onwards_func_][$1]='future OS version'
             fi
           else
             [gl_cv_onwards_func_][$1]='future OS version'
           fi
           ;;
])

AC_DEFUN([gl_CHECK_FUNCS_CASE_FOR_MACOS],
[
         darwin*)
           if test "x$gl_cv_compiler_check_future_option" != "xnone"; then
             dnl Use a compile test, not a link test.
             saved_ac_compile="$ac_compile"
             ac_compile="$ac_compile $gl_cv_compiler_check_future_option"
             saved_ac_compile_for_check_decl="$ac_compile_for_check_decl"
             ac_compile_for_check_decl="$ac_compile_for_check_decl $gl_cv_compiler_check_future_option"
             unset [ac_cv_have_decl_][$1]
             AC_CHECK_DECL([$1], , , [$2])
             ac_compile="$saved_ac_compile"
             ac_compile_for_check_decl="$saved_ac_compile_for_check_decl"
             [ac_cv_func_][$1]="$[ac_cv_have_decl_][$1]"
             if test $[ac_cv_func_][$1] = yes; then
               [gl_cv_onwards_func_][$1]=yes
             else
               dnl This is a bit complicated, because here we need the behaviour
               dnl of AC_CHECK_DECL before the
               dnl commit e1bbc9b93cdff61d70719c224b37970e065008bb (2025-05-26).
               [ac_cv_have_decl_][$1][_saved]="$[ac_cv_have_decl_][$1]"
               unset [ac_cv_have_decl_][$1]
               ac_c_future_darwin_options_saved="$ac_c_future_darwin_options"
               ac_cxx_future_darwin_options_saved="$ac_cxx_future_darwin_options"
               ac_c_future_darwin_options=
               ac_cxx_future_darwin_options=
               AC_CHECK_DECL([$1], , , [$2])
               ac_c_future_darwin_options="$ac_c_future_darwin_options_saved"
               ac_cxx_future_darwin_options="$ac_cxx_future_darwin_options_saved"
               if test $[ac_cv_have_decl_][$1] = yes; then
                 [gl_cv_onwards_func_][$1]='future OS version'
               else
                 [gl_cv_onwards_func_][$1]=no
               fi
               [ac_cv_have_decl_][$1]="$[ac_cv_have_decl_][$1][_saved]"
               unset [ac_cv_have_decl_][$1][_saved]
             fi
           else
             AC_CHECK_FUNC([$1])
             [gl_cv_onwards_func_][$1]=$[ac_cv_func_][$1]
           fi
           ;;
])

AC_DEFUN([gl_CHECK_FUNCS_SET_RESULTS],
[
  case "$[gl_cv_onwards_func_][$1]" in
    future*) [ac_cv_func_][$1]=no ;;
    *)       [ac_cv_func_][$1]=$[gl_cv_onwards_func_][$1] ;;
  esac
  if test $[ac_cv_func_][$1] = yes; then
    AC_DEFINE([HAVE_]m4_translit([[$1]],
                                 [abcdefghijklmnopqrstuvwxyz],
                                 [ABCDEFGHIJKLMNOPQRSTUVWXYZ]),
              [1], [Define to 1 if you have the `$1' function.])
  fi
])

dnl gl_CHECK_FUNCS_ANDROID([func], [[#include <foo.h>]])
dnl is like AC_CHECK_FUNCS([func]), taking into account a portability problem
dnl on Android.
dnl
dnl When code is compiled on Android, it is in the context of a certain
dnl "Android API level", which indicates the minimum version of Android on
dnl which the app can be installed. In other words, you don't compile for a
dnl specific version of Android. You compile for all versions of Android,
dnl onwards from the given API level.
dnl Thus, the question "does the OS have the function func" has three possible
dnl answers:
dnl   - yes, in all versions starting from the given API level,
dnl   - no, in no version,
dnl   - not in the given API level, but in a later version of Android.
dnl
dnl In detail, this works as follows:
dnl If func was added to Android API level, say, 28, then the libc.so has the
dnl symbol func always, whereas the header file <foo.h> declares func
dnl conditionally:
dnl   #if __ANDROID_API__ >= 28
dnl   ... func (...) __INTRODUCED_IN(28);
dnl   #endif
dnl Thus, when compiling with "clang -target armv7a-unknown-linux-android28",
dnl the function func is declared and exists in libc.
dnl Whereas when compiling with "clang -target armv7a-unknown-linux-android27",
dnl the function func is not declared but exists in libc.
dnl
dnl This macro sets two variables:
dnl   - gl_cv_onwards_func_<func>   to yes / no / "future OS version"
dnl   - ac_cv_func_<func>           to yes / no / no
dnl The first variable allows distinguishing all three cases.
dnl The second variable is set, so that an invocation
dnl   gl_CHECK_FUNCS_ANDROID([func], [[#include <foo.h>]])
dnl can be used as a drop-in replacement for
dnl   AC_CHECK_FUNCS([func]).
AC_DEFUN([gl_CHECK_FUNCS_ANDROID],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_CACHE_CHECK([for [$1]],
    [[gl_cv_onwards_func_][$1]],
    [gl_SILENT([
       case "$host_os" in
         gl_CHECK_FUNCS_CASE_FOR_ANDROID([$1], [$2])
         gl_CHECK_FUNCS_DEFAULT_CASE([$1])
       esac
      ])
    ])
  gl_CHECK_FUNCS_SET_RESULTS([$1])
])

dnl gl_CHECK_FUNCS_MACOS([func], [[#include <foo.h>]])
dnl is like AC_CHECK_FUNCS([func]), taking into account a portability problem
dnl on macOS.
dnl
dnl When code is compiled on macOS, it is in the context of a certain minimum
dnl macOS version, that can be set through the option '-mmacosx-version-min='.
dnl In other words, you don't compile for a specific version of macOS. You
dnl compile for all versions of macOS, onwards from the given version.
dnl Thus, the question "does the OS have the function func" has three possible
dnl answers:
dnl   - yes, in all versions starting from the given version,
dnl   - no, in no version,
dnl   - not in the given version, but in a later version of macOS.
dnl
dnl In detail, this works as follows:
dnl If func was added to, say, macOS version 13, then the libc has the
dnl symbol func always, whereas the header file <foo.h> declares func
dnl conditionally with a special availability attribute:
dnl   ... func (...) __attribute__((availability(macos,introduced=13.0)));
dnl Thus, when compiling with "clang mmacosx-version-min=13", there is no
dnl warning about the use of func, and the resulting binary
dnl   - runs fine on macOS 13,
dnl   - aborts with a dyld "Symbol not found" message on macOS 12.
dnl Whereas, when compiling with "clang mmacosx-version-min=12", there is a
dnl   warning: 'func' is only available on macOS 13.0 or newer
dnl   [-Wunguarded-availability-new],
dnl and the resulting binary
dnl   - runs fine on macOS 13,
dnl   - crashes with a SIGSEGV (signal 11) on macOS 12.
dnl
dnl This macro sets two variables:
dnl   - gl_cv_onwards_func_<func>   to yes / no / "future OS version"
dnl   - ac_cv_func_<func>           to yes / no / no
dnl The first variable allows distinguishing all three cases.
dnl The second variable is set, so that an invocation
dnl   gl_CHECK_FUNCS_MACOS([func], [[#include <foo.h>]])
dnl can be used as a drop-in replacement for
dnl   AC_CHECK_FUNCS([func]).
AC_DEFUN([gl_CHECK_FUNCS_MACOS],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_PREPARE_CHECK_FUNCS_MACOS])
  AC_CACHE_CHECK([for [$1]],
    [[gl_cv_onwards_func_][$1]],
    [gl_SILENT([
       case "$host_os" in
         gl_CHECK_FUNCS_CASE_FOR_MACOS([$1], [$2])
         gl_CHECK_FUNCS_DEFAULT_CASE([$1])
       esac
      ])
    ])
  gl_CHECK_FUNCS_SET_RESULTS([$1])
])

dnl gl_CHECK_FUNCS_ANDROID_MACOS([func], [[#include <foo.h>]])
dnl is like AC_CHECK_FUNCS([func]), taking into account a portability problem
dnl on Android and on macOS.
dnl It is the combination of gl_CHECK_FUNCS_ANDROID and gl_CHECK_FUNCS_MACOS.
AC_DEFUN([gl_CHECK_FUNCS_ANDROID_MACOS],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_PREPARE_CHECK_FUNCS_MACOS])
  AC_CACHE_CHECK([for [$1]],
    [[gl_cv_onwards_func_][$1]],
    [gl_SILENT([
       case "$host_os" in
         gl_CHECK_FUNCS_CASE_FOR_ANDROID([$1], [$2])
         gl_CHECK_FUNCS_CASE_FOR_MACOS([$1], [$2])
         gl_CHECK_FUNCS_DEFAULT_CASE([$1])
       esac
      ])
    ])
  gl_CHECK_FUNCS_SET_RESULTS([$1])
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
