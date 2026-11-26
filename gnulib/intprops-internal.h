/* intprops-internal.h -- properties of integer types not visible to users

   Copyright (C) 2001-2026 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef _GL_INTPROPS_INTERNAL_H
#define _GL_INTPROPS_INTERNAL_H

#include <limits.h>

/* Pacify GCC 13.2 in some calls to _GL_EXPR_SIGNED.  */
#if 4 < __GNUC__ + (3 <= __GNUC_MINOR__) && !defined __clang__
# pragma GCC diagnostic ignored "-Wtype-limits"
#endif

/* Nonzero if this compiler has GCC bug 68193 or Clang bug 25764.  See:
   https://gcc.gnu.org/PR68193
   https://github.com/llvm/llvm-project/issues/25764
   For now, assume GCC < 14 and all Clang versions generate bogus
   warnings for _Generic.  This matters only for compilers that
   lack relevant builtins.  */
#if (__GNUC__ && __GNUC__ < 14) || defined __clang__
# define _GL__GENERIC_BOGUS 1
#else
# define _GL__GENERIC_BOGUS 0
#endif

/* Suppress -Wuseless-cast for, e.g., gcc-14 -std=gnu99.  */
#if __STDC_VERSION__ < 201112 && 14 <= __GNUC__
# pragma GCC diagnostic ignored "-Wuseless-cast"
#endif

/* Return a value with the common real type of E and V and the value of V.
   Do not evaluate E.  */
#define _GL_INT_CONVERT(e, v) ((1 ? 0 : (e)) + (v))

/* The extra casts in the following macros work around compiler bugs,
   e.g., in Cray C 5.0.3.0.  */

/* True if the standard integer or standard real type T is signed.  */
#if (__STDC_VERSION__ < 201112 || (defined _MSC_VER && _MSC_VER < 1944) \
     || _GL__GENERIC_BOGUS)
# define _GL_TYPE_SIGNED(t) (! ((t) 0 < (t) -1))
#else
/* Pacify -Wuseless-cast, but do not default to the simpler expression;
   see <https://gcc.gnu.org/PR125261>.  */
# define _GL_TYPE_SIGNED(t) \
   (_Generic ((t) {0}, \
              bool: 0, char: CHAR_MIN < 0, signed char: 1, unsigned char: 0, \
              short int: 1, unsigned short int: 0, int: 1, unsigned int: 0, \
              long int: 1, unsigned long int: 0, long long int: 1, unsigned long long int: 0, \
              float: 1, double: 1, long double: 1))
#endif

/* Return 1 if the real expression E, after promotion, has a
   signed or floating type.  Do not evaluate E.  */
#define _GL_EXPR_SIGNED(e) (_GL_INT_CONVERT (e, -1) < 0)


/* Minimum and maximum values for integer types and expressions.  */

/* The width in bits of the integer type or expression T.
   Do not evaluate T.  T must not be a bit-field expression.
   Padding bits are not supported; this is checked at compile-time below.  */
#define _GL_TYPE_WIDTH(t) (sizeof (t) * CHAR_BIT)

/* The maximum and minimum values for the type of the expression E,
   after integer promotion.  E is not evaluated.  */
#define _GL_INT_MINIMUM(e)                                              \
  (_GL_EXPR_SIGNED (e)                                                  \
   ? ~ _GL_SIGNED_INT_MAXIMUM (e)                                       \
   : _GL_INT_CONVERT (e, 0))
#define _GL_INT_MAXIMUM(e)                                              \
  (_GL_EXPR_SIGNED (e)                                                  \
   ? _GL_SIGNED_INT_MAXIMUM (e)                                         \
   : _GL_INT_CONVERT (e, -1))
#define _GL_SIGNED_INT_MAXIMUM(e)                                       \
  (((_GL_INT_CONVERT (e, 1) << (_GL_TYPE_WIDTH (+ (e)) - 2)) - 1) * 2 + 1)

/* Work around OpenVMS incompatibility with C99.  */
#if !defined LLONG_MAX && defined __INT64_MAX
# define LLONG_MAX __INT64_MAX
# define LLONG_MIN __INT64_MIN
#endif

/* This include file assumes that signed types are two's complement without
   padding bits; the above macros have undefined behavior otherwise.
   If this is a problem for you, please let us know how to fix it for your host.
   This assumption is tested by the intprops-tests module.  */

/* Does the __typeof__ keyword work?  This could be done by
   'configure', but for now it's easier to do it by hand.  */
#if ((defined __GNUC__ && 2 <= __GNUC__) \
     || (defined __clang_major__ && 4 <= __clang_major__) \
     || (defined __IBMC__ && 1210 <= __IBMC__ && defined __IBM__TYPEOF__) \
     || (defined __SUNPRO_C && 0x5110 <= __SUNPRO_C && !__STDC__) \
     || (defined _MSC_VER && 1939 <= _MSC_VER && !defined __cplusplus))
# define _GL_HAVE___TYPEOF__ 1
#else
# define _GL_HAVE___TYPEOF__ 0
#endif

/* Return 1 if the integer type or expression T might be signed.  Return 0
   if it is definitely unsigned.  T must not be a bit-field expression.
   This macro does not evaluate its argument, and expands to an
   integer constant expression.  */
#if _GL_HAVE___TYPEOF__
# define _GL_SIGNED_TYPE_OR_EXPR(t) _GL_TYPE_SIGNED (__typeof__ (t))
#else
# define _GL_SIGNED_TYPE_OR_EXPR(t) 1
#endif

/* Return 1 if - A would overflow in [MIN,MAX] arithmetic.
   A should not have side effects, and A's type should be an
   integer with minimum value MIN and maximum MAX.  */
#define _GL_INT_NEGATE_RANGE_OVERFLOW(a, min, max) \
  ((min) < 0 ? (a) < - (max) : 0 < (a))

/* True if __builtin_add_overflow (A, B, P) and __builtin_sub_overflow
   (A, B, P) work when P is non-null.  */
#ifdef __EDG__
/* EDG-based compilers like nvc 22.1 cannot add 64-bit signed to unsigned
   <https://bugs.gnu.org/53256>.  */
# define _GL_HAS_BUILTIN_ADD_OVERFLOW 0
#elif defined __has_builtin
# define _GL_HAS_BUILTIN_ADD_OVERFLOW __has_builtin (__builtin_add_overflow)
/* __builtin_{add,sub}_overflow exists but is not reliable in GCC 5.x and 6.x,
   see <https://gcc.gnu.org/PR98269>.  */
#elif 7 <= __GNUC__
# define _GL_HAS_BUILTIN_ADD_OVERFLOW 1
#else
# define _GL_HAS_BUILTIN_ADD_OVERFLOW 0
#endif

/* True if __builtin_mul_overflow (A, B, P) works when P is non-null.  */
#if defined __clang_major__ && __clang_major__ < 21
/* Work around Clang bug <https://github.com/llvm/llvm-project/issues/16778>. */
# define _GL_HAS_BUILTIN_MUL_OVERFLOW 0
#else
# define _GL_HAS_BUILTIN_MUL_OVERFLOW _GL_HAS_BUILTIN_ADD_OVERFLOW
#endif

/* True if __builtin_add_overflow_p (A, B, C) works, and similarly for
   __builtin_sub_overflow_p and __builtin_mul_overflow_p.  */
#ifdef __EDG__
/* In EDG-based compilers like ICC 2021.3 and earlier,
   __builtin_add_overflow_p etc. are not treated as integral constant
   expressions even when all arguments are.  */
# define _GL_HAS_BUILTIN_OVERFLOW_P 0
#elif defined __has_builtin
# define _GL_HAS_BUILTIN_OVERFLOW_P __has_builtin (__builtin_mul_overflow_p)
#else
# define _GL_HAS_BUILTIN_OVERFLOW_P (7 <= __GNUC__)
#endif

#if (!defined _GL_STDCKDINT_H && 202311 <= __STDC_VERSION__ \
     && ! (_GL_HAS_BUILTIN_ADD_OVERFLOW && _GL_HAS_BUILTIN_MUL_OVERFLOW))
# include <stdckdint.h>
#endif

/* Store the low-order bits of A + B, A - B, A * B, respectively, into *R.
   Return 1 if the result overflows.  Arguments should not have side
   effects and A, B and *R can be of any integer type other than char,
   bool, a bit-precise integer type, or an enumeration type.  */
#if _GL_HAS_BUILTIN_ADD_OVERFLOW
# define _GL_INT_ADD_WRAPV(a, b, r) __builtin_add_overflow (a, b, r)
# define _GL_INT_SUBTRACT_WRAPV(a, b, r) __builtin_sub_overflow (a, b, r)
#elif defined ckd_add && defined ckd_sub && !defined _GL_STDCKDINT_H
# define _GL_INT_ADD_WRAPV(a, b, r) ckd_add (r, + (a), + (b))
# define _GL_INT_SUBTRACT_WRAPV(a, b, r) ckd_sub (r, + (a), + (b))
#else
# define _GL_INT_ADD_WRAPV(a, b, r) \
   _GL_INT_OP_WRAPV (a, b, r, +, _GL_INT_ADD_RANGE_OVERFLOW)
# define _GL_INT_SUBTRACT_WRAPV(a, b, r) \
   _GL_INT_OP_WRAPV (a, b, r, -, _GL_INT_SUBTRACT_RANGE_OVERFLOW)
#endif
#if _GL_HAS_BUILTIN_MUL_OVERFLOW
# if ((9 < __GNUC__ + (3 <= __GNUC_MINOR__) \
       || (__GNUC__ == 8 && 4 <= __GNUC_MINOR__)) \
      && !defined __clang__ && !defined __EDG__)
#  define _GL_INT_MULTIPLY_WRAPV(a, b, r) __builtin_mul_overflow (a, b, r)
# else
   /* Work around GCC bug 91450.  */
#  define _GL_INT_MULTIPLY_WRAPV(a, b, r) \
    ((!_GL_SIGNED_TYPE_OR_EXPR (*(r)) && _GL_EXPR_SIGNED (a) && _GL_EXPR_SIGNED (b) \
      && _GL_INT_MULTIPLY_RANGE_OVERFLOW (a, b, \
                                          (__typeof__ (*(r))) 0, \
                                          (__typeof__ (*(r))) -1)) \
     ? ((void) __builtin_mul_overflow (a, b, r), 1) \
     : __builtin_mul_overflow (a, b, r))
# endif
#elif defined ckd_mul && !defined _GL_STDCKDINT_H
# define _GL_INT_MULTIPLY_WRAPV(a, b, r) ckd_mul (r, + (a), + (b))
#else
# define _GL_INT_MULTIPLY_WRAPV(a, b, r) \
   _GL_INT_OP_WRAPV (a, b, r, *, _GL_INT_MULTIPLY_RANGE_OVERFLOW)
#endif

/* Store the low-order bits of A <op> B into *R, where OP specifies
   the operation and OVERFLOW the overflow predicate.  Return 1 if the
   result overflows.  Arguments should not have side effects,
   and A, B and *R can be of any integer type other than char, bool, a
   bit-precise integer type, or an enumeration type.  */
#if 201112 <= __STDC_VERSION__ && !_GL__GENERIC_BOGUS
# define _GL_INT_OP_WRAPV(a, b, r, op, overflow) \
   (_Generic \
    (*(r), \
     signed char: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                        signed char, SCHAR_MIN, SCHAR_MAX), \
     unsigned char: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                        unsigned char, 0, UCHAR_MAX), \
     short int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                        short int, SHRT_MIN, SHRT_MAX), \
     unsigned short int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                        unsigned short int, 0, USHRT_MAX), \
     int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                        int, INT_MIN, INT_MAX), \
     unsigned int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                        unsigned int, 0, UINT_MAX), \
     long int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long int, \
                        long int, LONG_MIN, LONG_MAX), \
     unsigned long int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long int, \
                        unsigned long int, 0, ULONG_MAX), \
     long long int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long long int, \
                        long long int, LLONG_MIN, LLONG_MAX), \
     unsigned long long int: \
       _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long long int, \
                        unsigned long long int, 0, ULLONG_MAX)))
#else
/* Store the low-order bits of A <op> B into *R, where OP specifies
   the operation and OVERFLOW the overflow predicate.  If *R is
   signed, its type is ST with bounds SMIN..SMAX; otherwise its type
   is UT with bounds U..UMAX.  ST and UT are narrower than int.
   Return 1 if the result overflows.  Arguments should not have side
   effects, and A, B and *R can be of any integer type other than
   char, bool, a bit-precise integer type, or an enumeration type.  */
# if _GL_HAVE___TYPEOF__
#  define _GL_INT_OP_WRAPV_SMALLISH(a,b,r,op,overflow,st,smin,smax,ut,umax) \
    (_GL_TYPE_SIGNED (__typeof__ (*(r))) \
     ? _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, st, smin, smax) \
     : _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, ut, 0, umax))
# else
#  define _GL_INT_OP_WRAPV_SMALLISH(a,b,r,op,overflow,st,smin,smax,ut,umax) \
    (overflow (a, b, smin, smax) \
     ? (overflow (a, b, 0, umax) \
        ? (*(r) = _GL_INT_OP_WRAPV_VIA_UNSIGNED (a,b,op,unsigned,st), 1) \
        : (*(r) = _GL_INT_OP_WRAPV_VIA_UNSIGNED (a,b,op,unsigned,st)) < 0) \
     : (overflow (a, b, 0, umax) \
        ? (*(r) = _GL_INT_OP_WRAPV_VIA_UNSIGNED (a,b,op,unsigned,st)) >= 0 \
        : (*(r) = _GL_INT_OP_WRAPV_VIA_UNSIGNED (a,b,op,unsigned,st), 0)))
# endif

# define _GL_INT_OP_WRAPV(a, b, r, op, overflow) \
   (sizeof *(r) == sizeof (signed char) \
    ? _GL_INT_OP_WRAPV_SMALLISH (a, b, r, op, overflow, \
                                 signed char, SCHAR_MIN, SCHAR_MAX, \
                                 unsigned char, UCHAR_MAX) \
    : sizeof *(r) == sizeof (short int) \
    ? _GL_INT_OP_WRAPV_SMALLISH (a, b, r, op, overflow, \
                                 short int, SHRT_MIN, SHRT_MAX, \
                                 unsigned short int, USHRT_MAX) \
    : sizeof *(r) == sizeof (int) \
    ? (_GL_EXPR_SIGNED (*(r)) \
       ? _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                          int, INT_MIN, INT_MAX) \
       : _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned int, \
                          unsigned int, 0, UINT_MAX)) \
    : _GL_INT_OP_WRAPV_LONGISH(a, b, r, op, overflow))
# ifdef LLONG_MAX
#  define _GL_INT_OP_WRAPV_LONGISH(a, b, r, op, overflow) \
    (sizeof *(r) == sizeof (long int) \
     ? (_GL_EXPR_SIGNED (*(r)) \
        ? _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long int, \
                           long int, LONG_MIN, LONG_MAX) \
        : _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long int, \
                           unsigned long int, 0, ULONG_MAX)) \
     : (_GL_EXPR_SIGNED (*(r)) \
        ? _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long long int, \
                           long long int, LLONG_MIN, LLONG_MAX) \
        : _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long long int, \
                           unsigned long long int, 0, ULLONG_MAX)))
# else
#  define _GL_INT_OP_WRAPV_LONGISH(a, b, r, op, overflow) \
    (_GL_EXPR_SIGNED (*(r)) \
     ? _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long int, \
                        long int, LONG_MIN, LONG_MAX) \
     : _GL_INT_OP_CALC (a, b, r, op, overflow, unsigned long int, \
                        unsigned long int, 0, ULONG_MAX))
# endif
#endif

/* Store the low-order bits of A <op> B into *R, where the operation
   is given by OP.  Use the unsigned type UT for calculation to avoid
   overflow problems.  *R's type is T, with extrema TMIN and TMAX.
   T can be any signed integer type other than char, bool, a
   bit-precise integer type, or an enumeration type.
   Return 1 if the result overflows.  */
#define _GL_INT_OP_CALC(a, b, r, op, overflow, ut, t, tmin, tmax) \
  (overflow (a, b, tmin, tmax) \
   ? (*(r) = _GL_INT_OP_WRAPV_VIA_UNSIGNED (a, b, op, ut, t), 1) \
   : (*(r) = _GL_INT_OP_WRAPV_VIA_UNSIGNED (a, b, op, ut, t), 0))

/* Return 1 if the integer expression -A would overflow.
   Arguments should not have side effects,
   and can be any signed integer type other than char, bool, a
   bit-precise integer type, or an enumeration type.
   These macros are tuned for their last input argument being a constant.  */

#if _GL_HAS_BUILTIN_OVERFLOW_P
# define _GL_INT_NEGATE_OVERFLOW(a) \
   __builtin_sub_overflow_p (0, a, _GL_INT_CONVERT (- (a), 0))
#else
# define _GL_INT_NEGATE_OVERFLOW(a) \
   _GL_INT_NEGATE_RANGE_OVERFLOW (a, _GL_INT_MINIMUM (a), _GL_INT_MAXIMUM (a))
#endif

/* Return the low-order bits of A <op> B, where the operation is given
   by OP.  Use the unsigned type UT for calculation to avoid undefined
   behavior on signed integer overflow, and convert the result to type T.
   UT is at least as wide as T and is no narrower than unsigned int,
   T is two's complement, and there is no padding or trap representations.
   Assume that converting UT to T yields the low-order bits, as is
   done in all known two's-complement C compilers.  E.g., see:
   https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html

   According to the C standard, converting UT to T yields an
   implementation-defined result or signal for values outside T's
   range.  However, code that works around this theoretical problem
   runs afoul of a compiler bug in Oracle Studio 12.3 x86.  See:
   https://lists.gnu.org/r/bug-gnulib/2017-04/msg00049.html
   As the compiler bug is real, don't try to work around the
   theoretical problem.  */

#define _GL_INT_OP_WRAPV_VIA_UNSIGNED(a, b, op, ut, t) \
  ((t) ((ut) (a) op (ut) (b)))

/* Return true if the numeric values A + B, A - B, A * B fall outside
   the range TMIN..TMAX.  Arguments should not have side effects
   and can be any integer type other than char, bool,
   a bit-precise integer type, or an enumeration type.
   TMIN should be signed and nonpositive.
   TMAX should be positive, and should be signed unless TMIN is zero.  */
#define _GL_INT_ADD_RANGE_OVERFLOW(a, b, tmin, tmax) \
  ((b) < 0 \
   ? (((tmin) \
       ? ((_GL_EXPR_SIGNED (_GL_INT_CONVERT (a, (tmin) - (b))) || (b) < (tmin)) \
          && (a) < (tmin) - (b)) \
       : (a) <= -1 - (b)) \
      || ((_GL_EXPR_SIGNED (a) ? 0 <= (a) : (tmax) < (a)) && (tmax) < (a) + (b))) \
   : (a) < 0 \
   ? (((tmin) \
       ? ((_GL_EXPR_SIGNED (_GL_INT_CONVERT (b, (tmin) - (a))) || (a) < (tmin)) \
          && (b) < (tmin) - (a)) \
       : (b) <= -1 - (a)) \
      || ((_GL_EXPR_SIGNED (_GL_INT_CONVERT (a, b)) || (tmax) < (b)) \
          && (tmax) < (a) + (b))) \
   : (tmax) < (b) || (tmax) - (b) < (a))
#define _GL_INT_SUBTRACT_RANGE_OVERFLOW(a, b, tmin, tmax) \
  (((a) < 0) == ((b) < 0) \
   ? ((a) < (b) \
      ? !(tmin) || -1 - (tmin) < (b) - (a) - 1 \
      : (tmax) < (a) - (b)) \
   : (a) < 0 \
   ? ((!_GL_EXPR_SIGNED (_GL_INT_CONVERT ((a) - (tmin), b)) && (a) - (tmin) < 0) \
      || (a) - (tmin) < (b)) \
   : ((! (_GL_EXPR_SIGNED (_GL_INT_CONVERT (tmax, b)) \
          && _GL_EXPR_SIGNED (_GL_INT_CONVERT ((tmax) + (b), a))) \
       && (tmax) <= -1 - (b)) \
      || (tmax) + (b) < (a)))
#define _GL_INT_MULTIPLY_RANGE_OVERFLOW(a, b, tmin, tmax) \
  ((b) < 0 \
   ? ((a) < 0 \
      ? (_GL_EXPR_SIGNED (_GL_INT_CONVERT (tmax, b)) \
         ? (a) < (tmax) / (b) \
         : ((_GL_INT_NEGATE_OVERFLOW (b) \
             ? _GL_INT_CONVERT (b, tmax) >> (_GL_TYPE_WIDTH (+ (b)) - 1) \
             : (tmax) / -(b)) \
            <= -1 - (a))) \
      : _GL_INT_NEGATE_OVERFLOW (_GL_INT_CONVERT (b, tmin)) && (b) == -1 \
      ? (_GL_EXPR_SIGNED (a) \
         ? 0 < (a) + (tmin) \
         : 0 < (a) && -1 - (tmin) < (a) - 1) \
      : (tmin) / (b) < (a)) \
   : (b) == 0 \
   ? 0 \
   : ((a) < 0 \
      ? (_GL_INT_NEGATE_OVERFLOW (_GL_INT_CONVERT (a, tmin)) && (a) == -1 \
         ? (_GL_EXPR_SIGNED (b) ? 0 < (b) + (tmin) : -1 - (tmin) < (b) - 1) \
         : (tmin) / (a) < (b)) \
      : (tmax) / (b) < (a)))

#endif /* _GL_INTPROPS_INTERNAL_H */
