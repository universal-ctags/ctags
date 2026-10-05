/* intprops.h -- properties of integer types

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

#ifndef _GL_INTPROPS_H
#define _GL_INTPROPS_H

#include "intprops-internal.h"

/* The extra casts in the following macros work around compiler bugs,
   e.g., in Cray C 5.0.3.0.  */

/* True if the arithmetic type T is an integer type.  bool counts as
   an integer.  */
#if (__STDC_VERSION__ < 201112 || (defined _MSC_VER && _MSC_VER < 1944) \
     || _GL__GENERIC_BOGUS)
# define TYPE_IS_INTEGER(t) ((t) 1.5 == 1)
#else
/* Pacify -Wuseless-cast and do not default to the simpler expression;
   see <https://gcc.gnu.org/PR125261>.  */
# define TYPE_IS_INTEGER(t) \
    (_Generic ((t) {0}, \
               bool: 1, char: 1, signed char: 1, unsigned char: 1, \
               short int: 1, unsigned short int: 1, int: 1, unsigned int: 1, \
               long int: 1, unsigned long int: 1, long long int: 1, unsigned long long int: 1, \
               float: 0, double: 0, long double: 0))
#endif

/* True if the standard integer or standard real type T is signed.  */
#define TYPE_SIGNED(t) _GL_TYPE_SIGNED (t)

/* Return 1 if the real expression E, after promotion, has a
   signed or floating type.  Do not evaluate E.  */
#define EXPR_SIGNED(e) _GL_EXPR_SIGNED (e)

/* The same value as as the arithmetic expression E, but with E's type
   after integer promotions.  For example, if E is of type 'enum {A, B}'
   then 'switch (INT_PROMOTE (E))' pacifies gcc -Wswitch-enum if some
   enum values are deliberately omitted from the switch's cases.
   Here, unary + is safer than a cast or inline function, as unary +
   does only integer promotions and is disallowed on pointers.  */
#define INT_PROMOTE(e) (+ (e))


/* Minimum and maximum values for integer types and expressions.  */

/* The width in bits of the integer type or expression T.
   Do not evaluate T.  T must not be a bit-field expression.
   Padding bits are not supported; this is checked at compile-time below.  */
#define TYPE_WIDTH(t) _GL_TYPE_WIDTH (t)

/* The maximum and minimum values for the standard integer type T.  */
#if (__STDC_VERSION__ < 201112 || (defined _MSC_VER && _MSC_VER < 1944) \
     || _GL__GENERIC_BOGUS)
# define TYPE_MINIMUM(t) ((t) ~ TYPE_MAXIMUM (t))
# define TYPE_MAXIMUM(t)                                                \
   ((t) (! TYPE_SIGNED (t)                                              \
         ? (t) -1                                                       \
         : ((((t) 1 << (TYPE_WIDTH (t) - 2)) - 1) * 2 + 1)))
#else
/* Pacify -Wuseless-cast and do not default to the simpler expressions;
   see <https://gcc.gnu.org/PR125261>.  */
# define TYPE_MINIMUM(t) \
   (_Generic ((t) {0}, \
              bool: (bool) 0, char: (char) CHAR_MIN, \
              signed char: (signed char) SCHAR_MIN, unsigned char: (unsigned char) 0, \
              short int: (short int) SHRT_MIN, unsigned short int: (unsigned short int) 0, \
              int: INT_MIN, unsigned int: 0u, \
              long int: LONG_MIN, unsigned long int: 0ul, \
              long long int: LLONG_MIN, unsigned long long int: 0ull))
# define TYPE_MAXIMUM(t) \
   (_Generic ((t) {0}, \
              bool: (bool) 1, char: (char) CHAR_MAX, \
              signed char: (signed char) SCHAR_MAX, unsigned char: (unsigned char) -1, \
              short int: (short int) SHRT_MAX, unsigned short int: (unsigned short int) -1, \
              int: INT_MAX, unsigned int: -1u, \
              long int: LONG_MAX, unsigned long int: -1ul, \
              long long int: LLONG_MAX, unsigned long long int: -1ull))
#endif

/* Bound on length of the string representing an unsigned integer
   value representable in B bits.  log10 (2.0) < 146/485.  The
   smallest value of B where this bound is not tight is 2621.  */
#define INT_BITS_STRLEN_BOUND(b) (((b) * 146 + 484) / 485)

/* Bound on length of the string representing an integer type or expression T.
   T must not be a bit-field expression.

   Subtract 1 for the sign bit if T is signed, and then add 1 more for
   a minus sign if needed.

   Because _GL_SIGNED_TYPE_OR_EXPR sometimes returns 1 when its argument is
   unsigned, this macro may overestimate the true bound by one byte when
   applied to unsigned types of size 2, 4, 16, ... bytes.  */
#define INT_STRLEN_BOUND(t)                                     \
  (INT_BITS_STRLEN_BOUND (TYPE_WIDTH (t) - _GL_SIGNED_TYPE_OR_EXPR (t)) \
   + _GL_SIGNED_TYPE_OR_EXPR (t))

/* Bound on buffer size needed to represent an integer type or expression T,
   including the terminating null.  T must not be a bit-field expression.  */
#define INT_BUFSIZE_BOUND(t) (INT_STRLEN_BOUND (t) + 1)


/* Range overflow checks.

   The INT_<op>_RANGE_OVERFLOW macros return 1 if the corresponding C
   operators overflow arithmetically when given the same arguments.
   These macros do not rely on undefined or implementation-defined behavior.
   Although their implementations are simple and straightforward,
   they are harder to use and may be less efficient than the
   INT_<op>_WRAPV, INT_<op>_OK, and INT_<op>_OVERFLOW macros described below.

   Example usage:

     long int i = ...;
     long int j = ...;
     if (INT_MULTIPLY_RANGE_OVERFLOW (i, j, LONG_MIN, LONG_MAX))
       printf ("multiply would overflow");
     else
       printf ("product is %ld", i * j);

   Restrictions on *_RANGE_OVERFLOW macros:

   These macros do not check for all possible numerical problems or
   undefined or unspecified behavior: they do not check for division
   by zero, for bad shift counts, or for shifting negative numbers.

   These macros may evaluate their arguments zero or multiple times,
   so the arguments should not have side effects.  The arithmetic
   arguments (including the MIN and MAX arguments) must be of the same
   integer type after the usual arithmetic conversions, and the type
   must have minimum value MIN and maximum MAX.  Unsigned types should
   use a zero MIN of the proper type.

   Because all arguments are subject to integer promotions, these
   macros typically do not work on types narrower than 'int'.

   These macros are tuned for constant MIN and MAX.  For commutative
   operations such as A + B, they are also tuned for constant B.  */

/* Return 1 if A + B would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  */
#define INT_ADD_RANGE_OVERFLOW(a, b, min, max)          \
  ((b) < 0                                              \
   ? (a) < (min) - (b)                                  \
   : (max) - (b) < (a))

/* Return 1 if A - B would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  */
#define INT_SUBTRACT_RANGE_OVERFLOW(a, b, min, max)     \
  ((b) < 0                                              \
   ? (max) + (b) < (a)                                  \
   : (a) < (min) + (b))

/* Return 1 if - A would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  */
#define INT_NEGATE_RANGE_OVERFLOW(a, min, max)          \
  _GL_INT_NEGATE_RANGE_OVERFLOW (a, min, max)

/* Return 1 if A * B would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  Avoid && and || as they tickle
   bugs in Sun C 5.11 2010/08/13 and other compilers; see
   <https://lists.gnu.org/r/bug-gnulib/2011-05/msg00401.html>.  */
#define INT_MULTIPLY_RANGE_OVERFLOW(a, b, min, max)     \
  ((b) < 0                                              \
   ? ((a) < 0                                           \
      ? (a) < (max) / (b)                               \
      : (b) == -1                                       \
      ? 0                                               \
      : (min) / (b) < (a))                              \
   : (b) == 0                                           \
   ? 0                                                  \
   : ((a) < 0                                           \
      ? (a) < (min) / (b)                               \
      : (max) / (b) < (a)))

/* Return 1 if A / B would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  Do not check for division by zero.  */
#define INT_DIVIDE_RANGE_OVERFLOW(a, b, min, max)       \
  ((min) < 0 && (b) == -1 && (a) < - (max))

/* Return 1 if A % B would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  Do not check for division by zero.
   Mathematically, % should never overflow, but on x86-like hosts
   INT_MIN % -1 traps, and the C standard permits this, so treat this
   as an overflow too.  */
#define INT_REMAINDER_RANGE_OVERFLOW(a, b, min, max)    \
  INT_DIVIDE_RANGE_OVERFLOW (a, b, min, max)

/* Return 1 if A << B would overflow in [MIN,MAX] arithmetic.
   See above for restrictions.  Here, MIN and MAX are for A only, and B need
   not be of the same type as the other arguments.  The C standard says that
   behavior is undefined for shifts unless 0 <= B < wordwidth, and that when
   A is negative then A << B has undefined behavior and A >> B has
   implementation-defined behavior, but do not check these other
   restrictions.  */
#define INT_LEFT_SHIFT_RANGE_OVERFLOW(a, b, min, max)   \
  ((a) < 0                                              \
   ? (a) < (min) >> (b)                                 \
   : (max) >> (b) < (a))

/* The _GL*_OVERFLOW macros have the same restrictions as the
   *_RANGE_OVERFLOW macros, except that they do not assume that operands
   (e.g., A and B) have the same type as MIN and MAX.  Instead, they assume
   that the result (e.g., A + B) has that type.  */
#if _GL_HAS_BUILTIN_OVERFLOW_P
# define _GL_ADD_OVERFLOW(a, b, min, max)                               \
   __builtin_add_overflow_p (a, b, _GL_INT_CONVERT ((a) + (b), 0))
# define _GL_SUBTRACT_OVERFLOW(a, b, min, max)                          \
   __builtin_sub_overflow_p (a, b, _GL_INT_CONVERT ((a) - (b), 0))
# define _GL_MULTIPLY_OVERFLOW(a, b, min, max)                          \
   __builtin_mul_overflow_p (a, b, _GL_INT_CONVERT ((a) * (b), 0))
#else
# define _GL_ADD_OVERFLOW(a, b, min, max)                                \
   ((min) < 0 ? INT_ADD_RANGE_OVERFLOW (a, b, min, max)                  \
    : (a) < 0 ? (b) <= (a) + (b)                                         \
    : (b) < 0 ? (a) <= (a) + (b)                                         \
    : (a) + (b) < (b))
# define _GL_SUBTRACT_OVERFLOW(a, b, min, max)                           \
   ((min) < 0 ? INT_SUBTRACT_RANGE_OVERFLOW (a, b, min, max)             \
    : (a) < 0 ? 1                                                        \
    : (b) < 0 ? (a) - (b) <= (a)                                         \
    : (a) < (b))
# define _GL_MULTIPLY_OVERFLOW(a, b, min, max)                           \
   (((min) == 0 && (((a) < 0 && 0 < (b)) || ((b) < 0 && 0 < (a))))       \
    || INT_MULTIPLY_RANGE_OVERFLOW (a, b, min, max))
#endif
#define _GL_DIVIDE_OVERFLOW(a, b, min, max)                             \
  ((min) < 0 ? (b) == _GL_INT_CONVERT (min, -1) && (a) < - (max)        \
   : (a) < 0 ? (b) <= (a) + (b) - 1                                     \
   : (b) < 0 && (a) + (b) <= (a))
#define _GL_REMAINDER_OVERFLOW(a, b, min, max)                          \
  ((min) < 0 ? (b) == _GL_INT_CONVERT (min, -1) && (a) < - (max)        \
   : (a) < 0 ? (a) % (b) != ((max) - (b) + 1) % (b)                     \
   : (b) < 0 && ! _GL_UNSIGNED_NEG_MULTIPLE (a, b, max))

/* Return a nonzero value if A is a mathematical multiple of B, where
   A is unsigned, B is negative, and MAX is the maximum value of A's
   type.  A's type must be the same as (A % B)'s type.  Normally (A %
   -B == 0) suffices, but things get tricky if -B would overflow.  */
#define _GL_UNSIGNED_NEG_MULTIPLE(a, b, max)                            \
  (((b) < -_GL_SIGNED_INT_MAXIMUM (b)                                   \
    ? (_GL_SIGNED_INT_MAXIMUM (b) == (max)                              \
       ? (a)                                                            \
       : (a) % (_GL_INT_CONVERT (a, _GL_SIGNED_INT_MAXIMUM (b)) + 1))   \
    : (a) % - (b))                                                      \
   == 0)

/* Check for integer overflow, and report low order bits of answer.

   The INT_<op>_OVERFLOW macros return 1 if the corresponding C operators
   might not yield numerically correct answers due to arithmetic overflow.
   The INT_<op>_WRAPV macros compute the low-order bits of the sum,
   difference, and product of two C integers, and return 1 if these
   low-order bits are not numerically correct.
   These macros work correctly on all known practical hosts, and do not rely
   on undefined behavior due to signed arithmetic overflow.

   Example usage, assuming A and B are long int:

     if (INT_MULTIPLY_OVERFLOW (a, b))
       printf ("result would overflow\n");
     else
       printf ("result is %ld (no overflow)\n", a * b);

   Example usage with WRAPV flavor:

     long int result;
     bool overflow = INT_MULTIPLY_WRAPV (a, b, &result);
     printf ("result is %ld (%s)\n", result,
             overflow ? "after overflow" : "no overflow");

   Restrictions on these macros:

   These macros do not check for all possible numerical problems or
   undefined or unspecified behavior: they do not check for division
   by zero, for bad shift counts, or for shifting negative numbers.

   These macros may evaluate their arguments zero or multiple times, so the
   arguments should not have side effects.

   The WRAPV macros are not constant expressions.  They support only
   +, binary -, and *.

   Because the WRAPV macros convert the result, they report overflow
   in different circumstances than the OVERFLOW macros do.  For
   example, in the typical case with 16-bit 'short' and 32-bit 'int',
   if A, B and *R are all of type 'short' then INT_ADD_OVERFLOW (A, B)
   returns false because the addition cannot overflow after A and B
   are converted to 'int', whereas INT_ADD_WRAPV (A, B, R) returns
   true or false depending on whether the sum fits into 'short'.

   These macros are tuned for their last input argument being a constant.

   A, B, and *R should be integers; they need not be the same type,
   and they need not be all signed or all unsigned.
   However, none of the integer types should be bit-precise,
   and *R's type should not be char, bool, or an enumeration type.

   Return 1 if the integer expressions A * B, A - B, -A, A * B, A / B,
   A % B, and A << B would overflow, respectively.  */

#define INT_ADD_OVERFLOW(a, b) \
  _GL_BINARY_OP_OVERFLOW (a, b, _GL_ADD_OVERFLOW)
#define INT_SUBTRACT_OVERFLOW(a, b) \
  _GL_BINARY_OP_OVERFLOW (a, b, _GL_SUBTRACT_OVERFLOW)
#define INT_NEGATE_OVERFLOW(a) _GL_INT_NEGATE_OVERFLOW (a)
#define INT_MULTIPLY_OVERFLOW(a, b) \
  _GL_BINARY_OP_OVERFLOW (a, b, _GL_MULTIPLY_OVERFLOW)
#define INT_DIVIDE_OVERFLOW(a, b) \
  _GL_BINARY_OP_OVERFLOW (a, b, _GL_DIVIDE_OVERFLOW)
#define INT_REMAINDER_OVERFLOW(a, b) \
  _GL_BINARY_OP_OVERFLOW (a, b, _GL_REMAINDER_OVERFLOW)
#define INT_LEFT_SHIFT_OVERFLOW(a, b) \
  INT_LEFT_SHIFT_RANGE_OVERFLOW (a, b, \
                                 _GL_INT_MINIMUM (a), _GL_INT_MAXIMUM (a))

/* Return 1 if the expression A <op> B would overflow,
   where OP_RESULT_OVERFLOW (A, B, MIN, MAX) does the actual test,
   assuming MIN and MAX are the minimum and maximum for the result type.
   Arguments should be free of side effects.  */
#define _GL_BINARY_OP_OVERFLOW(a, b, op_result_overflow)        \
  op_result_overflow (a, b,                                     \
                      _GL_INT_MINIMUM (_GL_INT_CONVERT (a, b)), \
                      _GL_INT_MAXIMUM (_GL_INT_CONVERT (a, b)))

/* Store the low-order bits of A + B, A - B, A * B, respectively, into *R.
   Return 1 if the result overflows.  See above for restrictions.  */
#define INT_ADD_WRAPV(a, b, r) _GL_INT_ADD_WRAPV (a, b, r)
#define INT_SUBTRACT_WRAPV(a, b, r) _GL_INT_SUBTRACT_WRAPV (a, b, r)
#define INT_MULTIPLY_WRAPV(a, b, r) _GL_INT_MULTIPLY_WRAPV (a, b, r)

/* The following macros compute A + B, A - B, and A * B, respectively.
   If no overflow occurs, they set *R to the result and return 1;
   otherwise, they return 0 and may modify *R.

   Example usage:

     long int result;
     if (INT_ADD_OK (a, b, &result))
       printf ("result is %ld\n", result);
     else
       printf ("overflow\n");

   A, B, and *R should be integers; they need not be the same type,
   and they need not be all signed or all unsigned.
   However, none of the integer types should be bit-precise,
   and *R's type should not be char, bool, or an enumeration type.

   These macros work correctly on all known practical hosts, and do not rely
   on undefined behavior due to signed arithmetic overflow.

   These macros are not constant expressions.

   These macros may evaluate their arguments zero or multiple times, so the
   arguments should not have side effects.

   These macros are tuned for B being a constant.  */

#define INT_ADD_OK(a, b, r) (! INT_ADD_WRAPV (a, b, r))
#define INT_SUBTRACT_OK(a, b, r) (! INT_SUBTRACT_WRAPV (a, b, r))
#define INT_MULTIPLY_OK(a, b, r) (! INT_MULTIPLY_WRAPV (a, b, r))

#endif /* _GL_INTPROPS_H */
