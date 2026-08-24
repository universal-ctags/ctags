/* A C macro for declaring that a function does not return.
   Copyright (C) 2011-2026 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* The _Noreturn keyword of C11.
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
# elif (2 < __GNUC__ + (8 <= __GNUC_MINOR__) || defined __clang__ \
        || 0x5110 <= __SUNPRO_C)
   /* Prefer __attribute__ ((__noreturn__)) to plain _Noreturn even if the
      latter works, as 'gcc -std=gnu99 -Wpedantic' warns about _Noreturn.  */
#  define _Noreturn __attribute__ ((__noreturn__))
# elif 1200 <= (defined _MSC_VER ? _MSC_VER : 0)
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn
# endif
#endif
