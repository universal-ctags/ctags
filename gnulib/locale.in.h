/* A POSIX <locale.h>.
   Copyright (C) 2007-2026 Free Software Foundation, Inc.

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

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

#if (defined _WIN32 && !defined __CYGWIN__ && defined __need_locale_t) \
    || defined _@GUARD_PREFIX@_ALREADY_INCLUDING_LOCALE_H

/* Special invocation convention:
   - Inside mingw header files,
   - To handle Solaris header files (through Solaris 10) when combined
     with gettext's libintl.h.  */

#@INCLUDE_NEXT@ @NEXT_LOCALE_H@

#else
/* Normal invocation convention.  */

#ifndef _@GUARD_PREFIX@_LOCALE_H

#define _@GUARD_PREFIX@_ALREADY_INCLUDING_LOCALE_H

/* The include_next requires a split double-inclusion guard.  */
#@INCLUDE_NEXT@ @NEXT_LOCALE_H@

#undef _@GUARD_PREFIX@_ALREADY_INCLUDING_LOCALE_H

#ifndef _@GUARD_PREFIX@_LOCALE_H
#define _@GUARD_PREFIX@_LOCALE_H

/* This file uses GNULIB_POSIXCHECK, HAVE_RAW_DECL_*.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* NetBSD 5.0 mis-defines NULL.  */
#include <stddef.h>

/* Mac OS X 10.5 defines the locale_t type in <xlocale.h>.  */
#if @HAVE_XLOCALE_H@
# include <xlocale.h>
#endif

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_ARG_NONNULL is copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

/* The LC_MESSAGES locale category is specified in POSIX, but not in ISO C.
   On systems that don't define it, use the same value as GNU libintl.  */
#if !defined LC_MESSAGES
# define LC_MESSAGES 1729
#endif

#if !@HAVE_LOCALE_T@
# if !defined GNULIB_defined_locale_t
/* The values of the POSIX-standardized LC_* macros are:

                  LC_COLLATE LC_CTYPE LC_MESSAGES LC_MONETARY LC_NUMERIC LC_TIME

   glibc, Solaris,     3        0           5         4            1        2
   Android
   macOS, *BSD         1        2           6         3            4        5
   native Windows      1        2        1729         3            4        5

   We map these to the log2(LC_*_MASK) values, chosen to be compatible with
   later releases of the same operating system.  */
#  if defined __APPLE__ && defined __MACH__          /* macOS */
/*                LC_COLLATE LC_CTYPE LC_MESSAGES LC_MONETARY LC_NUMERIC LC_TIME

   category            1        2           6         3            4        5
   log2(LC_*_MASK)     0        1           2         3            4        5
 */
#   define _gl_log2_lc_mask(category) ((0x2543100 >> (4 * (category))) & 0xf)
#  elif defined __FreeBSD__ || defined __DragonFly__ /* FreeBSD */
/*                LC_COLLATE LC_CTYPE LC_MESSAGES LC_MONETARY LC_NUMERIC LC_TIME

   category            1        2           6         3            4        5
   log2(LC_*_MASK)     0        1           5         2            3        4
 */
#   define _gl_log2_lc_mask(category) ((category) - 1)
#  elif defined _WIN32 && !defined __CYGWIN__        /* native Windows */
#   define _gl_log2_lc_mask(category) \
      ((category) == LC_MESSAGES ? 0 : (category))
#  else                           /* glibc, Solaris, Android, NetBSD, OpenBSD */
#   define _gl_log2_lc_mask(category) (category)
#  endif
/* From there we map them to array indices 0..5.  */
#  if (_gl_log2_lc_mask (LC_COLLATE) == 0 || _gl_log2_lc_mask (LC_CTYPE) == 0 \
       || _gl_log2_lc_mask (LC_MESSAGES) == 0)
  /* glibc, Solaris, Android, macOS, FreeBSD, native Windows */
#   define _gl_log2_lcmask_to_index(c) (c)
#   define _gl_index_to_log2_lcmask(i) (i)
#  else
  /* NetBSD, OpenBSD */
#   define _gl_log2_lcmask_to_index(c) ((c) - 1)
#   define _gl_index_to_log2_lcmask(i) ((i) + 1)
#  endif
/* Define the LC_*_MASK macros.  */
#  define LC_COLLATE_MASK  (1 << _gl_log2_lc_mask (LC_COLLATE))
#  define LC_CTYPE_MASK    (1 << _gl_log2_lc_mask (LC_CTYPE))
#  define LC_MESSAGES_MASK (1 << _gl_log2_lc_mask (LC_MESSAGES))
#  define LC_MONETARY_MASK (1 << _gl_log2_lc_mask (LC_MONETARY))
#  define LC_NUMERIC_MASK  (1 << _gl_log2_lc_mask (LC_NUMERIC))
#  define LC_TIME_MASK     (1 << _gl_log2_lc_mask (LC_TIME))
#  define LC_ALL_MASK \
     (LC_COLLATE_MASK | LC_CTYPE_MASK | LC_MESSAGES_MASK | LC_MONETARY_MASK \
      | LC_NUMERIC_MASK | LC_TIME_MASK)
/* Now define the locale_t type.  */
struct gl_locale_category_t
{
  char *name;
  bool is_c_locale;
#  if @HAVE_WINDOWS_LOCALE_T@
  /* Use the native Windows '_locale_t' type.
     Documentation:
     <https://learn.microsoft.com/en-us/cpp/c-runtime-library/locale>
     This field is NULL if is_c_locale is true.  But don't use this NULL value,
     since for the native Windows *_l functions a null _locale_t means to use
     the global locale.  */
  _locale_t system_locale;
#  endif
};
struct gl_locale_t
{
  struct gl_locale_category_t category[6];
};
typedef struct gl_locale_t *locale_t;
#  define LC_GLOBAL_LOCALE ((locale_t)(-1))
#  define GNULIB_defined_locale_t 1
# endif
#endif

/* On native Windows with MSVC, 'struct lconv' lacks the members int_p_* and
   int_n_*.  Instead of overriding 'struct lconv', merely define these member
   names as macros.  This avoids trouble in C++ mode.  */
#if defined _MSC_VER
# define int_p_cs_precedes   p_cs_precedes
# define int_p_sign_posn     p_sign_posn
# define int_p_sep_by_space  p_sep_by_space
# define int_n_cs_precedes   n_cs_precedes
# define int_n_sign_posn     n_sign_posn
# define int_n_sep_by_space  n_sep_by_space
#endif

/* Bionic libc's 'struct lconv' is just a dummy.  */
#if @REPLACE_STRUCT_LCONV@
# if !defined GNULIB_defined_struct_lconv
#  define lconv rpl_lconv
struct lconv
{
  /* All 'char *' are actually 'const char *'.  */

  /* Members that depend on the LC_NUMERIC category of the locale.  See
     <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap07.html#tag_07_03_04> */

  /* Symbol used as decimal point.  */
  char *decimal_point;
  /* Symbol used to separate groups of digits to the left of the decimal
     point.  */
  char *thousands_sep;
  /* Definition of the size of groups of digits to the left of the decimal
     point.  */
  char *grouping;

  /* Members that depend on the LC_MONETARY category of the locale.  See
     <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap07.html#tag_07_03_03> */

  /* Symbol used as decimal point.  */
  char *mon_decimal_point;
  /* Symbol used to separate groups of digits to the left of the decimal
     point.  */
  char *mon_thousands_sep;
  /* Definition of the size of groups of digits to the left of the decimal
     point.  */
  char *mon_grouping;
  /* Sign used to indicate a value >= 0.  */
  char *positive_sign;
  /* Sign used to indicate a value < 0.  */
  char *negative_sign;

  /* For formatting local currency.  */
  /* Currency symbol (3 characters) followed by separator (1 character).  */
  char *currency_symbol;
  /* Number of digits after the decimal point.  */
  char frac_digits;
  /* For values >= 0: 1 if the currency symbol precedes the number, 0 if it
     comes after the number.  */
  char p_cs_precedes;
  /* For values >= 0: Position of the sign.  */
  char p_sign_posn;
  /* For values >= 0: Placement of spaces between currency symbol, sign, and
     number.  */
  char p_sep_by_space;
  /* For values < 0: 1 if the currency symbol precedes the number, 0 if it
     comes after the number.  */
  char n_cs_precedes;
  /* For values < 0: Position of the sign.  */
  char n_sign_posn;
  /* For values < 0: Placement of spaces between currency symbol, sign, and
     number.  */
  char n_sep_by_space;

  /* For formatting international currency.  */
  /* Currency symbol (3 characters) followed by separator (1 character).  */
  char *int_curr_symbol;
  /* Number of digits after the decimal point.  */
  char int_frac_digits;
  /* For values >= 0: 1 if the currency symbol precedes the number, 0 if it
     comes after the number.  */
  char int_p_cs_precedes;
  /* For values >= 0: Position of the sign.  */
  char int_p_sign_posn;
  /* For values >= 0: Placement of spaces between currency symbol, sign, and
     number.  */
  char int_p_sep_by_space;
  /* For values < 0: 1 if the currency symbol precedes the number, 0 if it
     comes after the number.  */
  char int_n_cs_precedes;
  /* For values < 0: Position of the sign.  */
  char int_n_sign_posn;
  /* For values < 0: Placement of spaces between currency symbol, sign, and
     number.  */
  char int_n_sep_by_space;
};
#  define GNULIB_defined_struct_lconv 1
# endif
#endif

#if @GNULIB_LOCALECONV@
/* The return type 'const struct lconv *' serves the purpose of producing
   warnings for invalid uses of the value returned from this function.  */
# if @REPLACE_LOCALECONV@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef localeconv
#   define localeconv rpl_localeconv
#   define GNULIB_defined_localeconv 1
#  endif
_GL_FUNCDECL_RPL (localeconv, const struct lconv *, (void), );
_GL_CXXALIAS_RPL (localeconv, const struct lconv *, (void));
# else
_GL_CXXALIAS_SYS_CAST (localeconv, const struct lconv *, (void));
#  if !defined localeconv && !defined __cplusplus
#   define localeconv() ((const struct lconv *) localeconv ())
#  endif
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (localeconv);
# endif
#elif @REPLACE_STRUCT_LCONV@
# if !GNULIB_LOCALECONV
#  undef localeconv
#  define localeconv localeconv_used_without_requesting_gnulib_module_localeconv
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_LOCALECONV
_GL_WARN_ON_USE (localeconv,
                 "localeconv returns too few information on some platforms - "
                 "use gnulib module localeconv for portability");
# endif
#endif

#if @GNULIB_SETLOCALE@
/* The return type 'const char *' serves the purpose of producing warnings
   for invalid uses of the value returned from this function.  */
# if @REPLACE_SETLOCALE@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef setlocale
#   define setlocale rpl_setlocale
#   define GNULIB_defined_setlocale 1
#  endif
_GL_FUNCDECL_RPL (setlocale, const char *,
                  (int category, const char *locale), );
_GL_CXXALIAS_RPL (setlocale, const char *,
                  (int category, const char *locale));
# else
_GL_CXXALIAS_SYS_CAST (setlocale, const char *,
                       (int category, const char *locale));
#  if !defined setlocale && !defined __cplusplus
#   define setlocale(...) ((const char *) setlocale (__VA_ARGS__))
#  endif
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (setlocale);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_SETLOCALE
_GL_WARN_ON_USE (setlocale, "setlocale works differently on native Windows - "
                 "use gnulib module setlocale for portability");
# endif
#endif

#if @GNULIB_SETLOCALE_NULL@
/* Included here for convenience.  */
# include "setlocale_null.h"
#endif

#if @GNULIB_NEWLOCALE@ || (@GNULIB_GETLOCALENAME_L_UNSAFE@ && @LOCALENAME_ENHANCE_LOCALE_FUNCS@ && @HAVE_NEWLOCALE@)
# if @REPLACE_NEWLOCALE@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef newlocale
#   define newlocale rpl_newlocale
#   define GNULIB_defined_newlocale 1
#  endif
_GL_FUNCDECL_RPL (newlocale, locale_t,
                  (int category_mask, const char *name, locale_t base),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (newlocale, locale_t,
                  (int category_mask, const char *name, locale_t base));
# else
#  if !@HAVE_NEWLOCALE@
_GL_FUNCDECL_SYS (newlocale, locale_t,
                  (int category_mask, const char *name, locale_t base),
                  _GL_ARG_NONNULL ((2)));
#  endif
_GL_CXXALIAS_SYS (newlocale, locale_t,
                  (int category_mask, const char *name, locale_t base));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (newlocale);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_NEWLOCALE
_GL_WARN_ON_USE (newlocale, "newlocale is not portable");
# endif
#endif

#if @GNULIB_DUPLOCALE@ || (@GNULIB_GETLOCALENAME_L_UNSAFE@ && @LOCALENAME_ENHANCE_LOCALE_FUNCS@ && @HAVE_DUPLOCALE@)
# if @REPLACE_DUPLOCALE@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef duplocale
#   define duplocale rpl_duplocale
#   define GNULIB_defined_duplocale 1
#  endif
_GL_FUNCDECL_RPL (duplocale, locale_t, (locale_t locale), _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (duplocale, locale_t, (locale_t locale));
# else
#  if !@HAVE_DUPLOCALE@
_GL_FUNCDECL_SYS (duplocale, locale_t, (locale_t locale), _GL_ARG_NONNULL ((1)));
#  endif
_GL_CXXALIAS_SYS (duplocale, locale_t, (locale_t locale));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (duplocale);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_DUPLOCALE
_GL_WARN_ON_USE (duplocale, "duplocale is unportable and buggy on some glibc systems - "
                 "use gnulib module duplocale for portability");
# endif
#endif

#if @GNULIB_FREELOCALE@ || (@GNULIB_GETLOCALENAME_L_UNSAFE@ && @LOCALENAME_ENHANCE_LOCALE_FUNCS@ && @HAVE_FREELOCALE@)
# if @REPLACE_FREELOCALE@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef freelocale
#   define freelocale rpl_freelocale
#   define GNULIB_defined_freelocale 1
#  endif
_GL_FUNCDECL_RPL (freelocale, void, (locale_t locale), _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (freelocale, void, (locale_t locale));
# else
#  if !@HAVE_FREELOCALE@
_GL_FUNCDECL_SYS (freelocale, void, (locale_t locale), _GL_ARG_NONNULL ((1)));
#  endif
/* Need to cast, because on FreeBSD and Mac OS X 10.13, the return type is
                                   int.  */
_GL_CXXALIAS_SYS_CAST (freelocale, void, (locale_t locale));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (freelocale);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_FREELOCALE
_GL_WARN_ON_USE (freelocale, "freelocale is not portable");
# endif
#endif

#if @GNULIB_GETLOCALENAME_L@
# if @REPLACE_GETLOCALENAME_L@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef getlocalename_l
#   define getlocalename_l rpl_getlocalename_l
#  endif
_GL_FUNCDECL_RPL (getlocalename_l, const char *,
                  (int category, locale_t locale),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (getlocalename_l, const char *,
                  (int category, locale_t locale));
# else
#  if !@HAVE_GETLOCALENAME_L@
_GL_FUNCDECL_SYS (getlocalename_l, const char *,
                  (int category, locale_t locale),
                  _GL_ARG_NONNULL ((2)));
#  endif
_GL_CXXALIAS_SYS (getlocalename_l, const char *,
                  (int category, locale_t locale));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (getlocalename_l);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_GETLOCALENAME_L
_GL_WARN_ON_USE (getlocalename_l, "getlocalename_l is not portable");
# endif
#endif

#endif /* _@GUARD_PREFIX@_LOCALE_H */
#endif /* _@GUARD_PREFIX@_LOCALE_H */
#endif /* !(__need_locale_t || _@GUARD_PREFIX@_ALREADY_INCLUDING_LOCALE_H) */
