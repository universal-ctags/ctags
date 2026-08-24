/* Convert multibyte character to 32-bit wide character.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2020.  */

#include <config.h>

/* Specification.  */
#include <uchar.h>

#include "attribute.h"

#include <errno.h>
#include <stdlib.h>

#if GL_CHAR32_T_IS_UNICODE
# include "lc-charset-unicode.h"
#endif

#if GNULIB_defined_mbstate_t /* AIX */
/* Implement mbrtoc32() on top of mbtowc() for the non-UTF-8 locales
   and directly for the UTF-8 locales.  */

/* Note: On AIX (64-bit) we can implement mbrtoc32 in two equivalent ways:
   - in a way that parallels the override of mbrtowc; this is the code branch
     here;
   - in a way that invokes the overridden mbrtowc; this would be the #else
     branch below.
   They are equivalent.  */

# if AVOID_ANY_THREADS

/* The option '--disable-threads' explicitly requests no locking.  */

# elif defined _WIN32 && !defined __CYGWIN__

#  define WIN32_LEAN_AND_MEAN  /* avoid including junk */
#  include <windows.h>

# elif HAVE_PTHREAD_API

#  include <pthread.h>
#  if HAVE_THREADS_H && HAVE_WEAK_SYMBOLS
#   include <threads.h>
#   pragma weak thrd_exit
#   define c11_threads_in_use() (thrd_exit != NULL)
#  else
#   define c11_threads_in_use() 0
#  endif

# elif HAVE_THREADS_H

#  include <threads.h>

# endif

# include "lc-charset-dispatch.h"
# include "mbtowc-lock.h"

static_assert (sizeof (mbstate_t) >= 4);
static char internal_state[4];

size_t
mbrtoc32 (char32_t *pwc, const char *s, size_t n, mbstate_t *ps)
{
# define FITS_IN_CHAR_TYPE(wc)  1
# include "mbrtowc-impl.h"
}

#else /* glibc, macOS, FreeBSD, NetBSD, OpenBSD, HP-UX, Solaris, Cygwin, mingw, MSVC, Minix, Android */

/* Implement mbrtoc32() based on the original mbrtoc32() or on mbrtowc().  */

# include <errno.h>
# include <stdlib.h>
# include <wchar.h>

# include "attribute.h"
# include "localcharset.h"
# include "streq-opt.h"

# if MBRTOC32_IN_C_LOCALE_MAYBE_EILSEQ
#  include "hard-locale.h"
#  include <locale.h>
# endif

# if (GNULIB_WCHAR_SINGLE_LOCALE && __GLIBC__ >= 2 && !__UCLIBC__)

/* Returns 1 if the current locale is an UTF-8 locale, 0 otherwise.  */
static inline int
is_locale_utf8 (void)
{
  const char *encoding = locale_charset ();
  return STREQ_OPT (encoding, "UTF-8", 'U', 'T', 'F', '-', '8', 0, 0, 0, 0);
}

/* Provide a speedup by caching the value of is_locale_utf8.  */
static int cached_is_locale_utf8 = -1;
static inline int
is_locale_utf8_cached (void)
{
  if (cached_is_locale_utf8 < 0)
    cached_is_locale_utf8 = is_locale_utf8 ();
  return cached_is_locale_utf8;
}

# endif

static mbstate_t internal_state;

size_t
mbrtoc32 (char32_t *pwc, const char *s, size_t n, mbstate_t *ps)
# undef mbrtoc32
{
  /* It's simpler to handle the case s == NULL upfront, than to worry about
     this case later, before every test of pwc and n.  */
  if (s == NULL)
    {
      pwc = NULL;
      s = "";
      n = 1;
    }

# if (MBRTOC32_EMPTY_INPUT_BUG || _GL_SMALL_WCHAR_T \
      || (GNULIB_WCHAR_SINGLE_LOCALE && __GLIBC__ >= 2 && !__UCLIBC__))
  if (n == 0)
    return (size_t) -2;
# endif

  if (ps == NULL)
    ps = &internal_state;

# if (GNULIB_WCHAR_SINGLE_LOCALE && __GLIBC__ >= 2 && !__UCLIBC__)
  /* Optimize the frequent case of an UTF-8 locale.
     Since here we are in the !GNULIB_defined_mbstate_t case, i.e. we use
     the system's mbstate_t type and have to provide interoperability with
     the system's mbsinit() function, this requires knowledge about how the
     system's UTF-8 mbrtoc32() function stores the state.  This knowledge is
     platform-specific.  For simplicity, we handle only glibc systems.  */
  if (is_locale_utf8_cached ())
    {
      /* Structure of mbstate_t =
         { int __count; union { wint_t __wch; char __wchb[4]; } __value; }
         (see glibc/iconv/gconv_simple.c function utf8_internal_loop):
         Bits 2..0 of __count is the number of input bytes already consumed.
         Bits 31..8 of __count is the number of input bytes expected for the
         entire byte sequence.
         __value.__wch is the already inferrable bits of the character, of
         the form (x << (r*6)) when r bytes are still expected.  */

      /* Here n > 0.  */

      size_t nstate = ps->__count & 7;
      char buf[4];
      const char *p;
      size_t m;

      if (nstate == 0)
        {
          p = s;
          m = n;
        }
      else
        {
          size_t t = ps->__count >> 8; /* total expected number of bytes */
          if (t > nstate && t <= 4)
            {
              buf[0] =
                (0x100 - (0x100 >> t)) | (ps->__value.__wch >> ((t - 1) * 6));
              if (nstate >= 2)
                {
                  buf[1] =
                    0x80 | ((ps->__value.__wch >> ((t - 2) * 6)) & 0x3F);
                  if (nstate >= 3)
                    {
                      buf[2] =
                        0x80 | ((ps->__value.__wch >> ((t - 3) * 6)) & 0x3F);
                    }
                }
            }
          else
            {
              errno = EINVAL;
              return (size_t)(-1);
            }
          p = buf;
          m = nstate;
          buf[m++] = s[0];
          if (n >= 2 && m < 4)
            {
              buf[m++] = s[1];
              if (n >= 3 && m < 4)
                buf[m++] = s[2];
            }
        }

      /* Here m > 0.  */

      int res;
      {
#  define FITS_IN_CHAR_TYPE(wc)  1
#  include "mbrtowc-impl-utf8.h"
      }

     success:
      /* res >= 0 is the corrected return value of
         mbtowc_with_lock (&wc, p, m).  */
      if (nstate >= (res > 0 ? res : 1))
        abort ();
      res -= nstate;
      ps->__count = 0;
      return res;

     incomplete:
      /* Here 0 < m < 4.  */
      {
        unsigned char c = (unsigned char) p[0];
        if (c < 0xE0)
          {
            ps->__count = (2 << 8) | m;
            ps->__value.__wch = (c & 0x1F) << 6;
          }
        else if (c < 0xF0)
          {
            ps->__count = (3 << 8) | m;
            ps->__value.__wch =
              ((c & 0x0F) << 12)
              | (m > 1 ? ((unsigned char) p[1] & 0x3F) << 6 : 0);
          }
        else
          {
            ps->__count = (4 << 8) | m;
            ps->__value.__wch =
              ((c & 0x07) << 18)
              | (m > 1 ? ((unsigned char) p[1] & 0x3F) << 12 : 0)
              | (m > 2 ? ((unsigned char) p[2] & 0x3F) << 6 : 0);
          }
      }
      return (size_t)(-2);

     invalid:
      errno = EILSEQ;
      /* The conversion state is undefined, says POSIX.  */
      return (size_t)(-1);
    }
# endif

# if HAVE_WORKING_MBRTOC32 && HAVE_WORKING_C32RTOMB && !MBRTOC32_MULTIBYTE_LOCALE_BUG
  /* mbrtoc32() may produce different values for wc than mbrtowc().  Therefore
     use mbrtoc32().  */

#  if defined _WIN32 && !defined __CYGWIN__
  char32_t wc;
  size_t ret = mbrtoc32 (&wc, s, n, ps);
  if (ret < (size_t) -2 && pwc != NULL)
    *pwc = wc;
#  else
  size_t ret = mbrtoc32 (pwc, s, n, ps);
#  endif

#  if GNULIB_MBRTOC32_REGULAR
  /* Verify that mbrtoc32 is regular.  */
  if (ret < (size_t) -3 && ! mbsinit (ps))
    /* This occurs on glibc 2.36.  */
    mbszero (ps);
  if (ret == (size_t) -3)
    abort ();
#  endif

#  if MBRTOC32_IN_C_LOCALE_MAYBE_EILSEQ
  if ((size_t) -2 <= ret && n != 0 && ! hard_locale (LC_CTYPE))
    {
      if (pwc != NULL)
        *pwc = (unsigned char) *s;
      return 1;
    }
#  endif

  return ret;

# elif _GL_SMALL_WCHAR_T

  /* Special-case all encodings that may produce wide character values
     > WCHAR_MAX.  */
  const char *encoding = locale_charset ();
  if (STREQ_OPT (encoding, "UTF-8", 'U', 'T', 'F', '-', '8', 0, 0, 0, 0))
    {
      /* Special-case the UTF-8 encoding.  Assume that the wide-character
         encoding in a UTF-8 locale is UCS-2 or, equivalently, UTF-16.  */
      /* Here n > 0.  */
      char *pstate = (char *)ps;
      size_t nstate = pstate[0];
      char buf[4];
      const char *p;
      size_t m;

      switch (nstate)
        {
        case 0:
          p = s;
          m = n;
          break;
        case 3:
          buf[2] = pstate[3];
          FALLTHROUGH;
        case 2:
          buf[1] = pstate[2];
          FALLTHROUGH;
        case 1:
          buf[0] = pstate[1];
          p = buf;
          m = nstate;
          buf[m++] = s[0];
          if (n >= 2 && m < 4)
            {
              buf[m++] = s[1];
              if (n >= 3 && m < 4)
                buf[m++] = s[2];
            }
          break;
        default:
          errno = EINVAL;
          return (size_t)(-1);
        }

      /* Here m > 0.  */

      int res;
      {
#  define FITS_IN_CHAR_TYPE(wc)  1
#  include "mbrtowc-impl-utf8.h"
      }

     success:
      if (nstate >= (res > 0 ? res : 1))
        abort ();
      res -= nstate;
      /* Set *ps to an initial state.  */
#  if defined _WIN32 && !defined __CYGWIN__
      /* Native Windows.  */
      /* MSVC defines 'mbstate_t' as an 8-byte struct; the first 4 bytes matter.
         On mingw, 'mbstate_t' is sometimes defined as 'int', sometimes defined
         as an 8-byte struct, of which the first 4 bytes matter.  */
      *(unsigned int *)pstate = 0;
#  elif defined __CYGWIN__
      /* Cygwin defines 'mbstate_t' as an 8-byte struct; the first 4 bytes
         matter.  */
      ps->__count = 0;
#  else
      pstate[0] = 0;
#  endif
      return res;

     incomplete:
      {
        size_t k = nstate;
        /* Here 0 <= k < m < 4.  */
        pstate[++k] = s[0];
        if (k < m)
          {
            pstate[++k] = s[1];
            if (k < m)
              pstate[++k] = s[2];
          }
        if (k != m)
          abort ();
      }
      pstate[0] = m;
      return (size_t)(-2);

     invalid:
      errno = EILSEQ;
      /* The conversion state is undefined, says POSIX.  */
      return (size_t)(-1);
    }
  else
    {
      wchar_t wc;
      size_t ret = mbrtowc (&wc, s, n, ps);
      if (ret < (size_t) -2 && pwc != NULL)
        *pwc = wc;
      return ret;
    }

# else

  /* char32_t and wchar_t are equivalent.  Use mbrtowc().  */
  wchar_t wc;
  size_t ret = mbrtowc (&wc, s, n, ps);

#  if GNULIB_MBRTOC32_REGULAR
  /* Ensure that mbrtoc32 is regular.  */
  if (ret < (size_t) -2 && ! mbsinit (ps))
    /* This occurs on glibc 2.12.  */
    mbszero (ps);
#  endif

#  if GL_CHAR32_T_IS_UNICODE && GL_CHAR32_T_VS_WCHAR_T_NEEDS_CONVERSION
  if (ret < (size_t) -2 && wc != 0)
    {
      wc = locale_encoding_to_unicode (wc);
      if (wc == 0)
        {
          ret = (size_t) -1;
          errno = EILSEQ;
        }
    }
#  endif
  if (ret < (size_t) -2 && pwc != NULL)
    *pwc = wc;
  return ret;

# endif
}

#endif
