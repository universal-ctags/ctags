/* Convert multibyte character to wide character.
   Copyright (C) 1999-2002, 2005-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2008.

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

#include <config.h>

/* Specification.  */
#include <wchar.h>

#if GNULIB_defined_mbstate_t
/* Implement mbrtowc() on top of mbtowc() for the non-UTF-8 locales
   and directly for the UTF-8 locales.  */

# include <errno.h>
# include <stdint.h>
# include <stdlib.h>

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

# include "attribute.h"
# include "lc-charset-dispatch.h"
# include "mbtowc-lock.h"

static_assert (sizeof (mbstate_t) >= 4);
static char internal_state[4];

size_t
mbrtowc (wchar_t *pwc, const char *s, size_t n, mbstate_t *ps)
{
# define FITS_IN_CHAR_TYPE(wc)  ((wc) <= WCHAR_MAX)
# include "mbrtowc-impl.h"
}

#else
/* Override the system's mbrtowc() function.  */

# include <errno.h>
# include <stdlib.h>

# include "attribute.h"
# include "localcharset.h"
# include "streq-opt.h"

# if MBRTOWC_IN_C_LOCALE_MAYBE_EILSEQ
#  include "hard-locale.h"
#  include <locale.h>
# endif

# if MBRTOWC_INVALID_UTF8_BUG || (GNULIB_WCHAR_SINGLE_LOCALE && __GLIBC__ >= 2 && !__UCLIBC__)

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

# undef mbrtowc

size_t
rpl_mbrtowc (wchar_t *pwc, const char *s, size_t n, mbstate_t *ps)
{
  /* It's simpler to handle the case s == NULL upfront, than to worry about
     this case later, before every test of pwc and n.  */
  if (s == NULL)
    {
      pwc = NULL;
      s = "";
      n = 1;
    }

# if (MBRTOWC_EMPTY_INPUT_BUG || MBRTOWC_INVALID_UTF8_BUG \
      || (GNULIB_WCHAR_SINGLE_LOCALE && __GLIBC__ >= 2 && !__UCLIBC__))
  if (n == 0)
    return (size_t) -2;
# endif

# if MBRTOWC_INVALID_UTF8_BUG || (GNULIB_WCHAR_SINGLE_LOCALE && __GLIBC__ >= 2 && !__UCLIBC__)
  /* Optimize the frequent case of an UTF-8 locale.
     Since here we are in the !GNULIB_defined_mbstate_t case, i.e. we use
     the system's mbstate_t type and have to provide interoperability with
     the system's mbsinit() function, this requires knowledge about how the
     system's UTF-8 mbrtowc() function stores the state.  This knowledge is
     platform-specific.  For simplicity, we handle only glibc and NetBSD
     systems.  */
  if (is_locale_utf8_cached ())
    {
      static mbstate_t internal_state;
      if (ps == NULL)
        ps = &internal_state;
      #if __GLIBC__ >= 2
      /* Structure of mbstate_t =
         { int __count; union { wint_t __wch; char __wchb[4]; } __value; }
         (see glibc/iconv/gconv_simple.c function utf8_internal_loop):
         Bits 2..0 of __count is the number of input bytes already consumed.
         Bits 31..8 of __count is the number of input bytes expected for the
         entire byte sequence.
         __value.__wch is the already inferrable bits of the character, of
         the form (x << (r*6)) when r bytes are still expected.  */
      #endif
      #ifdef __NetBSD__
      /* Structure of mbstate_t =
         union { int64_t __mbstateL; char __mbstate8[128]; }
         (see src/lib/libc/citrus/modules/citrus_utf8.c):
         { void *header; char ch[6]; int chlen; },
         i.e. ch[0..5] is __mbstate8[sizeof(void*)+0..sizeof(void*)+5],
              chlen is __mbstate8[sizeof(void*)+8..sizeof(void*)+11].  */
      #endif

      /* Here n > 0.  */

      size_t nstate;
      #if __GLIBC__ >= 2
      nstate = ps->__count & 7;
      #endif
      #ifdef __NetBSD__
      nstate = *(int *) &ps->__mbstate8[sizeof (void *) + 8];
      #endif
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
          #if __GLIBC__ >= 2
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
          #endif
          #ifdef __NetBSD__
          buf[0] = ps->__mbstate8[sizeof (void *) + 0];
          if (nstate >= 2)
            {
              buf[1] = ps->__mbstate8[sizeof (void *) + 1];
              if (nstate >= 3)
                {
                  buf[2] = ps->__mbstate8[sizeof (void *) + 2];
                }
            }
          #endif
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
#  define FITS_IN_CHAR_TYPE(wc)  ((wc) <= WCHAR_MAX)
#  include "mbrtowc-impl-utf8.h"
      }

     success:
      /* res >= 0 is the corrected return value of
         mbtowc_with_lock (&wc, p, m).  */
      if (nstate >= (res > 0 ? res : 1))
        abort ();
      res -= nstate;
      #if __GLIBC__ >= 2
      ps->__count = 0;
      #endif
      #ifdef __NetBSD__
      *(int *) &ps->__mbstate8[sizeof (void *) + 8] = 0;
      #endif
      return res;

     incomplete:
      /* Here 0 < m < 4.  */
      {
        #if __GLIBC__ >= 2
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
        #endif
        #ifdef __NetBSD__
        *(int *) &ps->__mbstate8[sizeof (void *) + 8] = m;
        ps->__mbstate8[sizeof (void *) + 0] = p[0];
        if (m > 1)
          {
            ps->__mbstate8[sizeof (void *) + 1] = p[1];
            if (m > 2)
              {
                ps->__mbstate8[sizeof (void *) + 2] = p[2];
              }
          }
        #endif
      }
      return (size_t)(-2);

     invalid:
      errno = EILSEQ;
      /* The conversion state is undefined, says POSIX.  */
      return (size_t)(-1);
    }
# endif

  wchar_t wc;
  if (! pwc)
    pwc = &wc;

# if MBRTOWC_RETVAL_BUG
  {
    static mbstate_t internal_state;

    /* Override mbrtowc's internal state.  We cannot call mbsinit() on the
       hidden internal state, but we can call it on our variable.  */
    if (ps == NULL)
      ps = &internal_state;

    if (!mbsinit (ps))
      {
        /* Parse the rest of the multibyte character byte for byte.  */
        size_t count = 0;
        for (; n > 0; s++, n--)
          {
            size_t ret = mbrtowc (&wc, s, 1, ps);

            if (ret == (size_t)(-1))
              return (size_t)(-1);
            count++;
            if (ret != (size_t)(-2))
              {
                /* The multibyte character has been completed.  */
                *pwc = wc;
                return (wc == 0 ? 0 : count);
              }
          }
        return (size_t)(-2);
      }
  }
# endif

  size_t ret;
# if MBRTOWC_STORES_INCOMPLETE_BUG
  ret = mbrtowc (&wc, s, n, ps);
  if (ret < (size_t) -2 && pwc != NULL)
    *pwc = wc;
# else
  ret = mbrtowc (pwc, s, n, ps);
# endif

# if MBRTOWC_NUL_RETVAL_BUG
  if (ret < (size_t) -2 && !*pwc)
    return 0;
# endif

# if MBRTOWC_IN_C_LOCALE_MAYBE_EILSEQ
  if ((size_t) -2 <= ret && n != 0 && ! hard_locale (LC_CTYPE))
    {
      unsigned char uc = *s;
      *pwc = uc;
      return 1;
    }
# endif

  return ret;
}

#endif
