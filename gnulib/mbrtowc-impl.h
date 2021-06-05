/* Convert multibyte character to wide character.
   Copyright (C) 1999-2002, 2005-2021 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2008.  */

/* This file contains the body of the mbrtowc and mbrtoc32 functions,
   when GNULIB_defined_mbstate_t is defined.  */

  char *pstate = (char *)ps;

  if (s == NULL)
    {
      pwc = NULL;
      s = "";
      n = 1;
    }

  if (n == 0)
    return (size_t)(-2);

  /* Here n > 0.  */

  if (pstate == NULL)
    pstate = internal_state;

  {
    size_t nstate = pstate[0];
    char buf[4];
    const char *p;
    size_t m;
    enc_t enc;
    int res;

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

    enc = locale_encoding_classification ();

    if (enc == enc_utf8) /* UTF-8 */
      {
        /* Achieve
             - multi-thread safety and
             - the ability to produce wide character values > WCHAR_MAX
           by not calling mbtowc() at all.  */
#include "mbrtowc-impl-utf8.h"
      }
    else
      {
        /* The hidden internal state of mbtowc would make this function not
           multi-thread safe.  Achieve multi-thread safety through a lock.  */
        wchar_t wc;
        res = mbtowc_with_lock (&wc, p, m);

        if (res >= 0)
          {
            if ((wc == 0) != (res == 0))
              abort ();
            if (pwc != NULL)
              *pwc = wc;
            goto success;
          }

        /* mbtowc does not distinguish between invalid and incomplete multibyte
           sequences.  But mbrtowc needs to make this distinction.
           There are two possible approaches:
             - Use iconv() and its return value.
             - Use built-in knowledge about the possible encodings.
           Given the low quality of implementation of iconv() on the systems
           that lack mbrtowc(), we use the second approach.
           The possible encodings are:
             - 8-bit encodings,
             - EUC-JP, EUC-KR, GB2312, EUC-TW, BIG5, GB18030, SJIS,
             - UTF-8 (already handled above).
           Use specialized code for each.  */
        if (m >= 4 || m >= MB_CUR_MAX)
          goto invalid;
        /* Here MB_CUR_MAX > 1 and 0 < m < 4.  */
        switch (enc)
          {
          /* As a reference for this code, you can use the GNU libiconv
             implementation.  Look for uses of the RET_TOOFEW macro.  */

          case enc_eucjp: /* EUC-JP */
            {
              if (m == 1)
                {
                  unsigned char c = (unsigned char) p[0];

                  if ((c >= 0xa1 && c < 0xff) || c == 0x8e || c == 0x8f)
                    goto incomplete;
                }
              if (m == 2)
                {
                  unsigned char c = (unsigned char) p[0];

                  if (c == 0x8f)
                    {
                      unsigned char c2 = (unsigned char) p[1];

                      if (c2 >= 0xa1 && c2 < 0xff)
                        goto incomplete;
                    }
                }
              goto invalid;
            }

          case enc_94: /* EUC-KR, GB2312, BIG5 */
            {
              if (m == 1)
                {
                  unsigned char c = (unsigned char) p[0];

                  if (c >= 0xa1 && c < 0xff)
                    goto incomplete;
                }
              goto invalid;
            }

          case enc_euctw: /* EUC-TW */
            {
              if (m == 1)
                {
                  unsigned char c = (unsigned char) p[0];

                  if ((c >= 0xa1 && c < 0xff) || c == 0x8e)
                    goto incomplete;
                }
              else /* m == 2 || m == 3 */
                {
                  unsigned char c = (unsigned char) p[0];

                  if (c == 0x8e)
                    goto incomplete;
                }
              goto invalid;
            }

          case enc_gb18030: /* GB18030 */
            {
              if (m == 1)
                {
                  unsigned char c = (unsigned char) p[0];

                  if ((c >= 0x90 && c <= 0xe3) || (c >= 0xf8 && c <= 0xfe))
                    goto incomplete;
                }
              else /* m == 2 || m == 3 */
                {
                  unsigned char c = (unsigned char) p[0];

                  if (c >= 0x90 && c <= 0xe3)
                    {
                      unsigned char c2 = (unsigned char) p[1];

                      if (c2 >= 0x30 && c2 <= 0x39)
                        {
                          if (m == 2)
                            goto incomplete;
                          else /* m == 3 */
                            {
                              unsigned char c3 = (unsigned char) p[2];

                              if (c3 >= 0x81 && c3 <= 0xfe)
                                goto incomplete;
                            }
                        }
                    }
                }
              goto invalid;
            }

          case enc_sjis: /* SJIS */
            {
              if (m == 1)
                {
                  unsigned char c = (unsigned char) p[0];

                  if ((c >= 0x81 && c <= 0x9f) || (c >= 0xe0 && c <= 0xea)
                      || (c >= 0xf0 && c <= 0xf9))
                    goto incomplete;
                }
              goto invalid;
            }

          default:
            /* An unknown multibyte encoding.  */
            goto incomplete;
          }
      }

   success:
    /* res >= 0 is the corrected return value of
       mbtowc_with_lock (&wc, p, m).  */
    if (nstate >= (res > 0 ? res : 1))
      abort ();
    res -= nstate;
    pstate[0] = 0;
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
