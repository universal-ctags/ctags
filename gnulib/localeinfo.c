/* locale information

   Copyright 2016-2026 Free Software Foundation, Inc.

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

/* Written by Paul Eggert.  */

#include <config.h>

#include <localeinfo.h>

#include <verify.h>

#include <limits.h>
#include <locale.h>
#include <stdcountof.h>
#include <stdlib.h>
#include <string.h>
#include <uchar.h>
#include <wchar.h>

/* The sbclen implementation relies on this.  */
verify (MB_LEN_MAX <= SCHAR_MAX);

/* Return true if the locale uses UTF-8.  */

static bool
is_using_utf8 (void)
{
  mbstate_t state; mbszero (&state);
  char32_t wc;
  return mbrtoc32 (&wc, "\xc4\x80", 2, &state) == 2 && wc == 0x100;
}

/* Return true if the locale is compatible enough with the C locale so
   that the locale is single-byte, bytes are in collating-sequence
   order, and there are no multi-character collating elements.  */

static bool
using_simple_locale (bool multibyte)
{
  /* The native character set is known to be compatible with
     the C locale.  The following test isn't perfect, but it's good
     enough in practice, as only ASCII and EBCDIC are in common use
     and this test correctly accepts ASCII and rejects EBCDIC.  */
  enum { native_c_charset =
    ('\b' == 8 && '\t' == 9 && '\n' == 10 && '\v' == 11 && '\f' == 12
     && '\r' == 13 && ' ' == 32 && '!' == 33 && '"' == 34 && '#' == 35
     && '%' == 37 && '&' == 38 && '\'' == 39 && '(' == 40 && ')' == 41
     && '*' == 42 && '+' == 43 && ',' == 44 && '-' == 45 && '.' == 46
     && '/' == 47 && '0' == 48 && '9' == 57 && ':' == 58 && ';' == 59
     && '<' == 60 && '=' == 61 && '>' == 62 && '?' == 63 && 'A' == 65
     && 'Z' == 90 && '[' == 91 && '\\' == 92 && ']' == 93 && '^' == 94
     && '_' == 95 && 'a' == 97 && 'z' == 122 && '{' == 123 && '|' == 124
     && '}' == 125 && '~' == 126)
  };

  if (!native_c_charset || multibyte)
    return false;

  /* As a heuristic, use strcoll to compare native character order.
     If this agrees with byte order the locale should be simple.
     This heuristic should work for all known practical locales,
     although it would be invalid for artificially-constructed locales
     where the native order is the collating-sequence order but there
     are multi-character collating elements.  */
  for (int i = 0; i < UCHAR_MAX; i++)
    if (0 <= strcoll (((char []) {i, 0}), ((char []) {i + 1, 0})))
      return false;

  return true;
}

/* Initialize *LOCALEINFO from the current locale.  */

void
init_localeinfo (struct localeinfo *localeinfo)
{
  localeinfo->multibyte = MB_CUR_MAX > 1;
  localeinfo->simple = using_simple_locale (localeinfo->multibyte);
  localeinfo->using_utf8 = is_using_utf8 ();

  for (int i = CHAR_MIN; i <= CHAR_MAX; i++)
    {
      char c = i;
      unsigned char uc = i;
      mbstate_t state; mbszero (&state);
      char32_t wc;
      size_t len = mbrtoc32 (&wc, &c, 1, &state);
      localeinfo->sbclen[uc] = len <= 1 ? 1 : - (int) - len;
      localeinfo->sbctowc[uc] = len <= 1 ? wc : WEOF;
    }
}

/* The set of char32_t values C such that there's a useful locale
   somewhere where C != c32toupper (C) && C != c32tolower (c32toupper (C)).
   For example, 0x00B5 (U+00B5 MICRO SIGN) is in this table, because
   c32toupper (0x00B5) == 0x039C (U+039C GREEK CAPITAL LETTER MU), and
   c32tolower (0x039C) == 0x03BC (U+03BC GREEK SMALL LETTER MU).  */
static unsigned short int const lonesome_lower[] =
  {
    0x00B5, 0x0131, 0x017F, 0x01C5, 0x01C8, 0x01CB, 0x01F2, 0x0345,
    0x03C2, 0x03D0, 0x03D1, 0x03D5, 0x03D6, 0x03F0, 0x03F1,

#if !defined __STDC_ISO_10646__ || __STDC_ISO_10646__ < 200305L
    /* U+03F2 GREEK LUNATE SIGMA SYMBOL lacks a specific uppercase
       counterpart in locales predating Unicode 4.0.0 (April 2003).  */
    0x03F2,
#endif

    0x03F5, 0x1C80, 0x1C81, 0x1C82, 0x1C83, 0x1C84, 0x1C85, 0x1C86,
    0x1C87, 0x1C88, 0x1E9B, 0x1FBE,
  };

/* Verify that the worst multibyte case fits.  This is 1 for c32toupper, 1 for
   c32tolower, and 1 for each entry in LONESOME_LOWER.  */
verify (1 + 1 + countof (lonesome_lower) <= CASE_FOLDED_BUFSIZE);

/* Whether char32_t values are Unicode code points.
   It is OK if only UTF-16 is supported,
   since this file converts only single-byte encodings to char32_t
   and in practice these encodings convert to characters in the BMP.  */
#ifdef GL_CHAR32_T_IS_UNICODE
# define CHAR32_T_IS_UNICODE GL_CHAR32_T_IS_UNICODE /* uchar-h-c23 */
#elif defined __STDC_ISO_10646__
# define CHAR32_T_IS_UNICODE 1 /* glibc, musl libc, Cygwin */
#else
# define CHAR32_T_IS_UNICODE 0
#endif

/* Find the characters equal to C after case-folding, other than C
   itself, and store them into FOLDED.  Return the number of characters
   stored; this is zero if C is WEOF.  */

int
case_folded_counterparts (wint_t c, char32_t folded[CASE_FOLDED_BUFSIZE])
{
  int n = 0;
  wint_t uc = c32toupper (c);

  if (CHAR32_T_IS_UNICODE || 1 < MB_CUR_MAX)
    {
      /* char32_t is Unicode, or this is a multibyte locale where
         it is impractical to look for all case-folded counterparts
         and where guessing Unicode will not produce false positives
         though it may miss some case-folded counterparts.  */
      wint_t lc = c32tolower (uc);
      if (uc != c)
        folded[n++] = uc;
      if (lc != uc && lc != c && c32toupper (lc) == uc)
        folded[n++] = lc;
      for (int i = 0; i < countof (lonesome_lower); i++)
        {
          wint_t li = lonesome_lower[i];
          if (li != lc && li != uc && li != c && c32toupper (li) == uc)
            folded[n++] = li;
        }
    }
  else if (c != WEOF)
    {
      /* A single-byte locale where it is not known that char32_t is Unicode,
         and C is not WEOF.  Check all 255 possibilities for counterparts.  */
        for (int i = 1; i <= UCHAR_MAX; i++)
          {
            wint_t li = btoc32 (i);
            if (li != c && c32toupper (li) == uc)
              folded[n++] = li;
          }
    }

  return n;
}
