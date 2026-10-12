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

#include <uchar.h>

#ifdef __cplusplus
extern "C" {
#endif


struct localeinfo
{
  /* MB_CUR_MAX > 1.  */
  bool multibyte;

  /* The locale is simple, like the C locale.  These locales can be
     processed more efficiently, as they are single-byte, their native
     character set is in collating-sequence order, and they do not
     have multi-character collating elements.  */
  bool simple;

  /* The locale uses UTF-8.  */
  bool using_utf8;

  /* An array indexed by byte values B that contains 1 if B is a
     single-byte character, -1 if B is an encoding error, and -2 if B
     is the leading byte of a multibyte character that contains more
     than one byte.  */
  signed char sbclen[(unsigned char) -1 + 1];

  /* An array indexed by byte values B.  If sbclen[B] == 1,
     this is the corresponding char32_t value.  Otherwise it is WEOF, as
     the byte is not a valid single-byte character, i.e., sbclen[B] == -1
     or -2.  */
  wint_t sbctowc[(unsigned char) -1 + 1];
};

extern void init_localeinfo (struct localeinfo *);

/* Maximum number of characters that can be the case-folded
   counterparts of a single character, not counting the character itself.
   Subtract from 256 one for U+0000.  This is a generous upper bound.  */
enum { CASE_FOLDED_BUFSIZE = (unsigned char) -1 };

extern int case_folded_counterparts (wint_t, char32_t[CASE_FOLDED_BUFSIZE]);


#ifdef __cplusplus
}
#endif
