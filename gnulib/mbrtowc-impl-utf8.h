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

/* This file contains the part of the body of the mbrtowc and mbrtoc32 functions
   that handles the special case of the UTF-8 encoding.  */

        /* Cf. unistr/u8-mbtouc.c.  */
        unsigned char c = (unsigned char) p[0];

        if (c < 0x80)
          {
            if (pwc != NULL)
              *pwc = c;
            res = (c == 0 ? 0 : 1);
            goto success;
          }
        if (c >= 0xc2)
          {
            if (c < 0xe0)
              {
                if (m == 1)
                  goto incomplete;
                else /* m >= 2 */
                  {
                    unsigned char c2 = (unsigned char) p[1];

                    if ((c2 ^ 0x80) < 0x40)
                      {
                        if (pwc != NULL)
                          *pwc = ((unsigned int) (c & 0x1f) << 6)
                                 | (unsigned int) (c2 ^ 0x80);
                        res = 2;
                        goto success;
                      }
                  }
              }
            else if (c < 0xf0)
              {
                if (m == 1)
                  goto incomplete;
                else
                  {
                    unsigned char c2 = (unsigned char) p[1];

                    if ((c2 ^ 0x80) < 0x40
                        && (c >= 0xe1 || c2 >= 0xa0)
                        && (c != 0xed || c2 < 0xa0))
                      {
                        if (m == 2)
                          goto incomplete;
                        else /* m >= 3 */
                          {
                            unsigned char c3 = (unsigned char) p[2];

                            if ((c3 ^ 0x80) < 0x40)
                              {
                                unsigned int wc =
                                  (((unsigned int) (c & 0x0f) << 12)
                                   | ((unsigned int) (c2 ^ 0x80) << 6)
                                   | (unsigned int) (c3 ^ 0x80));

                                if (FITS_IN_CHAR_TYPE (wc))
                                  {
                                    if (pwc != NULL)
                                      *pwc = wc;
                                    res = 3;
                                    goto success;
                                  }
                              }
                          }
                      }
                  }
              }
            else if (c <= 0xf4)
              {
                if (m == 1)
                  goto incomplete;
                else
                  {
                    unsigned char c2 = (unsigned char) p[1];

                    if ((c2 ^ 0x80) < 0x40
                        && (c >= 0xf1 || c2 >= 0x90)
                        && (c < 0xf4 || (/* c == 0xf4 && */ c2 < 0x90)))
                      {
                        if (m == 2)
                          goto incomplete;
                        else
                          {
                            unsigned char c3 = (unsigned char) p[2];

                            if ((c3 ^ 0x80) < 0x40)
                              {
                                if (m == 3)
                                  goto incomplete;
                                else /* m >= 4 */
                                  {
                                    unsigned char c4 = (unsigned char) p[3];

                                    if ((c4 ^ 0x80) < 0x40)
                                      {
                                        unsigned int wc =
                                          (((unsigned int) (c & 0x07) << 18)
                                           | ((unsigned int) (c2 ^ 0x80) << 12)
                                           | ((unsigned int) (c3 ^ 0x80) << 6)
                                           | (unsigned int) (c4 ^ 0x80));

                                        if (FITS_IN_CHAR_TYPE (wc))
                                          {
                                            if (pwc != NULL)
                                              *pwc = wc;
                                            res = 4;
                                            goto success;
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
        goto invalid;
