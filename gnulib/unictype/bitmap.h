/* Three-level bitmap lookup.
   Copyright (C) 2000-2002, 2005-2007, 2009-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2000-2002.

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

static inline int bitmap_lookup (const void *table, ucs4_t uc);

/* These values are currently hardcoded into gen-uni-tables.c, function
   output_predicate().  */
#define header_0 16
#define header_2 9
#define header_3 127
#define header_4 15

static inline int
bitmap_lookup (const void *table, ucs4_t uc)
{
  unsigned int index1 = uc >> header_0;
  if (index1 < ((const int *) table)[0])
    {
      int lookup1 = ((const int *) table)[1 + index1];
      if (lookup1 >= 0)
        {
          unsigned int index2 = (uc >> header_2) & header_3;
          int lookup2 = ((const short *) table)[lookup1 + index2];
          if (lookup2 >= 0)
            {
              unsigned int index3 = (uc >> 5) & header_4;
              unsigned int lookup3 = ((const unsigned int *) table)[lookup2 + index3];

              return (lookup3 >> (uc & 0x1f)) & 1;
            }
        }
    }
  return 0;
}
