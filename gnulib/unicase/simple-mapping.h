/* Simple case mapping for Unicode characters.
   Copyright (C) 2002, 2006, 2009-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2009.

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

ucs4_t
FUNC (ucs4_t uc)
{
  unsigned int index1 = uc >> mapping_header_0;
  if (index1 < mapping_header_1)
    {
      int lookup1 = u_mapping.level1[index1];
      if (lookup1 >= 0)
        {
          unsigned int index2 = (uc >> mapping_header_2) & mapping_header_3;
          int lookup2 = u_mapping.level2[lookup1 + index2];
          if (lookup2 >= 0)
            {
              unsigned int index3 = (uc & mapping_header_4);
              int lookup3 = u_mapping.level3[lookup2 + index3];

              return uc + lookup3;
            }
        }
    }
  return uc;
}
