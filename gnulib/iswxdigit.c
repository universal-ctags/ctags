/* Test wide character for being a hexadecimal digit.
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

#include <config.h>

/* Specification.  */
#include <wctype.h>

int
iswxdigit (wint_t wc)
{
  return ((wc >= '0' && wc <= '9')
#if 'A' == 0x41 && 'a' == 0x61
          /* Optimization, assuming ASCII */
          || ((wc & ~0x20) >= 'A' && (wc & ~0x20) <= 'F')
#else
          || (wc >= 'A' && wc <= 'F') || (wc >= 'a' && wc <= 'f')
#endif
         );
}
