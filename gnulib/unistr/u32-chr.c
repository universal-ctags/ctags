/* Search character in piece of UTF-32 string.
   Copyright (C) 1999, 2002, 2006, 2009-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2002.

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

/* Don't use the const-improved function macros in this compilation unit.  */
#define _LIBUNISTRING_NO_CONST_GENERICS

#include <config.h>

/* Specification.  */
#include "unistr.h"

uint32_t *
u32_chr (const uint32_t *s, size_t n, ucs4_t uc)
{
  for (; n > 0; s++, n--)
    {
      if (*s == uc)
        return (uint32_t *) s;
    }
  return NULL;
}
