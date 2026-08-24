/* Copy a size-bounded string.
   Copyright (C) 1999, 2011-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 1999.  */

#include <config.h>

/* Specification.  */
#include <string.h>

char *
strncpy (char *dest, const char *src, size_t n)
{
  char *destptr = dest;

  for (; n > 0 && (*destptr = *src) != '\0'; src++, destptr++, n--)
    ;

  /* This behavior is rarely useful, but it is specified by the ISO C
     standard.  */
  for (; n > 0; n--)
    *destptr++ = '\0';

  return dest;
}
