/* Find the length of STRING, but scan at most MAXLEN characters.
   Copyright (C) 2005-2007, 2009-2026 Free Software Foundation, Inc.

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

#include <string.h>

/* Find the length of S, but scan at most MAXLEN bytes.
   S must be a string if it starts with fewer than MAXLEN initialized bytes.
   If no '\0' terminator is found in that many bytes, return MAXLEN.  */

size_t
strnlen (const char *s, size_t maxlen)
{
  /* Do not use memchr, because on some platforms memchr has
     undefined behavior if MAXLEN exceeds the number of bytes in S.  */
  size_t i;
  for (i = 0; i < maxlen && s[i]; i++)
    continue;
  return i;
}
