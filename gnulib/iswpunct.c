/* Test wide character for being a punctuation or symbol character.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#include <ctype.h>

int
iswpunct (wint_t wc)
#undef iswpunct
{
#if defined __ANDROID__
  if ((unsigned int) wc < 128)
    return ispunct ((unsigned int) wc);
#endif
  return iswpunct (wc);
}
