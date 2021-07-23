/* Test whether a character is a blank.

   Copyright (C) 2009-2021 Free Software Foundation, Inc.

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
#include <ctype.h>

int
isblank (int c)
{
  /* On all known platforms, in all predefined locales, isblank(c) is likely
     equivalent with  (c == ' ' || c == '\t').  Look at the glibc definition
     (in glibc/localedata/locales/i18n): The "blank" characters are '\t', ' ',
     U+1680, U+180E, U+2000..U+2006, U+2008..U+200A, U+205F, U+3000, and none
     except the first two is present in a common 8-bit encoding.  Therefore
     the substitute for other platforms is not more complicated than this.  */
  return (c == ' ' || c == '\t');
}
