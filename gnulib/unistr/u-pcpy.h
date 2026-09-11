/* Copy piece of UTF-8/16/32 string, return pointer after last written unit.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2023.

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

UNIT *
FUNC (UNIT *dest, const UNIT *src, size_t n)
{
  return U_CPY (dest, src, n) + n;
}
