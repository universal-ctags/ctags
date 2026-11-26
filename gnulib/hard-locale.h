/* Determine whether a locale is hard.

   Copyright (C) 1999, 2003-2004, 2009-2026 Free Software Foundation, Inc.

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

#ifndef HARD_LOCALE_H_
#define HARD_LOCALE_H_ 1

#ifdef __cplusplus
extern "C" {
#endif


/* Return true if the specified CATEGORY of the current locale is hard, i.e.
   different from the C or POSIX locale that has a fixed behavior.
   CATEGORY must be one of the LC_* values, but not LC_ALL.
   Note: This function uses the current global locale; it ignores the
   per-thread locale.  */
extern bool hard_locale (int category);


#ifdef __cplusplus
}
#endif

#endif /* HARD_LOCALE_H_ */
