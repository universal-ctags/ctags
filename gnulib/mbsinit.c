/* Test for initial conversion state.
   Copyright (C) 2008-2021 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2008.

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
#include <wchar.h>

#include "verify.h"

#if GNULIB_defined_mbstate_t

/* Platforms that lack mbsinit() also lack mbrlen(), mbrtowc(), mbsrtowcs()
   and wcrtomb(), wcsrtombs().
   We assume that
     - sizeof (mbstate_t) >= 4,
     - only stateless encodings are supported (such as UTF-8 and EUC-JP, but
       not ISO-2022 variants),
     - for each encoding, the number of bytes for a wide character is <= 4.
       (This maximum is attained for UTF-8, GB18030, EUC-TW.)
   We define the meaning of mbstate_t as follows:
     - In mb -> wc direction, mbstate_t's first byte contains the number of
       buffered bytes (in the range 0..3), followed by up to 3 buffered bytes.
       See mbrtowc.c.
     - In wc -> mb direction, mbstate_t contains no information. In other
       words, it is always in the initial state.  */

verify (sizeof (mbstate_t) >= 4);

int
mbsinit (const mbstate_t *ps)
{
  const char *pstate = (const char *)ps;

  return pstate == NULL || pstate[0] == 0;
}

#else

int
mbsinit (const mbstate_t *ps)
{
# if defined _WIN32 && !defined __CYGWIN__
  /* Native Windows.  */
  /* MSVC defines 'mbstate_t' as an 8-byte struct; the first 4 bytes matter.
     On mingw, 'mbstate_t' is sometimes defined as 'int', sometimes defined as
     an 8-byte struct, of which the first 4 bytes matter.  */
  return ps == NULL || *(const unsigned int *)ps == 0;
# else
  /* Minix, HP-UX 11.00, Solaris 2.6, Interix, ...  */
  /* Maybe this definition works, maybe not...  */
  return ps == NULL || *(const char *)ps == 0;
# endif
}

#endif
