/* Dispatching based on the current locale's character encoding.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2018.  */

#include <wchar.h>

#if GNULIB_defined_mbstate_t

/* A classification of special values of the encoding of the current locale.  */
typedef enum
  {
    enc_other,      /* other */
    enc_utf8,       /* UTF-8 */
    enc_eucjp,      /* EUC-JP */
    enc_94,         /* EUC-KR, GB2312, BIG5 */
    enc_euctw,      /* EUC-TW */
    enc_gb18030,    /* GB18030 */
    enc_sjis        /* SJIS */
  }
  enc_t;

/* Returns a classification of special values of the encoding of the current
   locale.  */
extern enc_t locale_encoding_classification (void);

#endif
