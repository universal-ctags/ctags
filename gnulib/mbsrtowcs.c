/* Convert string to wide string.
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

#include <errno.h>
#include <limits.h>
#include <stdlib.h>

#include "strnlen1.h"


extern mbstate_t _gl_mbsrtowcs_state;

#define FUNC mbsrtowcs
#define DCHAR_T wchar_t
#define INTERNAL_STATE _gl_mbsrtowcs_state
#define MBRTOWC mbrtowc
#include "mbsrtowcs-impl.h"
