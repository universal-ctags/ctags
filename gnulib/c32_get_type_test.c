/* Get descriptor for a 32-bit wide character property.
   Copyright (C) 2011-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2023.  */

#include <config.h>

#define IN_C32_GET_TYPE_TEST
/* Specification.  */
#include <uchar.h>

#include <string.h>
#include <wctype.h>

#if _GL_WCHAR_T_IS_UCS4
_GL_EXTERN_INLINE
#endif
c32_type_test_t
c32_get_type_test (const char *name)
{
#if _GL_WCHAR_T_IS_UCS4
  return wctype (name);
#else
  switch (name[0])
    {
    case 'a':
      switch (name[1])
        {
        case 'l':
          switch (name[2])
            {
            case 'n':
              if (streq (name + 3, "um"))
                return c32isalnum;
              break;
            case 'p':
              if (streq (name + 3, "ha"))
                return c32isalpha;
              break;
            default:
              break;
            }
          break;
        default:
          break;
        }
      break;
    case 'b':
      if (streq (name + 1, "lank"))
        return c32isblank;
      break;
    case 'c':
      if (streq (name + 1, "ntrl"))
        return c32iscntrl;
      break;
    case 'd':
      if (streq (name + 1, "igit"))
        return c32isdigit;
      break;
    case 'g':
      if (streq (name + 1, "raph"))
        return c32isgraph;
      break;
    case 'l':
      if (streq (name + 1, "ower"))
        return c32islower;
      break;
    case 'p':
      switch (name[1])
        {
        case 'r':
          if (streq (name + 2, "int"))
            return c32isprint;
          break;
        case 'u':
          if (streq (name + 2, "nct"))
            return c32ispunct;
          break;
        default:
          break;
        }
      break;
    case 's':
      if (streq (name + 1, "pace"))
        return c32isspace;
      break;
    case 'u':
      if (streq (name + 1, "pper"))
        return c32isupper;
      break;
    case 'x':
      if (streq (name + 1, "digit"))
        return c32isxdigit;
      break;
    default:
      break;
    }
  return (c32_type_test_t) 0;
#endif
}
