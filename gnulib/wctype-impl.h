/* Get descriptor for a wide character property.
   Copyright (C) 2011-2026 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2011.

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

wctype_t
wctype (const char* name)
{
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
                return (wctype_t) iswalnum;
              break;
            case 'p':
              if (streq (name + 3, "ha"))
                return (wctype_t) iswalpha;
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
        return (wctype_t) iswblank;
      break;
    case 'c':
      if (streq (name + 1, "ntrl"))
        return (wctype_t) iswcntrl;
      break;
    case 'd':
      if (streq (name + 1, "igit"))
        return (wctype_t) iswdigit;
      break;
    case 'g':
      if (streq (name + 1, "raph"))
        return (wctype_t) iswgraph;
      break;
    case 'l':
      if (streq (name + 1, "ower"))
        return (wctype_t) iswlower;
      break;
    case 'p':
      switch (name[1])
        {
        case 'r':
          if (streq (name + 2, "int"))
            return (wctype_t) iswprint;
          break;
        case 'u':
          if (streq (name + 2, "nct"))
            return (wctype_t) iswpunct;
          break;
        default:
          break;
        }
      break;
    case 's':
      if (streq (name + 1, "pace"))
        return (wctype_t) iswspace;
      break;
    case 'u':
      if (streq (name + 1, "pper"))
        return (wctype_t) iswupper;
      break;
    case 'x':
      if (streq (name + 1, "digit"))
        return (wctype_t) iswxdigit;
      break;
    default:
      break;
    }
  return NULL;
}
