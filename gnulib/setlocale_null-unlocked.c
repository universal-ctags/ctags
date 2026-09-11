/* Query the name of the current global locale, without locking.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2019.  */

#include <config.h>

/* Specification.  */
#include "setlocale_null.h"

#include <errno.h>
#include <locale.h>
#include <string.h>
#if defined _WIN32 && !defined __CYGWIN__
# include <wchar.h>
#endif

/* Use the system's setlocale() function, not the gnulib override, here.  */
#undef setlocale

const char *
setlocale_null_unlocked (int category)
{
  const char *result = setlocale (category, NULL);

#ifdef __ANDROID__
  if (result == NULL)
    switch (category)
      {
      case LC_CTYPE:
      case LC_NUMERIC:
      case LC_TIME:
      case LC_COLLATE:
      case LC_MONETARY:
      case LC_MESSAGES:
      case LC_ALL:
      case LC_PAPER:
      case LC_NAME:
      case LC_ADDRESS:
      case LC_TELEPHONE:
      case LC_MEASUREMENT:
        result = "C";
        break;
      default:
        break;
      }
#endif

  return result;
}

int
setlocale_null_r_unlocked (int category, char *buf, size_t bufsize)
{
#if defined _WIN32 && !defined __CYGWIN__ && defined _MSC_VER
  /* On native Windows, nowadays, the setlocale() implementation is based
     on _wsetlocale() and uses malloc() for the result.  We are better off
     using _wsetlocale() directly.  */
  const wchar_t *result = _wsetlocale (category, NULL);

  if (result == NULL)
    {
      /* CATEGORY is invalid.  */
      if (bufsize > 0)
        /* Return an empty string in BUF.
           This is a convenience for callers that don't want to write explicit
           code for handling EINVAL.  */
        buf[0] = '\0';
      return EINVAL;
    }
  else
    {
      size_t length = wcslen (result);
      if (length < bufsize)
        {
          /* Convert wchar_t[] -> char[], assuming plain ASCII.  */
          for (size_t i = 0; i <= length; i++)
            buf[i] = result[i];

          return 0;
        }
      else
        {
          if (bufsize > 0)
            {
              /* Return a truncated result in BUF.
                 This is a convenience for callers that don't want to write
                 explicit code for handling ERANGE.  */
              /* Convert wchar_t[] -> char[], assuming plain ASCII.  */
              for (size_t i = 0; i < bufsize; i++)
                buf[i] = result[i];
              buf[bufsize - 1] = '\0';
            }
          return ERANGE;
        }
    }
#else
  const char *result = setlocale_null_unlocked (category);

  if (result == NULL)
    {
      /* CATEGORY is invalid.  */
      if (bufsize > 0)
        /* Return an empty string in BUF.
           This is a convenience for callers that don't want to write explicit
           code for handling EINVAL.  */
        buf[0] = '\0';
      return EINVAL;
    }
  else
    {
      size_t length = strlen (result);
      if (length < bufsize)
        {
          memcpy (buf, result, length + 1);
          return 0;
        }
      else
        {
          if (bufsize > 0)
            {
              /* Return a truncated result in BUF.
                 This is a convenience for callers that don't want to write
                 explicit code for handling ERANGE.  */
              memcpy (buf, result, bufsize - 1);
              buf[bufsize - 1] = '\0';
            }
          return ERANGE;
        }
    }
#endif
}
