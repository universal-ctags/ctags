/* Once-only initialization in multithreaded situations.
   Copyright (C) 2005-2026 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2005.
   Based on GCC's gthr-posix.h, gthr-posix95.h.  */

#include <config.h>

#include "glthread/once.h"

#include <errno.h>

/* ========================================================================= */

#if USE_ISOC_THREADS || USE_ISOC_AND_POSIX_THREADS

#endif

/* ========================================================================= */

#if USE_POSIX_THREADS

static const pthread_once_t fresh_once = PTHREAD_ONCE_INIT;

int
glthread_once_singlethreaded (pthread_once_t *once_control)
{
  /* We don't know whether pthread_once_t is an integer type, a floating-point
     type, a pointer type, or a structure type.  */
  char *firstbyte = (char *)once_control;
  if (*firstbyte == *(const char *)&fresh_once)
    {
      /* First time use of once_control.  Invert the first byte.  */
      *firstbyte = ~ *(const char *)&fresh_once;
      return 1;
    }
  else
    return 0;
}

# if !(PTHREAD_IN_USE_DETECTION_HARD || USE_POSIX_THREADS_WEAK)

int
glthread_once_multithreaded (pthread_once_t *once_control,
                             void (*init_function) (void))
{
  int err = pthread_once (once_control, init_function);
  if (err == ENOSYS)
    {
      /* This happens on FreeBSD 11: The pthread_once function in libc returns
         ENOSYS.  */
      if (glthread_once_singlethreaded (once_control))
        init_function ();
      return 0;
    }
  return err;
}

# endif

#endif

/* ========================================================================= */

#if USE_WINDOWS_THREADS

#endif

/* ========================================================================= */
