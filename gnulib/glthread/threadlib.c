/* Multithreading primitives.
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2005.  */

#include <config.h>

/* ========================================================================= */

#if USE_POSIX_THREADS || USE_ISOC_AND_POSIX_THREADS

/* Use the POSIX threads library.  */

# include <errno.h>
# include <pthread.h>
# include <stdlib.h>

# if PTHREAD_IN_USE_DETECTION_HARD

#  if defined __FreeBSD__ || defined __DragonFly__                 /* FreeBSD */

/* Test using pthread_key_create.  */

int
glthread_in_use (void)
{
  static int tested;
  static int result; /* 1: linked with -lpthread, 0: only with libc */

  if (!tested)
    {
      pthread_key_t key;
      int err = pthread_key_create (&key, NULL);

      if (err == ENOSYS)
        result = 0;
      else
        {
          result = 1;
          if (err == 0)
            pthread_key_delete (key);
        }
      tested = 1;
    }
  return result;
}

#  else                                                     /* Solaris, HP-UX */

/* Test using pthread_create.  */

/* The function to be executed by a dummy thread.  */
static void *
dummy_thread_func (void *arg)
{
  return arg;
}

int
glthread_in_use (void)
{
  static int tested;
  static int result; /* 1: linked with -lpthread, 0: only with libc */

  if (!tested)
    {
      pthread_t thread;

      if (pthread_create (&thread, NULL, dummy_thread_func, NULL) != 0)
        /* Thread creation failed.  */
        result = 0;
      else
        {
          /* Thread creation works.  */
          void *retval;
          if (pthread_join (thread, &retval) != 0)
            abort ();
          result = 1;
        }
      tested = 1;
    }
  return result;
}

#  endif

# endif

#endif

/* ========================================================================= */

/* This declaration is solely to ensure that after preprocessing
   this file is never empty.  */
typedef int dummy;
