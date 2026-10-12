/* POSIX once-only control.
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
#include <pthread.h>

#if (defined _WIN32 && ! defined __CYGWIN__) && USE_WINDOWS_THREADS
# include "windows-once.h"
#endif

#if (defined _WIN32 && ! defined __CYGWIN__) && USE_WINDOWS_THREADS
/* Use Windows threads.  */

int
pthread_once (pthread_once_t *once_control, void (*initfunction) (void))
{
  glwthread_once (once_control, initfunction);
  return 0;
}

#elif HAVE_PTHREAD_H
/* Provide workarounds for POSIX threads.  */

# if defined __CYGWIN__

#  include <stdlib.h>

int
pthread_once (pthread_once_t *once_control, void (*initfunction) (void))
{
  /* In this implementation, we reuse the type
       typedef struct { pthread_mutex_t mutex; int state; } pthread_once_t;
       #define PTHREAD_ONCE_INIT { PTHREAD_MUTEX_INITIALIZER, 0 }
     while assigning the following meaning to the state:
       state = (<number of waiting threads> << 16) + <1 if done>
     In other words:
       state = { unsigned int num_threads : 16; unsigned int done : 16; }
   */
  struct actual_state
    {
      _Atomic unsigned short num_threads;
      /* done == 0: initial state
         done == 1: initfunction executed, lock still active
         done == 2: initfunction executed, lock no longer usable */
      _Atomic unsigned short done;
    };
  struct actual_state *state_p = (struct actual_state *) &once_control->state;
  /* This test is not necessary.  It's only an optimization, to establish
     a fast path for the common case that the 'done' word is already > 0.  */
  if (state_p->done == 0)
    {
      /* Increment num_threads (atomically), to indicate that this thread will
         possibly take the lock.  */
      state_p->num_threads += 1;
      /* Test the 'done' word.  */
      if (state_p->done == 0)
        {
          /* The 'done' word is still zero.  Now take the lock.  */
          pthread_mutex_lock (&once_control->mutex);
          /* Test the 'done' word again.  */
          if (state_p->done == 0)
            {
              /* Execute the initfunction.  */
              (*initfunction) ();
              /* Set the 'done' word to 1 (atomically).  */
              state_p->done = 1;
            }
          /* Now the 'done' word is 1.  Release the lock.  */
          pthread_mutex_unlock (&once_control->mutex);
        }
      /* Here, done is > 0.  */
      /* Decrement num_threads (atomically).  */
      if ((state_p->num_threads -= 1) == 0)
        {
          /* num_threads is now zero, and done is > 0.
             No other thread will need to use the lock.
             We can therefore destroy the lock, to free resources.  */
          if (__sync_bool_compare_and_swap (&state_p->done, 1, 2))
            pthread_mutex_destroy (&once_control->mutex);
        }
    }
  /* Proof of correctness:
     * num_threads is incremented and then decremented by some threads.
       Therefore, num_threads always stays >= 0, and is == 0 at the end.
     * The 'done' word, once > 0, stays > 0 (since it is never assigned 0).
     * The 'done' word is changed from == 0 to > 0 only while the lock
       is taken. Therefore, only the first thread that succeeds in taking
       the lock executes the initfunction and sets the 'done' word to a
       value > 0; the other threads that take the lock do no side effects
       between taking and releasing the lock.
     * The 'done' word does not change any more once it is 2.
       Therefore, it can be changed from 1 to 2 only once.
     * pthread_mutex_destroy gets invoked right after 'done' has been changed
       from 1 to 2.  Therefore, pthread_mutex_destroy gets invoked only once.
     * After a moment where num_threads was 0 and done was > 0, no thread can
       reach the pthread_mutex_lock invocation. Proof:
       - At such a moment, no thread is in the code range between
           state_p->num_threads += 1
         and
           state_p->num_threads -= 1
       - After such a moment, some thread can increment num_threads, but from
         there they cannot reach the pthread_mutex_lock invocation, because the
           if (state_p->done == 0)
         test prevents that.
     * From this it follows that:
       - pthread_mutex_destroy cannot be executed while the lock is taken
         (because pthread_mutex_destroy is only executed after a moment where
         num_threads was 0 and done was > 0).
       - Once pthread_mutex_destroy has been executed, the lock is not used any
         more.
   */
  return 0;
}

# endif

#else
/* Provide a dummy implementation for single-threaded applications.  */

int
pthread_once (pthread_once_t *once_control, void (*initfunction) (void))
{
  if (*once_control == 0)
    {
      *once_control = ~ 0;
      initfunction ();
    }
  return 0;
}

#endif
