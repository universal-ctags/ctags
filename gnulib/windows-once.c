/* Once-only control (native Windows implementation).
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
   Based on GCC's gthr-win32.h.  */

#include <config.h>

/* Specification.  */
#include "windows-once.h"

#include <stdlib.h>

void
glwthread_once (glwthread_once_t *once_control, void (*initfunction) (void))
{
  if (once_control->inited <= 0)
    {
      InterlockedIncrement (&once_control->num_threads);
      /* If once_control->started is == -1, set it to 0.  */
      if (InterlockedCompareExchange (&once_control->started, 0, -1) < 0)
        {
          /* This thread is the first one to come to this once_control.  */
          InitializeCriticalSection (&once_control->lock);
          EnterCriticalSection (&once_control->lock);
          once_control->inited = 0;
          initfunction ();
          once_control->inited = 1;
          LeaveCriticalSection (&once_control->lock);
        }
      else
        {
          /* Some other thread has already started the initialization.
             Yield the CPU while waiting for the other thread to finish
             initializing and taking the lock.  */
          while (once_control->inited < 0)
            Sleep (0);
          if (once_control->inited <= 0)
            {
              /* Take the lock.  This blocks until the other thread has
                 finished calling the initfunction.  */
              EnterCriticalSection (&once_control->lock);
              LeaveCriticalSection (&once_control->lock);
              if (!(once_control->inited > 0))
                abort ();
            }
        }
      /* Here once_control->started == 0 and once_control->inited > 0.  */
      if (InterlockedDecrement (&once_control->num_threads) == 0)
        /* once_control->num_threads is now zero, and
           once_control->started == 0 and once_control->inited > 0.
           No other thread will need to use the lock.
           We can therefore destroy the lock, to free resources.  */
        /* If once_control->inited is == 1, set it to 2.  */
        if (InterlockedCompareExchange (&once_control->inited, 2, 1) == 1)
          DeleteCriticalSection (&once_control->lock);
    }
  /* Proof of correctness:
     * num_threads is incremented and then decremented by some threads.
       Therefore, num_threads always stays >= 0, and is == 0 at the end.
     * The first thread to go through the once_control->started fence
       initializes the lock and moves inited from <= 0 to > 0.  The other
       threads don't move inited from <= 0 to > 0.
     * started, once == 0, stays == 0.
     * inited, once > 0, stays > 0 (since at the place where it is assigned 0,
       it cannot be > 0).
     * inited does not change any more once it is 2.
       Therefore, it can be changed from 1 to 2 only once.
     * DeleteCriticalSection gets invoked right after inited has been changed
       from 1 to 2.  Therefore, DeleteCriticalSection gets invoked only once.
     * After a moment where num_threads was 0 and started was 0 and
       inited was > 0, no thread can reach an InitializeCriticalSection or
       EnterCriticalSection invocation.  Proof:
       - At such a moment, no thread is in the code range between
           InterlockedIncrement (&once_control->num_threads)
         and
           InterlockedDecrement (&once_control->num_threads)
       - After such a moment, some thread can increment num_threads, but from
         there they cannot reach the InitializeCriticalSection invocation,
         because the once_control->started test prevents that, and they cannot
         reach the EnterCriticalSection invocation in the other branch because
         the
           if (once_control->inited <= 0)
         test prevents that.
     * From this it follows that:
       - DeleteCriticalSection cannot be executed while the lock is taken
         (because DeleteCriticalSection is only executed after a moment where
         num_threads was 0 and started was 0 and inited was > 0).
       - Once DeleteCriticalSection has been executed, the lock is not used any
         more.
   */
}
