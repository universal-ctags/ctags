/* Return the internal lock used by mbrtowc and mbrtoc32.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.

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

/* Written by Bruno Haible <bruno@clisp.org>, 2019-2020.  */

#include <config.h>

/* When it is known that the gl_get_mbtowc_lock function is defined
   by a dependency library, it should not be defined here.  */
#if OMIT_MBTOWC_LOCK

/* This declaration is solely to ensure that after preprocessing
   this file is never empty.  */
typedef int dummy;

#else

/* This file defines the internal lock used by mbrtowc and mbrtoc32.
   It is a separate compilation unit, so that only one copy of it is
   present when linking statically.  */

/* Prohibit renaming this symbol.  */
# undef gl_get_mbtowc_lock

/* Macro for exporting a symbol (function, not variable) defined in this file,
   when compiled into a shared library.  */
# ifndef DLL_EXPORTED
#  if HAVE_VISIBILITY
  /* Override the effect of the compiler option '-fvisibility=hidden'.  */
#   define DLL_EXPORTED __attribute__((__visibility__("default")))
#  elif defined _WIN32 || defined __CYGWIN__
#   define DLL_EXPORTED __declspec(dllexport)
#  else
#   define DLL_EXPORTED
#  endif
# endif

# if defined _WIN32 && !defined __CYGWIN__

#  define WIN32_LEAN_AND_MEAN  /* avoid including junk */
#  include <windows.h>

#  include "windows-initguard.h"

/* The return type is a 'CRITICAL_SECTION *', not a 'glwthread_mutex_t *',
   because the latter is not guaranteed to be a stable ABI in the future.  */

/* Make sure the function gets exported from DLLs.  */
DLL_EXPORTED CRITICAL_SECTION *gl_get_mbtowc_lock (void);

static glwthread_initguard_t guard = GLWTHREAD_INITGUARD_INIT;
static CRITICAL_SECTION lock;

/* Returns the internal lock used by mbrtowc and mbrtoc32.  */
CRITICAL_SECTION *
gl_get_mbtowc_lock (void)
{
  if (!guard.done)
    {
      if (InterlockedIncrement (&guard.started) == 0)
        {
          /* This thread is the first one to need the lock.  Initialize it.  */
          InitializeCriticalSection (&lock);
          guard.done = 1;
        }
      else
        {
          /* Don't let guard.started grow and wrap around.  */
          InterlockedDecrement (&guard.started);
          /* Yield the CPU while waiting for another thread to finish
             initializing this mutex.  */
          while (!guard.done)
            Sleep (0);
        }
    }
  return &lock;
}

# elif HAVE_PTHREAD_API

#  include <pthread.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

/* Make sure the function gets exported from shared libraries.  */
DLL_EXPORTED pthread_mutex_t *gl_get_mbtowc_lock (void);

/* Returns the internal lock used by mbrtowc and mbrtoc32.  */
pthread_mutex_t *
gl_get_mbtowc_lock (void)
{
  return &mutex;
}

# elif HAVE_THREADS_H

#  include <threads.h>
#  include <stdlib.h>

static int volatile init_needed = 1;
static once_flag init_once = ONCE_FLAG_INIT;
static mtx_t mutex;

static void
atomic_init (void)
{
  if (mtx_init (&mutex, mtx_plain) != thrd_success)
    abort ();
  init_needed = 0;
}

/* Make sure the function gets exported from shared libraries.  */
DLL_EXPORTED mtx_t *gl_get_mbtowc_lock (void);

/* Returns the internal lock used by mbrtowc and mbrtoc32.  */
mtx_t *
gl_get_mbtowc_lock (void)
{
  if (init_needed)
    call_once (&init_once, atomic_init);
  return &mutex;
}

# endif

# if (defined _WIN32 || defined __CYGWIN__) && !defined _MSC_VER
/* Make sure the '__declspec(dllimport)' in mbrtowc.c and mbrtoc32.c does not
   cause a link failure when no DLLs are involved.  */
#  if defined _WIN64 || defined _LP64
#   define IMP(x) __imp_##x
#  else
#   define IMP(x) _imp__##x
#  endif
void * IMP(gl_get_mbtowc_lock) = &gl_get_mbtowc_lock;
# endif

#endif
