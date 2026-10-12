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
   Based on GCC's gthr-posix.h, gthr-posix95.h, gthr-win32.h.  */

/* This file contains once-only initialization primitives for use with a given
   thread library.
   It does not contain primitives for creating threads or for other
   synchronization primitives.

  Once-only execution:
     Type:                gl_once_t
     Initializer:         gl_once_define(extern, name)
     Execution:           gl_once (name, initfunction);
   Equivalent functions with control of error handling:
     Execution:           err = glthread_once (&name, initfunction);
*/


#ifndef _ONCE_H
#define _ONCE_H

/* This file uses HAVE_THREADS_H.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

#include <stdlib.h>

#if !defined c11_threads_in_use
# if HAVE_THREADS_H && USE_POSIX_THREADS_FROM_LIBC
#  define c11_threads_in_use() 1
# elif HAVE_THREADS_H && USE_POSIX_THREADS_WEAK
#  include <threads.h>
#  pragma weak thrd_exit
#  define c11_threads_in_use() (thrd_exit != NULL)
# else
#  define c11_threads_in_use() 0
# endif
#endif

/* ========================================================================= */

#if USE_ISOC_THREADS || USE_ISOC_AND_POSIX_THREADS

/* Use the ISO C threads library.  */

# include <threads.h>

# ifdef __cplusplus
extern "C" {
# endif

/* -------------------------- gl_once_t datatype -------------------------- */

typedef once_flag gl_once_t;
# define gl_once_define(STORAGECLASS, NAME) \
    STORAGECLASS once_flag NAME = ONCE_FLAG_INIT;
# define glthread_once(ONCE_CONTROL, INITFUNCTION) \
    (call_once (ONCE_CONTROL, INITFUNCTION), 0)

# ifdef __cplusplus
}
# endif

#endif

/* ========================================================================= */

#if USE_POSIX_THREADS

/* Use the POSIX threads library.  */

# include <pthread.h>

# ifdef __cplusplus
extern "C" {
# endif

# if PTHREAD_IN_USE_DETECTION_HARD

/* The pthread_in_use() detection needs to be done at runtime.  */
#  define pthread_in_use() \
     glthread_in_use ()
extern int glthread_in_use (void);

# endif

# if USE_POSIX_THREADS_WEAK

/* Use weak references to the POSIX threads library.  */

/* Weak references avoid dragging in external libraries if the other parts
   of the program don't use them.  Here we use them, because we don't want
   every program that uses libintl to depend on libpthread.  This assumes
   that libpthread would not be loaded after libintl; i.e. if libintl is
   loaded first, by an executable that does not depend on libpthread, and
   then a module is dynamically loaded that depends on libpthread, libintl
   will not be thread-safe.  */

/* The way to test at runtime whether libpthread is present is to test
   whether a function pointer's value, such as &pthread_mutex_init, is
   non-NULL.  However, some versions of GCC have a bug through which, in
   PIC mode, &foo != NULL always evaluates to true if there is a direct
   call to foo(...) in the same function.  To avoid this, we test the
   address of a function in libpthread that we don't use.  */

#  pragma weak pthread_mutex_init
#  pragma weak pthread_mutex_lock
#  pragma weak pthread_mutex_unlock
#  pragma weak pthread_mutex_destroy
/* Work around clang bug <https://github.com/llvm/llvm-project/issues/104670> */
#  ifndef pthread_rwlock_init
#   pragma weak pthread_rwlock_init
#  endif
#  pragma weak pthread_rwlock_rdlock
#  pragma weak pthread_rwlock_wrlock
#  pragma weak pthread_rwlock_unlock
#  pragma weak pthread_rwlock_destroy
#  pragma weak pthread_once
#  pragma weak pthread_cond_init
#  pragma weak pthread_cond_wait
#  pragma weak pthread_cond_signal
#  pragma weak pthread_cond_broadcast
#  pragma weak pthread_cond_destroy
#  pragma weak pthread_mutexattr_init
#  pragma weak pthread_mutexattr_settype
#  pragma weak pthread_mutexattr_destroy
/* Work around clang bug <https://github.com/llvm/llvm-project/issues/104670> */
#  ifndef pthread_rwlockattr_init
#   pragma weak pthread_rwlockattr_init
#  endif
#  if __GNU_LIBRARY__ > 1
#   pragma weak pthread_rwlockattr_setkind_np
#  endif
#  pragma weak pthread_rwlockattr_destroy
#  ifndef pthread_self
#   pragma weak pthread_self
#  endif

#  if !PTHREAD_IN_USE_DETECTION_HARD
    /* Considering all platforms with USE_POSIX_THREADS_WEAK, only few symbols
       can be used to determine whether libpthread is in use.  These are:
         pthread_mutexattr_gettype
         pthread_rwlockattr_destroy
         pthread_rwlockattr_init
     */
#   pragma weak pthread_mutexattr_gettype
#   define pthread_in_use() \
      (pthread_mutexattr_gettype != NULL || c11_threads_in_use ())
#  endif

# else

#  if !PTHREAD_IN_USE_DETECTION_HARD
#   define pthread_in_use() 1
#  endif

# endif

/* -------------------------- gl_once_t datatype -------------------------- */

typedef pthread_once_t gl_once_t;
# define gl_once_define(STORAGECLASS, NAME) \
    STORAGECLASS pthread_once_t NAME = PTHREAD_ONCE_INIT;
# if PTHREAD_IN_USE_DETECTION_HARD || USE_POSIX_THREADS_WEAK
#  define glthread_once(ONCE_CONTROL, INITFUNCTION) \
     (pthread_in_use ()                                                        \
      ? pthread_once (ONCE_CONTROL, INITFUNCTION)                              \
      : (glthread_once_singlethreaded (ONCE_CONTROL) ? (INITFUNCTION (), 0) : 0))
# else
#  define glthread_once(ONCE_CONTROL, INITFUNCTION) \
     (pthread_in_use ()                                                        \
      ? glthread_once_multithreaded (ONCE_CONTROL, INITFUNCTION)               \
      : (glthread_once_singlethreaded (ONCE_CONTROL) ? (INITFUNCTION (), 0) : 0))
extern int glthread_once_multithreaded (pthread_once_t *once_control,
                                        void (*init_function) (void));
# endif
extern int glthread_once_singlethreaded (pthread_once_t *once_control);

# ifdef __cplusplus
}
# endif

#endif

/* ========================================================================= */

#if USE_WINDOWS_THREADS

# define WIN32_LEAN_AND_MEAN  /* avoid including junk */
# include <windows.h>

# include "windows-once.h"

# ifdef __cplusplus
extern "C" {
# endif

/* We can use CRITICAL_SECTION directly, rather than the native Windows Event,
   Mutex, Semaphore types, because
     - we need only to synchronize inside a single process (address space),
       not inter-process locking,
     - we don't need to support trylock operations.  (TryEnterCriticalSection
       does not work on Windows 95/98/ME.  Packages that need trylock usually
       define their own mutex type.)  */

/* There is no way to statically initialize a CRITICAL_SECTION.  It needs
   to be done lazily, once only.  For this we need spinlocks.  */

/* -------------------------- gl_once_t datatype -------------------------- */

typedef glwthread_once_t gl_once_t;
# define gl_once_define(STORAGECLASS, NAME) \
    STORAGECLASS gl_once_t NAME = GLWTHREAD_ONCE_INIT;
# define glthread_once(ONCE_CONTROL, INITFUNCTION) \
    (glwthread_once (ONCE_CONTROL, INITFUNCTION), 0)

# ifdef __cplusplus
}
# endif

#endif

/* ========================================================================= */

#if !(USE_ISOC_THREADS || USE_POSIX_THREADS || USE_ISOC_AND_POSIX_THREADS || USE_WINDOWS_THREADS)

/* Provide dummy implementation if threads are not supported.  */

/* -------------------------- gl_once_t datatype -------------------------- */

typedef int gl_once_t;
# define gl_once_define(STORAGECLASS, NAME) \
    STORAGECLASS gl_once_t NAME = 0;
# define glthread_once(ONCE_CONTROL, INITFUNCTION) \
    (*(ONCE_CONTROL) == 0 ? (*(ONCE_CONTROL) = ~ 0, INITFUNCTION (), 0) : 0)

#endif

/* ========================================================================= */

/* Macros with built-in error handling.  */

/* -------------------------- gl_once_t datatype -------------------------- */

#define gl_once(NAME, INITFUNCTION) \
   do                                           \
     {                                          \
       if (glthread_once (&NAME, INITFUNCTION)) \
         abort ();                              \
     }                                          \
   while (0)

/* ========================================================================= */

#endif /* _ONCE_H */
