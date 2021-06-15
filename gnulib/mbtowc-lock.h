/* Use the internal lock used by mbrtowc and mbrtoc32.
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

/* Use a lock, so that no two threads can invoke mbtowc at the same time.  */

static inline int
mbtowc_unlocked (wchar_t *pwc, const char *p, size_t m)
{
  /* Put the hidden internal state of mbtowc into its initial state.
     This is needed at least with glibc, uClibc, and MSVC CRT.
     See <https://sourceware.org/bugzilla/show_bug.cgi?id=9674>.  */
  mbtowc (NULL, NULL, 0);

  return mbtowc (pwc, p, m);
}

/* Prohibit renaming this symbol.  */
#undef gl_get_mbtowc_lock

#if GNULIB_MBRTOWC_SINGLE_THREAD

/* All uses of this function are in a single thread.  No locking needed.  */

static int
mbtowc_with_lock (wchar_t *pwc, const char *p, size_t m)
{
  return mbtowc_unlocked (pwc, p, m);
}

#elif defined _WIN32 && !defined __CYGWIN__

extern __declspec(dllimport) CRITICAL_SECTION *gl_get_mbtowc_lock (void);

static int
mbtowc_with_lock (wchar_t *pwc, const char *p, size_t m)
{
  CRITICAL_SECTION *lock = gl_get_mbtowc_lock ();
  int ret;

  EnterCriticalSection (lock);
  ret = mbtowc_unlocked (pwc, p, m);
  LeaveCriticalSection (lock);

  return ret;
}

#elif HAVE_PTHREAD_API /* AIX, IRIX, Cygwin */

extern
# if defined _WIN32 || defined __CYGWIN__
  __declspec(dllimport)
# endif
  pthread_mutex_t *gl_get_mbtowc_lock (void);

# if HAVE_WEAK_SYMBOLS /* IRIX */

   /* Avoid the need to link with '-lpthread'.  */
#  pragma weak pthread_mutex_lock
#  pragma weak pthread_mutex_unlock

   /* Determine whether libpthread is in use.  */
#  pragma weak pthread_mutexattr_gettype
   /* See the comments in lock.h.  */
#  define pthread_in_use() \
     (pthread_mutexattr_gettype != NULL || c11_threads_in_use ())

# else
#  define pthread_in_use() 1
# endif

static int
mbtowc_with_lock (wchar_t *pwc, const char *p, size_t m)
{
  if (pthread_in_use())
    {
      pthread_mutex_t *lock = gl_get_mbtowc_lock ();
      int ret;

      if (pthread_mutex_lock (lock))
        abort ();
      ret = mbtowc_unlocked (pwc, p, m);
      if (pthread_mutex_unlock (lock))
        abort ();

      return ret;
    }
  else
    return mbtowc_unlocked (pwc, p, m);
}

#elif HAVE_THREADS_H

extern mtx_t *gl_get_mbtowc_lock (void);

static int
mbtowc_with_lock (wchar_t *pwc, const char *p, size_t m)
{
  mtx_t *lock = gl_get_mbtowc_lock ();
  int ret;

  if (mtx_lock (lock) != thrd_success)
    abort ();
  ret = mbtowc_unlocked (pwc, p, m);
  if (mtx_unlock (lock) != thrd_success)
    abort ();

  return ret;
}

#endif
