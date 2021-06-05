# pthread_rwlock_rdlock.m4 serial 4
dnl Copyright (C) 2017-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Inspired by
dnl https://github.com/linux-test-project/ltp/blob/master/testcases/open_posix_testsuite/conformance/interfaces/pthread_rwlock_rdlock/2-2.c
dnl by Intel Corporation.

dnl Test whether in a situation where
dnl   - an rwlock is taken by a reader and has a writer waiting,
dnl   - an additional reader requests the lock,
dnl   - the waiting writer and the requesting reader threads have the same
dnl     priority,
dnl the requesting reader thread gets blocked, so that at some point the
dnl waiting writer can acquire the lock.
dnl Without such a guarantee, when there a N readers and each of the readers
dnl spends more than 1/Nth of the time with the lock held, there is a high
dnl probability that the waiting writer will not get the lock in a given finite
dnl time, a phenomenon called "writer starvation".
dnl Without such a guarantee, applications have a hard time avoiding writer
dnl starvation.
dnl
dnl POSIX:2017 makes this requirement only for implementations that support TPS
dnl (Thread Priority Scheduling) and only for the scheduling policies SCHED_FIFO
dnl and SCHED_RR, see
dnl https://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_rwlock_rdlock.html
dnl but this test verifies the guarantee regardless of TPS and regardless of
dnl scheduling policy.
dnl Glibc does not provide this guarantee (and never will on Linux), see
dnl https://sourceware.org/bugzilla/show_bug.cgi?id=13701
dnl https://bugzilla.redhat.com/show_bug.cgi?id=1410052
AC_DEFUN([gl_PTHREAD_RWLOCK_RDLOCK_PREFER_WRITER],
[
  AC_REQUIRE([gl_THREADLIB_EARLY])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether pthread_rwlock_rdlock prefers a writer to a reader],
    [gl_cv_pthread_rwlock_rdlock_prefer_writer],
    [save_LIBS="$LIBS"
     LIBS="$LIBS $LIBMULTITHREAD"
     AC_RUN_IFELSE(
       [AC_LANG_SOURCE([[
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

#define SUCCEED() exit (0)
#define FAILURE() exit (1)
#define UNEXPECTED(n) (exit (10 + (n)))

/* The main thread creates the waiting writer and the requesting reader threads
   in the default way; this guarantees that they have the same priority.
   We can reuse the main thread as first reader thread.  */

static pthread_rwlock_t lock;
static pthread_t reader1;
static pthread_t writer;
static pthread_t reader2;
static pthread_t timer;
/* Used to pass control from writer to reader2 and from reader2 to timer,
   as in a relay race.
   Passing control from one running thread to another running thread
   is most likely faster than to create the second thread.  */
static pthread_mutex_t baton;

static void *
timer_func (void *ignored)
{
  /* Step 13 (can be before or after step 12):
     The timer thread takes the baton, then waits a moment to make sure
     it can tell whether the second reader thread is blocked at step 12.  */
  if (pthread_mutex_lock (&baton))
    UNEXPECTED (13);
  usleep (100000);
  /* By the time we get here, it's clear that the second reader thread is
     blocked at step 12.  This is the desired behaviour.  */
  SUCCEED ();
}

static void *
reader2_func (void *ignored)
{
  int err;

  /* Step 8 (can be before or after step 7):
     The second reader thread takes the baton, then waits a moment to make sure
     the writer thread has reached step 7.  */
  if (pthread_mutex_lock (&baton))
    UNEXPECTED (8);
  usleep (100000);
  /* Step 9: The second reader thread requests the lock.  */
  err = pthread_rwlock_tryrdlock (&lock);
  if (err == 0)
    FAILURE ();
  else if (err != EBUSY)
    UNEXPECTED (9);
  /* Step 10: Launch a timer, to test whether the next call blocks.  */
  if (pthread_create (&timer, NULL, timer_func, NULL))
    UNEXPECTED (10);
  /* Step 11: Release the baton.  */
  if (pthread_mutex_unlock (&baton))
    UNEXPECTED (11);
  /* Step 12: The second reader thread requests the lock.  */
  err = pthread_rwlock_rdlock (&lock);
  if (err == 0)
    FAILURE ();
  else
    UNEXPECTED (12);
}

static void *
writer_func (void *ignored)
{
  /* Step 4: Take the baton, so that the second reader thread does not go ahead
     too early.  */
  if (pthread_mutex_lock (&baton))
    UNEXPECTED (4);
  /* Step 5: Create the second reader thread.  */
  if (pthread_create (&reader2, NULL, reader2_func, NULL))
    UNEXPECTED (5);
  /* Step 6: Release the baton.  */
  if (pthread_mutex_unlock (&baton))
    UNEXPECTED (6);
  /* Step 7: The writer thread requests the lock.  */
  if (pthread_rwlock_wrlock (&lock))
    UNEXPECTED (7);
  return NULL;
}

int
main ()
{
  reader1 = pthread_self ();

  /* Step 1: The main thread initializes the lock and the baton.  */
  if (pthread_rwlock_init (&lock, NULL))
    UNEXPECTED (1);
  if (pthread_mutex_init (&baton, NULL))
    UNEXPECTED (1);
  /* Step 2: The main thread acquires the lock as a reader.  */
  if (pthread_rwlock_rdlock (&lock))
    UNEXPECTED (2);
  /* Step 3: Create the writer thread.  */
  if (pthread_create (&writer, NULL, writer_func, NULL))
    UNEXPECTED (3);
  /* Job done.  Go to sleep.  */
  for (;;)
    {
      sleep (1);
    }
}
]])],
       [gl_cv_pthread_rwlock_rdlock_prefer_writer=yes],
       [gl_cv_pthread_rwlock_rdlock_prefer_writer=no],
       [case "$host_os" in
                         # Guess no on glibc systems.
          *-gnu* | gnu*) gl_cv_pthread_rwlock_rdlock_prefer_writer="guessing no" ;;
                         # Guess no on musl systems.
          *-musl*)       gl_cv_pthread_rwlock_rdlock_prefer_writer="guessing no" ;;
                         # Guess no on bionic systems.
          *-android*)    gl_cv_pthread_rwlock_rdlock_prefer_writer="guessing no" ;;
                         # Guess yes on native Windows with the mingw-w64 winpthreads library.
                         # Guess no on native Windows with the gnulib windows-rwlock module.
          mingw*)        if test "$gl_use_threads" = yes || test "$gl_use_threads" = posix; then
                           gl_cv_pthread_rwlock_rdlock_prefer_writer="guessing yes"
                         else
                           gl_cv_pthread_rwlock_rdlock_prefer_writer="guessing no"
                         fi
                         ;;
                         # If we don't know, obey --enable-cross-guesses.
          *)             gl_cv_pthread_rwlock_rdlock_prefer_writer="$gl_cross_guess_normal" ;;
         esac
       ])
     LIBS="$save_LIBS"
    ])
  case "$gl_cv_pthread_rwlock_rdlock_prefer_writer" in
    *yes)
      AC_DEFINE([HAVE_PTHREAD_RWLOCK_RDLOCK_PREFER_WRITER], [1],
        [Define if the 'pthread_rwlock_rdlock' function prefers a writer to a reader.])
      ;;
  esac
])
