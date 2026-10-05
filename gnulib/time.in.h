/* A more-standard <time.h>.

   Copyright (C) 2007-2026 Free Software Foundation, Inc.

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

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* This file uses #include_next of a system file that defines time_t.
   For the 'year2038' module to work right, <config.h> needs to have been
   included before.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* Don't get in the way of glibc when it includes time.h merely to
   declare a few standard symbols, rather than to declare all the
   symbols.  (However, skip this for MinGW as it treats __need_time_t
   incompatibly.)  Also, Solaris 8 <time.h> eventually includes itself
   recursively; if that is happening, just include the system <time.h>
   without adding our own declarations.  */
#if (((defined __need_time_t || defined __need_clock_t \
       || defined __need_timespec)                     \
      && !defined __MINGW32__)                         \
     || defined _@GUARD_PREFIX@_TIME_H)

# @INCLUDE_NEXT@ @NEXT_TIME_H@

#else

# define _@GUARD_PREFIX@_TIME_H

/* mingw's <time.h> provides the functions asctime_r, ctime_r, gmtime_r,
   localtime_r only if <unistd.h> or <pthread.h> has been included before.  */
# if defined __MINGW32__
#  include <unistd.h>
# endif

# @INCLUDE_NEXT@ @NEXT_TIME_H@

/* This file uses _GL_ATTRIBUTE_DEPRECATED, GNULIB_POSIXCHECK,
   HAVE_RAW_DECL_*.  */
# if !_GL_CONFIG_H_INCLUDED
#  error "Please include config.h first."
# endif

/* NetBSD 5.0 mis-defines NULL.  */
# include <stddef.h>

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_ARG_NONNULL is copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

/* Some systems don't define struct timespec (e.g., AIX 4.1).
   Or they define it with the wrong member names or define it in <sys/time.h>
   (e.g., FreeBSD circa 1997).  Stock Mingw prior to 3.0 does not define it,
   but the pthreads-win32 library defines it in <pthread.h>.  */
# if ! @TIME_H_DEFINES_STRUCT_TIMESPEC@
#  if @SYS_TIME_H_DEFINES_STRUCT_TIMESPEC@
#   include <sys/time.h>
#  elif @PTHREAD_H_DEFINES_STRUCT_TIMESPEC@
#   include <pthread.h>
#  elif @UNISTD_H_DEFINES_STRUCT_TIMESPEC@
#   include <unistd.h>
#  else

#   ifdef __cplusplus
extern "C" {
#   endif

#   if !GNULIB_defined_struct_timespec
#    undef timespec
#    define timespec rpl_timespec
struct timespec
{
  time_t tv_sec;
  long int tv_nsec;
};
#    define GNULIB_defined_struct_timespec 1
#   endif

#   ifdef __cplusplus
}
#   endif

#  endif
# endif

# if !GNULIB_defined_struct_time_t_must_be_integral
/* https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_types.h.html
   requires time_t to be an integer type, even though C99 permits floating
   point.  We don't know of any implementation that uses floating
   point, and it is much easier to write code that doesn't have to
   worry about that corner case, so we force the issue.  */
struct __time_t_must_be_integral {
  unsigned int __floating_time_t_unsupported : (time_t) 1;
};
#  define GNULIB_defined_struct_time_t_must_be_integral 1
# endif

/* Define TIME_UTC, a positive integer constant used for timespec_get().  */
# if ! @TIME_H_DEFINES_TIME_UTC@
#  if !GNULIB_defined_TIME_UTC
#   define TIME_UTC 1
#   define GNULIB_defined_TIME_UTC 1
#  endif
# endif

# if @GNULIB_TZNAME@
/* tzname[0..1]: Abbreviated time zone names, set by the tzset() function.  */
#  if NEED_DECL_TZNAME
extern
#   ifdef __cplusplus
  "C"
#   endif
  char *tzname[];
#  endif
#  if defined _WIN32 && !defined __CYGWIN__
/* On native Windows, map 'tzname' to '_tzname' etc., so that -loldnames is not
   required.  */
#   undef tzname
#   define tzname _tzname
#  endif
# endif

/* Set *TS to the current time, and return BASE.
   Upon failure, return 0.  */
# if @GNULIB_TIMESPEC_GET@
#  if @REPLACE_TIMESPEC_GET@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef timespec_get
#    define timespec_get rpl_timespec_get
#   endif
_GL_FUNCDECL_RPL (timespec_get, int, (struct timespec *ts, int base),
                                     _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (timespec_get, int, (struct timespec *ts, int base));
#  else
#   if !@HAVE_TIMESPEC_GET@
_GL_FUNCDECL_SYS (timespec_get, int, (struct timespec *ts, int base),
                                     _GL_ARG_NONNULL ((1)));
#   endif
_GL_CXXALIAS_SYS (timespec_get, int, (struct timespec *ts, int base));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (timespec_get);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_TIMESPEC_GET
_GL_WARN_ON_USE (timespec_get, "timespec_get is unportable - "
                 "use gnulib module timespec_get for portability");
#  endif
# endif

/* Set *TS to the current time resolution, and return BASE.
   Upon failure, return 0.  */
# if @GNULIB_TIMESPEC_GETRES@
#  if @REPLACE_TIMESPEC_GETRES@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef timespec_getres
#    define timespec_getres rpl_timespec_getres
#   endif
_GL_FUNCDECL_RPL (timespec_getres, int, (struct timespec *ts, int base),
                                        _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (timespec_getres, int, (struct timespec *ts, int base));
#  else
#   if !@HAVE_TIMESPEC_GETRES@
_GL_FUNCDECL_SYS (timespec_getres, int, (struct timespec *ts, int base),
                                        _GL_ARG_NONNULL ((1)));
#   endif
_GL_CXXALIAS_SYS (timespec_getres, int, (struct timespec *ts, int base));
#  endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (timespec_getres);
# endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_TIMESPEC_GETRES
_GL_WARN_ON_USE (timespec_getres, "timespec_getres is unportable - "
                 "use gnulib module timespec_getres for portability");
#  endif
# endif

/* Return the number of seconds that have elapsed since the Epoch.  */
# if @GNULIB_TIME@
#  if @REPLACE_TIME@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define time rpl_time
#   endif
_GL_FUNCDECL_RPL (time, time_t, (time_t *__tp), );
_GL_CXXALIAS_RPL (time, time_t, (time_t *__tp));
#  else
_GL_CXXALIAS_SYS (time, time_t, (time_t *__tp));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (time);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_TIME
_GL_WARN_ON_USE (time, "time has consistency problems - "
                 "use gnulib module time for portability");
#  endif
# endif

/* Sleep for at least RQTP seconds unless interrupted,  If interrupted,
   return -1 and store the remaining time into RMTP.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/nanosleep.html>.  */
# if @GNULIB_NANOSLEEP@
#  if @REPLACE_NANOSLEEP@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define nanosleep rpl_nanosleep
#   endif
_GL_FUNCDECL_RPL (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp),
                  _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp));
#  else
#   if ! @HAVE_NANOSLEEP@
_GL_FUNCDECL_SYS (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp),
                  _GL_ARG_NONNULL ((1)));
#   endif
_GL_CXXALIAS_SYS (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp));
#  endif
_GL_CXXALIASWARN (nanosleep);
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_NANOSLEEP
_GL_WARN_ON_USE (nanosleep, "nanosleep is unportable - "
                 "use gnulib module nanosleep for portability");
#  endif
# endif

/* Initialize time conversion information.  */
# if @GNULIB_TZSET@
#  if @REPLACE_TZSET@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef tzset
#    define tzset rpl_tzset
#   endif
_GL_FUNCDECL_RPL (tzset, void, (void), );
_GL_CXXALIAS_RPL (tzset, void, (void));
#  elif defined _WIN32 && !defined __CYGWIN__
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef tzset
#    define tzset _tzset
#   endif
_GL_CXXALIAS_MDA (tzset, void, (void));
#  else
_GL_CXXALIAS_SYS (tzset, void, (void));
#  endif
_GL_CXXALIASWARN (tzset);
# elif @GNULIB_MDA_TZSET@
/* On native Windows, map 'tzset' to '_tzset', so that -loldnames is not
   required.  In C++ with GNULIB_NAMESPACE, avoid differences between
   platforms by defining GNULIB_NAMESPACE::tzset always.  */
#  if defined _WIN32 && !defined __CYGWIN__
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef tzset
#    define tzset _tzset
#   endif
_GL_CXXALIAS_MDA (tzset, void, (void));
#  else
_GL_CXXALIAS_SYS (tzset, void, (void));
#  endif
_GL_CXXALIASWARN (tzset);
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_TZSET
_GL_WARN_ON_USE (tzset, "tzset has portability problems - "
                 "use gnulib module tzset for portability");
#  endif
# endif

/* Return the 'time_t' representation of TP and normalize TP.  */
# if @GNULIB_MKTIME@
#  if @REPLACE_MKTIME@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define mktime rpl_mktime
#   endif
_GL_FUNCDECL_RPL (mktime, time_t, (struct tm *__tp), _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (mktime, time_t, (struct tm *__tp));
#  else
_GL_CXXALIAS_SYS (mktime, time_t, (struct tm *__tp));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (mktime);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_MKTIME
_GL_WARN_ON_USE (mktime, "mktime has portability problems - "
                 "use gnulib module mktime for portability");
#  endif
# endif

/* Convert TIMER to RESULT, assuming local time and UTC respectively.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/localtime_r.html> and
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/gmtime_r.html>.  */
# if @GNULIB_TIME_R@
#  if @REPLACE_LOCALTIME_R@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef localtime_r
#    define localtime_r rpl_localtime_r
#   endif
_GL_FUNCDECL_RPL (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result),
                                            _GL_ARG_NONNULL ((1, 2)));
_GL_CXXALIAS_RPL (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result));
#  else
#   if ! @HAVE_DECL_LOCALTIME_R@
_GL_FUNCDECL_SYS (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result),
                                            _GL_ARG_NONNULL ((1, 2)));
#   endif
_GL_CXXALIAS_SYS (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result));
#  endif
#  if @HAVE_DECL_LOCALTIME_R@
_GL_CXXALIASWARN (localtime_r);
#  endif
#  if @REPLACE_LOCALTIME_R@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef gmtime_r
#    define gmtime_r rpl_gmtime_r
#   endif
_GL_FUNCDECL_RPL (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result),
                                         _GL_ARG_NONNULL ((1, 2)));
_GL_CXXALIAS_RPL (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result));
#  else
#   if ! @HAVE_DECL_LOCALTIME_R@
_GL_FUNCDECL_SYS (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result),
                                         _GL_ARG_NONNULL ((1, 2)));
#   endif
_GL_CXXALIAS_SYS (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result));
#  endif
#  if @HAVE_DECL_LOCALTIME_R@
_GL_CXXALIASWARN (gmtime_r);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_LOCALTIME_R
_GL_WARN_ON_USE (localtime_r, "localtime_r is unportable - "
                 "use gnulib module time_r for portability");
#  endif
#  if HAVE_RAW_DECL_GMTIME_R
_GL_WARN_ON_USE (gmtime_r, "gmtime_r is unportable - "
                 "use gnulib module time_r for portability");
#  endif
# endif

/* Convert TIMER to RESULT, assuming local time and UTC respectively.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/localtime.html> and
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/gmtime.html>.  */
# if @GNULIB_LOCALTIME@ || @REPLACE_LOCALTIME@
#  if @REPLACE_LOCALTIME@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef localtime
#    define localtime rpl_localtime
#   endif
_GL_FUNCDECL_RPL (localtime, struct tm *, (time_t const *__timer),
                                          _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (localtime, struct tm *, (time_t const *__timer));
#  else
_GL_CXXALIAS_SYS (localtime, struct tm *, (time_t const *__timer));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (localtime);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_LOCALTIME
_GL_WARN_ON_USE (localtime, "localtime has portability problems - "
                 "use gnulib module localtime for portability");
#  endif
# endif

# if 0 || @REPLACE_GMTIME@
#  if @REPLACE_GMTIME@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef gmtime
#    define gmtime rpl_gmtime
#   endif
_GL_FUNCDECL_RPL (gmtime, struct tm *, (time_t const *__timer),
                                       _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (gmtime, struct tm *, (time_t const *__timer));
#  else
_GL_CXXALIAS_SYS (gmtime, struct tm *, (time_t const *__timer));
#  endif
_GL_CXXALIASWARN (gmtime);
# endif

/* Parse BUF as a timestamp, assuming FORMAT specifies its layout, and store
   the resulting broken-down time into TM.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html>.  */
# if @GNULIB_STRPTIME@
#  if ! @HAVE_STRPTIME@
_GL_FUNCDECL_SYS (strptime, char *, (char const *restrict __buf,
                                     char const *restrict __format,
                                     struct tm *restrict __tm),
                                    _GL_ARG_NONNULL ((1, 2, 3)));
#  endif
_GL_CXXALIAS_SYS (strptime, char *, (char const *restrict __buf,
                                     char const *restrict __format,
                                     struct tm *restrict __tm));
_GL_CXXALIASWARN (strptime);
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_STRPTIME
_GL_WARN_ON_USE (strptime, "strptime is unportable - "
                 "use gnulib module strptime for portability");
#  endif
# endif

/* Convert *TP to a date and time string.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/ctime.html>.  */
# if @GNULIB_CTIME@
#  if @REPLACE_CTIME@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define ctime rpl_ctime
#   endif
#   ifndef __cplusplus
_GL_ATTRIBUTE_DEPRECATED
#   endif
_GL_FUNCDECL_RPL (ctime, char *, (time_t const *__tp),
                                 _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (ctime, char *, (time_t const *__tp));
#  else
_GL_CXXALIAS_SYS (ctime, char *, (time_t const *__tp));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (ctime);
#  endif
# elif defined GNULIB_POSIXCHECK
/* No need to warn about portability, as a more serious warning is below.  */
# endif

/* Convert *TP to a date and time string.  See
   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html>.  */
# if @GNULIB_STRFTIME@
#  if @REPLACE_STRFTIME@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define strftime rpl_strftime
#   endif
_GL_FUNCDECL_RPL (strftime, size_t,
                  (char *restrict __buf, size_t __bufsize,
                   const char *restrict __fmt, const struct tm *restrict __tp),
                  _GL_ARG_NONNULL ((1, 3, 4)));
_GL_CXXALIAS_RPL (strftime, size_t,
                  (char *restrict __buf, size_t __bufsize,
                   const char *restrict __fmt, const struct tm *restrict __tp));
#  else
_GL_CXXALIAS_SYS (strftime, size_t,
                  (char *restrict __buf, size_t __bufsize,
                   const char *restrict __fmt, const struct tm *restrict __tp));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (strftime);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_STRFTIME
_GL_WARN_ON_USE (strftime, "strftime has portability problems - "
                 "use gnulib module strftime-fixes for portability");
#  endif
# endif

# if @GNULIB_TIME_RZ@
/* Functions that use a first-class time zone data type, instead of
   relying on an implicit global time zone.
   Inspired by NetBSD.  */

/* Represents a time zone.
   (timezone_t) NULL stands for UTC.  */
#  if !@HAVE_TZALLOC@
#   if !GNULIB_defined_timezone_t
#    if !@HAVE_TIMEZONE_T@
typedef struct tm_zone *timezone_t;
#    else
typedef struct tm_zone *rpl_timezone_t;
#     define timezone_t rpl_timezone_t
#    endif
#    define GNULIB_defined_timezone_t 1
#   endif
#  endif

/* tzalloc (name)
   Returns a time zone object for the given time zone NAME.  This object
   represents the time zone that other functions would use it the TZ
   environment variable was set to NAME.
   If NAME is NULL, the result represents the time zone that other functions
   would use it the TZ environment variable was unset.
   May return NULL if NAME is invalid (this is platform dependent) or
   upon memory allocation failure.  */
#  if !@HAVE_TZALLOC@
_GL_FUNCDECL_SYS (tzalloc, timezone_t, (char const *__name), );
_GL_CXXALIAS_SYS (tzalloc, timezone_t, (char const *__name));
#  endif

/* tzfree (tz)
   Free a time zone object, preserving errno.
   The argument must have been returned by tzalloc().  */
#  if !@HAVE_TZALLOC@
_GL_FUNCDECL_SYS (tzfree, void, (timezone_t __tz), );
_GL_CXXALIAS_SYS (tzfree, void, (timezone_t __tz));
#  else
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef tzfree
#    define tzfree rpl_tzfree
#   endif
_GL_FUNCDECL_RPL (tzfree, void, (timezone_t __tz), );
_GL_CXXALIAS_RPL (tzfree, void, (timezone_t __tz));
#  endif

/* localtime_rz (tz, &t, &result)
   Converts an absolute time T to a broken-down time RESULT, assuming the
   time zone TZ.
   This function is like 'localtime_r', but relies on the argument TZ instead
   of an implicit global time zone.  */
#  if @REPLACE_LOCALTIME_RZ@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef localtime_rz
#    define localtime_rz rpl_localtime_rz
#   endif
_GL_FUNCDECL_RPL (localtime_rz, struct tm *,
                  (timezone_t __tz, time_t const *restrict __timer,
                   struct tm *restrict __result),
                  _GL_ARG_NONNULL ((2, 3)));
_GL_CXXALIAS_RPL (localtime_rz, struct tm *,
                  (timezone_t __tz, time_t const *restrict __timer,
                   struct tm *restrict __result));
#  else
#   if !@HAVE_TZALLOC@
_GL_FUNCDECL_SYS (localtime_rz, struct tm *,
                  (timezone_t __tz, time_t const *restrict __timer,
                   struct tm *restrict __result),
                  _GL_ARG_NONNULL ((2, 3)));
#   endif
_GL_CXXALIAS_SYS (localtime_rz, struct tm *,
                  (timezone_t __tz, time_t const *restrict __timer,
                   struct tm *restrict __result));
#  endif

/* mktime_z (tz, &tm)
   Normalizes the broken-down time TM and converts it to an absolute time,
   assuming the time zone TZ.  Returns the absolute time.
   This function is like 'mktime', but relies on the argument TZ instead
   of an implicit global time zone.  */
#  if @REPLACE_MKTIME_Z@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef mktime_z
#    define mktime_z rpl_mktime_z
#   endif
_GL_FUNCDECL_RPL (mktime_z, time_t,
                  (timezone_t __tz, struct tm *restrict __tm),
                  _GL_ARG_NONNULL ((2)));
_GL_CXXALIAS_RPL (mktime_z, time_t,
                  (timezone_t __tz, struct tm *restrict __tm));
#  else
#   if !@HAVE_TZALLOC@
_GL_FUNCDECL_SYS (mktime_z, time_t,
                  (timezone_t __tz, struct tm *restrict __tm),
                  _GL_ARG_NONNULL ((2)));
#   endif
_GL_CXXALIAS_SYS (mktime_z, time_t,
                  (timezone_t __tz, struct tm *restrict __tm));
#  endif

/* Time zone abbreviation strings (returned by 'localtime_rz' or 'mktime_z'
   in the 'tm_zone' member of 'struct tm') are valid as long as
     - the 'struct tm' argument is not destroyed or overwritten,
   and
     - the 'timezone_t' argument is not freed through tzfree().  */

# endif

/* Convert TM to a time_t value, assuming UTC.  */
# if @GNULIB_TIMEGM@
#  if @REPLACE_TIMEGM@
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef timegm
#    define timegm rpl_timegm
#   endif
_GL_FUNCDECL_RPL (timegm, time_t, (struct tm *__tm), _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (timegm, time_t, (struct tm *__tm));
#  else
#   if ! @HAVE_TIMEGM@
_GL_FUNCDECL_SYS (timegm, time_t, (struct tm *__tm), _GL_ARG_NONNULL ((1)));
#   endif
_GL_CXXALIAS_SYS (timegm, time_t, (struct tm *__tm));
#  endif
#  if __GLIBC__ >= 2
_GL_CXXALIASWARN (timegm);
#  endif
# elif defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_TIMEGM
_GL_WARN_ON_USE (timegm, "timegm is unportable - "
                 "use gnulib module timegm for portability");
#  endif
# endif

/* Encourage applications to avoid unsafe functions that can overrun
   buffers when given outlandish struct tm values.  Portable
   applications should use strftime (or even sprintf) instead.  */
# if defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_ASCTIME
_GL_WARN_ON_USE (asctime, "asctime can overrun buffers in some cases - "
                 "better use strftime (or even sprintf) instead");
#  endif
# endif
# if defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_ASCTIME_R
_GL_WARN_ON_USE (asctime_r, "asctime_r can overrun buffers in some cases - "
                 "better use strftime (or even sprintf) instead");
#  endif
# endif
# if defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_CTIME
_GL_WARN_ON_USE (ctime, "ctime can overrun buffers in some cases - "
                 "better use strftime (or even sprintf) instead");
#  endif
# endif
# if defined GNULIB_POSIXCHECK
#  if HAVE_RAW_DECL_CTIME_R
_GL_WARN_ON_USE (ctime_r, "ctime_r can overrun buffers in some cases - "
                 "better use strftime (or even sprintf) instead");
#  endif
# endif

#endif
