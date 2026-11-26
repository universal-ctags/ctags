/* A GNU-like <sched.h>.
   Copyright (C) 2008-2026 Free Software Foundation, Inc.

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

#ifndef _@GUARD_PREFIX@_SCHED_H

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

/* The include_next requires a split double-inclusion guard.  */
#if @HAVE_SCHED_H@
# if @HAVE_SYS_CDEFS_H@
#  include <sys/cdefs.h>
# endif
# @INCLUDE_NEXT@ @NEXT_SCHED_H@
#endif

#ifndef _@GUARD_PREFIX@_SCHED_H
#define _@GUARD_PREFIX@_SCHED_H

/* This file uses GNULIB_POSIXCHECK, HAVE_RAW_DECL_*.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* Get pid_t.
   This is needed on glibc 2.11 (see
   glibc bug <https://sourceware.org/PR13198>)
   and Mac OS X 10.5.  */
#include <sys/types.h>

#ifdef __KLIBC__
/* On OS/2 kLIBC, struct sched_param is in spawn.h.  */
# include <spawn.h>
#endif

#ifdef __VMS
/* On OpenVMS, struct sched_param is in <pthread.h>.  */
# include <pthread.h>
#endif

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */

/* The definition of _GL_WARN_ON_USE is copied here.  */

#if !@HAVE_STRUCT_SCHED_PARAM@

# if !GNULIB_defined_struct_sched_param
struct sched_param
{
  int sched_priority;
};
#  define GNULIB_defined_struct_sched_param 1
# endif

#endif

#if !(defined SCHED_FIFO && defined SCHED_RR && defined SCHED_OTHER)
# define SCHED_FIFO   1
# define SCHED_RR     2
# define SCHED_OTHER  0
#endif

#if @GNULIB_SCHED_YIELD@
# if @REPLACE_SCHED_YIELD@
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef sched_yield
#   define sched_yield rpl_sched_yield
#  endif
_GL_FUNCDECL_RPL (sched_yield, int, (void), );
_GL_CXXALIAS_RPL (sched_yield, int, (void));
# else
#  if !@HAVE_SCHED_YIELD@
_GL_FUNCDECL_SYS (sched_yield, int, (void), );
#  endif
_GL_CXXALIAS_SYS (sched_yield, int, (void));
# endif
# if __GLIBC__ >= 2
_GL_CXXALIASWARN (sched_yield);
# endif
#elif defined GNULIB_POSIXCHECK
# if HAVE_RAW_DECL_SCHED_YIELD
_GL_WARN_ON_USE (sched_yield, "sched_yield is not portable - "
                 "use gnulib module sched_yield for portability");
# endif
#endif

#endif /* _@GUARD_PREFIX@_SCHED_H */
#endif /* _@GUARD_PREFIX@_SCHED_H */
