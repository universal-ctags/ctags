# extensions-aix.m4
# serial 1
dnl Copyright (C) 2024-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# On AIX, most extensions are already enabled through the _ALL_SOURCE macro,
# defined by gl_USE_SYSTEM_EXTENSIONS.  gl_USE_AIX_EXTENSIONS additionally
# activates more GNU and Linux-like behaviours, affecting
#   - the time_t type,
#   - errno values in <errno.h>: ENOTEMPTY
#   - functions in <stdlib.h>: malloc calloc realloc valloc
#     <https://www.ibm.com/docs/en/aix/7.3?topic=m-malloc-free-realloc-calloc-mallopt-mallinfo-mallinfo-heap-alloca-valloc-posix-memalign-subroutine>
#   - functions in <string.h>: strerror_r (returns 'char *', like glibc)
#   - functions in <dirent.h>: scandir, alphasort, readdir_r
#   - functions in <netdb.h>: gethostbyname_r gethostbyaddr_r
#   - declarations in <unistd.h>: sbrk
# and a couple of secondary <sys/*> header files.

AC_DEFUN_ONCE([gl_USE_AIX_EXTENSIONS],
[
  AC_DEFINE([_LINUX_SOURCE_COMPAT], [1],
    [Define so that AIX headers are more compatible with GNU/Linux.])
])
