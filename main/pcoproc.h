/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   replaces popen/pclose
*/

#ifndef PCOPROC_H
#define PCOPROC_H

#include <stdio.h>

enum pcoproc_error {
	PCOPROC_SUCCESSFUL = 0,
	PCOPROC_ERROR_WPIPE,
	PCOPROC_ERROR_RPIPE,
	PCOPROC_ERROR_FORK,
	/* PCOPROC_ERROR_EXECVE  = -4, */
};

extern enum pcoproc_error pcoprocOpen (const char *filename, char *const argv[],
					FILE** readfp, FILE** writefp);
extern int  pcoprocClose (FILE* fp);

#endif	/* PCOPROC_H */
