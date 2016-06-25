/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   replaces popen/pclose
*/
#include "general.h"
#include "pcoproc.h"

#ifdef MAIN
/*
 * PATH=/usr/bin gcc -O0 -g -Wall pcoproc.c htable.c -DMAIN
 */
#include <stdlib.h>
#ifndef xCalloc
#define xCalloc(n,Type)    (Type *)calloc((size_t)(n), sizeof (Type))
#endif
#ifndef xMalloc
#define xMalloc(n,Type)    (Type *)malloc((size_t)(n) * sizeof (Type))
#endif
#ifndef eFree
#define eFree(x) free(x)
#endif
#else
#include "routines.h"
#endif	/* MAIN */

#if defined (HAVE_COPROC) || defined (MAIN)
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "htable.h"


#define W_PARENT_EPT 1
#define W_CHILD_EPT  0
#define R_PARENT_EPT 0
#define R_CHILD_EPT  1

static hashTable *pid_table;

struct pid_wrapper
{
	int ref_count;
	pid_t pid;
};

static void pid_wrapper_free (void *ptr)
{
	struct pid_wrapper *pid = ptr;

	pid->ref_count--;
	if (pid->ref_count == 0)
		eFree (ptr);
}

static void pidtable_put (FILE* fp, struct pid_wrapper* pid)
{
	if (pid_table == NULL)
		pid_table = hashTableNew (7,
					  hashPtrhash,
					  hashPtreq,
					  (void (*)(void *))fclose,
					  pid_wrapper_free);

	hashTablePutItem (pid_table, fp, pid);
}

static enum pcoprocError
coproc (const char *filename, char *const argv[], pid_t *pid, int *for_reading_from_coproc, int *for_writing_to_coproc)
{
	int wpipe[2];
	int rpipe[2];

	if (for_writing_to_coproc)
	{
		if (pipe(wpipe) < 0)
			return PCOPROC_ERROR_WPIPE;
	}
	if (for_reading_from_coproc)
	{
		if (pipe(rpipe) < 0)
		{
			if (for_writing_to_coproc)
			{
				close (wpipe[0]);
				close (wpipe[1]);
			}
			return PCOPROC_ERROR_RPIPE;
		}
	}


	*pid = fork ();
	if (*pid < 0)
	{
		if (for_writing_to_coproc)
		{
			close (wpipe[0]);
			close (wpipe[1]);
		}
		if (for_reading_from_coproc)
		{
			close (rpipe[0]);
			close (rpipe[1]);
		}
		return PCOPROC_ERROR_FORK;
	}
	else if (*pid == 0)
	{			/* child */

		int i;
		if (for_writing_to_coproc)
		{
			close (wpipe[W_PARENT_EPT]);
			if (wpipe[W_CHILD_EPT] != STDIN_FILENO)
			{
				dup2 (wpipe[W_CHILD_EPT], STDIN_FILENO);
				close (wpipe[W_CHILD_EPT]);
			}
		}
		if (for_reading_from_coproc)
		{
			close (rpipe[R_PARENT_EPT]);
			if (rpipe[R_CHILD_EPT] != STDOUT_FILENO)
			{
				dup2 (rpipe[R_CHILD_EPT], STDOUT_FILENO);
				close (rpipe[R_CHILD_EPT]);
			}
		}

		for (i = 0; i < 1024; i++)
			if (i != STDIN_FILENO && i != STDOUT_FILENO && i != STDERR_FILENO)
				close (i);

		execv (filename, argv);
		_exit(127);
		return PCOPROC_ERROR_FORK; /* meaningless */
	}
	else
	{			/* parent */
		if (for_writing_to_coproc)
		{
			close (wpipe[W_CHILD_EPT]);
			*for_writing_to_coproc = wpipe[W_PARENT_EPT];
		}
		if (for_reading_from_coproc)
		{
			close (rpipe[R_CHILD_EPT]);
			*for_reading_from_coproc = rpipe[R_PARENT_EPT];
		}

		return 0;
	}

}

extern enum pcoprocError pcoprocOpen (const char *filename, char *const argv[],
					FILE** readfp, FILE** writefp)
{
	struct pid_wrapper *value;

	pid_t pid;
	int for_reading_from_coproc, for_writing_from_coproc;
	enum pcoprocError r;

	r = coproc (filename, argv, &pid,
		    readfp? &for_reading_from_coproc: NULL,
		    writefp? &for_writing_from_coproc: NULL);

	if (r != PCOPROC_SUCCESSFUL)
		return r;

	value = xMalloc (1, struct pid_wrapper);
	value->pid = pid;
	value->ref_count = 0;
	if (writefp)
	{
		*writefp = fdopen (for_writing_from_coproc, "w");
		if (*writefp)
		{
			value->ref_count++;
			pidtable_put (*writefp, value);
		}
		else
			perror("fdopen(w)");

	}
	if (readfp)
	{
		*readfp = fdopen (for_reading_from_coproc, "r");
		if (*readfp)
		{
			value->ref_count++;
			pidtable_put (*readfp, value);
		}
		else
			perror ("fdopen(r)");
	}

	if (value->ref_count == 0)
	{
		eFree (value);
		/* TODO: all perror should be replaced with error */
	}
	return r;
}

extern int  pcoprocClose (FILE* fp)
{
	struct pid_wrapper* pid_wrapper;
	pid_t pid;
	int stat;
	int ref_count;

	if (!pid_table)
		return -1;

	pid_wrapper = hashTableGetItem (pid_table, fp);
	if (pid_wrapper)
	{
		pid = pid_wrapper->pid;
		ref_count = pid_wrapper->ref_count;

		hashTableDeleteItem (pid_table, fp);
		if (ref_count == 1)
		{
			while (waitpid(pid, &stat, 0) < 0)
			{
				if (errno != EINTR)
					return -1;
			}
			return stat;
		}
	}
	return -2;
}
#else
extern enum pcoprocError pcoprocOpen (const char *filename, char *const argv[],
					FILE** readfp, FILE** writefp)
{
	*readfp = NULL;
	*writefp = NULL;
	return PCOPROC_SUCCESSFUL;
}
extern int  pcoprocClose (FILE* fp)
{
	return 1;
}
#endif


#ifdef MAIN
int
main (int argc, char** argv)
{
	FILE *in, *out;
	char* const a[] = {
		"/bin/tr",
		"a-z",
		"A-Z",
		NULL
	};
	enum pcoprocError r;
	r = pcoprocOpen ("/bin/tr", a, &in, &out);
	switch (r) {
	case PCOPROC_ERROR_WPIPE:
		perror ("wpipe");
		return 1;
	case PCOPROC_ERROR_RPIPE:
		perror("rpipe");
		return 1;
	case PCOPROC_ERROR_FORK:
		perror("fork");
		return 1;
	case PCOPROC_SUCCESSFUL:
		break;
	}
	fprintf(out, "abc\n");
	fflush (out);
	pcoprocClose (out);

	int c;
	while ((c = fgetc(in)) != EOF)
		putchar (c);

	return pcoprocClose (in);
}
#endif
