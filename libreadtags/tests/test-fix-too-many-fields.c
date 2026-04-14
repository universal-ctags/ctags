/*
*   Copyright (c) 2026, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing the fix for handling unescaping
*/

#include "readtags.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

int
main (void)
{
	char *srcdir = getenv ("srcdir");
	if (srcdir)
	{
		if (chdir (srcdir) == -1)
		{
			perror ("chdir");
			return 99;
		}
	}

	tagFile *t;
	tagFileInfo info;

	const char *tags0 = "./too-many-fields.tags";
	t = tagsOpen (tags0, &info);
	if (t == NULL
		|| info.status.opened == 0)
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d)\n",
				 t, info.status.opened);
		return 1;
	}
	fprintf (stderr, "ok\n");

	tagEntry e;
	tagResult r;
	int err;

	r = tagsFirst (t, &e);
	if (r != TagFailure)
	{
		fprintf (stderr, "unexpected successful returned from tagsFirst\n");
		return 1;
	}

	err = tagsGetErrno (t);
	if (err != EOVERFLOW)
	{
		if (err > 0)
		{
			errno = err;
			perror("tagsFirst");
		}
		fprintf (stderr, "unexpected errno returned from tagsFirst: %d\n", err);
		return 1;
	}

	r = tagsClose(t);
	if (r != TagSuccess)
	{
		fprintf (stderr, "error in tagsClose\n");
		return 1;
	}

	return 0;
}
