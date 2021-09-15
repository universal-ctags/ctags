/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing the guard condition for NULL pointer dereference
*/

#include "readtags.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

	const char *tags0 = "./null-deref.tags";
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

	/* Without fix, this program crashes in tagsFirst(). */
	tagsFirst (t, &e);
	tagsClose (t);
	return 0;
}
