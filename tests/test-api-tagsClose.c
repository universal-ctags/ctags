/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsClose() API function for broken argument
*/

#include "readtags.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
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

	fprintf (stderr, "closing NULL...");
	if (tagsClose (NULL) == TagSuccess)
	{
		fprintf (stderr, "successful unexpectedly\n");
		return 1;
	}

	return 0;
}
