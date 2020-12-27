/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsSetSort() API function
*/

#include "readtags.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

static int
check (tagFile *t, tagSortType method)
{
	fprintf (stderr, "set the sort method to %d...", method);
	if (tagsSetSortType (t, method) != TagSuccess)
	{
		fprintf (stderr, "failed unexpectedly\n");
		return 1;
	}
	fprintf (stderr, "ok\n");
	return 0;
}

static int
check_invalid (tagFile *t, tagSortType method, int error_expected)
{
	fprintf (stderr, "passing invalid value (%d) as a sort method...", method);
	if (tagsSetSortType (t, method) == TagSuccess)
	{
		fprintf (stderr, "successful unexpectedly\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	int err = tagsGetErrno (t);
	fprintf (stderr, "verifying the error number...");
	if (err != error_expected)
	{
		fprintf (stderr, "unexpected error number (actual: %d, expected: %d)\n",
				 err, error_expected);
		return 1;
	}
	fprintf (stderr, "ok\n");
	return 0;
}

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

	const char *sorted_tags = "./duplicated-names--sorted-yes.tags";
	fprintf (stderr, "opening a tags file (%s)...", sorted_tags);
	t = tagsOpen (sorted_tags, &info);
	if (t == NULL
		|| info.status.opened == 0)
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d)\n",
				 t, info.status.opened);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "verify sort type defined in the tags file...");
	if (info.file.sort != TAG_SORTED)
	{
		fprintf (stderr, "unexpected result: %d (expected: %d)\n",
				 info.file.sort, TAG_SORTED);
		return 1;
	}
	fprintf (stderr, "ok (%d)\n", info.file.sort);

	tagSortType methods[] = {
		TAG_FOLDSORTED,
		TAG_UNSORTED,
		TAG_SORTED,
	};

	for (int i = 0; i < sizeof (methods)/sizeof (methods[0]); i++)
		if (check (t, methods [i]))
			return 1;

	struct expectation {
		int method;
		int err;
	} invalid_methods [] = {
		{
			.method = -1,
			.err = TagErrnoUnexpectedSortedMethod,
		},
		{
			/* Once an invalid argument is passed, calling the API function with the
			 * same tagFile is failed always. tagsSetSortType() is not an exception. */
			.method = TAG_SORTED,
			.err  = TagErrnoInvalidArgument,
		},
	};

	for (int i = 0; i < sizeof (invalid_methods)/sizeof (invalid_methods[0]); i++)
		if (check_invalid (t, invalid_methods [i].method, invalid_methods [i].err))
			return 1;

	fprintf (stderr, "closing the tag file...");
	if (tagsClose (t) != TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	return 0;
}
