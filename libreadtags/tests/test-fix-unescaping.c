/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing the fix for handling unescaping
*/

#include "readtags.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define NEXT()										\
	r = tagsNext (t, &e);							\
	if (r != TagSuccess)							\
	{												\
		fprintf (stderr, "error in tagsNext\n");	\
		return 1;									\
	}												\
	do {} while (0)

#define CHECK(EXP,FIELD)												\
	if (strcmp (EXP, e.FIELD) != 0)										\
	{																	\
		fprintf (stderr, "unexpected " #FIELD "(expected: %s, actual: %s) in tagsFirst\n", \
				 EXP, e.FIELD);											\
		return 1;														\
	} \
	do {} while (0)

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

	const char *tags0 = "./unescaping.tags";
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

	r = tagsFirst (t, &e);
	if (r != TagSuccess)
	{
		fprintf (stderr, "error in tagsFirst\n");
		return 1;
	}
	CHECK ("aa", name);
	CHECK ("main/Makefile", file);
	CHECK ("/^%:$/", address.pattern);
	CHECK ("t", kind);

	NEXT ();
	CHECK ("\taa", name);
	CHECK ("parsers/Makefile", file);
	CHECK ("/^%:$/", address.pattern);
	CHECK ("t", kind);

	NEXT ();
	CHECK ("bb", name);
	CHECK ("main/Makefile", file);
	CHECK ("/^%:$/", address.pattern);
	CHECK ("t", kind);

	NEXT ();
	CHECK ("\\\aa", name);
	CHECK ("parsers/cxx/Makefile", file);
	CHECK ("/^%:$/", address.pattern);
	CHECK ("t", kind);

	if (r != TagSuccess)
	{
		fprintf (stderr, "error in tagsClose\n");
		return 1;
	}

	return 0;
}
