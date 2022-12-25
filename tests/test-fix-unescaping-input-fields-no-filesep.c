/*
*   Copyright (c) 2022, Masatake YAMATO
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

#include "test-fields.h"


#define CHECK3(NAME,FILE,PAT)					\
	CHECK ((NAME), name);						\
	CHECK ((FILE), file);						\
	CHECK ((PAT),  address.pattern)

#define NEXT_CHECK3(NAME,FILE,PAT)				\
	NEXT ();									\
	CHECK3 (NAME,FILE,PAT)

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

	const char *tags0 = "./unescaping-input-fields-no-filesep.tags";
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

	CHECK3 ("tab0", "\\tabc", "/^tab0$/");
	NEXT_CHECK3 ("tab1", "a\\tbc", "/^tab1$/");
	NEXT_CHECK3 ("tab2", "ab\\tc", "/^tab2$/");
	NEXT_CHECK3 ("tab3", "abc\\t", "/^tab3$/");
	NEXT_CHECK3 ("tab4", "\\\\abc", "/^tab4$/");
	NEXT_CHECK3 ("tab5", "a\\\\bc", "/^tab5$/");
	NEXT_CHECK3 ("tab6", "ab\\\\c", "/^tab6$/");
	NEXT_CHECK3 ("tab7", "abc\\\\", "/^tab7$/");
	NEXT_CHECK3 ("tab8", "\\nabc", "/^tab8$/");
	NEXT_CHECK3 ("tab9", "a\\nbc", "/^tab9$/");
	NEXT_CHECK3 ("taba", "ab\\nc", "/^taba$/");
	NEXT_CHECK3 ("tabb", "abc\\n", "/^tabb$/");
	NEXT_CHECK3 ("tabc", "\\n\\\\abc", "/^tabc$/");
	NEXT_CHECK3 ("tabd", "\\\\\\nabc", "/^tabd$/");
	NEXT_CHECK3 ("tabe", "a\\n\\\\bc", "/^tabe$/");
	NEXT_CHECK3 ("tabf", "a\\\\\\nbc", "/^tabf$/");
	NEXT_CHECK3 ("tabg", "abc\\n\\\\", "/^tabg$/");
	NEXT_CHECK3 ("tabh", "abc\\\\\\n", "/^tabh$/");
	NEXT_CHECK3 ("tabi", "\\t\\\\\\n",  "/^tabi$/");
	NEXT_CHECK3 ("tabj", "ab\\t\\\\\\nc",  "/^tabj$/");

	r = tagsClose(t);
	if (r != TagSuccess)
	{
		fprintf (stderr, "error in tagsClose\n");
		return 1;
	}

	return 0;
}
