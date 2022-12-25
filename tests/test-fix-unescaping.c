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

#include "test-fields.h"


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

	NEXT ();
	CHECK ("\x01level1", name);
	CHECK ("input.rst", file);
	CHECK ("/^\x01level1$/", address.pattern);
	CHECK ("c", kind);

	NEXT ();
	CHECK ("level2", name);
	CHECK ("input.rst", file);
	CHECK ("/^level2$/", address.pattern);
	CHECK ("s", kind);
	CHECK_X ("scope", "chapter:\x01level1");

	NEXT ();
	CHECK ("!level3\x03", name);
	CHECK ("input.rst", file);
	CHECK ("/^!level3\x03$/", address.pattern);
	CHECK ("S", kind);
	CHECK_X ("scope", "section:level2");

	NEXT ();
	CHECK ("!level1+", name);
	CHECK ("input.rst", file);
	CHECK ("/^!level1+$/", address.pattern);
	CHECK ("c", kind);

	NEXT ();
	CHECK ("level2+", name);
	CHECK ("input.rst", file);
	CHECK ("/^level2+$/", address.pattern);
	CHECK ("s", kind);
	CHECK_X ("scope", "chapter:!level1+");

	NEXT ();
	CHECK ("\x02level3+\x04", name);
	CHECK ("input.rst", file);
	CHECK ("/^\x02level3+\x04$/", address.pattern);
	CHECK ("S", kind);
	CHECK_X ("scope", "section:level2+");

	NEXT ();
	CHECK ("ClassFour", name);
	CHECK ("input.php", file);
	CHECK ("/^  use NS2\\\\{NS30 as NameSpaceTreePointO, NS31\\\\Cls4 as ClassFour};$/", address.pattern);
	CHECK ("a", kind);
	CHECK_X ("typeref", "unknown:NS2\\NS31\\Cls4");

	r = tagsClose(t);
	if (r != TagSuccess)
	{
		fprintf (stderr, "error in tagsClose\n");
		return 1;
	}

	return 0;
}
