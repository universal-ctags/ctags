/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsFindPseudoTag() API function
*/

#include "readtags.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


struct expectation {
	char *name;
	char *file;
	char *pattern;
};

#define COUNT(x) (sizeof(x)/sizeof(x[0]))

static int
check_finding0 (tagFile *t, struct expectation *expectations, int count, int match)
{
	tagEntry e;
	struct expectation *x;
	int err;

	for (int i = 0; i < count; i++)
	{
		x = expectations + i;
		fprintf (stderr, "[%d/%d] finding ptags in %s matching...", i + 1, count,
				 match == TAG_FULLMATCH? "full": "partial");
		if (tagsFindPseudoTag (t, &e, x->name, match) != TagSuccess)
		{
			if ((err = tagsGetErrno (t)))
				fprintf (stderr, "error: %d\n", err);
			else
				fprintf (stderr, "cannot find: %s\n", x->name);
			return 1;
		}
		fprintf (stderr, "found\n");

		fprintf (stderr, "checking name field...");
		if (match == TAG_FULLMATCH)
		{
			if (!(e.name && strcmp (x->name, e.name) == 0))
			{
				fprintf (stderr, "unexpected: %s\n", e.name? e.name: "<NULL>");
				return 1;
			}
		}
		else
		{
			if (!(e.name && strncmp (x->name, e.name, strlen(x->name)) == 0))
			{
				fprintf (stderr, "unexpected: strncmp(%s, %s)\n",
						 x->name, e.name? e.name: "<NULL>");
				return 1;
			}
		}
		fprintf (stderr, "ok\n");

		fprintf (stderr, "checking file field...");
		if (!(e.file && strcmp (x->file, e.file) == 0))
		{
			fprintf (stderr, "unexpected: %s\n", e.file? e.file: "<NULL>");
			return 1;
		}
		fprintf (stderr, "ok\n");

		fprintf (stderr, "checking pattern field...");
		if (!(e.address.pattern && strcmp (x->pattern, e.address.pattern) == 0))
		{
			fprintf (stderr, "unexpected: %s\n",
					 e.address.pattern? e.address.pattern: "<NULL>");
			return 1;
		}
		fprintf (stderr, "ok\n");
	}

	return 0;
}

static int
check_finding(const char *tags, struct expectation *expectations, int count, int match)
{
	tagFile *t;
	tagFileInfo info;

	fprintf (stderr, "opening %s...", tags);
	t = tagsOpen (tags, &info);
	if (!t)
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d, error_number: %d)\n",
				 t, info.status.opened, info.status.error_number);
		return 1;
	}
	fprintf (stderr, "ok\n");

	if (check_finding0 (t, expectations, count, match) != 0)
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

	/*
	 * sorted=yes
	 */
	const char *tags_sorted_yes = "./ptag-sort-yes.tags";
	struct expectation exp_sorted_yes [] = {
		{ "!_JSON_OUTPUT_VERSION", "0.0", "/in development/", },
		{ "!_TAG_FILE_FORMAT", "2", "/extended format; --format=1 will not append ;\" to lines/", },
		{ "!_TAG_FILE_SORTED", "1", "/0=unsorted, 1=sorted, 2=foldcase/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "D,macroparam", "/parameters inside macro definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "C,custom", "/customizable variables/", },
		{ "!_TAG_OUTPUT_FILESEP", "slash", "/slash or backslash/", },
		{ "!_TAG_OUTPUT_MODE", "u-ctags", "/u-ctags or e-ctags/", },
		{ "!_TAG_PATTERN_LENGTH_LIMIT", "96", "/0 for no limit/", },
		{ "!_TAG_PROGRAM_AUTHOR", "Universal Ctags Team", "//", },
		{ "!_TAG_PROGRAM_NAME", "Universal Ctags", "/Derived from Exuberant Ctags/", },
		{ "!_TAG_PROGRAM_URL", "https://ctags.io/", "/official site/", },
		{ "!_TAG_PROGRAM_VERSION", "0.0.0", "/9b73623f/", },
	};

	if (check_finding (tags_sorted_yes, exp_sorted_yes, COUNT(exp_sorted_yes),
					   TAG_FULLMATCH) != 0)
		return 1;

	struct expectation exp_sorted_yes_partial [] = {
		{ "!_JSON", "0.0", "/in development/", },
		{ "!_TAG_FILE", "2", "/extended format; --format=1 will not append ;\" to lines/", },
		{ "!_TAG_KIND_DESCRIPTION", "D,macroparam", "/parameters inside macro definitions/", },
		{ "!_TAG_OUTPUT_", "slash", "/slash or backslash/", },
		{ "!_TAG_PATT", "96", "/0 for no limit/", },
		{ "!_TAG_PROGRAM_AUTHOR", "Universal Ctags Team", "//", },
		{ "!_TAG_PROGRAM_N", "Universal Ctags", "/Derived from Exuberant Ctags/", },
	};

	if (check_finding (tags_sorted_yes, exp_sorted_yes_partial, COUNT(exp_sorted_yes_partial),
					   TAG_PARTIALMATCH) != 0)
		return 1;

	/*
	 * sorted=no
	 */
	const char *tags_sorted_no = "./ptag-sort-no.tags";
	struct expectation exp_sorted_no [] = {
		{ "!_JSON_OUTPUT_VERSION", "0.0", "/in development/", },
		{ "!_TAG_FILE_FORMAT", "2", "/extended format; --format=1 will not append ;\" to lines/", },
		{ "!_TAG_FILE_SORTED", "0", "/0=unsorted, 1=sorted, 2=foldcase/", },
		{ "!_TAG_PROGRAM_AUTHOR", "Universal Ctags Team", "//", },
		{ "!_TAG_PROGRAM_NAME", "Universal Ctags", "/Derived from Exuberant Ctags/", },
		{ "!_TAG_PROGRAM_URL", "https://ctags.io/", "/official site/", },
		{ "!_TAG_PROGRAM_VERSION", "0.0.0", "/9b73623f/", },
		{ "!_TAG_OUTPUT_MODE", "u-ctags", "/u-ctags or e-ctags/", },
		{ "!_TAG_OUTPUT_FILESEP", "slash", "/slash or backslash/", },
		{ "!_TAG_PATTERN_LENGTH_LIMIT", "96", "/0 for no limit/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "d,macro", "/macro definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "u,unknown", "/unknown type of definitions/", },
	};

	if (check_finding (tags_sorted_no, exp_sorted_no, COUNT(exp_sorted_no), TAG_FULLMATCH) != 0)
		return 1;

	struct expectation exp_sorted_no_partial [] = {
		{ "!_JSON", "0.0", "/in development/", },
		{ "!_TAG_FILE", "2", "/extended format; --format=1 will not append ;\" to lines/", },
		{ "!_TAG_KIND_DESCRIPTION", "d,macro", "/macro definitions/", },
		{ "!_TAG_OUTPUT_", "u-ctags", "/u-ctags or e-ctags/", },
		{ "!_TAG_PATT", "96", "/0 for no limit/", },
		{ "!_TAG_PROGRAM_AUTHOR", "Universal Ctags Team", "//", },
		{ "!_TAG_PROGRAM_N", "Universal Ctags", "/Derived from Exuberant Ctags/", },
	};

	if (check_finding (tags_sorted_no, exp_sorted_no_partial, COUNT(exp_sorted_no_partial),
					   TAG_PARTIALMATCH) != 0)
		return 1;

	/* No entry */
	const char *tags = "./ptag-sort-yes.tags";
	tagFileInfo info;

	fprintf (stderr, "opening %s...", tags);
	tagFile *t = tagsOpen (tags, &info);
	if (!t)
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d, error_number: %d)\n",
				 t, info.status.opened, info.status.error_number);
		return 1;
	}
	fprintf (stderr, "ok\n");

	struct non_existing_testcase {
		const char *matchstr;
		int match;
	} ne_tcase [2] = {
		{
			.matchstr = "full",
			.match = TAG_FULLMATCH,
		},
		{
			.matchstr = "partial",
			.match = TAG_PARTIALMATCH,
		}
	};
	for (int i = 0; i < COUNT(ne_tcase); i++)
	{
		fprintf (stderr, "try to find non-existing tag in %s matching...",
				 ne_tcase[i].matchstr);
		tagResult r = tagsFindPseudoTag (t, NULL, "!NO_SUCH_PTAG",
										 ne_tcase[i].match);
		int err = tagsGetErrno (t);
		if (r == TagSuccess)
		{
			fprintf (stderr, "found one unexpectedly\n");
			return 1;
		}
		else if (err != 0)
		{
			fprintf (stderr, "unexpected errno: %d\n", err);
			return 1;
		}
		fprintf (stderr, "ok\n");
	}

	fprintf (stderr, "closing the tag file...");
	if (tagsClose (t) != TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	return 0;
}
