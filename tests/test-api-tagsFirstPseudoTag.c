/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsFirstPseudoTag() and tagsNextPseudoTag() API functions
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
check_iterating0 (tagFile *t, struct expectation *expectations, int count)
{
	tagEntry e;
	struct expectation *x;
	int err;

	for (int i = 0; i < count; i++)
	{
		fprintf (stderr, "[%d/%d] iterating ptags...", i + 1, count);
		if (i == 0)
		{
			if (tagsFirstPseudoTag (t, &e) != TagSuccess)
			{
				if ((err = tagsGetErrno (t)))
					fprintf (stderr, "error: %d\n", err);
				else
					fprintf (stderr, "no more ptag\n");
				return 1;
			}
		}
		else
		{
			if (tagsNextPseudoTag (t, &e) != TagSuccess)
			{
				if ((err = tagsGetErrno (t)))
					fprintf (stderr, "error: %d\n", err);
				else
					fprintf (stderr, "no more ptag\n");
				return 1;
			}
		}
		fprintf (stderr, "found\n");

		x = expectations + i;

		fprintf (stderr, "checking name field...");
		if (!(e.name && strcmp (x->name, e.name) == 0))
		{
			fprintf (stderr, "unexpected: %s\n", e.name? e.name: "<NULL>");
			return 1;
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

	fprintf (stderr, "verifying no remain....");
	if (tagsNextPseudoTag (t, &e) == TagSuccess)
	{
		if ((err = tagsGetErrno (t)))
			fprintf (stderr, "error: %d\n", err);
		else
			fprintf (stderr, "still existing\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	return 0;
}

static int
check_iterating (const char *tags, struct expectation *expectations, int count)
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

	if (check_iterating0 (t, expectations, count) != 0)
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
		{ "!_TAG_KIND_DESCRIPTION!C", "L,label", "/goto labels/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "d,macro", "/macro definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "e,enumerator", "/enumerators (values inside an enumeration)/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "f,function", "/function definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "g,enum", "/enumeration names/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "h,header", "/included header files/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "l,local", "/local variables/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "m,member", "/struct, and union members/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "p,prototype", "/function prototypes/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "s,struct", "/structure names/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "t,typedef", "/typedefs/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "u,union", "/union names/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "v,variable", "/variable definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "x,externvar", "/external and forward variable declarations/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "z,parameter", "/function parameters inside function definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "C,custom", "/customizable variables/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "D,derivedMode", "/derived major mode/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "G,group", "/customization groups/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "H,face", "/customizable faces/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "M,minorMode", "/minor modes/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "T,theme", "/custom themes/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "V,varalias", "/aliases for variables/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "a,alias", "/aliases for functions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "c,const", "/constants/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "e,error", "/errors/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "f,function", "/functions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "i,inline", "/inline function/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "m,macro", "/macros/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "s,subst", "/inline function/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "u,unknown", "/unknown type of definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "v,variable", "/variables/", },
		{ "!_TAG_OUTPUT_FILESEP", "slash", "/slash or backslash/", },
		{ "!_TAG_OUTPUT_MODE", "u-ctags", "/u-ctags or e-ctags/", },
		{ "!_TAG_PATTERN_LENGTH_LIMIT", "96", "/0 for no limit/", },
		{ "!_TAG_PROGRAM_AUTHOR", "Universal Ctags Team", "//", },
		{ "!_TAG_PROGRAM_NAME", "Universal Ctags", "/Derived from Exuberant Ctags/", },
		{ "!_TAG_PROGRAM_URL", "https://ctags.io/", "/official site/", },
		{ "!_TAG_PROGRAM_VERSION", "0.0.0", "/9b73623f/", },
	};

	if (check_iterating (tags_sorted_yes, exp_sorted_yes, COUNT(exp_sorted_yes)) != 0)
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
		{ "!_TAG_KIND_DESCRIPTION!C", "e,enumerator", "/enumerators (values inside an enumeration)/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "f,function", "/function definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "g,enum", "/enumeration names/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "h,header", "/included header files/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "l,local", "/local variables/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "m,member", "/struct, and union members/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "p,prototype", "/function prototypes/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "s,struct", "/structure names/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "t,typedef", "/typedefs/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "u,union", "/union names/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "v,variable", "/variable definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "x,externvar", "/external and forward variable declarations/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "z,parameter", "/function parameters inside function definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "L,label", "/goto labels/", },
		{ "!_TAG_KIND_DESCRIPTION!C", "D,macroparam", "/parameters inside macro definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "u,unknown", "/unknown type of definitions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "f,function", "/functions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "v,variable", "/variables/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "c,const", "/constants/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "m,macro", "/macros/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "a,alias", "/aliases for functions/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "V,varalias", "/aliases for variables/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "s,subst", "/inline function/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "i,inline", "/inline function/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "e,error", "/errors/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "M,minorMode", "/minor modes/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "D,derivedMode", "/derived major mode/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "C,custom", "/customizable variables/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "G,group", "/customization groups/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "H,face", "/customizable faces/", },
		{ "!_TAG_KIND_DESCRIPTION!EmacsLisp", "T,theme", "/custom themes/", },
	};

	if (check_iterating (tags_sorted_no, exp_sorted_no, COUNT(exp_sorted_no)) != 0)
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


	fprintf (stderr, "visiting the first tag with NULL tagEntry...");
	if (tagsFirstPseudoTag (t, NULL) != TagSuccess)
	{
		int err;
		if ((err = tagsGetErrno (t)))
			fprintf (stderr, "error: %d\n", err);
		else
			fprintf (stderr, "no first ptag\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "visiting the next tag with NULL tagEntry...");
	if (tagsNextPseudoTag (t, NULL) != TagSuccess)
	{
		int err;
		if ((err = tagsGetErrno (t)))
			fprintf (stderr, "error: %d\n", err);
		else
			fprintf (stderr, "no more ptag\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "closing the tag file...");
	if (tagsClose (t) != TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	return 0;
}
