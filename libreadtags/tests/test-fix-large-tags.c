/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing the way to report "too large tags file".
*/

#include "readtags.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#define	TAGS "./remove-me-after-testing.tags"
#define KEY "xdigitValue"

static int
make_large_tags (const char *output)
{
	FILE *fp = fopen(output, "w");
	if (fp == NULL)
		return 1;

	int r = 0;
	const char *ptags [] = {
		"!_TAG_FILE_FORMAT	2	/extended format; --format=1 will not append ;\" to lines/",
		"!_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted, 2=foldcase/",
		"!_TAG_OUTPUT_EXCMD	mixed	/number, pattern, mixed, or combineV2/",
		"!_TAG_OUTPUT_FILESEP	slash	/slash or backslash/",
		"!_TAG_OUTPUT_MODE	u-ctags	/u-ctags or e-ctags/",
		"!_TAG_PATTERN_LENGTH_LIMIT	96	/0 for no limit/",
		"!_TAG_PROC_CWD	/home/jet/var/libreadtags-new/	//",
		"!_TAG_PROGRAM_AUTHOR	Universal Ctags Team	//",
		"!_TAG_PROGRAM_NAME	Universal Ctags	/Derived from Exuberant Ctags/",
		"!_TAG_PROGRAM_URL	https://ctags.io/	/official site/",
		"!_TAG_PROGRAM_VERSION	5.9.0	/966e51373/",
	};

	for (unsigned int i = 0; i < sizeof (ptags) / sizeof (ptags[0]); i++)
	{
		if (fputs(ptags[i], fp) < 0)
		{
			r = 1;
			break;
		}
		if (fputc('\n', fp) == EOF)
		{
			r = 1;
			break;
		}
	}

	const unsigned int count = 21474836;	/* $(( (2 ** 31) / 100)) */
	for (unsigned int i = 0; i < count; i++)
	{
		if (fputs("EmptyString", fp) < 0)
		{
			r = 1;
			break;
		}

		if (fprintf(fp, "%u", i) < 1)
		{
			r = 1;
			break;
		}
		if (fputs("	readtags.c	/^static const char *const EmptyString", fp) < 0)
		{
			r = 1;
			break;
		}

		if (fprintf(fp, "%u", i) < 1)
		{
			r = 1;
			break;
		}
		if (fputs(" = \"\";$/;\"	v	typeref:typename:const char * const	file:\n", fp) < 0)
		{
			r = 1;
			break;
		}
	}

	if (r == 0)
	{
		if (fputs(KEY, fp) < 0)
			r = 1;
		else if (fputs("	readtags.c	/^static int xdigitValue (char digit)$/;\"	f	typeref:typename:int	file:\n", fp) < 0)
			r = 1;
	}

	if (fclose (fp))
		r = 1;

	return r;
}

int
main (void)
{
	fprintf (stderr, "generating a large (> 2G) tags file (%s)...", TAGS);

	errno = 0;
	int r = make_large_tags (TAGS);
	if (r)
	{
		fprintf(stderr, "unexpected result: %s%s%d\n",
				errno? strerror(errno): "",
				errno? ": ": "",
				r);
		return 99;
	}
	fprintf(stderr, "done\n");

	fprintf(stderr, "opening tags file (%s)...", TAGS);
	tagFileInfo info;
	tagFile *t = tagsOpen (TAGS, &info);
	if ((info.status.opened == 0
		 && (
			 info.status.error_number == EFBIG
			 || info.status.error_number == TagErrnoFileMaybeTooBig)))
	{
		r = 1;
		fprintf(stderr, "failed with expected error: %d\n", info.status.error_number);
	}
	else if (t && info.status.opened)
		fprintf(stderr, "done\n");
	else
	{
		r = 1;
		fprintf (stderr, "unexpected result (t: %p, opened: %d, error_number: %d<%s>)\n",
				 t, info.status.opened, info.status.error_number,
				 (info.status.error_number >= 0)? strerror (info.status.error_number): "");
	}

	if (t)
	{
		tagEntry e;
		fprintf (stderr, "finding \"%s\"...", KEY);
		if (tagsFind (t, &e, KEY, TAG_FULLMATCH) == TagSuccess)
			fprintf (stderr, "found as expected\n");
		else
		{
			r = 1;
			fprintf (stderr, "not found unexpectedly\n");
		}
		tagsClose (t);
	}

	remove(TAGS);
	return r;
}
