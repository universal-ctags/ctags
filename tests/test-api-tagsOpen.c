/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsOpen() API function
*/

#include "readtags.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


static int
check_info (tagFileInfo *info, tagFileInfo *expected)
{

	fprintf (stderr, "inspecting info->file.format...");
	if (info->file.format != expected->file.format)
	{
		fprintf (stderr, "unexpected result: %d\n", info->file.format);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "inspecting info->file.sort...");
	if (info->file.sort != expected->file.sort)
	{
		fprintf (stderr, "unexpected result: %d\n", info->file.sort);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "inspecting info->program.author...");
	if (strcmp (info->program.author, expected->program.author))
	{
		fprintf (stderr, "unexpected result: %s\n", info->program.author);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "inspecting info->program.name...");
	if (strcmp (info->program.name, expected->program.name))
	{
		fprintf (stderr, "unexpected result: %s\n", info->program.name);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "inspecting info->program.url...");
	if (strcmp (info->program.url, expected->program.url))
	{
		fprintf (stderr, "unexpected result: %s\n", info->program.url);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "inspecting info->program.version...");
	if (strcmp (info->program.version, expected->program.version))
	{
		fprintf (stderr, "unexpected result: %s\n", expected->program.version);
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

	const char *tags0 = "./no-such-file.tags";
	fprintf (stderr, "opening no-existing tags file...");
	t = tagsOpen (tags0, &info);
	if (! (t == NULL
		   && info.status.opened == 0
		   && info.status.error_number != 0))
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d, error_number: %d)\n",
				 t, info.status.opened, info.status.error_number);
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "opening no-existing tags file with NULL tagFileInfo...");
	t = tagsOpen (tags0, NULL);
	if (t != NULL)
	{
		fprintf (stderr, "unexpected result (t: %p)\n", t);
		return 1;
	}
	fprintf (stderr, "ok\n");

	tagFileInfo expected1 = {
		.file.format = 2,
		.file.sort = TAG_SORTED,
		.program.author = "Darren Hiebert",
		.program.name = "Exuberant Ctags",
		.program.url = "http://ctags.sourceforge.net",
		.program.version = "5.8",
	};
	const char *tags1 = "./api-tagsOpen-ectags.tags";
	fprintf (stderr, "opening an existing tags file...");
	t = tagsOpen (tags1, &info);
	if (t == NULL
		|| info.status.opened == 0)
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d)\n",
				 t, info.status.opened);
		return 1;
	}
	fprintf (stderr, "ok\n");


	if (check_info (&info, &expected1) != 0)
		return 1;

	fprintf (stderr, "closing the tag file...");
	if (tagsClose (t) != TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "opening an existing tags file with NULL tagFileInfo...");
	t = tagsOpen (tags1, NULL);
	if (t == NULL)
	{
		fprintf (stderr, "unexpected result\n");
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

	fprintf (stderr, "opening a / (an error is expected)...");
	info.status.error_number = 0;
	t = tagsOpen ("/", &info);
	if (t != NULL)
	{
		fprintf (stderr, "unexpected result (!NULL)\n");
		return 1;
	}
	else if (info.status.opened)
	{
		fprintf (stderr, "unexpected result (opened != 0)\n");
		return 1;
	}
	else if (info.status.error_number == 0)
	{
		fprintf (stderr, "no error\n");
		return 1;
	}
	fprintf (stderr, "ok (errno: %d)\n", info.status.error_number);

	fprintf (stderr, "closing the unopened tag file...");
	if (tagsClose (t) == TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "opening an broken tags file (format: unexpected number)...");
	t = tagsOpen ("./api-tagsOpen-wrong-format-num.tags", &info);
	if (t)
	{
		fprintf (stderr, "opened well unexpectedly (NULL)\n");
		return 1;
	}
	else if (info.status.error_number != TagErrnoUnexpectedFormat)
	{
		fprintf (stderr, "unexpected error (!= TagErrnoUnexpectedFormat)\n");
	}
	fprintf (stderr, "ok\n");
	fprintf (stderr, "closing the unopened tag file...");
	if (tagsClose (t) == TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "opening an broken tags file (format: not a number)...");
	t = tagsOpen ("./api-tagsOpen-wrong-format-nonum.tags", &info);
	if (t)
	{
		fprintf (stderr, "opened well unexpectedly (NULL)\n");
		return 1;
	}
	else if (info.status.error_number != TagErrnoUnexpectedFormat)
	{
		fprintf (stderr, "unexpected error (!= TagErrnoUnexpectedFormat)\n");
	}
	fprintf (stderr, "ok\n");
	fprintf (stderr, "closing the unopened tag file...");
	if (tagsClose (t) == TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "opening an broken tags file (sort: unexpected number)...");
	t = tagsOpen ("./api-tagsOpen-wrong-sort-method-num.tags", &info);
	if (t)
	{
		fprintf (stderr, "opened well unexpectedly (NULL)\n");
		return 1;
	}
	else if (info.status.error_number != TagErrnoUnexpectedSortedMethod)
	{
		fprintf (stderr, "unexpected error (!= TagErrnoUnexpectedSortedMethod)\n");
	}
	fprintf (stderr, "ok\n");
	fprintf (stderr, "closing the unopened tag file...");
	if (tagsClose (t) == TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	fprintf (stderr, "opening an broken tags file (sort: not a number)...");
	t = tagsOpen ("./api-tagsOpen-wrong-sort-method-nonum.tags", &info);
	if (t)
	{
		fprintf (stderr, "opened well unexpectedly (NULL)\n");
		return 1;
	}
	else if (info.status.error_number != TagErrnoUnexpectedSortedMethod)
	{
		fprintf (stderr, "unexpected error (!= TagErrnoUnexpectedSortedMethod)\n");
	}
	fprintf (stderr, "ok\n");
	fprintf (stderr, "closing the unopened tag file...");
	if (tagsClose (t) == TagSuccess)
	{
		fprintf (stderr, "unexpected result\n");
		return 1;
	}
	fprintf (stderr, "ok\n");

	const char* broken_PROGRAM_AUTHOR [6] = {
		"Universal Ctags Team",
		"Universal Ctags Team",
		"",
		NULL,
		NULL,
		NULL,
	};
	for (int i = 0; i < 6; i++)
	{
		char tagf_name_tmpl [] = "./api-tagsOpen-incomplete-program-author-%d.tags";
		char tagf_name [sizeof (tagf_name_tmpl) - 1];
		fprintf (stderr, "opening a tags file with incomplete PROGRAM_AUTHOR field [trimming level: %d]...", i);
		snprintf (tagf_name, sizeof (tagf_name), tagf_name_tmpl, i);
		t = tagsOpen (tagf_name, &info);
		if (t == NULL)
		{
			fprintf (stderr, "unexpected error: %d %s\n", info.status.error_number,
					 info.status.error_number > 0
					 ? strerror (info.status.error_number)
					 : "");
			return 1;
		}
		if (!((broken_PROGRAM_AUTHOR [i] == info.program.author)
			  || (broken_PROGRAM_AUTHOR [i]
				  && info.program.author
				  && strcmp (broken_PROGRAM_AUTHOR [i], info.program.author)) == 0))
		{
			fprintf (stderr, "unexpected value: %s (!= %s)\n",
					 info.program.author? info.program.author: "(null)",
					 broken_PROGRAM_AUTHOR [i]? broken_PROGRAM_AUTHOR [i]: "(null)");
			return 1;
		}
		fprintf (stderr, "closing the unopened tag file...");
		if (tagsClose (t) == TagFailure)
		{
			fprintf (stderr, "successful unexpectedly\n");
			return 1;
		}
		fprintf (stderr, "ok\n");
	}

	return 0;
}
