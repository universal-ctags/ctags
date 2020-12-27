/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsFirst() and tagsNext() API functions
*/

#include "readtags.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

struct entryExpectaion {
	tagResult result;
	const char *name;
	int err;
};

#define LAST {											\
		.result = TagFailure,							\
		.name   = "last",								\
	}

#define isLast(ex) ((ex)->result == TagFailure && (ex)->name)

struct expectation {
	char *file;
	struct entryExpectaion first;
	struct entryExpectaion *next;
};

static int
check_one (tagFile *t, struct entryExpectaion *ex, tagEntry *e, int first)

{
	fprintf (stderr, "getting the %s entry...", first? "first": "next");
	tagResult r;

	r = (first? tagsFirst: tagsNext) (t, e);
	if (r == ex->result)
	{
		if (r == TagSuccess)
		{
			if (strcmp (e->name, ex->name) == 0)
				fprintf (stderr, "found expected one: %s\n", e->name);
			else
			{
				fprintf (stderr, "found unexpected one: %s (expected: %s)\n", e->name, ex->name);
				return 1;
			}
		}
		else
		{
			fprintf (stderr, "nothing found expectedly\n");

			fprintf (stderr, "comparing errno...");
			int err = tagsGetErrno (t);
			if (err == ex->err)
				fprintf (stderr, "matched: %d\n", err);
			else
			{
				fprintf (stderr, "unmatched: %d (expected: %d)\n", err, ex->err);
				return 1;
			}
		}
	}
	else if (ex->result == TagSuccess)
	{
		fprintf (stderr, "nothing found unexpectedly\n");
		return 1;
	}
	else
	{
		fprintf (stderr, "something found unexpectedly: %s\n", e->name);
		return 1;
	}


	return 0;
}

static int
check (struct expectation *x)
{
	tagFile *t;
	tagFileInfo info;
	const char *tags = x->file;

	fprintf (stderr, "opening %s...", tags);
	t = tagsOpen (tags, &info);
	if (!t)
	{
		fprintf (stderr, "unexpected result (t: %p, opened: %d, error_number: %d)\n",
				 t, info.status.opened, info.status.error_number);
		return 1;
	}
	fprintf (stderr, "ok\n");

	tagEntry e;
	if (check_one (t, &x->first, &e, 1))
		return 1;

	for (int i = 0; !isLast (x->next + i); i++)
		if (check_one (t, x->next + i, &e, 0))
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

	struct expectation expectations [] = {
		{
			.file = "duplicated-names--sorted-yes.tags",
			.first = {
				.result = TagSuccess,
				.name = "M",
			},
			.next = ((struct entryExpectaion[]) {
					{TagSuccess, "N"},
					{TagSuccess, "O"},
					{TagSuccess, "m"},
					{TagSuccess, "main"},
					{TagSuccess, "n"},
					{TagSuccess, "n"},
					{TagSuccess, "n"},
					{TagSuccess, "n"},
					{TagSuccess, "n"},
					{TagSuccess, "n"},
					{TagSuccess, "o"},
					LAST,
				}),
		},
		{
			.file = "empty.tags",
			.first = {
				.result = TagFailure,
				.err    = 0,
			},
			.next = ((struct entryExpectaion []) {
					{TagFailure, NULL, 0},
					LAST,
				}),
		},
		{
			.file = "empty-no-newline.tags",
			.first = {
				.result = TagFailure,
				.err    = 0,
			},
			.next = ((struct entryExpectaion []) {
					{TagFailure, NULL, 0},
					LAST,
				}),
		},
		{
			.file = "broken-line-field.tags",
			.first = {
				.result = TagFailure,
				.name   = NULL,
				.err = TagErrnoUnexpectedLineno,
			},
			.next = ((struct entryExpectaion []) {
					LAST,
				}),
		},
		{
			.file = "broken-line-field-other-than-first.tags",
			.first = {
				.result = TagSuccess,
				.name   = "M",
			},
			.next = ((struct entryExpectaion []) {
					{
						.result = TagSuccess,
						.name   = "N",
					},
					{
						.result = TagFailure,
						.name   = NULL,
						.err    = TagErrnoUnexpectedLineno,

					},
					LAST,
				}),
		},
	};

	for (int i = 0; i < sizeof (expectations) / sizeof (expectations[0]); i++)
		if (check (expectations + i))
			return 1;
	return 0;
}
