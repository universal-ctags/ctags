/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing tagsFind() and tagsFindNext() API functions
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
	char *kind;
	char *scope_kind;
	char *scope_name;
	char *typeref;
	int fileScope;
};

#define COUNT(x) (sizeof(x)/sizeof(x[0]))

static int
check_finding0 (tagFile *t, const char *name, const int options,
			   struct expectation *expectations, int count)
{
	tagEntry e;
	struct expectation *x;

	for (int i = 0; i < count; i++)
	{
		fprintf (stderr, "[%d/%d] finding \"%s\" (%d)...", i + 1, count, name, options);
		if (i == 0)
		{
			if (tagsFind (t, &e, name, options) != TagSuccess)
			{
				fprintf (stderr, "not found\n");
				return 1;
			}
		}
		else
		{
			if (tagsFindNext (t, &e) != TagSuccess)
			{
				fprintf (stderr, "not found\n");
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
			fprintf (stderr, "unexpected\n");
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

		fprintf (stderr, "checking kind field...");
		if (!(e.kind && strcmp (x->kind, e.kind) == 0))
		{
			fprintf (stderr, "unexpected\n");
			return 1;
		}
		fprintf (stderr, "ok\n");

		if (x->scope_kind)
		{
			fprintf (stderr, "checking scope field...");
			const char *scope = tagsField (&e, x->scope_kind);
			if (scope == NULL || strcmp (scope, x->scope_name) != 0)
			{
				fprintf (stderr, "unexpected: %s\n", scope? scope: "<NULL>");
				return 1;
			}
			fprintf (stderr, "ok\n");
		}

		if (x->typeref)
		{
			fprintf (stderr, "checking typeref field...");
			const char *typeref = tagsField (&e, "typeref");
			if (typeref == NULL || strcmp (typeref, x->typeref) != 0)
			{
				fprintf (stderr, "unexpected: %s\n", typeref? typeref: "<NULL>");
				return 1;
			}
			fprintf (stderr, "ok\n");
		}

		fprintf (stderr, "checking file field...");
		if (x->fileScope != e.fileScope)
		{
			fprintf (stderr, "unexpected\n");
			return 1;
		}
		fprintf (stderr, "ok\n");
	}

	if (tagsFindNext (t, &e) == TagSuccess)
		return 1;

	return 0;
}

static int
check_finding (const char *tags, const char *name, const int options,
			   struct expectation *expectations, int count)
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

	if (check_finding0 (t, name, options, expectations, count) != 0)
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
	const char *tags_sorted_yes = "./duplicated-names--sorted-yes.tags";
	struct expectation sorted_yes_FULLMATCH_OBSERVECASE [] = {
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^		int n;$/",
			.kind = "l",
			.scope_kind = "function",
			.scope_name = "main",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^	for (int n = 0; n < 1; n++)$/",
			.kind = "l",
			.scope_kind = "function",
			.scope_name = "main",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^	int n;$/",
			.kind = "m",
			.scope_kind = "struct",
			.scope_name = "n",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^int main(int n)$/",
			.kind = "z",
			.scope_kind = "function",
			.scope_name = "main",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^struct n {$/",
			.kind = "s",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = NULL,
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^typedef int n;$/",
			.kind = "t",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 1,
		},
	};

	struct expectation sorted_yes_FULLMATCH_IGNORECASE [COUNT(sorted_yes_FULLMATCH_OBSERVECASE) + 1] = {
		[0] = {
			.name = "N",
			.file = "input.c",
			.pattern = "/^int N;$/",
			.kind = "v",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
	};
	for (int i = 0; i < COUNT(sorted_yes_FULLMATCH_OBSERVECASE); i++)
		sorted_yes_FULLMATCH_IGNORECASE [i + 1] = sorted_yes_FULLMATCH_OBSERVECASE [i];

	struct expectation sorted_yes_PARTIALMATCH_OBSERVECASE [] = {
		{
			.name = "m",
			.file = "input.c",
			.pattern = "/^int m;$/",
			.kind = "v",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
		{
			.name = "main",
			.file = "input.c",
			.pattern = "/^int main(int n)$/",
			.kind = "f",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
	};

	struct expectation sorted_yes_PARTIALMATCH_IGNORECASE [COUNT(sorted_yes_PARTIALMATCH_OBSERVECASE) + 1] = {
		[0] = {
			.name = "M",
			.file = "input.c",
			.pattern = "/^int M (void) { return 0; }$/",
			.kind = "f",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
	};
	for (int i = 0; i < COUNT(sorted_yes_PARTIALMATCH_OBSERVECASE); i++)
		sorted_yes_PARTIALMATCH_IGNORECASE [i + 1] = sorted_yes_PARTIALMATCH_OBSERVECASE [i];

	if (check_finding (tags_sorted_yes, "n", TAG_FULLMATCH|TAG_OBSERVECASE,
					   sorted_yes_FULLMATCH_OBSERVECASE, COUNT(sorted_yes_FULLMATCH_OBSERVECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_yes, "n", TAG_FULLMATCH|TAG_IGNORECASE,
					   sorted_yes_FULLMATCH_IGNORECASE, COUNT(sorted_yes_FULLMATCH_IGNORECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_yes, "m", TAG_PARTIALMATCH|TAG_OBSERVECASE,
					   sorted_yes_PARTIALMATCH_OBSERVECASE, COUNT(sorted_yes_PARTIALMATCH_OBSERVECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_yes, "m", TAG_PARTIALMATCH|TAG_IGNORECASE,
					   sorted_yes_PARTIALMATCH_IGNORECASE, COUNT(sorted_yes_PARTIALMATCH_IGNORECASE)) != 0)
		return 1;


	/*
	 * sorted=no
	 */
	const char *tags_sorted_no = "./duplicated-names--sorted-no.tags";
	struct expectation sorted_no_FULLMATCH_OBSERVECASE [] = {
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^struct n {$/",
			.kind = "s",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = NULL,
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^	int n;$/",
			.kind = "m",
			.scope_kind = "struct",
			.scope_name = "n",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^typedef int n;$/",
			.kind = "t",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^int main(int n)$/",
			.kind = "z",
			.scope_kind = "function",
			.scope_name = "main",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^	for (int n = 0; n < 1; n++)$/",
			.kind = "l",
			.scope_kind = "function",
			.scope_name = "main",
			.typeref = "typename:int",
			.fileScope = 1,
		},
		{
			.name = "n",
			.file = "input.c",
			.pattern = "/^		int n;$/",
			.kind = "l",
			.scope_kind = "function",
			.scope_name = "main",
			.typeref = "typename:int",
			.fileScope = 1,
		},
	};

	struct expectation sorted_no_FULLMATCH_IGNORECASE [COUNT(sorted_no_FULLMATCH_OBSERVECASE) + 1] = {
		[0] = {
			.name = "N",
			.file = "input.c",
			.pattern = "/^int N;$/",
			.kind = "v",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
	};
	for (int i = 0; i < COUNT(sorted_no_FULLMATCH_OBSERVECASE); i++)
		sorted_no_FULLMATCH_IGNORECASE [i + 1] = sorted_no_FULLMATCH_OBSERVECASE [i];

	struct expectation sorted_no_PARTIALMATCH_OBSERVECASE [] = {
		{
			.name = "main",
			.file = "input.c",
			.pattern = "/^int main(int n)$/",
			.kind = "f",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
		{
			.name = "m",
			.file = "input.c",
			.pattern = "/^int m;$/",
			.kind = "v",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
	};

	struct expectation sorted_no_PARTIALMATCH_IGNORECASE [COUNT(sorted_no_PARTIALMATCH_OBSERVECASE) + 1] = {
		[COUNT(sorted_no_PARTIALMATCH_OBSERVECASE)] = {
			.name = "M",
			.file = "input.c",
			.pattern = "/^int M (void) { return 0; }$/",
			.kind = "f",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
	};
	for (int i = 0; i < COUNT(sorted_no_PARTIALMATCH_OBSERVECASE); i++)
		sorted_no_PARTIALMATCH_IGNORECASE [i] = sorted_no_PARTIALMATCH_OBSERVECASE [i];

	if (check_finding (tags_sorted_no, "n", TAG_FULLMATCH|TAG_OBSERVECASE,
					   sorted_no_FULLMATCH_OBSERVECASE, COUNT(sorted_no_FULLMATCH_OBSERVECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_no, "n", TAG_FULLMATCH|TAG_IGNORECASE,
					   sorted_no_FULLMATCH_IGNORECASE, COUNT(sorted_no_FULLMATCH_IGNORECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_no, "m", TAG_PARTIALMATCH|TAG_OBSERVECASE,
					   sorted_no_PARTIALMATCH_OBSERVECASE, COUNT(sorted_no_PARTIALMATCH_OBSERVECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_no, "m", TAG_PARTIALMATCH|TAG_IGNORECASE,
					   sorted_no_PARTIALMATCH_IGNORECASE, COUNT(sorted_no_PARTIALMATCH_IGNORECASE)) != 0)
		return 1;


	/*
	 * sorted=foldcase
	 */
	const char *tags_sorted_foldcase = "./duplicated-names--sorted-foldcase.tags";
	struct expectation sorted_foldcase_FULLMATCH_OBSERVECASE [COUNT(sorted_yes_FULLMATCH_OBSERVECASE)];
	for (int i = 0; i < COUNT(sorted_foldcase_FULLMATCH_OBSERVECASE); i++)
		sorted_foldcase_FULLMATCH_OBSERVECASE [i] =  sorted_yes_FULLMATCH_OBSERVECASE [i];

	struct expectation sorted_foldcase_FULLMATCH_IGNORECASE [COUNT(sorted_foldcase_FULLMATCH_OBSERVECASE) + 1] = {
		[4] = {
			.name = "N",
			.file = "input.c",
			.pattern = "/^int N;$/",
			.kind = "v",
			.scope_kind = NULL,
			.scope_name = NULL,
			.typeref = "typename:int",
			.fileScope = 0,
		},
		[5] = sorted_foldcase_FULLMATCH_OBSERVECASE[4],
		[6] = sorted_foldcase_FULLMATCH_OBSERVECASE[5],
	};
	for (int i = 0; i < 4; i++)
		sorted_foldcase_FULLMATCH_IGNORECASE [i] = sorted_foldcase_FULLMATCH_OBSERVECASE [i];

	struct expectation sorted_foldcase_PARTIALMATCH_OBSERVECASE [COUNT(sorted_yes_PARTIALMATCH_OBSERVECASE)];
	for (int i = 0; i < COUNT(sorted_foldcase_PARTIALMATCH_OBSERVECASE); i++)
		sorted_foldcase_PARTIALMATCH_OBSERVECASE [i] =  sorted_yes_PARTIALMATCH_OBSERVECASE [i];

	struct expectation sorted_foldcase_PARTIALMATCH_IGNORECASE [COUNT(sorted_yes_PARTIALMATCH_IGNORECASE)];
	for (int i = 0; i < COUNT(sorted_foldcase_PARTIALMATCH_IGNORECASE); i++)
		sorted_foldcase_PARTIALMATCH_IGNORECASE [i] =  sorted_yes_PARTIALMATCH_IGNORECASE [i];


	if (check_finding (tags_sorted_foldcase, "n", TAG_FULLMATCH|TAG_OBSERVECASE,
					   sorted_foldcase_FULLMATCH_OBSERVECASE, COUNT(sorted_foldcase_FULLMATCH_OBSERVECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_foldcase, "n", TAG_FULLMATCH|TAG_IGNORECASE,
					   sorted_foldcase_FULLMATCH_IGNORECASE, COUNT(sorted_foldcase_FULLMATCH_IGNORECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_foldcase, "m", TAG_PARTIALMATCH|TAG_OBSERVECASE,
					   sorted_foldcase_PARTIALMATCH_OBSERVECASE, COUNT(sorted_foldcase_PARTIALMATCH_OBSERVECASE)) != 0)
		return 1;

	if (check_finding (tags_sorted_foldcase, "m", TAG_PARTIALMATCH|TAG_IGNORECASE,
					   sorted_foldcase_PARTIALMATCH_IGNORECASE, COUNT(sorted_foldcase_PARTIALMATCH_IGNORECASE)) != 0)
		return 1;


	return 0;
}
