/*
 *   Copyright (c) 2023, Masatake YAMATO
 *   Copyright (c) 2023, Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for kerledoc used in
 *   linux kernel.
 *
 *   References:
 *   - https://www.kernel.org/doc/html/v4.9/kernel-documentation.html#writing-kernel-doc-comments
 *   - https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/Documentation/doc-guide/kernel-doc.rst
 *   - https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/scripts/kernel-doc
 */

#include "general.h"  /* must always come first */
#include "cpreprocessor.h"

#include "entry.h"
#include "keyword.h"
#include "kind.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

#include <ctype.h>
#include <string.h>

static langType Lang_kerneldoc;

typedef enum {
	K_UNKNOWN,
	K_STRUCT,
	K_UNION,
	K_ENUM,
	K_TYPEDEF,
	K_PARAM,
	K_FUNCTION,
	K_MEMBER,
	K_ENUMERATOR,
	K_DOC,
} kernelDocKind;

static scopeSeparator KernelDocRootSeparators [] = {
	{ K_DOC, "\"\"" },
};

static kindDefinition KernelDocKinds [] = {
	{ true,  'Y', "unknown",   "unknown items",
	  ATTACH_SEPARATORS(KernelDocRootSeparators)            },
	{ true,  's', "struct",    "struct names",
	  ATTACH_SEPARATORS(KernelDocRootSeparators)            },
	{ true,  'u', "union",     "union names",
	  ATTACH_SEPARATORS(KernelDocRootSeparators)            },
	{ true,  'g', "enum",      "enumeration names",
	  ATTACH_SEPARATORS(KernelDocRootSeparators)            },
	{ true,  't', "typedef",   "typedefs",
	  ATTACH_SEPARATORS(KernelDocRootSeparators)            },
	{ true,  'z', "parameter", "parameters"                 },
	{ true,  'f', "function",   "functions and macros",
	  ATTACH_SEPARATORS(KernelDocRootSeparators)            },
	{ true,  'm', "member",     "struct, and union members" },
	{ true,  'e', "enumerator", "enumerators (values inside an enumeration)" },
	{ true,  'd', "doc",        "overviews marked with \"DOC:\"" },
};

struct sKernelDocSubparser {
	cPreProcessorSubparser cpp;
	int doc;
	int current;				/* cork index */
};

enum {
	KEYWORD_struct,
	KEYWORD_typedef,
	KEYWORD_union,
	KEYWORD_enum,
};

static const keywordTable KernelDocKeywordTable[] = {
	{ "struct",  KEYWORD_struct  },
	{ "typedef", KEYWORD_typedef },
	{ "union",   KEYWORD_union   },
	{ "enum",    KEYWORD_enum    },
};

static void findKernelDocTags (void)
{
	scheduleRunningBaseparser (0);
}

static int parseFollowingAtmark (cPreProcessorSubparser *s, const char *c)
{
	int r = CORK_NIL;
	vString *obj = vStringNew ();

	while (*c && *c != ':')
		vStringPut (obj, *c++);
	if (*c == ':' && !vStringIsEmpty (obj))
	{
		tagEntryInfo e;
		int kindex = K_PARAM;
		int current = ((struct sKernelDocSubparser *)s)->current;
		tagEntryInfo *parent = getEntryInCorkQueue (current);

		if (parent)
		{
			switch (parent->kindIndex)
			{
			case K_TYPEDEF:
			case K_FUNCTION:
				break;
			case K_UNION:
			case K_STRUCT:
				kindex = K_MEMBER;
				break;
			case K_ENUM:
				kindex = K_ENUMERATOR;
				break;
			}
		}
		initTagEntry (&e, vStringValue (obj), kindex);
		e.extensionFields.scopeIndex = current;
		r = makeTagEntry(&e);
	}
	vStringDelete (obj);
	return r;
}

static bool firstLineNotify (cPreProcessorSubparser *s, char firstchar, const char *line)
{
	int kind = K_UNKNOWN;
	int r = CORK_NIL;
	const char *c = line;
	vString *obj = NULL;

	while (isspace((unsigned char)*c) || *c == '*')
		c++;

	/* Handling in-line documentation. */
	if (((struct sKernelDocSubparser *)s)->current != CORK_NIL
		&& *c == '@')
	{
		c++;
		if (parseFollowingAtmark (s, c) != CORK_NIL)
			return true;
	}

	obj = vStringNew ();
	while (*c)
	{
		if (isspace((unsigned char)*c))
		{
			int k = lookupKeyword (vStringValue (obj), Lang_kerneldoc);
			if (k == KEYWORD_struct)
			{
				vStringClear(obj);
				kind = K_STRUCT;
				c++;
				continue;
			}
			else if (k == KEYWORD_union)
			{
				vStringClear(obj);
				kind = K_UNION;
				c++;
				continue;

			}
			else if (k == KEYWORD_enum)
			{
				vStringClear(obj);
				kind = K_ENUM;
				c++;
				continue;

			}
			else if (k == KEYWORD_typedef)
			{
				vStringClear(obj);
				kind = K_TYPEDEF;
				c++;
				continue;
			}
			else
			{
				c++;
				break;
			}
		}
		else if (*c == ':' && strcmp (vStringValue(obj), "DOC") == 0)
		{
			vStringClear(obj);
			kind = K_DOC;
			c++;
			break;
		}
		vStringPut(obj, *c++);
	}

	while (isspace((unsigned char)*c))
		c++;

	if (kind == K_DOC)
	{
		vStringCopyS(obj, c);
		vStringStripTrailing(obj);
		if (vStringLength(obj) > 0)
		{
			r = makeSimpleTag(obj, kind);
			((struct sKernelDocSubparser *)s)->doc = r;
			goto out;
		}
	}

	size_t len = vStringLength(obj);
	if (len > 2
		&& (vStringChar(obj, len - 1) == ')') && (vStringChar(obj, len - 2) == '('))
	{
		kind = K_FUNCTION;
		vStringTruncate (obj, len - 2);
		len -= 2;
	}

	if (*c == '-' && isspace((unsigned char)*(c + 1)) && len > 0)
	{
		r = makeSimpleTag(obj, kind);
		if (((struct sKernelDocSubparser *)s)->doc != CORK_NIL)
		{
			tagEntryInfo *e = getEntryInCorkQueue (r);
			if (e)
				e->extensionFields.scopeIndex = ((struct sKernelDocSubparser *)s)->doc;
		}
		((struct sKernelDocSubparser *)s)->current = r;
	}

 out:
	vStringDelete(obj);

	return r != CORK_NIL;
}

static void restLineNotify (cPreProcessorSubparser *s, const char *line)
{
	const char *c = line;

	while (isspace((unsigned char)*c) || *c == '*')
		c++;

	if (*c == '@')
	{
		c++;
		parseFollowingAtmark(s, c);
	}
}

static void endOfCommentNotify(cPreProcessorSubparser *s)
{
	int r = ((struct sKernelDocSubparser *)s)->current;
	tagEntryInfo *e = getEntryInCorkQueue (r);
	if (e)
		e->extensionFields.endLine = getInputLineNumber ();
}

static void initialize (const langType language)
{
	Lang_kerneldoc = language;
}

static void inputStart (subparser *s)
{
	struct sKernelDocSubparser *kerneldoc = (struct sKernelDocSubparser*)s;

	kerneldoc->doc = CORK_NIL;
	kerneldoc->current = CORK_NIL;
}

extern parserDefinition* KernelDocParser (void)
{
	static struct sKernelDocSubparser kernelDocSubparser = {
		.cpp = {
			.subparser = {
				.direction = SUBPARSER_BASE_RUNS_SUB,
				.inputStart = inputStart,
				.inputEnd = NULL,
			},
			.firstLineNotify = firstLineNotify,
			.restLineNotify  = restLineNotify,
			.endOfCommentNotify = endOfCommentNotify,
		},
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "CPreProcessor", &kernelDocSubparser },
	};

	parserDefinition* def = parserNew ("KernelDoc");
	def->enabled   = false;

	def->kindTable = KernelDocKinds;
	def->kindCount = ARRAY_SIZE (KernelDocKinds);

	def->parser = findKernelDocTags;
	def->initialize = initialize;

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->keywordTable = KernelDocKeywordTable;
	def->keywordCount = ARRAY_SIZE (KernelDocKeywordTable);

	def->useCork = CORK_QUEUE;
	return def;
}
