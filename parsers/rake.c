/*
*   Copyright (c) 2022 Masatake YAMATO <yamato@redhat.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Rakefile.
*
*   Reference:
*   - https://ruby.github.io/rake/doc/rakefile_rdoc.html
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "kind.h"
#include "numarray.h"
#include "parse.h"
#include "subparser.h"

#include "ruby.h"

/*
* DATA STRUCTURES
*/
struct sRakeSubparser {
	rubySubparser ruby;
	intArray *namespaces;
};

typedef enum {
	K_TASK,
	K_NAMESPACE,
	K_FILE,
	K_DIRECTORY,
	K_MULTITASK,
} rakeKind;

/*
* DATA DEFINITIONS
*/
static scopeSeparator RakeGenericSeparators [] = {
	{ KIND_WILDCARD_INDEX, ":" },
};

static kindDefinition RakeKinds [] = {
	{ true, 't', "task", "tasks",
	  ATTACH_SEPARATORS(RakeGenericSeparators)},
	{ true, 'n', "namespace", "namespaces",
	  ATTACH_SEPARATORS(RakeGenericSeparators)},
	/* F/file is reserved in the main part. */
	{ true, 'f', "File", "file tasks",
	  ATTACH_SEPARATORS(RakeGenericSeparators)},
	{ true, 'd', "directory", "directory tasks",
	  ATTACH_SEPARATORS(RakeGenericSeparators)},
	{ true, 'm', "multitask", "multi tasks",
	  ATTACH_SEPARATORS(RakeGenericSeparators)},
};

/*
* FUNCTIONS
*/
static void findRakeTags (void)
{
	scheduleRunningBaseparser (0);
}

static vString *readTask (const unsigned char **cp)
{
	vString *vstr = NULL;
	unsigned char b;

	switch (**cp)
	{
	case '\'':
	case '"':
		b = **cp;
		++*cp;
		vstr = vStringNew ();
		if (!rubyParseString (cp, b, vstr))
		{
			vStringDelete (vstr);
			vstr = NULL;
		}
		break;
	case ':':
		++*cp;
		vstr = vStringNew ();
		if (!rubyParseMethodName (cp, vstr))
		{
			vStringDelete (vstr);
			vstr = NULL;
		}
		break;
	default:
		vstr = vStringNew ();
		if (!rubyParseMethodName (cp, vstr))
		{
			vStringDelete (vstr);
			vstr = NULL;
		}
		break;
	}
	return vstr;
}

static int makeSimpleRakeTag (vString *vstr, int kindIndex, rubySubparser *subparser)
{
	int r = makeSimpleTag (vstr, kindIndex);
	tagEntryInfo *e = getEntryInCorkQueue (r);
	if (e)
	{
		struct sRakeSubparser *rake = (struct sRakeSubparser *)subparser;
		if (!intArrayIsEmpty (rake->namespaces))
			e->extensionFields.scopeIndex = intArrayLast(rake->namespaces);
	}
	return r;
}

static int parseTask (rubySubparser *s, int kind, const unsigned char **cp)
{
	vString *vstr = NULL;
	rubySkipWhitespace (cp);
	vstr = readTask (cp);
	if (vstr)
	{
		int r = makeSimpleRakeTag (vstr, kind, s);
		vStringDelete (vstr);
		return r;
	}
	return CORK_NIL;
}

static int lineNotify (rubySubparser *s, const unsigned char **cp)
{
	struct taskType {
		const char *keyword;
		rakeKind    kind;
	} taskTypes [] = {
		{ "task",       K_TASK      },
		{ "namespace",  K_NAMESPACE },
		{ "file",       K_FILE      },
		{ "directory",  K_DIRECTORY },
		{ "multitask",  K_MULTITASK },
	};

	for (int i = 0; i < ARRAY_SIZE(taskTypes); i++)
	{
		if (rubyCanMatchKeywordWithAssign (cp, taskTypes[i].keyword))
			return parseTask (s, taskTypes[i].kind, cp);
	}

#if 0
	if (rubyCanMatchKeywordWithAssign (cp, "Rake::TestTask.new"))
	{
		rubySkipWhitespace (cp);
		if (**cp == '(')
		{
			vString *vstr = NULL;
			++*cp;
			rubySkipWhitespace (cp);
			vstr = readTask (cp);
			if (vstr)
			{
				int r = makeSimpleTag (vstr, K_TARGET);
				vStringDelete (vstr);
				return r;
			}
		}
	}
#endif

	return CORK_NIL;
}

static void inputStart (subparser *s)
{
	struct sRakeSubparser *rake = (struct sRakeSubparser *)s;

	intArrayClear (rake->namespaces);
}

static void enterBlockNotify (rubySubparser *s, int corkIndex)
{
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);

	if (e && e->kindIndex == K_NAMESPACE)
	{
		struct sRakeSubparser *rake = (struct sRakeSubparser *)s;
		intArrayAdd (rake->namespaces, corkIndex);
	}
}

static void leaveBlockNotify (rubySubparser *s, int corkIndex)
{
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);

	if (e && e->kindIndex == K_NAMESPACE)
	{
		struct sRakeSubparser *rake = (struct sRakeSubparser *)s;
		intArrayRemoveLast (rake->namespaces);
	}
}

static struct sRakeSubparser rakeSubparser = {
	.ruby = {
		.subparser = {
			.direction = SUBPARSER_SUB_RUNS_BASE,
			.inputStart = inputStart,
		},
		.lineNotify = lineNotify,
		.enterBlockNotify = enterBlockNotify,
		.leaveBlockNotify = leaveBlockNotify,
	},
	.namespaces = NULL,
};

static void initialize (const langType language CTAGS_ATTR_UNUSED)
{
	rakeSubparser.namespaces = intArrayNew ();
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (rakeSubparser.namespaces)
		intArrayDelete (rakeSubparser.namespaces);
}

extern parserDefinition* RakeParser (void)
{
	static const char *const extensions [] = { "rake", NULL };
	static const char *const patterns [] = { "Rakefile", NULL };

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Ruby", &rakeSubparser },
	};

	parserDefinition* const def = parserNew ("Rake");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable  = RakeKinds;
	def->kindCount  = ARRAY_SIZE (RakeKinds);
	def->extensions = extensions;
	def->patterns   = patterns;
	def->parser     = findRakeTags;
	def->initialize = initialize;
	def->finalize   = finalize;
	def->useCork    = CORK_QUEUE;
	def->requestAutomaticFQTag = true;

	return def;
}
