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

#include <string.h>

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
	K_XTASK,
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
	{ true, 'x', "xtask", "tasks defined with special constructor",
	  ATTACH_SEPARATORS(RakeGenericSeparators)},
};

/*
* FUNCTIONS
*/
static void findRakeTags (void)
{
	scheduleRunningBaseparser (0);
}

static vString *readTask (const unsigned char **cp, bool *variable)
{
	vString *vstr = NULL;
	unsigned char b;
	const unsigned char *start;

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
		start = *cp;
		if (!rubyParseMethodName (cp, vstr))
		{
			vStringDelete (vstr);
			vstr = NULL;
		}
		else
		{
			const char *end = strstr((const char *)start, vStringValue (vstr));
			if (end)
			{
				end += vStringLength (vstr);
				if (*end != ':')
					*variable = true;
			}
		}
		break;
	}
	return vstr;
}

static int makeSimpleRakeTag (vString *vstr, int kindIndex, rubySubparser *subparser,
							  bool anonymous)
{
	if (anonymous)
	{
		vStringPut (vstr, '_');
		anonConcat (vstr, kindIndex);
	}

	int r = makeSimpleTag (vstr, kindIndex);
	tagEntryInfo *e = getEntryInCorkQueue (r);
	if (e)
	{
		struct sRakeSubparser *rake = (struct sRakeSubparser *)subparser;
		if (!intArrayIsEmpty (rake->namespaces))
			e->extensionFields.scopeIndex = intArrayLast(rake->namespaces);
		if (anonymous)
			markTagExtraBit (e, XTAG_ANONYMOUS);
	}
	return r;
}

struct taskType {
	const char *keyword;
	rakeKind    kind;
};

static int parseTask (rubySubparser *s, int kind, const unsigned char **cp)
{
	vString *vstr = NULL;
	bool variable = false;
	rubySkipWhitespace (cp);
	vstr = readTask (cp, &variable);
	if (vstr)
	{
		int r = makeSimpleRakeTag (vstr, kind, s, variable);
		vStringDelete (vstr);
		return r;
	}
	return CORK_NIL;
}

static int parseXTask (rubySubparser *s, struct taskType *xtask, const unsigned char **cp)
{
	rubySkipWhitespace (cp);
	if (**cp == '(' || **cp == '"' || **cp == '\'')
	{
		vString *vstr = NULL;
		bool variable = false;
		if (**cp == '(')
		{
			++*cp;
			rubySkipWhitespace (cp);
		}
		vstr = readTask (cp, &variable);
		if (vstr)
		{
			int r = makeSimpleRakeTag (vstr, xtask->kind, s, variable);
			vStringDelete (vstr);
			tagEntryInfo *e = getEntryInCorkQueue (r);
			e->extensionFields.typeRef [0] = eStrdup ("typename");
			e->extensionFields.typeRef [1] = eStrdup (xtask->keyword);
			return r;
		}
	}
	return CORK_NIL;
}

static int lineNotify (rubySubparser *s, const unsigned char **cp)
{
	struct taskType taskTypes [] = {
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

	struct taskType xtaskTypes [] = {
		{ "RSpec::Core::RakeTask.new", K_XTASK },
		{ "Cucumber::Rake::Task.new",  K_XTASK },
		{ "Rake::TestTask.new",        K_XTASK },
		{ "Rake::PackageTask.new",     K_XTASK },
		/* ... */
	};

	for (int i = 0; i < ARRAY_SIZE(xtaskTypes); i++)
	{
		if (rubyCanMatchKeywordWithAssign (cp, xtaskTypes[i].keyword))
			return parseXTask (s, xtaskTypes + i, cp);;
	}

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
