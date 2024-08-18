/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Bats scripts.
*
*   Reference:
*   https://bats-core.readthedocs.io/en/stable/writing-tests.html
*
*/

#include "general.h"  /* must always come first */

#include "debug.h"
#include "entry.h"
#include "kind.h"
#include "parse.h"
#include "x-sh.h"

#include <ctype.h>


struct sBatsSubparser {
	shSubparser sh;
	int kind;
	int role;
};


typedef enum {
	R_SCRIPT_LOADED,
} batsScriptRole;

static roleDefinition BatsScriptRoles [] = {
	{ true, "loaded", "script loaed with \"load\" command" },
};

typedef enum  {
	K_TEST,
	K_SCRIPT,
} batsKind;

static kindDefinition BatsKinds[] = {
	{ true, 't', "test", "test cases", },
	{ true, 'S', "script", "scripts",
	  .referenceOnly = true, ATTACH_ROLES(BatsScriptRoles)},
};


static void inputStart (subparser *s)
{
	struct sBatsSubparser *bats = (struct sBatsSubparser*)s;

	bats->kind = KIND_GHOST_INDEX;
	bats->role = ROLE_DEFINITION_INDEX;
}

static int lineNotify (shSubparser *s, const unsigned char *cp)
{
	struct sBatsSubparser *bats = (struct sBatsSubparser*)s;

	if (cp[0] == '@' &&
		cp[1] == 't' &&
		cp[2] == 'e' &&
		cp[3] == 's' &&
		cp[4] == 't' &&
		isspace(cp[5]))
	{
		bats->kind = K_TEST;
		bats->role = ROLE_DEFINITION_INDEX;
		return 5;
	}
	else if (cp[0] == 'l' &&
			 cp[1] == 'o' &&
			 cp[2] == 'a' &&
			 cp[3] == 'd' &&
			 isspace(cp[4]))
	{
		bats->kind = K_SCRIPT;
		bats->role = R_SCRIPT_LOADED;
		return 4;
	}
	return 0;
}

static int readDString (const unsigned char *cp, vString *name)
{
	const unsigned char *initial = cp;
	bool escape = false;

	while (*cp)
	{
		if (escape)
		{
			if (!(*cp == '\\' || *cp == '"'))
				vStringPut(name, '\\');
			vStringPut(name, *cp);
			escape = false;
		}
		else if (*cp == '\\')
			escape = true;
		else if (*cp == '"')
			break;
		else
			vStringPut(name, *cp);
		cp++;
	}

	return cp - initial;
}

static int readSString (const unsigned char *cp, vString *name)
{
	const unsigned char *initial = cp;

	while (*cp)
	{
		if (*cp == '\'')
			break;
		vStringPut(name, *cp);
		cp++;
	}

	return cp - initial;
}

static int readToken(const unsigned char *cp, vString *name)
{
	const unsigned char *initial = cp;
	bool escape = false;

	while (*cp)
	{
		if (escape)
		{
			vStringPut(name, *cp);
			escape = false;
		}
		else if (*cp == '\\')
			escape = true;
		else if (isspace(*cp)
				 || *cp == ';'
				 || *cp == '|'
				 || *cp == '&'
				 || *cp == '>'
				 || *cp == '<')
			break;
		else
			vStringPut(name, *cp);
		cp++;
	}

	return cp - initial;
}

static int extractName (shSubparser *s, const unsigned char *cp,
						vString *name)
{
	int n;

	if (*cp == '"')
		n = readDString(cp + 1, name);
	else if (*cp == '\'')
		n = readSString(cp + 1, name);
	else
		n = readToken(cp, name);

	return n;
}

static int makeTag (shSubparser *s, vString *name)
{
	int r;
	struct sBatsSubparser *bats = (struct sBatsSubparser*)s;

	Assert(bats->kind != KIND_GHOST_INDEX);
	r = makeSimpleRefTag(name, bats->kind, bats->role);
	bats->kind = KIND_GHOST_INDEX;
	bats->role = ROLE_DEFINITION_INDEX;

	return r;
}

static struct sBatsSubparser batsSubparser = {
	.sh = {
		.subparser = {
			.direction = SUBPARSER_SUB_RUNS_BASE,
			.inputStart = inputStart,
		},
		.lineNotify  = lineNotify,
		.extractName = extractName,
		.makeTag     = makeTag,
	},
};

static void findBatsTags(void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* BatsParser (void)
{
	static const char *const extensions [] = { "bats", NULL };
	parserDefinition* const def = parserNew("Bats");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Sh", &batsSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = BatsKinds;
	def->kindCount = ARRAY_SIZE(BatsKinds);

	def->extensions = extensions;
	def->parser = findBatsTags;
	def->useCork = CORK_QUEUE;

	return def;
}
