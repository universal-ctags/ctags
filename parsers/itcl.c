/*
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"  /* must always come first */
#include "tcl.h"
#include "entry.h"
#include "parse.h"
#include "string.h"

enum {
	K_CLASS,
	K_METHOD,
};

static kindDefinition ITclKinds[] = {
	{ true, 'c', "class", "classes" },
	{ true, 'm', "method", "methods" },
};

static int commandNotify (tclSubparser *s, char *command,
						  int parentIndex)
{
	int r = CORK_NIL;

	if ((strcmp (command, "class") == 0)
		|| (strcmp (command, "itcl::class") == 0))
	{
		tokenInfo *token = newTclToken ();

		tokenRead (token);
		if (tokenIsType (token, TCL_IDENTIFIER))
		{
			tagEntryInfo e;

			initTagEntry(&e, vStringValue (token->string), ITclKinds + K_CLASS);
			e.extensionFields.scopeIndex = parentIndex;
			r = makeTagEntry (&e);
		}
		tokenDestroy(token);
	}
	else if ((strcmp (command, "public") == 0)
			 || (strcmp (command, "protected") == 0)
			 || (strcmp (command, "private") == 0))
	{
		tokenInfo *token = newTclToken ();
		tokenRead (token);
		if (tokenIsType (token, TCL_IDENTIFIER)
			&& (strcmp(vStringValue(token->string), "method") == 0))
		{
			tokenRead (token);
			if (tokenIsType (token, TCL_IDENTIFIER))
			{
				tagEntryInfo e;

				initTagEntry(&e, vStringValue (token->string), ITclKinds + K_METHOD);
				e.extensionFields.scopeIndex = parentIndex;
				r = makeTagEntry (&e);
				/* TODO: modifier should be attached. */
			}
		}
		else
			tokenUnread (token);
		tokenDestroy(token);
	}
	return r;
}

static void findITclTags(void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

extern parserDefinition* ITclParser (void)
{
	static const char *const extensions [] = { "itcl", NULL };
	parserDefinition* const def = parserNew("ITcl");

	static tclSubparser itclSubparser = {
		.subparser = {
			.direction = SUBPARSER_BI_DIRECTION,
		},
		.commandNotify = commandNotify,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Tcl", &itclSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = ITclKinds;
	def->kindCount = ARRAY_SIZE(ITclKinds);

	def->extensions = extensions;
	def->parser = findITclTags;;
	def->useCork = true;

	return def;
}
