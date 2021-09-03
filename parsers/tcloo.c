/*
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"  /* must always come first */
#include "tcl.h"
#include "param.h"
#include "parse.h"
#include "entry.h"
#include "tokeninfo.h"

#include <string.h>


struct tclooSubparser {
	tclSubparser tcl;
	bool foundTclOONamespaceImported;
};

static scopeSeparator TclOOGenericSeparators [] = {
	{ KIND_WILDCARD_INDEX, "::" },
};

enum TclOOKind {
	K_CLASS,
	K_METHOD,
};

static kindDefinition TclOOKinds[] = {
	{ true, 'c', "class", "classes" },
	{ true, 'm', "method", "methods",
	  ATTACH_SEPARATORS(TclOOGenericSeparators) },
};

static bool tclooForceUse;

static void parseMethod (tokenInfo *token, int owner)
{
	tokenRead (token);
	if (tokenIsType (token, TCL_IDENTIFIER))
	{
		tagEntryInfo e;

		initTagEntry(&e, tokenString (token), K_METHOD);
		e.extensionFields.scopeIndex = owner;
		makeTagEntry (&e);
	}
	skipToEndOfTclCmdline (token);
}

static void parseSuperclass (tokenInfo *token, int this_class)
{
	tokenRead (token);
	if (tokenIsType (token, TCL_IDENTIFIER))
	{
		tagEntryInfo *e = getEntryInCorkQueue(this_class);

		if (e)
		{
			if (e->extensionFields.inheritance)
			{   /* superclass is used twice in a class. */
				eFree ((void *)e->extensionFields.inheritance);
			}
			e->extensionFields.inheritance = eStrdup(tokenString(token));
		}
	}
	skipToEndOfTclCmdline (token);
}

static int parseClass (tclSubparser *s CTAGS_ATTR_UNUSED, int parentIndex,
					   void *pstate)
{
	tokenInfo *token = newTclToken (pstate);
	int r = CORK_NIL;

	tokenRead (token);
	if (tokenIsType (token, TCL_IDENTIFIER)
		&& (strcmp(tokenString(token), "create") == 0))
	{
		tokenRead (token);
		if (tokenIsType (token, TCL_IDENTIFIER))
		{
			tagEntryInfo e;

			initTagEntry(&e, tokenString (token), K_CLASS);
			e.extensionFields.scopeIndex = parentIndex;
			r = makeTagEntry (&e);
		}

		if (tokenSkipToType (token, '{'))
		{
			do {
				tokenRead (token);
				if (tokenIsType (token, TCL_IDENTIFIER)
					|| tokenIsType (token, TCL_KEYWORD))
				{
					if (strcmp(tokenString(token), "method") == 0)
						parseMethod(token, r);
					else if (strcmp(tokenString(token), "superclass") == 0)
						parseSuperclass(token, r);
					else
						skipToEndOfTclCmdline (token);
				}
				else if (token->type == '}')
					break;
			} while (!tokenIsEOF(token));
		}
	}

	skipToEndOfTclCmdline (token);
	tokenDelete(token);
	return r;
}

static int commandNotify (tclSubparser *s, char *command,
						  int parentIndex, void *pstate)
{
	struct tclooSubparser *tcloo = (struct tclooSubparser *)s;
	int r = CORK_NIL;

	if ((tcloo->foundTclOONamespaceImported
		 && (strcmp (command, "class") == 0))
		|| (strcmp (command, "oo::class") == 0))
		r = parseClass (s, parentIndex, pstate);

	return r;
}

static void namespaceImportNotify (tclSubparser *s, char *namespace,
								   void *pstate CTAGS_ATTR_UNUSED)
{
	struct tclooSubparser *tcloo = (struct tclooSubparser *)s;

	if (strcmp(namespace, "oo::*") == 0
		|| strcmp(namespace, "oo::class") == 0)
		tcloo->foundTclOONamespaceImported = true;
}

static void inputStart (subparser *s)
{
	struct tclooSubparser *tcloo = (struct tclooSubparser *)s;

	tcloo->foundTclOONamespaceImported = tclooForceUse;
}

static struct tclooSubparser tclooSubparser = {
	.tcl = {
		.subparser = {
			.direction = SUBPARSER_BI_DIRECTION,
			.inputStart = inputStart,
		},
		.commandNotify = commandNotify,
		.namespaceImportNotify = namespaceImportNotify,
	},
};

static void findTclOOTags(void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void tclooForceUseParamHandler (const langType language CTAGS_ATTR_UNUSED,
									  const char *name, const char *arg)
{
	tclooForceUse = paramParserBool (arg, tclooForceUse, name, "parameter");
}

static parameterHandlerTable TclOOParameterHandlerTable [] = {
	{ .name = "forceUse",
	  .desc = "enable the parser even when `oo' namespace is not specified in the input (true or [false])" ,
	  .handleParameter = tclooForceUseParamHandler,
	},
};

extern parserDefinition* TclOOParser (void)
{
	parserDefinition* const def = parserNew("TclOO");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Tcl", &tclooSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = TclOOKinds;
	def->kindCount = ARRAY_SIZE(TclOOKinds);

	def->parser = findTclOOTags;
	def->useCork = CORK_QUEUE;
	def->requestAutomaticFQTag = true;

	def->parameterHandlerTable = TclOOParameterHandlerTable;
	def->parameterHandlerCount = ARRAY_SIZE(TclOOParameterHandlerTable);

	return def;
}
