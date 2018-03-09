/*
*   Copyright (c) 2017, Masatake YAMATO <yamato@redhat.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   A parser for javascript part of vue file
*/
/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "jscript.h"
#include "read.h"

#include <string.h>


typedef enum {
	K_FUNCTION,
	K_CLASS,
	K_METHOD,
	K_PROPERTY,
	K_CONSTANT,
	K_VARIABLE,
	K_GENERATOR,
} vueJScriptKind;

static kindDefinition VueJScriptKinds [] = {
	{ true,  'f', "function",	  "functions"		   },
	{ true,  'c', "class",		  "classes"			   },
	{ true,  'm', "method",		  "methods"			   },
	{ true,  'p', "property",	  "properties"		   },
	{ true,  'C', "constant",	  "constants"		   },
	{ true,  'v', "variable",	  "global variables"   },
	{ true,  'g', "generator",	  "generators"		   }
};


struct sVueJScriptSubparser {
	javaScriptSubparser jscript;
	int scopeIndex;
};

static void inputStart (subparser *s)
{
	struct sVueJScriptSubparser *vueJscript = (struct sVueJScriptSubparser*)s;

	vueJscript->scopeIndex = CORK_NIL;
}

static bool level2InDefaultJSDefaultScope(tagEntryInfo *tag)
{
	if (tag->extensionFields.scopeName)
	{
		if (strncmp(tag->extensionFields.scopeName, "default.", 8) == 0)
		{
			if (strchr(tag->extensionFields.scopeName + 8, '.') == NULL)
				return true;
		}
	}
	return false;
}

static void tagEntryNotify (javaScriptSubparser *s, tagEntryInfo *tag)
{
	struct sVueJScriptSubparser *v = (struct sVueJScriptSubparser*)s;

	if (tag->extensionFields.scopeKindIndex == JSTAG_CLASS
		&& tag->extensionFields.scopeName
		&& (strcmp(tag->extensionFields.scopeName, "default") == 0))
	{
		tagEntryInfo e = *tag;
		e.langType = getInputLanguage ();
		e.kindIndex = tag->kindIndex; /* TODO: Mapping from JavaScript to VueJavaScript must be prepared.*/
		e.extensionFields.scopeName = "default";
		e.extensionFields.scopeKindIndex = K_CLASS;
		e.extensionFields.scopeIndex = CORK_NIL;
		v->scopeIndex = makeTagEntry (&e);
	}
	else if (level2InDefaultJSDefaultScope(tag))
	{
		tagEntryInfo e = *tag;
		e.langType = getInputLanguage ();
		e.kindIndex = tag->kindIndex; /* TODO: Mapping from JavaScript to VueJavaScript must be prepared.*/
		e.extensionFields.scopeIndex = v->scopeIndex;
		makeTagEntry (&e);
	}
}

static void doNothing (void)
{
}

extern parserDefinition* VueJavaScriptParser (void)
{
	static struct sVueJScriptSubparser vueJScriptSubparser = {
		.jscript = {
			.subparser = {
				.direction = SUBPARSER_BASE_RUNS_SUB,
				.inputStart = inputStart,
			},
			.tagEntryNotify = tagEntryNotify,
		},
		.scopeIndex = CORK_NIL,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "JavaScript", &vueJScriptSubparser },
	};

	parserDefinition* const def = parserNew ("VueJavaScript");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable      = VueJScriptKinds;
	def->kindCount  = ARRAY_SIZE (VueJScriptKinds);
	def->parser     = doNothing;
	def->useCork    = true;
	def->requestAutomaticFQTag = true;
	return def;
}
