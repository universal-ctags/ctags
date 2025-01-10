/*
*   Copyright (c) 2025 Masatake YAMATO
*   Copyright (c) 2025 Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for *if files in SELinux policy definitions:
*
*   https://github.com/SELinuxProject/refpolicy/blob/main/policy/support/loadable_module.spt
*
*/

#include "general.h"	/* must always come first */

#include <string.h>

#include "x-m4.h"

#include "debug.h"
#include "entry.h"
#include "read.h"
#include "keyword.h"
#include "kind.h"
#include "parse.h"

typedef enum {
	INTERFACE_KIND,
	TEMPLATE_KIND,
} autoconfKind;

static kindDefinition SELinuxInterfaceKinds[] = {
	{ true, 'i', "interface", "interfaces" },
	{ true, 't', "template", "templates" },
};

typedef enum {
	KEYWORD_interface,
	KEYWORD_template,
} selinuxInterfaceKeywordId;

static const keywordTable selinuxInterfaceKeywordTable[] = {
	{ "interface", KEYWORD_interface, },
	{ "template",  KEYWORD_template, },
};

static bool doesLineCommentStart (m4Subparser *m4 CTAGS_ATTR_UNUSED, int c, const char* token CTAGS_ATTR_UNUSED)
{
	return (c == '#');
}

static bool doesStringLiteralStart (m4Subparser *m4 CTAGS_ATTR_UNUSED, int c CTAGS_ATTR_UNUSED)
{
	// return (c == '"') || (c == '\'') || (c == '`');
	return false;
}

static bool doesWantToParseInsideQuotes (m4Subparser *m4 CTAGS_ATTR_UNUSED, intArray *indexStack CTAGS_ATTR_UNUSED)
{
	return false;
}

static int makeSELinuxInterfaceTag (int kind)
{
	int index = CORK_NIL;
	vString * name;

	name = vStringNew();
	readM4MacroArgument(name);
	if (vStringLength (name) > 0)
		index = makeSimpleTag(name, kind);
	vStringDelete (name);

	return index;
}

static int newMacroCallback (m4Subparser *m4 CTAGS_ATTR_UNUSED, const char* token)
{
	int keyword;
	int index = CORK_NIL;

	keyword = lookupKeyword (token, getInputLanguage ());

	switch (keyword)
	{
	case KEYWORD_NONE:
		break;
	case KEYWORD_interface:
		index = makeSELinuxInterfaceTag (INTERFACE_KIND);
		break;
	case KEYWORD_template:
		index = makeSELinuxInterfaceTag (TEMPLATE_KIND);
		break;
	default:
		AssertNotReached ();
	}
	return index;
}

static void findSELinuxInterfaceTags(void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* SELinuxInterfaceParser (void)
{
	static const char *const extensions [] = { "if", NULL };
	parserDefinition* const def = parserNew("SELinuxInterface");

	static m4Subparser selinuxInterfaceSubparser = {
		.subparser = {
			.direction = SUBPARSER_SUB_RUNS_BASE,
		},
		.probeLanguage  = NULL,
		.newMacroNotify = newMacroCallback,
		.doesLineCommentStart = doesLineCommentStart,
		.doesStringLiteralStart = doesStringLiteralStart,
		.doesWantToParseInsideQuotes = doesWantToParseInsideQuotes,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "M4", &selinuxInterfaceSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = SELinuxInterfaceKinds;
	def->kindCount = ARRAY_SIZE(SELinuxInterfaceKinds);
	def->extensions = extensions;
	def->parser = findSELinuxInterfaceTags;
	def->useCork = CORK_QUEUE;

	def->keywordTable = selinuxInterfaceKeywordTable;
	def->keywordCount = ARRAY_SIZE (selinuxInterfaceKeywordTable);

	return def;
}
