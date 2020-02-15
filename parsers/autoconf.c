/*
 *   Copyright (c) 2011, Colomban Wendling <colomban@geany.org>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for Autoconf files.
 */

#include "general.h"	/* must always come first */

#include <string.h>

#include "autoconf.h"
#include "m4.h"

#include "debug.h"
#include "entry.h"
#include "read.h"
#include "keyword.h"
#include "kind.h"
#include "parse.h"


static roleDefinition AutoconfOptwithRoles [] = {
	{ true, "cmdline", "specified in a configure command line" },
};

static roleDefinition AutoconfOptenableRoles [] = {
	{ true, "cmdline", "specified in a configure command line" },
};

static kindDefinition AutoconfKinds[] = {
	{ true, 'p', "package", "packages" },
	{ true, 't', "template", "templates" },
	{ true, 'm', "macro", "autoconf macros" },
	{ true, 'w', "optwith", "options specified with --with-...",
	  .referenceOnly = false, ATTACH_ROLES(AutoconfOptwithRoles)},
	{ true, 'e', "optenable", "options specified with --enable-...",
	  .referenceOnly = false, ATTACH_ROLES(AutoconfOptenableRoles)},
	{ true, 's', "subst", "substitution keys"},
	{ true, 'c', "condition", "automake conditions" },
	{ true, 'd', "definition", "definitions" },
};

typedef enum {
	KEYWORD_init,
	KEYWORD_template,
	KEYWORD_defun,
	KEYWORD_argwith,
	KEYWORD_argenable,
	KEYWORD_subst,
	KEYWORD_conditional,
	KEYWORD_define,
} autoconfKeywordId;

static const keywordTable autoconfKeywordTable[] = {
	{ "AC_INIT",            KEYWORD_init, },
	{ "AH_TEMPLATE",        KEYWORD_template, },
	{ "AC_DEFUN",           KEYWORD_defun, },
	{ "AC_DEFUN_ONCE",      KEYWORD_defun, },
	{ "AC_ARG_WITH",        KEYWORD_argwith, },
	{ "AC_ARG_ENABLE",      KEYWORD_argenable, },
	{ "AC_SUBST",           KEYWORD_subst, },
	{ "AM_CONDITIONAL",     KEYWORD_conditional, },
	{ "AC_DEFINE",          KEYWORD_define, },
	{ "AC_DEFINE_UNQUOTED", KEYWORD_define, },
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

static bool probeLanguage (m4Subparser *m4 CTAGS_ATTR_UNUSED, const char* token)
{
	return strncmp (token, "m4_", 3) == 0
		|| strncmp (token, "AC_", 3) == 0
		|| strncmp (token, "AM_", 3) == 0
		|| strncmp (token, "AS_", 3) == 0
		|| strncmp (token, "AH_", 3) == 0
		;
}

static int makeAutoconfTag (int kind)
{
	int index = CORK_NIL;
	vString * name;

	name = vStringNew();
	readM4MacroArgument(name);
	if (vStringLength (name) > 0)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue(name), kind);
		index = makeTagEntry(&e);
	}
	vStringDelete (name);

	return index;
}

static int newMacroCallback (m4Subparser *m4 CTAGS_ATTR_UNUSED, const char* token)
{
	int keyword;
	int index = CORK_NIL;

	keyword = lookupKeyword (token, getInputLanguage ());

	/* TODO:
	   AH_VERBATIM
	 */
	switch (keyword)
	{
	case KEYWORD_NONE:
		break;
	case KEYWORD_init:
		index = makeAutoconfTag (AUTOCONF_PACKAGE_KIND);
		break;
	case KEYWORD_template:
		index = makeAutoconfTag (AUTOCONF_TEMPLATE_KIND);
		break;
	case KEYWORD_defun:
		index = makeAutoconfTag (AUTOCONF_MACRO_KIND);
		break;
	case KEYWORD_argwith:
		index = makeAutoconfTag (AUTOCONF_OPTWITH_KIND);
		break;
	case KEYWORD_argenable:
		index = makeAutoconfTag (AUTOCONF_OPTENABLE_KIND);
		break;
	case KEYWORD_subst:
		index = makeAutoconfTag (AUTOCONF_SUBST_KIND);
		break;
	case KEYWORD_conditional:
		index = makeAutoconfTag (AUTOCONF_CONDITION_KIND);
		break;
	case KEYWORD_define:
		index = makeAutoconfTag (AUTOCONF_DEFINITION_KIND);
		break;
	default:
		AssertNotReached ();
	}
	return index;
}

static void exclusiveSubparserChosenCallback (subparser *s CTAGS_ATTR_UNUSED, void *data CTAGS_ATTR_UNUSED)
{
	setM4Quotes ('[', ']');
}

static void findAutoconfTags(void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* AutoconfParser (void)
{
	static const char *const patterns [] = { "configure.in", NULL };
	static const char *const extensions [] = { "ac", NULL };
	parserDefinition* const def = parserNew("Autoconf");

	static m4Subparser autoconfSubparser = {
		.subparser = {
			.direction = SUBPARSER_BI_DIRECTION,
			.exclusiveSubparserChosenNotify = exclusiveSubparserChosenCallback,
		},
		.probeLanguage  = probeLanguage,
		.newMacroNotify = newMacroCallback,
		.doesLineCommentStart = doesLineCommentStart,
		.doesStringLiteralStart = doesStringLiteralStart,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "M4", &autoconfSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = AutoconfKinds;
	def->kindCount = ARRAY_SIZE(AutoconfKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findAutoconfTags;
	def->useCork = CORK_QUEUE;

	def->keywordTable = autoconfKeywordTable;
	def->keywordCount = ARRAY_SIZE (autoconfKeywordTable);

	return def;
}
