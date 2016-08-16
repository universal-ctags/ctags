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

#include "m4.h"

#include "debug.h"
#include "entry.h"
#include "read.h"
#include "keyword.h"
#include "kind.h"
#include "parse.h"

enum {
	PACKAGE_KIND,
	TEMPLATE_KIND,
	MACRO_KIND,
	OPTWITH_KIND,
	OPTENABLE_KIND,
	SUBST_KIND,
	CONDITION_KIND,
	DEFINITION_KIND,
};

static kindOption AutoconfKinds[] = {
	{ TRUE, 'p', "package", "packages" },
	{ TRUE, 't', "template", "templates" },
	{ TRUE, 'm', "macro", "autoconf macros" },
	{ TRUE, 'w', "optwith", "options specified with --with-..."},
	{ TRUE, 'e', "optenable", "options specified with --enable-..."},
	{ TRUE, 's', "subst", "substitution keys"},
	{ TRUE, 'c', "condition", "automake conditions" },
	{ TRUE, 'd', "definition", "definitions" },
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

static void *autoconfPrepareForNewInput (void)
{
	static int roleIndex;

	roleIndex = ROLE_INDEX_DEFINITION;
	return &roleIndex;
}

static boolean autoconfDoesStartLineComment (int c, const char* token, void *data)
{
	return (c == '#');
}

static boolean autoconfDoesStartStringLiteral (int c, void *data)
{
	return (c == '"') || (c == '\'') || (c == '`');
}

static boolean autoconfProbeLanguage (const char* token)
{
	return strncmp (token, "m4_", 3) == 0
		|| strncmp (token, "AC_", 3) == 0
		|| strncmp (token, "AM_", 3) == 0
		|| strncmp (token, "AS_", 3) == 0
		|| strncmp (token, "AH_", 3) == 0
		;
}

static int autoconfMakeTag (int kind)
{
	int index = CORK_NIL;
	vString * name;

	if (!isLanguageEnabled (getInputLanguage ()))
		return CORK_NIL;

	name = vStringNew();
	readM4MacroArgument(name);
	if (vStringLength (name) > 0)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue(name), AutoconfKinds + kind);
		index = makeTagEntry(&e);
	}
	vStringDelete (name);

	return index;
}

struct m4HandleTokenResult autoconfHandleMacro (const char* token, void *data)
{
	static langType lang = LANG_IGNORE;
	int keyword;
	struct m4HandleTokenResult result = {
		.index = CORK_NIL,
		.consumed = TRUE,
	};

	if (lang == LANG_IGNORE)
		lang = getNamedLanguage ("Autoconf", 0);

	keyword = lookupKeyword (token, lang);

	/* TODO:
	   AH_VERBATIM
	 */
	switch (keyword)
	{
	case KEYWORD_NONE:
		result.consumed = FALSE;
		break;
	case KEYWORD_init:
		result.index = autoconfMakeTag (PACKAGE_KIND);
		break;
	case KEYWORD_template:
		result.index = autoconfMakeTag (TEMPLATE_KIND);
		break;
	case KEYWORD_defun:
		result.index = autoconfMakeTag (MACRO_KIND);
		break;
	case KEYWORD_argwith:
		result.index = autoconfMakeTag (OPTWITH_KIND);
		break;
	case KEYWORD_argenable:
		result.index = autoconfMakeTag (OPTENABLE_KIND);
		break;
	case KEYWORD_subst:
		result.index = autoconfMakeTag (SUBST_KIND);
		break;
	case KEYWORD_conditional:
		result.index = autoconfMakeTag (CONDITION_KIND);
		break;
	case KEYWORD_define:
		result.index = autoconfMakeTag (DEFINITION_KIND);
		break;
	default:
		AssertNotReached ();
		result.consumed = FALSE;
	}
	return result;
}

static struct m4ParserClient AutoconfM4Client = {
	/* the value will be updated when the Autoconf parser is initialized. */
	.lang = LANG_IGNORE,

	.quoteOpen = '[',
	.quoteClose = ']',
	.prepareForNewInput = autoconfPrepareForNewInput,
	.doesStartLineComment = autoconfDoesStartLineComment,
	.doesStartStringLiteral = autoconfDoesStartStringLiteral,
	.probeLanguage = autoconfProbeLanguage,
	.handleMacro = autoconfHandleMacro,
};

static void initializeAutoconfParser (const langType language)
{
	AutoconfM4Client.lang = language;
	registerM4ParserClient ("M4", &AutoconfM4Client);
}

static void findAutoconfTags(void)
{
	langType lang = getInputLanguage ();

	runM4Parser (lang);
}

extern parserDefinition* AutoconfParser (void)
{
	static const char *const patterns [] = { "configure.in", NULL };
	static const char *const extensions [] = { "ac", NULL };
	parserDefinition* const def = parserNew("Autoconf");

	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "M4" },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kinds = AutoconfKinds;
	def->kindCount = ARRAY_SIZE(AutoconfKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findAutoconfTags;
	def->initialize = initializeAutoconfParser;
	def->useCork = TRUE;

	def->keywordTable = autoconfKeywordTable;
	def->keywordCount = ARRAY_SIZE (autoconfKeywordTable);

	return def;
}
