/*
 *   python-entry-points.c
 *
 *   Copyright (c) 2025, Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for config files of
 *   Python's entry_points.txt file.
 *
 *   https://packaging.python.org/en/latest/specifications/entry-points/
 *
 */

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "keyword.h"
#include "kind.h"
#include "parse.h"
#include "x-iniconf.h"
#include "x-python.h"

#include <string.h>

/*
*   DATA DECLARATIONS
*/
struct sPythonEntryPointsSubparser {
	iniconfSubparser iniconf;
};

typedef enum {
	K_CONSOLE,
	K_GUI,
	COUNT_KINDS					/* must be last */
} pythonEntryPointsKind;

typedef enum {
	KEY_UNDEFINED = -1,
	KEY_CONSOLE_SCRIPTS,
	KEY_GUI_SCRIPTS,
} pythonEntryPointsKeyword;

/*
*   DATA DEFINITIONS
*/
static kindDefinition PythonEntryPointsKinds [COUNT_KINDS] = {
	{ true, 'c', "console", "entry points of console_scripts" },
	{ true, 'g', "gui", "entry points of gui_scripts" },
};

static const keywordTable PythonEntryPointsKeywords [] = {
	{ "console_scripts", KEY_CONSOLE_SCRIPTS },
	{ "gui_scripts", KEY_GUI_SCRIPTS },
};

static langType Lang_python;	/* For making foreign tags */

/*
* FUNCTION DEFINITIONS
*/
static void newDataCallback (iniconfSubparser *iniconf,
							 const char *section, const char *key, const char *value)
{
	if (!section)
		return;
	if (!key)
		return;

	int kind;
	langType lang = getSubparserLanguage (&iniconf->subparser);
	switch (lookupKeyword (section, lang))
	{
	case KEY_CONSOLE_SCRIPTS:
		kind = K_CONSOLE;
		break;
	case KEY_GUI_SCRIPTS:
		kind = K_GUI;
		break;
	default:
		return;
	}

	tagEntryInfo e;
	initTagEntry (&e, key, kind);
	makeTagEntry (&e);

	if (!value)
		return;

	char *sep = strrchr (value, ':');
	if (sep)
		*sep = '\0';

	initForeignRefTagEntry (&e, value, Lang_python,
							PYTHON_MODULE_KIND, PYTHON_MODULE_ENTRY_POINT);
	int mod = makeTagEntry (&e);

	if (sep && sep[1])
	{
		initForeignRefTagEntry (&e, sep + 1, Lang_python,
								PYTHON_FUNCTION_KIND, PYTHON_FUNCTION_ENTRY_POINT);
		e.extensionFields.scopeIndex = mod;
		makeTagEntry (&e);
	}

	if (sep)
		*sep = ':';
}

static void findPythonEntryPointsTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* PythonEntryPointsParser (void)
{
	static const char *const patterns [] = { "entry_points.txt", NULL };
	static struct sPythonEntryPointsSubparser pythonEntryPointsSubparser = {
		.iniconf = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
			},
			.newDataNotify = newDataCallback,
		},
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Iniconf", &pythonEntryPointsSubparser },
		[1] = { DEPTYPE_FOREIGNER, "Python",  &Lang_python },
	};

	parserDefinition* const def = parserNew ("PythonEntryPoints");
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->patterns   = patterns;

	def->kindTable = PythonEntryPointsKinds;
	def->kindCount  = ARRAY_SIZE (PythonEntryPointsKinds);

	def->keywordTable = PythonEntryPointsKeywords;
	def->keywordCount = ARRAY_SIZE (PythonEntryPointsKeywords);

	def->parser = findPythonEntryPointsTags;
	def->useCork    = CORK_QUEUE;

	return def;
}
