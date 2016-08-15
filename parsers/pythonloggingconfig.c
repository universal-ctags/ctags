/*
 *   pythonLoggingConfig.c
 *
 *   Copyright (c) 2016, Masatake YAMATO <yamato@redhat.com>
 *   Copyright (c) 2016, Red Hat, K.K.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for config files of
 *   python's logging.config module.
 *
 *   https://docs.python.org/2/library/logging.config.html
 *
 */

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "iniconf.h"
#include "kind.h"
#include "read.h"
#include "routines.h"
#include <string.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_LOGGER_SECTION,
	K_LOGGER_QUALNAME,
} pythonLoggingConfigKind;

static kindOption PythonLoggingConfigKinds [] = {
	{ TRUE, 'L', "loggerSection", "logger sections" },
	{ TRUE, 'q', "qualname",      "logger qualnames" },
};

#define LOGGER_PREFIX "logger_"
#define LOGGER_LEN (sizeof("logger_") - 1)


static void makePythonLoggingConfigTag (const char *section, const char *key, const char *value,
					   void *userData)
{
	tagEntryInfo e;

	if (section && (strncmp (LOGGER_PREFIX, section, LOGGER_LEN) == 0))
	{
		if (key == NULL && value == NULL)
		{
			const char *logger = section + LOGGER_LEN;
			if (logger [0] == '\0')
				goto out;

			initTagEntry (&e, logger, PythonLoggingConfigKinds + K_LOGGER_SECTION);
			*((int *)userData) = makeTagEntry (&e);
		}
		else if (key && (strcmp (key, "qualname") == 0)
			 && value && value[0] != '\0')
		{
			initTagEntry (&e, value, PythonLoggingConfigKinds + K_LOGGER_QUALNAME);
			e.extensionFields.scopeIndex = *((int *)userData);
			makeTagEntry (&e);
		}
	}

out:
	return;
}

static void *pythonLoggingConfigPrepareForNewInput (void)
{
	static int sectionIndex;

	sectionIndex = CORK_NIL;
	return &sectionIndex;
}

static boolean pythonLoggingConfigProbeLanguage (const char *section, const char *key, const char *value)
{
	if (section && (strncmp (LOGGER_PREFIX, section, LOGGER_LEN) == 0))
		return TRUE;
	else
		return FALSE;
}

static struct iniconfParserClient PythonLoggingConfigIniconfClient = {
	.lang = LANG_IGNORE,
	.prepareForNewInput = pythonLoggingConfigPrepareForNewInput,
	.probeLanguage      = pythonLoggingConfigProbeLanguage,
	.handleInputData    = makePythonLoggingConfigTag,
};

static void findPythonLoggingConfigTags (void)
{
	int sectionIndex = CORK_NIL;

	runIniconfParser (makePythonLoggingConfigTag, &sectionIndex);
}

static void initializePythonLoggingConfigParser (const langType language)
{
	PythonLoggingConfigIniconfClient.lang = language;
	registerIniconfParserClient (&PythonLoggingConfigIniconfClient);
}

extern parserDefinition* PythonLoggingConfigParser (void)
{
	parserDefinition* const def = parserNew ("PythonLoggingConfig");
	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "Iniconf" },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->initialize = initializePythonLoggingConfigParser;

	def->kinds      = PythonLoggingConfigKinds;
	def->kindCount  = ARRAY_SIZE (PythonLoggingConfigKinds);
	def->parser     = findPythonLoggingConfigTags;
	def->useCork    = TRUE;

	return def;
}
