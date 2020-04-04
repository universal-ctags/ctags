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
#include "parse.h"
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

static kindDefinition PythonLoggingConfigKinds [] = {
	{ true, 'L', "loggerSection", "logger sections" },
	{ true, 'q', "qualname",      "logger qualnames" },
};

#define LOGGER_PREFIX "logger_"
#define LOGGER_LEN (sizeof("logger_") - 1)

struct sPythonLoggingConfigSubparser {
	iniconfSubparser iniconf;
	int index;
};

static void newDataCallback (iniconfSubparser *iniconf,
							 const char *section, const char *key, const char *value)
{
	tagEntryInfo e;

	if (section && (strncmp (LOGGER_PREFIX, section, LOGGER_LEN) == 0))
	{
		if (key == NULL && value == NULL)
		{
			const char *logger = section + LOGGER_LEN;
			if (logger [0] == '\0')
				goto out;

			initTagEntry (&e, logger, K_LOGGER_SECTION);
			((struct sPythonLoggingConfigSubparser *)iniconf)->index = makeTagEntry (&e);
		}
		else if (key && (strcmp (key, "qualname") == 0)
			 && value && value[0] != '\0')
		{
			initTagEntry (&e, value, K_LOGGER_QUALNAME);
			e.extensionFields.scopeIndex = ((struct sPythonLoggingConfigSubparser*)iniconf)->index;
			makeTagEntry (&e);
		}
	}

out:
	return;
}

static bool probeLanguage (const char *section, const char *key CTAGS_ATTR_UNUSED, const char *value CTAGS_ATTR_UNUSED)
{
	if (section && (strncmp (LOGGER_PREFIX, section, LOGGER_LEN) == 0))
		return true;
	else
		return false;
}


static void exclusiveSubparserChosenCallback (subparser *s, void *data CTAGS_ATTR_UNUSED)
{
	((struct sPythonLoggingConfigSubparser *)s)->index = CORK_NIL;
}

static void findPythonLoggingConfigTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* PythonLoggingConfigParser (void)
{
	static struct sPythonLoggingConfigSubparser pythonLoggingConfigSubparser = {
		.iniconf = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
				.exclusiveSubparserChosenNotify = exclusiveSubparserChosenCallback,
			},
			.probeLanguage = probeLanguage,
			.newDataNotify = newDataCallback,
		},
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Iniconf", &pythonLoggingConfigSubparser },
	};

	parserDefinition* const def = parserNew ("PythonLoggingConfig");
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable      = PythonLoggingConfigKinds;
	def->kindCount  = ARRAY_SIZE (PythonLoggingConfigKinds);
	def->parser     = findPythonLoggingConfigTags;
	def->useCork    = CORK_QUEUE;

	return def;
}
