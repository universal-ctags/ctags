/*
*
*   Copyright (c) 2015, Red Hat, Inc.
*   Copyright (c) 2015, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "x-iniconf.h"
#include "x-systemdunit.h"
#include "parse.h"
#include "selectors.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_NAME,
} dbusServiceKind;

static kindDefinition DbusServiceKinds [] = {
	{ true, 'n', "name", "names" },
};

static langType Lang_systemdunit;

static void newDataCallback (iniconfSubparser *s CTAGS_ATTR_UNUSED,
							 const char *section, const char *key, const char *value)
{
	if (section && key && strcmp (section, "D-BUS Service") == 0)
	{
		tagEntryInfo e;
		if (strcmp (key, "Name") == 0)
		{
			initTagEntry (&e, value, K_NAME);
			makeTagEntry (&e);
		}
		else if (strcmp (key, "SystemdService") == 0)
		{
			initForeignRefTagEntry(&e, value, Lang_systemdunit,
								   SYSTEMD_UNIT_KIND, SYSTEMD_UNIT_FOREIGNLANG_ROLE);
			makeTagEntry (&e);
		}
		/* TODO: Exec, User */
	}
}

static void findDbusServiceTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* DbusServiceParser (void)
{
	static const char *const extensions [] = { "service", NULL };
	static iniconfSubparser dbusServiceSubparser = {
		.subparser = {
			.direction = SUBPARSER_SUB_RUNS_BASE,
		},
		.newDataNotify = newDataCallback,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Iniconf", &dbusServiceSubparser },
		[1] = { DEPTYPE_FOREIGNER, "SystemdUnit", &Lang_systemdunit },
	};

	static selectLanguage selectors[] = {
		selectByDBusServiceAndSystemdUnitSectionNames,
		NULL
	};

	parserDefinition* const def = parserNew ("DBusService");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable      = DbusServiceKinds;
	def->kindCount  = ARRAY_SIZE (DbusServiceKinds);
	def->extensions = extensions;
	def->selectLanguage = selectors;
	def->parser     = findDbusServiceTags;
	def->useCork    = CORK_QUEUE;

	return def;
}
