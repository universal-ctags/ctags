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

#include <ctype.h>
#include <string.h>

#include "entry.h"
#include "iniconf.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"



/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_UNIT,
} systemdUnitKind;

typedef enum {
	R_UNIT_Requires,
	R_UNIT_Wants,
	R_UNIT_After,
	R_UNIT_Before,
	R_UNIT_RequiredBy,
	R_UNIT_WantedBy,

} systemdUnitRole;

static roleDefinition SystemdUnitUnitRoles [] = {
	{ true, "Requires", "referred in Requires key" },
	{ true, "Wants", "referred in Wants key" },
	{ true, "After", "referred in After key" },
	{ true, "Before", "referred in Before key" },
	{ true, "RequiredBy", "referred in RequiredBy key" },
	{ true, "WantedBy", "referred in WantedBy key" },
	/* ... */
};

static kindDefinition SystemdUnitKinds [] = {
	{ true, 'u', "unit", "units",
	  .referenceOnly = true, ATTACH_ROLES(SystemdUnitUnitRoles)},
};

static int roleOf (const char* key, int kind)
{
	int i;

	for (i = 0; i < SystemdUnitKinds [kind].nRoles; i++)
	{
		if (strcmp (SystemdUnitKinds [kind].roles[i].name, key) == 0)
			return i;
	}

	return -1;
}

static void makeSystemdReferencedUnit (const char *value, int kind, int role)
{
	vString *unit = vStringNew ();

	while (*value != '\0')
	{
		if (*value == ',')
		{
			makeSimpleRefTag (unit, kind, role);
			vStringClear (unit);
		}
		else if (! isspace ((int) *value))
			vStringPut (unit, *value);

		value++;
	}

	if (vStringLength (unit) > 0)
		makeSimpleRefTag (unit, kind, role);
	vStringDelete (unit);
}

static void newDataCallback (iniconfSubparser *s CTAGS_ATTR_UNUSED,
							 const char *section CTAGS_ATTR_UNUSED, const char *key, const char *value)
{
	int r;

	if (isXtagEnabled (XTAG_REFERENCE_TAGS) && value)
	{
		r = roleOf (key, K_UNIT);
		if (r >= 0)
			makeSystemdReferencedUnit (value, K_UNIT, r);
	}
}

static void findSystemdUnitTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* SystemdUnitParser (void)
{
	static const char *const extensions [] = { "service", "socket", "device",
											   "mount", "automount", "swap", "target",
											   "path", "timer", "snapshot",
											   "slice", NULL };
	static iniconfSubparser systemdUnitSubparser = {
		.subparser = {
			.direction = SUBPARSER_SUB_RUNS_BASE,
		},
		.newDataNotify = newDataCallback,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Iniconf", &systemdUnitSubparser },
	};

	parserDefinition* const def = parserNew ("SystemdUnit");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable      = SystemdUnitKinds;
	def->kindCount  = ARRAY_SIZE (SystemdUnitKinds);
	def->extensions = extensions;
	def->parser     = findSystemdUnitTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
