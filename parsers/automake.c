/*
*   Copyright (c) 2000-2005, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for makefiles.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#include "meta-make.h"

#include "entry.h"
#include "htable.h"
#include "kind.h"
#include "parse.h"
#include "read.h"


typedef enum {
	K_AM_DIR,
	K_AM_PROGRAM,
	K_AM_MAN,
	K_AM_LTLIBRARY,
	K_AM_LIBRARY,
	K_AM_SCRIPT,
	K_AM_DATA,
	K_AM_CONDITION,
} makeAMKind;

typedef enum {
	R_AM_DIR_PROGRAMS,
	R_AM_DIR_MANS,
	R_AM_DIR_LTLIBRARIES,
	R_AM_DIR_LIBRARIES,
	R_AM_DIR_SCRIPTS,
	R_AM_DIR_DATA,
} makeAMDirectoryRole;

static roleDesc AutomakeDirectoryRoles [] = {
	{ true, "program",   "directory for PROGRAMS primary" },
	{ true, "man",       "directory for MANS primary" },
	{ true, "ltlibrary", "directory for LTLIBRARIES primary"},
	{ true, "library",   "directory for LIBRARIES primary"},
	{ true, "script",    "directory for SCRIPTS primary"},
	{ true, "data",      "directory for DATA primary"},
};

typedef enum {
	R_AM_CONDITION_BRANCHED,
} makeAMConditionRole;

static roleDesc AutomakeConditionRoles [] = {
	{ true, "branched",  "used for branching" },
};

static scopeSeparator AutomakeSeparators [] = {
	{ 'd'          , "/" },
};

static kindOption AutomakeKinds [] = {
	{ true, 'd', "directory", "directories",
	  .referenceOnly = false, ATTACH_ROLES(AutomakeDirectoryRoles)},
	{ true, 'P', "program",   "programs",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ true, 'M', "man",       "manuals",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ true, 'T', "ltlibrary", "ltlibraries",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ true, 'L', "library",   "libraries",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ true, 'S', "script",    "scripts",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ true, 'D', "data",      "datum",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ true, 'c', "condition", "conditions",
	  .referenceOnly = true, ATTACH_ROLES(AutomakeConditionRoles) },
};

static hashTable* AutomakeDirectories;


struct sBlacklist {
	enum { BL_END, BL_PREFIX } type;
	const char* substr;
	size_t len;
} am_blacklist [] = {
	{ BL_PREFIX, "EXTRA",  5 },
	{ BL_PREFIX, "noinst", 6 },
	{ BL_PREFIX, "check",  5 },
	{ BL_END,    NULL,     0 },
};


static bool bl_check (const char *name, struct sBlacklist *blacklist)
{
	if ((blacklist->type == BL_PREFIX) &&
	    (strncmp (blacklist->substr, name, blacklist->len) == 0))
		return false;
	else
		return true;
}

static int lookupAutomakeDirectory (vString *const name)
{
	int *i = hashTableGetItem (AutomakeDirectories,  vStringValue (name));

	if (i)
		return *i;
	else
		return CORK_NIL;
}

static void addAutomakeDirectory (vString *const name, int corkIndex)
{
	char * k = eStrdup (vStringValue (name));
	int  * i = xMalloc (1, int);

	*i = corkIndex;

	hashTablePutItem (AutomakeDirectories, k, i);
}

static bool AutomakeMakeTag (vString *const name, const char* suffix, bool appending,
			    int kindex, int rindex, struct sBlacklist *blacklist,
			    void *data)
{
	int *index = data;
	size_t expected_len;
	size_t len;
	char* tail;
	vString *subname;
	int i;

	len = vStringLength (name);
	expected_len = strlen (suffix);

	if (len <= expected_len)
		return false;

	for (i = 0; blacklist[i].type != BL_END; i++)
	{
		if (bl_check (vStringValue(name), blacklist + i) == false)
			return false;
	}

	tail = vStringValue (name) + len - expected_len;
	if (strcmp (tail, suffix))
		return false;

	subname = vStringNew();

	/* ??? dist, nodist, nobase, notrans,... */
	if (strncmp (vStringValue(name), "dist_", 5) == 0)
		vStringNCopyS(subname, vStringValue(name) + 5, len - expected_len - 5);
	else
		vStringNCopyS(subname, vStringValue(name), len - expected_len);

	if (rindex == ROLE_INDEX_DEFINITION)
	{
		*index = makeSimpleTag (subname, AutomakeKinds, kindex);
		addAutomakeDirectory (subname, *index);
	}
	else
	{
		*index = CORK_NIL;
		if (appending)
			*index = lookupAutomakeDirectory (subname);

		if ((!appending) || (*index == CORK_NIL))
			*index = makeSimpleRefTag (subname, AutomakeKinds, kindex, rindex);
	}

	vStringDelete (subname);
	return true;
}

static void valuesFoundAM (struct makeParserClient *client, vString *name, void *data)
{
	int p;
	tagEntryInfo *parent;
	int k;
	tagEntryInfo elt;

	p = *(int *)data;

	if (p == CORK_NIL)
		return;

	parent = getEntryInCorkQueue (p);
	if (((parent->kind - AutomakeKinds) == K_AM_DIR)
	    && (parent->extensionFields.roleIndex != ROLE_INDEX_DEFINITION))
	{
		k = K_AM_PROGRAM + parent->extensionFields.roleIndex;
		initTagEntry (&elt, vStringValue (name), AutomakeKinds + k);
		elt.extensionFields.scopeIndex = p;
		makeTagEntry (&elt);
	}
}

static void refCondtionAM (vString *directive)
{
	makeSimpleRefTag (directive, AutomakeKinds,
			  K_AM_CONDITION, R_AM_CONDITION_BRANCHED);
}

static int nextChar (void)
{
	int c = getcFromInputFile ();
	if (c == '\\')
	{
		c = getcFromInputFile ();
		if (c == '\n')
			c = nextChar ();
	}
	return c;
}

static int skipToNonWhite (int c)
{
	while (c != '\n' && isspace (c))
		c = nextChar ();
	return c;
}

static void directiveFoundAM (struct makeParserClient *client,
			      vString *directive, void *data)
{
	int c;
	if (! strcmp (vStringValue (directive), "if"))
	{
		vString *condition = vStringNew ();

		c = skipToNonWhite (nextChar ());
		while (c != EOF && c != '\n')
		{
			/* the operator for negation should not be
			   part of the condition name. */
			if (c != '!')
				vStringPut (condition, c);
			c = nextChar ();
		}
		if (c == '\n')
			ungetcToInputFile (c);
		vStringStripTrailing (condition);
		if (vStringLength (condition) > 0 )
			refCondtionAM (condition);
		vStringDelete (condition);
	}
}

static void newMacroAM (struct makeParserClient *client,
			vString *const name, bool with_define_directive,
			bool appending, void * data)
{
	*((int *)data)  = CORK_NIL;

	if (with_define_directive)
		return;

	(void)(0
	       || AutomakeMakeTag (name, "dir", appending,
				   K_AM_DIR, ROLE_INDEX_DEFINITION, am_blacklist,
				   data)
	       || AutomakeMakeTag (name, "_PROGRAMS", appending,
				   K_AM_DIR, R_AM_DIR_PROGRAMS, am_blacklist,
				   data)
	       || AutomakeMakeTag (name, "_MANS", appending,
				   K_AM_DIR, R_AM_DIR_MANS, am_blacklist,
				   data)
	       || AutomakeMakeTag (name, "_LTLIBRARIES", appending,
				   K_AM_DIR, R_AM_DIR_LTLIBRARIES, am_blacklist,
				   data)
	       || AutomakeMakeTag (name, "_LIBRARIES", appending,
				   K_AM_DIR, R_AM_DIR_LIBRARIES, am_blacklist,
				   data)
	       || AutomakeMakeTag (name, "_SCRIPTS", appending,
				   K_AM_DIR, R_AM_DIR_SCRIPTS, am_blacklist,
				   data)
	       || AutomakeMakeTag  (name, "_DATA", appending,
				    K_AM_DIR, R_AM_DIR_DATA, am_blacklist,
				    data)
		);
}

static void findAutomakeTags (void)
{
	int index = CORK_NIL;
	struct makeParserClient client = {
		.valuesFound    = valuesFoundAM,
		.directiveFound = directiveFoundAM,
		.newMacro = newMacroAM,
	};

	AutomakeDirectories = hashTableNew (11, hashCstrhash, hashCstreq, eFree, eFree);

	runMakeParser (&client, &index);

	hashTableDelete (AutomakeDirectories);
}

extern parserDefinition* AutomakeParser (void)
{
	static const char *const patterns [] = { "Makefile.am", NULL };
	parserDefinition* const def = parserNew ("Automake");

	def->kinds      = AutomakeKinds;
	def->kindCount  = ARRAY_SIZE (AutomakeKinds);
	def->patterns   = patterns;
	def->parser     = findAutomakeTags;
	def->useCork    = true;
	return def;
}
