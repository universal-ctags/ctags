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

#include "make.h"

#include "entry.h"
#include "htable.h"
#include "kind.h"
#include "parse.h"
#include "read.h"
#include "subparser.h"


typedef enum {
	K_AM_DIR,
	K_AM_PROGRAM,
	K_AM_MAN,
	K_AM_LTLIBRARY,
	K_AM_LIBRARY,
	K_AM_SCRIPT,
	K_AM_DATA,
	K_AM_CONDITION,
	K_AM_SUBDIR,
} makeAMKind;

typedef enum {
	R_AM_DIR_PROGRAMS,
	R_AM_DIR_MANS,
	R_AM_DIR_LTLIBRARIES,
	R_AM_DIR_LIBRARIES,
	R_AM_DIR_SCRIPTS,
	R_AM_DIR_DATA,
} makeAMDirectoryRole;

static roleDefinition AutomakeDirectoryRoles [] = {
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

static roleDefinition AutomakeConditionRoles [] = {
	{ true, "branched",  "used for branching" },
};

static scopeSeparator AutomakeSeparators [] = {
	{ K_AM_DIR        , "/" },
};

static kindDefinition AutomakeKinds [] = {
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
	{ true, 's', "subdir", "subdirs" },
};

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

struct sAutomakeSubparser {
	makeSubparser make;

	hashTable* directories;
	int index;
	bool in_subdirs;
};


static bool bl_check (const char *name, struct sBlacklist *blacklist)
{
	if ((blacklist->type == BL_PREFIX) &&
	    (strncmp (blacklist->substr, name, blacklist->len) == 0))
		return false;
	else
		return true;
}

static int lookupAutomakeDirectory (hashTable* directories,  vString *const name)
{
	int *i = hashTableGetItem (directories,  vStringValue (name));

	if (i)
		return *i;
	else
		return CORK_NIL;
}

static void addAutomakeDirectory (hashTable* directories, vString *const name, int corkIndex)
{
	char * k = vStringStrdup (name);
	int  * i = xMalloc (1, int);

	*i = corkIndex;

	hashTablePutItem (directories, k, i);
}

static bool automakeMakeTag (struct sAutomakeSubparser* automake,
							 char* name, const char* suffix, bool appending,
							 int kindex, int rindex, struct sBlacklist *blacklist)
{
	size_t expected_len;
	size_t len;
	char* tail;
	vString *subname;
	int i;

	len = strlen (name);
	expected_len = strlen (suffix);

	if (len <= expected_len)
		return false;

	for (i = 0; blacklist[i].type != BL_END; i++)
	{
		if (bl_check (name, blacklist + i) == false)
			return false;
	}

	tail = name + len - expected_len;
	if (strcmp (tail, suffix))
		return false;

	subname = vStringNew();

	/* ??? dist, nodist, nobase, notrans,... */
	if (strncmp (name, "dist_", 5) == 0)
		vStringNCopyS(subname, name + 5, len - expected_len - 5);
	else
		vStringNCopyS(subname, name, len - expected_len);

	if (rindex == ROLE_DEFINITION_INDEX)
	{
		automake->index = makeSimpleTag (subname, kindex);
		addAutomakeDirectory (automake->directories, subname, automake->index);
	}
	else
	{
		automake->index = CORK_NIL;
		if (appending)
			automake->index = lookupAutomakeDirectory (automake->directories, subname);

		if ((!appending) || (automake->index == CORK_NIL))
			automake->index = makeSimpleRefTag (subname, kindex, rindex);
	}

	vStringDelete (subname);
	return true;
}

static void valueCallback (makeSubparser *make, char *name)
{
	struct sAutomakeSubparser *automake = (struct sAutomakeSubparser *)make;
	int p = automake->index;
	tagEntryInfo *parent;
	int k;
	tagEntryInfo elt;

	parent = getEntryInCorkQueue (p);
	if (parent && (parent->kindIndex == K_AM_DIR)
	    && (parent->extensionFields.roleBits))
	{
		int roleIndex;
		for (roleIndex = 0; roleIndex < ARRAY_SIZE(AutomakeDirectoryRoles); roleIndex++)
			if (parent->extensionFields.roleBits & ((roleBitsType)1) << roleIndex)
				break;

		k = K_AM_PROGRAM + roleIndex;
		initTagEntry (&elt, name, k);
		elt.extensionFields.scopeIndex = p;
		makeTagEntry (&elt);
	}
	else if (automake->in_subdirs)
	{
		initTagEntry (&elt, name, K_AM_SUBDIR);
		makeTagEntry (&elt);
	}
}

static void refCondtionAM (vString *directive)
{
	makeSimpleRefTag (directive,
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

static void directiveCallback (makeSubparser *make CTAGS_ATTR_UNUSED, char *directive)
{
	int c;
	if (! strcmp (directive, "if"))
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

static void newMacroCallback (makeSubparser *make, char* name, bool with_define_directive,
							  bool appending)
{
	struct sAutomakeSubparser *automake = (struct sAutomakeSubparser *)make;

	automake->index = CORK_NIL;
	automake->in_subdirs = false;

	if (with_define_directive)
		return;

	if (strcmp (name, "SUBDIRS") == 0)
	{
		automake->in_subdirs = true;
		return;
	}

	(void)(0
	       || automakeMakeTag (automake,
							   name, "dir", appending,
							   K_AM_DIR, ROLE_DEFINITION_INDEX, am_blacklist)
	       || automakeMakeTag (automake,
							   name, "_PROGRAMS", appending,
							   K_AM_DIR, R_AM_DIR_PROGRAMS, am_blacklist)
	       || automakeMakeTag (automake,
							   name, "_MANS", appending,
							   K_AM_DIR, R_AM_DIR_MANS, am_blacklist)
	       || automakeMakeTag (automake,
							   name, "_LTLIBRARIES", appending,
							   K_AM_DIR, R_AM_DIR_LTLIBRARIES, am_blacklist)
	       || automakeMakeTag (automake,
							   name, "_LIBRARIES", appending,
							   K_AM_DIR, R_AM_DIR_LIBRARIES, am_blacklist)
	       || automakeMakeTag (automake,
							   name, "_SCRIPTS", appending,
							   K_AM_DIR, R_AM_DIR_SCRIPTS, am_blacklist)
	       || automakeMakeTag  (automake,
								name, "_DATA", appending,
								K_AM_DIR, R_AM_DIR_DATA, am_blacklist)
		);
}

static void inputStart (subparser *s)
{
	struct sAutomakeSubparser *automake = (struct sAutomakeSubparser*)s;

	automake->directories = hashTableNew (11, hashCstrhash, hashCstreq, eFree, eFree);
	automake->index = CORK_NIL;
	automake->in_subdirs = false;
}

static void inputEnd (subparser *s)
{
	struct sAutomakeSubparser *automake = (struct sAutomakeSubparser*)s;

	hashTableDelete (automake->directories);
	automake->directories = NULL;
}

static void findAutomakeTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* AutomakeParser (void)
{
	static const char *const extensions [] = { "am", NULL };
	static const char *const patterns [] = { "Makefile.am", NULL };
	static struct sAutomakeSubparser automakeSubparser = {
		.make = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
				.inputStart = inputStart,
				.inputEnd = inputEnd,
			},
			.valueNotify = valueCallback,
			.directiveNotify = directiveCallback,
			.newMacroNotify = newMacroCallback,
		},
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Make", &automakeSubparser },
	};

	parserDefinition* const def = parserNew ("Automake");


	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable      = AutomakeKinds;
	def->kindCount  = ARRAY_SIZE (AutomakeKinds);
	def->extensions = extensions;
	def->patterns   = patterns;
	def->parser     = findAutomakeTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
