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

typedef enum {
	X_CANONICALIZED_NAME,
} automakeXtag;

static xtagDefinition AutomakeXtagTable [] = {
	{
		.enabled = true,
		.name = "canonicalizedName",
		.description = "Include canonicalized object name like libctags_a",
	},
};

struct sBlacklist {
	enum { BL_END, BL_PREFIX } type;
	const char* substr;
	size_t len;
};

struct sAutomakeSubparser {
	makeSubparser make;

	hashTable* directories;
	int index;
	bool in_subdirs;
};


static bool bl_check0 (const char *name, const struct sBlacklist *blacklist)
{
	if ((blacklist->type == BL_PREFIX) &&
	    (strncmp (blacklist->substr, name, blacklist->len) == 0))
		return false;
	else
		return true;
}

static bool bl_check (const char *name, const struct sBlacklist *blacklist, size_t *prefix_len)
{
	for (int i = 0; blacklist[i].type != BL_END; i++)
	{
		if (bl_check0 (name, blacklist + i) == false)
		{
			if (prefix_len)
				*prefix_len = blacklist [i].len;
			return false;
		}
	}
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

static const char *skipPrefix(const char *name)
{
	size_t prefix_len;

	/* Drop "dist_" in "dist_data_DATA" */
	const static struct sBlacklist obj_blacklist [] = {
		{ BL_PREFIX, "dist_",    5 },
		{ BL_PREFIX, "nodist_",  7 },
		{ BL_PREFIX, "nobase_",  7 },
		{ BL_PREFIX, "notrans_", 8 },
		{ BL_END,    NULL,       0 },
	};
	while (bl_check(name, obj_blacklist, &prefix_len) == false)
		name += prefix_len;
	/* => data_DATA */

	/* Drop  "check" in "check_PROGRAM" */
	const static struct sBlacklist dir_blacklist [] = {
		{ BL_PREFIX, "EXTRA_",  6 },
		{ BL_PREFIX, "noinst_", 7 },
		{ BL_PREFIX, "check_",  6 },
		{ BL_END,    NULL,     0 },
	};
	if (bl_check(name, dir_blacklist, &prefix_len) == false)
		name += (prefix_len - 1);
	/* keep the initial `_' */
	/* => "_PROGRAM" */

	return name;
}

static bool automakeMakeTag (struct sAutomakeSubparser* automake,
							 const char* name, const char* suffix, bool appending,
							 int kindex, int rindex)
{
	size_t expected_len;
	size_t len;
	const char* tail;
	vString *subname;

	name = skipPrefix(name);

	len = strlen (name);
	expected_len = strlen (suffix);
	if (len < expected_len)
		return false;

	tail = name + len - expected_len;
	if (strcmp (tail, suffix))
		return false;

	subname = vStringNewNInit(name, len - expected_len);

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
		{
			bool placeholder = false;
			if (vStringIsEmpty (subname))
			{
				vStringCatS(subname, "DUMMY");
				placeholder = true;
			}
			automake->index = makeSimpleRefTag (subname, kindex, rindex);
			if (placeholder)
			{
				tagEntryInfo *e = getEntryInCorkQueue (automake->index);
				if (e)
					e->placeholder = 1;
			}
		}
	}

	vStringDelete (subname);
	return true;
}

static void canonicalizeName (vString *dst, const char *name)
{
	while (*name)
	{
		if (isalnum ((unsigned char) *name) ||
			*name == '_' || *name == '@')
			vStringPut (dst, *name);
		else
			vStringPut (dst, '_');
		name++;
	}
}

static void makeCanonicalizedTag (tagEntryInfo *e, const char *name)
{
	vString *xname = vStringNew ();

	canonicalizeName (xname, name);
	e->name = vStringValue (xname);
	if (strcmp(e->name, name))
	{
		makeTagEntry (e);
		markTagExtraBit (e, AutomakeXtagTable[X_CANONICALIZED_NAME].xtype);
	}
	vStringDelete (xname);
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
		if (!parent->placeholder)
			elt.extensionFields.scopeIndex = p;
		makeTagEntry (&elt);

		if (isXtagEnabled (AutomakeXtagTable[X_CANONICALIZED_NAME].xtype)
			&& (k == K_AM_PROGRAM || k == K_AM_LTLIBRARY || k == K_AM_LIBRARY))
			makeCanonicalizedTag (&elt, name);
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
							   K_AM_DIR, ROLE_DEFINITION_INDEX)
	       || automakeMakeTag (automake,
							   name, "_PROGRAMS", appending,
							   K_AM_DIR, R_AM_DIR_PROGRAMS)
	       || automakeMakeTag (automake,
							   name, "_MANS", appending,
							   K_AM_DIR, R_AM_DIR_MANS)
	       || automakeMakeTag (automake,
							   name, "_LTLIBRARIES", appending,
							   K_AM_DIR, R_AM_DIR_LTLIBRARIES)
	       || automakeMakeTag (automake,
							   name, "_LIBRARIES", appending,
							   K_AM_DIR, R_AM_DIR_LIBRARIES)
	       || automakeMakeTag (automake,
							   name, "_SCRIPTS", appending,
							   K_AM_DIR, R_AM_DIR_SCRIPTS)
	       || automakeMakeTag  (automake,
								name, "_DATA", appending,
								K_AM_DIR, R_AM_DIR_DATA)
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
	static const char *const patterns [] = { "Makefile.am", "GNUmakefile.am", NULL };
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
	def->xtagTable = AutomakeXtagTable;
	def->xtagCount = ARRAY_SIZE (AutomakeXtagTable);
	def->useCork    = CORK_QUEUE;
	def->versionCurrent = 1;
	def->versionAge = 1;
	return def;
}
