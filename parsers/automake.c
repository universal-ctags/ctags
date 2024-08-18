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

#include "x-make.h"

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
	K_AM_PSEUDODIR,
} makeAMKind;

typedef enum {
	R_AM_DIR_PROGRAMS,
	R_AM_DIR_MANS,
	R_AM_DIR_LTLIBRARIES,
	R_AM_DIR_LIBRARIES,
	R_AM_DIR_SCRIPTS,
	R_AM_DIR_DATA,
} makeAMDirectoryRole;

#define DIR_ROLES \
	{ true, "program",   "directory for PROGRAMS primary" },	\
	{ true, "man",       "directory for MANS primary" },		\
	{ true, "ltlibrary", "directory for LTLIBRARIES primary"},	\
	{ true, "library",   "directory for LIBRARIES primary"},	\
	{ true, "script",    "directory for SCRIPTS primary"},		\
	{ true, "data",      "directory for DATA primary"},

static roleDefinition AutomakeDirectoryRoles [] = {
	DIR_ROLES
};

static roleDefinition AutomakePseudodirRoles [] = {
	DIR_ROLES
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
	{ false,'p', "pseudodir", "placeholder for EXTRA_, noinst_, and _check_ prefixed primaries (internal use)",
	  .referenceOnly = true, ATTACH_ROLES(AutomakePseudodirRoles)},
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

struct sPrefix {
	const char* substr;
	size_t len;
};

struct sAutomakeSubparser {
	makeSubparser make;

	hashTable* directories;
	int index;
	bool in_subdirs;
};


static bool has_prefix0 (const char *name, const struct sPrefix *prefix)
{
	if (strncmp (prefix->substr, name, prefix->len) == 0)
		return false;
	else
		return true;
}

static size_t has_prefix (const char *name, const struct sPrefix *prefixlist)
{
	for (int i = 0; prefixlist[i].substr; i++)
	{
		if (has_prefix0 (name, prefixlist + i) == false)
			return prefixlist [i].len;
	}
	return 0;
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
	static const struct sPrefix obj_prefixlist [] = {
		{ "dist_",    5 },
		{ "nodist_",  7 },
		{ "nobase_",  7 },
		{ "notrans_", 8 },
		{ NULL,       0 },
	};
	while ((prefix_len = has_prefix(name, obj_prefixlist)))
		name += prefix_len;
	/* => data_DATA */

	return name;
}

static bool automakeMakeTag (struct sAutomakeSubparser* automake,
							 const char* name, size_t len,
							 const char* suffix, size_t suffix_len,
							 bool appending,
							 int kindex, int rindex)
{
	const char* tail;
	vString *subname;

	suffix_len = strlen (suffix);
	if (len < suffix_len)
		return false;

	tail = name + len - suffix_len;
	if (strcmp (tail, suffix))
		return false;

	subname = vStringNewNInit(name, len - suffix_len);

	if (rindex == ROLE_DEFINITION_INDEX)
	{
		automake->index = makeSimpleTag (subname, kindex);
		addAutomakeDirectory (automake->directories, subname, automake->index);
	}
	else
	{
		bool pseudodir = false;
		if (strcmp(vStringValue (subname), "EXTRA") == 0
			|| strcmp(vStringValue (subname), "noinst") == 0
			|| strcmp(vStringValue (subname), "check") == 0)
		{
			pseudodir = true;
			if (kindex == K_AM_DIR)
				kindex = K_AM_PSEUDODIR;
		}

		automake->index = CORK_NIL;
		if (appending || pseudodir)
			automake->index = lookupAutomakeDirectory (automake->directories, subname);

		if ((!appending) || (automake->index == CORK_NIL))
		{
			automake->index = makeSimpleRefTag (subname, kindex, rindex);
			if ((automake->index != CORK_NIL) && pseudodir)
				addAutomakeDirectory (automake->directories, subname, automake->index);
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
	if (parent && (parent->kindIndex == K_AM_DIR || parent->kindIndex == K_AM_PSEUDODIR)
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
							  bool appending, int scopeIndex CTAGS_ATTR_UNUSED)
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

	size_t len = strlen (name);
	if (automakeMakeTag (automake,
						 name, len, "dir", 3, appending,
						 K_AM_DIR, ROLE_DEFINITION_INDEX))
		return;

	const char *trimmed_name = skipPrefix (name);
	if (trimmed_name != name)
		len = strlen (trimmed_name);

#define S(X) X, strlen(X)
	(void)(0
	       || automakeMakeTag (automake,
							   trimmed_name, len, S("_PROGRAMS"),
							   appending, K_AM_DIR, R_AM_DIR_PROGRAMS)
	       || automakeMakeTag (automake,
							   trimmed_name, len, S("_MANS"),
							   appending, K_AM_DIR, R_AM_DIR_MANS)
	       || automakeMakeTag (automake,
							   trimmed_name, len, S("_LTLIBRARIES"), appending,
							   K_AM_DIR, R_AM_DIR_LTLIBRARIES)
	       || automakeMakeTag (automake,
							   trimmed_name, len, S("_LIBRARIES"), appending,
							   K_AM_DIR, R_AM_DIR_LIBRARIES)
	       || automakeMakeTag (automake,
							   trimmed_name, len, S("_SCRIPTS"), appending,
							   K_AM_DIR, R_AM_DIR_SCRIPTS)
	       || automakeMakeTag  (automake,
								trimmed_name, len, S("_DATA"), appending,
								K_AM_DIR, R_AM_DIR_DATA)
		);
#undef S
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
	static const char *const aliases [] = { "makefile-automake", NULL };

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
	def->aliases    = aliases;
	def->parser     = findAutomakeTags;
	def->xtagTable = AutomakeXtagTable;
	def->xtagCount = ARRAY_SIZE (AutomakeXtagTable);
	def->useCork    = CORK_QUEUE;
	def->versionCurrent = 1;
	def->versionAge = 1;
	return def;
}
