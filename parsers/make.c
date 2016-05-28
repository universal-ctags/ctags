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

#include "htable.h"
#include "kind.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_MACRO, K_TARGET, K_INCLUDE,
	COUNT_KIND,
} makeKind;

typedef enum {
	R_INCLUDE_GENERIC,
	R_INCLUDE_OPTIONAL,
} makeMakefileRole;

static roleDesc MakeMakefileRoles [] = {
        { TRUE, "included", "included" },
	{ TRUE, "optional", "optionally included"},
};

static kindOption MakeKinds [] = {
	{ TRUE, 'm', "macro",  "macros"},
	{ TRUE, 't', "target", "targets"},
	{ TRUE, 'I', "makefile", "makefiles",
	  .referenceOnly = TRUE, ATTACH_ROLES(MakeMakefileRoles)},
};

typedef enum {
	AM_KIND_START = COUNT_KIND,
	K_AM_DIR = AM_KIND_START,
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
	{ TRUE, "program",   "directory for PROGRAMS primary" },
	{ TRUE, "man",       "directory for MANS primary" },
	{ TRUE, "ltlibrary", "directory for LTLIBRARIES primary"},
	{ TRUE, "library",   "directory for LIBRARIES primary"},
	{ TRUE, "script",    "directory for SCRIPTS primary"},
	{ TRUE, "data",      "directory for DATA primary"},
};

typedef enum {
	R_AM_CONDITION_BRANCHED,
} makeAMConditionRole;

static roleDesc AutomakeConditionRoles [] = {
	{ TRUE, "branched",  "used for branching" },
};

static scopeSeparator AutomakeSeparators [] = {
	{ 'd'          , "/" },
};

static kindOption AutomakeKinds [] = {
	[AM_KIND_START] =
	{ TRUE, 'd', "directory", "directories",
	  .referenceOnly = FALSE, ATTACH_ROLES(AutomakeDirectoryRoles)},
	{ TRUE, 'P', "program",   "programs",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ TRUE, 'M', "man",       "manuals",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ TRUE, 'T', "ltlibrary", "ltlibraries",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ TRUE, 'L', "library",   "libraries",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ TRUE, 'S', "script",    "scripts",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ TRUE, 'D', "data",      "datum",
	  ATTACH_SEPARATORS(AutomakeSeparators) },
	{ TRUE, 'c', "condition", "conditions",
	  .referenceOnly = TRUE, ATTACH_ROLES(AutomakeConditionRoles) },
};

static hashTable* AutomakeDirectories;

/*
*   FUNCTION DEFINITIONS
*/

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

static void skipLine (void)
{
	int c;
	do
		c = nextChar ();
	while (c != EOF  &&  c != '\n');
	if (c == '\n')
		ungetcToInputFile (c);
}

static int skipToNonWhite (int c)
{
	while (c != '\n' && isspace (c))
		c = nextChar ();
	return c;
}

static boolean isIdentifier (int c)
{
	return (boolean)(c != '\0' && (isalnum (c)  ||  strchr (".-_/$(){}%", c) != NULL));
}

static boolean isSpecialTarget (vString *const name)
{
	size_t i = 0;
	/* All special targets begin with '.'. */
	if (vStringLength (name) < 1 || vStringChar (name, i++) != '.') {
		return FALSE;
	}
	while (i < vStringLength (name)) {
		char ch = vStringChar (name, i++);
		if (ch != '_' && !isupper (ch))
		{
			return FALSE;
		}
	}
	return TRUE;
}

static void newTarget (vString *const name)
{
	/* Ignore GNU Make's "special targets". */
	if  (isSpecialTarget (name))
	{
		return;
	}
	makeSimpleTag (name, MakeKinds, K_TARGET);
}

static void (* valuesFoundCB) (vString *name, void *data);
static void (* directiveFoundCB) (vString *name, void *data);

static void (* newMacroCB) (vString *const name, boolean with_define_directive, boolean appending, void *data);
static void newMacro (vString *const name, boolean with_define_directive, boolean appending, void *data)
{
	if (!appending)
		makeSimpleTag (name, MakeKinds, K_MACRO);
	if (newMacroCB)
		newMacroCB (name, with_define_directive, appending, data);
}

static void newInclude (vString *const name, boolean optional)
{
	makeSimpleRefTag (name, MakeKinds, K_INCLUDE,
			  optional? R_INCLUDE_OPTIONAL: R_INCLUDE_GENERIC);
}

static boolean isAcceptableAsInclude (vString *const name)
{
	if (strcmp (vStringValue (name), "$") == 0)
		return FALSE;
	return TRUE;
}

static void readIdentifier (const int first, vString *const id)
{
	int depth = 0;
	int c = first;
	vStringClear (id);
	while (isIdentifier (c) || (depth > 0 && c != EOF && c != '\n'))
	{
		if (c == '(' || c == '{')
			depth++;
		else if (depth > 0 && (c == ')' || c == '}'))
			depth--;
		vStringPut (id, c);
		c = nextChar ();
	}
	ungetcToInputFile (c);
	vStringTerminate (id);
}

static void findMakeTagsCommon (void *data)
{
	stringList *identifiers = stringListNew ();
	boolean newline = TRUE;
	boolean in_define = FALSE;
	boolean in_value  = FALSE;
	boolean in_rule = FALSE;
	boolean variable_possible = TRUE;
	boolean appending = FALSE;
	int c;

	while ((c = nextChar ()) != EOF)
	{
		if (newline)
		{
			if (in_rule)
			{
				if (c == '\t' || (c = skipToNonWhite (c)) == '#')
				{
					skipLine ();  /* skip rule or comment */
					c = nextChar ();
				}
				else if (c != '\n')
					in_rule = FALSE;
			}
			else if (in_value)
				in_value = FALSE;

			stringListClear (identifiers);
			variable_possible = (boolean)(!in_rule);
			newline = FALSE;
		}
		if (c == '\n')
			newline = TRUE;
		else if (isspace (c))
			continue;
		else if (c == '#')
			skipLine ();
		else if (variable_possible && c == '?')
		{
			c = nextChar ();
			ungetcToInputFile (c);
			variable_possible = (c == '=');
		}
		else if (variable_possible && c == '+')
		{
			c = nextChar ();
			ungetcToInputFile (c);
			variable_possible = (c == '=');
			appending = TRUE;
		}
		else if (variable_possible && c == ':' &&
				 stringListCount (identifiers) > 0)
		{
			c = nextChar ();
			ungetcToInputFile (c);
			if (c != '=')
			{
				unsigned int i;
				for (i = 0; i < stringListCount (identifiers); i++)
					newTarget (stringListItem (identifiers, i));
				stringListClear (identifiers);
				in_rule = TRUE;
			}
		}
		else if (variable_possible && c == '=' &&
				 stringListCount (identifiers) == 1)
		{
			newMacro (stringListItem (identifiers, 0), FALSE, appending, data);
			in_value = TRUE;
			in_rule = FALSE;
			appending = FALSE;
		}
		else if (variable_possible && isIdentifier (c))
		{
			vString *name = vStringNew ();
			readIdentifier (c, name);
			stringListAdd (identifiers, name);

			if (in_value && valuesFoundCB)
				valuesFoundCB (name, data);

			if (stringListCount (identifiers) == 1)
			{
				if (in_define && ! strcmp (vStringValue (name), "endef"))
					in_define = FALSE;
				else if (in_define)
					skipLine ();
				else if (! strcmp (vStringValue (name), "define"))
				{
					in_define = TRUE;
					c = skipToNonWhite (nextChar ());
					vStringClear (name);
					/* all remaining characters on the line are the name -- even spaces */
					while (c != EOF && c != '\n')
					{
						vStringPut (name, c);
						c = nextChar ();
					}
					if (c == '\n')
						ungetcToInputFile (c);
					vStringTerminate (name);
					vStringStripTrailing (name);
					newMacro (name, TRUE, FALSE, data);
				}
				else if (! strcmp (vStringValue (name), "export"))
					stringListClear (identifiers);
				else if (! strcmp (vStringValue (name), "include")
					 || ! strcmp (vStringValue (name), "sinclude")
					 || ! strcmp (vStringValue (name), "-include"))
				{
					boolean optional = (vStringValue (name)[0] == 'i')? FALSE: TRUE;
					while (1)
					{
						c = skipToNonWhite (nextChar ());
						readIdentifier (c, name);
						vStringStripTrailing (name);
						if (isAcceptableAsInclude(name))
							newInclude (name, optional);

						/* non-space characters after readIdentifier() may
						 * be rejected by the function:
						 * e.g.
						 * include $*
						 *
						 * Here, remove such characters from input stream.
						 */
						do
							c = nextChar ();
						while (c != EOF && c != '\n' && (!isspace (c)));
						if (c == '\n')
							ungetcToInputFile (c);

						if (c == EOF || c == '\n')
							break;
					}
				}
				else
				{
					if (directiveFoundCB)
						directiveFoundCB (name, data);

				}
			}
		}
		else
			variable_possible = FALSE;
	}
	stringListDelete (identifiers);
}

static void findMakeTags (void)
{
	findMakeTagsCommon (NULL);
}

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


static boolean bl_check (const char *name, struct sBlacklist *blacklist)
{
	if ((blacklist->type == BL_PREFIX) &&
	    (strncmp (blacklist->substr, name, blacklist->len) == 0))
		return FALSE;
	else
		return TRUE;
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

static boolean AutomakeMakeTag (vString *const name, const char* suffix, boolean appending,
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
		return FALSE;

	for (i = 0; blacklist[i].type != BL_END; i++)
	{
		if (bl_check (vStringValue(name), blacklist + i) == FALSE)
			return FALSE;
	}

	tail = vStringValue (name) + len - expected_len;
	if (strcmp (tail, suffix))
		return FALSE;

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
	return TRUE;
}

static void newMacroAM (vString *const name, boolean with_define_directive,
			boolean appending, void * data)
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

static void valuesFoundAM (vString *name, void *data)
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

static void directiveFoundAM (vString *directive, void *data)
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
		vStringTerminate (condition);
		vStringStripTrailing (condition);
		if (vStringLength (condition) > 0 )
			refCondtionAM (condition);
		vStringDelete (condition);
	}
}

static void findAutomakeTags (void)
{
	int index = CORK_NIL;
	void *backup_newMacro = newMacroCB;
	void *backup_valuesFound = valuesFoundCB;
	void *backup_directiveFound = directiveFoundCB;

	AutomakeDirectories = hashTableNew (11, hashCstrhash, hashCstreq, eFree, eFree);
	newMacroCB = newMacroAM;
	valuesFoundCB = valuesFoundAM;
	directiveFoundCB = directiveFoundAM;
	findMakeTagsCommon (&index);
	valuesFoundCB = backup_valuesFound;
	newMacroCB = backup_newMacro;
	directiveFoundCB = backup_directiveFound;

	hashTableDelete (AutomakeDirectories);
}

extern parserDefinition* MakefileParser (void)
{
	static const char *const patterns [] = { "[Mm]akefile", "GNUmakefile", NULL };
	static const char *const extensions [] = { "mak", "mk", NULL };
	parserDefinition* const def = parserNew ("Make");
	def->kinds      = MakeKinds;
	def->kindCount  = ARRAY_SIZE (MakeKinds);
	def->patterns   = patterns;
	def->extensions = extensions;
	def->parser     = findMakeTags;
	return def;
}

extern parserDefinition* AutomakeParser (void)
{
	int i;
	static const char *const patterns [] = { "Makefile.am", NULL };
	parserDefinition* const def = parserNew ("Automake");

	for (i = 0; i < AM_KIND_START; i++)
		AutomakeKinds [i] = MakeKinds [i];

	def->kinds      = AutomakeKinds;
	def->kindCount  = ARRAY_SIZE (AutomakeKinds);
	def->patterns   = patterns;
	def->parser     = findAutomakeTags;
	def->useCork    = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
