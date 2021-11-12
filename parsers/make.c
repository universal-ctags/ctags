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
#include "kind.h"
#include "numarray.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"
#include "vstring.h"
#include "xtag.h"


/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_MACRO, K_TARGET, K_INCLUDE,
} makeKind;

typedef enum {
	R_INCLUDE_GENERIC,
	R_INCLUDE_OPTIONAL,
} makeMakefileRole;

static roleDefinition MakeMakefileRoles [] = {
	{ true, "included", "included" },
	{ true, "optional", "optionally included"},
};

static kindDefinition MakeKinds [] = {
	{ true, 'm', "macro",  "macros"},
	{ true, 't', "target", "targets"},
	{ true, 'I', "makefile", "makefiles",
	  .referenceOnly = true, ATTACH_ROLES(MakeMakefileRoles)},
};


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

static bool isIdentifier (int c)
{
	return (bool)(c != '\0' && (isalnum (c)  ||  strchr (".-_/$(){}%", c) != NULL));
}

static bool isSpecialTarget (vString *const name)
{
	size_t i = 0;
	/* All special targets begin with '.'. */
	if (vStringLength (name) < 1 || vStringChar (name, i++) != '.') {
		return false;
	}
	while (i < vStringLength (name)) {
		char ch = vStringChar (name, i++);
		if (ch != '_' && !isupper (ch))
		{
			return false;
		}
	}
	return true;
}

static int makeSimpleMakeTag (vString *const name, makeKind kind)
{
	if (!isLanguageEnabled (getInputLanguage ()))
		return CORK_NIL;

	return makeSimpleTag (name, kind);
}

static void makeSimpleMakeRefTag (const vString* const name, const int kind,
				  int roleIndex)
{
	if (!isLanguageEnabled (getInputLanguage ()))
		return;

	makeSimpleRefTag (name, kind, roleIndex);
}

static int newTarget (vString *const name)
{
	/* Ignore GNU Make's "special targets". */
	if  (isSpecialTarget (name))
	{
		return CORK_NIL;
	}
	return makeSimpleMakeTag (name, K_TARGET);
}

static int newMacro (vString *const name, bool with_define_directive, bool appending)
{
	int r = CORK_NIL;
	subparser *s;

	if (!appending)
		r = makeSimpleMakeTag (name, K_MACRO);

	foreachSubparser(s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->newMacroNotify)
			m->newMacroNotify (m, vStringValue(name), with_define_directive, appending);
		leaveSubparser();
	}

	return r;
}

static void valueFound (vString *const name)
{
	subparser *s;
	foreachSubparser(s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->valueNotify)
			m->valueNotify (m, vStringValue (name));
		leaveSubparser();
	}
}

static void directiveFound (vString *const name)
{
	subparser *s;
	foreachSubparser (s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->directiveNotify)
			m->directiveNotify (m, vStringValue (name));
		leaveSubparser();
	}
}

static void newInclude (vString *const name, bool optional)
{
	makeSimpleMakeRefTag (name, K_INCLUDE,
			      optional? R_INCLUDE_OPTIONAL: R_INCLUDE_GENERIC);
}

static bool isAcceptableAsInclude (vString *const name)
{
	if (strcmp (vStringValue (name), "$") == 0)
		return false;
	return true;
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
}

static void endTargets (intArray *targets, unsigned long lnum)
{
	for (unsigned int i = 0; i < intArrayCount (targets); i++)
	{
		int cork_index = intArrayItem (targets, i);
		tagEntryInfo *e = getEntryInCorkQueue (cork_index);
		if (e)
			e->extensionFields.endLine = lnum;
	}
	intArrayClear (targets);
}

static void findMakeTags (void)
{
	stringList *identifiers = stringListNew ();
	bool newline = true;
	int  current_macro = CORK_NIL;
	bool in_value  = false;
	intArray *current_targets = intArrayNew ();
	bool variable_possible = true;
	bool appending = false;
	int c;
	subparser *sub;

	sub = getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser (sub, NULL);

	while ((c = nextChar ()) != EOF)
	{
		if (newline)
		{
			if (!intArrayIsEmpty (current_targets))
			{
				if (c == '\t' || (c = skipToNonWhite (c)) == '#')
				{
					skipLine ();  /* skip rule or comment */
					c = nextChar ();
				}
				else if (c != '\n')
					endTargets (current_targets, getInputLineNumber () - 1);
			}
			else if (in_value)
				in_value = false;

			stringListClear (identifiers);
			variable_possible = intArrayIsEmpty (current_targets);
			newline = false;
		}
		if (c == '\n')
			newline = true;
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
			appending = true;
		}
		else if ((! in_value) && variable_possible && c == ':' &&
				 stringListCount (identifiers) > 0)
		{
			c = nextChar ();
			ungetcToInputFile (c);
			if (c != '=')
			{
				unsigned int i;
				for (i = 0; i < stringListCount (identifiers); i++)
				{
					int r = newTarget (stringListItem (identifiers, i));
					if (r != CORK_NIL)
						intArrayAdd (current_targets, r);
				}
				stringListClear (identifiers);
			}
		}
		else if (variable_possible && c == '=' &&
				 stringListCount (identifiers) == 1)
		{
			newMacro (stringListItem (identifiers, 0), false, appending);

			in_value = true;
			endTargets (current_targets, getInputLineNumber () - 1);
			appending = false;
		}
		else if (variable_possible && isIdentifier (c))
		{
			vString *name = vStringNew ();
			readIdentifier (c, name);
			stringListAdd (identifiers, name);

			if (in_value)
				valueFound(name);

			if (stringListCount (identifiers) == 1)
			{
				if ((current_macro != CORK_NIL) && ! strcmp (vStringValue (name), "endef"))
				{
					tagEntryInfo *e = getEntryInCorkQueue(current_macro);

					current_macro = CORK_NIL;
					if (e)
						e->extensionFields.endLine = getInputLineNumber ();
				}
				else if (current_macro != CORK_NIL)
					skipLine ();
				else if (! strcmp (vStringValue (name), "define"))
				{
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
					vStringStripTrailing (name);

					current_macro = newMacro (name, true, false);
				}
				else if (! strcmp (vStringValue (name), "export"))
					stringListClear (identifiers);
				else if (! strcmp (vStringValue (name), "include")
					 || ! strcmp (vStringValue (name), "sinclude")
					 || ! strcmp (vStringValue (name), "-include"))
				{
					bool optional = (vStringValue (name)[0] == 'i')? false: true;
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
					directiveFound (name);
			}
		}
		else
			variable_possible = false;
	}

	endTargets (current_targets, getInputLineNumber ());

	intArrayDelete (current_targets);
	stringListDelete (identifiers);
}


extern parserDefinition* MakefileParser (void)
{
	static const char *const patterns [] = { "[Mm]akefile", "GNUmakefile", NULL };
	static const char *const extensions [] = { "mak", "mk", NULL };
	static const char *const aliases [] = {
		/* the mode name in emacs */
		"makefile",
		NULL };
	parserDefinition* const def = parserNew ("Make");
	def->kindTable      = MakeKinds;
	def->kindCount  = ARRAY_SIZE (MakeKinds);
	def->patterns   = patterns;
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findMakeTags;
	def->useCork = CORK_QUEUE;
	return def;
}
