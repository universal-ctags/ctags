/*
*   Copyright (c) 2000-2005, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for makefiles.
*
*   References:
*   - https://www.gnu.org/software/make/manual/html_node/index.html
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
		if (ch != '_' && !isupper ((unsigned char) ch))
		{
			return false;
		}
	}
	return true;
}

static int makeSimpleMakeTag (vString *const name, makeKind kind, int scopeIndex)
{
	if (!isLanguageEnabled (getInputLanguage ()))
		return CORK_NIL;

	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kind);
	e.extensionFields.scopeIndex = scopeIndex;
	return makeTagEntry (&e);
}

static void makeSimpleMakeRefTag (const vString* const name, const int kind,
				  int roleIndex, int scopeIndex)
{
	if (!isLanguageEnabled (getInputLanguage ()))
		return;

	tagEntryInfo e;
	initRefTagEntry (&e, vStringValue (name), kind, roleIndex);
	e.extensionFields.scopeIndex = scopeIndex;
	makeTagEntry (&e);
}

static int newTarget (vString *const name, int scopeIndex)
{
	/* Ignore GNU Make's "special targets". */
	if  (isSpecialTarget (name))
	{
		return CORK_NIL;
	}
	return makeSimpleMakeTag (name, K_TARGET, scopeIndex);
}

static int newMacro (vString *const name, bool with_define_directive, bool appending, int scopeIndex)
{
	int r = CORK_NIL;
	subparser *s;

	if (!appending)
		r = makeSimpleMakeTag (name, K_MACRO, scopeIndex);

	foreachSubparser(s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->newMacroNotify)
			m->newMacroNotify (m, vStringValue(name), with_define_directive, appending, scopeIndex);
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

static void newInclude (vString *const name, bool optional, int scopeIndex)
{
	makeSimpleMakeRefTag (name, K_INCLUDE,
			      optional? R_INCLUDE_OPTIONAL: R_INCLUDE_GENERIC, scopeIndex);
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
		setTagEndLineToCorkEntry (cork_index, lnum);
	}
	intArrayClear (targets);
}

static bool isTheLastTargetOnTheSameLine (intArray *current_targets,
										  unsigned long line)
{
	if (!intArrayIsEmpty (current_targets))
	{
		int r = intArrayLast (current_targets);
		tagEntryInfo *e = getEntryInCorkQueue (r);
		if (e && e->lineNumber == line)
			return true;
	}

	return false;
}

/*
 * Naming
 *
 *   X = ...
 *
 * We refer to X as a macro or a variable macro (varmac) here.
 *
 *   define M
 *    ...
 *   endef
 *
 * We refer to M as a macro or a macro definition (macdef) here.
 */
static void findMakeTags0 (void)
{
	/* MAINLY for tracking the candidates of MULTIPLE targets like:
	 *
	 * clean distclean install:
	 *     ...
	 */
	stringList *identifiers = stringListNew ();

	/* parsing the right side of =, :=, +=, ?=, !=
	 *
	 * Nothing to do with targets or macdefs.
	 */
	bool in_varmac_value  = false;

	/* For tracking a tag for a macdef. */
	int  current_macdef = CORK_NIL;

	/* For tracking tags for (multiple) targets.
	 */
	intArray *current_targets = intArrayNew ();

	/* Reading char can be a part of macro name.
	 * In the other words, we are "not in a recipe". */
	bool macro_possible = true;

	/* A char just read*/
	int c;

	/* '\n' seen. */
	bool newline = true;

	/* += seen. */
	bool appending = false;

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
			else if (in_varmac_value)
				in_varmac_value = false;

			stringListClear (identifiers);
			macro_possible = intArrayIsEmpty (current_targets);
			newline = false;
		}
		if (c == '\n')
			newline = true;
		else if (isspace (c))
			continue;
		else if (c == '#')
			skipLine ();
		else if (macro_possible && (c == '?' || c == '!'))
		{
			c = nextChar ();
			ungetcToInputFile (c);
			macro_possible = (c == '=');
		}
		else if (macro_possible && c == '+')
		{
			c = nextChar ();
			ungetcToInputFile (c);
			macro_possible = (c == '=');
			appending = true;
		}
		else if ((! in_varmac_value) && macro_possible && c == ':' &&
				 stringListCount (identifiers) > 0)
		{
			c = nextChar ();
			ungetcToInputFile (c);
			if (c != '=')
			{
				unsigned int i;
				for (i = 0; i < stringListCount (identifiers); i++)
				{
					int r = newTarget (stringListItem (identifiers, i), current_macdef);
					if (r != CORK_NIL)
						intArrayAdd (current_targets, r);
				}
				stringListClear (identifiers);
			}
		}
		else if (macro_possible && c == '=' &&
				 stringListCount (identifiers) > 0
				 && !in_varmac_value)
		{
			newMacro (stringListItem (identifiers, 0), false, appending, current_macdef);
			stringListClear (identifiers);

			in_varmac_value = true;
			unsigned long curline = getInputLineNumber ();
			unsigned long adj = isTheLastTargetOnTheSameLine (current_targets,
															  curline)? 0: 1;
			endTargets (current_targets, curline - adj);
			appending = false;
		}
		else if (macro_possible && isIdentifier (c))
		{
			vString *name = vStringNew ();
			readIdentifier (c, name);
			stringListAdd (identifiers, name);

			if (in_varmac_value)
				valueFound(name);

			if (stringListCount (identifiers) == 1)
			{
				if ((current_macdef != CORK_NIL) && ! strcmp (vStringValue (name), "endef"))
				{
					setTagEndLineToCorkEntry (current_macdef, getInputLineNumber ());
					current_macdef = CORK_NIL;
					stringListClear (identifiers);
				}
				else if (in_varmac_value && current_macdef != CORK_NIL)
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

					current_macdef = newMacro (name, true, false, CORK_NIL);
					stringListClear (identifiers);
				}
				else if (! strcmp (vStringValue (name), "export")
						 || ! strcmp (vStringValue (name), "override"))
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
						if (!vStringIsEmpty (name) && isAcceptableAsInclude(name))
							newInclude (name, optional, current_macdef);

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
					stringListClear (identifiers);
				}
				else
					directiveFound (name);
			}
		}
		else
			macro_possible = false;
	}

	endTargets (current_targets, getInputLineNumber ());

	intArrayDelete (current_targets);
	stringListDelete (identifiers);
}

static void findMakeTags (void)
{

	subparser *sub = getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser (sub, NULL);

	findMakeTags0 ();
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
