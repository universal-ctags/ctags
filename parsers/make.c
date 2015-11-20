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
	K_MACRO, K_TARGET, K_INCLUDE
} makeKind;

typedef enum {
	R_INCLUDE_GENERIC,
	R_INCLUDE_OPTIONAL,
} makeIncludeRole;

static roleDesc MakeIncludeRoles [] = {
	RoleTemplateGeneric,
	{ TRUE, "optional", "included as an optional makefile"},
};

static kindOption MakeKinds [] = {
	{ TRUE, 'm', "macro",  "macros"},
	{ TRUE, 't', "target", "targets"},
	{ TRUE, 'I', "include", "includes",
	  .referenceOnly = TRUE, ATTACH_ROLES(MakeIncludeRoles)},
};

/*
*   FUNCTION DEFINITIONS
*/

static int nextChar (void)
{
	int c = fileGetc ();
	if (c == '\\')
	{
		c = fileGetc ();
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
		fileUngetc (c);
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

static void newMacro (vString *const name)
{
	makeSimpleTag (name, MakeKinds, K_MACRO);
}

static void newInclude (vString *const name, boolean optional)
{
	if (isXtagEnabled (XTAG_REFERENCE_TAGS))
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
		if (c == '(' || c == '}')
			depth++;
		else if (depth > 0 && (c == ')' || c == '}'))
			depth--;
		vStringPut (id, c);
		c = nextChar ();
	}
	fileUngetc (c);
	vStringTerminate (id);
}

static void findMakeTags (void)
{
	stringList *identifiers = stringListNew ();
	boolean newline = TRUE;
	boolean in_define = FALSE;
	boolean in_rule = FALSE;
	boolean variable_possible = TRUE;
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
			fileUngetc (c);
			variable_possible = (c == '=');
		}
		else if (variable_possible && c == ':' &&
				 stringListCount (identifiers) > 0)
		{
			c = nextChar ();
			fileUngetc (c);
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
			newMacro (stringListItem (identifiers, 0));
			skipLine ();
			in_rule = FALSE;
		}
		else if (variable_possible && isIdentifier (c))
		{
			vString *name = vStringNew ();
			readIdentifier (c, name);
			stringListAdd (identifiers, name);

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
						fileUngetc (c);
					vStringTerminate (name);
					vStringStripTrailing (name);
					newMacro (name);
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
							fileUngetc (c);

						if (c == EOF || c == '\n')
							break;
					}
				}
			}
		}
		else
			variable_possible = FALSE;
	}
	stringListDelete (identifiers);
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

/* vi:set tabstop=4 shiftwidth=4: */
