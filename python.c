/*
*   $Id$
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Python language
*   files.
*/
/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_CLASS, K_FUNCTION, K_MEMBER
} pythonKind;

static kindOption PythonKinds[] = {
	{TRUE, 'c', "class",    "classes"},
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'm', "member",   "class members"}
};

/*
*   FUNCTION DEFINITIONS
*/
/* tagEntryInfo and vString should be preinitialized/preallocated but not
 * necessary. If successful you will find class name in vString
 */

#define vStringLast(vs) ((vs)->buffer[(vs)->length - 1])

static boolean isIdentifierFirstCharacter (int c)
{
	return (boolean) (isalpha (c) || c == '_');
}

static boolean isIdentifierCharacter (int c)
{
	return (boolean) (isalnum (c) || c == '_');
}

static void makeFunctionTag (vString *const function, vString *const class)
{
	tagEntryInfo tag;
	initTagEntry (&tag, vStringValue (function));
	if (vStringLength (class) > 0)
	{
		tag.kindName = "member";
		tag.kind = 'm';
		tag.extensionFields.scope [0] = "class";
		tag.extensionFields.scope [1] = vStringValue (class);
	}
	else
	{
		tag.kindName = "function";
		tag.kind = 'f';
	}
	if (strncmp (vStringValue (function), "__", 2) == 0 &&
		strcmp (vStringValue (function), "__init__") != 0)
	{
		tag.extensionFields.access = "private";
		tag.isFileScope = TRUE;
	}
	else
	{
		tag.extensionFields.access = "public";
	}
	makeTagEntry (&tag);
	if (vStringLength (class) > 0  &&  Option.include.qualifiedTags)
	{
		vString *tagname = vStringNew ();
		vStringCat (tagname, class);
		vStringPut (tagname, '.');
		vStringCat (tagname, function);
		tag.name = vStringValue (tagname);
		makeTagEntry (&tag);
		vStringDelete (tagname);
	}
}

static void makeClassTag (vString *const class, vString *const inheritance)
{
	tagEntryInfo tag;
	initTagEntry (&tag, vStringValue (class));
	tag.kindName = "class";
	tag.kind = 'c';
	tag.extensionFields.inheritance = vStringValue (inheritance);
	makeTagEntry (&tag);
}

static const char *skipSpace (const char *cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

/* Starting at ''cp'', parse an identifier into ''identifier''. */
static const char *parseIdentifier (const char *cp, vString *const identifier)
{
	vStringClear (identifier);
	while (isIdentifierCharacter ((int) *cp))
	{
		vStringPut (identifier, (int) *cp);
		++cp;
	}
	vStringTerminate (identifier);
	return cp;
}

static void parseClass (const char *cp, vString *const class)
{
	vString *const inheritance = vStringNew ();
	vStringClear (inheritance);
	cp = parseIdentifier (cp, class);
	cp = skipSpace (cp);
	if (*cp == '(')
	{
		++cp;
		while (*cp != ')')
		{
			if (*cp == '\0')
			{
				/* Closing parenthesis can be in follow up line. */
				cp = (const char *) fileReadLine ();
				if (!cp) break;
				vStringPut (inheritance, ' ');
				continue;
			}
			vStringPut (inheritance, *cp);
			++cp;
		}
		vStringTerminate (inheritance);
	}
	makeClassTag (class, inheritance);
	vStringDelete (inheritance);
}

static void parseFunction (const char *cp, vString *const def,
	vString *const class)
{
	cp = parseIdentifier (cp, def);
	makeFunctionTag (def, class);
}

static void findPythonTags (void)
{
	vString *const class = vStringNew (); /* current class */
	vString *const def = vStringNew (); /* current function */
	vString *const identifier = vStringNew ();
	vString *const continuation = vStringNew ();
	const char *line;
	int class_indent = 0;
	int def_indent = 0;
	int skip_indent = 0;
	int line_skip = 0;
	boolean longStringLiteral = FALSE;

	while ((line = (const char *) fileReadLine ()) != NULL)
	{
		const char *cp = line;
		int indent;

		cp = skipSpace (cp);

		if (*cp == '#' || *cp == '\0')  /* skip comment or blank line */
			continue;

		/* Deal with line continuation. */
		if (!line_skip) vStringClear(continuation);
		vStringCatS(continuation, line);
		vStringStripTrailing(continuation);
		if (vStringLast(continuation) == '\\')
		{
			vStringChop(continuation);
			vStringCatS(continuation, " ");
			line_skip = 1;
			continue;
		}
		cp = line = vStringValue(continuation);
		cp = skipSpace (cp);
		indent = cp - line;
		line_skip = 0;

		if (longStringLiteral)
		{
			cp = strstr (cp, "\"\"\"");
			if (cp == NULL)
				continue;
			else
			{
				longStringLiteral = FALSE;
				cp += 3;
			}
		}
		if (isIdentifierFirstCharacter ((int) *cp))
		{
			if (indent <= class_indent)
				vStringClear (class);

			if (indent <= def_indent)
				vStringClear (def);

			cp = parseIdentifier (cp, identifier);
			if (isspace ((int) *cp))
			{
				cp = skipSpace (cp);
				if (!skip_indent || indent <= skip_indent)
				{
					if (strcmp (vStringValue (identifier), "def") == 0)
					{
						/* Currently, ctags can not handle functions in
						 * functions for python - they are simple ignored.
						 */
						if (!vStringLength (def))
						{
							parseFunction (cp, def, class);
							def_indent = indent;
						}
						else
						{
							skip_indent = indent;
						}
					}
					else if (strcmp (vStringValue (identifier), "class") == 0)
					{
						/* Currently, ctags can not handle classes in functions
						 * or classes for python - they are simply ignored. */
						if (!vStringLength (class) && !vStringLength (def))
						{
							parseClass (cp, class);
							class_indent = indent;
						}
						else
						{
							skip_indent = indent;
						}
					}
				}
			}
		}
		if ((cp = strstr (cp, "\"\"\"")) != NULL)
		{
			cp += 3;
			cp = strstr (cp, "\"\"\"");
			if (cp == NULL)
				longStringLiteral = TRUE;
		}
	}
	vStringDelete (identifier);
	vStringDelete (class);
	vStringDelete (def);
	vStringDelete (continuation);
}

extern parserDefinition *PythonParser (void)
{
	static const char *const extensions[] = { "py", "python", NULL };
	parserDefinition *def = parserNew ("Python");
	def->kinds = PythonKinds;
	def->kindCount = KIND_COUNT (PythonKinds);
	def->extensions = extensions;
	def->parser = findPythonTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
