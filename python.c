/*
*   $Id$
*
*   Copyright (c) 2000-2001, Darren Hiebert
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
#include "general.h"	/* must always come first */

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

static const unsigned char *skipSpace (const unsigned char *cp)
{
    while (isspace ((int) *cp))
	++cp;
    return cp;
}

static const unsigned char *parseIdentifier (
	const unsigned char *cp, vString *const identifier)
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

static void parseClass (const unsigned char *cp, vString *const class)
{
    vString *const inheritance = vStringNew ();
    vStringClear (inheritance);
    cp = parseIdentifier (cp, class);
    cp = skipSpace (cp);
    if (*cp == '(')
    {
	++cp;
	while (*cp != ')'  &&  *cp != '\0')
	{
	    vStringPut (inheritance, *cp);
	    ++cp;
	}
	vStringTerminate (inheritance);
    }
    makeClassTag (class, inheritance);
    vStringDelete (inheritance);
}

static void parseFunction (const unsigned char *cp, vString *const class)
{
    vString *const identifier = vStringNew ();
    cp = parseIdentifier (cp, identifier);
    makeFunctionTag (identifier, class);
    vStringDelete (identifier);
}

static void findPythonTags (void)
{
    vString *const class = vStringNew ();
    vString *const identifier = vStringNew ();
    const unsigned char *line;
    int class_indent = 0;
    boolean longStringLiteral = FALSE;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;
	int indent;

	cp = skipSpace (cp);
	indent = cp - line;

	if (*cp == '#')	/* skip initial comment */
	    continue;
	
	if (longStringLiteral)
	{
	    cp = (const unsigned char*) strstr ((const char*) cp, "\"\"\"");
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

	    cp = parseIdentifier (cp, identifier);
	    if (!isspace ((int) *cp))
		continue;
	    cp = skipSpace (cp);
	    if (strcmp (vStringValue (identifier), "def") == 0)
		parseFunction (cp, class);
	    else if (strcmp (vStringValue (identifier), "class") == 0)
	    {
		parseClass (cp, class);
		class_indent = indent;
	    }
	}
	if (strstr ((const char*) cp, "\"\"\"") != NULL)
	{
	    cp += 3;
	    cp = (const unsigned char*) strstr ((const char*) cp, "\"\"\"");
	    if (cp == NULL)
		longStringLiteral = TRUE;
	}
    }
    vStringDelete (identifier);
    vStringDelete (class);
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

/* vi:set tabstop=8 shiftwidth=4: */
