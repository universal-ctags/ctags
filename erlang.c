/*
*   $Id$
*
*   Copyright (c) 2003, Brent Fulgham <bfulgham@debian.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Erlang language
*   files.  Some of the parsing constructs are based on the Emacs 'etags'
*   program by Francesco Potori <pot@gnu.org>
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
    K_FUNCTION, K_MODULE, K_RECORD, K_MACRO
} erlangKind;

static kindOption ErlangKinds[] = {
    {TRUE, 'f', "function", "functions"},
    {TRUE, 'm', "module",   "modules"},
    {TRUE, 'r', "record",   "record definitions"},
    {TRUE, 's', "macro",    "macro definitions"},
};

/*
*   FUNCTION DEFINITIONS
*/
/* tagEntryInfo and vString should be preinitialized/preallocated but not
 * necessary. If successful you will find class name in vString
 */

static boolean isIdentifierFirstCharacter (int c)
{
    return (boolean) (isalpha (c));
}

static boolean isIdentifierCharacter (int c)
{
    return (boolean) (isalnum (c) || c == '_' || c == ':');
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

static void makeGenericTag (
	erlangKind kind, vString *const identifier, vString *const module)
{
    tagEntryInfo tag;
    
    initTagEntry (&tag, vStringValue (identifier));
    tag.kindName = ErlangKinds[kind].name;
    tag.kind = ErlangKinds[kind].letter;

    if (module && (vStringLength (module) > 0) )
    {
        tag.extensionFields.scope [0] = "module";
        tag.extensionFields.scope [1] = vStringValue (module);
    }
    makeTagEntry (&tag);
}

static void makeErlangTag (const unsigned char *cp, erlangKind kind,
                vString *const module)
{
    vString *const identifier = vStringNew ();
    cp = parseIdentifier (cp, identifier);
    makeGenericTag (kind, identifier, module);
    vStringDelete (identifier);
}

static void makeModuleTag (const unsigned char *cp, erlangKind kind,
                vString *const module)
{
    vString *const identifier = vStringNew ();
    cp = parseIdentifier (cp, identifier);
    makeGenericTag (kind, identifier, NULL);

    /* All further entries go in the new module */
    vStringCopy (module, identifier);
    vStringDelete (identifier);
}

static void makeFunctionTag (vString *const name, vString *const module)
{
    tagEntryInfo tag;

    initTagEntry (&tag, vStringValue (name));
    tag.kindName = ErlangKinds[K_FUNCTION].name;
    tag.kind = ErlangKinds[K_FUNCTION].letter;

    if (module && (vStringLength (module) > 0) )
    {
        tag.extensionFields.scope [0] = "module";
        tag.extensionFields.scope [1] = vStringValue (module);
    }
    makeTagEntry (&tag);
}

/*
 * Add a function definition tag if:
 * 1) There is no white-space at the start of the line
 * 2) It doesn't match the previous identifier
 */
static void parseFunction (const unsigned char *cp, vString *const previous,
                vString *const module)
{
    /*
     * Rule 1 is met if we are in this routine, so just find the identifier,
     * determine if it is different from the previous, and add it.
     */
    vString *const identifier = vStringNew ();
    cp = parseIdentifier (cp, identifier);
    
    if ( (vStringLength (identifier) != vStringLength (previous) )
         || (strncmp (vStringValue (identifier), vStringValue (previous),
		 vStringLength (identifier)) != 0))
    {
	makeFunctionTag (identifier, module);
	vStringCopy (previous, identifier);
    }
    vStringDelete (identifier);
}

/*
 * Directives are of the form:
 * -module(foo)
 * -define(foo, bar)
 * -record(graph, {vtab = notable, cyclic = true}).
 */
static void parseDirective (const unsigned char *cp,
                vString *const module)
{
    /*
     * A directive will be either a record definition or a directive.
     * Record definitions are handled separately
     */
    vString *const directive = vStringNew ();
    cp = parseIdentifier (cp, directive);
    cp = skipSpace (cp);
    if (*cp == '(')
	++cp;

    if (strncmp (vStringValue (directive), "record", 6) == 0)
	makeErlangTag (cp, K_RECORD, module);
    else if (strncmp (vStringValue (directive), "define", 6) == 0)
    	makeErlangTag (cp, K_MACRO, module);
    else if (strncmp (vStringValue (directive), "module", 6) == 0)
    	makeModuleTag (cp, K_MODULE, module);
    /* Otherwise, it was an import, export, etc. */
    
    vStringDelete (directive);
}

static void findErlangTags (void)
{
    vString *const previous = vStringNew ();
    vString *const module = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;

	if (*cp == '%')	/* skip initial comment */
	    continue;
	if (*cp == '"') /* strings sometimes start in column one */
	    continue;

	if ( *cp == '-')
	{
	    ++cp;	/* Move off of the '-' */
	    parseDirective(cp, module);
	}
	else if (isIdentifierFirstCharacter ((int) *cp))
	    parseFunction (cp, previous, module);
    }
    vStringDelete (previous);
    vStringDelete (module);
}

extern parserDefinition *ErlangParser (void)
{
    static const char *const extensions[] = { "erl", "ERL", "hrl", "HRL", NULL };
    parserDefinition *def = parserNew ("Erlang");
    def->kinds = ErlangKinds;
    def->kindCount = KIND_COUNT (ErlangKinds);
    def->extensions = extensions;
    def->parser = findErlangTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
