/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for assembly language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef enum {
    K_NONE = -1, K_DEFINE, K_LABEL, K_MACRO, K_TYPE
} AsmKind;

typedef struct {
    const char *operator;
    AsmKind kind;
} OpType;

/*
*   DATA DEFINITIONS
*/
static kindOption AsmKinds [] = {
    { TRUE, 'd', "define", "defines" },
    { TRUE, 'l', "label",  "labels"  },
    { TRUE, 'm', "macro",  "macros"  },
    { TRUE, 't', "type",   "types"   }
};

static const OpType OpTypes [] = {
    { "endm",     K_NONE   },
    { "endmacro", K_NONE   },
    { "endp",     K_NONE   },
    { "ends",     K_NONE   },
    { "equ",      K_DEFINE },
    { "label",    K_LABEL  },
    { "macro",    K_MACRO  },
    { "proc",     K_LABEL  },
    { "record",   K_TYPE   },
    { "set",      K_DEFINE },
    { "struct",   K_TYPE   },
    { "=",        K_DEFINE },
    { ":=",       K_DEFINE }
};

static unsigned int OpTypeCount = sizeof (OpTypes) / sizeof (OpType);

/*
*   FUNCTION DEFINITIONS
*/
static boolean isInitialSymbolCharacter (int c)
{
    return (boolean) (c != '\0' && (isalpha (c) || strchr ("_$", c) != NULL));
}

static boolean isSymbolCharacter (int c)
{
    /* '?' character is allowed in AMD 29K family */
    return (boolean) (c != '\0' && (isalnum (c) || strchr ("_$?", c) != NULL));
}

static boolean readPreProc (const unsigned char *const line)
{
    boolean result;
    const unsigned char *cp = line;
    vString *name = vStringNew ();
    while (isSymbolCharacter ((int) *cp))
    {
	vStringPut (name, *cp);
	++cp;
    }
    vStringTerminate (name);
    result = (boolean) (strcmp (vStringValue (name), "define") == 0);
    if (result)
    {
	while (isspace ((int) *cp))
	    ++cp;
	vStringClear (name);
	while (isSymbolCharacter ((int) *cp))
	{
	    vStringPut (name, *cp);
	    ++cp;
	}
	vStringTerminate (name);
	makeSimpleTag (name, AsmKinds, K_DEFINE);
    }
    vStringDelete (name);
    return result;
}

static AsmKind operatorKind (
	const vString *const operator,
	boolean *const found)
{
    AsmKind result = K_NONE;
    *found = FALSE;
    if (vStringLength (operator) > 0)
    {
	unsigned int i;
	for (i = 0  ;  i < OpTypeCount && !*found  ;  ++i)
	{
	    if (strcasecmp (vStringValue (operator), OpTypes [i].operator) == 0)
	    {
		if (OpTypes [i].kind != K_NONE)
		    result = OpTypes [i].kind;
		*found = TRUE;
	    }
	}
    }
    return result;
}

static boolean isDefineOperator (const vString *const operator)
{
    const unsigned char *const op =
	(unsigned char*) vStringValue (operator); 
    const size_t length = vStringLength (operator);
    const boolean result = (boolean) (toupper ((int) *op) == 'D'  &&
	(length == 2 ||
	 (length > 2 && strchr (". \t", (int) op [2]) != NULL)));
    return result;
}

static void makeAsmTag (
	const vString *const name,
	const vString *const operator,
	const boolean initialSymbol,
	const boolean hasColon)
{
    if (vStringLength (name) > 0)
    {
	boolean found;
	const AsmKind kind = operatorKind (operator, &found);
	if (found)
	{
	    if (kind != K_NONE)
		makeSimpleTag (name, AsmKinds, kind);
	}
	else if (initialSymbol)
	{
	    if (isDefineOperator (operator))
		makeSimpleTag (name, AsmKinds, K_DEFINE);
	    else if (hasColon)
		makeSimpleTag (name, AsmKinds, K_LABEL);
	    else if (vStringLength (operator) > 0)
		makeSimpleTag (name, AsmKinds, K_LABEL);
	}
    }
}

static const unsigned char* readSymbol (
	const unsigned char *const start,
	vString *const sym)
{
    const unsigned char *cp = start;
    vStringClear (sym);
    if (isInitialSymbolCharacter ((int) *cp))
    {
	while (isSymbolCharacter ((int) *cp))
	{
	    vStringPut (sym, *cp);
	    ++cp;
	}
	vStringTerminate (sym);
    }
    return cp;
}

static void findAsmTags (void)
{
    vString *name = vStringNew ();
    vString *operator = vStringNew ();
    const unsigned char *line;
    boolean inCComment = FALSE;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;
	boolean hasColon = FALSE;
	boolean initialSymbol = isInitialSymbolCharacter ((int) *cp);
	const boolean isComment = (boolean)
		(*cp != '\0' && strchr (";*@", *cp) != NULL);

	/* skip comments */
	if (strncmp ((const char*) cp, "/*", (size_t) 2) == 0)
	{
	    inCComment = TRUE;
	    cp += 2;
	}
	if (inCComment)
	{
	    do
	    {
		if (strncmp ((const char*) cp, "*/", (size_t) 2) == 0)
		{
		    inCComment = FALSE;
		    cp += 2;
		    break;
		}
		++cp;
	    } while (*cp != '\0');
	}
	if (isComment || inCComment)
	    continue;

	/* read preprocessor defines */
	if (*cp == '#')
	{
	    ++cp;
	    readPreProc (cp);
	    continue;
	}

	/* read symbol */
	if (initialSymbol)
	{
	    cp = readSymbol (cp, name);
	    if (*cp == ':')
	    {
		hasColon = TRUE;
		++cp;
	    }
	    else if (! isspace ((int) *cp))
	    {
		vStringClear (name);
		initialSymbol = FALSE;
	    }
	}
	else if (! isspace ((int) *cp))
	    continue;

	/* skip white space */
	while (isspace ((int) *cp))
	    ++cp;

	/* skip leading dot */
	if (*cp == '.')
	    ++cp;

	/* read operator */
	vStringClear (operator);
	while (*cp != '\0'  &&  ! isspace ((int) *cp))
	{
	    vStringPut (operator, *cp);
	    ++cp;
	}
	vStringTerminate (operator);

	/* attempt second read of symbol */
	if (!initialSymbol)
	{
	    while (isspace ((int) *cp))
		++cp;
	    cp = readSymbol (cp, name);
	}
	makeAsmTag (name, operator, initialSymbol, hasColon);
    }
    vStringDelete (name);
    vStringDelete (operator);
}

extern parserDefinition* AsmParser (void)
{
    static const char *const extensions [] = {
	"asm", "ASM", "s", "S", NULL
    };
    static const char *const patterns [] = {
	"*.A51",
	"*.29[kK]",
	"*.[68][68][kKsSxX]",
	"*.[xX][68][68]",
	NULL
    };
    parserDefinition* def = parserNew ("Asm");
    def->kinds      = AsmKinds;
    def->kindCount  = KIND_COUNT (AsmKinds);
    def->extensions = extensions;
    def->patterns   = patterns;
    def->parser     = findAsmTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
