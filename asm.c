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
#include "vstring.h"
#ifdef HAVE_REGCOMP
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h> /* declare off_t (not known to regex.h on FreeBSD) */
# endif
# include "regex.h"
#endif

/*
*   MACROS
*/
/* '?' character is allowed in AMD 29K family */
#define SYMBOL "[[:alpha:]_$][[:alnum:]_$?]*"
#define EQU "(:?=|(:[[:blank:]]+)?\\.?(equ|set|d[bcdfpqstw][.[:space:]]))"

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_LABEL
} AsmKind;

static kindOption AsmKinds [] = {
    { TRUE, 'l', "label", "labels"}
};


/*
*   FUNCTION DEFINITIONS
*/

static void checkLabel (const char *line, const regexMatch *matches,
			unsigned int count)
{
#ifdef HAVE_REGCOMP
    static regex_t LabelReject;
    static boolean compiled = FALSE;
    if (! compiled)
    {
	/* rejects patterns matching other constructs */
	compiled = (boolean) (regcomp (&LabelReject, EQU,
		REG_EXTENDED|REG_ICASE) == 0);
    }
    if (compiled && count > 2)
    {
	const char *match1 = line + matches [1].start;
	const char *match2 = line + matches [2].start;
	if (regexec (&LabelReject, match2, (size_t) 0, NULL, 0) != 0)
	{
	    vString *name = vStringNew ();
	    vStringNCopyS (name, match1, matches [1].length);
	    makeSimpleTag (name, AsmKinds, K_LABEL);
	    vStringDelete (name);
	}
    }
#endif
}

/*
 * Based upon code samples from http://www.programmersheaven.com/zone5/
 */
static void installAsmRegex (const langType language)
{
    /*
     * abc =
     * abc :=
     * abc \.?EQU
     * abc \.?SET
     * abc \.?D[BCDFPQSTW][. ]
     */
    addTagRegex (language,
	"^(" SYMBOL ")[[:blank:]]*(" EQU ")",
	"\\1", "d,define", "i");
    /* gas .equ */
    addTagRegex (language,
	"^[[:blank:]]*\\.(equ|set)[[:blank:]]+(" SYMBOL ")",
	"\\2", "d,define", "i");

    /*
     * abc:
     * abc LABEL
     */
    addCallbackRegex (language,
	"^(" SYMBOL ")[[:blank:]]*(:|[[:blank:]]label[[:space:]])",
	"i", checkLabel);

    addCallbackRegex (language,
	"^(" SYMBOL ")[[:blank:]]+([[:alpha:]].*)$",
	"i", checkLabel);

    /* MASM proc */
    addTagRegex (language,
	"^(" SYMBOL ")[[:blank:]]+proc[[:space:]]",
	"\\1", "l,label", "i");
    /* TASM proc */
    addTagRegex (language,
	"^proc[[:blank:]]+(" SYMBOL ")",
	"\\1", "l,label", "i");

    /* gas macro */
    addTagRegex (language,
	"^[[:blank:]]*\\.macro[[:blank:]]*(" SYMBOL ")",
	"\\1", "m,macro", "i");
    /* MASM macro */
    addTagRegex (language,
	"^(" SYMBOL ")[[:blank:]]+macro[[:space:]]",
	"\\1", "m,macro", "i");
    /* TASM macro */
    addTagRegex (language,
	"^macro[[:blank:]]+(" SYMBOL ")",
	"\\1", "m,macro", "i");

    /* MASM structures */
    addTagRegex (language,
	"^(" SYMBOL ")[[:blank:]]+(struct|record)[[:space:]]",
	"\\1", "t,type", "i");
    /* TASM structures */
    addTagRegex (language,
	"^struct[[:blank:]]+(" SYMBOL ")",
	"\\1", "t,type", "i");
}

extern parserDefinition* AsmParser (void)
{
    static const char *const extensions [] = {
	"asm", "ASM", "s", "S", NULL
    };
    static const char *const patterns [] = {
	"*.29[kK]",
	"*.[68][68][kKsSxX]",
	"*.[xX][68][68]",
	"*.A5[01]",
	NULL
    };
    parserDefinition* def = parserNew ("Asm");
    def->kinds      = AsmKinds;
    def->kindCount  = KIND_COUNT (AsmKinds);
    def->extensions = extensions;
    def->patterns   = patterns;
    def->initialize = installAsmRegex;
    def->regex      = TRUE;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
