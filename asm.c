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

/*
*   FUNCTION DEFINITIONS
*/

static void installAsmRegex (const langType language)
{
    addTagRegex (language,
	"^([[:alpha:]_.$][[:alnum:]_$]*)[[:space:]]+(=|\\.?(equ|set)|d[cbdfpqtw])[[:space:].]",
	"\\1", "d,define", "i");
    /* gas .equ */
    addTagRegex (language,
	"^[[:blank:]]*\\.(equ|set)[[:blank:]]+([[:alpha:]_.$][[:alnum:]_$]*)",
	"\\1", "d,define", "i");

    addTagRegex (language,
	"^([[:alpha:]_.$][[:alnum:]_$]*)[[:space:]]*(:|[[:blank:]]\\.?label[[:space:]])",
	"\\1", "l,label", "i");

    /* MASM proc */
    addTagRegex (language,
	"^([[:alpha:]_.$][[:alnum:]_$]*)[[:blank:]]+proc[[:space:]]",
	"\\1", "l,label", "i");
    /* TASM proc */
    addTagRegex (language,
	"^proc[[:blank:]]+([[:alpha:]_.$][[:alnum:]_$]*)",
	"\\1", "l,label", "i");

    /* gas macro */
    addTagRegex (language,
	"^[[:blank:]]*\\.macro[[:blank:]]*([[:alpha:]_.$][[:alnum:]_$]*)",
	"\\1", "m,macro", "i");
    /* MASM macro */
    addTagRegex (language,
	"^([[:alpha:]_.$][[:alnum:]_$]*)[[:blank:]]+macro[[:space:]]",
	"\\1", "m,macro", "i");
    /* TASM macro */
    addTagRegex (language,
	"^macro[[:space:]]+([[:alpha:]_.$][[:alnum:]_$]*)",
	"\\1", "m,macro", "i");

    /* MASM structures */
    addTagRegex (language,
	"^([[:alpha:]_.$][[:alnum:]_$]*)[[:blank:]]+(struct|record)[[:space:]]",
	"\\1", "t,type", "i");
    /* TASM structures */
    addTagRegex (language,
	"^struct[[:space:]]+([[:alpha:]_.$][[:alnum:]_$]*)",
	"\\1", "t,type", "i");

    /* make sure this regex is last, or it will catch lines matched above */
    addTagRegex (language,
	"^([[:alpha:]_][[:alnum:]_.$]*)[[:blank:]]",
	"\\1", "l,label", "i");
}

extern parserDefinition* AsmParser (void)
{
    static const char *const extensions [] = { "asm", "ASM", "s", "S", NULL };
    parserDefinition* def = parserNew ("Asm");
    def->extensions = extensions;
    def->initialize = installAsmRegex;
    def->regex      = TRUE;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
