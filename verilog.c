/*
 *  $Id$
 *
 *  Copyright (c) 2002, Nam SungHyun <namsh@kldp.org>
 *
 *  This source code is released for free distribution under the terms of the
 *  GNU General Public License.
 *
 *  This module contains functions for generating tags for the Verilog HDL
 *  (Hardware Description Language).
 *
 *  o	Should I support next cases (@ == *)?
 *
 *	    reg net_1; wire net_2; // net_2 should be tagged
 *
 *	    /@ comment @/ reg net_3; // net_3 should be tagged
 *	    wire /@ comment @/ net_3; // net_3 should be tagged
 *
 *	    /@ multi-line comment start
 *		reg net_4; // net_4 should NOT be tagged
 *	     @/
 *
 *	    // multiline declarations
 *	    inout [(`ABUSWIDTH-1):0]
 *			    addrbus;
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
 *   DATA DECLARATIONS
 */
typedef enum {
    K_UNDEFINED = -1,
    K_FUNCTION, K_MODULE, K_PARAMETER, K_PORT, K_REG, K_TASK,
    K_VARIABLE, K_WIRE
} verilogKind;

typedef struct {
    const char *keyword;
    verilogKind kind;
} keywordAssoc;

/*
 *   DATA DEFINITIONS
 */
static kindOption VerilogKinds [] = {
    { TRUE, 'f', "function",  "functions" },
    { TRUE, 'm', "module",    "modules" },
    { TRUE, 'P', "parameter", "parameters, defines" },
    { TRUE, 'p', "port",      "ports (input, output, inout)" },
    { TRUE, 'r', "reg",       "registers" },
    { TRUE, 't', "task",      "tasks" },
    { TRUE, 'v', "variable",  "variables (integer, real, time)" },
    { TRUE, 'w', "wire",      "wires (supply, tri, wire, wand, ...)" }
};

static keywordAssoc keywordTable [] = {
    { "`define",   K_PARAMETER },
    { "function",  K_FUNCTION },
    { "inout",     K_PORT },
    { "input",     K_PORT },
    { "integer",   K_VARIABLE },
    { "module",    K_MODULE },
    { "output",    K_PORT },
    { "parameter", K_PARAMETER },
    { "real",      K_VARIABLE },
    { "reg",       K_REG },
    { "supply0",   K_WIRE },
    { "supply1",   K_WIRE },
    { "task",      K_TASK },
    { "time",      K_VARIABLE },
    { "tri0",      K_WIRE },
    { "tri1",      K_WIRE },
    { "triand",    K_WIRE },
    { "tri",       K_WIRE },
    { "trior",     K_WIRE },
    { "trireg",    K_WIRE },
    { "wand",      K_WIRE },
    { "wire",      K_WIRE },
    { "wor",       K_WIRE }
};

#define K_NUM (sizeof(keywordTable) / sizeof(keywordTable[0]))

/*
 *   FUNCTION DEFINITIONS
 */

static void findVerilogTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;
	verilogKind vkind = K_UNDEFINED;
	unsigned int i;

	while (isspace ((int) *cp))
	    ++cp;

	if (cp [0] == '/' && cp [1] == '/')
	    continue;

	for (i = 0  ;  i < K_NUM  ;  ++i)
	{
	    const keywordAssoc *p = keywordTable + i;
	    size_t klen = strlen (p->keyword);
	    if (strncmp ((const char*) cp, p->keyword, klen) == 0
		&& isspace ((int) cp [klen]))
	    {
		cp += klen + 1;
		vkind = p->kind;
		break;
	    }
	}

	if (vkind != K_UNDEFINED)
	{
	    /* Many keywords can have bit width.
	     *	    reg [3:0] net_name;
	     *	    inout [(`DBUSWIDTH-1):0] databus;
	     */
	    for ( ;  *cp  ;  ++cp)
	    {
		if (*cp == '[')
		{
		    while (*cp != ']')
			++cp;
		}
		else if (isspace ((int) *cp) == 0)
		    break;
	    }

	    if (*cp != '\0')
	    {
		for ( ;  isalnum ((int) *cp) || *cp == '_'  ;  ++cp)
		    vStringPut (name, (int) *cp);
		vStringTerminate (name);
		makeSimpleTag (name, VerilogKinds, vkind);
		vStringClear (name);
	    }
	}
    }
    vStringDelete (name);
}

extern parserDefinition* VerilogParser (void)
{
    static const char *const extensions [] = { "v", NULL };
    parserDefinition* def = parserNew ("Verilog");
    def->kinds      = VerilogKinds;
    def->kindCount  = KIND_COUNT (VerilogKinds);
    def->extensions = extensions;
    def->parser     = findVerilogTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
