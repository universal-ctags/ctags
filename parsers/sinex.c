/*
*   Copyright (c) 2025, Franck Reinquin <freinquin@free.fr>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Define tags by looking for block names in a SINEX file
* 
*   The format is fairly simple :
*   %=SNX <version and other details>
*   +BLOCK_NAME
*     content line 1
*     content line 2
*     ...
*   -BLOCK_NAME
*   
*   SINEX blocks are stored one after another and are not nested. Comment
*   lines start with '*' and can be present inside or outside blocks.
*   
*   The parser does not check the block names, its only goal is to register
*   all blocks to help navigate in a text editor (e.g geany).
* 
*   References :
*   https://www.iers.org/SharedDocs/Publikationen/EN/IERS/Documents/ac/sinex/sinex_v202_pdf.pdf
*/
#include "general.h"

#include <string.h>
#include <ctype.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

/* the SINEX standard sets a maximum line length of 80 chars ; a block name has
 * a '+'/'-' prefix so can be up to 79 characters long */
#define MAX_BLOCK_NAME_LEN   79

typedef enum eSinexKinds {
	K_BLOCK
} sinexKind;

static kindDefinition SinexKinds [] = {
	{ true, 'b', "block",		"blocks" }
};


static void blockNameCopy (char blockName[MAX_BLOCK_NAME_LEN+1], const char *line)
{
	strncpy (blockName, (const char *)&line[1], MAX_BLOCK_NAME_LEN);
	blockName[MAX_BLOCK_NAME_LEN] = '\0'; /* THIS IS O.K. */

	// remove possible trailing spaces
	for (char * ptr = blockName+strlen(blockName)-1 ; (ptr>=blockName) && isspace(*ptr) ; ptr--)
		*ptr = '\0' ;
}


static void findSinexTags (void)
{
	const unsigned char *line;
	tagEntryInfo e;
	char blockNameStart[MAX_BLOCK_NAME_LEN+1];
	char blockNameEnd[MAX_BLOCK_NAME_LEN+1];
	bool inBlock = false ;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		if (line[0] == '+')
		{
			blockNameCopy(blockNameStart, (const char *)line);
			if (strlen(blockNameStart) > 0) {
				initTagEntry (&e, (const char * const)blockNameStart, K_BLOCK);
				inBlock = true ;
			}
		} 
		else if (inBlock && (line[0] == '-'))
		{
			unsigned long lineNumber = getInputLineNumber ();
			blockNameCopy(blockNameEnd, (const char *)line);
			if (strcmp (blockNameStart, blockNameEnd) == 0)
			{
				setTagEndLine(&e, lineNumber);
				makeTagEntry (&e);
				inBlock = false ;
			}
		}
	}
}

/* parser definition */
extern parserDefinition* SinexParser (void)
{
	static const char *const extensions [] = { "snx", "sinex", NULL };
	parserDefinition* def = parserNew ("SINEX");
	def->kindTable  = SinexKinds;
	def->kindCount  = ARRAY_SIZE (SinexKinds);
	def->extensions = extensions;
	def->parser     = findSinexTags;
	return def;
}
