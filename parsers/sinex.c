/***************************************************************************
 * sinex.c
 * Define tags by looking for block names in a SINEX file
 * 
 * SINEX format :
 * https://www.iers.org/SharedDocs/Publikationen/EN/IERS/Documents/ac/sinex/sinex_v202_pdf.pdf
 * 
 * The format is fairly simple :
 * %SNX <version and other details>
 * +BLOCK_NAME
 *   content line 1
 *   content line 2
 *   ...
 * -BLOCK_NAME
 * 
 * SINEX blocks are stored one after another and are not nested. Comment
 * lines start with '*' and can be present inside or outside blocks.
 * 
 * The parser does not check the block names, its only goal is to register
 * all blocks to help navigate in a text editor (e.g geany).
 * 
 * Author - Franck Reinquin <freinquin@free.fr>
 * License GPL-2
 **************************************************************************/
#include "general.h"

#include <string.h>
#include <ctype.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"


typedef enum eSinexKinds {
	K_HEADER, K_BLOCK
} sinexKind;

static kindDefinition SinexKinds [] = {
	{ true, 'h', "header",		"header" },
	{ true, 'b', "block",		"blocks" }
};


static void findSinexTags (void)
{
	const unsigned char *line;
	unsigned long lineNumber ;
	tagEntryInfo e;
	unsigned char blockNameStart[80];   // block max len = 79
	unsigned char blockNameEnd[80];
	bool inBlock = 0 ;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		if (line[0] == '+')
		{
			MIOPos filePosition = getInputFilePosition ();
			lineNumber = getInputLineNumber ();
			strncpy ((char *)blockNameStart, (const char *)&line[1], 79);
			blockNameStart[79] = '\0' ;      // safeguard if line length > 80 (should not happen)
			initTagEntry (&e, (const char * const)blockNameStart, K_BLOCK);
			updateTagLine (&e, lineNumber, filePosition);
			inBlock = 1 ;
		} 
		else if (inBlock && (line[0] == '-'))
		{
			lineNumber = getInputLineNumber ();
			strncpy ((char *)blockNameEnd, (const char *)&line[1], 79);
			blockNameEnd[79] = '\0' ;      // safeguard if line length > 80 (should not happen)
			if (strcmp ((const char *)blockNameStart, (const char *)blockNameEnd) == 0)
			{
				setTagEndLine(&e, lineNumber);
				makeTagEntry (&e);
				inBlock = 0 ;
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
