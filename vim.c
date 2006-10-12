/*
*	$Id$
*
*	Copyright (c) 2000-2003, Darren Hiebert
*
*	This source code is released for free distribution under the terms of the
*	GNU General Public License.
*
*	Thanks are due to Jay Glanville for significant improvements.
*
*	This module contains functions for generating tags for user-defined
*	functions for the Vim editor.
*/

/*
*	INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*	DATA DEFINITIONS
*/
typedef enum {
	K_AUGROUP,
	K_FUNCTION,
	K_VARIABLE
} vimKind;

static kindOption VimKinds [] = {
	{ TRUE,  'a', "augroup",  "autocommand groups" },
	{ TRUE,  'f', "function", "function definitions" },
	{ TRUE,  'v', "variable", "variable definitions" },
};

/*
*	FUNCTION DEFINITIONS
*/

/* This function takes a char pointer, tries to find a scope separator in the
 * string, and if it does, returns a pointer to the character after the colon,
 * and the character defining the scope.
 * If a colon is not found, it returns the original pointer.
 */
static const unsigned char* skipPrefix (const unsigned char* name, int *scope)
{
	const unsigned char* result = name;
	int counter;
	if (scope != NULL)
		*scope = '\0';
	if (name[1] == ':')
	{
		if (scope != NULL)
			*scope = *name;
		result = name + 2;
	}
	else if (strncasecmp ((const char*) name, "<SID>", (size_t) 5) == 0)
	{
		if (scope != NULL)
			*scope = *name;
		result = name + 5;
	}
	else
	{
		/*
		 * Vim7 check for dictionaries or autoload function names
		 */
		counter = 0;
		do
		{
			switch ( name[counter] )
			{
				case '.':
					/* Set the scope to d - Dictionary */
					*scope = 'd';
					break;
				case '#':
					/* Set the scope to a - autoload */
					*scope = 'a';
					break;
			}
			++counter;
		} while (isalnum ((int) name[counter]) ||  
				name[counter] == '_'		   ||  
				name[counter] == '.'		   ||  
				name[counter] == '#'
				);
	}
	return result;
}

static void findVimTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;
	boolean inFunction = FALSE;
	int scope;

	while ((line = fileReadLine ()) != NULL)
	{
		while (isspace ((int) *line))
			++line;
		if ((int) *line == '"')
			continue;  /* skip comment */
		if (strncmp ((const char*) line, "fu", (size_t) 2) == 0)
		{
			const unsigned char *cp = line + 1;
			inFunction = TRUE;

			if ((int) *++cp == 'n'	&&	(int) *++cp == 'c'	&&
				(int) *++cp == 't'	&&	(int) *++cp == 'i'	&&
				(int) *++cp == 'o'	&&	(int) *++cp == 'n')
					++cp;
			if ((int) *cp == '!')
				++cp;
			if (isspace ((int) *cp))
			{
				while (isspace ((int) *cp))
					++cp;
				cp = skipPrefix (cp, &scope);
				if (isupper ((int) *cp)  ||  
						scope == 's'  ||  /* script scope */
						scope == '<'  ||  /* script scope */
						scope == 'd'  ||  /* dictionary */
						scope == 'a')	  /* autoload */
				{
					do
					{
						vStringPut (name, (int) *cp);
						++cp;
					} while (isalnum ((int) *cp) ||  *cp == '_' ||	*cp == '.' ||  *cp == '#');
					vStringTerminate (name);
					makeSimpleTag (name, VimKinds, K_FUNCTION);
					vStringClear (name);
				}
			}
		}

		if	(strncmp ((const char*) line, "aug", (size_t) 3) == 0)
		{
			/* Found Autocommand Group (augroup) */
			const unsigned char *cp = line + 2;
			if ((int) *++cp == 'r' && (int) *++cp == 'o' &&
				(int) *++cp == 'u' && (int) *++cp == 'p')
					++cp;
			if (isspace ((int) *cp))
			{
				while (isspace ((int) *cp))
					++cp; 
				if (strncasecmp ((const char*) cp, "end", (size_t) 3) != 0)
				{	 
					do
					{
						vStringPut (name, (int) *cp);
						++cp;
					} while (isalnum ((int) *cp)  ||  *cp == '_');
					vStringTerminate (name);
					makeSimpleTag (name, VimKinds, K_AUGROUP);
					vStringClear (name);
				}
			}
		}

		if (strncmp ((const char*) line, "endf", (size_t) 4) == 0)
			inFunction = FALSE;

		if (!inFunction  &&
				strncmp ((const char*) line, "let", (size_t) 3) == 0)
		{
			/* we've found a variable declared outside of a function!! */
			const unsigned char *cp = line + 3;
			/* get the name */
			if (isspace ((int) *cp))
			{
				/* deal with spaces, $, @ and & */
				while (!isalnum ((int) *cp))
					++cp;
				cp = skipPrefix (cp, &scope);
				do
				{
					vStringPut (name, (int) *cp);
					++cp;
				} while (isalnum ((int) *cp)  ||  *cp == '_');
				vStringTerminate (name);
				makeSimpleTag (name, VimKinds, K_VARIABLE);
				vStringClear (name);
			}
		}
	}
	vStringDelete (name);
}

extern parserDefinition* VimParser (void)
{
	static const char *const extensions [] = { "vim", NULL };
	parserDefinition* def = parserNew ("Vim");
	def->kinds		= VimKinds;
	def->kindCount	= KIND_COUNT (VimKinds);
	def->extensions = extensions;
	def->parser		= findVimTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
