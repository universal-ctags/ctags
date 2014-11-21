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
#include <setjmp.h>
#ifdef DEBUG
#include <stdio.h>
#endif


#include "parse.h"
#include "read.h"
#include "vstring.h"

#if 0
typedef struct sLineInfo {
	tokenType	type;
	keywordId	keyword;
	vString *	string;
	vString *	scope;
	unsigned long lineNumber;
	fpos_t filePosition;
} lineInfo;
#endif

/*
*	DATA DEFINITIONS
*/
typedef enum {
	K_AUGROUP,
	K_COMMAND,
	K_FUNCTION,
	K_MAP,
	K_VARIABLE,
	K_FILENAME
} vimKind;

static kindOption VimKinds [] = {
	{ TRUE,  'a', "augroup",  "autocommand groups" },
	{ TRUE,  'c', "command",  "user-defined commands" },
	{ TRUE,  'f', "function", "function definitions" },
	{ TRUE,  'm', "map",      "maps" },
	{ TRUE,  'v', "variable", "variable definitions" },
	{ TRUE,  'n', "filename", "vimball filename" },
};

/*
 *	 DATA DECLARATIONS
 */

#if 0
typedef enum eException {
	ExceptionNone, ExceptionEOF 
} exception_t;
#endif

/*
 *	DATA DEFINITIONS
 */

#if 0
static jmp_buf Exception;
#endif

/*
 *	FUNCTION DEFINITIONS
 */

static boolean parseVimLine (const unsigned char *line, int infunction);

/* This function takes a char pointer, tries to find a scope separator in the
 * string, and if it does, returns a pointer to the character after the colon,
 * and the character defining the scope.
 * If a colon is not found, it returns the original pointer.
 */
static const unsigned char* skipPrefix (const unsigned char* name, int *scope)
{
	const unsigned char* result = name;
	int counter;
	size_t length;
	length = strlen((const char*)name);
	if (scope != NULL)
		*scope = '\0';
	if (length > 3 && name[1] == ':')
	{
		if (scope != NULL)
			*scope = *name;
		result = name + 2;
	}
	else if (length > 5 && strncasecmp ((const char*) name, "<SID>", (size_t) 5) == 0)
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

static boolean starts_with_cmd (const unsigned char* line, const char* word)
{
	int lenword = strlen(word);
	if (strncmp((const char*) line, word, lenword) == 0
			&& (line[lenword] == '\0' ||
				line[lenword] == ' ' ||
				line[lenword] == '!' ||
				line[lenword] == '\t'))
		return TRUE;
	return FALSE;
}


static boolean isMap (const unsigned char* line)
{
	/*
	 * There are many different short cuts for specifying a map.
	 * This routine should capture all the permutations.
	 */
	if (
			starts_with_cmd(line, "map") ||
			starts_with_cmd(line, "nm") ||
			starts_with_cmd(line, "nma") ||
			starts_with_cmd(line, "nmap") ||
			starts_with_cmd(line, "vm") ||
			starts_with_cmd(line, "vma") ||
			starts_with_cmd(line, "vmap") ||
			starts_with_cmd(line, "om") ||
			starts_with_cmd(line, "oma") ||
			starts_with_cmd(line, "omap") ||
			starts_with_cmd(line, "im") ||
			starts_with_cmd(line, "ima") ||
			starts_with_cmd(line, "imap") ||
			starts_with_cmd(line, "lm") ||
			starts_with_cmd(line, "lma") ||
			starts_with_cmd(line, "lmap") ||
			starts_with_cmd(line, "cm") ||
			starts_with_cmd(line, "cma") ||
			starts_with_cmd(line, "cmap") ||
			starts_with_cmd(line, "no") ||
			starts_with_cmd(line, "nor") ||
			starts_with_cmd(line, "nore") ||
			starts_with_cmd(line, "norem") ||
			starts_with_cmd(line, "norema") ||
			starts_with_cmd(line, "noremap") ||
			starts_with_cmd(line, "nno") ||
			starts_with_cmd(line, "nnor") ||
			starts_with_cmd(line, "nnore") ||
			starts_with_cmd(line, "nnorem") ||
			starts_with_cmd(line, "nnorema") ||
			starts_with_cmd(line, "nnoremap") ||
			starts_with_cmd(line, "vno") ||
			starts_with_cmd(line, "vnor") ||
			starts_with_cmd(line, "vnore") ||
			starts_with_cmd(line, "vnorem") ||
			starts_with_cmd(line, "vnorema") ||
			starts_with_cmd(line, "vnoremap") ||
			starts_with_cmd(line, "ono") ||
			starts_with_cmd(line, "onor") ||
			starts_with_cmd(line, "onore") ||
			starts_with_cmd(line, "onorem") ||
			starts_with_cmd(line, "onorema") ||
			starts_with_cmd(line, "onoremap") ||
			starts_with_cmd(line, "ino") ||
			starts_with_cmd(line, "inor") ||
			starts_with_cmd(line, "inore") ||
			starts_with_cmd(line, "inorem") ||
			starts_with_cmd(line, "inorema") ||
			starts_with_cmd(line, "inoremap") ||
			starts_with_cmd(line, "lno") ||
			starts_with_cmd(line, "lnor") ||
			starts_with_cmd(line, "lnore") ||
			starts_with_cmd(line, "lnorem") ||
			starts_with_cmd(line, "lnorema") ||
			starts_with_cmd(line, "lnoremap") ||
			starts_with_cmd(line, "cno") ||
			starts_with_cmd(line, "cnor") ||
			starts_with_cmd(line, "cnore") ||
			starts_with_cmd(line, "cnorem") ||
			starts_with_cmd(line, "cnorema") ||
			starts_with_cmd(line, "cnoremap")
			)
			return TRUE;

	return FALSE;
}

static const unsigned char * readVimLine (void)
{
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		while (isspace ((int) *line))
			++line;

		if ((int) *line == '"')
			continue;  /* skip comment */

		break;
	}

	return line;
}

static const unsigned char * readVimballLine (void)
{
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		break;
	}

	return line;
}

static void parseFunction (const unsigned char *line)
{
	vString *name = vStringNew ();
	/* boolean inFunction = FALSE; */
	int scope;

	const unsigned char *cp = line + 1;

	if ((int) *++cp == 'n'	&&	(int) *++cp == 'c'	&&
		(int) *++cp == 't'	&&	(int) *++cp == 'i'	&&
		(int) *++cp == 'o'	&&	(int) *++cp == 'n')
			++cp;
	if ((int) *cp == '!')
		++cp;
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp;

		if (*cp)
		{
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

	/* TODO - update struct to indicate inside function */
	while ((line = readVimLine ()) != NULL)
	{
		/* 
		 * Vim7 added the for/endfo[r] construct, so we must first
		 * check for an "endfo", before a "endf"
		 */
		if ( (!strncmp ((const char*) line, "endfo", (size_t) 5) == 0) && 
				(strncmp ((const char*) line, "endf", (size_t) 4) == 0)   )
			break;

		parseVimLine(line, TRUE);
	}
	vStringDelete (name);
}

static void parseAutogroup (const unsigned char *line)
{
	vString *name = vStringNew ();

	/* Found Autocommand Group (augroup) */
	const unsigned char *cp = line + 2;
	if ((int) *++cp == 'r' && (int) *++cp == 'o' &&
			(int) *++cp == 'u' && (int) *++cp == 'p')
		++cp;
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp; 

		if (*cp)
		{
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
	vStringDelete (name);
}

static boolean parseCommand (const unsigned char *line)
{
	vString *name = vStringNew ();
	boolean cmdProcessed = TRUE;

	/* 
	 * Found a user-defined command 
	 *
	 * They can have many options preceeded by a dash
	 * command! -nargs=+ -complete Select  :call s:DB_execSql("select " . <q-args>)
	 * The name of the command should be the first word not preceeded by a dash
	 *
	 */
	const unsigned char *cp = line;

	if ( cp && ( (int) *cp == '\\' ) ) 
	{
		/*
		 * We are recursively calling this function is the command
		 * has been continued on to the next line
		 *
		 * Vim statements can be continued onto a newline using a \
		 * to indicate the previous line is continuing.
		 *
		 * com -nargs=1 -bang -complete=customlist,EditFileComplete
		 * 			\ EditFile edit<bang> <args>
		 *
		 * If the following lines do not have a line continuation
		 * the command must not be spanning multiple lines and should
		 * be synatically incorrect.
		 */
		if ((int) *cp == '\\')
			++cp;

		while (*cp && isspace ((int) *cp))
			++cp; 
	}
	else if ( line && 
                     (!strncmp ((const char*) line, "comp", (size_t) 4) == 0) && 
		                (!strncmp ((const char*) line, "comc", (size_t) 4) == 0) && 
				          (strncmp ((const char*) line, "com", (size_t) 3) == 0) )
	{
		cp += 2;
		if ((int) *++cp == 'm' && (int) *++cp == 'a' &&
				(int) *++cp == 'n' && (int) *++cp == 'd')
			++cp;

		if ((int) *cp == '!')
			++cp;

		if ((int) *cp != ' ')
		{
			/*
			 * :command must be followed by a space.  If it is not, it is 
			 * not a valid command.
			 * Treat the line as processed and continue.
			 */
			cmdProcessed = TRUE;
			goto cleanUp;
		}

		while (*cp && isspace ((int) *cp))
			++cp; 
	} 
	else 
	{
		/*
		 * We are recursively calling this function.  If it does not start
		 * with "com" or a line continuation character, we have moved off
		 * the command line and should let the other routines parse this file.
		 */
		cmdProcessed = FALSE;
		goto cleanUp;
	}

	/*
	 * Strip off any spaces and options which are part of the command.
	 * These should preceed the command name.
	 */
	do
	{
		if (isspace ((int) *cp))
		{
			++cp;
		}
		else if (*cp == '-')
		{
			/* 
			 * Read until the next space which separates options or the name
			 */
			while (*cp && !isspace ((int) *cp))
				++cp; 
		}
		else if (!isalnum ((int) *cp))
		{
			/*
			 * Broken syntax: throw away this line
			 */
			cmdProcessed = TRUE;
			goto cleanUp;
		}
	} while ( *cp &&  !isalnum ((int) *cp) );

	if ( ! *cp )
	{
		/*
		 * We have reached the end of the line without finding the command name.
		 * Read the next line and continue processing it as a command.
		 */
		if ((line = readVimLine ()) != NULL)
			cmdProcessed = parseCommand(line);
		else
			cmdProcessed = FALSE;
		goto cleanUp;
	}

	do
	{
		vStringPut (name, (int) *cp);
		++cp;
	} while (isalnum ((int) *cp)  ||  *cp == '_');

	vStringTerminate (name);
	makeSimpleTag (name, VimKinds, K_COMMAND);
	vStringClear (name);

cleanUp:
	vStringDelete (name);

	return cmdProcessed;
}

static void parseLet (const unsigned char *line, int infunction)
{
	vString *name = vStringNew ();

	const unsigned char *cp = line + 3;
	const unsigned char *np = line;
	/* get the name */
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp;

		/* 
		 * Ignore lets which set:
		 *    &  - local buffer vim settings
		 *    @  - registers
		 *    [  - Lists or Dictionaries
		 */
		if (!*cp || *cp == '&' || *cp == '@' || *cp == '[' )
			goto cleanUp;

		/* 
		 * Ignore vim variables which are read only
		 *    v: - Vim variables.
		 */
		np = cp;
		++np;
		if ((int) *cp == 'v' && (int) *np == ':' )
			goto cleanUp;

		/* Skip non-global vars in functions */
		if (infunction && (int) *cp != 'g')
			goto cleanUp;

		/* deal with spaces, $, @ and & */
		while (*cp && *cp != '$' && !isalnum ((int) *cp))
			++cp;

		if (!*cp)
			goto cleanUp;

		/* cp = skipPrefix (cp, &scope); */
		do
		{
			if (!*cp)
				break;

			vStringPut (name, (int) *cp);
			++cp;
		} while (isalnum ((int) *cp)  ||  *cp == '_'  ||  *cp == '#'  ||  *cp == ':'  ||  *cp == '$');
		vStringTerminate (name);
		makeSimpleTag (name, VimKinds, K_VARIABLE);
		vStringClear (name);
	}

cleanUp:
	vStringDelete (name);
}

static boolean parseMap (const unsigned char *line)
{
	vString *name = vStringNew ();

	const unsigned char *cp = line;

	/* Remove map */
	while (*cp && isalnum ((int) *cp))
		++cp;

	if ((int) *cp == '!')
		++cp;

	/*
	 * Maps follow this basic format
	 *     map 
     *    nnoremap <silent> <F8> :Tlist<CR>
     *    map <unique> <Leader>scdt <Plug>GetColumnDataType
     *    inoremap ,,, <esc>diwi<<esc>pa><cr></<esc>pa><esc>kA
     *    inoremap <buffer> ( <C-R>=PreviewFunctionSignature()<LF> 
	 *
	 * The Vim help shows the various special arguments available to a map:
	 * 1.2 SPECIAL ARGUMENTS					*:map-arguments*
     *    <buffer>
	 *    <silent>
	 *    <script>
	 *    <unique>
	 *    <special>
	 *    <expr>
	 *
	 * Strip the special arguments from the map command, this should leave
	 * the map name which we will use as the "name".
	 */
	
	do
	{
		while (*cp && isspace ((int) *cp))
			++cp; 

		if (strncmp ((const char*) cp, "<Leader>", (size_t) 8) == 0)
			break;
	
		if (
				strncmp ((const char*) cp, "<buffer>", (size_t) 8) == 0 ||
				strncmp ((const char*) cp, "<silent>", (size_t) 8) == 0 ||
				strncmp ((const char*) cp, "<script>", (size_t) 8) == 0 ||
				strncmp ((const char*) cp, "<unique>", (size_t) 8) == 0
		   )
		{
			cp += 8;
			continue;
		}
	
		if (strncmp ((const char*) cp, "<expr>", (size_t) 6) == 0)
		{
			cp += 6;
			continue;
		}
	
		if (strncmp ((const char*) cp, "<special>", (size_t) 9) == 0)
		{
			cp += 9;
			continue;
		}
	
		break;
	} while (*cp);

	do
	{
		vStringPut (name, (int) *cp);
		++cp;
	} while (*cp && *cp != ' ');

	vStringTerminate (name);
	makeSimpleTag (name, VimKinds, K_MAP);
	vStringClear (name);

	vStringDelete (name);

	return TRUE;
}

static boolean parseVimLine (const unsigned char *line, int infunction)
{
	boolean readNextLine = TRUE;

	if ( (!strncmp ((const char*) line, "comp", (size_t) 4) == 0) && 
			(!strncmp ((const char*) line, "comc", (size_t) 4) == 0) && 
			(strncmp ((const char*) line, "com", (size_t) 3) == 0) )
	{
		readNextLine = parseCommand(line);
		/* TODO - Handle parseCommand returning FALSE */
	}

	else if (isMap(line))
	{
		parseMap(line);
	}

	else if (strncmp ((const char*) line, "fu", (size_t) 2) == 0)
	{
		parseFunction(line);
	}

	else if	(strncmp ((const char*) line, "aug", (size_t) 3) == 0)
	{
		parseAutogroup(line);
	}

	else if ( strncmp ((const char*) line, "let", (size_t) 3) == 0 )
	{
		parseLet(line, infunction);
	}

	return readNextLine;
}

static void parseVimFile (const unsigned char *line)
{
	boolean readNextLine = TRUE;

	while (line != NULL)
	{
		readNextLine = parseVimLine(line, FALSE);

		if ( readNextLine )
			line = readVimLine();

	}
}

static void parseVimBallFile (const unsigned char *line)
{
	vString *fname = vStringNew ();
	const unsigned char *cp;
	int file_line_count;
	int i;

	/*
	 * Vimball Archives follow this format
	 *    " Vimball Archiver comment
	 *    UseVimball
	 *    finish
	 *    filename
	 *    line count (n) for filename
	 *    (n) lines
	 *    filename
	 *    line count (n) for filename
	 *    (n) lines
	 *    ...
	 */

	/* Next line should be "finish" */
	line = readVimLine();
	if (line == NULL)
	{
		return;
	}
	while (line != NULL)
	{
		/* Next line should be a filename */
		line = readVimLine();
		if (line == NULL)
		{
			return;
		}
		else
		{
			cp = line;
			do
			{
				vStringPut (fname, (int) *cp);
				++cp;
			} while (isalnum ((int) *cp) ||  *cp == '.' ||  *cp == '/' ||	*cp == '\\');
			vStringTerminate (fname);
			makeSimpleTag (fname, VimKinds, K_FILENAME);
			vStringClear (fname);
		}

		file_line_count = 0;
		/* Next line should be the line count of the file */
		line = readVimLine();
		if (line == NULL)
		{
			return;
		}
		else
		{
			file_line_count = atoi( (const char *) line );
		}

		/* Read all lines of the file */
		for ( i=0; i<file_line_count; i++ )
		{
			line = readVimballLine();
			if (line == NULL)
			{
				return;
			}
		}
	}

	vStringDelete (fname);
}

static void findVimTags (void)
{
	const unsigned char *line;
	/* TODO - change this into a structure */

	line = readVimLine();

    if (line == NULL)
    {
            return;
    }

	if ( strncmp ((const char*) line, "UseVimball", (size_t) 10) == 0 )
	{
		parseVimBallFile (line);
	}
	else
	{
		parseVimFile (line);
	}
}

extern parserDefinition* VimParser (void)
{
	static const char *const extensions [] = { "vim", "vba", NULL };
	static const char *const patterns [] = { "vimrc", "[._]vimrc", "gvimrc",
		"[._]gvimrc", NULL };
	parserDefinition* def = parserNew ("Vim");
	def->kinds		= VimKinds;
	def->kindCount	= KIND_COUNT (VimKinds);
	def->extensions = extensions;
	def->patterns   = patterns;
	def->parser		= findVimTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
