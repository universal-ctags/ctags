/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Thanks are due to Jay Glanville for significant improvements.
*
*   This module contains functions for generating tags for user-defined
*   functions for the Vim editor.
*/

/*
 *  INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>
#ifdef DEBUG
#include <stdio.h>
#endif

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#if 0
typedef struct sLineInfo {
	tokenType type;
	keywordId keyword;
	vString *string;
	vString *scope;
	unsigned long lineNumber;
	MIOPos filePosition;
} lineInfo;
#endif

/*
 * DATA DEFINITIONS
 */
typedef enum {
	K_AUGROUP,
	K_COMMAND,
	K_FUNCTION,
	K_MAP,
	K_VARIABLE,
	K_FILENAME,
	K_CONST,
} vimKind;

static kindDefinition VimKinds [] = {
	{ true,  'a', "augroup",  "autocommand groups" },
	{ true,  'c', "command",  "user-defined commands" },
	{ true,  'f', "function", "function definitions" },
	{ true,  'm', "map",      "maps" },
	{ true,  'v', "variable", "variable definitions" },
	{ true,  'n', "filename", "vimball filename" },
	{ true,  'C', "constant", "constant definitions" },
};

/*
 *  DATA DECLARATIONS
 */

#if 0
typedef enum eException {
	ExceptionNone, ExceptionEOF
} exception_t;
#endif

/*
 *  DATA DEFINITIONS
 */

#if 0
static jmp_buf Exception;
#endif

/*
 *  FUNCTION DEFINITIONS
 */

static bool parseVimLine (const unsigned char *line, int infunction);

/* This function takes a char pointer, tries to find a scope separator in the
 * string, and if it does, returns a pointer to the character after the colon,
 * and the character defining the scope.
 * If a colon is not found, it returns the original pointer.
 */
static const unsigned char *skipPrefix (const unsigned char *name, int *scope)
{
	const unsigned char *result = name;
	int counter;
	size_t length;
	length = strlen ((const char *) name);
	if (scope != NULL)
		*scope = '\0';
	if (length > 3 && name[1] == ':')
	{
		if (scope != NULL)
			*scope = *name;
		result = name + 2;
	}
	else if (length > 5 && strncasecmp ((const char *) name, "<SID>", (size_t) 5) == 0)
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
			switch (name[counter])
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
				name[counter] == '_'           ||
				name[counter] == '.'           ||
				name[counter] == '#'
				);
	}
	return result;
}

static bool isWordChar (const unsigned char c)
{
	return (isalnum (c) || c == '_');
}

/* checks if a word at the start of `p` matches at least `min_len` first
 * characters from `word` */
static bool wordMatchLen (const unsigned char *p, const char *const word, size_t min_len)
{
	const unsigned char *w = (const unsigned char *) word;
	size_t n = 0;

	while (*p && *p == *w)
	{
		p++;
		w++;
		n++;
	}

	if (isWordChar (*p))
		return false;

	return n >= min_len;
}

static const unsigned char *skipWord (const unsigned char *p)
{
	while (*p && isWordChar (*p))
		p++;
	return p;
}

static bool isMap (const unsigned char *line)
{
	/*
	 * There are many different short cuts for specifying a map.
	 * This routine should capture all the permutations.
	 */
	return (wordMatchLen (line, "map", 3) ||
			wordMatchLen (line, "nmap", 2) ||
			wordMatchLen (line, "vmap", 2) ||
			wordMatchLen (line, "xmap", 2) ||
			wordMatchLen (line, "smap", 4) ||
			wordMatchLen (line, "omap", 2) ||
			wordMatchLen (line, "imap", 2) ||
			wordMatchLen (line, "lmap", 2) ||
			wordMatchLen (line, "cmap", 2) ||
			wordMatchLen (line, "noremap", 2) ||
			wordMatchLen (line, "nnoremap", 2) ||
			wordMatchLen (line, "vnoremap", 2) ||
			wordMatchLen (line, "xnoremap", 2) ||
			wordMatchLen (line, "snoremap", 4) ||
			wordMatchLen (line, "onoremap", 3) ||
			wordMatchLen (line, "inoremap", 3) ||
			wordMatchLen (line, "lnoremap", 2) ||
			wordMatchLen (line, "cnoremap", 3));
}

static const unsigned char *readVimLine (void)
{
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		while (isspace ((int) *line))
			++line;

		if ((int) *line == '"')
			continue;  /* skip comment */

		break;
	}

	return line;
}

static const unsigned char *readVimballLine (void)
{
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		break;
	}

	return line;
}

static vString *parseSignature (const unsigned char *cp,
								tagEntryInfo *e,
								vString *buf)
{
	/* TODO capture parameters */

	Assert (e);
	Assert (cp);

	if (!buf)
	{
		buf = vStringNew ();
		vStringPut (buf, *cp);
		++cp;
	}

	while (*cp != '\0')
	{
		if (isspace ((int) *cp)
			&& vStringLast (buf) == ',')
		{
			++cp;
			continue;
		}
		vStringPut (buf, *cp);
		if (*cp == ')')
			break;
		++cp;
	}

	if (*cp == ')')
	{
		e->extensionFields.signature = vStringDeleteUnwrap (buf);
		buf = NULL;
	}

	return buf;
}

static void parseFunction (const unsigned char *line)
{
	vString *name = vStringNew ();
	vString *signature = NULL;
	/* bool inFunction = false; */
	int scope;
	const unsigned char *cp = line;
	int index = CORK_NIL;
	tagEntryInfo *e = NULL;

	if (*cp == '!')
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
					scope == 'g'  ||  /* global scope */
					scope == 'd'  ||  /* dictionary */
					scope == 'a')     /* autoload */
			{
				char prefix[3] = { [0] = (char)scope, [1] = ':', [2] = '\0' };
				if (scope == 's')
					vStringCatS (name, prefix);

				do
				{
					vStringPut (name, (int) *cp);
					++cp;
				} while (isalnum ((int) *cp) || *cp == '_' || *cp == '.' || *cp == '#');
				index = makeSimpleTag (name, K_FUNCTION);
				vStringClear (name);

				e = getEntryInCorkQueue (index);
				if (e && isFieldEnabled (FIELD_SIGNATURE))
				{
					while (*cp && isspace ((int) *cp))
						++cp;
					if (*cp == '(')
						signature = parseSignature (cp, e, NULL);
				}
			}
		}
	}

	/* TODO - update struct to indicate inside function */
	while ((line = readVimLine ()) != NULL)
	{
		if (signature)
		{
			cp = line;
			while (*cp && isspace ((int) *cp))
				++cp;
			/* A backslash at the start of a line stands for a line continuation.
			 * https://vimhelp.org/repeat.txt.html#line-continuation */
			if (*cp == '\\')
				signature = parseSignature (++cp, e, signature);
		}

		if (wordMatchLen (line, "endfunction", 4) || wordMatchLen (line, "enddef", 6))
		{
			if (e)
				e->extensionFields.endLine = getInputLineNumber ();
			break;
		}

		parseVimLine (line, true);
	}
	if (signature)
		vStringDelete (signature);
	vStringDelete (name);
}

static void parseAutogroup (const unsigned char *line)
{
	vString *name = vStringNew ();

	/* Found Autocommand Group (augroup) */
	const unsigned char *cp = line;
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp;

		if (*cp)
		{
			const unsigned char *end = skipWord (cp);

			/* "end" (caseless) has a special meaning and should not generate a tag */
			if (end > cp && strncasecmp ((const char *) cp, "end", end - cp) != 0)
			{
				vStringNCatS (name, (const char *) cp, end - cp);
				makeSimpleTag (name, K_AUGROUP);
				vStringClear (name);
			}
		}
	}
	vStringDelete (name);
}

static bool parseCommand (const unsigned char *line)
{
	vString *name = vStringNew ();
	bool cmdProcessed = true;

	/*
	 * Found a user-defined command
	 *
	 * They can have many options preceded by a dash
	 * command! -nargs=+ -complete Select  :call s:DB_execSql("select " . <q-args>)
	 * The name of the command should be the first word not preceded by a dash
	 *
	 */
	const unsigned char *cp = line;

	if (cp && (*cp == '\\'))
	{
		/*
		 * We are recursively calling this function is the command
		 * has been continued on to the next line
		 *
		 * Vim statements can be continued onto a newline using a \
		 * to indicate the previous line is continuing.
		 *
		 * com -nargs=1 -bang -complete=customlist,EditFileComplete
		 *          \ EditFile edit<bang> <args>
		 *
		 * If the following lines do not have a line continuation
		 * the command must not be spanning multiple lines and should
		 * be syntactically incorrect.
		 */
		if (*cp == '\\')
			++cp;

		while (*cp && isspace ((int) *cp))
			++cp;
	}
	else if (line && wordMatchLen (cp, "command", 3))
	{
		cp = skipWord (cp);

		if (*cp == '!')
			++cp;

		if (*cp != ' ')
		{
			/*
			 * :command must be followed by a space.  If it is not, it is
			 * not a valid command.
			 * Treat the line as processed and continue.
			 */
			cmdProcessed = true;
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
		cmdProcessed = false;
		goto cleanUp;
	}

	/*
	 * Strip off any spaces and options which are part of the command.
	 * These should precede the command name.
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
			cmdProcessed = true;
			goto cleanUp;
		}
	} while (*cp &&  !isalnum ((int) *cp));

	if (!*cp)
	{
		/*
		 * We have reached the end of the line without finding the command name.
		 * Read the next line and continue processing it as a command.
		 */
		if ((line = readVimLine ()) != NULL)
			cmdProcessed = parseCommand (line);
		else
			cmdProcessed = false;
		goto cleanUp;
	}

	do
	{
		vStringPut (name, (int) *cp);
		++cp;
	} while (isalnum ((int) *cp) || *cp == '_');

	makeSimpleTag (name, K_COMMAND);
	vStringClear (name);

cleanUp:
	vStringDelete (name);

	return cmdProcessed;
}

static void parseVariableOrConstant (const unsigned char *line, int infunction, int kindIndex)
{
	vString *name = vStringNew ();

	const unsigned char *cp = line;
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
		if (!*cp || *cp == '&' || *cp == '@' || *cp == '[')
			goto cleanUp;

		/*
		 * Ignore vim variables which are read only
		 *    v: - Vim variables.
		 */
		np = cp;
		++np;
		if (*cp == 'v' && *np == ':')
			goto cleanUp;

		/* Skip non-global vars in functions */
		if (infunction && (*np != ':' || *cp != 'g'))
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
		} while (isalnum ((int) *cp) || *cp == '_' || *cp == '#' || *cp == ':' || *cp == '$');
		makeSimpleTag (name, kindIndex);
		vStringClear (name);
	}

cleanUp:
	vStringDelete (name);
}

static bool parseMap (const unsigned char *line)
{
	vString *name = vStringNew ();
	const unsigned char *cp = line;

	if (*cp == '!')
		++cp;

	/*
	 * Maps follow this basic format
	 *    map
	 *    nnoremap <silent> <F8> :Tlist<CR>
	 *    map <unique> <Leader>scdt <Plug>GetColumnDataType
	 *    inoremap ,,, <esc>diwi<<esc>pa><cr></<esc>pa><esc>kA
	 *    inoremap <buffer> ( <C-R>=PreviewFunctionSignature()<LF>
	 *
	 * The Vim help shows the various special arguments available to a map:
	 * 1.2 SPECIAL ARGUMENTS                    *:map-arguments*
	 *    <buffer>
	 *    <nowait>
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

		if (strncmp ((const char *) cp, "<Leader>", (size_t) 8) == 0)
			break;

		if (
				strncmp ((const char *) cp, "<buffer>", (size_t) 8) == 0 ||
				strncmp ((const char *) cp, "<nowait>", (size_t) 8) == 0 ||
				strncmp ((const char *) cp, "<silent>", (size_t) 8) == 0 ||
				strncmp ((const char *) cp, "<script>", (size_t) 8) == 0 ||
				strncmp ((const char *) cp, "<unique>", (size_t) 8) == 0
		   )
		{
			cp += 8;
			continue;
		}

		if (strncmp ((const char *) cp, "<expr>", (size_t) 6) == 0)
		{
			cp += 6;
			continue;
		}

		if (strncmp ((const char *) cp, "<special>", (size_t) 9) == 0)
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

	makeSimpleTag (name, K_MAP);
	vStringClear (name);

	vStringDelete (name);

	return true;
}

static bool parseVimLine (const unsigned char *line, int infunction)
{
	bool readNextLine = true;

	if (wordMatchLen (line, "command", 3))
	{
		readNextLine = parseCommand (line);
		/* TODO - Handle parseCommand returning false */
	}

	else if (isMap (line))
	{
		parseMap (skipWord (line));
	}

	else if (wordMatchLen (line, "function", 2) || wordMatchLen (line, "def", 3))
	{
		parseFunction (skipWord (line));
	}

	else if (wordMatchLen (line, "augroup", 3))
	{
		parseAutogroup (skipWord (line));
	}

	else if (wordMatchLen (line, "let", 3))
	{
		parseVariableOrConstant (skipWord (line), infunction, K_VARIABLE);
	}
	else if (wordMatchLen (line, "const", 4))
	{
		parseVariableOrConstant (skipWord (line), infunction, K_CONST);
	}

	return readNextLine;
}

static void parseVimFile (const unsigned char *line)
{
	bool readNextLine = true;

	while (line != NULL)
	{
		readNextLine = parseVimLine (line, false);

		if (readNextLine)
			line = readVimLine ();

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
	line = readVimLine ();

	while (line != NULL)
	{
		/* Next line should be a filename */
		line = readVimLine ();
		if (line == NULL)
		{
			goto cleanUp;
		}
		else
		{
			cp = line;
			do
			{
				vStringPut (fname, (int) *cp);
				++cp;
			} while (isalnum ((int) *cp) || *cp == '.' || *cp == '/' || *cp == '\\');
			makeSimpleTag (fname, K_FILENAME);
			vStringClear (fname);
		}

		file_line_count = 0;
		/* Next line should be the line count of the file */
		line = readVimLine ();
		if (line == NULL)
		{
			goto cleanUp;
		}
		else
		{
			file_line_count = atoi ((const char *) line);
		}

		/* Read all lines of the file */
		for (i = 0; i < file_line_count; i++)
		{
			line = readVimballLine ();
			if (line == NULL)
			{
				goto cleanUp;
			}
		}
	}

cleanUp:
	vStringDelete (fname);
}

static void findVimTags (void)
{
	const unsigned char *line;
	/* TODO - change this into a structure */

	line = readVimLine ();

	if (line == NULL)
	{
			return;
	}

	if (strncmp ((const char *) line, "UseVimball", (size_t) 10) == 0)
	{
		parseVimBallFile (line);
	}
	else
	{
		parseVimFile (line);
	}
}

extern parserDefinition *VimParser (void)
{
	static const char *const extensions [] = { "vim", "vba", NULL };
	static const char *const patterns [] = { "vimrc", "[._]vimrc", "gvimrc",
		"[._]gvimrc", NULL };
	parserDefinition *def = parserNew ("Vim");
	def->kindTable      = VimKinds;
	def->kindCount  = ARRAY_SIZE (VimKinds);
	def->extensions = extensions;
	def->patterns   = patterns;
	def->parser     = findVimTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
