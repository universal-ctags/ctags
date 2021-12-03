/*
*   Copyright (c) 2001-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for the Pascal language,
*   including some extensions for Object Pascal.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION, K_PROCEDURE
} pascalKind;

static kindDefinition PascalKinds [] = {
	{ true, 'f', "function",  "functions"},
	{ true, 'p', "procedure", "procedures"}
};

/*
*   FUNCTION DEFINITIONS
*/

static void createPascalTag (
		tagEntryInfo* const tag, const vString* const name, const int kind,
		const vString *arglist, const vString *vartype)
{
	if (PascalKinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
		initTagEntry (tag, vStringValue (name), kind);
		if (arglist && !vStringIsEmpty (arglist))
		{
			tag->extensionFields.signature = vStringValue (arglist);
		}
		if (vartype && !vStringIsEmpty (vartype))
		{
			tag->extensionFields.typeRef[0] = "typename";
			tag->extensionFields.typeRef[1] = vStringValue (vartype);
		}
	}
	else
		/* TODO: Passing NULL as name makes an assertion behind initTagEntry failure */
		initTagEntry (tag, NULL, KIND_GHOST_INDEX);
}

static void makePascalTag (const tagEntryInfo* const tag)
{
	if (tag->name != NULL)
		makeTagEntry (tag);
}

static const unsigned char* dbp;

#define starttoken(c) (isalpha ((int) c) || (int) c == '_')
#define intoken(c)    (isalnum ((int) c) || (int) c == '_' || (int) c == '.')
#define endtoken(c)   (! intoken (c)  &&  ! isdigit ((int) c))

static bool tail (const char *cp)
{
	bool result = false;
	register int len = 0;

	while (*cp != '\0' && tolower ((int) *cp) == tolower ((int) dbp [len]))
		cp++, len++;
	if (*cp == '\0' && !intoken (dbp [len]))
	{
		dbp += len;
		result = true;
	}
	return result;
}

static void parseArglist (const char *buf, vString *arglist, vString *vartype)
{
	const char *start, *end;
	int level;

	if (NULL == buf || arglist == NULL)
		return;

	/* parse argument list which can be missing like in "function ginit:integer;" */
	if (NULL != (start = strchr (buf, '(')))
	{
		for (level = 1, end = start + 1; level > 0; ++end)
		{
			if ('\0' == *end)
				break;
			else if ('(' == *end)
				++ level;
			else if (')' == *end)
				-- level;
		}
	}
	else /* if no argument list was found, continue looking for a return value */
	{
		start = NULL;
		end = buf;
	}

	/* parse return type if requested by passing a non-NULL vartype argument */
	if (NULL != vartype)
	{
		char *var, *var_start;

		if (NULL != (var = strchr (end, ':')))
		{
			var++; /* skip ':' */
			while (isspace ((int) *var))
				++var;

			if (starttoken (*var))
			{
				var_start = var;
				var++;
				while (intoken (*var))
					var++;
				if (endtoken (*var))
				{
					vStringNCatS (vartype, var_start, var - var_start);
				}
			}
		}
	}

	if (NULL == start) /* no argument list */
		vStringCatS (arglist, "()");
	else
		vStringNCatS (arglist, start, end - start);
}

/* Algorithm adapted from from GNU etags.
 * Locates tags for procedures & functions.  Doesn't do any type- or
 * var-definitions.  It does look for the keyword "extern" or "forward"
 * immediately following the procedure statement; if found, the tag is
 * skipped.
 */
static void findPascalTags (void)
{
	vString *name = vStringNew ();
	vString *arglist = vStringNew ();
	vString *vartype = vStringNew ();
	tagEntryInfo tag;
	pascalKind kind = K_FUNCTION;
		/* each of these flags is true iff: */
	bool incomment = false;  /* point is inside a comment */
	int comment_char = '\0';    /* type of current comment */
	bool inquote = false;    /* point is inside '..' string */
	bool get_tagname = false;/* point is after PROCEDURE/FUNCTION
		keyword, so next item = potential tag */
	bool found_tag = false;  /* point is after a potential tag */
	bool inparms = false;    /* point is within parameter-list */
	bool verify_tag = false;
		/* point has passed the parm-list, so the next token will determine
		 * whether this is a FORWARD/EXTERN to be ignored, or whether it is a
		 * real tag
		 */

	dbp = readLineFromInputFile ();
	while (dbp != NULL)
	{
		int c = *dbp++;

		if (c == '\0')  /* if end of line */
		{
			dbp = readLineFromInputFile ();
			if (dbp == NULL  ||  *dbp == '\0')
				continue;
			if (!((found_tag && verify_tag) || get_tagname))
				c = *dbp++;
					/* only if don't need *dbp pointing to the beginning of
					 * the name of the procedure or function
					 */
		}
		if (incomment)
		{
			if (comment_char == '{' && c == '}')
				incomment = false;
			else if (comment_char == '(' && c == '*' && *dbp == ')')
			{
				dbp++;
				incomment = false;
			}
			continue;
		}
		else if (inquote)
		{
			if (c == '\'')
				inquote = false;
			continue;
		}
		else switch (c)
		{
			case '\'':
				inquote = true;  /* found first quote */
				continue;
			case '{':  /* found open { comment */
				incomment = true;
				comment_char = c;
				continue;
			case '(':
				if (*dbp == '*')  /* found open (* comment */
				{
					incomment = true;
					comment_char = c;
					dbp++;
				}
				else if (found_tag)  /* found '(' after tag, i.e., parm-list */
					inparms = true;
				continue;
			case ')':  /* end of parms list */
				if (inparms)
					inparms = false;
				continue;
			case ';':
				if (found_tag && !inparms)  /* end of proc or fn stmt */
				{
					verify_tag = true;
					break;
				}
				continue;
		}
		if (found_tag && verify_tag && *dbp != ' ')
		{
			/* check if this is an "extern" declaration */
			if (*dbp == '\0')
				continue;
			if (tolower ((int) *dbp == 'e'))
			{
				if (tail ("extern"))  /* superfluous, really! */
				{
					found_tag = false;
					verify_tag = false;
				}
			}
			else if (tolower ((int) *dbp) == 'f')
			{
				if (tail ("forward"))  /*  check for forward reference */
				{
					found_tag = false;
					verify_tag = false;
				}
			}
			if (found_tag && verify_tag)  /* not external proc, so make tag */
			{
				found_tag = false;
				verify_tag = false;
				makePascalTag (&tag);
				continue;
			}
		}
		if (get_tagname)  /* grab name of proc or fn */
		{
			const unsigned char *cp;

			if (*dbp == '\0')
				continue;

			/* grab block name */
			while (isspace ((int) *dbp))
				++dbp;
			if (!starttoken(*dbp))
				continue;
			for (cp = dbp  ;  *cp != '\0' && !endtoken (*cp)  ;  cp++)
				continue;
			vStringNCopyS (name, (const char*) dbp,  cp - dbp);

			vStringClear (arglist);
			vStringClear (vartype);
			parseArglist ((const char*) cp, arglist, (kind == K_FUNCTION) ? vartype : NULL);

			createPascalTag (&tag, name, kind, arglist, (kind == K_FUNCTION) ? vartype : NULL);
			dbp = cp;  /* set dbp to e-o-token */
			get_tagname = false;
			found_tag = true;
			/* and proceed to check for "extern" */
		}
		else if (!incomment && !inquote && !found_tag)
		{
			switch (tolower ((int) c))
			{
				case 'c':
					if (tail ("onstructor"))
					{
						get_tagname = true;
						kind = K_PROCEDURE;
					}
					break;
				case 'd':
					if (tail ("estructor"))
					{
						get_tagname = true;
						kind = K_PROCEDURE;
					}
					break;
				case 'p':
					if (tail ("rocedure"))
					{
						get_tagname = true;
						kind = K_PROCEDURE;
					}
					break;
				case 'f':
					if (tail ("unction"))
					{
						get_tagname = true;
						kind = K_FUNCTION;
					}
					break;
			}
		}  /* while not eof */
	}
	vStringDelete (arglist);
	vStringDelete (vartype);
	vStringDelete (name);
}

extern parserDefinition* PascalParser (void)
{
	static const char *const extensions [] = { "p", "pas", NULL };
	parserDefinition* def = parserNew ("Pascal");
	def->extensions = extensions;
	def->kindTable      = PascalKinds;
	def->kindCount  = ARRAY_SIZE (PascalKinds);
	def->parser     = findPascalTags;
	return def;
}
