/*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for scripts for the
*   Bourne shell (and its derivatives, the Korn and Z shells).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "kind.h"
#include "parse.h"
#include "read.h"
#include "promise.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_NOTHING = -1,		/* place holder. Never appears on tags file. */
	K_ALIAS,
	K_FUNCTION,
	K_SOURCE,
	K_HEREDOCLABEL,
} shKind;

typedef enum {
	R_SCRIPT_LOADED,
} shScriptRole;

static roleDefinition ShScriptRoles [] = {
	{ true, "loaded", "loaded" },
};

typedef enum {
	R_HEREDOC_ENDMARKER,
} shHeredocRole;

static roleDefinition ShHeredocRoles [] = {
	{ true, "endmarker", "end marker" },
};

static kindDefinition ShKinds [] = {
	{ true, 'a', "alias", "aliases"},
	{ true, 'f', "function", "functions"},
	{ true, 's', "script", "script files",
	  .referenceOnly = true, ATTACH_ROLES (ShScriptRoles) },
	{ true, 'h', "heredoc", "label for here document",
	  .referenceOnly = false, ATTACH_ROLES (ShHeredocRoles) },
};

/*
*   FUNCTION DEFINITIONS
*/

static bool isFileChar  (int c)
{
	return (isalnum (c)
		|| c == '_' || c == '-'
		|| c == '/' || c == '.'
		|| c == '+' || c == '^'
		|| c == '%' || c == '@'
		|| c == '~');
}

static bool isIdentChar (int c)
{
	return (isalnum (c) || c == '_' || c == '-');
}

/* bash allows all kinds of crazy stuff as the identifier after 'function' */
static bool isBashFunctionChar (int c)
{
	return (c > 1 /* NUL and SOH are disallowed */ && c != 0x7f &&
	        /* blanks are disallowed, but VT and FF (and CR to some extent, but
	         * let's not fall into the pit of craziness) */
	        c != ' ' && c != '\t' && c != '\n' && c != '\r' &&
	        c != '"' && c != '\'' && c != '$' && c != '`' && c != '\\' &&
	        c != '&' && c != ';' &&
	        c != '(' && c != ')' &&
	        c != '<' && c != '>');
}

static const unsigned char *skipDoubleString (const unsigned char *cp)
{
	const unsigned char* prev = cp;
	cp++;
	while ((*cp != '"' || *prev == '\\') && *cp != '\0')
	{
		prev = cp;
		cp++;
	}
	return cp;
}

static const unsigned char *skipSingleString (const unsigned char *cp)
{
	cp++;
	while (*cp != '\'' && *cp != '\0')
		cp++;
	return cp;
}

static bool isEnvCommand (const vString *cmd)
{
	const char *lc = vStringValue(cmd);
	const char * tmp = baseFilename (lc);

	return (strcmp(tmp, "env") == 0);
}

static int readDestfileName (const unsigned char *cp, vString *destfile)
{
	const unsigned char *origin = cp;

	while (isspace ((int) *cp))
		++cp;

	/* >... */
	if (*cp != '>')
		return 0;

	/* >>... */
	if (*cp == '>')
		++cp;

	while (isspace ((int) *cp))
		++cp;

	if (!isFileChar ((int) *cp))
		return 0;

	vStringClear(destfile);
	do {
		vStringPut (destfile, (int) *cp);
		++cp;
	} while (isFileChar ((int) *cp));

	if (vStringLength(destfile) > 0)
		return cp - origin;

	return 0;
}

struct hereDocParsingState {
	vString *args[2];
	vString *destfile;
	langType sublang;
	unsigned long startLine;

	int corkIndex;
};

static void hdocStateInit (struct hereDocParsingState *hstate)
{
	hstate->args[0] = vStringNew ();
	hstate->args[1] = vStringNew ();
	hstate->destfile = vStringNew ();

	hstate->corkIndex = CORK_NIL;
	hstate->sublang = LANG_IGNORE;
}

static void hdocStateClear (struct hereDocParsingState *hstate)
{
	vStringClear (hstate->args[0]);
	vStringClear (hstate->args[1]);
	vStringClear (hstate->destfile);
}

static void hdocStateFini (struct hereDocParsingState *hstate)
{
	vStringDelete (hstate->args[0]);
	vStringDelete (hstate->args[1]);
	vStringDelete (hstate->destfile);
}

static void hdocStateUpdateArgs (struct hereDocParsingState *hstate,
										   vString *name)
{
	if (vStringIsEmpty(hstate->args[0]))
		vStringCopy(hstate->args[0], name);
	else if (vStringIsEmpty(hstate->args[1]))
		vStringCopy(hstate->args[1], name);
}

static void hdocStateMakePromiseMaybe (struct hereDocParsingState *hstate)
{
	if (hstate->sublang != LANG_IGNORE)
		makePromise (getLanguageName(hstate->sublang),
					 hstate->startLine, 0,
					 getInputLineNumber(), 0,
					 0);
	hstate->sublang = LANG_IGNORE;
}

static void hdocStateRecordStartlineFromDestfileMaybe (struct hereDocParsingState *hstate)
{
	const char *f = vStringValue(hstate->destfile);

	if (hstate->sublang != LANG_IGNORE)
		return;

	hstate->sublang = getLanguageForFilename (f, 0);
	if (hstate->sublang != LANG_IGNORE)
		hstate->startLine = getInputLineNumber () + 1;
	vStringClear (hstate->destfile);
}

static void hdocStateRecordStatelineMaybe (struct hereDocParsingState *hstate)
{
	if (!vStringIsEmpty(hstate->args[0]))
	{
		const char *cmd;

		cmd = vStringValue(hstate->args[0]);
		if (isEnvCommand (hstate->args[0]))
		{
			cmd = NULL;
			if (!vStringIsEmpty(hstate->args[1]))
				cmd = vStringValue(hstate->args[1]);
		}

		if (cmd)
		{
			hstate->sublang = getLanguageForCommand (cmd, 0);
			if (hstate->sublang != LANG_IGNORE)
				hstate->startLine = getInputLineNumber () + 1;
		}
	}

	if (vStringLength(hstate->destfile) > 0)
		hdocStateRecordStartlineFromDestfileMaybe (hstate);
}

static int hdocStateReadDestfileName (struct hereDocParsingState *hstate,
									  const unsigned char* cp,
									  const vString *const hereDocDelimiter)
{
	int d = readDestfileName (cp, hstate->destfile);

	if (d > 0 && hereDocDelimiter)
		hdocStateRecordStartlineFromDestfileMaybe (hstate);

	return d;
}

static void hdocStateUpdateTag (struct hereDocParsingState *hstate, unsigned long endLine)
{
	tagEntryInfo *tag = getEntryInCorkQueue (hstate->corkIndex);
	if (tag)
	{
		tag->extensionFields.endLine = endLine;
		hstate->corkIndex = CORK_NIL;
	}
}

static void findShTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;
	vString *hereDocDelimiter = NULL;
	bool hereDocIndented = false;
	bool (* check_char)(int);

	struct hereDocParsingState hstate;
	hdocStateInit (&hstate);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;
		shKind found_kind = K_NOTHING;

		if (hereDocDelimiter)
		{
			if (hereDocIndented)
			{
				while (*cp == '\t')
					cp++;
			}
			if ((strncmp ((const char *) cp, vStringValue (hereDocDelimiter), vStringLength (hereDocDelimiter)) == 0)
				&& ((*(cp + vStringLength (hereDocDelimiter)) == '\0')
					|| isspace (*(cp + vStringLength (hereDocDelimiter)) )))
			{
				hdocStateUpdateTag (&hstate, getInputLineNumber ());
				hdocStateMakePromiseMaybe (&hstate);

				if (!vStringIsEmpty(hereDocDelimiter))
					makeSimpleRefTag(hereDocDelimiter, K_HEREDOCLABEL, R_HEREDOC_ENDMARKER);
				vStringDelete (hereDocDelimiter);
				hereDocDelimiter = NULL;
			}
			continue;
		}

		hdocStateClear (&hstate);
		while (*cp != '\0')
		{
			/* jump over whitespace */
			while (isspace ((int)*cp))
				cp++;

			/* jump over strings */
			if (*cp == '"')
				cp = skipDoubleString (cp);
			else if (*cp == '\'')
				cp = skipSingleString (cp);
			/* jump over comments */
			else if (*cp == '#')
				break;
			/* jump over here-documents */
			else if (cp[0] == '<' && cp[1] == '<')
			{
				const unsigned char *start, *end;
				bool trimEscapeSequences = false;
				bool quoted = false;
				cp += 2;
				/* an optional "-" strips leading tabulations from the heredoc lines */
				if (*cp != '-')
					hereDocIndented = false;
				else
				{
					hereDocIndented = true;
					cp++;
				}
				while (isspace (*cp))
					cp++;
				start = end = cp;
				/* the delimiter can be surrounded by quotes */
				if (*cp == '"')
				{
					start++;
					end = cp = skipDoubleString (cp);
					/* we need not to worry about variable substitution, they
					 * don't happen in heredoc delimiter definition */
					trimEscapeSequences = true;
					quoted = true;
				}
				else if (*cp == '\'')
				{
					start++;
					end = cp = skipSingleString (cp);
					quoted = true;
				}
				else
				{
					while (isIdentChar ((int) *cp))
						cp++;
					end = cp;
				}
				if (end > start || quoted)
				{
					/* The input may be broken as a shell script but we need to avoid
					   memory leaking. */
					if (hereDocDelimiter)
						vStringClear(hereDocDelimiter);
					else
						hereDocDelimiter = vStringNew ();
					for (; end > start; start++)
					{
						if (trimEscapeSequences && *start == '\\')
							start++;
						vStringPut (hereDocDelimiter, *start);
					}
					if (vStringLength(hereDocDelimiter) > 0)
						hstate.corkIndex = makeSimpleTag(hereDocDelimiter, K_HEREDOCLABEL);

					hdocStateRecordStatelineMaybe(&hstate);
				}
			}

			check_char = isBashFunctionChar;

			if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  &&
				isspace ((int) cp [8]))
			{
				found_kind = K_FUNCTION;
				cp += 8;
			}
			else if (strncmp ((const char*) cp, "alias", (size_t) 5) == 0  &&
				isspace ((int) cp [5]))
			{
				check_char = isIdentChar;
				found_kind = K_ALIAS;
				cp += 5;
			}
			else if (cp [0] == '.'
				    && isspace((int) cp [1]))
			{
				found_kind = K_SOURCE;
				++cp;
				check_char = isFileChar;
			}
			else if (strncmp ((const char*) cp, "source", (size_t) 6) == 0
					 && isspace((int) cp [6]))
			{
				found_kind = K_SOURCE;
				cp += 6;
				check_char = isFileChar;
			}

			if (found_kind != K_NOTHING)
				while (isspace ((int) *cp))
					++cp;

			// Get the name of the function, alias or file to be read by source
			if (! check_char ((int) *cp))
			{
				found_kind = K_NOTHING;

				int d = hdocStateReadDestfileName (&hstate, cp,
												   hereDocDelimiter);
				if (d > 0)
					cp += d;
				else if (*cp != '\0')
					++cp;
				continue;
			}
			while (check_char ((int) *cp))
			{
				vStringPut (name, (int) *cp);
				++cp;
			}

			while (isspace ((int) *cp))
				++cp;

			if ((found_kind != K_SOURCE)
			    && *cp == '(')
			{
				++cp;
				while (isspace ((int) *cp))
					++cp;
				if (*cp == ')')
				{
					found_kind = K_FUNCTION;
					++cp;
				}
			}

			if (found_kind != K_NOTHING)
			{
				if (found_kind == K_SOURCE)
						makeSimpleRefTag (name, K_SOURCE, R_SCRIPT_LOADED);
				else
					makeSimpleTag (name, found_kind);
				found_kind = K_NOTHING;
			}
			else if (!hereDocDelimiter)
				hdocStateUpdateArgs (&hstate, name);
			vStringClear (name);
		}
	}
	hdocStateFini (&hstate);
	vStringDelete (name);
	if (hereDocDelimiter)
		vStringDelete (hereDocDelimiter);
}

extern parserDefinition* ShParser (void)
{
	static const char *const extensions [] = {
		"sh", "SH", "bsh", "bash", "ksh", "zsh", "ash", NULL
	};
	static const char *const aliases [] = {
		"sh", "bash", "ksh", "zsh", "ash",
		/* major mode name in emacs */
		"shell-script",
		NULL
	};
	parserDefinition* def = parserNew ("Sh");
	def->kindTable      = ShKinds;
	def->kindCount  = ARRAY_SIZE (ShKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findShTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
