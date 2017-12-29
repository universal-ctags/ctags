/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2017, Masatake YAMATO
*   Copyright (c) 2017, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for TCL scripts.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "tokeninfo.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "keyword.h"
#include "entry.h"
#include "routines.h"
#include "debug.h"
#include "ptrarray.h"
#include "tcl.h"

#include <string.h>



/*
*   DATA DEFINITIONS
*/

static scopeSeparator TclGenericSeparators [] = {
	{ KIND_WILDCARD_INDEX, "::" },
};

typedef enum {
	K_PROCEDURE, K_NAMESPACE
} tclKind;

static kindDefinition TclKinds [] = {
	{ true, 'p', "procedure", "procedures",
	  ATTACH_SEPARATORS(TclGenericSeparators)},
	{ true, 'n', "namespace", "namespaces",
	  ATTACH_SEPARATORS(TclGenericSeparators)},
};

enum {
	KEYWORD_PROC,
	KEYWORD_NAMESPACE,
	KEYWORD_EVAL,
	KEYWORD_PACKAGE,
};

typedef int keywordId; /* to allow KEYWORD_NONE */


static const keywordTable TclKeywordTable[] = {
	/* keyword			keyword ID */
	{ "proc",			KEYWORD_PROC		},
	{ "namespace",		KEYWORD_NAMESPACE	},
	{ "eval",			KEYWORD_EVAL		},
	{ "package",        KEYWORD_PACKAGE     },
};

/*
*   FUNCTION DEFINITIONS
*/


static void initToken (tokenInfo *token, void *data);
static void readToken (tokenInfo *const token, void *data);
static void clearToken (tokenInfo *token);
static void copyToken (tokenInfo *dest, tokenInfo *src, void *data CTAGS_ATTR_UNUSED);

struct sTclParserState {
	enum TclTokenType lastTokenType;
};

#define TOKEN_PSTATE(TOKEN) \
	TOKENX(TOKEN, struct tokenExtra)->pstate

struct tokenExtra {
	int scopeIndex;
	struct sTclParserState *pstate;
};

struct tokenTypePair typePairs [] = {
	{ '{', '}' },
	{ '[', ']' },
};


static struct tokenInfoClass tclTokenInfoClass = {
	.nPreAlloc = 4,
	.typeForUndefined = TOKEN_TCL_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_TCL_KEYWORD,
	.typeForEOF       = TOKEN_TCL_EOF,
	.extraSpace       = sizeof (struct tokenExtra),
	.pairs            = typePairs,
	.pairCount        = ARRAY_SIZE (typePairs),
	.init             = initToken,
	.read             = readToken,
	.clear            = clearToken,
	.copy             = copyToken,
};

extern tokenInfo *newTclToken (void *pstate)
{
	return newTokenFull (&tclTokenInfoClass, pstate);
}

static void clearToken (tokenInfo *token)
{
	TOKENX (token, struct tokenExtra)->scopeIndex = CORK_NIL;
	TOKENX (token, struct tokenExtra)->pstate = NULL;
}

static void copyToken (tokenInfo *dest, tokenInfo *src, void *data CTAGS_ATTR_UNUSED)
{
	TOKENX (dest, struct tokenExtra)->scopeIndex =
		TOKENX (src, struct tokenExtra)->scopeIndex;
	TOKENX (dest, struct tokenExtra)->pstate =
		TOKENX (src, struct tokenExtra)->pstate;
}

static void readString (vString *string)
{
	int c;
	bool escaped = false;

	while (1)
	{
		c = getcFromInputFile ();
		switch (c)
		{
		case EOF:
			return;
		case '\\':
			vStringPut (string, c);
			escaped = true;
			break;
		case '"':
			vStringPut (string, c);
			if (escaped)
				escaped = false;
			else
				return;
			break;
		default:
			escaped = false;
			vStringPut (string, c);
			break;
		}
	}
}

static void readIdentifier (vString *string)
{
	while (1)
	{
		int c = getcFromInputFile ();
		if (isgraph (c) && (!strchr ("{}[]", c)))
			vStringPut (string, c);
		else
		{
			ungetcToInputFile (c);
			break;
		}
	}
}

static keywordId resolveKeyword (vString *string)
{
	char *s = vStringValue (string);
	static langType lang = LANG_AUTO;

	if (lang == LANG_AUTO)
		lang = getInputLanguage ();

	return lookupKeyword (s, lang);
}

static void initToken (tokenInfo *token, void *data)
{
	TOKENX (token, struct tokenExtra)->pstate = data;
}

static void readToken0 (tokenInfo *const token, struct sTclParserState *pstate)
{
	int c = EOF;
	bool escaped;
	bool bol = (pstate->lastTokenType == TOKEN_TCL_EOL
				|| pstate->lastTokenType == ';'
				|| pstate->lastTokenType == TOKEN_TCL_UNDEFINED);
	token->type		= TOKEN_TCL_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

 getNextChar:
	escaped = false;

	do {
		c = getcFromInputFile ();
	} while (c == ' ' || c== '\t' || c == '\f');

	if (c == '\\')
	{
		bol = false;
		int c0 = getcFromInputFile ();
		switch (c0)
		{
		case '\n':
		case '\r':
			goto getNextChar;
		default:
			escaped = true;
			c = c0;
			break;
		}
	}

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
	case EOF:
		token->type = TOKEN_TCL_EOF;
		break;
	case '\n':
	case '\r':
		token->type = TOKEN_TCL_EOL;
		break;
	case '#':
		if (!escaped)
		{
			if (bol)
			{
				do
					c = getcFromInputFile ();
				while (c != EOF && c != '\r' && c != '\n');
			}
			goto getNextChar;
		}
	case '"':
		if (!escaped)
		{
			token->type = TOKEN_TCL_STRING;
			vStringPut (token->string, c);
			readString (token->string);
			break;
		}
	case ';':
	case '{':
	case '}':
	case '[':
	case ']':
		if (!escaped)
		{
			token->type = c;
			break;
		}
	case '$':
		if (!escaped)
		{
			vStringPut (token->string, c);
			token->type = TOKEN_TCL_VARIABLE;

			int c0 = getcFromInputFile ();
			if (c0 == EOF)
				break;

			vStringPut (token->string, c0);
			if (c0 == '{')
			{

				while ((c0 = getcFromInputFile ()) != EOF)
				{
					vStringPut (token->string, c0);
					if (c0 == '}')
						break;
				}
			}
			else
				readIdentifier (token->string);
			break;
		}
	default:
			vStringPut (token->string, c);
			readIdentifier (token->string);

			token->keyword = resolveKeyword (token->string);
			if (token->keyword == KEYWORD_NONE)
				token->type = TOKEN_TCL_IDENTIFIER;
			else
				token->type = TOKEN_TCL_KEYWORD;
			break;
	}
}

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED)
{
	struct sTclParserState *pstate = TOKEN_PSTATE(token);

	readToken0 (token, pstate);

	pstate->lastTokenType = token->type;
}

static bool tokenIsEOL (tokenInfo *const token)
{
	if (token->type == ';'
		|| tokenIsType (token, TCL_EOL)
		|| tokenIsEOF (token))
		return true;
	return false;
}

static void skipToEndOfCmdline (tokenInfo *const token)
{
	while (!tokenIsEOL (token))
	{
		if ((token->type == '{')
			|| (token->type == '['))
			tokenSkipOverPair(token);
		tokenRead (token);
	}
}

extern void skipToEndOfTclCmdline (tokenInfo *const token)
{
	skipToEndOfCmdline (token);
}

static bool isAbsoluteIdentifier(tokenInfo *const token)
{
	return !strncmp (vStringValue (token->string), "::", 2);
}

static const char* getLastComponentInIdentifier(tokenInfo *const token)
{
	const char* s = vStringValue (token->string);
	char *last = strrstr(s, "::");

	if (last)
		return last + 2;
	else
		return NULL;
}

static void notifyPackageRequirement (tokenInfo *const token)
{
	subparser *sub;

	foreachSubparser (sub, false)
	{
		tclSubparser *tclsub = (tclSubparser *)sub;

		if (tclsub->packageRequirementNotify)
		{
			enterSubparser(sub);
			tclsub->packageRequirementNotify (tclsub, vStringValue (token->string),
											  TOKEN_PSTATE(token));
			leaveSubparser();
		}
	}
}

static void notifyNamespaceImport (tokenInfo *const token)
{
	subparser *sub;

	foreachSubparser (sub, false)
	{
		tclSubparser *tclsub = (tclSubparser *)sub;

		if (tclsub->namespaceImportNotify)
		{
			enterSubparser(sub);
			tclsub->namespaceImportNotify (tclsub, vStringValue (token->string),
										   TOKEN_PSTATE(token));
			leaveSubparser();
		}
	}
}

static int notifyCommand (tokenInfo *const token, unsigned int parent)
{
	subparser *sub;
	int r = CORK_NIL;

	foreachSubparser (sub, false)
	{
		tclSubparser *tclsub = (tclSubparser *)sub;

		if (tclsub->commandNotify)
		{
			enterSubparser(sub);
			r = tclsub->commandNotify (tclsub, vStringValue (token->string), parent,
									   TOKEN_PSTATE(token));
			leaveSubparser();
			if (r != CORK_NIL)
				break;
		}
	}
	return r;
}

static void parseProc (tokenInfo *const token,
					   unsigned int parent)
{
	int index = CORK_NIL;
	int index_fq = CORK_NIL;

	tokenRead (token);

	if (tokenIsType(token, TCL_IDENTIFIER))
	{
		const char *last = getLastComponentInIdentifier (token);
		if (last)
		{
			tagEntryInfo e;

			initTagEntry (&e, last, K_PROCEDURE);
			e.lineNumber = token->lineNumber;
			e.filePosition = token->filePosition;

			int len  = (last - vStringValue (token->string));
			vString *ns = vStringNew();
			if (isAbsoluteIdentifier (token))
			{
				if (len > 2)
					vStringNCopy (ns, token->string, len - 2);
			}
			else if (parent == CORK_NIL)
				vStringCopy (ns, token->string);
			else
			{
				tagEntryInfo *e_parent = getEntryInCorkQueue (parent);
				vStringCatS(ns, e_parent->name);
				vStringCatS(ns, "::");
				vStringNCopy(ns, token->string, len - 2);
			}


			if (vStringLength(ns) > 0)
			{
				e.extensionFields.scopeKindIndex = K_NAMESPACE;
				e.extensionFields.scopeName = vStringValue (ns);
			}

			index = makeTagEntry (&e);

			if (isXtagEnabled(XTAG_QUALIFIED_TAGS))
			{
				vStringCatS (ns, "::");
				vStringCatS (ns, last);

				index_fq = makeSimpleTag (ns, K_PROCEDURE);
				tagEntryInfo *e_fq = getEntryInCorkQueue (index_fq);
				markTagExtraBit (e_fq, XTAG_QUALIFIED_TAGS);
			}
			vStringDelete (ns);
		}
		else
		{
			tagEntryInfo *ep;
			index = makeSimpleTag (token->string, K_PROCEDURE);
			ep = getEntryInCorkQueue (index);
			ep->extensionFields.scopeIndex = parent;
		}
	}

	if (!tokenIsEOL (token))
	{
		tokenRead (token);
		skipToEndOfCmdline(token);
	}

	if (index != CORK_NIL)
	{
		tagEntryInfo *e;

		e = getEntryInCorkQueue (index);
		e->extensionFields.endLine = token->lineNumber;

		if (index_fq != CORK_NIL)
		{
			e = getEntryInCorkQueue (index_fq);
			e->extensionFields.endLine = token->lineNumber;
		}
	}
}

static void parseNamespace (tokenInfo *const token,
							unsigned int parent)
{
	tokenRead (token);
	if (tokenIsEOF(token))
		return;

	if (tokenIsType (token, TCL_IDENTIFIER) &&
		(strcmp(tokenString(token), "import") == 0))
	{
		while (1)
		{
			tokenRead (token);

			if (!tokenIsType (token, TCL_IDENTIFIER))
				break;

			if (vStringValue(token->string)[0] == '-')
				continue;

			notifyNamespaceImport (token);
		}
		skipToEndOfCmdline(token);
		return;
	}
	else if (!tokenIsKeyword (token, EVAL))
		return;

	tokenRead (token);
	if (!tokenIsType (token, TCL_IDENTIFIER))
	{
		skipToEndOfCmdline(token);
		return;
	}

	int index = makeSimpleTag (token->string, K_NAMESPACE);
	if (parent != CORK_NIL && strncmp(vStringValue (token->string), "::", 2))
	{
		tagEntryInfo *e = getEntryInCorkQueue (index);
		e->extensionFields.scopeIndex = parent;
	}

	tokenRead (token);
	if (token->type != '{')
	{
		skipToEndOfCmdline(token);
		return;
	}

	do {
		tokenRead (token);
		if (tokenIsKeyword (token, NAMESPACE))
			parseNamespace (token, index);
		else if (tokenIsKeyword (token, PROC))
			parseProc (token, index);
		else if (tokenIsType (token, TCL_IDENTIFIER))
		{
			notifyCommand (token, index);
			skipToEndOfCmdline(token); /* ??? */
		}
		else if (token->type == '}')
		{
			tagEntryInfo *e = getEntryInCorkQueue (index);
			e->extensionFields.endLine = token->lineNumber;
			break;
		}
		else
			skipToEndOfCmdline(token);
	} while (!tokenIsEOF(token));
}

static void parsePackage (tokenInfo *const token)
{
	tokenRead (token);
	if (tokenIsType (token, TCL_IDENTIFIER)
		&& (strcmp (vStringValue (token->string), "require") == 0))
	{
	next:
		tokenRead (token);
		if (tokenIsType (token, TCL_IDENTIFIER)
			&& (vStringLength (token->string) > 0))
		{
			if (vStringValue(token->string)[0] == '-')
				goto next;

			if (tokenIsType (token, TCL_IDENTIFIER)
				&& (vStringLength (token->string) > 0))
			{
				notifyPackageRequirement (token);
			}
		}
	}
	skipToEndOfCmdline(token);
}


static void findTclTags (void)
{
	struct sTclParserState pstate = {
		.lastTokenType = TOKEN_TCL_UNDEFINED,
	};
	tokenInfo *const token = newTclToken (&pstate);

	do {
		tokenRead (token);
		if (tokenIsKeyword (token, NAMESPACE))
			parseNamespace (token, CORK_NIL);
		else if (tokenIsKeyword (token, PROC))
			parseProc (token, CORK_NIL);
		else if (tokenIsKeyword (token, PACKAGE))
			parsePackage (token);
		else if (tokenIsType (token, TCL_IDENTIFIER))
		{
			notifyCommand (token, CORK_NIL);
			skipToEndOfCmdline(token); /* ??? */
		}
		else
			skipToEndOfCmdline(token);
	} while (!tokenIsEOF(token));

	tokenDestroy (token);
	flashTokenBacklog (&tclTokenInfoClass);
}

extern parserDefinition* TclParser (void)
{
	static const char *const extensions [] = { "tcl", "tk", "wish", "exp", NULL };
	static const char *const aliases [] = {"expect", "tclsh", NULL };

	parserDefinition* def = parserNew ("Tcl");
	def->kindTable      = TclKinds;
	def->kindCount  = ARRAY_SIZE (TclKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->keywordTable = TclKeywordTable;
	def->keywordCount = ARRAY_SIZE (TclKeywordTable);

	def->parser     = findTclTags;
	def->useCork    = true;
	def->requestAutomaticFQTag = true;
	return def;
}
