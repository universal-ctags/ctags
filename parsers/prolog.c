/*
*   Copyright (c) 2025, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for prolog source file.
*
*   References:
*   - https://www.swi-prolog.org/pldoc/man?section=syntax
*   - https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_45.html
*   - https://www.az-prolog.com/manual/manuals/manual_program.html (in Japanese)
*
*/

/*
*   INCLUDE FILES
*/
#include "general.h"        /* must always come first */

#include "entry.h"
#include "field.h"
#include "keyword.h"
#include "param.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "tokeninfo.h"
#include "vstring.h"
#include "xtag.h"

#include <string.h>

/*
*   DATA DECLARATIONS
*/

typedef enum {
	K_PREDICATE,
	K_GRAMMAR,
	K_MODULE,
} prologKind;

typedef enum {
	F_ARITY,
} prologField;

typedef enum {
	X_ARITY,
} prologXtag;

typedef enum {
	R_MODULE_CHAINELT,
} prologModuleRole;


typedef enum {
	/* 0..255 are the byte's value */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_KEYWORD,

	TOKEN_STRATOM,
	TOKEN_STRING,
	TOKEN_APPLY,
	TOKEN_RULEOP,
	TOKEN_DCGOP,

	TOKEN_CODE,
	TOKEN_ATOM,
	TOKEN_VAR,
	TOKEN_NUM,

} prologTokenType;

typedef enum {
	KEYWORD_MODULE,
#if 0
	KEYWORD_USE_MODULE,
	KEYWORD_AUTOLOAD,
	KEYWORD_LIBRARY,
#endif
} prologKeyword;

/*
*   FUNCTION PROTOTYPES
*/

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED);

/*
*   DATA DEFINITIONS
*/

static roleDefinition PrologModuleRoles [] = {
	/* Currently V parser wants this items. */
	{ false, "chainElt", "(EXPERIMENTAL)used as an element in a module-qualified atom like module:predicate" },
};

static kindDefinition PrologKinds []  = {
	{true,  'p', "predicate", "predicates", },
	{true,  'g', "grammar",   "grammars",   },
	{true,  'm', "module",    "modules",
	 .referenceOnly = false, ATTACH_ROLES(PrologModuleRoles),
	},
};

static fieldDefinition PrologFields[] = {
	{ .name = "arity",
	  .description = "the number of parameters",
	  .enabled = true,
	  .dataType = FIELDTYPE_INTEGER,
	},
};

static xtagDefinition PrologXtags [] = {
	{ .enabled     = true,
	  .name        = "arityAppended",
	  .description = "Include predicates with their arities",
	},
};

static bool prologAllowNestedComments = true;
static bool prologAllowNestedCommentsHandler (const langType language CTAGS_ATTR_UNUSED,
											  const char *name, const char *arg)
{
	prologAllowNestedComments = paramParserBool (arg, prologAllowNestedComments, name, "parameter");
	return true;
}

static paramDefinition PrologParams [] = {
	{ .name = "allowNestedComments",
	  .desc = "allow nested comments like \"/* /* */ */\" ([true] or false)",
	  .handleParam = prologAllowNestedCommentsHandler,
	  /* https://www.swi-prolog.org/pldoc/man?section=nestedcomments */
	},
};

static struct tokenTypePair prologTypePairs [] = {
	{ '(', ')' },
	{ '[', ']' },
	{ '{', '}' },
};

static const keywordTable PrologKeywords[] = {
	{ "module", KEYWORD_MODULE },
};

static struct tokenInfoClass prologTokenInfoClass = {
	.nPreAlloc = 4,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.extraSpace       = 0,
	.pairs            = prologTypePairs,
	.pairCount        = ARRAY_SIZE (prologTypePairs),
	.read             = readToken,
};

static langType Lang_prolog;

/*
*   FUNCTION DEFINITIONS
*/

static tokenInfo *newPrologToken (void)
{
	return newToken (&prologTokenInfoClass);
}

static void skipNestedBlockComment (int c)
{
	int depth = 1;

	while (c != EOF)
	{
		if (c == '/')
		{
			c = getcFromInputFile ();
			if (c == '*')
			{
				++depth;
				c = getcFromInputFile ();
			}
		}
		else if (c == '*')
		{
			c = getcFromInputFile ();
			if (c == '/')
			{
				if (--depth == 0)
					return;
				c = getcFromInputFile ();
			}
		}
		else
			c = getcFromInputFile ();
	}
}

static void skipBlockComment (int c)
{
	while (c != EOF)
	{
		if (c == '*')
		{
			c = getcFromInputFile ();
			if (c == '/')
				return;
		}
		else
			c = getcFromInputFile ();
	}
}

static void readString (tokenInfo *const token, int c)
{
	int terminator = c;

	tokenPutc (token, c);
	c = getcFromInputFile ();

	while (c != EOF)
	{
		tokenPutc (token, c);

		if (c == '\\')
		{
			/* consume the next char */
			c = getcFromInputFile ();
			if (c != EOF)
			{
				tokenPutc (token, c);
				c = getcFromInputFile ();
			}
			continue;
		}

		if (c == terminator)
			return;

		c = getcFromInputFile ();
	}
}

static void readNumber (tokenInfo *const token, int c)
{
	bool dpoint_seen = false;

	tokenPutc (token, c);
	c = getcFromInputFile ();

	while (c != EOF)
	{
		// SWI prolog accepts:
		//
		//   1_000_000
		//   1 000 000
		//   1_000_/*more*/000
		//
		// We handle _ here.
		//
		if (isdigit (c) || c == '_')
		{
			tokenPutc (token, c);
			c = getcFromInputFile ();
			continue;
		}

		if (c == '.')
		{
			if (dpoint_seen)
			{
				ungetcToInputFile ('.');
				return;
			}

			dpoint_seen = true;
			int c0 = getcFromInputFile ();
			if (isdigit (c0))
			{
				tokenPutc (token, '.');
				tokenPutc (token, c0);
				c = getcFromInputFile ();
				continue;
			}

			ungetcToInputFile (c0);
			ungetcToInputFile ('.');
			return;
		}

		ungetcToInputFile (c);
		return;
	}
}

static void readAtom (tokenInfo *const token, int c)
{
	tokenPutc (token, c);
	c = getcFromInputFile ();

	while (c != EOF)
	{
		if (isalnum (c) || c == '_')
		{
			tokenPutc (token, c);
			c = getcFromInputFile ();
			continue;
		}

		ungetcToInputFile (c);
		return;
	}
}

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED)
{
	int c;

	token->type = TOKEN_UNDEFINED;
	token->keyword = KEYWORD_NONE;
	vStringClear (token->string);

 getNextChar:
	do
		c = getcFromInputFile ();
	while (c == ' ' || c == '\t' || c == '\f' || c == '\r' || c == '\n');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case '%':
		do
			c = getcFromInputFile ();
		while (c != EOF && c != '\n');
		goto getNextChar;
	case '/':
	{
		int c0 = getcFromInputFile ();
		if (c0 == '*')
		{
			c0 = getcFromInputFile ();
			if (prologAllowNestedComments)
				skipNestedBlockComment (c0);
			else
				skipBlockComment (c0);
			goto getNextChar;
		}
		ungetcToInputFile (c0);
		tokenPutc (token, c);
		token->type = c;
		break;
	}
	case '\'':
	{
		readString (token, c);
		token->type = TOKEN_STRATOM;
		break;
	}
	case '"':
	{
		readString (token, c);
		token->type = TOKEN_STRING;
		break;
	}

	case '(':
	case ')':
	case '{':
	case '}':
	case '[':
	case ']':
	case ',':
	case '.':
		tokenPutc(token, c);
		token->type = c;
		break;
	case '=':
	{
		tokenPutc(token, c);
		int c0 = getcFromInputFile ();
		if (c0 == '.')
		{
			int c1 = getcFromInputFile ();
			if (c1 == '.')
			{
				tokenCatS (token, "..");
				token->type = TOKEN_APPLY;
				break;
			}
			ungetcToInputFile (c1);
		}
		ungetcToInputFile (c0);
		token->type = c;
		break;
	}
	case ':':
	{
		tokenPutc(token, c);
		int c0 = getcFromInputFile ();
		if (c0 == '-')
		{
			tokenPutc(token, c0);
			token->type = TOKEN_RULEOP;
			break;
		}
		ungetcToInputFile (c0);
		token->type = c;
		break;
	}
	case '-':
	{
		tokenPutc(token, c);

		int c0 = getcFromInputFile ();
		if (c0 == '-')
		{
			int c1 = getcFromInputFile ();
			if (c1 == '>')
			{
				tokenCatS (token, "->");
				token->type = TOKEN_DCGOP;
				break;
			}
			ungetcToInputFile (c1);
		}
		ungetcToInputFile (c0);
		token->type = c;
		break;
	}
	default:
		if (c == '0')
		{
			int c0 = getcFromInputFile ();
			if (c0 == '\'')
			{
				tokenPutc(token, c);
				c = getcFromInputFile ();
				if (c != EOF)
					tokenPutc(token, c);

				token->type = TOKEN_CODE;
				break;
			}
			ungetcToInputFile (c0);
			readNumber (token, c);
			token->type = TOKEN_NUM;
			return;
		}
		if (isdigit(c))
		{
			readNumber (token, c);
			token->type = TOKEN_NUM;
		}
		else if (c == '_' || ('A' <= c && c <= 'Z'))
		{
			readAtom (token, c);
			token->type = TOKEN_VAR;
		}
		else if ('a' <= c && c <= 'z')
		{
			readAtom (token, c);
			token->keyword = lookupKeyword (tokenString (token), Lang_prolog);
			token->type = (token->keyword == KEYWORD_NONE)
				? TOKEN_ATOM
				: TOKEN_KEYWORD;
		}
		else if (c == '-')
		{
			tokenPutc (token, c);
			c = getcFromInputFile ();
			if (isdigit (c))
			{
				readNumber (token, c);
				token->type = TOKEN_NUM;
			}
			else
			{
				/* I wonder what I should do here? */
				ungetcToInputFile (c);
				token->type = TOKEN_ATOM;
			}
		}
		else
		{
			tokenPutc (token, c);
			if (c == '\\')
			{
				int c0 = getcFromInputFile ();
				if (c0 != EOF)
					tokenPutc (token, c0);
			}
			token->type = c;
#if 0
			tokenCatS (token, " REST :: ");
			tokenCatS (token, getInputFileName ());
#endif
		}
		break;
	}
}

static void makePrologTagEntry (tokenInfo *const atom, prologKind kindex, int arity,
								unsigned long endLine, int module)
{
	tagEntryInfo e, ex;
	char arity_str[16];

	tokenInitTagEntry (atom, &e, kindex);
	setTagEndLine (&e, endLine);
	e.extensionFields.scopeIndex = module;
	snprintf (arity_str, sizeof (arity_str), "%d", arity);
	attachParserField (&e, PrologFields[F_ARITY].ftype, arity_str);
	makeTagEntry (&e);

	tokenPutc (atom, '/');
	tokenCatS (atom, arity_str);
	tokenInitTagEntry (atom, &ex, kindex);
	setTagEndLine (&ex, endLine);
	ex.extensionFields.scopeIndex = module;
	attachParserField (&ex, PrologFields[F_ARITY].ftype, arity_str);
	markTagExtraBit (&ex, PrologXtags[X_ARITY].xtype);
	makeTagEntry (&ex);
}

static int parseArguments (tokenInfo *const t)
{
	tokenRead (t);

	int arity = 0;
	while (!tokenIsEOF (t) && tokenSkipToOneOfTypes (t, true, ',', '(', ')'))
	{
		if (t->type == '(')
			tokenSkipOverPair (t);
		else
		{
			arity++;
			if (t->type == ')')
				break;
		}
		tokenRead (t);
	}
	return arity;
}

static void deleteBackToken (tokenInfo *t)
{
	if (!tokenIsEOF (t))
		tokenUnread (t);
	tokenDelete (t);
}

static int lookModuleAhead (void)
{
	int r = CORK_NIL;
	tokenInfo *openparen = newPrologToken ();

	tokenRead (openparen);
	if (openparen->type == '(')
	{
		tokenInfo *mod = newPrologToken ();
		tokenRead (mod);
		if (tokenIsType (mod, ATOM))
			r = tokenMakeSimpleTag (mod, K_MODULE);
		deleteBackToken (mod);
	}
	deleteBackToken (openparen);

	return r;
}

static int lookBodyAhead (void)
{
	int r = CORK_NIL;

	tokenInfo *modpred = newPrologToken ();

	tokenRead (modpred);
	if (tokenIsKeyword (modpred, MODULE))
		r = lookModuleAhead ();
	deleteBackToken (modpred);

	return r;
}

static int parseClause (tokenInfo *const t, int module)
{
	tokenInfo *atom = NULL;
	int arity = 0;

	if (tokenIsType (t, ATOM)
		|| tokenIsType (t, STRATOM)
		|| tokenIsType (t, KEYWORD))
	{
		atom = newPrologToken ();
		tokenCopy (atom, t);

		tokenRead (t);
		if (tokenIsEOF (t))
		{
			tokenDelete (atom);
			return module;
		}
		if (t->type == ':')
		{
			int chainelt = makeSimpleRefTag (atom->string, K_MODULE, R_MODULE_CHAINELT);
			tokenDelete (atom);
			tokenRead (t);
			parseClause (t, chainelt);
			return module;
		}
		else
			tokenUnread (t);
	}
	else if (tokenIsType (t, RULEOP))
	{
		int newmod = lookBodyAhead ();
		tokenSkipToOneOfTypes (t, true, '.');
		if (newmod != CORK_NIL)
			module = newmod;
		return module;
	}
	else
	{
		tokenSkipToOneOfTypes (t, true, '.');
		return module;
	}

	tokenRead (t);
	if (atom)
	{
		int kindex = K_PREDICATE;

		if (t->type == '(')
		{
			arity = parseArguments (t);
			tokenRead (t);
		}

		if (tokenIsType (t, RULEOP)
			|| tokenIsType (t, DCGOP))
		{
			if (tokenIsType (t, DCGOP))
				kindex = K_GRAMMAR;
			tokenSkipToOneOfTypes (t, true, '.');
		}

		if (t->type == '.')
			makePrologTagEntry (atom, kindex, arity, t->lineNumber,
								module);

		tokenDelete (atom);
	}

	return module;
}

static void findPrologTags (void)
{
	int module = CORK_NIL;
	tokenInfo *t = newPrologToken ();

	while (1)
	{
		tokenRead (t);
		if (tokenIsEOF (t))
			break;
		module = parseClause (t, module);
	}

	tokenDelete (t);
}

extern void initialize (const langType language)
{
	Lang_prolog = language;
}

extern parserDefinition* PrologParser (void)
{
	static const char *const extensions [] = {"pl", NULL};

	/* Interpreters supporting shebang:
	 *
	 * + swipl
	 *   https://www.swi-prolog.org/pldoc/man?section=plscript
	 * + gprolog
	 *   http://www.gprolog.org/manual/html_node/gprolog007.html
	 * + yap
	 *   https://github.com/search?q=repo%3Avscosta%2Fyap%20%20%22%23!%22&type=code
	 */
	static const char *const aliases [] = {
		"swipl",
		"gprolog",
		"yap",
		NULL
	};

	static selectLanguage selectors[] = { selectPerlOrPrologByDistinctiveToken, NULL };

	parserDefinition* const def = parserNew ("Prolog");
	def->initialize     = initialize;
	def->parser         = findPrologTags;
	def->extensions     = extensions;
	def->aliases        = aliases;
	def->selectLanguage = selectors;
	def->keywordTable   = PrologKeywords;
	def->keywordCount   = ARRAY_SIZE(PrologKeywords);
	def->useCork        = CORK_QUEUE;
	def->kindTable      = PrologKinds;
	def->kindCount      = ARRAY_SIZE(PrologKinds);
	def->fieldTable     = PrologFields;
	def->fieldCount     = ARRAY_SIZE(PrologFields);
	def->xtagTable      = PrologXtags;
	def->xtagCount      = ARRAY_SIZE(PrologXtags);
	def->paramTable     = PrologParams;
	def->paramCount     = ARRAY_SIZE(PrologParams);
	def->requestAutomaticFQTag = true;
	def->defaultScopeSeparator = ":";

	return def;
}
