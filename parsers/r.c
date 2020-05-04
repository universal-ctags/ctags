/*
*   Copyright (c) 2003-2004, Ascher Stefan <stievie@utanet.at>
*   Copyright (c) 2020, Masatake YAMATO <yamato@redhat.com>
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for R language files.
*   R is a programming language for statistical computing.
*   R is GPL Software, get it from http://www.r-project.org/
*
*   The language references are available at
*   https://cran.r-project.org/manuals.html, and
*   https://cran.r-project.org/doc/manuals/r-release/R-lang.html
*
*   The base library (including library and source fucntions) release is at
*   https://stat.ethz.ch/R-manual/R-devel/library/base/html/00Index.html
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "selectors.h"
#include "tokeninfo.h"
#include "trace.h"
#include "vstring.h"


#include <string.h>
#include <ctype.h>	/* to define isalpha(), isalnum(), isspace() */

/*
*   MACROS
*/
#ifdef DEBUG
#define R_TRACE_TOKEN_TEXT(TXT,T,Q) TRACE_PRINT("<%s> token: %s (%s), parent: %s", \
												(TXT),					\
												tokenIsTypeVal(T, '\n')? "\\n": tokenString(T), \
												tokenTypeStr(T->type),	\
												(Q) == CORK_NIL? "": getEntryInCorkQueue(Q)->name)
#define R_TRACE_TOKEN(T,Q) TRACE_PRINT("token: %s (%s), parent: %s", \
									   tokenIsTypeVal((T), '\n')? "\\n": tokenString(T), \
									   tokenTypeStr((T)->type),			\
									   (Q) == CORK_NIL? "": getEntryInCorkQueue(Q)->name)

#define R_TRACE_ENTER() TRACE_ENTER_TEXT("token: %s (%s), parent: %s", \
										 tokenIsTypeVal(token, '\n')? "\\n": tokenString(token), \
										 tokenTypeStr(token->type), \
										 parent == CORK_NIL? "": getEntryInCorkQueue(parent)->name)
#define R_TRACE_LEAVE() TRACE_LEAVE()
#else
#define R_TRACE_TOKEN_TEXT(TXT,T,Q) do {} while (0);
#define R_TRACE_TOKEN(T,Q) do {} while (0);
#define R_TRACE_ENTER() do {} while (0);
#define R_TRACE_LEAVE() do {} while (0);
#endif

/*
*   DATA DEFINITIONS
*/

typedef enum {
	K_UNDEFINED = -1,
	K_FUNCTION,
	K_LIBRARY,
	K_SOURCE,
	K_GLOBALVAR,
	K_FUNCVAR,
	K_PARAM,
	KIND_COUNT
} rKind;

typedef enum {
	R_LIBRARY_ATTACHED_BY_LIBRARY,
	R_LIBRARY_ATTACHED_BY_REQUIRE,
} rLibraryRole;

typedef enum {
	R_SOURCE_LOADED_BY_SOURCE,
} rSourceRole;

static roleDefinition RLibraryRoles [] = {
	{ true, "library", "library attached by library fucntion" },
	{ true, "require", "library attached by require fucntion" },
};

static roleDefinition RSourceRoles [] = {
	{ true, "source", "source loaded by source fucntion" },
};

static kindDefinition RKinds[KIND_COUNT] = {
	{true, 'f', "function", "functions"},
	{true, 'l', "library", "libraries",
	 .referenceOnly = true, ATTACH_ROLES (RLibraryRoles) },
	{true, 's', "source", "sources",
	 .referenceOnly = true, ATTACH_ROLES (RSourceRoles) },
	{true, 'g', "globalVar", "global variables"},
	{true, 'v', "functionVar", "function variables"},
	{false,'z', "parameter",  "function parameters inside function definitions" },
};

typedef enum {
	F_ASSIGNMENT_OPERATOR,
} rField;

static fieldDefinition RFields [] = {
	{
		.name = "assignmentop",
		.description = "operator for assignment",
		.enabled = false,
	},
};

enum eKeywordId
{
	KEYWORD_FUNCTION,
	KEYWORD_IF,
	KEYWORD_ELSE,
	KEYWORD_FOR,
	KEYWORD_WHILE,
	KEYWORD_REPEAT,
	KEYWORD_IN,
	KEYWORD_NEXT,
	KEYWORD_BREAK,
	KEYWORD_TRUE,
	KEYWORD_FALSE,
	KEYWORD_NULL,
	KEYWORD_INF,
	KEYWORD_NAN,
	KEYWORD_NA,
	KEYWORD_SOURCE,
	KEYWORD_LIBRARY,
};

typedef int keywordId;			/* to allow KEYWORD_NONE */

static const keywordTable RKeywordTable [] = {
	{ "function", KEYWORD_FUNCTION },
	{ "if",       KEYWORD_IF       },
	{ "else",     KEYWORD_ELSE     },
	{ "for",      KEYWORD_FOR      },
	{ "while",    KEYWORD_WHILE    },
	{ "repeat",   KEYWORD_REPEAT   },
	{ "in",       KEYWORD_IN       },
	{ "next",     KEYWORD_NEXT     },
	{ "break",    KEYWORD_BREAK    },
	{ "TRUE",     KEYWORD_TRUE,    },
	{ "FALSE",    KEYWORD_FALSE,   },
	{ "NULL",     KEYWORD_NULL,    },
	{ "Inf",      KEYWORD_INF,     },
	{ "NaN",      KEYWORD_NAN,     },
	{ "NA",       KEYWORD_NA,      },
	{ "NA_integer_",   KEYWORD_NA, },
	{ "NA_real_",      KEYWORD_NA, },
	{ "NA_complex_",   KEYWORD_NA, },
	{ "NA_character_", KEYWORD_NA, },
	{ "source",   KEYWORD_SOURCE   },
	{ "library",  KEYWORD_LIBRARY  },
	{ "require",  KEYWORD_LIBRARY  },
};

enum RTokenType {
	/* 0..255 are the byte's values */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_KEYWORD,
	TOKEN_NEWLINE,
	TOKEN_NUMBER,				/* 1, 1L */
	TOKEN_SYMBOL,				/* [0-9a-zA-Z._] */
	TOKEN_STRING,
	TOKEN_OPERATOR,				/* - + ! ~ ? : * / ^ %...%, <, > ==
								 * >=, <=, &, &&, |, || */
	TOKEN_DOTS,					/* ... */
	TOKEN_DOTS_N,				/* ..1, ..2, etc */
	TOKEN_LASSIGN,				/* <-, <<- */
	TOKEN_RASSIGN,				/* ->, ->> */
	TOKEN_DBRACKET_OEPN,		/* [[ */
	TOKEN_DBRACKET_CLOSE,		/* ]] */
	TOKEN_SCOPE,				/* ::, ::: */
};

#ifdef DEBUG
static const char *tokenTypeStr(enum RTokenType e);
#endif

static struct tokenTypePair typePairs [] = {
	{ '{', '}' },
	{ '[', ']' },
	{ '(', ')' },
	{ TOKEN_DBRACKET_OEPN, TOKEN_DBRACKET_CLOSE },
};

typedef struct sRToken {
	tokenInfo base;
	int scopeIndex;
	int parenDepth;
	vString *signature;
} rToken;

#define R(TOKEN) ((rToken *)TOKEN)

static int blackHoleIndex;

static void readToken (tokenInfo *const token, void *data);
static void clearToken (tokenInfo *token);
static struct tokenInfoClass rTokenInfoClass = {
	.nPreAlloc        = 4,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.extraSpace       = sizeof (rToken) - sizeof (tokenInfo),
	.pairs            = typePairs,
	.pairCount        = ARRAY_SIZE (typePairs),
	.init             = NULL,
	.read             = readToken,
	.clear            = clearToken,
	.copy             = NULL,
};


/*
 * FUNCTION PROTOTYPES
 */

static void parseStatement (tokenInfo *const token, int parent, tokenInfo *const funcall, bool in_continuous_pair);
static void parsePair (tokenInfo *const token, int parent, tokenInfo *const funcall);


/*
*   FUNCTION DEFINITIONS
*/

static int makeSimpleRTag (tokenInfo *const token, int parent, int kind)
{
	if (kind != K_FUNCTION)
	{
		if (kind == K_FUNCVAR)
		{
			if (anyKindsEntryInScope (parent, tokenString (token),
									  (int[]){K_FUNCVAR, K_PARAM}, 2) != CORK_NIL)
				return CORK_NIL;
		}
		else if (anyKindEntryInScope (parent, tokenString (token), kind) != CORK_NIL)
			return CORK_NIL;
	}

	int corkIndex = makeSimpleTag (token->string, kind);
	tagEntryInfo *tag = getEntryInCorkQueue (corkIndex);
	if (tag)
	{
		tag->extensionFields.scopeIndex = parent;
		registerEntry (corkIndex);
	}
	return corkIndex;
}

static void clearToken (tokenInfo *token)
{
	R (token)->parenDepth = 0;
	R (token)->scopeIndex = CORK_NIL;
	if (R (token)->signature)
	{
		vStringDelete (R (token)->signature);
		R (token)->signature = NULL;
	}
}

static void readString (tokenInfo *const token, void *data)
{
	int c;
	bool escaped = false;

	int c0 = tokenString(token)[0];

	while (1)
	{
		c = getcFromInputFile ();
		switch (c)
		{
		case EOF:
			return;
		case '\'':
		case '"':
		case '`':
			tokenPutc (token, c);
			if (!escaped && c == c0)
				return;
			escaped = false;
			break;
		case '\\':
			tokenPutc (token, c);
			escaped = !escaped;
			break;
		default:
			tokenPutc (token, c);
			escaped = false;
			break;
		}
	}
}

static void readNumber (tokenInfo *const token, void *data)
{
	int c;

	/* 10.3.1 Constants
	 *
	 * Valid numeric constants: 1 10 0.1 .2 1e-7 1.2e+7
	 * Valid integer constants:  1L, 0x10L, 1000000L, 1e6L
	 * Valid numeric constants:  1.1L, 1e-3L, 0x1.1p-2
	 * Valid complex constants: 2i 4.1i 1e-2i
	 */
	while ((c = getcFromInputFile ()))
	{
		if (isxdigit (c) || c == '.' || c == 'E'
			|| c == '+' || c == '-'
			|| c == 'L' || c == 'x' || c == 'p'
			|| c == 'i')
			tokenPutc (token, c);
		else
		{
			ungetcToInputFile (c);
			break;
		}
	}
}

static void readSymbol (tokenInfo *const token, void *data)
{
	int c;
	while ((c = getcFromInputFile ()))
	{
		if (isalnum (c) || c == '.' || c == '_')
			tokenPutc (token, c);
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

	return lookupCaseKeyword (s, lang);
}

static bool signatureExpectingParameter (vString *signature)
{
	if (vStringLast (signature) == '(')
		return true;

	for (size_t i = vStringLength (signature); i > 0; i--)
	{
		char c = vStringItem (signature, i - 1);
		if (c == ' ')
			continue;
		else if (c == ',')
			return true;
		break;
	}
	return false;
}

static void readToken (tokenInfo *const token, void *data)
{
	int c, c0;

	token->type = TOKEN_UNDEFINED;
	token->keyword = KEYWORD_NONE;
	vStringClear (token->string);

	do
		c = getcFromInputFile ();
	while (c == ' ' || c== '\t' || c == '\f');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case '#':
		while (1)
		{
			c = getcFromInputFile ();
			if (c == EOF)
			{
				token->type = TOKEN_EOF;
				break;
			}
			else if (c == '\n')
			{
				token->type = c;
				tokenPutc (token, c);
				break;
			}
		}
		break;
	case '\n':
	case ';':
		token->type = c;
		tokenPutc (token, c);
		break;
	case '\'':
	case '"':
	case '`':
		token->type = TOKEN_STRING;
		tokenPutc (token, c);
		readString (token, data);
		break;
	case '+':
	case '/':
	case '^':
	case '~':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		break;
	case ':':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == ':')
		{
			tokenPutc (token, c);
			token->type = TOKEN_SCOPE;
			c = getcFromInputFile ();
			if (c == ':')
				tokenPutc (token, c);
			else
				ungetcToInputFile (c);
		}
		else
			ungetcToInputFile (c);
		break;
	case '&':
	case '|':
	case '*':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c0 = getcFromInputFile ();
		if (c == c0)
			tokenPutc (token, c0);
		else
			ungetcToInputFile (c0);
		break;
	case '=':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '=')
			tokenPutc (token, c);
		else
		{
			token->type = '=';
			ungetcToInputFile (c);
		}
		break;
	case '-':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '>')
		{
			token->type = TOKEN_RASSIGN;
			tokenPutc (token, c);
			c = getcFromInputFile ();
			if (c == '>')
				tokenPutc (token, c);
			else
				ungetcToInputFile (c);
		}
		else
			ungetcToInputFile (c);
		break;
	case '>':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '=')
			tokenPutc (token, c);
		else
			ungetcToInputFile (c);
		break;
	case '<':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();

		/* <<- */
		if (c == '<')
		{
			tokenPutc (token, c);
			c = getcFromInputFile ();
		}

		if (c == '-')
		{
			token->type = TOKEN_LASSIGN;
			tokenPutc (token, c);
		}
		else if (c == '=')
			tokenPutc (token, c);
		else
			ungetcToInputFile (c);
		break;
	case '%':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		do
		{
			c = getcFromInputFile ();
			if (c == EOF)
				break;

			tokenPutc (token, c);
			if (c == '%')
				break;
		}
		while (1);
		break;
	case '!':
		token->type = TOKEN_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '=')
			tokenPutc (token, c);
		else
			ungetcToInputFile (c);
		break;
	case '{':
	case '}':
	case '(':
	case ')':
	case ',':
	case '$':
	case '@':
		token->type = c;
		tokenPutc (token, c);
		break;
	case '[':
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '[')
		{
			token->type = TOKEN_DBRACKET_OEPN;
			tokenPutc (token, c);
		}
		else
		{
			token->type = '[';
			ungetcToInputFile (c);
		}
		break;
	case ']':
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == ']')
		{
			token->type = TOKEN_DBRACKET_CLOSE;
			tokenPutc (token, c);
		}
		else
		{
			token->type = ']';
			ungetcToInputFile (c);
		}
		break;
	case '.':
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (isdigit(c))
		{
			token->type = TOKEN_NUMBER;
			tokenPutc (token, c);
			readNumber(token, data);
		}
		else if (isalpha (c) || c == '_')
		{
			token->type = TOKEN_SYMBOL;
			tokenPutc (token, c);
			readSymbol (token, data);

			token->keyword = resolveKeyword (token->string);
			if (token->keyword != KEYWORD_NONE)
				token->type = TOKEN_KEYWORD;
		}
		else if (c == '.')
		{
			token->type = TOKEN_DOTS;
			tokenPutc (token, c);

			c = getcFromInputFile ();
			if (c == '.')
				tokenPutc (token, c);
			else if (isdigit(c))
			{
				token->type = TOKEN_DOTS_N;
				do
				{
					tokenPutc (token, c);
					c = getcFromInputFile ();
				}
				while (isdigit(c));
				ungetcToInputFile (c);
			}
			else if (isalpha (c) || c == '_')
			{
				token->type = TOKEN_SYMBOL;
				tokenPutc (token, c);
				readSymbol (token, data);

				token->keyword = resolveKeyword (token->string);
				if (token->keyword != KEYWORD_NONE)
					token->type = TOKEN_KEYWORD;
			}
			else
			{
				token->type = TOKEN_UNDEFINED;
				ungetcToInputFile (c);
			}
		}
		break;
	default:
		tokenPutc (token, c);
		if (isdigit (c))
		{
			token->type = TOKEN_NUMBER;
			readNumber(token, data);
		}
		else if (isalpha (c))
		{
			token->type = TOKEN_SYMBOL;
			readSymbol (token, data);

			token->keyword = resolveKeyword (token->string);
			if (token->keyword != KEYWORD_NONE)
				token->type = TOKEN_KEYWORD;
		}
		else
			token->type = TOKEN_UNDEFINED;
		break;
	}

	/* Handle parameters in a signature */
	if (R(token)->signature && !tokenIsType(token, EOF) && !tokenIsTypeVal(token, '\n'))
	{
		vString *signature = R (token)->signature;

		if (tokenIsTypeVal (token, '('))
			R (token)->parenDepth++;
		else if (tokenIsTypeVal (token, ')'))
			R (token)->parenDepth--;

		if (R (token)->parenDepth == 1 && tokenIsType (token, SYMBOL)
			&& signatureExpectingParameter (signature))
			makeSimpleRTag (token, R (token)->scopeIndex, K_PARAM);

		if (vStringLast (signature) != '(' &&
			!tokenIsTypeVal (token, ',') &&
			!tokenIsTypeVal (token, ')'))
			vStringPut (signature, ' ');
		vStringCat (signature, token->string);
	}
}

static tokenInfo *newRToken(void)
{
	return newToken (&rTokenInfoClass);
}


static void tokenReadNoNewline (tokenInfo *const token)
{
	while (1)
	{
		tokenRead(token);
		if (!tokenIsTypeVal (token, '\n'))
			break;
	}
}

static void parseRightSide (tokenInfo *const token, tokenInfo *const symbol, int parent)
{
	R_TRACE_ENTER();

	int corkIndex = CORK_NIL;
	char *const assignment_operator = eStrdup (tokenString (token));
	vString *signature = NULL;

	tokenReadNoNewline (token);

	bool in_func = tokenIsKeyword (token, FUNCTION);
	if (in_func)
	{
		corkIndex = makeSimpleRTag (symbol, parent,
								   parent == CORK_NIL? K_FUNCTION: K_FUNCVAR);
		tokenReadNoNewline (token);

		/* Signature */
		if (tokenIsTypeVal (token, '('))
		{
			if (corkIndex == CORK_NIL)
				tokenSkipOverPair (token);
			else
			{
				signature = vStringNewInit("(");
				R (token)->signature = signature;
				R (token)->scopeIndex = corkIndex;
				R (token)->parenDepth = 1;
				tokenSkipOverPair (token);
				R (token)->parenDepth = 0;
				R (token)->scopeIndex = CORK_NIL;
				R (token)->signature = NULL;
			}
			tokenReadNoNewline (token);
		}
	}
	else
		corkIndex = makeSimpleRTag (symbol, parent,
									parent == CORK_NIL? K_GLOBALVAR: K_FUNCVAR);

	int new_scope = (in_func
					 ? (corkIndex == CORK_NIL
						? blackHoleIndex
						: corkIndex)
					 : parent);
	R_TRACE_TOKEN_TEXT("body", token, new_scope);

	parseStatement (token, new_scope, NULL, false);

	tagEntryInfo *tag = getEntryInCorkQueue (corkIndex);
	if (tag)
	{
		tag->extensionFields.endLine = token->lineNumber;
		if (signature)
		{
			tag->extensionFields.signature = vStringDeleteUnwrap(signature);
			signature = NULL;
		}
		attachParserField (tag, true,
						   RFields [F_ASSIGNMENT_OPERATOR].ftype,
						   assignment_operator);
	}

	vStringDelete (signature);	/* NULL is acceptable. */
	eFree (assignment_operator);
	R_TRACE_LEAVE();
}

/* Parse arguments for library and source. */
static bool preParseExternalEntitiy (tokenInfo *const token, tokenInfo *const funcall)
{
	TRACE_ENTER();

	bool r = true;
	tokenInfo *prefetch_token = newRToken ();

	tokenReadNoNewline (prefetch_token);
	if (tokenIsType (prefetch_token, SYMBOL)
		|| tokenIsType (prefetch_token, STRING))
	{
		tokenInfo *const loaded_obj_token = newTokenByCopying (prefetch_token);
		tokenReadNoNewline (prefetch_token);
		if (tokenIsTypeVal (prefetch_token, ')')
			|| tokenIsTypeVal (prefetch_token, ','))
		{
			if (tokenIsTypeVal (prefetch_token, ')'))
				r = false;

			makeSimpleRefTag (loaded_obj_token->string,
							  (tokenIsKeyword (funcall, LIBRARY)
							   ? K_LIBRARY
							   : K_SOURCE),
							  (tokenIsKeyword (funcall, LIBRARY)
							   ? (strcmp (tokenString(funcall), "library") == 0
								  ? R_LIBRARY_ATTACHED_BY_LIBRARY
								  : R_LIBRARY_ATTACHED_BY_REQUIRE)
							   : R_SOURCE_LOADED_BY_SOURCE));
			tokenDelete (loaded_obj_token);
		}
		else if (tokenIsEOF (prefetch_token))
		{
			tokenCopy (token, prefetch_token);
			tokenDelete (loaded_obj_token);
			r = false;
		}
		else
		{
			tokenUnread (prefetch_token);
			tokenUnread (loaded_obj_token);
			tokenDelete (loaded_obj_token);
		}
	}
	else if (tokenIsEOF (prefetch_token))
	{
		tokenCopy (token, prefetch_token);
		r = false;
	}
	else
		tokenUnread (prefetch_token);

	tokenDelete (prefetch_token);

	TRACE_LEAVE_TEXT(r
					 ? "unread tokens and request parsing again to the upper context"
					 : "parse all arguments");
	return r;
}


/* If funcall is non-NULL, this pair represents the argument list for the function
 * call for FUNCALL. */
static void parsePair (tokenInfo *const token, int parent, tokenInfo *const funcall)
{
	R_TRACE_ENTER();

	bool in_continuous_pair = tokenIsTypeVal (token, '(')
		|| tokenIsTypeVal (token, '[')
		|| tokenIsType(token, DBRACKET_OEPN);
	bool is_funcall = funcall && tokenIsTypeVal (token, '(');
	bool done = false;

	if (is_funcall)
	{
		if 	(tokenIsKeyword (funcall, LIBRARY) ||
			 tokenIsKeyword (funcall, SOURCE))
			done = !preParseExternalEntitiy (token, funcall);
	}

	if (done)
	{
		R_TRACE_LEAVE();
		return;
	}

	do
	{
		tokenRead (token);
		R_TRACE_TOKEN_TEXT("inside pair", token, parent);
		parseStatement (token, parent, funcall, in_continuous_pair);
	}
	while (! (tokenIsEOF (token)
			  || tokenIsTypeVal (token, ')')
			  || tokenIsTypeVal (token, '}')
			  || tokenIsTypeVal (token, ']')
			  || tokenIsType (token, DBRACKET_CLOSE)));
	R_TRACE_LEAVE();
}

static void parseStatement (tokenInfo *const token, int parent,
							tokenInfo *const funcall, bool in_continuous_pair)
{
	R_TRACE_ENTER();

	do
	{
		if (tokenIsEOF (token))
			break;
		else if (tokenIsTypeVal (token, ';'))
		{
			R_TRACE_TOKEN_TEXT ("break with ;", token, parent);
			break;
		}
		else if (tokenIsTypeVal (token, '\n'))
		{
			R_TRACE_TOKEN_TEXT ("break with \\n", token, parent);
			break;
		}
		else if (tokenIsType (token, SYMBOL)
				 || tokenIsType (token, STRING)
				 || tokenIsType (token, KEYWORD))
		{
			tokenInfo *const symbol = newTokenByCopying (token);

			if (in_continuous_pair)
				tokenReadNoNewline (token);
			else
				tokenRead (token);

			if (tokenIsType (token, LASSIGN))
			{
				/* Assignment */
				parseRightSide (token, symbol, parent);
				R_TRACE_TOKEN_TEXT ("break with right side", token, parent);
				tokenDelete(symbol);
				break;
			}
			else if (tokenIsTypeVal (token, '='))
			{
				/* Assignment */
				if (funcall)
				{
					tokenRead (token);
					R_TRACE_TOKEN_TEXT("(in arg list) after = body", token, parent);
				}
				else
				{
					parseRightSide (token, symbol, parent);
					R_TRACE_TOKEN_TEXT ("break with right side", token, parent);
					tokenDelete(symbol);
					break;
				}
			}
			else if (tokenIsTypeVal (token, '('))
			{
				/* function call */
				parsePair (token, parent, symbol);
				tokenRead (token);
				R_TRACE_TOKEN_TEXT("after arglist", token, parent);
			}
			else if (tokenIsTypeVal (token, '$')
					 || tokenIsTypeVal (token, '@')
					 || tokenIsType (token, SCOPE))
			{
				tokenReadNoNewline (token); /* Skip the next identifier */
				tokenRead (token);
				R_TRACE_TOKEN_TEXT("after $", token, parent);
			}
			else
				R_TRACE_TOKEN_TEXT("else after symbol", token, parent);
			tokenDelete(symbol);
		}
		else if (tokenIsType (token, RASSIGN))
		{
			char *const assignment_operator = eStrdup (tokenString (token));
			tokenReadNoNewline (token);
			if (tokenIsType (token, SYMBOL)
				|| tokenIsType (token, STRING))
			{
				int corkIndex = makeSimpleRTag (token, parent,
												parent == CORK_NIL? K_GLOBALVAR: K_FUNCVAR);
				tagEntryInfo *tag = getEntryInCorkQueue (corkIndex);
				if (tag)
					attachParserField (tag, true,
									   RFields [F_ASSIGNMENT_OPERATOR].ftype,
									   assignment_operator);
				tokenRead (token);
			}
			eFree (assignment_operator);
			R_TRACE_TOKEN_TEXT("after ->", token, parent);
		}
		else if (tokenIsType (token, OPERATOR))
		{
			tokenReadNoNewline (token);
			R_TRACE_TOKEN_TEXT("after operator", token, parent);
		}
		else if (tokenIsTypeVal (token, '(')
				 || tokenIsTypeVal (token, '{')
				 || tokenIsTypeVal (token, '[')
				 || tokenIsType (token, DBRACKET_OEPN))
		{
			parsePair (token, parent, NULL);
			tokenRead (token);
			R_TRACE_TOKEN_TEXT("after pair", token, parent);
		}
		else if (tokenIsTypeVal (token, ')')
				 || tokenIsTypeVal (token, '}')
				 || tokenIsTypeVal (token, ']')
				 || tokenIsType (token, DBRACKET_CLOSE))
		{
			R_TRACE_TOKEN_TEXT ("break with close", token, parent);
			break;
		}
		else if (tokenIsTypeVal (token, '$')
				 || tokenIsTypeVal (token, '@')
				 || tokenIsType (token, SCOPE))
		{
			tokenReadNoNewline (token); /* Skip the next identifier */
			tokenRead (token);
			R_TRACE_TOKEN_TEXT("after $", token, parent);
		}
		else
		{
			tokenRead (token);
			R_TRACE_TOKEN_TEXT("else", token, parent);
		}
	}
	while (!tokenIsEOF (token));

	R_TRACE_LEAVE();
}

static void findRTags (void)
{
	tokenInfo *const token = newRToken ();

	blackHoleIndex = makePlaceholder ("**BLACK-HOLE/DON'T TAG ME**");
	registerEntry (blackHoleIndex);

	TRACE_PRINT ("install blackhole: %d", blackHoleIndex);

	do
	{
		tokenRead(token);
		R_TRACE_TOKEN(token, CORK_NIL);
		parseStatement (token, CORK_NIL, NULL, false);
	}
	while (!tokenIsEOF (token));

	TRACE_PRINT ("run blackhole", blackHoleIndex);
	markAllEntriesInScopeAsPlaceholder (blackHoleIndex);

	tokenDelete (token);
}

extern parserDefinition *RParser (void)
{
	static const char *const extensions[] = { "r", "R", "s", "q", NULL };
	parserDefinition *const def = parserNew ("R");
	static selectLanguage selectors[] = { selectByArrowOfR,
										  NULL };

	def->extensions = extensions;
	def->kindTable = RKinds;
	def->kindCount = ARRAY_SIZE(RKinds);
	def->fieldTable = RFields;
	def->fieldCount = ARRAY_SIZE (RFields);
	def->keywordTable = RKeywordTable;
	def->keywordCount = ARRAY_SIZE(RKeywordTable);
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	def->parser = findRTags;
	def->selectLanguage = selectors;

	return def;
}

#ifdef DEBUG
static const char *tokenTypeStr(enum RTokenType e)
{ /* Generated by misc/enumstr.sh with cmdline:
     parsers/r.c RTokenType tokenTypeStr TOKEN_ --use-lower-bits-as-is */
	switch (e)
	{
		case            TOKEN_EOF: return "EOF";
		case      TOKEN_UNDEFINED: return "UNDEFINED";
		case        TOKEN_KEYWORD: return "KEYWORD";
		case        TOKEN_NEWLINE: return "NEWLINE";
		case         TOKEN_NUMBER: return "NUMBER";
		case         TOKEN_SYMBOL: return "SYMBOL";
		case         TOKEN_STRING: return "STRING";
		case       TOKEN_OPERATOR: return "OPERATOR";
		case           TOKEN_DOTS: return "DOTS";
		case         TOKEN_DOTS_N: return "DOTS_N";
		case        TOKEN_LASSIGN: return "LASSIGN";
		case        TOKEN_RASSIGN: return "RASSIGN";
		case  TOKEN_DBRACKET_OEPN: return "DBRACKET_OEPN";
		case TOKEN_DBRACKET_CLOSE: return "DBRACKET_CLOSE";
		case          TOKEN_SCOPE: return "SCOPE";
		default:                   break;
	}
	static char buf[3];
	if (isprint (e))
	{
		buf[0] = e;
		buf[1] = '\0';
	}
	else if (e == '\n')
	{
		buf[0] = '\\';
		buf[1] = 'n';
		buf[2] = '\0';
	}
	else
	{
		buf[0] = '\0';
	}
	return buf;
}
#endif
