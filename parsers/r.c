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
*   The base library (including library and source functions) release is at
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
#include "subparser.h"
#include "r.h"

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
	K_VECTOR,
	K_LIST,
	K_DATAFRAME,
	K_NAMEATTR,
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
	{ true, "library", "library attached by library function" },
	{ true, "require", "library attached by require function" },
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
	{true, 'g', "globalVar", "global variables having values other than function()"},
	{true, 'v', "functionVar", "function variables having values other than function()"},
	{false,'z', "parameter",  "function parameters inside function definitions" },
	{true, 'c', "vector", "vectors explicitly created with `c()'" },
	{true, 'L', "list", "lists explicitly created with `list()'" },
	{true, 'd', "dataframe", "data frame explicitly created with `data.frame()'" },
	{true, 'n', "nameattr", "names attribtes in vectors, lists, or dataframes" },
};

struct sKindExtraInfo {
	const char *anon_prefix;
	const char *ctor;
};

static struct sKindExtraInfo kindExtraInfo[KIND_COUNT] = {
	[K_FUNCTION] = {
		"anonFunc",
		"function",
	},
	[K_VECTOR] = {
		"anonVec",
		"c",
	},
	[K_LIST] = {
		"anonList",
		"list",
	},
	[K_DATAFRAME] = {
		"anonDataFrame",
		"data.frame",
	},
};

typedef enum {
	F_ASSIGNMENT_OPERATOR,
	F_CONSTRUCTOR,
} rField;

static fieldDefinition RFields [] = {
	{
		.name = "assignmentop",
		.description = "operator for assignment",
		.enabled = false,
	},
	{
		.name = "constructor",
		.description = "function used for making value assigned to the nameattr tag",
		.enabled = true,
	}
};

typedef int keywordId;			/* to allow KEYWORD_NONE */

static const keywordTable RKeywordTable [] = {
	{ "c",        KEYWORD_R_C        },
	{ "list",     KEYWORD_R_LIST     },
	{ "data.frame",KEYWORD_R_DATAFRAME },
	{ "function", KEYWORD_R_FUNCTION },
	{ "if",       KEYWORD_R_IF       },
	{ "else",     KEYWORD_R_ELSE     },
	{ "for",      KEYWORD_R_FOR      },
	{ "while",    KEYWORD_R_WHILE    },
	{ "repeat",   KEYWORD_R_REPEAT   },
	{ "in",       KEYWORD_R_IN       },
	{ "next",     KEYWORD_R_NEXT     },
	{ "break",    KEYWORD_R_BREAK    },
	{ "TRUE",     KEYWORD_R_TRUE,    },
	{ "FALSE",    KEYWORD_R_FALSE,   },
	{ "NULL",     KEYWORD_R_NULL,    },
	{ "Inf",      KEYWORD_R_INF,     },
	{ "NaN",      KEYWORD_R_NAN,     },
	{ "NA",       KEYWORD_R_NA,      },
	{ "NA_integer_",   KEYWORD_R_NA, },
	{ "NA_real_",      KEYWORD_R_NA, },
	{ "NA_complex_",   KEYWORD_R_NA, },
	{ "NA_character_", KEYWORD_R_NA, },
	{ "source",   KEYWORD_R_SOURCE   },
	{ "library",  KEYWORD_R_LIBRARY  },
	{ "require",  KEYWORD_R_LIBRARY  },
};

#ifdef DEBUG
static const char *tokenTypeStr(enum RTokenType e);
#endif

static struct tokenTypePair typePairs [] = {
	{ '{', '}' },
	{ '[', ']' },
	{ '(', ')' },
};

typedef struct sRToken {
	tokenInfo base;
	int scopeIndex;
	int parenDepth;
	vString *signature;
	int kindIndexForParams;		/* Used only when gathering parameters */
} rToken;

#define R(TOKEN) ((rToken *)TOKEN)

static int blackHoleIndex;

static langType Lang_R;

static void readToken (tokenInfo *const token, void *data);
static void clearToken (tokenInfo *token);
static struct tokenInfoClass rTokenInfoClass = {
	.nPreAlloc        = 4,
	.typeForUndefined = TOKEN_R_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_R_KEYWORD,
	.typeForEOF       = TOKEN_R_EOF,
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
static bool parseStatement (tokenInfo *const token, int parent, bool in_arglist, bool in_continuous_pair);
static void parsePair (tokenInfo *const token, int parent, tokenInfo *const funcall);

static  int notifyReadRightSideSymbol (tokenInfo *const symbol,
									   const char *const assignmentOperator,
									   int parent,
									   tokenInfo *const token);
static  int makeSimpleSubparserTag (int langType, tokenInfo *const token, int parent,
									bool in_func, int kindInR, const char *assignmentOperator);
static  bool askSubparserTagAcceptancy (tagEntryInfo *pe);
static  bool askSubparserTagHasFunctionAlikeKind (tagEntryInfo *e);
static  int notifyReadFuncall (tokenInfo *const func, tokenInfo *const token, int parent);

/*
*   FUNCTION DEFINITIONS
*/
static bool hasKindsOrCtors (tagEntryInfo * e, int kinds[], size_t count)
{
       if (e->langType == Lang_R)
	   {
		   for (size_t i = 0; i < count; i++)
		   {
			   if (e->kindIndex == kinds[i])
				   return true;
		   }
	   }
	   else
	   {
		   bool function = false;
		   for (size_t i = 0; i < count; i++)
		   {
			   if (K_FUNCTION == kinds[i])
			   {
				   function = true;
				   break;
			   }
		   }
		   if (function && askSubparserTagHasFunctionAlikeKind (e))
			   return true;
	   }

	   const char *tmp = getParserFieldValueForType (e,
													 RFields [F_CONSTRUCTOR].ftype);
	   if (tmp == NULL)
		   return false;

	   for (size_t i = 0; i < count; i++)
	   {
		   const char * ctor = kindExtraInfo [kinds[i]].ctor;
		   if (ctor && strcmp (tmp, ctor) == 0)
               return true;
	   }

       return false;
}

static int searchScopeOtherThan (int scope, int kinds[], size_t count)
{
	do
	{
		tagEntryInfo * e = getEntryInCorkQueue (scope);
		if (!e)
			return CORK_NIL;

		if (!hasKindsOrCtors (e, kinds, count))
			return scope;

		scope = e->extensionFields.scopeIndex;
	}
	while (1);
}

static int makeSimpleRTagR (tokenInfo *const token, int parent, int kind,
							const char * assignmentOp)
{
	if (assignmentOp && (strlen (assignmentOp) == 3))
	{
		/* <<- or ->> is used here. */
		if (anyKindsEntryInScopeRecursive (parent, tokenString (token),
										   (int[]){K_FUNCTION,
												   K_GLOBALVAR,
												   K_FUNCVAR,
												   K_PARAM}, 4) != CORK_NIL)
			return CORK_NIL;

		parent = CORK_NIL;
	}

	/* If the tag (T) to be created is defined in a scope and
	   the scope already has another tag having the same name
	   as T, T should not be created. */
	tagEntryInfo *pe = getEntryInCorkQueue (parent);
	int cousin = CORK_NIL;
	if (pe && ((pe->langType == Lang_R && pe->kindIndex == K_FUNCTION)
			   || (pe->langType != Lang_R && askSubparserTagHasFunctionAlikeKind (pe))))
	{
		cousin = anyEntryInScope (parent, tokenString (token));
		if (kind == K_GLOBALVAR)
			kind = K_FUNCVAR;
	}
	else if (pe && (kind == K_GLOBALVAR)
			 && hasKindsOrCtors (pe, (int[]){K_VECTOR, K_LIST, K_DATAFRAME}, 3))
	{
		parent = searchScopeOtherThan (pe->extensionFields.scopeIndex,
									   (int[]){K_VECTOR, K_LIST, K_DATAFRAME}, 3);
		if (parent == CORK_NIL)
			cousin = anyKindEntryInScope (parent, tokenString (token), K_GLOBALVAR);
		else
		{
			cousin = anyKindEntryInScope (parent, tokenString (token), K_FUNCVAR);
			kind = K_FUNCVAR;
		}
	}
	else if (pe)
	{
		/* The condition for tagging is a bit relaxed here.
		   Even if the same name tag is created in the scope, a name
		   is tagged if kinds are different. */
		cousin = anyKindEntryInScope (parent, tokenString (token), kind);
	}
	if (cousin != CORK_NIL)
		return CORK_NIL;

	int corkIndex = makeSimpleTag (token->string, kind);
	tagEntryInfo *tag = getEntryInCorkQueue (corkIndex);
	if (tag)
	{
		tag->extensionFields.scopeIndex = parent;
		if (assignmentOp)
		{
			if (strlen (assignmentOp) > 0)
				attachParserField (tag, true,
								   RFields [F_ASSIGNMENT_OPERATOR].ftype,
								   assignmentOp);
			else
				markTagExtraBit (tag, XTAG_ANONYMOUS);
		}
		registerEntry (corkIndex);
	}
	return corkIndex;
}

static int makeSimpleRTag (tokenInfo *const token, int parent, bool in_func, int kind,
						   const char * assignmentOp)
{
	int r;
	const char *ctor = kindExtraInfo [kind].ctor;
	tagEntryInfo *pe = (parent == CORK_NIL)? NULL: getEntryInCorkQueue (parent);

	/* makeTagWithTranslation method for subparsers
	   called from makeSimpleSubparserTag expects
	   kind should be resolved. */
	if (pe && hasKindsOrCtors (pe, (int[]){K_VECTOR, K_LIST, K_DATAFRAME}, 3))
	{
		if (assignmentOp
			&& strcmp (assignmentOp, "=") == 0)
			kind = K_NAMEATTR;
	}

	bool foreign_tag = false;
	if (pe == NULL || pe->langType == Lang_R ||
		!askSubparserTagAcceptancy (pe))
		r = makeSimpleRTagR (token, parent, kind, assignmentOp);
	else
	{
		foreign_tag = true;
		r = makeSimpleSubparserTag (pe->langType, token, parent, in_func,
									kind, assignmentOp);
	}

	if ((kind == K_NAMEATTR || foreign_tag) && ctor)
	{
		tagEntryInfo *e = getEntryInCorkQueue (r);
		if (e)
			attachParserField (e, true,
							   RFields [F_CONSTRUCTOR].ftype,
							   ctor);
	}

	return r;
}

static void clearToken (tokenInfo *token)
{
	R (token)->parenDepth = 0;
	R (token)->scopeIndex = CORK_NIL;
	R (token)->kindIndexForParams = KIND_GHOST_INDEX;
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
		char c = vStringChar (signature, i - 1);
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

	token->type = TOKEN_R_UNDEFINED;
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
		token->type = TOKEN_R_EOF;
		break;
	case '#':
		while (1)
		{
			c = getcFromInputFile ();
			if (c == EOF)
			{
				token->type = TOKEN_R_EOF;
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
		token->type = TOKEN_R_STRING;
		tokenPutc (token, c);
		readString (token, data);
		break;
	case '+':
	case '/':
	case '^':
	case '~':
		token->type = TOKEN_R_OPERATOR;
		tokenPutc (token, c);
		break;
	case ':':
		token->type = TOKEN_R_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == ':')
		{
			tokenPutc (token, c);
			token->type = TOKEN_R_SCOPE;
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
		token->type = TOKEN_R_OPERATOR;
		tokenPutc (token, c);
		c0 = getcFromInputFile ();
		if (c == c0)
			tokenPutc (token, c0);
		else
			ungetcToInputFile (c0);
		break;
	case '=':
		token->type = TOKEN_R_OPERATOR;
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
		token->type = TOKEN_R_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '>')
		{
			token->type = TOKEN_R_RASSIGN;
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
		token->type = TOKEN_R_OPERATOR;
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (c == '=')
			tokenPutc (token, c);
		else
			ungetcToInputFile (c);
		break;
	case '<':
		token->type = TOKEN_R_OPERATOR;
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
			token->type = TOKEN_R_LASSIGN;
			tokenPutc (token, c);
		}
		else if (c == '=')
			tokenPutc (token, c);
		else
			ungetcToInputFile (c);
		break;
	case '%':
		token->type = TOKEN_R_OPERATOR;
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
		token->type = TOKEN_R_OPERATOR;
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
	case '[':
	case ']':
	case ',':
	case '$':
	case '@':
		token->type = c;
		tokenPutc (token, c);
		break;
	case '.':
		tokenPutc (token, c);
		c = getcFromInputFile ();
		if (isdigit(c))
		{
			token->type = TOKEN_R_NUMBER;
			tokenPutc (token, c);
			readNumber(token, data);
		}
		else if (isalpha (c) || c == '_')
		{
			token->type = TOKEN_R_SYMBOL;
			tokenPutc (token, c);
			readSymbol (token, data);

			token->keyword = resolveKeyword (token->string);
			if (token->keyword != KEYWORD_NONE)
				token->type = TOKEN_R_KEYWORD;
		}
		else if (c == '.')
		{
			token->type = TOKEN_R_DOTS;
			tokenPutc (token, c);

			c = getcFromInputFile ();
			if (c == '.')
				tokenPutc (token, c);
			else if (isdigit(c))
			{
				token->type = TOKEN_R_DOTS_N;
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
				token->type = TOKEN_R_SYMBOL;
				tokenPutc (token, c);
				readSymbol (token, data);

				token->keyword = resolveKeyword (token->string);
				if (token->keyword != KEYWORD_NONE)
					token->type = TOKEN_R_KEYWORD;
			}
			else
			{
				token->type = TOKEN_R_UNDEFINED;
				ungetcToInputFile (c);
			}
		}
		break;
	default:
		tokenPutc (token, c);
		if (isdigit (c))
		{
			token->type = TOKEN_R_NUMBER;
			readNumber(token, data);
		}
		else if (isalpha (c))
		{
			token->type = TOKEN_R_SYMBOL;
			readSymbol (token, data);

			token->keyword = resolveKeyword (token->string);
			if (token->keyword != KEYWORD_NONE)
				token->type = TOKEN_R_KEYWORD;
		}
		else
			token->type = TOKEN_R_UNDEFINED;
		break;
	}

	/* Handle parameters in a signature */
	if (R(token)->signature && !tokenIsType(token, R_EOF) && !tokenIsTypeVal(token, '\n'))
	{
		vString *signature = R (token)->signature;

		if (tokenIsTypeVal (token, '('))
			R (token)->parenDepth++;
		else if (tokenIsTypeVal (token, ')'))
			R (token)->parenDepth--;

		if (R (token)->kindIndexForParams != KIND_GHOST_INDEX
			&& R (token)->parenDepth == 1 && tokenIsType (token, R_SYMBOL)
			&& signatureExpectingParameter (signature))
			makeSimpleRTag (token, R (token)->scopeIndex, false,
							R (token)->kindIndexForParams, NULL);

		if (vStringLast (signature) != '(' &&
			!tokenIsTypeVal (token, ',') &&
			!tokenIsTypeVal (token, ')'))
			vStringPut (signature, ' ');
		vStringCat (signature, token->string);
	}
}

#define newRToken rNewToken
extern tokenInfo *rNewToken (void)
{
	return newToken (&rTokenInfoClass);
}

#define tokenReadNoNewline rTokenReadNoNewline
extern void rTokenReadNoNewline (tokenInfo *const token)
{
	while (1)
	{
		tokenRead(token);
		if (!tokenIsTypeVal (token, '\n'))
			break;
	}
}

static void setupCollectingSignature (tokenInfo *const token,
									  vString   *signature,
									  int kindIndexForParams,
									  int corkIndex)
{
	R (token)->signature = signature;
	R (token)->kindIndexForParams = kindIndexForParams;
	R (token)->scopeIndex = corkIndex;
	R (token)->parenDepth = 1;
}

extern void rSetupCollectingSignature (tokenInfo *const token,
									   vString   *signature)
{
	setupCollectingSignature (token, signature,
							  KIND_GHOST_INDEX, CORK_NIL);
}

static void teardownCollectingSignature (tokenInfo *const token)
{
	R (token)->parenDepth = 0;
	R (token)->scopeIndex = CORK_NIL;
	R (token)->kindIndexForParams = KIND_GHOST_INDEX;
	R (token)->signature = NULL;
}

extern void rTeardownCollectingSignature (tokenInfo *const token)
{
	teardownCollectingSignature (token);
}

static int getKindForToken (tokenInfo *const token)
{
	if (tokenIsKeyword (token, R_FUNCTION))
		return K_FUNCTION;
	else if (tokenIsKeyword (token, R_C))
		return K_VECTOR;
	else if (tokenIsKeyword (token, R_LIST))
		return K_LIST;
	else if (tokenIsKeyword (token, R_DATAFRAME))
		return K_DATAFRAME;
	return K_GLOBALVAR;
}

static bool findNonPlaceholder (int corkIndex, tagEntryInfo *entry, void *data)
{
	bool *any_non_placehoders = data;
	if (!entry->placeholder)
	{
		*any_non_placehoders = true;
		return false;
	}
	return true;
}

static void parseRightSide (tokenInfo *const token, tokenInfo *const symbol, int parent)
{
	R_TRACE_ENTER();

	char *const assignment_operator = eStrdup (tokenString (token));
	vString *signature = NULL;

	tokenReadNoNewline (token);

	int kind = getKindForToken (token);

	/* Call sub parsers */
	int corkIndex = notifyReadRightSideSymbol (symbol,
											   assignment_operator,
											   parent,
											   token);
	if (corkIndex == CORK_NIL)
	{
		/* No subparser handle the symbol */
		corkIndex = makeSimpleRTag (symbol, parent, kind == K_FUNCTION,
									kind,
									assignment_operator);
	}

	if (kind == K_FUNCTION)
	{
		/* parse signature */
		tokenReadNoNewline (token);
		if (tokenIsTypeVal (token, '('))
		{
			if (corkIndex == CORK_NIL)
				tokenSkipOverPair (token);
			else
			{
				signature = vStringNewInit("(");
				setupCollectingSignature (token, signature, K_PARAM, corkIndex);
				tokenSkipOverPair (token);
				teardownCollectingSignature (token);
			}
			tokenReadNoNewline (token);
		}
		parent = (corkIndex == CORK_NIL
				  ? blackHoleIndex
				  : corkIndex);
	}
	else if (kind == K_VECTOR || kind == K_LIST || kind == K_DATAFRAME)
	{
		tokenRead (token);
		parsePair (token, corkIndex, NULL);
		tokenRead (token);
		parent = corkIndex;
	}

	R_TRACE_TOKEN_TEXT("body", token, parent);

	parseStatement (token, parent, false, false);

	tagEntryInfo *tag = getEntryInCorkQueue (corkIndex);
	if (tag)
	{
		tag->extensionFields.endLine = token->lineNumber;
		if (signature)
		{
			tag->extensionFields.signature = vStringDeleteUnwrap(signature);
			signature = NULL;
		}
		/* If a vector has no named attribte and it has no lval,
		 * we don't make a tag for the vector. */
		if ((kind == K_VECTOR || kind == K_LIST || kind == K_DATAFRAME)
			&& *assignment_operator == '\0')
		{
			bool any_non_placehoders = false;
			foreachEntriesInScope (corkIndex, NULL,
								   findNonPlaceholder, &any_non_placehoders);
			if (!any_non_placehoders)
				tag->placeholder = 1;
		}
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
	if (tokenIsType (prefetch_token, R_SYMBOL)
		|| tokenIsType (prefetch_token, R_STRING))
	{
		tokenInfo *const loaded_obj_token = newTokenByCopying (prefetch_token);
		tokenReadNoNewline (prefetch_token);
		if (tokenIsTypeVal (prefetch_token, ')')
			|| tokenIsTypeVal (prefetch_token, ','))
		{
			if (tokenIsTypeVal (prefetch_token, ')'))
				r = false;

			makeSimpleRefTag (loaded_obj_token->string,
							  (tokenIsKeyword (funcall, R_LIBRARY)
							   ? K_LIBRARY
							   : K_SOURCE),
							  (tokenIsKeyword (funcall, R_LIBRARY)
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

static bool preParseLoopCounter(tokenInfo *const token, int parent)
{
	bool r = true;
	TRACE_ENTER();

	tokenReadNoNewline (token);
	if (tokenIsType (token, R_SYMBOL))
		makeSimpleRTag (token, parent, false, K_GLOBALVAR, NULL);

	if (tokenIsEOF (token)
		|| tokenIsTypeVal (token, ')'))
		r = false;

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
		|| tokenIsTypeVal (token, '[');
	bool is_funcall = funcall && tokenIsTypeVal (token, '(');
	bool done = false;

	if (is_funcall)
	{
		if 	(tokenIsKeyword (funcall, R_LIBRARY) ||
			 tokenIsKeyword (funcall, R_SOURCE))
			done = !preParseExternalEntitiy (token, funcall);
		else if (tokenIsKeyword (funcall, R_FOR))
			done = !preParseLoopCounter (token, parent);
		else if (notifyReadFuncall (funcall, token, parent) != CORK_NIL)
			done = true;
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
		parseStatement (token, parent, (funcall != NULL), in_continuous_pair);
	}
	while (! (tokenIsEOF (token)
			  || tokenIsTypeVal (token, ')')
			  || tokenIsTypeVal (token, '}')
			  || tokenIsTypeVal (token, ']')));
	R_TRACE_LEAVE();
}

static bool isAtConstructorInvocation (void)
{
	bool r = false;

	tokenInfo *const token = newRToken ();
	tokenRead (token);
	if (tokenIsTypeVal (token, '('))
		r = true;
	tokenUnread (token);
	tokenDelete (token);
	return r;
}

static bool parseStatement (tokenInfo *const token, int parent,
							bool in_arglist, bool in_continuous_pair)
{
	R_TRACE_ENTER();
	int last_count = rTokenInfoClass.read_counter;

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
		else if ((tokenIsKeyword (token, R_FUNCTION)
				  || ((tokenIsKeyword (token, R_C)
					   || tokenIsKeyword (token, R_LIST)
					   || tokenIsKeyword (token, R_DATAFRAME))
					  && isAtConstructorInvocation ())))
		{
			/* This statement doesn't start with a symbol.
			 * This function is not assigned to any symbol. */
			tokenInfo *const anonfunc = newTokenByCopying (token);
			int kind = getKindForToken (token);
			anonGenerate (anonfunc->string,
						  kindExtraInfo [kind].anon_prefix, kind);
			tokenUnread (token);
			vStringClear (token->string);
			parseRightSide (token, anonfunc, parent);
			tokenDelete (anonfunc);
		}
		else if (tokenIsType (token, R_SYMBOL)
				 || tokenIsType (token, R_STRING)
				 || tokenIsType (token, R_KEYWORD))
		{
			tokenInfo *const symbol = newTokenByCopying (token);

			if (in_continuous_pair)
				tokenReadNoNewline (token);
			else
				tokenRead (token);

			if (tokenIsType (token, R_LASSIGN))
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
				if (in_arglist)
				{
					/* Ignore the left side symbol. */
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
					 || tokenIsType (token, R_SCOPE))
			{
				tokenReadNoNewline (token); /* Skip the next identifier */
				tokenRead (token);
				R_TRACE_TOKEN_TEXT("after $", token, parent);
			}
			else
				R_TRACE_TOKEN_TEXT("else after symbol", token, parent);
			tokenDelete(symbol);
		}
		else if (tokenIsType (token, R_RASSIGN))
		{
			char *const assignment_operator = eStrdup (tokenString (token));
			tokenReadNoNewline (token);
			if (tokenIsType (token, R_SYMBOL)
				|| tokenIsType (token, R_STRING))
			{
				makeSimpleRTag (token, parent, false,
								K_GLOBALVAR, assignment_operator);
				tokenRead (token);
			}
			eFree (assignment_operator);
			R_TRACE_TOKEN_TEXT("after ->", token, parent);
		}
		else if (tokenIsType (token, R_OPERATOR))
		{
			tokenReadNoNewline (token);
			R_TRACE_TOKEN_TEXT("after operator", token, parent);
		}
		else if (tokenIsTypeVal (token, '(')
				 || tokenIsTypeVal (token, '{')
				 || tokenIsTypeVal (token, '['))
		{
			parsePair (token, parent, NULL);
			tokenRead (token);
			R_TRACE_TOKEN_TEXT("after pair", token, parent);
		}
		else if (tokenIsTypeVal (token, ')')
				 || tokenIsTypeVal (token, '}')
				 || tokenIsTypeVal (token, ']'))
		{
			R_TRACE_TOKEN_TEXT ("break with close", token, parent);
			break;
		}
		else if (tokenIsTypeVal (token, '$')
				 || tokenIsTypeVal (token, '@')
				 || tokenIsType (token, R_SCOPE))
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

	return (last_count != rTokenInfoClass.read_counter);
}

extern bool rParseStatement (tokenInfo *const token, int parentIndex, bool in_arglist)
{
	pushLanguage (Lang_R);
	bool r = parseStatement (token, parentIndex, in_arglist, true);
	popLanguage ();
	return r;
}

static  int notifyReadRightSideSymbol (tokenInfo *const symbol,
									   const char *const assignmentOperator,
									   int parent,
									   tokenInfo *const token)
{
	subparser *sub;
	int q = CORK_NIL;

	foreachSubparser (sub, false)
	{
		rSubparser *rsub = (rSubparser *)sub;
		if (rsub->readRightSideSymbol)
		{
			enterSubparser (sub);
			q = rsub->readRightSideSymbol (rsub, symbol, assignmentOperator, parent, token);
			leaveSubparser ();
			if (q != CORK_NIL)
				break;
		}
	}

	return q;
}

static  int makeSimpleSubparserTag (int langType,
									tokenInfo *const token, int parent,
									bool in_func, int kindInR,
									const char *assignmentOperator)
{
	int q = CORK_NIL;
	subparser *sub = getLanguageSubparser (langType, false);
	if (sub)
	{
		rSubparser *rsub = (rSubparser *)sub;
		if (rsub->makeTagWithTranslation)
		{
			enterSubparser (sub);
			q = rsub->makeTagWithTranslation (rsub,
											  token, parent,
											  in_func, kindInR,
											  assignmentOperator);
			leaveSubparser ();
		}
	}
	return q;
}

static  bool askSubparserTagAcceptancy (tagEntryInfo *pe)
{
	bool q = false;
	subparser *sub = getLanguageSubparser (pe->langType, false);
	{
		rSubparser *rsub = (rSubparser *)sub;
		if (rsub->askTagAcceptancy)
		{
			enterSubparser (sub);
			q = rsub->askTagAcceptancy (rsub, pe);
			leaveSubparser ();
		}
	}
	return q;
}

static  bool askSubparserTagHasFunctionAlikeKind (tagEntryInfo *e)
{
	bool q = false;
	pushLanguage (Lang_R);
	subparser *sub = getLanguageSubparser (e->langType, false);
	Assert (sub);
	popLanguage ();
	rSubparser *rsub = (rSubparser *)sub;
	if (rsub->hasFunctionAlikeKind)
	{
		enterSubparser (sub);
		q = rsub->hasFunctionAlikeKind (rsub, e);
		leaveSubparser ();
	}
	return q;
}

static  int notifyReadFuncall (tokenInfo *const func,
							   tokenInfo *const token,
							   int parent)
{
	int q = CORK_NIL;
	subparser *sub;
	foreachSubparser (sub, false)
	{
		rSubparser *rsub = (rSubparser *)sub;
		if (rsub->readFuncall)
		{
			enterSubparser (sub);
			q = rsub->readFuncall (rsub, func, token, parent);
			leaveSubparser ();
			if (q != CORK_NIL)
				break;
		}
	}
	return q;
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
		parseStatement (token, CORK_NIL, false, false);
	}
	while (!tokenIsEOF (token));

	TRACE_PRINT ("run blackhole", blackHoleIndex);
	markAllEntriesInScopeAsPlaceholder (blackHoleIndex);

	tokenDelete (token);
}

static void initializeRParser (const langType language)
{
	Lang_R = language;
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
	def->initialize = initializeRParser;

	return def;
}

extern vString *rExtractNameFromString (vString* str)
{
	int offset = 0;

	if (vStringLength (str) == 0)
		return NULL;

	char b = vStringChar (str, 0);
	if (b == '\'' || b == '"' || b == '`')
		offset = 1;

	if (offset && vStringLength (str) < 3)
		return NULL;

	vString *n = vStringNewInit (vStringValue (str) + offset);
	if (vStringChar (n, vStringLength (n) - 1) == b)
		vStringChop (n);

	return n;
}

#ifdef DEBUG
static const char *tokenTypeStr(enum RTokenType e)
{ /* Generated by misc/enumstr.sh with cmdline:
     parsers/r.c RTokenType tokenTypeStr TOKEN_R_ --use-lower-bits-as-is */
	switch (e)
	{
		case            TOKEN_R_EOF: return "EOF";
		case      TOKEN_R_UNDEFINED: return "UNDEFINED";
		case        TOKEN_R_KEYWORD: return "KEYWORD";
		case        TOKEN_R_NEWLINE: return "NEWLINE";
		case         TOKEN_R_NUMBER: return "NUMBER";
		case         TOKEN_R_SYMBOL: return "SYMBOL";
		case         TOKEN_R_STRING: return "STRING";
		case       TOKEN_R_OPERATOR: return "OPERATOR";
		case           TOKEN_R_DOTS: return "DOTS";
		case         TOKEN_R_DOTS_N: return "DOTS_N";
		case        TOKEN_R_LASSIGN: return "LASSIGN";
		case        TOKEN_R_RASSIGN: return "RASSIGN";
		case          TOKEN_R_SCOPE: return "SCOPE";
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
