/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2014-2016, Colomban Wendling <ban@herbesfolles.org>
*	Copyright (c) 2021, David Yang <davidyang6us@gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for GDScript language
*   files. This module is derived from the Python module.
*
*	GDScript language reference:
*	https://docs.godotengine.org/en/latest/tutorials/scripting/gdscript/gdscript_basics.html
*	https://docs.godotengine.org/en/stable/development/file_formats/gdscript_grammar.html#doc-gdscript-grammar
*	https://godotengine.org/article/gdscript-progress-report-new-gdscript-now-merged
*
*/

#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "nestlevel.h"
#include "read.h"
#include "parse.h"
#include "vstring.h"
#include "keyword.h"
#include "routines.h"
#include "debug.h"
#include "xtag.h"
#include "objpool.h"
#include "strlist.h"

#define isIdentifierChar(c) \
	(isalnum (c) || (c) == '_' || (c) >= 0x80)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))

enum {
	KEYWORD_class,
	KEYWORD_func,
	KEYWORD_extends,
	KEYWORD_pass,
	KEYWORD_return,
	KEYWORD_lambda,
	KEYWORD_variable,
	KEYWORD_const,
	KEYWORD_enum,
	KEYWORD_class_name,
	KEYWORD_signal,
	KEYWORD_modifier,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum {
	ACCESS_PRIVATE,
	ACCESS_PROTECTED,
	ACCESS_PUBLIC,
	COUNT_ACCESS
} accessType;

static const char *const GDScriptAccesses[COUNT_ACCESS] = {
	"private",
	"protected",
	"public"
};

typedef enum {
	F_ANNOTATIONS,
	COUNT_FIELD
} gdscriptField;

static fieldDefinition GDScriptFields[COUNT_FIELD] = {
	{ .name = "annotations",
	  .description = "annotations on functions and variables",
	  .enabled = true },
};

typedef enum {
	K_CLASS,
	K_METHOD,
	K_VARIABLE,
	K_CONST,
	K_ENUM,
	K_ENUMERATOR,
	K_PARAMETER,
	K_LOCAL_VARIABLE,
	K_SIGNAL,
	COUNT_KIND
} gdscriptKind;

typedef enum {
	GDSCRIPT_CLASS_EXTENDED,
} gdscriptClassRole;

static roleDefinition GDScriptClassRoles [] = {
	{ true, "extended",   "used as a base class for extending" },
};

static kindDefinition GDScriptKinds[COUNT_KIND] = {
	{true, 'c', "class",	"classes",
	 .referenceOnly = false, ATTACH_ROLES(GDScriptClassRoles)},
	{true, 'm', "method",	"methods"},
	{true, 'v', "variable",	"variables"},
	{true, 'C', "const", "constants"},
	{true, 'g', "enum",	"enumeration names"},
	{true, 'e', "enumerator",	"enumerated values"},
	{false,'z', "parameter",	"function parameters"},
	{false,'l', "local",	"local variables"},
	{true, 's', "signal",   "signals"},
};

typedef enum {
	X_IMPLICIT_CLASS,
} gdscriptXtag;

static xtagDefinition GDScriptXtagTable [] = {
	{
		.enabled     = false,
		.name        = "implicitClass",
		.description = "Include tag for the implicitly defined unnamed class",
	},
};

static const keywordTable GDScriptKeywordTable[] = {
	/* keyword			keyword ID */
	{ "class",			KEYWORD_class			},
	{ "func",			KEYWORD_func			},
	{ "extends",		KEYWORD_extends			},
	{ "lambda",			KEYWORD_lambda			}, // Future GDScript lambda currently uses func, may change
	{ "pass",			KEYWORD_pass			},
	{ "return",			KEYWORD_return			},
	{ "var",			KEYWORD_variable		},
	{ "const",			KEYWORD_const			},
	{ "enum",			KEYWORD_enum			},
	{ "class_name",		KEYWORD_class_name		},
	{ "signal",			KEYWORD_signal			},

};

const static struct keywordGroup modifierKeywords = {
	.value = KEYWORD_modifier,
	.addingUnlessExisting = false,
	.keywords = {
		"static",
		"remote", "remotesync",
		"master", "mastersycn",
		"puppet", "puppetsync",
		NULL,
	},
};

typedef enum eTokenType {
	/* 0..255 are the byte's value */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_INDENT,
	TOKEN_KEYWORD,
	TOKEN_OPERATOR,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_ARROW,				/* -> */
	TOKEN_WHITESPACE,
} tokenType;

typedef struct {
	int				type;
	keywordId		keyword;
	vString *		string;
	int				indent;
	unsigned long	lineNumber;
	MIOPos			filePosition;
} tokenInfo;

struct gdscriptNestingLevelUserData {
	int indentation;
};
#define GDS_NL(nl) ((struct gdscriptNestingLevelUserData *) nestingLevelGetUserData (nl))

static langType Lang_gdscript;
static unsigned int TokenContinuationDepth = 0;
static tokenInfo *NextToken = NULL;
static NestingLevels *GDScriptNestingLevels = NULL;
static objPool *TokenPool = NULL;


// Always reports single-underscores as protected
static accessType accessFromIdentifier (const vString *const ident, int parentKind)
{
	const char *const p = vStringValue (ident);
	const size_t len = vStringLength (ident);

	/* inside a function/method, private */
	if (parentKind != -1 && parentKind != K_CLASS)
		return ACCESS_PRIVATE;
	/* not starting with "_", public */
	else if (len < 1 || p[0] != '_')
		return ACCESS_PUBLIC;
	/* "_...": suggested as non-public, but easily accessible */
	else
		return ACCESS_PROTECTED;
}

static void initGDScriptEntry (tagEntryInfo *const e, const tokenInfo *const token,
							 const gdscriptKind kind)
{
	accessType access;
	int parentKind = -1;
	NestingLevel *nl;

	initTagEntry (e, vStringValue (token->string), kind);

	e->lineNumber	= token->lineNumber;
	e->filePosition	= token->filePosition;

	nl = nestingLevelsGetCurrent (GDScriptNestingLevels);
	if (nl)
	{
		tagEntryInfo *nlEntry = getEntryOfNestingLevel (nl);

		e->extensionFields.scopeIndex = nl->corkIndex;

		/* nlEntry can be NULL if a kind was disabled.  But what can we do
		 * here?  Even disabled kinds should count for the hierarchy I
		 * guess -- as it'd otherwise be wrong -- but with cork we're
		 * fucked up as there's nothing to look up.  Damn. */
		if (nlEntry)
			parentKind = nlEntry->kindIndex;
	}

	access = accessFromIdentifier (token->string, parentKind);
	e->extensionFields.access = GDScriptAccesses[access];
	/* FIXME: should we really set isFileScope in addition to access? */
	if (access == ACCESS_PRIVATE)
		e->isFileScope = true;
}

static int makeClassTag (const tokenInfo *const token,
						 const vString *const inheritance)
{
	if (GDScriptKinds[K_CLASS].enabled)
	{
		tagEntryInfo e;

		initGDScriptEntry (&e, token, K_CLASS);

		e.extensionFields.inheritance = inheritance ? vStringValue (inheritance) : "";

		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static vString *makeDecoratorString (const stringList *const strlist)
{
	vString *vstr = vStringNew ();

	for (unsigned int i = 0; i < stringListCount (strlist); i++)
	{
		vString *elt = stringListItem (strlist, i);
		if (i != 0 && (vStringValue (elt) > 0
					   && vStringValue (elt)[0] != '('))
			vStringPut (vstr, ',');
		vStringCat (vstr, elt);
	}
	return vstr;
}

static int makeFunctionTag (const tokenInfo *const token,
							int kind,
							const vString *const arglist,
							const stringList *const decorators)
{
	if (GDScriptKinds[kind].enabled)
	{
		tagEntryInfo e;
		vString *vstr = NULL;
		int r;

		initGDScriptEntry (&e, token, kind);

		if (arglist)
			e.extensionFields.signature = vStringValue (arglist);
		if (decorators && stringListCount (decorators) > 0)
		{
			vstr = makeDecoratorString (decorators);
			attachParserField (&e, false, GDScriptFields[F_ANNOTATIONS].ftype,
							   vStringValue (vstr));
		}

		r = makeTagEntry (&e);
		vStringDelete (vstr);	/* NULL is ignored. */

		return r;
	}

	return CORK_NIL;
}

static int makeSimpleGDScriptTag (const tokenInfo *const token, gdscriptKind const kind)
{
	if (GDScriptKinds[kind].enabled)
	{
		tagEntryInfo e;

		initGDScriptEntry (&e, token, kind);
		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static int makeSimpleGDScriptRefTag (const tokenInfo *const token,
									 gdscriptKind const kind,
									 int roleIndex, xtagType xtag)
{
	if (isXtagEnabled (XTAG_REFERENCE_TAGS))
	{
		tagEntryInfo e;

		initRefTagEntry (&e, vStringValue (token->string),
						 kind, roleIndex);

		e.lineNumber	= token->lineNumber;
		e.filePosition	= token->filePosition;

		if (xtag != XTAG_UNKNOWN)
			markTagExtraBit (&e, xtag);

		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->string = vStringNew ();
	return token;
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	vStringDelete (token->string);
	eFree (token);
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->indent		= 0;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	dest->indent = src->indent;
	vStringCopy(dest->string, src->string);
}

/* Skip a single or double quoted string. */
static void readString (vString *const string, const int delimiter)
{
	int escaped = 0;
	int c;

	while ((c = getcFromInputFile ()) != EOF)
	{
		if (escaped)
		{
			vStringPut (string, c);
			escaped--;
		}
		else if (c == '\\')
			escaped++;
		else if (c == delimiter || c == '\n' || c == '\r')
		{
			if (c != delimiter)
				ungetcToInputFile (c);
			break;
		}
		else
			vStringPut (string, c);
	}
}

/* Skip a single or double triple quoted string. */
static void readTripleString (vString *const string, const int delimiter)
{
	int c;
	int escaped = 0;
	int n = 0;
	while ((c = getcFromInputFile ()) != EOF)
	{
		if (c == delimiter && ! escaped)
		{
			if (++n >= 3)
				break;
		}
		else
		{
			for (; n > 0; n--)
				vStringPut (string, delimiter);
			if (c != '\\' || escaped)
				vStringPut (string, c);
			n = 0;
		}

		if (escaped)
			escaped--;
		else if (c == '\\')
			escaped++;
	}
}

static void readIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	}
	while (isIdentifierChar (c));
	ungetcToInputFile (c);
}

static void ungetToken (tokenInfo *const token)
{
	Assert (NextToken == NULL);
	NextToken = newToken ();
	copyToken (NextToken, token);
}

static void readTokenFull (tokenInfo *const token, bool inclWhitespaces)
{
	int c;
	int n;

	/* if we've got a token held back, emit it */
	if (NextToken)
	{
		copyToken (token, NextToken);
		deleteToken (NextToken);
		NextToken = NULL;
		return;
	}

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	n = 0;
	do
	{
		c = getcFromInputFile ();
		n++;
	}
	while (c == ' ' || c == '\t' || c == '\f');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	if (inclWhitespaces && n > 1 && c != '\r' && c != '\n')
	{
		ungetcToInputFile (c);
		vStringPut (token->string, ' ');
		token->type = TOKEN_WHITESPACE;
		return;
	}

	switch (c)
	{
		case EOF:
			token->type = TOKEN_EOF;
			break;

		case '\'':
		case '"':
		{
			int d = getcFromInputFile ();
			token->type = TOKEN_STRING;
			vStringPut (token->string, c);
			if (d != c)
			{
				ungetcToInputFile (d);
				readString (token->string, c);
			}
			else if ((d = getcFromInputFile ()) == c)
				readTripleString (token->string, c);
			else /* empty string */
				ungetcToInputFile (d);
			vStringPut (token->string, c);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			break;
		}

		case '=':
		{
			int d = getcFromInputFile ();
			vStringPut (token->string, c);
			if (d == c)
			{
				vStringPut (token->string, d);
				token->type = TOKEN_OPERATOR;
			}
			else
			{
				ungetcToInputFile (d);
				token->type = c;
			}
			break;
		}

		case '-':
		{
			int d = getcFromInputFile ();
			if (d == '>')
			{
				vStringPut (token->string, c);
				vStringPut (token->string, d);
				token->type = TOKEN_ARROW;
				break;
			}
			ungetcToInputFile (d);
			/* fall through */
		}
		case '+':
		case '*':
		case '%':
		case '<':
		case '>':
		case '/':
		{
			int d = getcFromInputFile ();
			vStringPut (token->string, c);
			if (d != '=')
			{
				ungetcToInputFile (d);
				token->type = c;
			}
			else
			{
				vStringPut (token->string, d);
				token->type = TOKEN_OPERATOR;
			}
			break;
		}

		/* eats newline to implement line continuation  */
		case '\\':
		{
			int d = getcFromInputFile ();
			if (d == '\r')
				d = getcFromInputFile ();
			if (d != '\n')
				ungetcToInputFile (d);
			goto getNextChar;
		}

		case '#': /* comment */
		case '\r': /* newlines for indent */
		case '\n':
		{
			int indent = 0;
			do
			{
				if (c == '#')
				{
					do
						c = getcFromInputFile ();
					while (c != EOF && c != '\r' && c != '\n');
				}
				if (c == '\r')
				{
					int d = getcFromInputFile ();
					if (d != '\n')
						ungetcToInputFile (d);
				}
				indent = 0;
				while ((c = getcFromInputFile ()) == ' ' || c == '\t' || c == '\f')
				{
					if (c == '\t')
						indent += 8 - (indent % 8);
					else if (c == '\f') /* yeah, it's weird */
						indent = 0;
					else
						indent++;
				}
			} /* skip completely empty lines, so retry */
			while (c == '\r' || c == '\n' || c == '#');
			ungetcToInputFile (c);
			if (TokenContinuationDepth > 0)
			{
				if (inclWhitespaces)
				{
					vStringPut (token->string, ' ');
					token->type = TOKEN_WHITESPACE;
				}
				else
					goto getNextChar;
			}
			else
			{
				token->type = TOKEN_INDENT;
				token->indent = indent;
			}
			break;
		}

		default:
			if (! isIdentifierChar (c))
			{
				vStringPut (token->string, c);
				token->type = c;
			}
			else
			{
				/* FIXME: handle U, B, R and F string prefixes? */
				readIdentifier (token->string, c);
				token->keyword = lookupKeyword (vStringValue (token->string), Lang_gdscript);
				if (token->keyword == KEYWORD_NONE)
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}

	// handle implicit continuation lines not to emit INDENT inside brackets
	if (token->type == '(' ||
		token->type == '{' ||
		token->type == '[')
	{
		TokenContinuationDepth ++;
	}
	else if (TokenContinuationDepth > 0 &&
			 (token->type == ')' ||
			  token->type == '}' ||
			  token->type == ']'))
	{
		TokenContinuationDepth --;
	}
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, false);
}

/*================================= parsing =================================*/


static void reprCat (vString *const repr, const tokenInfo *const token)
{
	if (token->type != TOKEN_INDENT &&
		token->type != TOKEN_WHITESPACE)
	{
		vStringCat (repr, token->string);
	}
	else if (vStringLength (repr) > 0 && vStringLast (repr) != ' ')
	{
		vStringPut (repr, ' ');
	}
}

static bool skipOverPair (tokenInfo *const token, int tOpen, int tClose,
						  vString *const repr, bool reprOuterPair)
{
	if (token->type == tOpen)
	{
		int depth = 1;

		if (repr && reprOuterPair)
			reprCat (repr, token);
		do
		{
			readTokenFull (token, true);
			if (repr && (reprOuterPair || token->type != tClose || depth > 1))
			{
				reprCat (repr, token);
			}
			if (token->type == tOpen)
				depth ++;
			else if (token->type == tClose)
				depth --;
		}
		while (token->type != TOKEN_EOF && depth > 0);
	}

	return token->type == tClose;
}

static void readQualifiedName (tokenInfo *const nameToken)
{
	readToken (nameToken);

	if (nameToken->type == TOKEN_IDENTIFIER ||
		nameToken->type == '.')
	{
		vString *qualifiedName = vStringNew ();
		tokenInfo *token = newToken ();

		while (nameToken->type == TOKEN_IDENTIFIER ||
			   nameToken->type == '.')
		{
			vStringCat (qualifiedName, nameToken->string);
			copyToken (token, nameToken);

			readToken (nameToken);
		}
		/* put the last, non-matching, token back */
		ungetToken (nameToken);

		copyToken (nameToken, token);
		nameToken->type = TOKEN_IDENTIFIER;
		vStringCopy (nameToken->string, qualifiedName);

		deleteToken (token);
		vStringDelete (qualifiedName);
	}
}

static vString *parseParamTypeAnnotation (tokenInfo *const token,
										  vString *arglist)
{
	readToken (token);
	if (token->type != ':')
	{
		ungetToken (token);
		return NULL;
	}

	reprCat (arglist, token);
	int depth = 0;
	vString *t = vStringNew ();
	while (true)
	{
		readTokenFull (token, true);
		if (token->type == TOKEN_WHITESPACE)
		{
			reprCat (arglist, token);
			continue;
		}
		else if (token->type == TOKEN_EOF)
			break;

		if (token->type == '(' ||
			token->type == '[' ||
			token->type == '{')
			depth ++;
		else if (token->type == ')' ||
				 token->type == ']' ||
				 token->type == '}')
			depth --;

		if (depth < 0
			|| (depth == 0 && (token->type == '='
							   || token->type == ',')))
		{
			ungetToken (token);
			return t;
		}
		reprCat (arglist, token);
		reprCat (t, token);
	}
	vStringDelete (t);
	return NULL;
}

static vString *parseReturnTypeAnnotation (tokenInfo *const token)
{
	readToken (token);
	if (token->type != TOKEN_ARROW)
	{
		return NULL;
	}

	int depth = 0;
	vString *t = vStringNew ();
	while (true)
	{
		readToken (token);
		if (token->type == TOKEN_EOF)
			break;

		if (token->type == '(' ||
			token->type == '[' ||
			token->type == '{')
			depth ++;
		else if (token->type == ')' ||
				 token->type == ']' ||
				 token->type == '}')
			depth --;
		if (depth == 0 && token->type == ':')
		{
			ungetToken (token);
			return t;
		}
		else
			reprCat (t, token);
	}
	vStringDelete (t);
	return NULL;
}

static bool parseClassOrDef (tokenInfo *const token,
							 const stringList *const decorators,
							 gdscriptKind kind)
{
	vString *arglist = NULL;
	tokenInfo *name = NULL;
	tokenInfo *parameterTokens[16] = { NULL };
	vString   *parameterTypes [ARRAY_SIZE(parameterTokens)] = { NULL };
	unsigned int parameterCount = 0;
	NestingLevel *lv;
	int corkIndex;

	readToken (token);
	if (token->type != TOKEN_IDENTIFIER)
		return false;

	name = newToken ();
	copyToken (name, token);

	readToken (token);
	/* collect parameters or inheritance */
	if (token->type == '(')
	{
		int prevTokenType = token->type;
		int depth = 1;

		arglist = vStringNew ();
		if (kind != K_CLASS)
			reprCat (arglist, token);

		do
		{
			if (token->type != TOKEN_WHITESPACE &&
				token->type != '*')
			{
				prevTokenType = token->type;
			}

			readTokenFull (token, true);
			if (kind != K_CLASS || token->type != ')' || depth > 1)
				reprCat (arglist, token);

			if (token->type == '(' ||
				token->type == '[' ||
				token->type == '{')
				depth ++;
			else if (token->type == ')' ||
					 token->type == ']' ||
					 token->type == '}')
				depth --;
			else if (kind != K_CLASS && depth == 1 &&
					 token->type == TOKEN_IDENTIFIER &&
					 (prevTokenType == '(' || prevTokenType == ',') &&
					 parameterCount < ARRAY_SIZE (parameterTokens) &&
					 GDScriptKinds[K_PARAMETER].enabled)
			{
				tokenInfo *parameterName = newToken ();

				copyToken (parameterName, token);
				parameterTokens[parameterCount] = parameterName;
				parameterTypes [parameterCount++] = parseParamTypeAnnotation (token, arglist);
			}
		}
		while (token->type != TOKEN_EOF && depth > 0);
	}
	else if (token->type == TOKEN_KEYWORD && token->keyword == KEYWORD_extends)
	{
		readToken (token);
		if (token->type == TOKEN_IDENTIFIER)
		{
			makeSimpleGDScriptRefTag (token, K_CLASS, GDSCRIPT_CLASS_EXTENDED, XTAG_UNKNOWN);
			arglist = vStringNewCopy (token->string);
		}
	}
	else if (kind == K_SIGNAL)
	{
		/* signal can be defined with no parameter list. */
		ungetToken (token);
	}

	if (kind == K_CLASS)
		corkIndex = makeClassTag (name, arglist);
	else
		corkIndex = makeFunctionTag (name, kind, arglist, decorators);

	lv = nestingLevelsPush (GDScriptNestingLevels, corkIndex);
	GDS_NL (lv)->indentation = token->indent;

	deleteToken (name);
	vStringDelete (arglist);

	if (parameterCount > 0)
	{
		unsigned int i;

		for (i = 0; i < parameterCount; i++)
		{
			int paramCorkIndex = makeSimpleGDScriptTag (parameterTokens[i], K_PARAMETER);
			deleteToken (parameterTokens[i]);
			tagEntryInfo *e = getEntryInCorkQueue (paramCorkIndex);
			if (e && parameterTypes[i])
			{
				e->extensionFields.typeRef [0] = eStrdup ("typename");
				e->extensionFields.typeRef [1] = vStringDeleteUnwrap (parameterTypes[i]);
				parameterTypes[i] = NULL;
			}
			vStringDelete (parameterTypes[i]); /* NULL is acceptable. */
		}
	}

	tagEntryInfo *e;
	vString *t;
	if (kind != K_CLASS
		&& (e = getEntryInCorkQueue (corkIndex))
		&& (t = parseReturnTypeAnnotation (token)))
	{
		e->extensionFields.typeRef [0] = eStrdup ("typename");
		e->extensionFields.typeRef [1] = vStringDeleteUnwrap (t);
	}

	if (kind == K_SIGNAL)
		nestingLevelsPop (GDScriptNestingLevels);

	return true;
}

static bool parseEnum (tokenInfo *const token)
{
	int corkIndex;

	readToken (token);

	if (token->type == '{')
	{
		tokenInfo *name = newToken ();
		copyToken (name, token);
		vStringClear (name->string);
		anonGenerate (name->string, "anon_enum_", K_ENUM);
		name->type = TOKEN_IDENTIFIER;
		corkIndex = makeSimpleGDScriptTag (name, K_ENUM);
		deleteToken (name);
		tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
		if (e)
			markTagExtraBit (e, XTAG_ANONYMOUS);
	}
	else if (token->type == TOKEN_IDENTIFIER)
	{
		corkIndex = makeSimpleGDScriptTag(token, K_ENUM);
		readToken (token);
	}
	else
		return false;

	if (token->type != '{')
		return false;

	readToken (token);
	nestingLevelsPush (GDScriptNestingLevels, corkIndex);

	while (token->type != '}' && token->type != TOKEN_EOF)
	{
		if (token->type == TOKEN_IDENTIFIER)
			makeSimpleGDScriptTag(token, K_ENUMERATOR);
		else if (token->type == '=')
		{
			/* Skip the right value. */
			do
				readToken (token);
			while (token->type != ','
				   && token->type != '}'
				   && token->type != TOKEN_EOF);
			if (token->type != ',')
				continue;
		}
		readToken (token);
	}

	tagEntryInfo *e;
	vString *t;
	if ((e = getEntryInCorkQueue (corkIndex))
		&& (t = parseReturnTypeAnnotation (token)))
	{
		e->extensionFields.typeRef [0] = eStrdup ("typename");
		e->extensionFields.typeRef [1] = vStringDeleteUnwrap (t);
	}

	nestingLevelsPop (GDScriptNestingLevels);
	return true;
}

static bool parseClassName (tokenInfo *const token)
{
	readToken (token);
	if (token->type == TOKEN_IDENTIFIER)
	{
		/* A class name is explicitly given with "class_name" keyword.
		 * Let's overwrite the anonymous tag for the class
		 */
		NestingLevel *nl = nestingLevelsGetNthFromRoot (GDScriptNestingLevels, 0);
		tagEntryInfo *klass = nl? getEntryInCorkQueue (nl->corkIndex): NULL;

		tagEntryInfo e;
		char *name = vStringStrdup (token->string);
		initTagEntry (&e, klass? "UNUSED": name, K_CLASS);

		if (klass)
		{
			eFree ((void *)klass->name);
			klass->name = name;
			name = NULL;
			unmarkTagExtraBit(klass, XTAG_ANONYMOUS);

			/* Adjust the position. */
			setTagPositionFromTag (klass, &e);
		}

		/* Extract B in class_name C extends B */
		readToken (token);
		if (token->type == TOKEN_KEYWORD
			&& token->keyword == KEYWORD_extends)
		{
			readToken (token);
			if (token->type == TOKEN_IDENTIFIER)
			{
				makeSimpleGDScriptRefTag (token, K_CLASS,
										  GDSCRIPT_CLASS_EXTENDED,
										  XTAG_UNKNOWN);
				if (klass)
				{
					if (klass->extensionFields.inheritance)
						eFree ((void *)klass->extensionFields.inheritance);
					klass->extensionFields.inheritance = vStringStrdup (token->string);
				}
				else
					e.extensionFields.inheritance = vStringValue(token->string);
			}
		}

		if (!klass)
			makeTagEntry (&e);

		if (name)
			eFree (name);
	}

	while (token->type != TOKEN_EOF &&
		   token->type != ';' &&
		   token->type != TOKEN_INDENT)
		readToken (token);

	return false;
}

static bool parseExtends (tokenInfo *const token)
{
	if (token->keyword == KEYWORD_extends)
	{
		readQualifiedName (token);
		if (token->type == TOKEN_IDENTIFIER)
		{
			makeSimpleGDScriptRefTag (token, K_CLASS, GDSCRIPT_CLASS_EXTENDED, XTAG_UNKNOWN);
			NestingLevel *nl = nestingLevelsGetCurrent (GDScriptNestingLevels);
			if (nl)
			{
				tagEntryInfo *klass = getEntryInCorkQueue (nl->corkIndex);
				if (klass)
				{
					if (klass->extensionFields.inheritance)
						eFree ((void *)klass->extensionFields.inheritance);
					klass->extensionFields.inheritance = vStringStrdup(token->string);
				}
			}
		}
	}
	readToken (token);
	return false;
}

/* this only handles the most common cases, but an annotation can be any
 * expression in theory.
 * this function assumes there must be an annotation, and doesn't do any check
 * on the token on which it is called: the caller should do that part. */
static bool skipVariableTypeAnnotation (tokenInfo *const token, vString *const repr)
{
	bool readNext = true;

	readToken (token);
	switch (token->type)
	{
		case '[': readNext = skipOverPair (token, '[', ']', repr, true); break;
		case '(': readNext = skipOverPair (token, '(', ')', repr, true); break;
		case '{': readNext = skipOverPair (token, '{', '}', repr, true); break;
		default: reprCat (repr, token);
	}
	if (readNext)
		readToken (token);
	/* skip subscripts and calls */
	while (token->type == '[' || token->type == '(' || token->type == '.' || token->type == '|')
	{
		switch (token->type)
		{
			case '[': readNext = skipOverPair (token, '[', ']', repr, true); break;
			case '(': readNext = skipOverPair (token, '(', ')', repr, true); break;
			case '|':
				reprCat (repr, token);
				skipVariableTypeAnnotation (token, repr);
				readNext = false;
				break;
			case '.':
				reprCat (repr, token);
				readToken (token);
				readNext = token->type == TOKEN_IDENTIFIER;
				if (readNext)
					reprCat (repr, token);
				break;
			default:  readNext = false; break;
		}
		if (readNext)
			readToken (token);
	}

	return false;
}

static bool parseVariable (tokenInfo *const token, const gdscriptKind kind,
							const stringList *const decorators,
							const int keyword)
{
	readToken(token);
	vString *type = vStringNew();
	tokenInfo *name = newToken ();
	copyToken (name, token);
	if (!name)
		return false;

	readToken (token);
	// Variable declarations with dotted names are illegal
	if (token->type == '.')
		return false;

	/* (parse and) skip annotations.  we need not to be too permissive because we
	 * aren't yet sure we're actually parsing a variable. */
	if (token->type == ':' && skipVariableTypeAnnotation (token, type))
		readToken (token);

	int index = makeSimpleGDScriptTag (name, kind);
	deleteToken(name);
	tagEntryInfo *e = getEntryInCorkQueue (index);

	if (e && decorators && stringListCount (decorators) > 0)
	{
		vString *vstr = makeDecoratorString (decorators);
		attachParserField (e, true, GDScriptFields[F_ANNOTATIONS].ftype,
						   vStringValue (vstr));
		vStringDelete (vstr);
	}

	vString *vtype = vStringNew();
	char * stype = vStringValue (type);
	if (strcmp(stype, "=") && strcmp(stype, ""))
	{
			vStringCatS(vtype, stype);
	}
	vStringDelete(type);

	if (e && vStringLength(vtype) > 0) /// TODO: Fix types away
	{
		e->extensionFields.typeRef [0] = eStrdup ("typename");
		e->extensionFields.typeRef [1] = vStringDeleteUnwrap (vtype);
	}
	else
	{
		vStringDelete(vtype);
	}


	while ((TokenContinuationDepth > 0 || token->type != ',') &&
		   token->type != TOKEN_EOF &&
		   token->type != ';' &&
		   token->type != TOKEN_INDENT)
	{
		readToken (token);
	}


	return false;
}

/* pops any level >= to indent */
static void setIndent (tokenInfo *const token)
{
	NestingLevel *lv = nestingLevelsGetCurrent (GDScriptNestingLevels);

	while (lv && GDS_NL (lv)->indentation >= token->indent)
	{
		tagEntryInfo *e = getEntryInCorkQueue (lv->corkIndex);
		if (e)
			e->extensionFields.endLine = token->lineNumber;

		nestingLevelsPop (GDScriptNestingLevels);
		lv = nestingLevelsGetCurrent (GDScriptNestingLevels);
	}
}

static int prepareUnnamedClass (struct NestingLevels *nls)
{
	{
		/* Ugly: we need a "position" on the input stream for making a tag.
		 * At the begining of parsing, the position is undefined.
		 * By reading a byte, the position is defined.
		 */
		int c = getcFromInputFile ();
		if (c == EOF)
			return CORK_NIL;
		ungetcToInputFile (c);
	}

	vString * tmp_class = anonGenerateNew ("anon_class_", K_CLASS);
	int corkIndex = makeSimpleTag (tmp_class, K_CLASS);
	vStringDelete (tmp_class);

	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	if (e)
		markTagExtraBit (e, XTAG_ANONYMOUS);

	/* This virtual scope should not be poped. */
	NestingLevel *lv = nestingLevelsPush (nls, corkIndex);
	GDS_NL (lv)->indentation = -1;

	return corkIndex;
}

static void findGDScriptTags (void)
{
	tokenInfo *const token = newToken ();
	stringList *decorators = stringListNew();
	bool atStatementStart = true;

	TokenContinuationDepth = 0;
	NextToken = NULL;
	GDScriptNestingLevels = nestingLevelsNew (sizeof (struct gdscriptNestingLevelUserData));

	if (isXtagEnabled (GDScriptXtagTable[X_IMPLICIT_CLASS].xtype))
		prepareUnnamedClass (GDScriptNestingLevels);

	readToken (token);
	while (token->type != TOKEN_EOF)
	{
		tokenType iterationTokenType = token->type;
		int iterationTokenKeyword = token->keyword;
		bool readNext = true;

		if (token->type == TOKEN_INDENT)
			setIndent (token);
		else if (token->keyword == KEYWORD_class ||
				 token->keyword == KEYWORD_func  ||
				 token->keyword == KEYWORD_signal)
		{
			gdscriptKind kind = K_METHOD;
			switch (token->keyword)
			{
			case KEYWORD_class:  kind = K_CLASS;  break;
			case KEYWORD_func:   kind = K_METHOD; break;
			case KEYWORD_signal: kind = K_SIGNAL; break;
			default:
				AssertNotReached();
			}
			readNext = parseClassOrDef (token, decorators, kind);
		}
		else if (token->keyword == KEYWORD_extends)
		{
			readNext = parseExtends (token);
		}
		else if (token->type == '(')
		{ /* skip parentheses to avoid finding stuff inside them */
			readNext = skipOverPair (token, '(', ')', NULL, false);
		}
		else if (token->keyword == KEYWORD_variable || token->keyword == KEYWORD_const)
		{
			NestingLevel *lv = nestingLevelsGetCurrent (GDScriptNestingLevels);
			tagEntryInfo *lvEntry = NULL;
			gdscriptKind kind = K_VARIABLE;

			if (lv)
				lvEntry = getEntryOfNestingLevel (lv);

			if (lvEntry && lvEntry->kindIndex != K_CLASS)
				kind = K_LOCAL_VARIABLE;

			if (token->keyword == KEYWORD_const)
				kind = K_CONST;

			readNext = parseVariable (token, kind, decorators, token->keyword);
		}
		else if (token->keyword == KEYWORD_enum)
		{
			readNext = parseEnum (token);
		}
		else if (token->keyword == KEYWORD_class_name)
		{
			readNext = parseClassName (token);
		}
		else if (token->type == TOKEN_KEYWORD
				 && token->keyword == KEYWORD_modifier)
		{
			stringListAdd (decorators, vStringNewCopy(token->string));
		}
		else if (token->type == '@' && atStatementStart &&
				 GDScriptFields[F_ANNOTATIONS].enabled)
		{
			/* collect decorators */
			readQualifiedName (token);
			if (token->type != TOKEN_IDENTIFIER
				&& (token->keyword != KEYWORD_modifier))
				readNext = false;
			else
			{
				stringListAdd (decorators, vStringNewCopy(token->string));
				readToken (token);

				vString *d = vStringNew ();
				readNext = skipOverPair (token, '(', ')', d, true);
				if (vStringLength (d) > 0)
					stringListAdd (decorators, d);
				else
					vStringDelete (d);
			}
		}

		/* clear collected decorators for any non-decorator tokens non-indent
		 * token.  decorator collection takes care of skipping the possible
		 * argument list, so we should never hit here parsing a decorator */
		if (iterationTokenType != TOKEN_INDENT &&
			iterationTokenType != '@' &&
			iterationTokenKeyword != KEYWORD_modifier &&
			GDScriptFields[F_ANNOTATIONS].enabled)
		{
			stringListClear (decorators);
		}

		atStatementStart = (token->type == TOKEN_INDENT || token->type == ';');

		if (readNext)
			readToken (token);
	}

	nestingLevelsFree (GDScriptNestingLevels);
	stringListDelete (decorators);
	deleteToken (token);
	Assert (NextToken == NULL);
}

static void initialize (const langType language)
{
	Lang_gdscript = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
	addKeywordGroup (&modifierKeywords, language);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

extern parserDefinition* GDScriptParser (void)
{
	static const char *const extensions[] = { "gd", NULL };
	parserDefinition *def = parserNew ("GDScript");
	def->kindTable = GDScriptKinds;
	def->kindCount = ARRAY_SIZE (GDScriptKinds);
	def->extensions = extensions;
	def->parser = findGDScriptTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->keywordTable = GDScriptKeywordTable;
	def->keywordCount = ARRAY_SIZE (GDScriptKeywordTable);
	def->fieldTable = GDScriptFields;
	def->fieldCount = ARRAY_SIZE (GDScriptFields);
	def->xtagTable     = GDScriptXtagTable;
	def->xtagCount     = ARRAY_SIZE(GDScriptXtagTable);
	def->useCork = CORK_QUEUE;
	def->requestAutomaticFQTag = true;
	return def;
}
