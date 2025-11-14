/*
*   Copyright (c) 2025, Kaisheng Xu
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains code for generating tags for TypeSpec language files
*   (https://microsoft.github.io/typespec/)
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "debug.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "keyword.h"
#include "entry.h"
#include "routines.h"
#include <string.h>

#define SCOPE_SEPARATOR "."

typedef enum {
	K_NAMESPACE,
	K_ENUM,
	K_OPERATION,
	K_INTERFACE,
	K_MODEL,
	K_UNION,
	K_ALIAS,
	K_PROPERTY,
	K_ENUMERATOR,
	COUNT_KIND
} typeSpecKind;

static kindDefinition TypeSpecKinds[COUNT_KIND] = {
	{ true, 'n', "namespace",	"namespaces" },
	{ true, 'g', "enum",		"enumeration names" },
	{ true, 'o', "operation",	"operations" },
	{ true, 'i', "interface",	"interfaces" },
	{ true, 'm', "model",		"models" },
	{ true, 'u', "union",		"unions" },
	{ true, 'a', "alias",		"aliases" },
	{ true, 'p', "property",	"properties" },
	{ true, 'e', "enumerator",	"enumerators (values inside an enumeration)", .version = 1 },
};

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_OPEN_ANGLE,
	TOKEN_CLOSE_ANGLE,
	TOKEN_EQUAL_SIGN,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_DECORATOR,
	TOKEN_SPREAD
} tokenType;

enum {
	KEYWORD_namespace,
	KEYWORD_enum,
	KEYWORD_op,
	KEYWORD_interface,
	KEYWORD_model,
	KEYWORD_union,
	KEYWORD_alias,
	KEYWORD_using,
	KEYWORD_import,
	KEYWORD_is,
	KEYWORD_extends
};

/* We need an integer that is not an unsigned to allow KEYWORD_NONE. */
typedef int keywordId;

static const keywordTable TypeSpecKeywordTable[] = {
	{ "namespace",	KEYWORD_namespace },
	{ "enum",		KEYWORD_enum },
	{ "op",			KEYWORD_op },
	{ "interface",	KEYWORD_interface },
	{ "model",		KEYWORD_model },
	{ "union",		KEYWORD_union },
	{ "alias",		KEYWORD_alias },
	{ "using",		KEYWORD_using },
	{ "import",		KEYWORD_import },
	{ "is",		 KEYWORD_is },
	{ "extends",	KEYWORD_extends }
};

typedef struct {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	int				scope;
	unsigned long	lineNumber;
	MIOPos			filePosition;
} tokenInfo;

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= CORK_NIL;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	vStringCopy (dest->string, src->string);
}

static void setScope (tokenInfo *const token, int scope)
{
	token->scope = scope;
}

static bool isIdentChar (const int c)
{
	return (isalnum (c) || c == '_' || c == '$' || c >= 0x80);
}

static bool isSpace (int c)
{
	return (c == '\t' || c == ' ' || c == '\v' ||
			c == '\n' || c == '\r' || c == '\f');
}

static int skipWhitespace (int c)
{
	while (isSpace (c))
		c = getcFromInputFile ();
	return c;
}

static int skipSingleComment (void)
{
	int c;
	do
	{
		c = getcFromInputFile ();
	} while (c != EOF && c != '\n');
	return c;
}

static int skipMultiComment (void)
{
	int c = getcFromInputFile ();
	int next = getcFromInputFile ();

	while (c != EOF && !(c == '*' && next == '/'))
	{
		c = next;
		next = getcFromInputFile ();
	}

	return getcFromInputFile ();
}

static void parseString (vString *const string, const int delimiter)
{
	while (true)
	{
		int c = getcFromInputFile ();

		if (c == '\\' && (c = getcFromInputFile ()) != EOF)
			vStringPut (string, c);
		else if (c == EOF || c == delimiter)
			break;
		else
			vStringPut (string, c);
	}
}

static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (isIdentChar (c));
	ungetcToInputFile (c);
}

static void readToken (tokenInfo *const token)
{
	int c;

	token->type		= TOKEN_UNDEFINED;
	vStringClear (token->string);

getNextChar:

	c = getcFromInputFile ();
	c = skipWhitespace (c);

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
		case EOF: token->type = TOKEN_EOF;					break;
		case '(': token->type = TOKEN_OPEN_PAREN;			break;
		case ')': token->type = TOKEN_CLOSE_PAREN;			break;
		case ';': token->type = TOKEN_SEMICOLON;			break;
		case ',': token->type = TOKEN_COMMA;				break;
		case '.':
		{
			/* Handle ... (spread) operator */
			int c1 = getcFromInputFile ();
			int c2 = getcFromInputFile ();
			if (c1 == '.' && c2 == '.')
			{
				token->type = TOKEN_SPREAD;
			}
			else
			{
				ungetcToInputFile (c2);
				ungetcToInputFile (c1);
				token->type = TOKEN_PERIOD;
			}
			break;
		}
		case ':': token->type = TOKEN_COLON;				break;
		case '{': token->type = TOKEN_OPEN_CURLY;			break;
		case '}': token->type = TOKEN_CLOSE_CURLY;			break;
		case '<': token->type = TOKEN_OPEN_ANGLE;			break;
		case '>': token->type = TOKEN_CLOSE_ANGLE;			break;
		case '=': token->type = TOKEN_EQUAL_SIGN;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;			break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;			break;

		case '\'':
		case '"':
			token->type = TOKEN_STRING;
			parseString (token->string, c);
			break;

		case '/':
		{
			int d = getcFromInputFile ();
			if (d == '/')  /* single-line comment */
			{
				skipSingleComment ();
				goto getNextChar;
			}
			else if (d == '*')  /* multi-line comment */
			{
				skipMultiComment ();
				goto getNextChar;
			}
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			break;
		}

		case '@':  /* decorator */
			token->type = TOKEN_DECORATOR;
			c = getcFromInputFile ();
			parseIdentifier (token->string, c);
			break;

		default:
			if (! isIdentChar (c))
				token->type = TOKEN_UNDEFINED;
			else
			{
				parseIdentifier (token->string, c);
				token->keyword = lookupCaseKeyword (
					vStringValue (token->string), getInputLanguage ());
				if (token->keyword == KEYWORD_NONE)
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}
}

static void initTypeSpecEntry (tagEntryInfo *const e, const tokenInfo *const token,
							  const typeSpecKind kind)
{
	initTagEntry (e, vStringValue (token->string), kind);

	e->lineNumber	= token->lineNumber;
	e->filePosition	= token->filePosition;

	e->extensionFields.scopeIndex = token->scope;
}

static int makeTypeSpecTag (const tokenInfo *const token, const typeSpecKind kind)
{
	tagEntryInfo e;
	initTypeSpecEntry (&e, token, kind);
	return makeTagEntry (&e);
}

static void enterScope (tokenInfo *const parentToken, int scope);

static void skipTypeParameters(tokenInfo *const token)
{
	int depth = 1;

	while (depth > 0 && token->type != TOKEN_EOF)
	{
		readToken(token);

		if (token->type == TOKEN_OPEN_ANGLE)
			depth++;
		else if (token->type == TOKEN_CLOSE_ANGLE)
			depth--;
	}
	readToken(token);
}

static void skipToSemicolon(tokenInfo *const token)
{
	while (token->type != TOKEN_SEMICOLON && token->type != TOKEN_EOF)
	{
		readToken(token);
	}
}

static void parseNamespace(tokenInfo *const token)
{
	vString *name = vStringNew();

	/* Read namespace components */
	do
	{
		readToken(token);

		if (token->type == TOKEN_IDENTIFIER)
		{
			if (!vStringIsEmpty(name))
				vStringCatS(name, SCOPE_SEPARATOR);
			vStringCat(name, token->string);

			readToken(token);
		}
		else
		{
			break;
		}
	} while (token->type == TOKEN_PERIOD);

	/* Create namespace tag */
	vStringCopy(token->string, name);
	int namespaceIndex = makeTypeSpecTag(token, K_NAMESPACE);

	/* Parse namespace body */
	if (token->type == TOKEN_SEMICOLON || token->type == TOKEN_OPEN_CURLY)
	{
		enterScope(token, namespaceIndex);
	}

	vStringDelete(name);
}

static void parseEnum(tokenInfo *const parentToken)
{
	tokenInfo *token = newToken();
	copyToken (token, parentToken);
	setScope(token, parentToken->scope);

	readToken(token);

	if (token->type == TOKEN_IDENTIFIER)
	{
		int enumIndex = makeTypeSpecTag(token, K_ENUM);
		readToken(token);

		if (token->type == TOKEN_OPEN_CURLY)
		{
			/* Parse enum body */
			while (token->type != TOKEN_CLOSE_CURLY && token->type != TOKEN_EOF)
			{
				if (token->type == TOKEN_IDENTIFIER)
				{
					/* Create tag for enumerator */
					setScope (token, enumIndex);
					makeTypeSpecTag(token, K_ENUMERATOR);
					readToken(token);

					/* Skip value assignment if any */
					if (token->type == TOKEN_COLON)
					{
						readToken(token);
						/* Skip the value */
						while (token->type != TOKEN_COMMA &&
							   token->type != TOKEN_CLOSE_CURLY &&
							   token->type != TOKEN_EOF)
						{
							readToken(token);
						}
					}

					/* Skip comma if present */
					if (token->type == TOKEN_COMMA)
						readToken(token);
				}
				else
					readToken(token);
			}
		}
	}

	deleteToken (token);
}

static void parseProperty(tokenInfo *const token)
{
	if (token->type == TOKEN_IDENTIFIER || token->type == TOKEN_STRING)
	{
		/* This is a property */
		makeTypeSpecTag(token, K_PROPERTY);

		/* Skip type declaration, modifiers, etc. until ; or , */
		while (token->type != TOKEN_SEMICOLON &&
			   token->type != TOKEN_COMMA &&
			   token->type != TOKEN_CLOSE_CURLY &&
			   token->type != TOKEN_EOF)
		{
		/* Skip generic parameters if any */
			readToken(token);
			if (token->type == TOKEN_OPEN_ANGLE)
			{
				skipTypeParameters(token);
			}
		}
	}
}

static void parseOperation(tokenInfo *const token)
{
	readToken(token);

	if (token->type == TOKEN_IDENTIFIER)
	{
		makeTypeSpecTag(token, K_OPERATION);
		readToken(token);

		/* Skip generic parameters if any */
		if (token->type == TOKEN_OPEN_ANGLE)
		{
			skipTypeParameters(token);
		}

		/* Handle "is" relationship */
		if (token->type == TOKEN_KEYWORD && token->keyword == KEYWORD_is)
		{
			/* Skip expression after "is" */
			do {
				readToken(token);

				if (token->type == TOKEN_OPEN_ANGLE)
				{
					skipTypeParameters(token);
				}

				if (token->type == TOKEN_SEMICOLON)
					break;

			} while (token->type != TOKEN_SEMICOLON && token->type != TOKEN_EOF);
		}
	}
}

static void parseInterface(tokenInfo *const token)
{
	readToken(token);

	if (token->type == TOKEN_IDENTIFIER)
	{
		int ifaceIndex = makeTypeSpecTag(token, K_INTERFACE);
		readToken(token);

		/* Skip extends clause */
		if (token->type == TOKEN_KEYWORD && token->keyword == KEYWORD_extends)
		{
			while (token->type != TOKEN_OPEN_CURLY && token->type != TOKEN_EOF)
			{
				readToken(token);
			}
		}

		if (token->type == TOKEN_OPEN_CURLY)
		{
			enterScope(token, ifaceIndex);
		}
	}
}

static void parseModel(tokenInfo *const token)
{
	readToken(token);

	if (token->type == TOKEN_IDENTIFIER)
	{
		int modelIndex = makeTypeSpecTag(token, K_MODEL);
		readToken(token);

		/* Skip extends clause if any */
		if (token->type == TOKEN_KEYWORD && token->keyword == KEYWORD_extends)
		{
			while (token->type != TOKEN_OPEN_CURLY && token->type != TOKEN_EOF)
			{
				readToken(token);
			}
		}

		if (token->type == TOKEN_OPEN_CURLY)
		{
			enterScope(token, modelIndex);
		}
	}
}

static void parseUnion(tokenInfo *const token)
{
	readToken(token);

	if (token->type == TOKEN_IDENTIFIER)
	{
		int unionIndex = makeTypeSpecTag(token, K_UNION);
		readToken(token);

		if (token->type == TOKEN_OPEN_CURLY)
		{
			enterScope(token, unionIndex);
		}
	}
}

static void parseAlias(tokenInfo *const token)
{
	readToken(token);

	if (token->type == TOKEN_IDENTIFIER)
	{
		makeTypeSpecTag(token, K_ALIAS);
		readToken(token);

		/* Skip generic parameters if any */
		if (token->type == TOKEN_OPEN_ANGLE)
		{
			skipTypeParameters(token);
		}

		/* Skip the rest until semicolon */
		skipToSemicolon(token);
	}
}

static void enterScope (tokenInfo *const parentToken, int scope)
{
	tokenInfo *token = newToken ();
	tagEntryInfo *e_parent;

	copyToken (token, parentToken);
	setScope (token, scope);

	readToken (token);
	while (token->type != TOKEN_EOF && token->type != TOKEN_CLOSE_CURLY)
	{
		/* Skip decorators */
		while (token->type == TOKEN_DECORATOR)
		{
			/* Skip the decorator body if it has parameters */
			readToken(token);
			if (token->type == TOKEN_OPEN_PAREN)
			{
				int depth = 1;
				while (depth > 0 && token->type != TOKEN_EOF)
				{
					readToken(token);
					if (token->type == TOKEN_OPEN_PAREN)
						depth++;
					else if (token->type == TOKEN_CLOSE_PAREN)
						depth--;
				}
				readToken(token);
			}
		}

		switch (token->type)
		{
			case TOKEN_KEYWORD:
				switch (token->keyword)
				{
					case KEYWORD_namespace:
						parseNamespace(token);
						break;

					case KEYWORD_enum:
						parseEnum(token);
						break;

					case KEYWORD_op:
						parseOperation(token);
						break;

					case KEYWORD_interface:
						parseInterface(token);
						break;

					case KEYWORD_model:
						parseModel(token);
						break;

					case KEYWORD_union:
						parseUnion(token);
						break;

					case KEYWORD_alias:
						parseAlias(token);
						break;

					case KEYWORD_using:
					case KEYWORD_import:
					case KEYWORD_is:
					case KEYWORD_extends:
						skipToSemicolon(token);
						break;

					default:
						readToken(token);
						break;
				}
				break;

			case TOKEN_SPREAD:
				/* Skip spread expressions (e.g. ...LivenessSessionData) */
				readToken(token);
				break;

			case TOKEN_IDENTIFIER:
			case TOKEN_STRING:
				e_parent = getEntryInCorkQueue (scope);
				if (e_parent
					&& (e_parent->kindIndex == K_MODEL
						|| e_parent->kindIndex == K_INTERFACE))
				{
					parseProperty(token);
				}
				break;

			default:
				break;
		}

		readToken(token);
	}

	copyToken (parentToken, token);
	/* the scope of parentToken is not changed in this function.  */
	deleteToken (token);
}

static void findTypeSpecTags (void)
{
	tokenInfo *const token = newToken ();

	do
	{
		enterScope (token, CORK_NIL);
	}
	while (token->type != TOKEN_EOF);

	deleteToken (token);
}

extern parserDefinition* TypeSpecParser (void)
{
	static const char *const extensions [] = { "tsp", NULL };
	parserDefinition* def = parserNew ("TypeSpec");
	def->kindTable  = TypeSpecKinds;
	def->kindCount  = ARRAY_SIZE (TypeSpecKinds);
	def->extensions = extensions;
	def->parser	 = findTypeSpecTags;
	def->keywordTable = TypeSpecKeywordTable;
	def->keywordCount = ARRAY_SIZE (TypeSpecKeywordTable);
	def->defaultScopeSeparator = SCOPE_SEPARATOR;
	def->useCork = CORK_QUEUE;
	def->versionCurrent = 1;
 	def->versionAge = 1;
	return def;
}
