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
	COUNT_KIND
} typeSpecKind;

static kindDefinition TypeSpecKinds[COUNT_KIND] = {
	{ true, 'n', "namespace",	"namespaces" },
	{ true, 'g', "enum",		"enums" },
	{ true, 'o', "operation",	"operations" },
	{ true, 'i', "interface",	"interfaces" },
	{ true, 'm', "model",		"models" },
	{ true, 'u', "union",		"unions" },
	{ true, 'a', "alias",		"aliases" },
	{ true, 'p', "property",	"properties" }
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
	TOKEN_EXTENDS,
	TOKEN_IS,
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
	KEYWORD_import
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
	{ "import",		KEYWORD_import }
};

typedef struct {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long	lineNumber;
	MIOPos			filePosition;
	int				parentKind;  /* KIND_GHOST_INDEX if none */
} tokenInfo;

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= vStringNew ();
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	token->parentKind	= KIND_GHOST_INDEX;

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src,
					   bool scope)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	vStringCopy (dest->string, src->string);
	dest->parentKind = src->parentKind;
	if (scope)
		vStringCopy (dest->scope, src->scope);
}

static void addToScope (tokenInfo *const token, const vString *const extra)
{
	if (vStringLength (token->scope) > 0)
		vStringCatS (token->scope, SCOPE_SEPARATOR);
	vStringCatS (token->scope, vStringValue (extra));
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
		if (c == '\r')
		{
			int next = getcFromInputFile ();
			if (next != '\n')
				ungetcToInputFile (next);
			else
				c = next;
		}
	} while (c != EOF && c != '\n' && c != '\r');
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
				
				/* Check for special keywords */
				if (strcmp (vStringValue (token->string), "extends") == 0)
					token->type = TOKEN_EXTENDS;
				else if (strcmp (vStringValue (token->string), "is") == 0)
					token->type = TOKEN_IS;
				else {
					token->keyword = lookupCaseKeyword (
						vStringValue (token->string), getInputLanguage ());
					if (token->keyword == KEYWORD_NONE)
						token->type = TOKEN_IDENTIFIER;
					else
						token->type = TOKEN_KEYWORD;
				}
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

	if (vStringLength (token->scope) > 0)
	{
		int parentKind = token->parentKind;
		Assert (parentKind >= 0);

		e->extensionFields.scopeKindIndex = parentKind;
		e->extensionFields.scopeName = vStringValue (token->scope);
	}
}

static void makeTypeSpecTag (const tokenInfo *const token, const typeSpecKind kind)
{
	if (TypeSpecKinds[kind].enabled)
	{
		tagEntryInfo e;

		initTypeSpecEntry (&e, token, kind);
		makeTagEntry (&e);
	}
}

static void enterScope (tokenInfo *const parentToken,
						const vString *const extraScope,
						const int parentKind);

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
}

static void skipEntityBody(tokenInfo *const token)
{
	int depth = 1;
	
	while (depth > 0 && token->type != TOKEN_EOF)
	{
		readToken(token);
		
		if (token->type == TOKEN_OPEN_CURLY)
			depth++;
		else if (token->type == TOKEN_CLOSE_CURLY)
			depth--;
	}
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
			if (vStringLength(name) > 0)
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
	makeTypeSpecTag(token, K_NAMESPACE);
	
	/* Parse namespace body */
	if (token->type == TOKEN_SEMICOLON)
	{
		/* Empty namespace, just a declaration */
	}
	else if (token->type == TOKEN_OPEN_CURLY)
	{
		enterScope(token, name, K_NAMESPACE);
	}
	
	vStringDelete(name);
}

static void parseEnum(tokenInfo *const token)
{
	readToken(token);
	
	if (token->type == TOKEN_IDENTIFIER)
	{
		makeTypeSpecTag(token, K_ENUM);
		readToken(token);
		
		if (token->type == TOKEN_OPEN_CURLY)
		{
			vString *enumName = vStringNewCopy(token->string);
			enterScope(token, enumName, K_ENUM);
			vStringDelete(enumName);
		}
	}
}

static void parseProperty(tokenInfo *const token, const typeSpecKind parentKind)
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
			readToken(token);
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
		if (token->type == TOKEN_IS)
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
		makeTypeSpecTag(token, K_INTERFACE);
		readToken(token);
		
		/* Skip extends clause */
		if (token->type == TOKEN_EXTENDS)
		{
			while (token->type != TOKEN_OPEN_CURLY && token->type != TOKEN_EOF)
			{
				readToken(token);
			}
		}
		
		if (token->type == TOKEN_OPEN_CURLY)
		{
			vString *interfaceName = vStringNewCopy(token->string);
			enterScope(token, interfaceName, K_INTERFACE);
			vStringDelete(interfaceName);
		}
	}
}

static void parseModel(tokenInfo *const token)
{
	readToken(token);
	
	if (token->type == TOKEN_IDENTIFIER)
	{
		makeTypeSpecTag(token, K_MODEL);
		readToken(token);
		
		/* Skip extends clause if any */
		if (token->type == TOKEN_EXTENDS)
		{
			while (token->type != TOKEN_OPEN_CURLY && token->type != TOKEN_EOF)
			{
				readToken(token);
			}
		}
		
		if (token->type == TOKEN_OPEN_CURLY)
		{
			vString *modelName = vStringNewCopy(token->string);
			enterScope(token, modelName, K_MODEL);
			vStringDelete(modelName);
		}
	}
}

static void parseUnion(tokenInfo *const token)
{
	readToken(token);
	
	if (token->type == TOKEN_IDENTIFIER)
	{
		makeTypeSpecTag(token, K_UNION);
		readToken(token);
		
		if (token->type == TOKEN_OPEN_CURLY)
		{
			vString *unionName = vStringNewCopy(token->string);
			enterScope(token, unionName, K_UNION);
			vStringDelete(unionName);
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

static void enterScope (tokenInfo *const parentToken,
						const vString *const extraScope,
						const int parentKind)
{
	tokenInfo *token = newToken ();
	int origParentKind = parentToken->parentKind;

	copyToken (token, parentToken, true);

	if (extraScope)
	{
		addToScope (token, extraScope);
		token->parentKind = parentKind;
	}

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
						skipToSemicolon(token);
						break;

					default:
						readToken(token);
						break;
				}
				break;
				
			case TOKEN_OPEN_CURLY:
				skipEntityBody(token);
				break;
				
			case TOKEN_SPREAD:
				/* Skip spread expressions (e.g. ...LivenessSessionData) */
				readToken(token);
				if (token->type == TOKEN_IDENTIFIER)
				{
					readToken(token);
					if (token->type == TOKEN_SEMICOLON)
						readToken(token);
				}
				break;

			case TOKEN_IDENTIFIER:
			case TOKEN_STRING:
				if (parentKind == K_MODEL || parentKind == K_INTERFACE)
				{
					parseProperty(token, parentKind);
				}
				readToken(token);
				break;

			default:
				readToken(token);
				break;
		}
	}

	copyToken (parentToken, token, false);
	parentToken->parentKind = origParentKind;
	deleteToken (token);
}

static void findTypeSpecTags (void)
{
	tokenInfo *const token = newToken ();

	do
	{
		enterScope (token, NULL, KIND_GHOST_INDEX);
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
	def->parser     = findTypeSpecTags;
	def->keywordTable = TypeSpecKeywordTable;
	def->keywordCount = ARRAY_SIZE (TypeSpecKeywordTable);
	return def;
}