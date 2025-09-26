/*
*   Copyright (c) 2015, Enrico Tröger <enrico.troeger@uvena.de>
*
*   Loosely based on the PHP tags parser since the syntax is somewhat similar
*   regarding variable and function definitions.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains code for generating tags for Windows PowerShell scripts
*   (https://en.wikipedia.org/wiki/PowerShell).
*
*   References:
*   - https://learn.microsoft.com/en-us/powershell/scripting/developer/windows-powershell-reference
*   - https://learn.microsoft.com/en-us/powershell/scripting/lang-spec/chapter-01
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

#define SCOPE_SEPARATOR "::"


#define ACCESS_UNDEFINED NULL
static const char *const accessTypes[] = {
	ACCESS_UNDEFINED,
	"global",
	"local",
	"script",
	"private"
};

typedef enum {
	K_FUNCTION,
	K_VARIABLE,
	K_CLASS,
	K_FILTER,
	K_ENUM,
	K_ENUMLABEL,
	COUNT_KIND
} powerShellKind;

static kindDefinition PowerShellKinds[COUNT_KIND] = {
	{ true, 'f', "function",	"functions" },
	{ true, 'v', "variable",	"variables" },
	{ true, 'c', "class",		"classes" },
	{ true, 'i', "filter",		"filter" },
	{ true, 'g', "enum",		"enum names" },
	{ true, 'e', "enumlabel",	"enum labels",
	  .version = 1 },
};


typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_OPERATOR,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_EQUAL_SIGN,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_VARIABLE
} tokenType;

enum {
	KEYWORD_function,
	KEYWORD_filter,
	KEYWORD_class,
	KEYWORD_enum,
};

/* We need an integer that is not an unsigned to allow KEYWORD_NONE. */
typedef int keywordId;

static const keywordTable PowerShellKeywordTable[] = {
	{ "function",	KEYWORD_function },
	{ "filter",		KEYWORD_filter },
	{ "class",		KEYWORD_class },
	{ "enum",		KEYWORD_enum },
};

typedef struct {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long	lineNumber;
	MIOPos			filePosition;
	int				parentKind; /* KIND_GHOST_INDEX if none */
	bool            herestring;	/* Meaningful only when type is TOKEN_STRING. */
} tokenInfo;


static const char *findValidAccessType (const char *const access)
{
	unsigned int i;
	if (access == ACCESS_UNDEFINED)
		return ACCESS_UNDEFINED; /* early out to save the for-loop if possible */
	for (i = 0; i < ARRAY_SIZE(accessTypes); i++)
	{
		if (accessTypes[i] == ACCESS_UNDEFINED)
			continue;
		if (strcasecmp (access, accessTypes[i]) == 0)
			return accessTypes[i];
		i++;
	}
	return ACCESS_UNDEFINED;
}

static void initPowerShellEntry (tagEntryInfo *const e, const tokenInfo *const token,
								 const powerShellKind kind, const char *const access)
{
	initTagEntry (e, vStringValue (token->string), kind);

	updateTagLine (e, token->lineNumber, token->filePosition);

	if (access != NULL)
		e->extensionFields.access = access;
	if (vStringLength (token->scope) > 0)
	{
		int parentKind = token->parentKind;
		Assert (parentKind >= 0);

		e->extensionFields.scopeKindIndex = parentKind;
		e->extensionFields.scopeName = vStringValue (token->scope);
	}
}

static void makeSimplePowerShellTag (const tokenInfo *const token, const powerShellKind kind,
									 const char *const access)
{
	if (PowerShellKinds[kind].enabled)
	{
		tagEntryInfo e;

		initPowerShellEntry (&e, token, kind, access);
		makeTagEntry (&e);
	}
}

static void makeFunctionTag (const tokenInfo *const token, const vString *const arglist,
							 const char *const access, int kind)
{
	if (PowerShellKinds[kind].enabled)
	{
		tagEntryInfo e;

		initPowerShellEntry (&e, token, kind, access);

		if (arglist)
			e.extensionFields.signature = vStringValue (arglist);

		makeTagEntry (&e);
	}
}

static void makeClassTag (const tokenInfo *const token)
{
	if (PowerShellKinds[K_CLASS].enabled)
	{
		tagEntryInfo e;

		initPowerShellEntry (&e, token, K_CLASS, NULL);

		makeTagEntry (&e);
	}
}

static void makeEnumTag (const tokenInfo *const token)
{
	if (PowerShellKinds[K_ENUM].enabled)
	{
		tagEntryInfo e;

		initPowerShellEntry (&e, token, K_ENUM, NULL);

		makeTagEntry (&e);
	}
}

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
	return (isalnum (c) || c == '_' || c == '-' || c >= 0x80);
}

static bool isScopeIdentifierChar (const int c)
{
	return (isIdentChar (c) || c == ':');
}

static void parseString (vString *const string, const int delimiter)
{
	while (true)
	{
		int c = getcFromInputFile ();

		if (delimiter == '"' && c == '`' && (c = getcFromInputFile ()) != EOF)
			vStringPut (string, c);
		else if (c == EOF || c == delimiter)
			break;
		else
			vStringPut (string, c);
	}
}

static bool parseHereString (vString *const string, const int delimiter)
{
	enum phs_state {
		PHS_INIT,
		PHS_NEWLINE0,
		PHS_QUOTE,
		PHS_AT,
		PHS_NEWLINE1,
		PHS_END = PHS_NEWLINE1
	} s = PHS_INIT;

	while (s != PHS_END)
	{
		int c = getcFromInputFile ();

		if (c == EOF)
			return false;

		vStringPut (string, c);

		if (s == PHS_INIT && c == '\n')
			s = PHS_NEWLINE0;
		else if (s == PHS_NEWLINE0 && c == delimiter)
			s = PHS_QUOTE;
		else if (s == PHS_QUOTE && c == '@')
			s = PHS_AT;
		else if (s == PHS_AT && c == '\n')
			s = PHS_NEWLINE1;
		else
			s = PHS_INIT;
	}

	Assert (vStringLength (string) >= 4);
	vStringTruncate (string, vStringLength (string) - 4);
	return true;
}

/* parse a identifier that may contain scopes, such as function names and
 * variable names.
 *
 * 	VariableName
 * 	FunctionName
 * 	local:VariableName
 * 	private:FunctionName
 */
static void parseScopeIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (isScopeIdentifierChar (c));
	ungetcToInputFile (c);
}

/* parse a identifier that do not contain scope, such as class names. */
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

static bool isSpace (int c)
{
	return (c == '\t' || c == ' ' || c == '\v' ||
			c == '\n' || c == '\r' || c == '\f');
}

static int skipWhitespaces (int c)
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

static void readTokenFull (tokenInfo *const token, bool includingScope)
{
	int c;

	token->type		= TOKEN_UNDEFINED;
	vStringClear (token->string);

getNextChar:

	c = getcFromInputFile ();
	c = skipWhitespaces (c);

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
		case EOF: token->type = TOKEN_EOF;					break;
		case '(': token->type = TOKEN_OPEN_PAREN;			break;
		case ')': token->type = TOKEN_CLOSE_PAREN;			break;
		case ';': token->type = TOKEN_SEMICOLON;			break;
		case ',': token->type = TOKEN_COMMA;				break;
		case '.': token->type = TOKEN_PERIOD;				break;
		case ':': token->type = TOKEN_COLON;				break;
		case '{': token->type = TOKEN_OPEN_CURLY;			break;
		case '}': token->type = TOKEN_CLOSE_CURLY;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;			break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;			break;
		case '=': token->type = TOKEN_EQUAL_SIGN;			break;

		case '\'':
		case '"':
			token->type = TOKEN_STRING;
			parseString (token->string, c);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			token->herestring = false;
			break;

		case '@':
		{
			int d = getcFromInputFile ();
			if (d == '\'' || d == '"')
			{
				bool is_herestr;
				token->type = TOKEN_STRING;
				is_herestr = parseHereString (token->string, d);
				token->lineNumber = getInputLineNumber ();
				token->filePosition = getInputFilePosition ();
				if (is_herestr)
					token->herestring = true;
				else
					token->type = TOKEN_UNDEFINED;
			}
			else if (d == EOF)
				token->type = TOKEN_UNDEFINED;
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			break;
		}
		case '<':
		{
			int d = getcFromInputFile ();
			if (d == '#')
			{
				/* <# ... #> multiline comment */
				do
				{
					c = skipToCharacterInInputFile ('#');
					if (c != EOF)
					{
						c = getcFromInputFile ();
						if (c == '>')
							break;
						else
							ungetcToInputFile (c);
					}
				} while (c != EOF);
				goto getNextChar;
			}
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			break;
		}

		case '#': /* comment */
			skipSingleComment ();
			goto getNextChar;
			break;

		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		{
			int d = getcFromInputFile ();
			if (d != '=')
				ungetcToInputFile (d);
			token->type = TOKEN_OPERATOR;
			break;
		}

		case '$': /* variable start */
		{
			int d = getcFromInputFile ();
			if (! isScopeIdentifierChar (d))
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			else
			{
				parseScopeIdentifier (token->string, d);
				token->type = TOKEN_VARIABLE;
			}
			break;
		}

		default:
			if (! isIdentChar (c))
				token->type = TOKEN_UNDEFINED;
			else
			{
				if (includingScope)
					parseScopeIdentifier (token->string, c);
				else
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

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, false);
}

static void enterScope (tokenInfo *const parentToken,
						const vString *const extraScope,
						const int parentKind);

/* strip a possible PowerShell scope specification and convert it to accessType */
static const char *parsePowerShellScope (tokenInfo *const token)
{
	const char *access = ACCESS_UNDEFINED;
	const char *const tokenName = vStringValue (token->string);
	const char *powershellScopeEnd;

	powershellScopeEnd = strchr (tokenName, ':');
	if (powershellScopeEnd)
	{
		size_t powershellScopeLen;
		vString * powershellScope = vStringNew ();

		powershellScopeLen = (size_t)(powershellScopeEnd - tokenName);
		/* extract the scope */
		vStringNCopyS (powershellScope, tokenName, powershellScopeLen);
		/* cut the resulting scope string from the identifier */
		memmove (vStringValue (token->string),
				 /* +1 to skip the leading colon */
				 vStringValue (token->string) + powershellScopeLen + 1,
				 /* +1 for the skipped leading colon and - 1 to include the trailing \0 byte */
				 token->string->length + 1 - powershellScopeLen - 1);
		token->string->length -= powershellScopeLen + 1;

		access = findValidAccessType (vStringValue (powershellScope));

		vStringDelete (powershellScope);
	}
	return access;
}


/* parse a function
 *
 * 	function myfunc($foo, $bar) {}
 */
static bool parseFunction (tokenInfo *const token, int kind)
{
	bool readNext = true;
	tokenInfo *nameFree = NULL;
	const char *access;

	readTokenFull (token, true);

	if (token->type != TOKEN_IDENTIFIER)
		return false;

	access = parsePowerShellScope (token);

	nameFree = newToken ();
	copyToken (nameFree, token, true);
	readToken (token);

	if (token->type == TOKEN_OPEN_PAREN)
	{
		vString *arglist = vStringNew ();
		int depth = 1;

		vStringPut (arglist, '(');
		do
		{
			readToken (token);

			switch (token->type)
			{
				case TOKEN_OPEN_PAREN:  depth++; break;
				case TOKEN_CLOSE_PAREN: depth--; break;
				default: break;
			}
			/* display part */
			switch (token->type)
			{
				case TOKEN_CLOSE_CURLY:		vStringPut (arglist, '}');		break;
				case TOKEN_CLOSE_PAREN:		vStringPut (arglist, ')');		break;
				case TOKEN_CLOSE_SQUARE:	vStringPut (arglist, ']');		break;
				case TOKEN_COLON:			vStringPut (arglist, ':');		break;
				case TOKEN_COMMA:			vStringCatS (arglist, ", ");	break;
				case TOKEN_EQUAL_SIGN:		vStringCatS (arglist, " = ");	break;
				case TOKEN_OPEN_CURLY:		vStringPut (arglist, '{');		break;
				case TOKEN_OPEN_PAREN:		vStringPut (arglist, '(');		break;
				case TOKEN_OPEN_SQUARE:		vStringPut (arglist, '[');		break;
				case TOKEN_PERIOD:			vStringPut (arglist, '.');		break;
				case TOKEN_SEMICOLON:		vStringPut (arglist, ';');		break;
				case TOKEN_STRING:			vStringCatS (arglist, "'...'");	break;

				case TOKEN_IDENTIFIER:
				case TOKEN_VARIABLE:
				{
					switch (vStringLast (arglist))
					{
						case 0:
						case ' ':
						case '{':
						case '(':
						case '[':
						case '.':
							/* no need for a space between those and the identifier */
							break;

						default:
							vStringPut (arglist, ' ');
							break;
					}
					if (token->type == TOKEN_VARIABLE)
						vStringPut (arglist, '$');
					vStringCat (arglist, token->string);
					break;
				}

				default: break;
			}
		}
		while (token->type != TOKEN_EOF && depth > 0);

		makeFunctionTag (nameFree, arglist, access, kind);
		vStringDelete (arglist);

		readToken (token);
	}
	else if (token->type == TOKEN_OPEN_CURLY)
	{	/* filters doesn't need to have an arglist */
		makeFunctionTag (nameFree, NULL, access, kind);
	}

	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, nameFree->string, kind);
	else
		readNext = false;

	deleteToken (nameFree);

	return readNext;
}

/* parse a class
 *
 * 	class MyClass {}
 * 	class Derived : Base {}
 */
static bool parseClass (tokenInfo *const token)
{
	bool readNext = true;
	vString *nameFree = NULL;

	readToken (token);

	if (token->type != TOKEN_IDENTIFIER)
		return false;

	makeClassTag (token);
	nameFree = vStringNewCopy (token->string);
	readToken (token);

	while (token->type != TOKEN_OPEN_CURLY && token->type != TOKEN_EOF)
	{
		readToken (token);
	}

	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, nameFree, K_CLASS);
	else
		readNext = false;

	vStringDelete (nameFree);

	return readNext;
}

/* parse a enum
 *
 * 	enum EnumName {}
 */
static bool parseEnum (tokenInfo *const token)
{
	bool readNext = true;
	vString *nameFree = NULL;

	readToken (token);

	if (token->type != TOKEN_IDENTIFIER)
		return false;

	makeEnumTag (token);
	nameFree = vStringNewCopy (token->string);
	readToken (token);

	while (token->type != TOKEN_OPEN_CURLY && token->type != TOKEN_EOF)
	{
		readToken (token);
	}

	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, nameFree, K_ENUM);
	else
		readNext = false;

	vStringDelete (nameFree);

	return readNext;
}

/* parses declarations of the form
 * 	<label> [= <int-value>]
 * that is, contents of an enum
 */
static bool parseEnumLabel (tokenInfo *const token)
{
	bool readNext = true;

	if (token->parentKind != K_ENUM)
		return false;

	if (token->type != TOKEN_IDENTIFIER)
		return false;

	makeSimplePowerShellTag (token, K_ENUMLABEL, ACCESS_UNDEFINED);
	readToken (token);
	if (token->type != TOKEN_EQUAL_SIGN)
		readNext = false;
	else /* skip int-value */
		readToken (token);

	return readNext;
}

/* parses declarations of the form
 * 	$var = VALUE
 */
static bool parseVariable (tokenInfo *const token)
{
	tokenInfo *name;
	bool readNext = true;
	const char *access;

	name = newToken ();
	copyToken (name, token, true);

	readToken (token);
	if (token->type == TOKEN_EQUAL_SIGN)
	{
		if (token->parentKind != K_FUNCTION &&
				token->parentKind != K_FILTER &&
				token->parentKind != K_CLASS)
		{   /* ignore local variables (i.e. within a function)
			 * TODO: Parses class properties to make tags. */
			access = parsePowerShellScope (name);
			makeSimplePowerShellTag (name, K_VARIABLE, access);
			readNext = true;
		}
	}
	else
		readNext = false;

	deleteToken (name);

	return readNext;
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
	while (token->type != TOKEN_EOF &&
		   token->type != TOKEN_CLOSE_CURLY)
	{
		bool readNext = true;

		switch (token->type)
		{
			case TOKEN_OPEN_CURLY:
				enterScope (token, NULL, KIND_GHOST_INDEX);
				break;

			case TOKEN_KEYWORD:
				switch (token->keyword)
				{
					case KEYWORD_function:
						readNext = parseFunction (token, K_FUNCTION);
						break;

					case KEYWORD_filter:
						readNext = parseFunction (token, K_FILTER);
						break;

					case KEYWORD_class:
						readNext = parseClass (token);
						break;

					case KEYWORD_enum:
						readNext = parseEnum (token);
						break;

					default: break;
				}
				break;

			case TOKEN_VARIABLE:
				readNext = parseVariable (token);
				break;

			case TOKEN_IDENTIFIER:
				if (parentKind == K_ENUM)
					readNext = parseEnumLabel (token);
				break;

			default: break;
		}

		if (readNext)
			readToken (token);
	}

	copyToken (parentToken, token, false);
	parentToken->parentKind = origParentKind;
	deleteToken (token);
}

static void findPowerShellTags (void)
{
	tokenInfo *const token = newToken ();

	do
	{
		enterScope (token, NULL, KIND_GHOST_INDEX);
	}
	while (token->type != TOKEN_EOF); /* keep going even with unmatched braces */

	deleteToken (token);
}

extern parserDefinition* PowerShellParser (void)
{
	static const char *const extensions [] = { "ps1", "psm1", NULL };
	parserDefinition* def = parserNew ("PowerShell");
	def->kindTable  = PowerShellKinds;
	def->kindCount  = ARRAY_SIZE (PowerShellKinds);
	def->extensions = extensions;
	def->parser     = findPowerShellTags;
	def->keywordTable = PowerShellKeywordTable;
	def->keywordCount = ARRAY_SIZE (PowerShellKeywordTable);

	def->versionCurrent = 1;
	def->versionAge = 1;

	return def;
}
