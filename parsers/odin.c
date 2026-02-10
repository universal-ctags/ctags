/*
*   Copyright (c) 2026, Ryan Funduk
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Reference:
*     https://odin-lang.org/docs/overview/
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"        /* must always come first */

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "read.h"
#include "objpool.h"
#include "parse.h"
#include "routines.h"
#include "vstring.h"

#include <string.h>

/*
 *	 MACROS
 */
#define MAX_COLLECTOR_LENGTH 512
#define isType(token,t) (bool) ((token)->type == (t))
#define isKeyword(token,k) (bool) ((token)->keyword == (k))
#define isStartIdentChar(c) (isalpha (c) ||  (c) == '_' || (c) > 128) /* XXX UTF-8 */
#define isIdentChar(c) (isStartIdentChar (c) || isdigit (c))
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))

/*
 *	 DATA DECLARATIONS
 */
enum eKeywordId {
	KEYWORD_package,
	KEYWORD_import,
	KEYWORD_foreign,
	KEYWORD_proc,
	KEYWORD_struct,
	KEYWORD_enum,
	KEYWORD_union,
	KEYWORD_bit_set,
	KEYWORD_bit_field,
	KEYWORD_map,
	KEYWORD_distinct,
	KEYWORD_when,
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_for,
	KEYWORD_switch,
	KEYWORD_return,
	KEYWORD_defer,
	KEYWORD_using,
	KEYWORD_where
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_NONE = -1,
	TOKEN_OTHER,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_SEMICOLON,
	TOKEN_DOT,
	TOKEN_COMMA,
	TOKEN_COLON,
	TOKEN_DOUBLE_COLON,
	TOKEN_COLON_EQUAL,
	TOKEN_EQUAL,
	TOKEN_ARROW,
	TOKEN_HASH,
	TOKEN_AT,
	TOKEN_CARET,
	TOKEN_EOF
} tokenType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	vString *string;		/* the name of the token */
	unsigned long lineNumber;	/* line number of tag */
	MIOPos filePosition;		/* file position of line containing name */
	int c;						/* Used in AppendTokenToVString */
} tokenInfo;

typedef struct sCollector {
	vString *str;
	size_t last_len;
} collector;

/*
 *   DATA DEFINITIONS
 */
static int Lang_odin;
static objPool *TokenPool = NULL;

typedef enum {
	ODINTAG_UNDEFINED = -1,
	ODINTAG_PACKAGE,
	ODINTAG_PROC,
	ODINTAG_CONST,
	ODINTAG_VAR,
	ODINTAG_STRUCT,
	ODINTAG_ENUM,
	ODINTAG_UNION,
	ODINTAG_MEMBER,
	ODINTAG_ENUMERATOR,
	ODINTAG_TYPE,
	ODINTAG_FOREIGN,
	ODINTAG_IMPORT_NAME
} odinKind;

typedef enum {
	R_ODINTAG_PACKAGE_IMPORTED
} OdinPackageRole;

static roleDefinition OdinPackageRoles [] = {
	{ true, "imported", "imported package" },
};

static kindDefinition OdinKinds[] = {
	{true, 'p', "package", "packages",
	  .referenceOnly = false, ATTACH_ROLES (OdinPackageRoles)},
	{true, 'f', "proc", "procedures"},
	{true, 'c', "const", "constants"},
	{true, 'v', "var", "variables"},
	{true, 's', "struct", "structs"},
	{true, 'e', "enum", "enums"},
	{true, 'u', "union", "unions"},
	{true, 'm', "member", "struct members"},
	{true, 'n', "enumerator", "enum values"},
	{true, 't', "type", "type aliases"},
	{true, 'g', "foreign", "foreign imports"},
	{true, 'i', "importName", "import names"}
};

static const keywordTable OdinKeywordTable[] = {
	{"package", KEYWORD_package},
	{"import", KEYWORD_import},
	{"foreign", KEYWORD_foreign},
	{"proc", KEYWORD_proc},
	{"struct", KEYWORD_struct},
	{"enum", KEYWORD_enum},
	{"union", KEYWORD_union},
	{"bit_set", KEYWORD_bit_set},
	{"bit_field", KEYWORD_bit_field},
	{"map", KEYWORD_map},
	{"distinct", KEYWORD_distinct},
	{"when", KEYWORD_when},
	{"if", KEYWORD_if},
	{"else", KEYWORD_else},
	{"for", KEYWORD_for},
	{"switch", KEYWORD_switch},
	{"return", KEYWORD_return},
	{"defer", KEYWORD_defer},
	{"using", KEYWORD_using},
	{"where", KEYWORD_where},
};

/*
 *   FUNCTION DEFINITIONS
 */
static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->string = vStringNew ();
	return token;
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	token->c = EOF;
	vStringClear (token->string);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const other)
{
	dest->type = other->type;
	dest->keyword = other->keyword;
	dest->c = other->c;
	vStringCopy (dest->string, other->string);
	dest->lineNumber = other->lineNumber;
	dest->filePosition = other->filePosition;
}

static void deletePoolToken (void* data)
{
	tokenInfo * const token = data;

	vStringDelete (token->string);
	eFree (token);
}

static void initialize (const langType language)
{
	Lang_odin = language;
	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (const langType language, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

static void parseString (vString *const string, const int delimiter)
{
	bool end = false;
	while (!end)
	{
		int c = getcFromInputFile ();
		if (c == EOF)
			end = true;
		else if (c == '\\' && delimiter != '`')
		{
			c = getcFromInputFile ();
			if (c != delimiter)
				vStringPut (string, '\\');
			vStringPut (string, c);
		}
		else if (c == delimiter)
			end = true;
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

static void collectorPut (collector *collector, char c)
{
	if (vStringLength (collector->str) > 0)
	{
		if (vStringLast (collector->str) == '(' && c == ' ')
			return;
		else if (vStringLast (collector->str) == ' ' && c == ')')
			vStringChop (collector->str);
	}

	collector->last_len = vStringLength (collector->str);
	vStringPut (collector->str, c);
}

static void collectorCatS (collector *collector, const char *cstr)
{
	collector->last_len = vStringLength (collector->str);
	vStringCatS (collector->str, cstr);
}

static void collectorCat (collector *collector, vString *str)
{
	collector->last_len = vStringLength (collector->str);
	vStringCat (collector->str, str);
}

static void collectorAppendToken (collector *collector, const tokenInfo *const token)
{
	if (token->type == TOKEN_ARROW)
		collectorCatS (collector, "->");
	else if (token->type == TOKEN_COLON_EQUAL)
		collectorCatS (collector, ":=");
	else if (token->type == TOKEN_STRING)
	{
		collector->last_len = vStringLength (collector->str);
		vStringPut (collector->str, '"');
		vStringCat (collector->str, token->string);
		vStringPut (collector->str, '"');
	}
	else if (token->type == TOKEN_IDENTIFIER || token->type == TOKEN_KEYWORD)
		collectorCat (collector, token->string);
	else if (token->c != EOF)
		collectorPut (collector, token->c);
}

static void collectorTruncate (collector *collector)
{
	vStringStripLeading (collector->str);
	vStringStripTrailing (collector->str);
}

static void readTokenFull (tokenInfo *const token, collector *collector)
{
	int c;
	bool firstWhitespace = true;
	bool whitespace;

	token->c = EOF;
	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:
	do
	{
		c = getcFromInputFile ();
		token->lineNumber = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
		whitespace = c == '\t'  ||  c == ' ' ||  c == '\r' || c == '\n';
		if (collector && whitespace && firstWhitespace && vStringLength (collector->str) < MAX_COLLECTOR_LENGTH)
		{
			firstWhitespace = false;
			collectorPut (collector, ' ');
		}
	}
	while (whitespace);

	switch (c)
	{
		case EOF:
			token->type = TOKEN_EOF;
			break;

		case ';':
			token->type = TOKEN_SEMICOLON;
			break;

		case '/':
			{
				bool hasNewline = false;
				int d = getcFromInputFile ();
				switch (d)
				{
					case '/':
						skipToCharacterInInputFile ('\n');
						goto getNextChar;
					case '*':
						do
						{
							do
							{
								d = getcFromInputFile ();
								if (d == '\n')
								{
									hasNewline = true;
								}
								else if (d == '/')
								{
									/* Check for nested comment */
									int e = getcFromInputFile ();
									if (e == '*')
									{
										/* Nested comment - recurse */
										int nest = 1;
										while (nest > 0 && d != EOF)
										{
											d = getcFromInputFile ();
											if (d == '*')
											{
												int f = getcFromInputFile ();
												if (f == '/')
													nest--;
												else
													ungetcToInputFile (f);
											}
											else if (d == '/')
											{
												int f = getcFromInputFile ();
												if (f == '*')
													nest++;
												else
													ungetcToInputFile (f);
											}
											else if (d == '\n')
												hasNewline = true;
										}
									}
									else
										ungetcToInputFile (e);
								}
							} while (d != EOF && d != '*');

							c = getcFromInputFile ();
							if (c == '/')
								break;
							else
								ungetcToInputFile (c);
						} while (c != EOF);

						ungetcToInputFile (hasNewline ? '\n' : ' ');
						goto getNextChar;
					default:
						token->type = TOKEN_OTHER;
						ungetcToInputFile (d);
						break;
				}
			}
			break;

		case '"':
		case '\'':
		case '`':
			token->type = TOKEN_STRING;
			parseString (token->string, c);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			break;

		case '-':
			{
				int d = getcFromInputFile ();
				if (d == '>')
					token->type = TOKEN_ARROW;
				else
				{
					ungetcToInputFile (d);
					token->type = TOKEN_OTHER;
				}
			}
			break;

		case ':':
			{
				int d = getcFromInputFile ();
				if (d == ':')
					token->type = TOKEN_DOUBLE_COLON;
				else if (d == '=')
					token->type = TOKEN_COLON_EQUAL;
				else
				{
					ungetcToInputFile (d);
					token->type = TOKEN_COLON;
				}
			}
			break;

		case '(':
			token->type = TOKEN_OPEN_PAREN;
			break;

		case ')':
			token->type = TOKEN_CLOSE_PAREN;
			break;

		case '{':
			token->type = TOKEN_OPEN_CURLY;
			break;

		case '}':
			token->type = TOKEN_CLOSE_CURLY;
			break;

		case '[':
			token->type = TOKEN_OPEN_SQUARE;
			break;

		case ']':
			token->type = TOKEN_CLOSE_SQUARE;
			break;

		case '.':
			token->type = TOKEN_DOT;
			break;

		case ',':
			token->type = TOKEN_COMMA;
			break;

		case '=':
			token->type = TOKEN_EQUAL;
			break;

		case '#':
			token->type = TOKEN_HASH;
			break;

		case '@':
			token->type = TOKEN_AT;
			break;

		case '^':
			token->type = TOKEN_CARET;
			break;

		default:
			if (isStartIdentChar (c))
			{
				parseIdentifier (token->string, c);
				token->lineNumber = getInputLineNumber ();
				token->filePosition = getInputFilePosition ();
				token->keyword = lookupKeyword (vStringValue (token->string), Lang_odin);
				if (isKeyword (token, KEYWORD_NONE))
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			else
				token->type = TOKEN_OTHER;
			break;
	}

	token->c = c;

	if (collector && vStringLength (collector->str) < MAX_COLLECTOR_LENGTH)
		collectorAppendToken (collector, token);
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, NULL);
}

static bool skipToMatchedNoRead (tokenInfo *const token, collector *collector)
{
	int nest_level = 0;
	tokenType open_token = token->type;
	tokenType close_token;

	switch (open_token)
	{
		case TOKEN_OPEN_PAREN:
			close_token = TOKEN_CLOSE_PAREN;
			break;
		case TOKEN_OPEN_CURLY:
			close_token = TOKEN_CLOSE_CURLY;
			break;
		case TOKEN_OPEN_SQUARE:
			close_token = TOKEN_CLOSE_SQUARE;
			break;
		default:
			return false;
	}

	nest_level++;
	while (nest_level > 0 && !isType (token, TOKEN_EOF))
	{
		readTokenFull (token, collector);
		if (isType (token, open_token))
			nest_level++;
		else if (isType (token, close_token))
			nest_level--;
	}

	return true;
}

static void skipToMatched (tokenInfo *const token, collector *collector)
{
	if (skipToMatchedNoRead (token, collector))
		readTokenFull (token, collector);
}

static int makeTagFull (tokenInfo *const token, const odinKind kind,
						const int scope, const char *argList,
						const int role)
{
	const char *const name = vStringValue (token->string);

	tagEntryInfo e;

	/* Don't record `_' placeholder variable  */
	if (kind == ODINTAG_VAR && name[0] == '_' && name[1] == '\0')
		return CORK_NIL;

	initRefTagEntry (&e, name, kind, role);

	updateTagLine (&e, token->lineNumber, token->filePosition);
	if (argList)
		e.extensionFields.signature = argList;

	e.extensionFields.scopeIndex = scope;
	return makeTagEntry (&e);
}

static int makeTag (tokenInfo *const token, const odinKind kind,
					const int scope, const char *argList)
{
	return makeTagFull (token, kind, scope, argList,
						ROLE_DEFINITION_INDEX);
}

static int makeRefTag (tokenInfo *const token, const odinKind kind,
					   const int role)
{
	return makeTagFull (token, kind, CORK_NIL, NULL, role);
}

static int parsePackage (tokenInfo *const token)
{
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		return makeTag (token, ODINTAG_PACKAGE, CORK_NIL, NULL);
	}
	return CORK_NIL;
}

static bool nameHasLower (const vString *name)
{
	for (const char *p = vStringValue (name); *p; p++)
		if (islower ((unsigned char) *p))
			return true;
	return false;
}

static void parseImport (tokenInfo *const token)
{
	/* import "path"
	 * import name "path"
	 */
	readToken (token);

	if (isType (token, TOKEN_IDENTIFIER))
	{
		tokenInfo *nameToken = newToken ();
		copyToken (nameToken, token);
		readToken (token);
		if (isType (token, TOKEN_STRING))
		{
			makeTag (nameToken, ODINTAG_IMPORT_NAME, CORK_NIL, NULL);
			makeRefTag (token, ODINTAG_PACKAGE, R_ODINTAG_PACKAGE_IMPORTED);
		}
		deleteToken (nameToken);
	}
	else if (isType (token, TOKEN_STRING))
	{
		makeRefTag (token, ODINTAG_PACKAGE, R_ODINTAG_PACKAGE_IMPORTED);
	}
}

static void parseForeignBlockProcs (tokenInfo *const token, const int scope)
{
	/* foreign name {
	 *     ProcName :: proc "c" (...) -> RetType ---
	 *     varName: Type
	 * }
	 */
	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		while (isType (token, TOKEN_AT))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
				skipToMatched (token, NULL);
			else if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}

		if (isType (token, TOKEN_CLOSE_CURLY) || isType (token, TOKEN_EOF))
			break;

		if (isType (token, TOKEN_IDENTIFIER))
		{
			tokenInfo *nameToken = newToken ();
			copyToken (nameToken, token);
			readToken (token);

			if (isType (token, TOKEN_DOUBLE_COLON))
			{
				readToken (token);
				if (isKeyword (token, KEYWORD_proc))
				{
					vString *signature = vStringNew ();
					collector sig_collector = { .str = signature, .last_len = 0, };

					readTokenFull (token, &sig_collector);

					if (isType (token, TOKEN_STRING))
					{
						vStringClear (signature);
						readTokenFull (token, &sig_collector);
					}

					if (isType (token, TOKEN_OPEN_PAREN))
					{
						vStringPut (signature, '(');
						skipToMatchedNoRead (token, &sig_collector);
						collectorTruncate (&sig_collector);
					}

					makeTag (nameToken, ODINTAG_PROC, scope,
							 vStringValue (signature));
					vStringDelete (signature);

					readToken (token);
					if (isType (token, TOKEN_ARROW))
					{
						readToken (token);
						if (isType (token, TOKEN_OPEN_PAREN))
							skipToMatched (token, NULL);
						else
						{
							while (!isType (token, TOKEN_EOF) &&
								   !isType (token, TOKEN_CLOSE_CURLY) &&
								   !isType (token, TOKEN_OTHER))
							{
								if (isType (token, TOKEN_OPEN_PAREN) ||
									isType (token, TOKEN_OPEN_SQUARE))
								{
									skipToMatched (token, NULL);
								}
								else
									readToken (token);
							}
						}
					}
					/* --- */
					while (isType (token, TOKEN_OTHER))
						readToken (token);
				}
			}
			else if (isType (token, TOKEN_COLON))
			{
				makeTag (nameToken, ODINTAG_VAR, scope, NULL);
				readToken (token);
				while (!isType (token, TOKEN_EOF) &&
					   !isType (token, TOKEN_CLOSE_CURLY) &&
					   !isType (token, TOKEN_AT) &&
					   !isType (token, TOKEN_IDENTIFIER))
				{
					if (isType (token, TOKEN_OPEN_PAREN) ||
						isType (token, TOKEN_OPEN_SQUARE))
					{
						skipToMatched (token, NULL);
					}
					else
						readToken (token);
				}
			}
			deleteToken (nameToken);
		}
		else
		{
			readToken (token);
		}
	}
}

static void parseForeign (tokenInfo *const token, const int scope)
{
	/* foreign import name "path"
	 * foreign import name { "path1", "path2" }
	 * foreign name { ... }  (proc block)
	 */
	readToken (token);

	if (isKeyword (token, KEYWORD_import))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
		{
			makeTag (token, ODINTAG_FOREIGN, scope, NULL);
			readToken (token);
			if (isType (token, TOKEN_OPEN_CURLY))
				skipToMatchedNoRead (token, NULL);
		}
	}
	else if (isType (token, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isType (token, TOKEN_OPEN_CURLY))
		{
			parseForeignBlockProcs (token, scope);
		}
	}
}

static void parseStructMembers (tokenInfo *const token, const int scope)
{
	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
			tokenInfo *memberToken = newToken ();
			copyToken (memberToken, token);
			bool multiField = false;

			readToken (token);
			if (isType (token, TOKEN_COMMA))
			{
				multiField = true;
				makeTag (memberToken, ODINTAG_MEMBER, scope, NULL);
				while (isType (token, TOKEN_COMMA))
				{
					readToken (token);
					if (isType (token, TOKEN_IDENTIFIER))
					{
						makeTag (token, ODINTAG_MEMBER, scope, NULL);
						readToken (token);
					}
				}
			}

			if (isType (token, TOKEN_COLON))
			{
				if (!multiField)
					makeTag (memberToken, ODINTAG_MEMBER, scope, NULL);
				readToken (token);
				while (!isType (token, TOKEN_EOF) &&
					   !isType (token, TOKEN_COMMA) &&
					   !isType (token, TOKEN_CLOSE_CURLY))
				{
					if (isType (token, TOKEN_OPEN_PAREN) ||
						isType (token, TOKEN_OPEN_CURLY) ||
						isType (token, TOKEN_OPEN_SQUARE))
					{
						skipToMatched (token, NULL);
					}
					else
						readToken (token);
				}
			}
			deleteToken (memberToken);
		}
		else if (isKeyword (token, KEYWORD_using))
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				makeTag (token, ODINTAG_MEMBER, scope, NULL);
				readToken (token);
				if (isType (token, TOKEN_COLON))
				{
					readToken (token);
					while (!isType (token, TOKEN_EOF) &&
						   !isType (token, TOKEN_COMMA) &&
						   !isType (token, TOKEN_CLOSE_CURLY))
					{
						if (isType (token, TOKEN_OPEN_PAREN) ||
							isType (token, TOKEN_OPEN_CURLY) ||
							isType (token, TOKEN_OPEN_SQUARE))
						{
							skipToMatched (token, NULL);
						}
						else
							readToken (token);
					}
				}
			}
		}
		else
		{
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
			readToken (token);
	}
}

static void parseEnumMembers (tokenInfo *const token, const int scope)
{
	readToken (token);

	if (isType (token, TOKEN_IDENTIFIER))
		readToken (token);

	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
			makeTag (token, ODINTAG_ENUMERATOR, scope, NULL);
			readToken (token);

			if (isType (token, TOKEN_EQUAL))
			{
				readToken (token);
				while (!isType (token, TOKEN_EOF) &&
					   !isType (token, TOKEN_COMMA) &&
					   !isType (token, TOKEN_CLOSE_CURLY))
				{
					readToken (token);
				}
			}
		}
		if (isType (token, TOKEN_COMMA))
			readToken (token);
	}
}

static void parseUnionMembers (tokenInfo *const token, const int scope)
{
	readToken (token);

	if (isType (token, TOKEN_OPEN_PAREN))
		skipToMatched (token, NULL);

	while (isType (token, TOKEN_HASH))
	{
		readToken (token);
		readToken (token);
	}

	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	/* union members are types, not named */
	skipToMatchedNoRead (token, NULL);
}

/* ^T, [N]T, map[K]V, proc(...)->T, etc. */
static void skipTypeExpression (tokenInfo *const token)
{
	while (isType (token, TOKEN_CARET))
		readToken (token);

	if (isType (token, TOKEN_OPEN_SQUARE))
		skipToMatched (token, NULL);

	if (isType (token, TOKEN_HASH))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			readToken (token);
		skipTypeExpression (token);
		return;
	}

	if (isKeyword (token, KEYWORD_map))
	{
		readToken (token);
		if (isType (token, TOKEN_OPEN_SQUARE))
			skipToMatched (token, NULL);
	}

	if (isKeyword (token, KEYWORD_proc))
	{
		readToken (token);
		if (isType (token, TOKEN_STRING))
			readToken (token);
		if (isType (token, TOKEN_OPEN_PAREN))
			skipToMatched (token, NULL);
		if (isType (token, TOKEN_ARROW))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
				skipToMatched (token, NULL);
			else
				skipTypeExpression (token);
		}
		return;
	}

	if (isType (token, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isType (token, TOKEN_DOT))
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}
		if (isType (token, TOKEN_OPEN_PAREN))
			skipToMatched (token, NULL);
	}
}

static void parseProcedure (tokenInfo *const token, tokenInfo *const nameToken, const int scope)
{
	/* proc "conv" (args) -> ret { body } */
	vString *signature = vStringNew ();
	collector sig_collector = { .str = signature, .last_len = 0, };

	readTokenFull (token, &sig_collector);

	if (isType (token, TOKEN_STRING))
	{
		vStringClear (signature);
		readTokenFull (token, &sig_collector);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		/* proc group: proc{a, b} */
		makeTag (nameToken, ODINTAG_PROC, scope, NULL);
		skipToMatchedNoRead (token, NULL);
		vStringDelete (signature);
		return;
	}

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (signature, '(');
		skipToMatchedNoRead (token, &sig_collector);
		collectorTruncate (&sig_collector);
	}

	int cork = makeTag (nameToken, ODINTAG_PROC, scope, vStringValue (signature));

	readToken (token);

	if (isType (token, TOKEN_ARROW))
	{
		readToken (token);
		if (isType (token, TOKEN_OPEN_PAREN))
			skipToMatched (token, NULL);
		else
			skipTypeExpression (token);
	}

	while (isType (token, TOKEN_HASH))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			readToken (token);
	}

	if (isKeyword (token, KEYWORD_where))
	{
		while (!isType (token, TOKEN_EOF) &&
			   !isType (token, TOKEN_OPEN_CURLY) &&
			   !isType (token, TOKEN_SEMICOLON))
		{
			readToken (token);
		}
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		skipToMatchedNoRead (token, NULL);
		tagEntryInfo *e = getEntryInCorkQueue (cork);
		if (e)
			setTagEndLine (e, getInputLineNumber());
	}
	else
	{
		/* bodyless: reclassify as type alias */
		tagEntryInfo *e = getEntryInCorkQueue (cork);
		if (e)
			e->kindIndex = ODINTAG_TYPE;
	}

	vStringDelete (signature);
}

static void parseDeclaration (tokenInfo *const token, const int scope)
{
	/* name :: proc(...)
	 * name :: struct/enum/union { ... }
	 * name :: distinct Type
	 * name :: value  (constant)
	 * name : type = value  (variable)
	 * name : type : value  (typed constant)
	 * name := value  (variable)
	 */
	tokenInfo *nameToken = newToken ();
	copyToken (nameToken, token);

	readToken (token);

	if (isType (token, TOKEN_DOUBLE_COLON))
	{
		readToken (token);

		if (isKeyword (token, KEYWORD_proc))
		{
			parseProcedure (token, nameToken, scope);
		}
		else if (isKeyword (token, KEYWORD_struct))
		{
			int member_scope = makeTag (nameToken, ODINTAG_STRUCT, scope, NULL);
			if (member_scope != CORK_NIL)
				registerEntry (member_scope);

			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
				skipToMatched (token, NULL);

			while (isType (token, TOKEN_HASH))
			{
				readToken (token);
				readToken (token);
				if (isType (token, TOKEN_OPEN_PAREN))
					skipToMatched (token, NULL);
			}

			if (isType (token, TOKEN_OPEN_CURLY))
			{
				parseStructMembers (token, member_scope);
			}

			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_enum))
		{
			int member_scope = makeTag (nameToken, ODINTAG_ENUM, scope, NULL);
			if (member_scope != CORK_NIL)
				registerEntry (member_scope);
			parseEnumMembers (token, member_scope);
			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_union))
		{
			int member_scope = makeTag (nameToken, ODINTAG_UNION, scope, NULL);
			parseUnionMembers (token, member_scope);
			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_bit_set))
		{
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				skipToMatchedNoRead (token, NULL);
		}
		else if (isKeyword (token, KEYWORD_bit_field))
		{
			int member_scope = makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
			if (member_scope != CORK_NIL)
				registerEntry (member_scope);

			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);

			if (isType (token, TOKEN_OPEN_CURLY))
			{
				/* bit_field | widths are TOKEN_OTHER, so struct parsing works */
				parseStructMembers (token, member_scope);
			}

			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_distinct))
		{
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
			readToken (token);
			while (isType (token, TOKEN_CARET))
				readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				skipToMatchedNoRead (token, NULL);
			else if (isKeyword (token, KEYWORD_map))
			{
				readToken (token);
				if (isType (token, TOKEN_OPEN_SQUARE))
					skipToMatched (token, NULL);
			}
			/* parseBlock's readToken advances past this */
		}
		else if (isKeyword (token, KEYWORD_map))
		{
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				skipToMatched (token, NULL);
		}
		else if (isType (token, TOKEN_IDENTIFIER))
		{
			/* Lowercase in name => type alias, else constant */
			if (nameHasLower (nameToken->string))
				makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
			else
				makeTag (nameToken, ODINTAG_CONST, scope, NULL);
		}
		else if (isType (token, TOKEN_STRING) ||
				 isType (token, TOKEN_OTHER))
		{
			makeTag (nameToken, ODINTAG_CONST, scope, NULL);
		}
		else if (isType (token, TOKEN_CARET) ||
				 isType (token, TOKEN_HASH) ||
				 isType (token, TOKEN_OPEN_SQUARE))
		{
			if (isType (token, TOKEN_HASH))
			{
				readToken (token);
				if (isType (token, TOKEN_IDENTIFIER))
					readToken (token);
				if (isKeyword (token, KEYWORD_proc))
				{
					parseProcedure (token, nameToken, scope);
				}
				else
				{
					if (nameHasLower (nameToken->string))
						makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
					else
						makeTag (nameToken, ODINTAG_CONST, scope, NULL);
					if (isType (token, TOKEN_OPEN_PAREN) ||
						isType (token, TOKEN_OPEN_SQUARE))
					{
						skipToMatchedNoRead (token, NULL);
					}
				}
			}
			else
			{
				makeTag (nameToken, ODINTAG_TYPE, scope, NULL);
				if (isType (token, TOKEN_CARET))
				{
					while (isType (token, TOKEN_CARET))
						readToken (token);
				}
				else if (isType (token, TOKEN_OPEN_SQUARE))
				{
					skipToMatched (token, NULL);
				}
			}
		}
		else if (isType (token, TOKEN_OPEN_PAREN) ||
				 isType (token, TOKEN_OPEN_CURLY))
		{
			makeTag (nameToken, ODINTAG_CONST, scope, NULL);
		}
	}
	else if (isType (token, TOKEN_COLON))
	{
		readToken (token);

		while (!isType (token, TOKEN_EOF) &&
			   !isType (token, TOKEN_EQUAL) &&
			   !isType (token, TOKEN_COLON) &&
			   !isType (token, TOKEN_SEMICOLON) &&
			   !isType (token, TOKEN_CLOSE_CURLY) &&
			   !isType (token, TOKEN_COMMA) &&
			   !isType (token, TOKEN_AT) &&
			   !isType (token, TOKEN_HASH))
		{
			if (isType (token, TOKEN_OPEN_PAREN) ||
				isType (token, TOKEN_OPEN_CURLY) ||
				isType (token, TOKEN_OPEN_SQUARE))
			{
				skipToMatched (token, NULL);
			}
			else
				readToken (token);
		}

		if (isType (token, TOKEN_COLON))
			makeTag (nameToken, ODINTAG_CONST, scope, NULL);
		else
			makeTag (nameToken, ODINTAG_VAR, scope, NULL);
	}
	else if (isType (token, TOKEN_COLON_EQUAL))
	{
		makeTag (nameToken, ODINTAG_VAR, scope, NULL);
	}
	else if (isType (token, TOKEN_COMMA))
	{
		makeTag (nameToken, ODINTAG_VAR, scope, NULL);

		while (isType (token, TOKEN_COMMA))
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				makeTag (token, ODINTAG_VAR, scope, NULL);
				readToken (token);
			}
		}
	}

	deleteToken (nameToken);
}

static void parseBlock (tokenInfo *const token, int scope, bool isTopLevel);

static void parseWhenBlock (tokenInfo *const token, const int scope)
{
	/* when cond { ... } else when cond { ... } else { ... } */
	readToken (token);

	for (;;)
	{
		/* Bail on declaration operators — expression-level when */
		while (!isType (token, TOKEN_EOF) &&
			   !isType (token, TOKEN_OPEN_CURLY) &&
			   !isType (token, TOKEN_DOUBLE_COLON) &&
			   !isType (token, TOKEN_COLON) &&
			   !isType (token, TOKEN_COLON_EQUAL))
		{
			if (isType (token, TOKEN_OPEN_PAREN) ||
				isType (token, TOKEN_OPEN_SQUARE))
			{
				skipToMatched (token, NULL);
			}
			else
				readToken (token);
		}

		if (!isType (token, TOKEN_OPEN_CURLY))
			break;

		readToken (token);
		parseBlock (token, scope, false);

		readToken (token);
		if (isKeyword (token, KEYWORD_else))
		{
			readToken (token);
			if (isKeyword (token, KEYWORD_when))
			{
				readToken (token);
				continue;
			}
			continue;
		}
		else
			break;
	}
}

static void parseBlock (tokenInfo *const token, int scope, bool isTopLevel)
{
	tokenType stopToken = isTopLevel ? TOKEN_EOF : TOKEN_CLOSE_CURLY;

	if (isTopLevel)
		readToken (token);

	while (!isType (token, TOKEN_EOF) && !isType (token, stopToken))
	{
		while (isType (token, TOKEN_AT))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
				skipToMatched (token, NULL);
			else if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}

		if (isType (token, stopToken) || isType (token, TOKEN_EOF))
			break;

		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_package:
					if (isTopLevel)
						scope = parsePackage (token);
					break;
				case KEYWORD_import:
					parseImport (token);
					break;
				case KEYWORD_foreign:
					parseForeign (token, scope);
					break;
				case KEYWORD_when:
					parseWhenBlock (token, scope);
					continue;
				case KEYWORD_if:
				case KEYWORD_for:
				case KEYWORD_switch:
					break;
				default:
					break;
			}
		}
		else if (isType (token, TOKEN_HASH))
		{
			readToken (token);
			if (isType (token, TOKEN_OTHER))
			{
				/* #+build — skip line to avoid misparsing */
				skipToCharacterInInputFile ('\n');
			}
			continue;
		}
		else if (isType (token, TOKEN_IDENTIFIER))
		{
			parseDeclaration (token, scope);
			/* parseDeclaration may leave token at next statement */
			if (isType (token, TOKEN_AT) || isType (token, TOKEN_HASH) ||
				isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
				continue;
		}
		else if (isType (token, TOKEN_OPEN_PAREN) ||
				 isType (token, TOKEN_OPEN_CURLY) ||
				 isType (token, TOKEN_OPEN_SQUARE))
		{
			skipToMatched (token, NULL);
			continue;
		}

		readToken (token);
	}
}

static void parseOdinFile (tokenInfo *const token)
{
	parseBlock (token, CORK_NIL, true);
}

static void findOdinTags (void)
{
	tokenInfo *const token = newToken ();

	parseOdinFile (token);

	deleteToken (token);
}

extern parserDefinition *OdinParser (void)
{
	static const char *const extensions[] = { "odin", NULL };
	parserDefinition *def = parserNew ("Odin");
	def->kindTable = OdinKinds;
	def->kindCount = ARRAY_SIZE (OdinKinds);
	def->extensions = extensions;
	def->parser = findOdinTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->keywordTable = OdinKeywordTable;
	def->keywordCount = ARRAY_SIZE (OdinKeywordTable);
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	def->requestAutomaticFQTag = true;
	return def;
}
