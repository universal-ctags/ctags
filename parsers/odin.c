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
	ODINTAG_FOREIGN
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
	{true, 'g', "foreign", "foreign imports"}
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
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
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
			if (c != '\'' && c != '\"')
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
	else if (token->type == TOKEN_DOUBLE_COLON)
		collectorCatS (collector, "::");
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

static void collectorTruncate (collector *collector, bool dropLast)
{
	if (dropLast)
		vStringTruncate (collector->str, collector->last_len);

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
						} while (c != EOF && c != '\0');

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
						const int scope, const char *argList, const char *typeref,
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
	if (typeref)
	{
		e.extensionFields.typeRef [0] = "typename";
		e.extensionFields.typeRef [1] = typeref;
	}

	e.extensionFields.scopeIndex = scope;
	return makeTagEntry (&e);
}

static int makeTag (tokenInfo *const token, const odinKind kind,
					const int scope, const char *argList, const char *typeref)
{
	return makeTagFull (token, kind, scope, argList, typeref,
						ROLE_DEFINITION_INDEX);
}

static int makeRefTag (tokenInfo *const token, const odinKind kind,
					   const int role)
{
	return makeTagFull (token, kind, CORK_NIL, NULL, NULL, role);
}

static int parsePackage (tokenInfo *const token)
{
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		return makeTag (token, ODINTAG_PACKAGE, CORK_NIL, NULL, NULL);
	}
	return CORK_NIL;
}

static void parseImport (tokenInfo *const token, const int scope)
{
	/* import "path"
	 * import name "path"
	 * import name "path" { ... } - for foreign
	 */
	readToken (token);

	if (isType (token, TOKEN_IDENTIFIER))
	{
		/* import name "path" */
		tokenInfo *nameToken = newToken ();
		copyToken (nameToken, token);

		readToken (token);
		if (isType (token, TOKEN_STRING))
		{
			makeRefTag (token, ODINTAG_PACKAGE, R_ODINTAG_PACKAGE_IMPORTED);
		}
		deleteToken (nameToken);
	}
	else if (isType (token, TOKEN_STRING))
	{
		/* import "path" */
		makeRefTag (token, ODINTAG_PACKAGE, R_ODINTAG_PACKAGE_IMPORTED);
	}
}

static void parseForeignBlockProcs (tokenInfo *const token, const int scope)
{
	/* Parse procedure declarations inside a foreign block:
	 * foreign name {
	 *     ProcName :: proc "c" (...) -> RetType ---
	 *     varName: Type
	 *     @(link_name="foo") Bar :: proc(...) ---
	 * }
	 */
	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		/* Skip attributes: @(...) or @name */
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

					/* Skip calling convention string: proc "c" (...) */
					if (isType (token, TOKEN_STRING))
					{
						vStringClear (signature);
						readTokenFull (token, &sig_collector);
					}

					if (isType (token, TOKEN_OPEN_PAREN))
					{
						vStringPut (signature, '(');
						skipToMatchedNoRead (token, &sig_collector);
						collectorTruncate (&sig_collector, false);
					}

					makeTag (nameToken, ODINTAG_PROC, scope,
							 vStringValue (signature), NULL);
					vStringDelete (signature);

					/* Skip past -> RetType and --- */
					readToken (token);
					if (isType (token, TOKEN_ARROW))
					{
						readToken (token);
						if (isType (token, TOKEN_OPEN_PAREN))
							skipToMatched (token, NULL);
						else
						{
							/* Skip return type tokens until --- delimiter */
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
					/* Skip --- (TOKEN_OTHER for each '-') */
					while (isType (token, TOKEN_OTHER))
						readToken (token);
				}
				else
				{
					makeTag (nameToken, ODINTAG_CONST, scope, NULL, NULL);
					readToken (token);
				}
			}
			else if (isType (token, TOKEN_COLON))
			{
				/* name: type (variable) — skip the type expression,
				 * handling brackets so we don't stop inside e.g.
				 * proc "c" (sig: i32) */
				makeTag (nameToken, ODINTAG_VAR, scope, NULL, NULL);
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
			/* else: not a declaration, token already advanced */

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
	 * foreign name { ... }  -- block with procedure declarations
	 */
	readToken (token);

	if (isKeyword (token, KEYWORD_import))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
		{
			makeTag (token, ODINTAG_FOREIGN, scope, NULL, NULL);
			readToken (token);
			if (isType (token, TOKEN_OPEN_CURLY))
				skipToMatched (token, NULL);
		}
	}
	else if (isType (token, TOKEN_IDENTIFIER))
	{
		/* foreign name { ... } -- block with procedure declarations */
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
				/* Multiple fields: a, b, c: type */
				multiField = true;
				makeTag (memberToken, ODINTAG_MEMBER, scope, NULL, NULL);
				while (isType (token, TOKEN_COMMA))
				{
					readToken (token);
					if (isType (token, TOKEN_IDENTIFIER))
					{
						makeTag (token, ODINTAG_MEMBER, scope, NULL, NULL);
						readToken (token);
					}
				}
			}

			if (isType (token, TOKEN_COLON))
			{
				/* field: type  OR  a, b, c: type — skip the type */
				if (!multiField)
					makeTag (memberToken, ODINTAG_MEMBER, scope, NULL, NULL);
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
			/* using field: create member tag then skip type */
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				makeTag (token, ODINTAG_MEMBER, scope, NULL, NULL);
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
	/* enum { A, B, C = 5, ... } */
	readToken (token);

	/* Skip generic params: enum($T: typeid) */
	if (isType (token, TOKEN_OPEN_PAREN))
		skipToMatched (token, NULL);

	/* Skip optional base type */
	if (isType (token, TOKEN_IDENTIFIER))
		readToken (token);

	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
			makeTag (token, ODINTAG_ENUMERATOR, scope, NULL, NULL);
			readToken (token);

			/* Skip optional value assignment */
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
		else if (isType (token, TOKEN_DOT))
		{
			/* .Member syntax for enum */
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				makeTag (token, ODINTAG_ENUMERATOR, scope, NULL, NULL);
				readToken (token);
			}
		}

		if (isType (token, TOKEN_COMMA))
			readToken (token);
	}
}

static void parseUnionMembers (tokenInfo *const token, const int scope)
{
	/* union { Type1, Type2, ... } */
	readToken (token);

	/* Skip generic params: union($T: typeid) */
	if (isType (token, TOKEN_OPEN_PAREN))
		skipToMatched (token, NULL);

	/* Skip #no_nil etc */
	while (isType (token, TOKEN_HASH))
	{
		readToken (token); /* skip directive name */
		readToken (token);
	}

	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	/* Just skip the union body - members are types, not named */
	skipToMatchedNoRead (token, NULL);
}

/* Skip a single type expression, leaving token at the first token
 * past the type. Handles ^T, [N]T, []T, map[K]V, proc(...)->T,
 * #type proc(...), distinct T, pkg.Type, and generic Type(T). */
static void skipTypeExpression (tokenInfo *const token)
{
	/* Prefix operators: ^, ^^, etc. */
	while (isType (token, TOKEN_CARET))
		readToken (token);

	/* Array/slice prefix: [N] or [] */
	if (isType (token, TOKEN_OPEN_SQUARE))
		skipToMatched (token, NULL);

	/* Hash directive: #type, #simd, etc. */
	if (isType (token, TOKEN_HASH))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			readToken (token);
	}

	/* distinct T */
	if (isKeyword (token, KEYWORD_distinct))
	{
		readToken (token);
		skipTypeExpression (token);
		return;
	}

	/* map[K]V */
	if (isKeyword (token, KEYWORD_map))
	{
		readToken (token);
		if (isType (token, TOKEN_OPEN_SQUARE))
			skipToMatched (token, NULL);
	}

	/* proc "conv" (...) -> T */
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

	/* Base identifier, possibly qualified (pkg.Type) or generic (Type(T)) */
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
	/* proc(args) -> return_type { body } */
	vString *signature = vStringNew ();
	collector sig_collector = { .str = signature, .last_len = 0, };

	readTokenFull (token, &sig_collector);

	/* Skip calling convention string: proc "c" (...) or proc "cdecl" (...) */
	if (isType (token, TOKEN_STRING))
	{
		vStringClear (signature);
		readTokenFull (token, &sig_collector);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		/* Procedure group: name :: proc{proc1, proc2} */
		makeTag (nameToken, ODINTAG_PROC, scope, NULL, NULL);
		skipToMatchedNoRead (token, NULL);
		vStringDelete (signature);
		return;
	}

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (signature, '(');
		skipToMatchedNoRead (token, &sig_collector);
		collectorTruncate (&sig_collector, false);
	}

	int cork = makeTag (nameToken, ODINTAG_PROC, scope, vStringValue (signature), NULL);

	readToken (token);

	if (isType (token, TOKEN_ARROW))
	{
		readToken (token);
		/* Skip return type(s) */
		if (isType (token, TOKEN_OPEN_PAREN))
			skipToMatched (token, NULL);
		else
			skipTypeExpression (token);
	}

	/* Skip 'where' clause if present */
	if (isKeyword (token, KEYWORD_where))
	{
		while (!isType (token, TOKEN_EOF) &&
			   !isType (token, TOKEN_OPEN_CURLY) &&
			   !isType (token, TOKEN_SEMICOLON))
		{
			readToken (token);
		}
	}

	/* Body present — this is a proc definition */
	if (isType (token, TOKEN_OPEN_CURLY))
	{
		skipToMatchedNoRead (token, NULL);
		tagEntryInfo *e = getEntryInCorkQueue (cork);
		if (e)
			setTagEndLine (e, getInputLineNumber());
	}
	else if (isType (token, TOKEN_OTHER) && token->c == '-')
	{
		/* --- foreign proc marker — still a proc, skip the dashes */
		while (isType (token, TOKEN_OTHER) && token->c == '-')
			readToken (token);
	}
	else
	{
		/* No body and no --- — procedure type, not definition. */
		tagEntryInfo *e = getEntryInCorkQueue (cork);
		if (e)
			e->kindIndex = ODINTAG_TYPE;
	}

	vStringDelete (signature);
}

static void parseDeclaration (tokenInfo *const token, const int scope)
{
	/* name :: value
	 * name :: proc(...)
	 * name :: struct { ... }
	 * name :: enum { ... }
	 * name :: union { ... }
	 * name :: distinct Type
	 * name : type = value
	 * name : type : value  (typed constant)
	 * name := value
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
			int member_scope = makeTag (nameToken, ODINTAG_STRUCT, scope, NULL, NULL);
			if (member_scope != CORK_NIL)
				registerEntry (member_scope);

			/* Skip generic params: struct($T: typeid) */
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
				skipToMatched (token, NULL);

			/* Skip struct attributes like #packed, #align etc */
			while (isType (token, TOKEN_HASH))
			{
				readToken (token); /* skip attribute name */
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
			int member_scope = makeTag (nameToken, ODINTAG_ENUM, scope, NULL, NULL);
			if (member_scope != CORK_NIL)
				registerEntry (member_scope);
			parseEnumMembers (token, member_scope);
			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_union))
		{
			int member_scope = makeTag (nameToken, ODINTAG_UNION, scope, NULL, NULL);
			parseUnionMembers (token, member_scope);
			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_bit_set))
		{
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL, NULL);
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				skipToMatchedNoRead (token, NULL);
		}
		else if (isKeyword (token, KEYWORD_bit_field))
		{
			int member_scope = makeTag (nameToken, ODINTAG_TYPE, scope, NULL, NULL);
			if (member_scope != CORK_NIL)
				registerEntry (member_scope);

			/* Skip backing type (e.g. u16) */
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);

			if (isType (token, TOKEN_OPEN_CURLY))
			{
				/* Reuse struct member parsing — `| width` is skipped as TOKEN_OTHER */
				parseStructMembers (token, member_scope);
			}

			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				setTagEndLine (e, getInputLineNumber());
		}
		else if (isKeyword (token, KEYWORD_distinct))
		{
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL, NULL);
			/* Skip the underlying type expression */
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
			/* token is now at the final type identifier;
			 * parseBlock's readToken will advance past it */
		}
		else if (isKeyword (token, KEYWORD_map))
		{
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL, NULL);
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				skipToMatched (token, NULL);
		}
		else if (isType (token, TOKEN_IDENTIFIER))
		{
			/* name :: Type  (type alias) or name :: value (constant)
			 * By convention, constants are SCREAMING_SNAKE_CASE (all
			 * uppercase). If the name contains any lowercase letter,
			 * it is a type alias. */
			const char *name = vStringValue (nameToken->string);
			bool has_lower = false;
			for (const char *p = name; *p; p++)
			{
				if (islower ((unsigned char) *p))
				{
					has_lower = true;
					break;
				}
			}
			if (has_lower)
				makeTag (nameToken, ODINTAG_TYPE, scope, NULL, NULL);
			else
				makeTag (nameToken, ODINTAG_CONST, scope, NULL, NULL);
		}
		else if (isType (token, TOKEN_STRING) ||
				 isType (token, TOKEN_OTHER))
		{
			/* name :: "string" or name :: 123 (constant) */
			makeTag (nameToken, ODINTAG_CONST, scope, NULL, NULL);
		}
		else if (isType (token, TOKEN_CARET) ||
				 isType (token, TOKEN_HASH) ||
				 isType (token, TOKEN_OPEN_SQUARE))
		{
			/* name :: ^Type, name :: #type proc(...), name :: [N]Type */
			makeTag (nameToken, ODINTAG_TYPE, scope, NULL, NULL);
			if (isType (token, TOKEN_CARET))
			{
				while (isType (token, TOKEN_CARET))
					readToken (token);
				/* token at type identifier */
			}
			else if (isType (token, TOKEN_OPEN_SQUARE))
			{
				skipToMatched (token, NULL);
				/* token at element type identifier */
			}
			/* For HASH, leave at #; parseBlock handles it via continue */
		}
		else if (isType (token, TOKEN_OPEN_PAREN) ||
				 isType (token, TOKEN_OPEN_CURLY))
		{
			makeTag (nameToken, ODINTAG_CONST, scope, NULL, NULL);
		}
	}
	else if (isType (token, TOKEN_COLON))
	{
		readToken (token);

		/* Skip the type - stop at tokens that can't be part of a type expression */
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
			makeTag (nameToken, ODINTAG_CONST, scope, NULL, NULL);
		else
			makeTag (nameToken, ODINTAG_VAR, scope, NULL, NULL);
	}
	else if (isType (token, TOKEN_COLON_EQUAL))
	{
		makeTag (nameToken, ODINTAG_VAR, scope, NULL, NULL);
	}
	else if (isType (token, TOKEN_COMMA))
	{
		makeTag (nameToken, ODINTAG_VAR, scope, NULL, NULL);

		while (isType (token, TOKEN_COMMA))
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				makeTag (token, ODINTAG_VAR, scope, NULL, NULL);
				readToken (token);
			}
		}
	}

	deleteToken (nameToken);
}

static void parseBlock (tokenInfo *const token, int scope, bool isTopLevel);

static void parseWhenBlock (tokenInfo *const token, const int scope)
{
	/* when condition { ... } else when condition { ... } else { ... }
	 * Skip condition tokens until '{', then parse the body as declarations. */
	readToken (token);

	for (;;)
	{
		/* Skip condition until '{'.
		 * Bail out if we hit '::' / ':' / ':=' — that means this was an
		 * expression-level `when` (ternary), not a block-level one, and
		 * we've overshot into the next declaration. */
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
		{
			/* Not a block when — bail, let caller handle current token */
			break;
		}

		/* Parse the body as top-level declarations */
		readToken (token);
		parseBlock (token, scope, false);
		/* token is now at '}' or EOF */

		/* Check for else / else when */
		readToken (token);
		if (isKeyword (token, KEYWORD_else))
		{
			readToken (token);
			if (isKeyword (token, KEYWORD_when))
			{
				/* else when condition { ... } — continue loop */
				readToken (token);
				continue;
			}
			/* else { ... } — token should be at '{' or condition */
			continue;
		}
		else
		{
			/* No else — we're done */
			break;
		}
	}
}

static void parseBlock (tokenInfo *const token, int scope, bool isTopLevel)
{
	tokenType stopToken = isTopLevel ? TOKEN_EOF : TOKEN_CLOSE_CURLY;

	if (isTopLevel)
		readToken (token);

	while (!isType (token, TOKEN_EOF) && !isType (token, stopToken))
	{
		/* Skip any attributes before a declaration */
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
					parseImport (token, scope);
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
			/* Skip directives like #assert, #load, #+build, etc.
			 * #+build has '+' (TOKEN_OTHER) before the directive name,
			 * and the rest of the line is a comma-separated list that
			 * would otherwise be misparsed as variable declarations. */
			readToken (token);
			if (isType (token, TOKEN_OTHER))
			{
				/* #+build or similar — skip to end of line */
				skipToCharacterInInputFile ('\n');
			}
			continue;
		}
		else if (isType (token, TOKEN_IDENTIFIER))
		{
			parseDeclaration (token, scope);
			/* parseDeclaration may leave token at the start of the next
			 * statement (e.g. bodyless proc types read one token ahead) */
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
