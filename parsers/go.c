/*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Reference:
*     https://golang.org/ref/spec
*/


/*
 *   INCLUDE FILES
 */
#include "general.h"        /* must always come first */

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "read.h"
#include "numarray.h"
#include "objpool.h"
#include "parse.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"
#include "field.h"
#include "htable.h"

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
	KEYWORD_const,
	KEYWORD_type,
	KEYWORD_var,
	KEYWORD_func,
	KEYWORD_struct,
	KEYWORD_interface,
	KEYWORD_map,
	KEYWORD_chan
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_NONE = -1,
	// Token not important for top-level Go parsing
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
	TOKEN_STAR,
	TOKEN_LEFT_ARROW,
	TOKEN_DOT,
	TOKEN_COMMA,
	TOKEN_EQUAL,
	TOKEN_3DOTS,
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

static int Lang_go;
static objPool *TokenPool = NULL;

typedef enum {
	GOTAG_UNDEFINED = -1,
	GOTAG_PACKAGE,
	GOTAG_FUNCTION,
	GOTAG_CONST,
	GOTAG_TYPE,
	GOTAG_VAR,
	GOTAG_STRUCT,
	GOTAG_INTERFACE,
	GOTAG_MEMBER,
	GOTAG_ANONMEMBER,
	GOTAG_METHODSPEC,
	GOTAG_UNKNOWN,
	GOTAG_PACKAGE_NAME,
	GOTAG_TALIAS,
	GOTAG_RECEIVER,
} goKind;

typedef enum {
	R_GOTAG_PACKAGE_IMPORTED,
} GoPackageRole;

static roleDefinition GoPackageRoles [] = {
	{ true, "imported", "imported package" },
};

typedef enum {
	R_GOTAG_UNKNOWN_RECEIVER,
} GoUnknownRole;

static roleDefinition GoUnknownRoles [] = {
	{ true, "receiverType", "receiver type" },
};

static kindDefinition GoKinds[] = {
	{true, 'p', "package", "packages",
	  .referenceOnly = false, ATTACH_ROLES (GoPackageRoles)},
	{true, 'f', "func", "functions"},
	{true, 'c', "const", "constants"},
	{true, 't', "type", "types"},
	{true, 'v', "var", "variables"},
	{true, 's', "struct", "structs"},
	{true, 'i', "interface", "interfaces"},
	{true, 'm', "member", "struct members"},
	{true, 'M', "anonMember", "struct anonymous members"},
	{true, 'n', "methodSpec", "interface method specification"},
	{true, 'u', "unknown", "unknown",
	 .referenceOnly = true, ATTACH_ROLES (GoUnknownRoles)},
	{true, 'P', "packageName", "name for specifying imported package"},
	{true, 'a', "talias", "type aliases"},
	{false,'R', "receiver", "receivers"},
};

static const keywordTable GoKeywordTable[] = {
	{"package", KEYWORD_package},
	{"import", KEYWORD_import},
	{"const", KEYWORD_const},
	{"type", KEYWORD_type},
	{"var", KEYWORD_var},
	{"func", KEYWORD_func},
	{"struct", KEYWORD_struct},
	{"interface", KEYWORD_interface},
	{"map", KEYWORD_map},
	{"chan", KEYWORD_chan}
};

typedef enum {
	F_PACKAGE,
	F_PACKAGE_NAME,
	F_HOW_IMPORTED,
} goField;

static fieldDefinition GoFields [] = {
	{
		.name = "package",
		.description = "the real package specified by the package name",
		.enabled = true,
	},
	{
		.name = "packageName",
		.description = "the name for referring the package",
		.enabled = true,
	},
	{
		.name = "howImported",
		.description = "how the package is imported (\"inline\" for `.' or \"init\" for `_')",
		.enabled = false,
	},
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
	vStringCopy(dest->string, other->string);
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
	Lang_go = language;
	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (const langType language, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

/*
 *   Parsing functions
 */

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
	ungetcToInputFile (c);		/* always unget, LF might add a semicolon */
}

static bool collectorIsEmpty(collector *collector)
{
	return !vStringLength(collector->str);
}

static void collectorPut (collector *collector, char c)
{
	if ((vStringLength(collector->str) > 2)
		&& strcmp (vStringValue (collector->str) + (vStringLength(collector->str) - 3),
				  "...") == 0
		&& c == ' ')
		return;
	else if (vStringLength(collector->str) > 0)
	{
		if (vStringLast(collector->str) == '(' && c == ' ')
			return;
		else if (vStringLast(collector->str) == ' ' && c == ')')
			vStringChop(collector->str);
	}

	collector->last_len = vStringLength (collector->str);
	vStringPut (collector->str, c);
}

static void collectorCatS (collector *collector, char *cstr)
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
	if (token->type == TOKEN_LEFT_ARROW)
		collectorCatS (collector, "<-");
	else if (token->type == TOKEN_STRING)
	{
		// only struct member annotations can appear in function prototypes
		// so only `` type strings are possible
		collector->last_len = vStringLength (collector->str);
		vStringPut(collector->str, '`');
		vStringCat(collector->str, token->string);
		vStringPut(collector->str, '`');
	}
	else if (token->type == TOKEN_IDENTIFIER || token->type == TOKEN_KEYWORD)
		collectorCat (collector, token->string);
	else if (token->type == TOKEN_3DOTS)
	{
		if ((vStringLength (collector->str) > 0)
			&& vStringLast(collector->str) != ' ')
			collectorPut (collector, ' ');
		collectorCatS (collector, "...");
	}
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
	static tokenType lastTokenType = TOKEN_NONE;
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
		if (c == '\n' && (lastTokenType == TOKEN_IDENTIFIER ||
						  lastTokenType == TOKEN_STRING ||
						  lastTokenType == TOKEN_OTHER ||
						  lastTokenType == TOKEN_CLOSE_PAREN ||
						  lastTokenType == TOKEN_CLOSE_CURLY ||
						  lastTokenType == TOKEN_CLOSE_SQUARE))
		{
			c = ';';  // semicolon injection
		}
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
						/* Line comments start with the
						 * character sequence // and
						 * continue through the next
						 * newline. A line comment acts
						 * like a newline.  */
						ungetcToInputFile ('\n');
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

		case '<':
			{
				int d = getcFromInputFile ();
				if (d == '-')
					token->type = TOKEN_LEFT_ARROW;
				else
				{
					ungetcToInputFile (d);
					token->type = TOKEN_OTHER;
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

		case '*':
			token->type = TOKEN_STAR;
			break;

		case '.':
			{
				int d, e;
				d = getcFromInputFile ();
				if (d == '.')
				{
					e = getcFromInputFile ();
					if (e == '.')
					{
						token->type = TOKEN_3DOTS;
						break;
					}
					else
					{
						ungetcToInputFile (e);
						ungetcToInputFile (d);
					}
				}
				else
					ungetcToInputFile (d);
			}
			token->type = TOKEN_DOT;
			break;

		case ',':
			token->type = TOKEN_COMMA;
			break;

		case '=':
			token->type = TOKEN_EQUAL;
			break;

		default:
			if (isStartIdentChar (c))
			{
				parseIdentifier (token->string, c);
				token->lineNumber = getInputLineNumber ();
				token->filePosition = getInputFilePosition ();
				token->keyword = lookupKeyword (vStringValue (token->string), Lang_go);
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

	lastTokenType = token->type;
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

	/*
	 * This routine will skip to a matching closing token.
	 * It will also handle nested tokens.
	 */
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

static bool skipType (tokenInfo *const token, collector *collector)
{
	// Type      = TypeName | TypeLit | "(" Type ")" .
	// Skips also function multiple return values "(" Type {"," Type} ")"
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipToMatched (token, collector);
		return true;
	}

	// TypeName  = QualifiedIdent.
	// QualifiedIdent = [ PackageName "." ] identifier .
	// PackageName    = identifier .
	if (isType (token, TOKEN_IDENTIFIER))
	{
		readTokenFull (token, collector);
		if (isType (token, TOKEN_DOT))
		{
			readTokenFull (token, collector);
			if (isType (token, TOKEN_IDENTIFIER))
				readTokenFull (token, collector);
		}
		return true;
	}

	// StructType     = "struct" "{" { FieldDecl ";" } "}"
	// InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
	if (isKeyword (token, KEYWORD_struct) || isKeyword (token, KEYWORD_interface))
	{
		readTokenFull (token, collector);
		// skip over "{}"
		skipToMatched (token, collector);
		return true;
	}

	// ArrayType   = "[" ArrayLength "]" ElementType .
	// SliceType = "[" "]" ElementType .
	// ElementType = Type .
	if (isType (token, TOKEN_OPEN_SQUARE))
	{
		skipToMatched (token, collector);
		return skipType (token, collector);
	}

	// PointerType = "*" BaseType .
	// BaseType = Type .
	// ChannelType = ( "chan" [ "<-" ] | "<-" "chan" ) ElementType .
	if (isType (token, TOKEN_STAR) || isKeyword (token, KEYWORD_chan) || isType (token, TOKEN_LEFT_ARROW))
	{
		readTokenFull (token, collector);
		return skipType (token, collector);
	}

	// MapType     = "map" "[" KeyType "]" ElementType .
	// KeyType     = Type .
	if (isKeyword (token, KEYWORD_map))
	{
		readTokenFull (token, collector);
		// skip over "[]"
		skipToMatched (token, collector);
		return skipType (token, collector);
	}

	// FunctionType   = "func" Signature .
	// Signature      = Parameters [ Result ] .
	// Result         = Parameters | Type .
	// Parameters     = "(" [ ParameterList [ "," ] ] ")" .
	if (isKeyword (token, KEYWORD_func))
	{
		readTokenFull (token, collector);
		// Parameters, skip over "()"
		skipToMatched (token, collector);
		// Result is parameters or type or nothing.  skipType treats anything
		// surrounded by parentheses as a type, and does nothing if what
		// follows is not a type.
		return skipType (token, collector);
	}

	return false;
}

static int makeTagFull (tokenInfo *const token, const goKind kind,
						const int scope, const char *argList, const char *typeref,
						const int role)
{
	const char *const name = vStringValue (token->string);

	tagEntryInfo e;

	/* Don't record `_' placeholder variable  */
	if (kind == GOTAG_VAR && name[0] == '_' && name[1] == '\0')
		return CORK_NIL;

	initRefTagEntry (&e, name, kind, role);

	e.lineNumber = token->lineNumber;
	e.filePosition = token->filePosition;
	if (argList)
		e.extensionFields.signature = argList;
	if (typeref)
	{
		/* Follows Cxx parser convention */
		e.extensionFields.typeRef [0] = "typename";
		e.extensionFields.typeRef [1] = typeref;
	}

	e.extensionFields.scopeIndex = scope;
	return makeTagEntry (&e);
}

static int makeTag (tokenInfo *const token, const goKind kind,
					const int scope, const char *argList, const char *typeref)
{
	return makeTagFull (token, kind, scope, argList, typeref,
						ROLE_DEFINITION_INDEX);
}

static int makeRefTag (tokenInfo *const token, const goKind kind,
					   const int role)
{
	return makeTagFull (token, kind, CORK_NIL, NULL, NULL, role);
}

static int parsePackage (tokenInfo *const token)
{
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		return makeTag (token, GOTAG_PACKAGE, CORK_NIL, NULL, NULL);
	}
	else
		return CORK_NIL;
}

static tokenInfo * parseReceiver (tokenInfo *const token, int *corkIndex)
{
	tokenInfo *receiver_type_token = NULL;
	int nest_level = 1;

	*corkIndex = CORK_NIL;

	/* Looking for an identifier before ')'. */
	while (nest_level > 0 && !isType (token, TOKEN_EOF))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
			if (*corkIndex == CORK_NIL)
				*corkIndex = makeTag (token, GOTAG_RECEIVER, CORK_NIL, NULL, NULL);
			if (!receiver_type_token)
				receiver_type_token = newToken ();
			copyToken (receiver_type_token, token);
		}

		readToken (token);
		if (isType (token, TOKEN_OPEN_PAREN))
			nest_level++;
		else if (isType (token, TOKEN_CLOSE_PAREN))
			nest_level--;
	}

	if (nest_level > 0 && receiver_type_token)
	{
		deleteToken (receiver_type_token);
		receiver_type_token = NULL;
	}

	if (receiver_type_token)
	{
		tagEntryInfo *e = getEntryInCorkQueue (*corkIndex);
		if (e)
		{
			e->extensionFields.typeRef [0] = eStrdup ("typename");
			e->extensionFields.typeRef [1] = vStringStrdup (receiver_type_token->string);
		}
	}
	readToken (token);
	return receiver_type_token;
}

static void parseFunctionOrMethod (tokenInfo *const token, const int scope)
{
	int receiver_cork = CORK_NIL;
	tokenInfo *receiver_type_token = NULL;

	// FunctionDecl = "func" identifier Signature [ Body ] .
	// Body         = Block.
	//
	// MethodDecl   = "func" Receiver MethodName Signature [ Body ] .
	// Receiver     = "(" [ identifier ] [ "*" ] BaseTypeName ")" .
	// BaseTypeName = identifier .

	// Pick up receiver type.
	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
		receiver_type_token = parseReceiver (token, &receiver_cork);

	if (isType (token, TOKEN_IDENTIFIER))
	{
		int cork;
		tagEntryInfo *e = NULL;
		tokenInfo *functionToken = newToken ();
		int func_scope;

		copyToken (functionToken, token);

		// Start recording signature
		vString *buffer = vStringNew ();
		collector collector = { .str = buffer, .last_len = 0, };

		// Skip over parameters.
		readTokenFull (token, &collector);
		skipToMatchedNoRead (token, &collector);

		collectorTruncate (&collector, false);
		if (receiver_type_token)
		{
			func_scope = anyEntryInScope (scope, vStringValue (receiver_type_token->string));
			if (func_scope == CORK_NIL)
				func_scope = makeTagFull(receiver_type_token, GOTAG_UNKNOWN,
										 scope, NULL, NULL,
										 R_GOTAG_UNKNOWN_RECEIVER);
		}
		else
			func_scope = scope;

		cork = makeTag (functionToken, GOTAG_FUNCTION,
						func_scope, vStringValue (buffer), NULL);
		if ((e = getEntryInCorkQueue (cork)))
		{
			tagEntryInfo *receiver = getEntryInCorkQueue (receiver_cork);
			if (receiver)
				receiver->extensionFields.scopeIndex = cork;
		}

		deleteToken (functionToken);

		vStringClear (collector.str);
		collector.last_len = 0;

		readTokenFull (token, &collector);

		// Skip over result.
		skipType (token, &collector);

		// Neither "{" nor " {".
		if (!(isType (token, TOKEN_OPEN_CURLY) && collector.last_len < 2))
		{
			collectorTruncate(&collector, isType (token, TOKEN_OPEN_CURLY));
			if (e)
			{
				e->extensionFields.typeRef [0] = eStrdup ("typename");
				e->extensionFields.typeRef [1] = vStringDeleteUnwrap (buffer);
				buffer = NULL;
			}
		}

		if (buffer)
			vStringDelete (buffer);

		// Skip over function body.
		if (isType (token, TOKEN_OPEN_CURLY))
		{
			skipToMatched (token, NULL);
			if (e)
				e->extensionFields.endLine = getInputLineNumber ();
		}
	}

	if (receiver_type_token)
		deleteToken(receiver_type_token);
}

static void attachTypeRefField (int scope, intArray *corks, const char *const type)
{
	int type_cork = anyEntryInScope (scope, type);
	tagEntryInfo *type_e = getEntryInCorkQueue (type_cork);

	for (unsigned int i = 0; i < intArrayCount (corks); i++)
	{
		int cork = intArrayItem (corks, i);
		tagEntryInfo *e = getEntryInCorkQueue (cork);
		if (!e)
			continue;
		e->extensionFields.typeRef [0] = eStrdup (type_e
												  ?GoKinds[type_e->kindIndex].name
												  :"typename");
		e->extensionFields.typeRef [1] = eStrdup (type);
	}
}

static void parseInterfaceMethods (tokenInfo *const token, const int scope)
{
	// InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
	// MethodSpec         = MethodName Signature | InterfaceTypeName .
	// MethodName         = identifier .
	// InterfaceTypeName  = TypeName .

	vString *inheritsBuf = vStringNew ();
	collector inherits = { .str = inheritsBuf, .last_len = 0, };

	readToken (token);
	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
			tokenInfo * headToken = newToken();
			copyToken (headToken, token);

			readToken (token);
			if(isType (token, TOKEN_DOT))
			{
				if (!collectorIsEmpty(&inherits))
					collectorPut (&inherits, ',');
				collectorAppendToken (&inherits, headToken);
				readTokenFull (token, NULL);
				if (isType (token, TOKEN_IDENTIFIER))
				{
					collectorPut (&inherits, '.');
					collectorAppendToken (&inherits, token);
					readToken (token);
				}
				/* If the token is not an identifier, the input
				   may be wrong. */
			}
			else if (isType (token, TOKEN_SEMICOLON))
			{
				if (!collectorIsEmpty(&inherits))
					collectorPut (&inherits, ',');
				collectorAppendToken (&inherits, headToken);
				readToken (token);
			}
			else if (isType (token, TOKEN_OPEN_PAREN))
			{
				// => Signature
				// Signature      = Parameters [ Result ] .
				vString *pbuf = vStringNew ();
				collector pcol = { .str = pbuf, .last_len = 0, };
				vString *rbuf = NULL;
				collector rcol = { .str = NULL, .last_len = 0, };

				// Parameters
				collectorPut (&pcol, '(');
				skipToMatched (token, &pcol);
				collectorTruncate(&pcol, true);

				if (!isType (token, TOKEN_SEMICOLON))
				{
					rbuf = vStringNew ();
					rcol.str = rbuf;

					collectorAppendToken (&rcol, token);
					skipType (token, &rcol);
					collectorTruncate(&rcol, true);
				}

				makeTag (headToken, GOTAG_METHODSPEC, scope,
						 vStringValue (pbuf),
						 rbuf? vStringValue(rbuf): NULL);

				if (rbuf)
					vStringDelete (rbuf);
				vStringDelete (pbuf);
			}
			deleteToken (headToken);
		}
		else
			readToken (token);
	}

	if (!collectorIsEmpty(&inherits))
	{
		tagEntryInfo *e = getEntryInCorkQueue (scope);
		if (e)
		{
			e->extensionFields.inheritance = vStringDeleteUnwrap (inheritsBuf);
			inheritsBuf = NULL;
		}
	}
	vStringDelete (inheritsBuf);
}

static void parseStructMembers (tokenInfo *const token, const int scope)
{
	// StructType     = "struct" "{" { FieldDecl ";" } "}" .
	// FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
	// AnonymousField = [ "*" ] TypeName .
	// Tag            = string_lit .

	readToken (token);
	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	vString *typeForAnonMember = vStringNew ();
	intArray *corkForFields = intArrayNew ();

	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		tokenInfo *memberCandidate = NULL;
		bool first = true;

		while (!isType (token, TOKEN_EOF))
		{
			if (isType (token, TOKEN_IDENTIFIER))
			{
				if (first)
				{
					// could be anonymous field like in 'struct {int}' - we don't know yet
					memberCandidate = newToken ();
					copyToken (memberCandidate, token);
					first = false;
				}
				else
				{
					int cork;
					if (memberCandidate)
					{
						// if we are here, there was a comma and memberCandidate isn't an anonymous field
						cork = makeTag (memberCandidate, GOTAG_MEMBER, scope, NULL, NULL);
						deleteToken (memberCandidate);
						memberCandidate = NULL;
						intArrayAdd (corkForFields, cork);
					}
					cork = makeTag (token, GOTAG_MEMBER, scope, NULL, NULL);
					intArrayAdd (corkForFields, cork);
				}
				readToken (token);
			}
			if (!isType (token, TOKEN_COMMA))
				break;
			readToken (token);
		}

		if (first && isType (token, TOKEN_STAR))
		{
			vStringPut (typeForAnonMember, '*');
			readToken (token);
		}
		else if (memberCandidate &&
				 (isType (token, TOKEN_DOT) ||
				  isType (token, TOKEN_STRING) ||
				  isType (token, TOKEN_SEMICOLON)))
			// memberCandidate is part of anonymous type
			vStringCat (typeForAnonMember, memberCandidate->string);

		// the above two cases that set typeForAnonMember guarantee
		// this is an anonymous member
		if (vStringLength (typeForAnonMember) > 0)
		{
			tokenInfo *anonMember = NULL;

			if (memberCandidate)
			{
				anonMember = newToken ();
				copyToken (anonMember, memberCandidate);
			}

			// TypeName of AnonymousField has a dot like package"."type.
			// Pick up the last package component, and store it to
			// memberCandidate.
			while (isType (token, TOKEN_IDENTIFIER) ||
				   isType (token, TOKEN_DOT))
			{
				if (isType (token, TOKEN_IDENTIFIER))
				{
					if (!anonMember)
						anonMember = newToken ();
					copyToken (anonMember, token);
					vStringCat (typeForAnonMember, anonMember->string);
				}
				else if (isType (token, TOKEN_DOT))
					vStringPut (typeForAnonMember, '.');
				readToken (token);
			}

			// optional tag
			if (isType (token, TOKEN_STRING))
				readToken (token);

			if (anonMember)
			{
				makeTag (anonMember, GOTAG_ANONMEMBER, scope, NULL,
						 vStringValue (typeForAnonMember));
				deleteToken (anonMember);
			}
		}
		else
		{
			vString *typeForMember = vStringNew ();
			collector collector = { .str = typeForMember, .last_len = 0, };

			collectorAppendToken (&collector, token);
			skipType (token, &collector);
			collectorTruncate (&collector, true);

			if (memberCandidate)
				makeTag (memberCandidate, GOTAG_MEMBER, scope, NULL,
						 vStringValue (typeForMember));

			attachTypeRefField (scope, corkForFields, vStringValue (typeForMember));
			intArrayClear (corkForFields);
			vStringDelete (typeForMember);
		}

		if (memberCandidate)
			deleteToken (memberCandidate);

		vStringClear (typeForAnonMember);

		while (!isType (token, TOKEN_SEMICOLON) && !isType (token, TOKEN_CLOSE_CURLY)
				&& !isType (token, TOKEN_EOF))
		{
			readToken (token);
			skipToMatched (token, NULL);
		}

		if (!isType (token, TOKEN_CLOSE_CURLY))
		{
			// we are at TOKEN_SEMICOLON
			readToken (token);
		}
	}

	intArrayDelete (corkForFields);
	vStringDelete (typeForAnonMember);
}

static void parseConstTypeVar (tokenInfo *const token, goKind kind, const int scope)
{
	// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
	// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
	// IdentifierList = identifier { "," identifier } .
	// ExpressionList = Expression { "," Expression } .
	// TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
	// TypeSpec     = identifier [ "=" ] Type .
	// VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
	// VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
	bool usesParens = false;
	intArray *corks
		= (kind == GOTAG_VAR || kind == GOTAG_CONST)? intArrayNew (): NULL;

	readToken (token);

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		usesParens = true;
		readToken (token);
	}

	do
	{
		tokenInfo *typeToken = NULL;
		int member_scope = scope;

		while (!isType (token, TOKEN_EOF))
		{
			if (isType (token, TOKEN_IDENTIFIER))
			{
				if (kind == GOTAG_TYPE)
				{
					typeToken = newToken ();
					copyToken (typeToken, token);
					readToken (token);
					if (isType (token, TOKEN_EQUAL))
					{
						kind = GOTAG_TALIAS;
						readToken (token);
					}

					if (isKeyword (token, KEYWORD_struct))
						member_scope = makeTag (typeToken, GOTAG_STRUCT,
												scope, NULL, NULL);
					else if (isKeyword (token, KEYWORD_interface))
						member_scope = makeTag (typeToken, GOTAG_INTERFACE,
												scope, NULL, NULL);
					else
						member_scope = makeTag (typeToken, kind,
												scope, NULL, NULL);

					if (member_scope != CORK_NIL)
						registerEntry (member_scope);
					break;
				}
				else
				{
					int c = makeTag (token, kind, scope, NULL, NULL);
					if (c != CORK_NIL && corks)
						intArrayAdd (corks, c);
				}
				readToken (token);
			}
			if (!isType (token, TOKEN_COMMA))
				break;
			readToken (token);
		}

		if (typeToken)
		{
			if (isKeyword (token, KEYWORD_struct))
				parseStructMembers (token, member_scope);
			else if (isKeyword (token, KEYWORD_interface))
				parseInterfaceMethods (token, member_scope);
			else
			{
				/* Filling "typeref:" field of typeToken. */
				vString *buffer = vStringNew ();
				collector collector = { .str = buffer, .last_len = 0, };

				collectorAppendToken (&collector, token);
				skipType (token, &collector);
				collectorTruncate (&collector, true);

				if ((member_scope != CORK_NIL) && !vStringIsEmpty (buffer))
				{
					tagEntryInfo *e = getEntryInCorkQueue (member_scope);
					if (e)
					{
						e->extensionFields.typeRef [0] = eStrdup ("typename");
						e->extensionFields.typeRef [1] = vStringDeleteUnwrap (buffer);
					}
				}
				else
					vStringDelete (buffer);
			}
			deleteToken (typeToken);
		}
		else if (corks)
		{
			vString *buffer = vStringNew ();
			collector collector = { .str = buffer, .last_len = 0, };

			collectorAppendToken (&collector, token);
			skipType (token, &collector);
			collectorTruncate (&collector, true);

			if (!vStringIsEmpty (buffer))
				attachTypeRefField (scope, corks, vStringValue (buffer));
			vStringDelete (buffer);
			intArrayClear (corks);
		}
		else
			skipType (token, NULL);

		while (!isType (token, TOKEN_SEMICOLON) && !isType (token, TOKEN_CLOSE_PAREN)
				&& !isType (token, TOKEN_EOF))
		{
			readToken (token);
			skipToMatched (token, NULL);
		}

		if (member_scope != scope && member_scope != CORK_NIL)
		{
			tagEntryInfo *e = getEntryInCorkQueue (member_scope);
			if (e)
				e->extensionFields.endLine = getInputLineNumber ();
		}

		if (usesParens && !isType (token, TOKEN_CLOSE_PAREN))
		{
			// we are at TOKEN_SEMICOLON
			readToken (token);
		}
	}
	while (!isType (token, TOKEN_EOF) &&
			usesParens && !isType (token, TOKEN_CLOSE_PAREN));

	intArrayDelete (corks);
}

static void parseImportSpec (tokenInfo *const token)
{
	// ImportSpec       = [ "." | PackageName ] ImportPath .
	// ImportPath       = string_lit .

	int packageName_cork = CORK_NIL;
	char *how_imported = NULL;
	if (isType (token, TOKEN_IDENTIFIER))
	{
		if (strcmp(vStringValue (token->string), "_") == 0)
			how_imported = "init";
		else
		{
			packageName_cork = makeTag (token, GOTAG_PACKAGE_NAME,
										CORK_NIL, NULL, NULL);
		}
		readToken (token);
	}
	else if (isType (token, TOKEN_DOT))
	{
		how_imported = "inline";
		readToken (token);
	}

	if (isType (token, TOKEN_STRING))
	{
		int package_cork =
			makeRefTag (token, GOTAG_PACKAGE, R_GOTAG_PACKAGE_IMPORTED);

		if (package_cork != CORK_NIL && how_imported)
			attachParserFieldToCorkEntry (package_cork,
										  GoFields [F_HOW_IMPORTED].ftype,
										  how_imported);

		if (packageName_cork != CORK_NIL)
		{
			attachParserFieldToCorkEntry (packageName_cork,
										  GoFields [F_PACKAGE].ftype,
										  vStringValue (token->string));
			if (package_cork != CORK_NIL)
			{
				tagEntryInfo *e = getEntryInCorkQueue (packageName_cork);
				if (e)
					attachParserFieldToCorkEntry (package_cork,
												  GoFields [F_PACKAGE_NAME].ftype,
												  e->name);
			}
		}
	}
}

static void parseImport (tokenInfo *const token)
{
	// ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .

	readToken (token);
	if (isType (token, TOKEN_EOF))
		return;

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		do
		{
			parseImportSpec (token);
			readToken (token);
		} while (!isType (token, TOKEN_EOF) &&
				 !isType (token, TOKEN_CLOSE_PAREN));
	}
	else
	{
		parseImportSpec (token);
		return;
	}
}

static void parseGoFile (tokenInfo *const token)
{
	int scope = CORK_NIL;
	do
	{
		readToken (token);

		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_package:
					scope = parsePackage (token);
					break;
				case KEYWORD_func:
					parseFunctionOrMethod (token, scope);
					break;
				case KEYWORD_const:
					parseConstTypeVar (token, GOTAG_CONST, scope);
					break;
				case KEYWORD_type:
					parseConstTypeVar (token, GOTAG_TYPE, scope);
					break;
				case KEYWORD_var:
					parseConstTypeVar (token, GOTAG_VAR, scope);
					break;
				case KEYWORD_import:
					parseImport (token);
					break;
				default:
					break;
			}
		}
		else if (isType (token, TOKEN_OPEN_PAREN) || isType (token, TOKEN_OPEN_CURLY) ||
			isType (token, TOKEN_OPEN_SQUARE))
		{
			skipToMatched (token, NULL);
		}
	} while (token->type != TOKEN_EOF);
}

static void findGoTags (void)
{
	tokenInfo *const token = newToken ();

	parseGoFile (token);

	deleteToken (token);
}

extern parserDefinition *GoParser (void)
{
	static const char *const extensions[] = { "go", NULL };
	parserDefinition *def = parserNew ("Go");
	def->kindTable = GoKinds;
	def->kindCount = ARRAY_SIZE (GoKinds);
	def->extensions = extensions;
	def->parser = findGoTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->keywordTable = GoKeywordTable;
	def->keywordCount = ARRAY_SIZE (GoKeywordTable);
	def->fieldTable = GoFields;
	def->fieldCount = ARRAY_SIZE (GoFields);
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	def->requestAutomaticFQTag = true;
	return def;
}
