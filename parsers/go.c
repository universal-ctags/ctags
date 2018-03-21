/*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   INCLUDE FILES
*/
#include "general.h"        /* must always come first */

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "read.h"
#include "main.h"
#include "objpool.h"
#include "routines.h"
#include "vstring.h"
#include "options.h"
#include "xtag.h"

/*
 *	 MACROS
 */
#define MAX_SIGNATURE_LENGTH 512
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
	TOKEN_EOF
} tokenType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	vString *string;		/* the name of the token */
	unsigned long lineNumber;	/* line number of tag */
	MIOPos filePosition;		/* file position of line containing name */
} tokenInfo;

/*
*   DATA DEFINITIONS
*/

static int Lang_go;
static objPool *TokenPool = NULL;
static vString *signature = NULL;

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
	GOTAG_UNKNOWN,
} goKind;

typedef enum {
	R_GOTAG_UNKNOWN_RECEIVER,
} GoUnknownRole;

static roleDefinition GoUnknownRoles [] = {
	{ true, "receiverType", "receiver type" },
};

static kindDefinition GoKinds[] = {
	{true, 'p', "package", "packages"},
	{true, 'f', "func", "functions"},
	{true, 'c', "const", "constants"},
	{true, 't', "type", "types"},
	{true, 'v', "var", "variables"},
	{true, 's', "struct", "structs"},
	{true, 'i', "interface", "interfaces"},
	{true, 'm', "member", "struct members"},
	{true, 'M', "anonMember", "struct anonymous members"},
	{true, 'u', "unknown", "unknown",
	 .referenceOnly = true, ATTACH_ROLES (GoUnknownRoles)},
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

static void readToken (tokenInfo *const token)
{
	int c;
	static tokenType lastTokenType = TOKEN_NONE;
	bool firstWhitespace = true;
	bool whitespace;

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
		if (signature && whitespace && firstWhitespace && vStringLength (signature) < MAX_SIGNATURE_LENGTH)
		{
			firstWhitespace = false;
			vStringPut(signature, ' ');
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
			token->type = TOKEN_DOT;
			break;

		case ',':
			token->type = TOKEN_COMMA;
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

	if (signature && vStringLength (signature) < MAX_SIGNATURE_LENGTH)
	{
		if (token->type == TOKEN_LEFT_ARROW)
			vStringCatS(signature, "<-");
		else if (token->type == TOKEN_STRING)
		{
			// only struct member annotations can appear in function prototypes
			// so only `` type strings are possible
			vStringPut(signature, '`');
			vStringCat(signature, token->string);
			vStringPut(signature, '`');
		}
		else if (token->type == TOKEN_IDENTIFIER || token->type == TOKEN_KEYWORD)
			vStringCat(signature, token->string);
		else if (c != EOF)
			vStringPut(signature, c);
	}

	lastTokenType = token->type;
}

static bool skipToMatchedNoRead (tokenInfo *const token)
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
		readToken (token);
		if (isType (token, open_token))
			nest_level++;
		else if (isType (token, close_token))
			nest_level--;
	}

	return true;
}

static void skipToMatched (tokenInfo *const token)
{
	if (skipToMatchedNoRead (token))
		readToken (token);
}

static bool skipType (tokenInfo *const token)
{
	// Type      = TypeName | TypeLit | "(" Type ")" .
	// Skips also function multiple return values "(" Type {"," Type} ")"
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipToMatched (token);
		return true;
	}

	// TypeName  = QualifiedIdent.
	// QualifiedIdent = [ PackageName "." ] identifier .
	// PackageName    = identifier .
	if (isType (token, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isType (token, TOKEN_DOT))
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}
		return true;
	}

	// StructType     = "struct" "{" { FieldDecl ";" } "}"
	// InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
	if (isKeyword (token, KEYWORD_struct) || isKeyword (token, KEYWORD_interface))
	{
		readToken (token);
		// skip over "{}"
		skipToMatched (token);
		return true;
	}

	// ArrayType   = "[" ArrayLength "]" ElementType .
	// SliceType = "[" "]" ElementType .
	// ElementType = Type .
	if (isType (token, TOKEN_OPEN_SQUARE))
	{
		skipToMatched (token);
		return skipType (token);
	}

	// PointerType = "*" BaseType .
	// BaseType = Type .
	// ChannelType = ( "chan" [ "<-" ] | "<-" "chan" ) ElementType .
	if (isType (token, TOKEN_STAR) || isKeyword (token, KEYWORD_chan) || isType (token, TOKEN_LEFT_ARROW))
	{
		readToken (token);
		return skipType (token);
	}

	// MapType     = "map" "[" KeyType "]" ElementType .
	// KeyType     = Type .
	if (isKeyword (token, KEYWORD_map))
	{
		readToken (token);
		// skip over "[]"
		skipToMatched (token);
		return skipType (token);
	}

	// FunctionType   = "func" Signature .
	// Signature      = Parameters [ Result ] .
	// Result         = Parameters | Type .
	// Parameters     = "(" [ ParameterList [ "," ] ] ")" .
	if (isKeyword (token, KEYWORD_func))
	{
		readToken (token);
		// Parameters, skip over "()"
		skipToMatched (token);
		// Result is parameters or type or nothing.  skipType treats anything
		// surrounded by parentheses as a type, and does nothing if what
		// follows is not a type.
		return skipType (token);
	}

	return false;
}

static int makeTag (tokenInfo *const token, const goKind kind,
	const int scope, const char *argList, const char *typeref)
{
	const char *const name = vStringValue (token->string);

	tagEntryInfo e;

	if (kind == GOTAG_UNKNOWN)
		initRefTagEntry (&e, name, kind,
						 R_GOTAG_UNKNOWN_RECEIVER);
	else
		initTagEntry (&e, name, kind);

	if (!GoKinds [kind].enabled)
		return CORK_NIL;

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

static tokenInfo * parseReceiver (tokenInfo *const token)
{
	tokenInfo *receiver_type_token = NULL;
	int nest_level = 1;

	/* Looking for an identifier before ')'. */
	while (nest_level > 0 && !isType (token, TOKEN_EOF))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
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

	readToken (token);
	return receiver_type_token;
}

static void parseFunctionOrMethod (tokenInfo *const token, const int scope)
{
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
		receiver_type_token = parseReceiver (token);

	if (isType (token, TOKEN_IDENTIFIER))
	{
		tokenInfo *functionToken = newToken ();
		int func_scope;

		copyToken (functionToken, token);

		// Start recording signature
		signature = vStringNew ();

		// Skip over parameters.
		readToken (token);
		skipToMatchedNoRead (token);

		vStringStripLeading (signature);
		vStringStripTrailing (signature);
		if (receiver_type_token)
			func_scope = makeTag(receiver_type_token, GOTAG_UNKNOWN,
								 scope, NULL, NULL);
		else
			func_scope = scope;
		makeTag (functionToken, GOTAG_FUNCTION,
				 func_scope, signature->buffer, NULL);
		deleteToken (functionToken);
		vStringDelete(signature);

		// Stop recording signature
		signature = NULL;

		readToken (token);

		// Skip over result.
		skipType (token);

		// Skip over function body.
		if (isType (token, TOKEN_OPEN_CURLY))
			skipToMatched (token);
	}

	if (receiver_type_token)
		deleteToken(receiver_type_token);
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
					if (memberCandidate)
					{
						// if we are here, there was a comma and memberCandidate isn't an anonymous field
						makeTag (memberCandidate, GOTAG_MEMBER, scope, NULL, NULL);
						deleteToken (memberCandidate);
						memberCandidate = NULL;
					}
					makeTag (token, GOTAG_MEMBER, scope, NULL, NULL);
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
		else if (memberCandidate)
		{
			// memberCandidate is a non-anonymous member
			makeTag (memberCandidate, GOTAG_MEMBER, scope, NULL, NULL);
			skipType (token);
		}

		if (memberCandidate)
			deleteToken (memberCandidate);

		vStringClear (typeForAnonMember);

		while (!isType (token, TOKEN_SEMICOLON) && !isType (token, TOKEN_CLOSE_CURLY)
				&& !isType (token, TOKEN_EOF))
		{
			readToken (token);
			skipToMatched (token);
		}

		if (!isType (token, TOKEN_CLOSE_CURLY))
		{
			// we are at TOKEN_SEMICOLON
			readToken (token);
		}
	}

	vStringDelete (typeForAnonMember);
}

static void parseConstTypeVar (tokenInfo *const token, goKind kind, const int scope)
{
	// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
	// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
	// IdentifierList = identifier { "," identifier } .
	// ExpressionList = Expression { "," Expression } .
	// TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
	// TypeSpec     = identifier Type .
	// VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
	// VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
	bool usesParens = false;
	int member_scope = scope;

	readToken (token);

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		usesParens = true;
		readToken (token);
	}

	do
	{
		tokenInfo *typeToken = NULL;

		while (!isType (token, TOKEN_EOF))
		{
			if (isType (token, TOKEN_IDENTIFIER))
			{
				if (kind == GOTAG_TYPE)
				{
					typeToken = newToken ();
					copyToken (typeToken, token);
					readToken (token);
					if (isKeyword (token, KEYWORD_struct))
						member_scope = makeTag (typeToken, GOTAG_STRUCT,
												scope, NULL, NULL);
					else if (isKeyword (token, KEYWORD_interface))
						member_scope = makeTag (typeToken, GOTAG_INTERFACE,
												scope, NULL, NULL);
					else
						member_scope = makeTag (typeToken, kind,
												scope, NULL, NULL);
					break;
				}
				else
					makeTag (token, kind, scope, NULL, NULL);
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
			else
				skipType (token);
			deleteToken (typeToken);
		}
		else
			skipType (token);

		while (!isType (token, TOKEN_SEMICOLON) && !isType (token, TOKEN_CLOSE_PAREN)
				&& !isType (token, TOKEN_EOF))
		{
			readToken (token);
			skipToMatched (token);
		}

		if (usesParens && !isType (token, TOKEN_CLOSE_PAREN))
		{
			// we are at TOKEN_SEMICOLON
			readToken (token);
		}
	}
	while (!isType (token, TOKEN_EOF) &&
			usesParens && !isType (token, TOKEN_CLOSE_PAREN));
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
				default:
					break;
			}
		}
		else if (isType (token, TOKEN_OPEN_PAREN) || isType (token, TOKEN_OPEN_CURLY) ||
			isType (token, TOKEN_OPEN_SQUARE))
		{
			skipToMatched (token);
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
	def->useCork = true;
	def->requestAutomaticFQTag = true;
	return def;
}
