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
#include "routines.h"
#include "vstring.h"
#include "options.h"
#include "xtag.h"

/*
 *	 MACROS
 */
#define MAX_SIGNATURE_LENGTH 512
#define isType(token,t) (boolean) ((token)->type == (t))
#define isKeyword(token,k) (boolean) ((token)->keyword == (k))

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
static vString *scope;
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
	GOTAG_MEMBER
} goKind;

static kindOption GoKinds[] = {
	{TRUE, 'p', "package", "packages"},
	{TRUE, 'f', "func", "functions"},
	{TRUE, 'c', "const", "constants"},
	{TRUE, 't', "type", "types"},
	{TRUE, 'v', "var", "variables"},
	{TRUE, 's', "struct", "structs"},
	{TRUE, 'i', "interface", "interfaces"},
	{TRUE, 'm', "member", "struct members"}
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

// XXX UTF-8
static boolean isStartIdentChar (const int c)
{
	return (boolean)
		(isalpha (c) ||  c == '_' || c > 128);
}

static boolean isIdentChar (const int c)
{
	return (boolean)
		(isStartIdentChar (c) || isdigit (c));
}

static void initialize (const langType language)
{
	Lang_go = language;
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	token->string = vStringNew ();
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	return token;
}

static tokenInfo *copyToken (tokenInfo *other)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->type = other->type;
	token->keyword = other->keyword;
	token->string = vStringNewCopy (other->string);
	token->lineNumber = other->lineNumber;
	token->filePosition = other->filePosition;
	return token;
}

static void deleteToken (tokenInfo * const token)
{
	if (token != NULL)
	{
		vStringDelete (token->string);
		eFree (token);
	}
}

/*
 *   Parsing functions
 */

static void parseString (vString *const string, const int delimiter)
{
	boolean end = FALSE;
	while (!end)
	{
		int c = getcFromInputFile ();
		if (c == EOF)
			end = TRUE;
		else if (c == '\\' && delimiter != '`')
		{
			c = getcFromInputFile ();
			if (c != '\'' && c != '\"')
				vStringPut (string, '\\');
			vStringPut (string, c);
		}
		else if (c == delimiter)
			end = TRUE;
		else
			vStringPut (string, c);
	}
	vStringTerminate (string);
}

static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (isIdentChar (c));
	vStringTerminate (string);
	ungetcToInputFile (c);		/* always unget, LF might add a semicolon */
}

static void readToken (tokenInfo *const token)
{
	int c;
	static tokenType lastTokenType = TOKEN_NONE;
	boolean firstWhitespace = TRUE;
	boolean whitespace;

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
			firstWhitespace = FALSE;
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
				boolean hasNewline = FALSE;
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
									hasNewline = TRUE;
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

static boolean skipToMatchedNoRead (tokenInfo *const token)
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
			return FALSE;
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

	return TRUE;
}

static void skipToMatched (tokenInfo *const token)
{
	if (skipToMatchedNoRead (token))
		readToken (token);
}

static boolean skipType (tokenInfo *const token)
{
	// Type      = TypeName | TypeLit | "(" Type ")" .
	// Skips also function multiple return values "(" Type {"," Type} ")"
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipToMatched (token);
		return TRUE;
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
		return TRUE;
	}

	// StructType     = "struct" "{" { FieldDecl ";" } "}"
	// InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
	if (isKeyword (token, KEYWORD_struct) || isKeyword (token, KEYWORD_interface))
	{
		readToken (token);
		// skip over "{}"
		skipToMatched (token);
		return TRUE;
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

	return FALSE;
}

static void makeTag (tokenInfo *const token, const goKind kind,
	tokenInfo *const parent_token, const goKind parent_kind,
	const char *argList)
{
	const char *const name = vStringValue (token->string);

	tagEntryInfo e;
	initTagEntry (&e, name, &(GoKinds [kind]));

	if (!GoKinds [kind].enabled)
		return;

	e.lineNumber = token->lineNumber;
	e.filePosition = token->filePosition;
	if (argList)
		e.extensionFields.signature = argList;

	if (parent_kind != GOTAG_UNDEFINED && parent_token != NULL)
	{
		e.extensionFields.scopeKind = &(GoKinds[parent_kind]);
		e.extensionFields.scopeName = vStringValue (parent_token->string);
	}
	makeTagEntry (&e);

	if (scope && isXtagEnabled(XTAG_QUALIFIED_TAGS))
	{
		vString *qualifiedName = vStringNew ();
		vStringCopy (qualifiedName, scope);
		vStringCatS (qualifiedName, ".");
		vStringCat (qualifiedName, token->string);
		e.name = vStringValue (qualifiedName);
		markTagExtraBit (&e, XTAG_QUALIFIED_TAGS);
		makeTagEntry (&e);
		vStringDelete (qualifiedName);
	}
}

static void parsePackage (tokenInfo *const token)
{
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		makeTag (token, GOTAG_PACKAGE, NULL, GOTAG_UNDEFINED, NULL);
		if (!scope && isXtagEnabled(XTAG_QUALIFIED_TAGS))
		{
			scope = vStringNew ();
			vStringCopy (scope, token->string);
		}
	}
}

static void parseFunctionOrMethod (tokenInfo *const token)
{
	// FunctionDecl = "func" identifier Signature [ Body ] .
	// Body         = Block.
	//
	// MethodDecl   = "func" Receiver MethodName Signature [ Body ] .
	// Receiver     = "(" [ identifier ] [ "*" ] BaseTypeName ")" .
	// BaseTypeName = identifier .

	// Skip over receiver.
	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
		skipToMatched (token);

	if (isType (token, TOKEN_IDENTIFIER))
	{
		tokenInfo *functionToken = copyToken (token);

		// Start recording signature
		signature = vStringNew ();

		// Skip over parameters.
		readToken (token);
		skipToMatchedNoRead (token);

		vStringStripLeading (signature);
		vStringStripTrailing (signature);
		makeTag (functionToken, GOTAG_FUNCTION, NULL, GOTAG_UNDEFINED, signature->buffer);
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
}

static void parseStructMembers (tokenInfo *const token, tokenInfo *const parent_token)
{
	// StructType     = "struct" "{" { FieldDecl ";" } "}" .
	// FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
	// AnonymousField = [ "*" ] TypeName .
	// Tag            = string_lit .

	readToken (token);
	if (!isType (token, TOKEN_OPEN_CURLY))
		return;

	readToken (token);
	while (!isType (token, TOKEN_EOF) && !isType (token, TOKEN_CLOSE_CURLY))
	{
		tokenInfo *memberCandidate = NULL;
		boolean first = TRUE;

		while (!isType (token, TOKEN_EOF))
		{
			if (isType (token, TOKEN_IDENTIFIER))
			{
				if (first)
				{
					// could be anonymous field like in 'struct {int}' - we don't know yet
					memberCandidate = copyToken (token);
					first = FALSE;
				}
				else
				{
					if (memberCandidate)
					{
						// if we are here, there was a comma and memberCandidate isn't an anonymous field
						makeTag (memberCandidate, GOTAG_MEMBER, parent_token, GOTAG_STRUCT, NULL);
						deleteToken (memberCandidate);
						memberCandidate = NULL;
					}
					makeTag (token, GOTAG_MEMBER, parent_token, GOTAG_STRUCT, NULL);
				}
				readToken (token);
			}
			if (!isType (token, TOKEN_COMMA))
				break;
			readToken (token);
		}

		// in the case of  an anonymous field, we already read part of the
		// type into memberCandidate and skipType() should return FALSE so no tag should
		// be generated in this case.
		if (skipType (token) && memberCandidate)
			makeTag (memberCandidate, GOTAG_MEMBER, parent_token, GOTAG_STRUCT, NULL);

		if (memberCandidate)
			deleteToken (memberCandidate);

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
}

static void parseConstTypeVar (tokenInfo *const token, goKind kind)
{
	// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
	// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
	// IdentifierList = identifier { "," identifier } .
	// ExpressionList = Expression { "," Expression } .
	// TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
	// TypeSpec     = identifier Type .
	// VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
	// VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
	boolean usesParens = FALSE;

	readToken (token);

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		usesParens = TRUE;
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
					typeToken = copyToken (token);
					readToken (token);
					if (isKeyword (token, KEYWORD_struct))
						makeTag (typeToken, GOTAG_STRUCT, NULL, GOTAG_UNDEFINED, NULL);
					else if (isKeyword (token, KEYWORD_interface))
						makeTag (typeToken, GOTAG_INTERFACE, NULL, GOTAG_UNDEFINED, NULL);
					else
						makeTag (typeToken, kind, NULL, GOTAG_UNDEFINED, NULL);
					break;
				}
				else
					makeTag (token, kind, NULL, GOTAG_UNDEFINED, NULL);
				readToken (token);
			}
			if (!isType (token, TOKEN_COMMA))
				break;
			readToken (token);
		}

		if (typeToken)
		{
			if (isKeyword (token, KEYWORD_struct))
				parseStructMembers (token, typeToken);
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
	do
	{
		readToken (token);

		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_package:
					parsePackage (token);
					break;
				case KEYWORD_func:
					parseFunctionOrMethod (token);
					break;
				case KEYWORD_const:
					parseConstTypeVar (token, GOTAG_CONST);
					break;
				case KEYWORD_type:
					parseConstTypeVar (token, GOTAG_TYPE);
					break;
				case KEYWORD_var:
					parseConstTypeVar (token, GOTAG_VAR);
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
	vStringDelete (scope);
	scope = NULL;
}

extern parserDefinition *GoParser (void)
{
	static const char *const extensions[] = { "go", NULL };
	parserDefinition *def = parserNew ("Go");
	def->kinds = GoKinds;
	def->kindCount = ARRAY_SIZE (GoKinds);
	def->extensions = extensions;
	def->parser = findGoTags;
	def->initialize = initialize;
	def->keywordTable = GoKeywordTable;
	def->keywordCount = ARRAY_SIZE (GoKeywordTable);
	return def;
}
