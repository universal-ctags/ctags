/*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   References for developers:
*     https://vlang.io/
*     https://github.com/vlang/vls/blob/master/tree_sitter_v/grammar.js
*/


#include "general.h"        /* must always come first */

#include <string.h>
#include <stdarg.h>

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

#define DEBUG_V_TOKENISER
#define DEBUG_V_PARSER

#define _NARGS(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
#define nArgs(...) _NARGS (__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))
#define vStringAccumulate(c, a) \
	do{ if (c && a) { vStringCat (c, a); vStringClear (a); } }while(0)
#define isToken(token, ...) \
	_isToken (token,  false, false, nArgs (__VA_ARGS__), __VA_ARGS__)
#define expectToken(token, ...) \
	_isToken (token,  true, false, nArgs (__VA_ARGS__), __VA_ARGS__)
#define isKeyword(token, ...) \
	_isToken (token, false, true, nArgs (__VA_ARGS__), __VA_ARGS__)
#define expectKeyword(token, ...) \
	_isToken (token, true, true, nArgs (__VA_ARGS__), __VA_ARGS__)
#define isWhitespace(c) (c == ' ' || c == '\t' || c == '\r' || c == '\n')
#define isDigit(c) (c >= '0' && c <= '9')
#define isHexDigit(c) (isDigit (c) || \
                       (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
#define isIdentInitial(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
#define isIdentSubsequent(c) (isIdentInitial (c) || isDigit (c) || c == '_')
#define unreadToken() (unreadTokenFull (NULL))

#ifdef DEBUG_V_PARSER
#define debugParserPrintf(...) debugPrintf(DEBUG_PARSE, __VA_ARGS__)
#else
#define debugParserPrintf(...) do{}while(0)
#endif
#define PARSER_PROLOGUE(c) \
	const char *const _prevParser = CurrentParser; \
	CurrentParser = c; \
    debugParserPrintf ("{%s:", CurrentParser)
#define PARSER_EPILOGUE() \
    debugParserPrintf (":%s}", CurrentParser); \
	CurrentParser = _prevParser

enum {
	KEYWORD_fn,
	KEYWORD_map,
	KEYWORD_or,
	KEYWORD_module,
	KEYWORD_as,
	KEYWORD_import,
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_const,
	KEYWORD_return,
	KEYWORD_mut,
	KEYWORD_pub,
	KEYWORD_defer,
	COUNT_KEYWORD,
	KEYWORD_TYPE,
	KEYWORD_REST,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

const static struct keywordGroup VTypeKeywords = {
	.value = KEYWORD_TYPE,
	.addingUnlessExisting = true,
	.keywords = {
		"voidptr", "byteptr", "charptr", "i8", "i16", "i32", "int", "i64",
		"byte", "u8", "u16", "u32", "u64", "f32", "f64", "char", "bool",
		"string", "rune", "array", "mapnode", "chan", "size_t", "usize",
		"isize", "float_literal", "int_literal", "thread", "IError", NULL
	},
};

const static struct keywordGroup VRestKeywords = {
	.value = KEYWORD_REST,
	.addingUnlessExisting = true,
	.keywords = {
		"shared", "static", "__global", "assert", "as",
		"go", "spawn", "asm", "type", "for", "in", "is",
		"union", "struct", "enum", "interface", "defer", "unsafe", "match",
		"lock", "rlock", "select",
		NULL
	},
};

static const keywordTable VKeywordTable[COUNT_KEYWORD] = {
	{"fn", KEYWORD_fn},
	{"map", KEYWORD_map},
	{"or", KEYWORD_or},
	{"module", KEYWORD_module},
	{"as", KEYWORD_as},
	{"import", KEYWORD_import},
	{"if", KEYWORD_if},
	{"else", KEYWORD_else},
	{"const", KEYWORD_const},
	{"return", KEYWORD_return},
	{"mut", KEYWORD_mut},
	{"pub", KEYWORD_pub},
	{"defer", KEYWORD_defer},
};

typedef enum eTokenType {
	TOKEN_NONE,
	TOKEN_KEYWORD,
	TOKEN_IDENT,
	TOKEN_TYPE, // type ident (e.g., "Foo"), not KEYWORD_TYPE (e.g. "int")
	TOKEN_IMMEDIATE,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_DECLARE,
	TOKEN_OPERATOR,
	TOKEN_COLON,
	TOKEN_EXCLAMATION,
	TOKEN_QUESTION,
	TOKEN_DOT,
	TOKEN_COMMA,
	TOKEN_SLICE,
	TOKEN_ASSIGN,
	TOKEN_ASTERISK,
	TOKEN_AMPERSAND,
	TOKEN_TILDE,
	TOKEN_EOF,
	COUNT_TOKEN
} tokenType;

static char *const tokenNames[COUNT_TOKEN] = {
	"NONE",
	"KEYWORD",
	"IDENT",
	"TYPE",
	"IMMEDIATE",
	"OPEN_PAREN",
	"CLOSE_PAREN",
	"OPEN_CURLY",
	"CLOSE_CURLY",
	"OPEN_SQUARE",
	"CLOSE_SQUARE",
	"DECLARE",
	"OPERATOR",
	"COLON",
	"EXCLAMATION",
	"QUESTION",
	"DOT",
	"COMMA",
	"SLICE",
	"ASSIGN",
	"ASTERISK",
	"AMPERSAND",
	"TILDE",
	"EOF"
};

typedef enum {
	ROLE_IMPORTED_MODULE,
	COUNT_MODULE_ROLE
} VModuleRole;

static roleDefinition VModuleRoles [COUNT_MODULE_ROLE] = {
	{ true, "imported", "imported module" },
};

typedef enum {
	ROLE_IMPORTED_SYMBOL,
	COUNT_UNKNOWN_ROLE
} VUnknownRole;

static roleDefinition VUnknownRoles [COUNT_UNKNOWN_ROLE] = {
	{ true, "imported", "imported symbol" },
};

typedef enum {
	KIND_NONE = -1,
	KIND_FUNCTION,
	KIND_MODULE,
	KIND_VARIABLE,
	KIND_CONST,
	//KIND_FPARAM,
	KIND_RPARAM,
	//KIND_CPARAM,
	/* KIND_STRUCT, */
	/* KIND_TYPE, */
	/* KIND_INTERFACE, */
	/* KING_ALIAS, */
	KIND_UNKNOWN,
	COUNT_KIND
} kindType;

static kindDefinition VKinds[COUNT_KIND] = {
	{true, 'f', "fn", "functions"},
	{true, 'm', "module", "modules",
	 .referenceOnly = false, ATTACH_ROLES (VModuleRoles)},
	{true, 'v', "variable", "variables"},
	{true, 'c', "const", "constants"},
	//{false, 'z', "param", "function parameters in functions"},
	{false, 'r', "receiver", "receivers in functions"},
	//{false, 'c', "closure", "closure parameters in functions"},
	/* {true, 's', "struct", "structs"}, */
	/* {true, 't', "type", "types"}, */
	/* {true, 'i', "interface", "interfaces"}, */
	/* {true, 'a', "alias", "type aliases"}, */
	{true, 'Y', "unknown", "unknown, imported symbols",
	 .referenceOnly = false, ATTACH_ROLES (VUnknownRoles)},
};

static fieldDefinition VFields [] = {
	{
		.name = "module",
		.description = "the name of the module",
		.enabled = true,
	},
};

typedef struct {
	int type;
	keywordId keyword;
	vString *string;
	unsigned long lineNumber;	/* line number of tagName */
	MIOPos filePosition;		/* file position of line containing name */
} tokenInfo;

static langType Lang_v;
static bool ReplayToken = false;
static vString *ReplayCapture = NULL;
//static tokenInfo *NextToken = NULL;
/* static NestingLevels *VNestingLevels = NULL; */
static objPool *TokenPool = NULL;
//static int currentScope = CORK_NIL;
static const char *CurrentParser;
/* static int AnonymousId = 0; */
static tokenType LastTokenType = TOKEN_NONE;
static size_t LastCaptureLen = 0;

static void parseBlock (tokenInfo *const token, int scope);
static void parseExpression (tokenInfo *const token, int scope);

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

	token->type			= TOKEN_NONE;
	token->keyword		= KEYWORD_NONE;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
}

static void copyPoolToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCat (dest->string, src->string);
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
}

static tokenInfo *dupToken (tokenInfo *const token)
{
	tokenInfo *newToken = newToken ();
	copyPoolToken (newToken, token);
	return newToken;
}

static bool getcFromInputFileAndKeepIfEq (int expect, vString *const capture)
{
	int c = getcFromInputFile ();
	if (c != expect)
		ungetcToInputFile (c);
	else if (capture)
		vStringPut (capture, c);
	return c == expect;
}

static int peekcFromInputFile ()
{
	int c = getcFromInputFile ();
	ungetcToInputFile (c);
	return c;
}

static bool isOneOf (int c, const char *set)
{
	for (; *set != '\0'; set++)
		if (*set == c)
			return true;
	return false;
}

static void skipInputFileTillEOL ()
{
	int c;
	do
		c = getcFromInputFile ();
	while (c != EOF && !isOneOf (c, "\r\n"));
}

static void skipInputFileTillCommentEnd ()
{
	int c = 0, last;
	do
	{
		last = c;
		c = getcFromInputFile ();
		if (last == '/' && c == '*')
			skipInputFileTillCommentEnd ();
		else if (last == '*' && c == '/')
			break;
	} while (c != EOF);
}

static void readTokenFull (tokenInfo *const token, vString *capture)
{
	int c;

	if (ReplayToken)
	{
#ifdef DEBUG_V_TOKENISER
		debugPrintf (DEBUG_PARSE, "*%s[]", tokenNames[token->type]);
#endif
		if (capture && ReplayCapture)
			vStringCat (capture, ReplayCapture);
		vStringDelete (ReplayCapture);

		ReplayToken = false;
		return;
	}

	LastCaptureLen = capture? vStringLength (capture) : 0;

	do
	{
		do
			c = getcFromInputFile ();
		while (isWhitespace (c));

		if (c == '/')
		{
			int d = getcFromInputFile ();
			if (d == '/')
				skipInputFileTillEOL ();
			else if (d == '*')
				skipInputFileTillCommentEnd ();
			else
			{
				ungetcToInputFile (d);
				break;
			}
		}
		else
			break;
	} while (true);

	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);

	if (capture && c != EOF &&
	    (isDigit (c) || isIdentInitial (c) || isOneOf (c, "-+.'\\*\"`")) &&
	    (LastTokenType == TOKEN_IDENT || LastTokenType == TOKEN_KEYWORD ||
	     LastTokenType == TOKEN_TYPE || LastTokenType == TOKEN_COMMA))
		vStringPut (capture, ' ');

#ifdef DEBUG_V_TOKENISER
	vString *const oldCapture = capture;
	capture = vStringNew ();
#endif

	if (capture && c != EOF)
		vStringPut (capture, c);

	if (c == EOF)
		token->type = TOKEN_EOF;
	else if (c == '(')
		token->type = TOKEN_OPEN_PAREN;
	else if (c == ')')
		token->type = TOKEN_CLOSE_PAREN;
	else if (c == '{')
		token->type = TOKEN_OPEN_CURLY;
	else if (c == '}')
		token->type = TOKEN_CLOSE_CURLY;
	else if (c == '[')
		token->type = TOKEN_OPEN_SQUARE;
	else if (c == ']')
		token->type = TOKEN_CLOSE_SQUARE;
	else if (c == '?')
		token->type = TOKEN_QUESTION;
	else if (c == ',')
		token->type = TOKEN_COMMA;
	else if (c == '~')
		token->type = TOKEN_TILDE;
	else if (isDigit (c) ||
	         (isOneOf (c, "-+.") && isDigit (peekcFromInputFile ())))
	{
		bool more;
		token->type = TOKEN_IMMEDIATE;
		do
		{
			vStringPut (token->string, c);
			c = getcFromInputFile ();
			more = isHexDigit (c) ||
				(isOneOf (c, ".-+xo_") && isHexDigit (peekcFromInputFile ())) ||
				(isOneOf (c, "eE") && (isDigit (peekcFromInputFile ()) ||
				                       isOneOf (peekcFromInputFile (), "-+")));
			if (capture && more)
				vStringPut (capture, c);
		} while (more);
		ungetcToInputFile (c);
	}
	else if (isOneOf (c, "+-*/%&|^"))
	{
		if (getcFromInputFileAndKeepIfEq ('=', capture))
			token->type = TOKEN_ASSIGN; // += -= *= /= %= &= |= ^=
		else if (c == '*')
			token->type = TOKEN_ASTERISK; // *
		else if (isOneOf (c, "&|") && getcFromInputFileAndKeepIfEq (c, capture))
			token->type = TOKEN_OPERATOR; // && ||
		else if (c == '&')
			token->type = TOKEN_AMPERSAND; // &
		else
			token->type = TOKEN_OPERATOR; // + - / % | ^
	}
	else if (isOneOf (c, "=!"))
	{
		if (getcFromInputFileAndKeepIfEq ('=', capture))
			token->type = TOKEN_OPERATOR; // == !=
		else if (c == '=')
			token->type = TOKEN_ASSIGN; // =
		else
			token->type = TOKEN_EXCLAMATION; // !
	}
	else if (isOneOf (c, "<>"))
	{
		if (getcFromInputFileAndKeepIfEq (c, capture))
		{
			if (c == '>')
				getcFromInputFileAndKeepIfEq ('>', capture);
			if (getcFromInputFileAndKeepIfEq ('=', capture))
				token->type = TOKEN_ASSIGN; // <<= >>= >>>=
			else
				token->type = TOKEN_OPERATOR; // << >> >>>
		}
		else
		{
			getcFromInputFileAndKeepIfEq ('=', capture);
			token->type = TOKEN_OPERATOR; // < > <= >=
		}
	}
	else if (c == '.')
	{
		if (getcFromInputFileAndKeepIfEq ('.', capture))
		{
			getcFromInputFileAndKeepIfEq ('.', capture);
			token->type = TOKEN_SLICE; // .. ...
		}
		else
			token->type = TOKEN_DOT; // .
	}
	else if (c == ':')
	{
		if (getcFromInputFileAndKeepIfEq ('=', capture))
			token->type = TOKEN_DECLARE; // :=
		else
			token->type = TOKEN_COLON; // :
	}
	else if (isOneOf (c, "\"'`"))
	{
		int terminator = c;
		bool inEscape = false;
		token->type = TOKEN_IMMEDIATE;
		do
		{
			if (!inEscape && c == '\\')
				inEscape = true;
			else
			{
				inEscape = false;
				vStringPut (token->string, c);
			}
			c = getcFromInputFile ();
			if (capture) vStringPut (capture, c);
		} while (c != EOF && (c != terminator || inEscape));
	}
	else if (isIdentInitial (c))
	{
		bool more;
		do
		{
			vStringPut (token->string, c);
			c = getcFromInputFile ();
			more = isIdentSubsequent (c);
			if (capture && more)
				vStringPut (capture, c);
		} while (more);
		ungetcToInputFile (c);

		token->keyword = lookupKeyword (vStringValue (token->string), Lang_v);
		if (token->keyword != KEYWORD_NONE)
			token->type = TOKEN_KEYWORD;
		else
		{
			char initChar = vStringChar (token->string, 0);
			if (initChar >= 'A' && initChar <= 'Z')
				token->type = TOKEN_TYPE;
			else
				token->type = TOKEN_IDENT;
		}
	}
	else
	{
		DebugStatement (
			debugPrintf (DEBUG_PARSE,
			             "\nUNRECOGNISED CHAR AT LINE %lu: %c (%u)\n",
			             token->lineNumber, c, c);
		);
	}

	LastTokenType = token->type;

#ifdef DEBUG_V_TOKENISER
	debugPrintf(DEBUG_PARSE, "%s[%s]", tokenNames[token->type], vStringValue (capture));
	if (oldCapture)
		vStringCat (oldCapture, capture);
	vStringDelete (capture);
#endif
}

static void unreadTokenFull (vString *const acc)
{
	debugParserPrintf ("#");
	ReplayToken = true;
	if (acc)
	{
		Assert (LastCaptureLen <= vStringLength (acc));
		ReplayCapture = vStringNewInit (vStringValue (acc) + LastCaptureLen);
	}
	ReplayCapture = acc? vStringNewCopy (acc) : ReplayCapture;
}

/* static void uncaptureToken (vString *const capture) */
/* { */
/* 	if (capture && LastCaptureLen < vStringLength (capture)) */
/* 		vStringTruncate (capture, LastCaptureLen); */
/* } */

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, NULL);
}

static bool _isToken (tokenInfo *const token, bool expected, bool keyword,
                      int nTypes, ...)
{
	va_list argp;
	va_start (argp, nTypes);
	bool found = false;
	if (!keyword)
		for (int i = 0; i < nTypes; i++)
			found = found || (token->type == va_arg (argp, tokenType));
	else if (token->type == TOKEN_KEYWORD)
		for (int i = 0; i < nTypes; i++)
			found = found || (token->keyword == va_arg (argp, keywordId));
	va_end (argp);
	vString *got, *exp;
	DebugStatement (
		if (expected && !found)
		{
			va_start (argp, nTypes);
			if (!keyword)
			{
				got = vStringNewInit ("TOKEN");
				exp = vStringNewInit (" (expected: ");
				for (int i = 0; i < nTypes; i++)
				{
					if (i > 0) vStringCatS (exp, ", ");
					vStringCatS (exp, tokenNames[va_arg (argp, tokenType)]);
				}
				vStringCatS (exp, ")");

			}
			else if (token->type == TOKEN_KEYWORD)
			{
				got = vStringNewInit ("KEYWORD");
				exp = vStringNewInit ("");
			}
			else
			{
				got = vStringNewInit ("TOKEN");
				exp = vStringNewInit (" (expected KEYWORD)");
			}
			if (token->type == TOKEN_KEYWORD)
				for (int i = 0; i < nTypes; i++)
					found = found || (token->keyword == va_arg (argp, keywordId));
			debugPrintf (
				DEBUG_PARSE, "\nUNEXPECTED %s IN {%s} AT LINE %lu: %s%s\n",
				vStringValue (got), CurrentParser, token->lineNumber,
				tokenNames[token->type], vStringValue (exp));
			vStringDelete (got);
			vStringDelete (exp);
			va_end (argp);
		}
	);
	// Although we don't show a debug parser error, we always return failure for
	// expectToken() on a keyword token, so that the call can be chained to an
	// expectKeyword() call to check it (note that isToken() is not affected).
	return (expected && !keyword && token->type == TOKEN_KEYWORD)? false : found;
}

static int makeTagFull (tokenInfo *const token, vString *const name,
                        const kindType kind, const int scope, int role,
                        vString *const argList, vString *retType,
                        vString *const access)
{
	tagEntryInfo e;

	const char *const tagName = vStringValue (name? name : token->string);
    if (!strcmp (tagName, "_")) return CORK_NIL; // ignore _
	initRefTagEntry (&e, tagName, kind, role);

	e.lineNumber = token->lineNumber;
	e.filePosition = token->filePosition;
	if (argList && !vStringIsEmpty (argList))
		e.extensionFields.signature = vStringValue (argList);
	if (access && !vStringIsEmpty (access))
		e.extensionFields.access = vStringValue (access);
	if (retType)
	{
		e.extensionFields.typeRef [0] = "typename";
		e.extensionFields.typeRef [1] = vStringValue( retType );
	}
	e.extensionFields.scopeIndex = scope;

	return makeTagEntry (&e);
}

static int makeFnTag (tokenInfo *const token, vString *const name,
                            const int scope, vString *const argList,
                            vString *const retType, vString *const access)
{
	return makeTagFull (token, name, KIND_FUNCTION, scope,
	                    ROLE_DEFINITION_INDEX, argList, retType, access);
}

static int makeTag (tokenInfo *const token, vString *const name, kindType kind,
                    int scope)
{
	return makeTagFull (token, name, kind, scope, ROLE_DEFINITION_INDEX, NULL,
	                    NULL, NULL);
}

static int makeTagEx (tokenInfo *const token, vString *const name, kindType kind,
                      int scope, vString *const access)
{
	return makeTagFull (token, name, kind, scope, ROLE_DEFINITION_INDEX, NULL,
	                    NULL, access);
}

static int makeReferenceTag (tokenInfo *const token, vString *const name,
                             kindType kind, int scope, int role)
{
	return makeTagFull (token, name, kind, scope, role, NULL, NULL, NULL);
}


static tokenType getClose (tokenType open)
{
	switch (open)
	{
	case TOKEN_OPEN_PAREN: return TOKEN_CLOSE_PAREN;
	case TOKEN_OPEN_SQUARE: return TOKEN_CLOSE_SQUARE;
	case TOKEN_OPEN_CURLY: return TOKEN_CLOSE_CURLY;
	default: return TOKEN_NONE;
	}
}

static void skipToClose (tokenType open, vString *const capture)
{
	tokenType close = getClose (open);
	if (close != TOKEN_NONE)
	{
		tokenInfo *const token = newToken ();
		int nest = 1;
		do
		{
			readTokenFull (token, capture);
			if (isToken (token, open))
				nest++;
			else if (isToken (token, close))
				nest--;
			else if (isToken (token, TOKEN_EOF))
				break;
		} while (nest > 0);
		deleteToken (token);
	}
}

// ident >['.' ident]*
static void parseFQIdent (tokenInfo *const token, vString *const capture,
                             vString *const identifier)
{
	PARSER_PROLOGUE ("fqident");
	vString *const acc = capture? vStringNew () : NULL;
	tokenType prev;
	do
	{
		vStringAccumulate (capture, acc);
		if (identifier)
		{
			if (isToken (token, TOKEN_DOT))
				vStringPut (identifier, '.');
			else
				vStringCat (identifier, token->string);
		}
		prev = token->type;
		readTokenFull (token, acc);
	} while (isToken (token, TOKEN_DOT, TOKEN_IDENT) && !isToken (token, prev));
	unreadTokenFull (acc);
	vStringDelete (acc);
	PARSER_EPILOGUE ();
}

// '(' >any* ')'
static void parseSimpleExpressionList (
	tokenInfo *const token, vString *const capture)
{
	PARSER_PROLOGUE ("simpleexprlist");
	int close = getClose (token->type);
	vString *acc = vStringNew ();
	do
	{
		readTokenFull (token, acc);
		vStringAccumulate (capture, acc);
	}
	while (!isToken (token, TOKEN_EOF, close));
	vStringDelete (acc);
	PARSER_EPILOGUE ();
}

// '(' >[expr [',' expr]*]? ')'
static void parseExpressionList (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("exprlist");
	int close = getClose (token->type);
	do
	{
		readToken (token);
		if (!isToken (token, TOKEN_EOF, close))
		{
			parseExpression (token, scope);
			readToken (token);
			expectToken (token, TOKEN_COMMA, close);
		}
	}
	while (!isToken (token, TOKEN_EOF, close));
	PARSER_EPILOGUE ();
}

// ['!' | '?']? >[[fqident | type] ['[' type ']'] | 'map' '[' type ']' fqtype]
static void parseFQType (tokenInfo *const token, vString *const capture)
{
	PARSER_PROLOGUE ("fqtype");

	vString *const acc = capture? vStringNew () : NULL;

	if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
		readTokenFull (token, acc);

	if (isToken (token, TOKEN_IDENT, TOKEN_TYPE) ||
	    isKeyword (token, KEYWORD_TYPE))
	{
		if (isToken (token, TOKEN_IDENT))
			parseFQIdent (token, acc, NULL);
		vStringAccumulate (capture, acc);

		readTokenFull (token, acc);
		if (isToken (token, TOKEN_OPEN_SQUARE))
		{
			skipToClose (TOKEN_OPEN_SQUARE, acc);
			vStringAccumulate (capture, acc);
		}
		else
			unreadToken ();
	}
	else if (isToken (token, KEYWORD_map))
	{
		vStringAccumulate (capture, acc);
		readTokenFull (token, acc);
		if (expectToken (token, TOKEN_OPEN_SQUARE))
		{
			skipToClose (TOKEN_OPEN_SQUARE, acc);
            readTokenFull (token, acc);
            parseFQType (token, acc);
			vStringAccumulate (capture, acc);
		}
	}
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

// fqident '(' >expr-list ')' ['!' | '?' | 'or' block]?
static void parseFnCall (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("fncall");
	parseExpressionList (token, scope);
	readToken (token);
	if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
		readToken (token);
	else if (isKeyword (token, KEYWORD_or))
	{
		readToken (token);
		if (expectToken (token, TOKEN_OPEN_CURLY))
			parseBlock (token, scope);
		else
			unreadToken ();
	}
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

// fqident ':=' >expr
static void parseDeclare (tokenInfo *const token, vString *const name,
                          int scope)
{
	PARSER_PROLOGUE ("declare");
	makeTag (token, name, KIND_VARIABLE, CORK_NIL);
	readToken (token);
	parseExpression (token, scope);
	PARSER_EPILOGUE ();
}

// 'if' >expr block ['else' [block | if] ]?
static void parseIf (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("if");
	readToken (token);
	parseExpression (token, scope);
	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, scope);
		readToken (token);
		if (isKeyword (token, KEYWORD_else))
		{
			readToken (token);
			if (isToken (token, TOKEN_OPEN_CURLY))
				parseBlock (token, scope);
			else if (isKeyword (token, KEYWORD_if))
				parseIf (token, scope);
			else
			{
				expectToken (token, TOKEN_OPEN_CURLY, TOKEN_KEYWORD) ||
					expectKeyword (token, KEYWORD_if);
				unreadToken();
			}
		}
		else
			unreadToken ();
	}
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

// '{' >[[immediate | ident] ':' expr]* '}'
static void parseMapInit (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("mapinit");
	do
	{
		readToken (token);
		if (isToken (token, TOKEN_COMMA))
			;
		else if (isToken (token, TOKEN_IDENT, TOKEN_IMMEDIATE))
		{
			readToken (token);
			if (isToken (token, TOKEN_COLON))
			{
				readToken (token);
				parseExpression (token, scope);
			}
		}
	} while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY));
	PARSER_EPILOGUE ();
}

// isExpr: 'fn' >[ident | clsrargs]? args fqtype? block
// !isExpr: 'fn' >receiver? ident args cargs? fqtype? block?
static void parseFn (tokenInfo *const token, int scope, bool isExpr,
                     vString *const access)
{
	PARSER_PROLOGUE (isExpr? "fne" : "fn");
	vString *receiver = NULL;
	vString *name = NULL;
	vString *acc = vStringNew ();
	vString *argList = vStringNew ();
	vString *retType = vStringNew ();
	tokenInfo *fnToken = dupToken (token);
	tokenInfo *rxToken = NULL;
	readToken (token);

	// receiver
	if (!isExpr && isToken (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (acc, '(');
		readTokenFull (token, acc);
		if (expectToken (token, TOKEN_IDENT, TOKEN_KEYWORD) ||
			expectKeyword (token, KEYWORD_mut))
		{
			if (isKeyword (token, KEYWORD_mut))
				readTokenFull (token, acc);
			if (expectToken (token, TOKEN_IDENT))
			{
				receiver = vStringNewCopy (token->string);
				rxToken = dupToken (token);
				readTokenFull (token, acc);
			}
		}
		if (isToken (token, TOKEN_ASTERISK))
			readTokenFull (token, acc);
		if (expectToken (token, TOKEN_TYPE, TOKEN_KEYWORD) ||
		    expectKeyword (token, KEYWORD_TYPE))
			readTokenFull (token, acc);
		if (expectToken (token, TOKEN_CLOSE_PAREN))
		{
			vStringAccumulate (argList, acc);
			readToken (token);
		}
		else
			skipToClose (TOKEN_OPEN_PAREN, NULL);
	}
	// name
	if ((isExpr && isToken (token, TOKEN_IDENT)) ||
		(!isExpr && expectToken (token, TOKEN_IDENT)))
	{
		deleteToken (fnToken);
		fnToken = dupToken (token);
		name = vStringNewCopy (token->string);
		readToken (token);
	}
	// closure args
	if (isExpr && name == NULL && isToken (token, TOKEN_OPEN_SQUARE))
	{
		vStringPut (argList, '[');
		parseSimpleExpressionList (token, acc);
		vStringAccumulate (argList, acc);
		readToken (token);
	}
	// args
	if (expectToken (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (argList, '(');
		parseSimpleExpressionList (token, acc);
		vStringAccumulate (argList, acc);
		readTokenFull (token, acc);
		// type
		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION, TOKEN_TYPE,
		             TOKEN_ASTERISK) ||
			isKeyword (token, KEYWORD_TYPE, KEYWORD_map))
		{
			parseFQType (token, acc);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		else if (isToken (token, TOKEN_OPEN_PAREN))
		{
			parseSimpleExpressionList (token, acc);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		int newScope = CORK_NIL;
		if (name)
		{
			newScope = makeFnTag (
				fnToken, name, scope, argList, retType, access);
			if (receiver)
				makeTag (rxToken, NULL, KIND_RPARAM, newScope);
		}
		// block
		if (expectToken (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, newScope);
			tagEntryInfo *entry = getEntryInCorkQueue (newScope);
			if (entry)
				entry->extensionFields.endLine = token->lineNumber;
		}
		else
			unreadToken ();
	}
	else
		unreadToken ();
	deleteToken (fnToken);
	deleteToken (rxToken);
	vStringDelete (receiver);
	vStringDelete (name);
	vStringDelete (acc);
	vStringDelete (argList);
	vStringDelete (retType);
	PARSER_EPILOGUE ();
}

static void parseExpression (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("expression");
	if (isToken (token, TOKEN_IDENT))
	{
		vString *const identifier = vStringNew ();
		parseFQIdent (token, NULL, identifier);

		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else if (isToken (token, TOKEN_DECLARE))
			parseDeclare (token, identifier, scope);
		else if (isToken (token, TOKEN_OPERATOR, TOKEN_AMPERSAND, TOKEN_ASTERISK))
		{
			readToken (token);
			parseExpression (token, scope);
		}
		else
			unreadToken ();

		vStringDelete (identifier);
	}
	else if (isToken (token, TOKEN_IMMEDIATE))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPERATOR, TOKEN_AMPERSAND, TOKEN_ASTERISK))
		{
			readToken (token);
			parseExpression (token, scope);
		}
		else
			unreadToken();
	}
	else if (isToken (token, TOKEN_TYPE))
	{
		parseFQType (token, NULL);

		readToken (token);
		if (isToken (token, TOKEN_OPEN_CURLY))
			parseMapInit (token, scope);
		else
			unreadToken ();
	}
	else if (isToken (token, TOKEN_AMPERSAND))
	{
		readToken (token);
		if (expectToken (token, TOKEN_TYPE, TOKEN_KEYWORD) ||
		    expectKeyword (token, KEYWORD_map, KEYWORD_TYPE))
			parseExpression (token, scope);
		else
			unreadToken ();
	}
	else if (isKeyword (token, KEYWORD_TYPE))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else if (isToken (token, TOKEN_OPEN_CURLY))
			parseMapInit (token, scope);
		else
			unreadToken ();
	}
	else if (isToken (token, TOKEN_TILDE, TOKEN_EXCLAMATION))
	{
		readToken (token);
		parseExpression (token, scope);
	}
	else if (isKeyword (token, KEYWORD_fn))
		parseFn (token, scope, true, NULL);
	else if (isKeyword (token, KEYWORD_if))
		parseIf (token, scope);
	else if (isToken (token, TOKEN_OPEN_SQUARE))
		parseExpressionList (token, scope);
	else
		expectToken (token, NULL);
	PARSER_EPILOGUE ();
}

// return >expr?
static void parseReturn (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("return");
	readToken (token);
	if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION, TOKEN_AMPERSAND,
	             TOKEN_IMMEDIATE, TOKEN_OPEN_PAREN, TOKEN_IDENT) ||
		isKeyword (token, KEYWORD_TYPE, KEYWORD_map))
		parseExpression (token, scope);
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

// defer >block
static void parseDefer (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("defer");
	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope);
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

static void parseStmt (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("stmt");
	if (isKeyword (token, KEYWORD_return))
		parseReturn (token, scope);
	else if (isKeyword (token, KEYWORD_defer))
		parseDefer (token, scope);
	else
		parseExpression (token, scope);
	PARSER_EPILOGUE ();
}

// '{' >statement* '}'
static void parseBlock (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("block");
	tokenType close = getClose (token->type);
	do
	{
		readToken (token);
		if (isToken (token, close))
			break;
		else
			parseStmt (token, scope);
	}
	while (!isToken (token, TOKEN_EOF));
	PARSER_EPILOGUE ();
}

// 'module' >ident
static int parseModule (tokenInfo *const token)
{
	PARSER_PROLOGUE ("module");
	int scope = CORK_NIL;
	readToken (token);
	if (expectToken (token, TOKEN_IDENT))
		scope = makeTag (token, NULL, KIND_MODULE, CORK_NIL);
	else
		unreadToken ();
	PARSER_EPILOGUE ();
	return scope;
}

// 'import' >fqident ['as' ident]? ['{' ident [',' ident]* '}']?
static void parseImport (tokenInfo *const token)
{
	PARSER_PROLOGUE ("import");
	readToken (token);
	if (expectToken (token, TOKEN_IDENT))
	{
		tokenInfo *moduleToken = dupToken (token);
		vString *moduleName = vStringNew ();
		parseFQIdent (token, NULL, moduleName);
		readToken (token);
		if (isKeyword (token, KEYWORD_as))
		{
			readToken (token);
			if (expectToken (token, TOKEN_IDENT))
				vStringCopy (moduleName, token->string);
			readToken (token);
		}
		int moduleScope = makeReferenceTag (moduleToken, moduleName, KIND_MODULE,
		                                    CORK_NIL, ROLE_IMPORTED_MODULE);
		vStringDelete (moduleName);
		deleteToken (moduleToken);
		if (isToken (token, TOKEN_OPEN_CURLY))
		{
			do
			{
				readToken (token);
				if (expectToken (token, TOKEN_IDENT))
				{
					makeReferenceTag (token, NULL, KIND_UNKNOWN, moduleScope,
					         ROLE_IMPORTED_SYMBOL);
					readToken (token);
					expectToken (token, TOKEN_COMMA, TOKEN_CLOSE_CURLY);
				}
			}
			while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY));
		}
		else
			unreadToken ();
	}
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

// ident >'=' expr
static void parseConstExpr (tokenInfo *const token, vString *const access)
{
	PARSER_PROLOGUE ("constexpr");
	makeTagEx (token, NULL, KIND_CONST, CORK_NIL, access);
	readToken (token);
	if (expectToken (token, TOKEN_ASSIGN))
	{
		readToken (token);
		parseExpression (token, CORK_NIL);
	}
	else
		unreadToken ();
	PARSER_EPILOGUE ();
}

// const >[constexpr | '(' constexpr* ')' ]
static void parseConst (tokenInfo *const token, vString *const access)
{
	PARSER_PROLOGUE ("const");
	readToken (token);
	if (isToken (token, TOKEN_OPEN_PAREN))
	{
		readToken (token);
		do
		{
			if (isToken (token, TOKEN_IDENT))
				parseConstExpr (token, access);
			readToken (token);
		} while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_PAREN));
	}
	else if (isToken (token, TOKEN_IDENT))
		parseConstExpr (token, access);
	else
	{
		expectToken (token, NULL);
		unreadToken ();
	}
	PARSER_EPILOGUE ();
}

static bool isAccess (tokenInfo *const token, vString *const capture)
{
	const char *add = NULL;
	if (isKeyword (token, KEYWORD_mut))
		add = "mut";
	else if (isKeyword (token, KEYWORD_pub))
		add = "pub";
	if (add && capture)
	{
		if (!vStringIsEmpty (capture))
			vStringPut (capture, ' ');
		vStringCatS (capture, add);
	}
	return !!add;
}

static void parseFile (tokenInfo *const token)
{
	PARSER_PROLOGUE ("file");
	int scope = CORK_NIL;
	vString *access = vStringNew ();
	do
	{
		readToken (token);
		if (isAccess (token, access))
			continue;

		if (isToken (token, TOKEN_OPEN_PAREN, TOKEN_OPEN_CURLY,
		             TOKEN_OPEN_SQUARE)) // attribtes
			skipToClose (token->type, NULL);
		else if (isKeyword (token, KEYWORD_module))
			scope = parseModule (token);
		else if (isKeyword (token, KEYWORD_fn))
			parseFn (token, scope, false, access);
		else if (isKeyword (token, KEYWORD_import))
			parseImport (token);
		else if (isKeyword (token, KEYWORD_const))
			parseConst (token, access);
		else
			expectToken (token, TOKEN_EOF);

		vStringClear (access);
	}
	while (!isToken (token, TOKEN_EOF));
	vStringDelete (access);
	PARSER_EPILOGUE ();
}

static void findVTags (void)
{
	tokenInfo *const token = newToken ();

	parseFile (token);
	debugParserPrintf ("\n");

	deleteToken (token);
}

static void initialize (const langType language)
{
	Lang_v = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
	addKeywordGroup (&VTypeKeywords, Lang_v);
	addKeywordGroup (&VRestKeywords, Lang_v);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

extern parserDefinition *VParser (void)
{
	static const char *const extensions[] = { "v", NULL };
	parserDefinition *def = parserNew ("V");
	def->kindTable = VKinds;
	def->kindCount = ARRAY_SIZE (VKinds);
	def->extensions = extensions;
	def->parser = findVTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->keywordTable = VKeywordTable;
	def->keywordCount = ARRAY_SIZE (VKeywordTable);
	def->fieldTable = VFields;
	def->fieldCount = ARRAY_SIZE (VFields);
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	def->requestAutomaticFQTag = true;
	return def;
}
