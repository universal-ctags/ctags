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
#define isIdentInitial(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || \
                           c == '_')
#define isIdentSubsequent(c) (isIdentInitial (c) || isDigit (c) || c == '_')
#define unreadToken(t) (unreadTokenFull (t, NULL))

#ifdef DEBUG
#define vDebugPrintf(...) \
	DebugStatement ( if (debug (DEBUG_PARSE)) printf (__VA_ARGS__); )
#define PARSER_PROLOGUE(c) \
	const char *const _prevParser = CurrentParser; \
	CurrentParser = (c); \
    vDebugPrintf ("{%s:", CurrentParser)
#define PARSER_EPILOGUE() \
    vDebugPrintf (":%s}", CurrentParser); \
	CurrentParser = _prevParser
#else
#define vDebugPrintf(...)
#define PARSER_PROLOGUE(c) do{}while(0)
#define PARSER_EPILOGUE() do{}while(0)
#endif

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
	KEYWORD_unsafe,
	KEYWORD_for,
	KEYWORD_in,
	KEYWORD_continue,
	KEYWORD_break,
	KEYWORD_assert,
	KEYWORD_struct,
	KEYWORD_interface,
	KEYWORD_is,
	KEYWORD_enum,
	KEYWORD_type,
	KEYWORD___global,
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
		"shared", "static",
		"go", "spawn", "asm", "type",
		"union", "match",
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
	{"unsafe", KEYWORD_unsafe},
	{"for", KEYWORD_for},
	{"in", KEYWORD_in},
	{"continue", KEYWORD_continue},
	{"break", KEYWORD_break},
	{"assert", KEYWORD_assert},
	{"struct", KEYWORD_struct},
	{"interface", KEYWORD_interface},
	{"is", KEYWORD_is},
	{"enum", KEYWORD_enum},
	{"type", KEYWORD_type},
	{"__global", KEYWORD___global},
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
	TOKEN_SEMICOLON,
	TOKEN_INCDECOP,
	TOKEN_EOF,
	COUNT_TOKEN
} tokenType;

#ifdef DEBUG
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
	"SEMICOLON",
	"INCDECOP",
	"EOF"
};
#endif

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
	KIND_LABEL,
	KIND_STRUCT,
	KIND_FIELD,
	KIND_METHOD,
	KIND_ENUMERATOR,
	KIND_ENUMERATION,
	KIND_ALIAS,
	KIND_INTERFACE,
	KIND_UNKNOWN,
	COUNT_KIND
} kindType;

static kindDefinition VKinds[COUNT_KIND] = {
	{true, 'f', "fn", "functions"},
	{true, 'p', "module", "modules",
	 .referenceOnly = false, ATTACH_ROLES (VModuleRoles)},
	{true, 'v', "variable", "variables"},
	{true, 'c', "const", "constants"},
	//{false, 'z', "param", "function parameters in functions"},
	{true, 'R', "receiver", "receivers in functions"},
	//{false, 'c', "closure", "closure parameters in functions"},
	{true, 'l', "label", "labels"},
	{true, 's', "struct", "structs"},
	{true, 'm', "field", "struct/interface members"},
	{true, 'n', "method", "interface methods"},
	{true, 'e', "enumerator", "enumerators (values inside an enumeration)"},
	{true, 'g', "enum", "enumeration names"},
	{true, 'a', "alias", "type aliases"},
	{true, 'i', "interface", "interfaces"},
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
static tokenInfo *ReplayToken = NULL;
static vString *ReplayCapture = NULL;
//static tokenInfo *NextToken = NULL;
/* static NestingLevels *VNestingLevels = NULL; */
static objPool *TokenPool = NULL;
//static int currentScope = CORK_NIL;
static const char *CurrentParser;
/* static int AnonymousId = 0; */
static tokenType LastTokenType = TOKEN_NONE;
static size_t LastCaptureLen = 0;

static void parseBlock (tokenInfo *const, int);
static void parseExpression (tokenInfo *const, int, vString *const, bool);
static void parseStruct (tokenInfo *const, vString *const, int, kindType);

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
	vStringCopy (dest->string, src->string);
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
}

static tokenInfo *dupToken (tokenInfo *const token)
{
	tokenInfo *newToken = newToken ();
	copyPoolToken (newToken, token);
	return newToken;
}

// _____________________________________________________________________________
//                                                                     TOKENISER

static bool getcFromInputFileIfEq (int expect, vString *const capture)
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
		copyPoolToken (token, ReplayToken);
		deleteToken (ReplayToken);
		ReplayToken = NULL;
#ifdef DEBUG_V_PARSER
		vDebugPrintf ("*%s[%s]", tokenNames[token->type],
		              ReplayCapture? vStringValue (ReplayCapture) : "");
#endif
		LastCaptureLen = capture? vStringLength (capture) : 0;
		if (ReplayCapture && capture)
			vStringCat (capture, ReplayCapture);
		vStringDelete (ReplayCapture);
		ReplayCapture = NULL;
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

#ifdef DEBUG_V_PARSER
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
	else if (c == ';')
		token->type = TOKEN_SEMICOLON;
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
		if (getcFromInputFileIfEq ('=', capture))
			token->type = TOKEN_ASSIGN; // += -= *= /= %= &= |= ^=
		else if (c == '*')
			token->type = TOKEN_ASTERISK; // *
		else if (isOneOf (c, "&|") && getcFromInputFileIfEq (c, capture))
			token->type = TOKEN_OPERATOR; // && ||
		else if (isOneOf (c, "+-") && getcFromInputFileIfEq (c, capture))
			token->type = TOKEN_INCDECOP; // ++ --
		else if (c == '&')
			token->type = TOKEN_AMPERSAND; // &
		else
			token->type = TOKEN_OPERATOR; // + - / % | ^
	}
	else if (isOneOf (c, "=!"))
	{
		if (getcFromInputFileIfEq ('=', capture))
			token->type = TOKEN_OPERATOR; // == !=
		else if (c == '=')
			token->type = TOKEN_ASSIGN; // =
		else
			token->type = TOKEN_EXCLAMATION; // !
	}
	else if (isOneOf (c, "<>"))
	{
		if (getcFromInputFileIfEq (c, capture))
		{
			if (c == '>')
				getcFromInputFileIfEq ('>', capture);
			if (getcFromInputFileIfEq ('=', capture))
				token->type = TOKEN_ASSIGN; // <<= >>= >>>=
			else
				token->type = TOKEN_OPERATOR; // << >> >>>
		}
		else
		{
			getcFromInputFileIfEq ('=', capture);
			token->type = TOKEN_OPERATOR; // < > <= >=
		}
	}
	else if (c == '.')
	{
		if (getcFromInputFileIfEq ('.', capture))
		{
			getcFromInputFileIfEq ('.', capture);
			token->type = TOKEN_SLICE; // .. ...
		}
		else
			token->type = TOKEN_DOT; // .
	}
	else if (c == ':')
	{
		if (getcFromInputFileIfEq ('=', capture))
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
	else if (c == '@')
	{
		token->type = TOKEN_IDENT;
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
		vDebugPrintf ("\nUNRECOGNISED CHAR AT LINE %lu: %c (%u)\n",
		              token->lineNumber, c, c);
	}

	LastTokenType = token->type;

#ifdef DEBUG_V_PARSER
	vDebugPrintf ("%s[%s]", tokenNames[token->type], vStringValue (capture));
	if (oldCapture)
		vStringCat (oldCapture, capture);
	vStringDelete (capture);
#endif
}

static void unreadTokenFull (tokenInfo *const token, vString *const acc)
{
#ifdef DEBUG_V_PARSER
	vDebugPrintf ("#");
#endif
	deleteToken (ReplayToken);
	ReplayToken = dupToken (token);
	if (acc)
	{
		Assert (LastCaptureLen <= vStringLength (acc));
		vStringDelete (ReplayCapture);
		ReplayCapture = vStringNewInit (vStringValue (acc) + LastCaptureLen);
		vStringTruncate (acc, LastCaptureLen);
	}
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, NULL);
}

// _____________________________________________________________________________
//                                                                    HELPER FNS

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
	DebugStatement (
		if (expected && !found)
		{
			vString *got;
			vString *exp;
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
			vDebugPrintf (
				"\nUNEXPECTED %s IN {%s} AT LINE %lu: %s%s\n",
				vStringValue (got), CurrentParser, token->lineNumber,
				tokenNames[token->type], vStringValue (exp));
			vStringDelete (got);
			vStringDelete (exp);
			va_end (argp);
		}
	);
	// Although we don't show a debug parser error, we always return failure for
	// expectToken() on a keyword token, so that the call can be chained to an
	// expectKeyword() call to check which keyword it is. E.g.:
	//     expectToken (token, xxx, TOKEN_KEYWORD) || expectKeyword (token, xxx)
	// Note that isToken() is not affected by this special case.
	return (expected && !keyword && token->type == TOKEN_KEYWORD)? false : found;
}

static int makeTagFull (tokenInfo *const token, const char *name,
                        const kindType kind, const int scope, int role,
                        vString *const argList, vString *retType,
                        const char *access)
{
	tagEntryInfo e;

	const char *const tagName =
		(name && *name != '\0')? name : vStringValue (token->string);
    if (!strcmp (tagName, "_")) return CORK_NIL; // ignore _
	initRefTagEntry (&e, tagName, kind, role);

	e.lineNumber = token->lineNumber;
	e.filePosition = token->filePosition;
	if (argList && !vStringIsEmpty (argList))
		e.extensionFields.signature = vStringValue (argList);
	if (access && *access != '\0')
		e.extensionFields.access = access;
	if (retType)
	{
		e.extensionFields.typeRef [0] = "typename";
		e.extensionFields.typeRef [1] = vStringValue( retType );
	}
	e.extensionFields.scopeIndex = scope;

	return makeTagEntry (&e);
}

static int makeFnTag (tokenInfo *const token, vString *const name,
                      kindType kind, const int scope, vString *const argList,
                      vString *const retType, vString *const access)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind,
	                    scope, ROLE_DEFINITION_INDEX, argList, retType,
	                    access? vStringValue (access) : NULL);
}

static int makeTag (tokenInfo *const token, vString *const name, kindType kind,
                    int scope)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind, scope,
	                    ROLE_DEFINITION_INDEX, NULL, NULL, NULL);
}

static int makeTagEx (tokenInfo *const token, vString *const name, kindType kind,
                      int scope, vString *const access)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind, scope,
	                    ROLE_DEFINITION_INDEX, NULL, NULL,
	                    access? vStringValue (access) : NULL);
}

static int makeReferenceTag (tokenInfo *const token, vString *const name,
                             kindType kind, int scope, int role)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind, scope,
	                    role, NULL, NULL, NULL);
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

static bool isAccess (tokenInfo *const token, vString **const capture)
{
	const char *add = NULL;
	if (isKeyword (token, KEYWORD_mut))
		add = "mut";
	else if (isKeyword (token, KEYWORD_pub))
		add = "pub";
	else if (isKeyword (token, KEYWORD___global))
		add = "__global";
	if (add && capture)
	{
		if (*capture == NULL)
			*capture = vStringNew ();
		if (!vStringIsEmpty (*capture))
			vStringPut (*capture, ' ');
		vStringCatS (*capture, add);
	}
	return !!add;
}

static char *nextCommaIdent (char *s)
{
	if (s)
	{
		while (*s != ',' && *s != '\0')
			s++;
		if (*s == ',')
			*(s++) = '\0';
		while (*s == ' ')
			s++;
		if (*s == '\0')
			s = NULL;
	}
	return s;
}

// _____________________________________________________________________________
//                                                                        PARSER

// Note: all parsers expect the caller to have already read the initial token
// prior to calling, but will not themselves over-read on exit. Parsers will
// unread the last token, if is not within their purview.

// ident ['.' ident]*
static void parseFQIdent (tokenInfo *const token, vString *const capture,
                             vString *const identifier)
{
	PARSER_PROLOGUE ("fqident");
	vString *const acc = capture? vStringNew () : NULL;
	tokenType prev = TOKEN_IDENT;
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
	unreadTokenFull (token, acc);
	vStringDelete (acc);
	PARSER_EPILOGUE ();
}

// '(' any* ')'
static void parseSimpleExprList (
	tokenInfo *const token, vString *const capture)
{
	PARSER_PROLOGUE ("simplelist");
	int close = getClose (token->type);
	do
		readTokenFull (token, capture);
	while (!isToken (token, TOKEN_EOF, close));
	PARSER_EPILOGUE ();
}

// ['!' | '?' | '[' ']' | '&']* [[type '.']* type | 'map' '[' type ']' fqtype | 'struct' struct]
static void parseFQType (tokenInfo *const token, vString *const capture,
                         int scope)
{
	PARSER_PROLOGUE ("fqtype");

	while (true)
	{
		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
			;
		else if (isToken (token, TOKEN_AMPERSAND))
			;
		else if (isToken (token, TOKEN_OPEN_SQUARE))
			skipToClose (TOKEN_OPEN_SQUARE, capture);
		else
			break;
		readTokenFull (token, capture);
	}

	if (isToken (token, TOKEN_TYPE))
	{
		bool prev;
		do
		{
			prev = isToken (token, TOKEN_DOT);
			readTokenFull (token, capture);
		}
		while (isToken (token, TOKEN_DOT, TOKEN_TYPE, TOKEN_IDENT) &&
		       isToken (token, TOKEN_DOT) != prev);
		unreadTokenFull (token, capture);
	}
	else if (isKeyword (token, KEYWORD_TYPE))
		;
	else if (isKeyword (token, KEYWORD_map))
	{
		readTokenFull (token, capture);
		if (expectToken (token, TOKEN_OPEN_SQUARE))
		{
			skipToClose (TOKEN_OPEN_SQUARE, capture);
			readTokenFull (token, capture);
			parseFQType (token, capture, scope);
		}
	}
	else if (isKeyword (token, KEYWORD_struct))
		parseStruct (token, NULL, scope, KIND_NONE);
	else
		unreadTokenFull (token, capture);

	PARSER_EPILOGUE ();
}

// '{' [[immediate | ident] ':' expr]* '}'
static void parseInit (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("init");
	bool initial = true;
	do
	{
		readToken (token);

		if (initial && isToken (token, TOKEN_SLICE))
		{
			readToken (token);
			if (expectToken (token, TOKEN_IDENT))
			{
				parseFQIdent (token, NULL, NULL);
				readToken (token);
			}
		}
		initial = false;

		if (isToken (token, TOKEN_COMMA))
			;
		else if (isToken (token, TOKEN_IDENT, TOKEN_IMMEDIATE))
		{
			readToken (token);
			if (isToken (token, TOKEN_COLON))
			{
				readToken (token);
				parseExpression (token, scope, NULL, false);
				readToken (token);
			}
		}
	} while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY));
	PARSER_EPILOGUE ();
}

// fqtype init?
static void parseFQTypeInit (tokenInfo *const token, int scope)
{
	parseFQType (token, NULL, scope);
	readToken (token);
	if (isToken (token, TOKEN_OPEN_CURLY))
		parseInit (token, scope);
	else
		unreadToken (token);
}

// '(' expr-list ')' ['!' | '?' | 'or' block]?
static void parseFnCall (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("fncall");
	tokenType close = getClose (token->type);

	readToken (token);
	if (!isToken (token, close))
	{
		parseExpression (token, scope, NULL, true);
		readToken (token);
	}

	if (expectToken (token, close))
	{
		readToken (token);
		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
			;
		else if (isKeyword (token, KEYWORD_or))
		{
			readToken (token);
			if (expectToken (token, TOKEN_OPEN_CURLY))
				parseBlock (token, scope);
			else
				unreadToken (token);
		}
		else
			unreadToken (token);
	}
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// ':=' expr-list
static void parseAssign (tokenInfo *const token, vString *const names,
                         int scope, vString *const accesses)
{
	PARSER_PROLOGUE ("assign");
	if (isToken (token, TOKEN_DECLARE))
	{
		char *name = names? vStringValue (names) : NULL;
		char *access = accesses? vStringValue (accesses) : NULL;
		while (name)
		{
			char *n = name, *a = access;
			name = nextCommaIdent (name);
			access = nextCommaIdent (access);
			makeTagFull (token, n, KIND_VARIABLE, CORK_NIL,
			             ROLE_DEFINITION_INDEX, NULL, NULL, a);
		}
	}
	readToken (token);
	parseExpression (token, scope, NULL, true);
	PARSER_EPILOGUE ();
}

// 'if' expr block ['else' [block | if] ]?
static void parseIf (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("if");
	readToken (token);
	parseExpression (token, scope, NULL, false);
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
				unreadToken (token);
			}
		}
		else
			unreadToken (token);
	}
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// NONE: 'fn' [ident | clsrargs]? args fqtype? block
// FUNCTION: 'fn' receiver? ident args cargs? fqtype? block?
// METHOD: ident args fqtype?
static void parseFn (tokenInfo *const token, int scope, vString *const access,
                     kindType kind)
{
	Assert (kind == KIND_FUNCTION || kind == KIND_METHOD || kind == KIND_NONE);
	PARSER_PROLOGUE (kind == KIND_FUNCTION? "fn-def" :
	                 (kind == KIND_METHOD? "method" : "lambda"));
	vString *receiver = NULL;
	vString *name = NULL;
	vString *acc = vStringNew ();
	vString *argList = vStringNew ();
	vString *retType = vStringNew ();
	tokenInfo *fnToken = dupToken (token);
	tokenInfo *rxToken = NULL;
	readToken (token);

	// receiver
	if (kind == KIND_FUNCTION && isToken (token, TOKEN_OPEN_PAREN))
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
	if ((kind == KIND_NONE && isToken (token, TOKEN_IDENT)) ||
		(kind == KIND_FUNCTION && expectToken (token, TOKEN_IDENT)))
	{
		deleteToken (fnToken);
		fnToken = dupToken (token);
		name = vStringNewCopy (token->string);
		readToken (token);
	}
	// closure args
	if (kind == KIND_NONE && name == NULL && isToken (token, TOKEN_OPEN_SQUARE))
	{
		vStringPut (argList, '[');
		parseSimpleExprList (token, acc);
		vStringAccumulate (argList, acc);
		readToken (token);
	}
	// args
	if (expectToken (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (argList, '(');
		parseSimpleExprList (token, acc);
		vStringAccumulate (argList, acc);
		readTokenFull (token, acc);
		// type
		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION, TOKEN_TYPE,
		             TOKEN_ASTERISK) ||
			isKeyword (token, KEYWORD_TYPE, KEYWORD_map))
		{
			parseFQType (token, acc, scope);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		else if (isToken (token, TOKEN_OPEN_PAREN))
		{
			parseSimpleExprList (token, acc);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		int newScope = CORK_NIL;
		if (kind != KIND_NONE || name)
		{
			kindType fnKind = kind == KIND_NONE? KIND_FUNCTION : kind;
			newScope = makeFnTag (fnToken, name, fnKind, scope,
			                      argList, retType, access);
			if (receiver)
				makeTag (rxToken, NULL, KIND_RPARAM, newScope);
		}
		// block
		if (kind != KIND_METHOD && expectToken (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, newScope);
			tagEntryInfo *entry = getEntryInCorkQueue (newScope);
			if (entry)
				entry->extensionFields.endLine = token->lineNumber;
		}
		else
			unreadToken (token);
	}
	else
		unreadToken (token);
	deleteToken (fnToken);
	deleteToken (rxToken);
	vStringDelete (receiver);
	vStringDelete (name);
	vStringDelete (acc);
	vStringDelete (argList);
	vStringDelete (retType);
	PARSER_EPILOGUE ();
}

// kw block
static void parseStmtBlock (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("stmt-block");
	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope);
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// kw '{' expr '}'
static void parseExprBlock (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("expr-block");
	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
	{
		readToken (token);
		if (!isToken (token, TOKEN_CLOSE_CURLY))
		{
			parseExpression (token, scope, NULL, false);
			readToken (token);
		}
		if (!expectToken (token, TOKEN_CLOSE_CURLY))
			skipToClose (TOKEN_OPEN_CURLY, NULL);
	}
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

static void parseExpression (tokenInfo *const token, int scope,
                             vString *const access, bool canBeList)
{
	PARSER_PROLOGUE (canBeList? "expr-list" : "expr");

	vString *_access = access;
	while (true)
	{
		if (isAccess (token, &_access))
		{
			readToken (token);
			continue;
		}
		break;
	}

	if (isToken (token, TOKEN_IDENT))
	{
		vString *const identifier = vStringNew ();
		parseFQIdent (token, NULL, identifier);
		readToken (token);
		if (isToken (token, TOKEN_COLON))
			makeTag (token, identifier, KIND_LABEL, scope);
		else if (canBeList && isToken (token, TOKEN_COMMA))
		{
			_access = _access? _access : vStringNew ();
			tokenType prev = TOKEN_IDENT;
			while (true)
			{
				tokenType tmp = token->type;
				if (prev == TOKEN_IDENT && isToken (token, TOKEN_COMMA))
				{
					vStringPut (identifier, ',');
					vStringPut (_access, ',');
				}
				else if (prev != TOKEN_IDENT && isToken (token, TOKEN_IDENT))
					parseFQIdent (token, NULL, identifier);
				else if (prev != TOKEN_IDENT && isAccess (token, &_access))
					;
				else
					break;
				prev = tmp;
				readToken (token);
			}
			if (isToken (token, TOKEN_DECLARE, TOKEN_ASSIGN))
				parseAssign (token, identifier, scope, _access);
			else
				unreadToken (token);
		}
		else
		{
			if (isToken (token, TOKEN_INCDECOP))
				readToken (token);

			if (isToken (token, TOKEN_OPEN_PAREN))
				parseFnCall (token, scope);
			else if (isToken (token, TOKEN_DECLARE, TOKEN_ASSIGN))
				parseAssign (token, identifier, scope, _access);
			else if (isToken (token, TOKEN_OPERATOR, TOKEN_AMPERSAND,
			                  TOKEN_ASTERISK))
			{
				readToken (token);
				parseExpression (token, scope, NULL, canBeList);
			}
			else if (isKeyword (token, KEYWORD_is))
			{
				readToken (token);
				parseFQType (token, NULL, scope);
			}
			else if (canBeList && isToken (token, TOKEN_COMMA))
			{
				readToken (token);
				parseExpression (token, scope, NULL, true);
			}
			else
				unreadToken (token);
		}

		vStringDelete (identifier);
	}
	else if (isToken (token, TOKEN_IMMEDIATE))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPERATOR, TOKEN_AMPERSAND, TOKEN_ASTERISK))
		{
			readToken (token);
			parseExpression (token, scope, NULL, canBeList);
		}
		else if (isToken (token, TOKEN_SLICE))
		{
			readToken (token);
			expectToken (token, TOKEN_IMMEDIATE);
		}
		else if (canBeList && isToken (token, TOKEN_COMMA))
		{
			readToken (token);
			parseExpression (token, scope, NULL, true);
		}
		else
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_TYPE))
		parseFQTypeInit (token, scope);
	else if (isToken (token, TOKEN_AMPERSAND))
	{
		readToken (token);
		if (expectToken (token, TOKEN_TYPE, TOKEN_KEYWORD) ||
		    expectKeyword (token, KEYWORD_map, KEYWORD_TYPE))
			parseFQTypeInit (token, scope);
		else
			unreadToken (token);
	}
	else if (isKeyword (token, KEYWORD_TYPE))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_TILDE, TOKEN_EXCLAMATION))
	{
		readToken (token);
		parseExpression (token, scope, NULL, canBeList);
	}
	else if (isKeyword (token, KEYWORD_fn))
		parseFn (token, scope, NULL, KIND_NONE);
	else if (isKeyword (token, KEYWORD_if))
		parseIf (token, scope);
	else if (isToken (token, TOKEN_OPEN_SQUARE))
	{
		tokenInfo *tmpToken = newToken (); // []type or [x,x,x]?
		readToken (tmpToken);
		unreadToken (tmpToken);
		if (isToken (tmpToken, TOKEN_CLOSE_SQUARE))
			parseFQTypeInit (token, scope);
		else
		{
			readToken (token);
			parseExpression (token, scope, NULL, true);
			readToken (token);
			if (!expectToken (token, TOKEN_CLOSE_SQUARE))
				unreadToken (token);
		}
		deleteToken (tmpToken);
	}
	else if (isKeyword (token, KEYWORD_unsafe))
		parseExprBlock (token, scope);
	else
		expectToken (token, NULL);

	if (!access)
		vStringDelete (_access);
	PARSER_EPILOGUE ();
}

// return expr-list?
static void parseReturn (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("return");
	readToken (token);
	if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION, TOKEN_AMPERSAND,
	             TOKEN_IMMEDIATE, TOKEN_OPEN_PAREN, TOKEN_IDENT) ||
		isKeyword (token, KEYWORD_TYPE, KEYWORD_map))
		parseExpression (token, scope, NULL, true);
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// for access? [ident [[',' access? ident]? ['in' expr]? |
//                     expr [';' expr]*]]? block
static void parseFor (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("for");
	tokenInfo *var1Token = NULL;
	tokenInfo *var2Token = NULL;
	vString *var1Access = NULL;
	vString *var2Access = NULL;
	readToken (token);
	if (isAccess (token, &var1Access))
		readToken (token);
	if (expectToken (token, TOKEN_IDENT, TOKEN_OPEN_CURLY) &&
	    isToken (token, TOKEN_IDENT))
	{
		var1Token = dupToken (token);
		readToken (token);
		if (isToken (token, TOKEN_COMMA) ||
			isKeyword (token, KEYWORD_in))
		{
			if (isToken (token, TOKEN_COMMA))
			{
				readToken (token);
				if (isAccess (token, &var2Access))
					readToken (token);
				if (expectToken (token, TOKEN_IDENT))
				{
					var2Token = dupToken (token);
					readToken (token);
				}
			}
			if (expectKeyword (token, KEYWORD_in))
			{
				makeTagEx (var1Token, NULL, KIND_VARIABLE, CORK_NIL, var1Access);
				if (var2Token)
					makeTagEx (var2Token, NULL, KIND_VARIABLE, CORK_NIL,
					           var2Access);
				readToken (token);
				parseExpression (token, scope, NULL, false);
				readToken (token);
			}
		}
		else
		{
			unreadToken (token);
			copyPoolToken (token, var1Token);
			while (true)
			{
				parseExpression (token, scope, var1Access, false);
				readToken (token);
				if (!isToken (token, TOKEN_SEMICOLON))
					break;
				readToken (token);
			}
		}
	}
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope);
	else
		unreadToken (token);
	deleteToken (var1Token);
	deleteToken (var2Token);
	vStringDelete (var1Access);
	vStringDelete (var2Access);
	PARSER_EPILOGUE ();
}

static void parseStatment (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("stmt");
	if (isKeyword (token, KEYWORD_return))
		parseReturn (token, scope);
	else if (isKeyword (token, KEYWORD_defer))
		parseStmtBlock (token, scope);
	else if (isKeyword (token, KEYWORD_for))
		parseFor (token, scope);
	else if (isKeyword (token, KEYWORD_assert))
	{
		readToken (token);
		parseExpression (token, scope, NULL, false);
	}
	else if (isKeyword (token, KEYWORD_continue, KEYWORD_break))
	{
		readToken (token);
		if (!isToken (token, TOKEN_IDENT))
			unreadToken (token);
	}
	else
		parseExpression (token, scope, NULL, true);
	PARSER_EPILOGUE ();
}

// '{' statement* '}'
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
			parseStatment (token, scope);
	}
	while (!isToken (token, TOKEN_EOF));
	PARSER_EPILOGUE ();
}

// 'module' ident
static int parseModule (tokenInfo *const token)
{
	PARSER_PROLOGUE ("module");
	int scope = CORK_NIL;
	readToken (token);
	if (expectToken (token, TOKEN_IDENT))
		scope = makeTag (token, NULL, KIND_MODULE, CORK_NIL);
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
	return scope;
}

// 'import' fqident ['as' ident]? ['{' ident [',' ident]* '}']?
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
			unreadToken (token);
	}
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// ident '=' expr
static void parseConstExpr (tokenInfo *const token, vString *const access)
{
	PARSER_PROLOGUE ("constexpr");
	makeTagEx (token, NULL, KIND_CONST, CORK_NIL, access);
	readToken (token);
	if (expectToken (token, TOKEN_ASSIGN))
	{
		readToken (token);
		parseExpression (token, CORK_NIL, NULL, false);
	}
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// 'const' [constexpr | '(' constexpr* ')' ]
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
		unreadToken (token);
	}
	PARSER_EPILOGUE ();
}

// NONE: kw ident? '{' [access? ident fqtype ['[' .* ']']? ['=' expr]?]* '}'
// else: kw ident '{' [access? ident fqtype ['[' .* ']']? ['=' expr]?]* '}'
static void parseStruct (tokenInfo *const token, vString *const access,
                         int scope, kindType kind)
{
	Assert (kind == KIND_STRUCT || kind == KIND_INTERFACE || kind == KIND_NONE);
	PARSER_PROLOGUE (kind == KIND_INTERFACE? "iface" : "struct");

	readToken (token);
	if ((kind != KIND_NONE && expectToken (token, TOKEN_TYPE)) ||
		(kind == KIND_NONE && expectToken (token, TOKEN_TYPE, TOKEN_OPEN_CURLY)))
	{
		int newScope = scope;
		if (isToken (token, TOKEN_TYPE))
		{
			kindType realKind = kind == KIND_NONE? KIND_STRUCT : kind;
			newScope = makeTagEx (token, NULL, realKind, scope, access);
			readToken (token);
		}
		if (expectToken (token, TOKEN_OPEN_CURLY))
		{
			bool initial = true;
			vString *fieldAccess = NULL;
			while (true)
			{
				vString *tmp = NULL;
				do
					readToken (token);
				while (isAccess (token, &tmp));
				if (tmp)
				{
					vStringDelete (fieldAccess);
					fieldAccess = tmp;
					if(expectToken (token, TOKEN_COLON))
						readToken (token);
				}

				if (initial && isToken (token, TOKEN_TYPE, TOKEN_IDENT))
				{
					parseFQType (token, NULL, newScope); // embedded struct
					readToken (token);
				}
				initial = false;

				if (expectToken (token, TOKEN_IDENT, TOKEN_CLOSE_CURLY))
				{
					if (isToken (token, TOKEN_CLOSE_CURLY))
						break;

					tokenInfo *fieldToken = dupToken (token);
					readToken (token);
					if (kind == KIND_INTERFACE &&
					    isToken (token, TOKEN_OPEN_PAREN))
					{
						unreadToken (token);
						parseFn (fieldToken, newScope, fieldAccess, KIND_METHOD);
					}
					else
					{
						parseFQType (token, NULL, newScope);
						makeTagEx (fieldToken, NULL, KIND_FIELD, newScope,
						           fieldAccess);

						readToken (token);
						if (isToken (token, TOKEN_OPEN_SQUARE))
						{
							skipToClose (TOKEN_OPEN_SQUARE, NULL);
							readToken (token);
						}
						if (isToken (token, TOKEN_ASSIGN))
						{
							readToken (token);
							parseExpression (token, newScope, NULL, false);
						}
						else
							unreadToken (token);
					}
					deleteToken (fieldToken);
				}
				else
				{
					skipToClose (TOKEN_OPEN_CURLY, NULL);
					break;
				}
			};
			vStringDelete (fieldAccess);
		}
		if (kind != KIND_NONE)
		{
			tagEntryInfo *entry = getEntryInCorkQueue (newScope);
			if (entry)
				entry->extensionFields.endLine = token->lineNumber;
		}
	}
	PARSER_EPILOGUE ();
}

// 'enum' type ['as' type]? '{' [ident ['=' immediate]?]* '}'
static void parseEnum (tokenInfo *const token, vString *const access, int scope)
{
	PARSER_PROLOGUE ("enum");
	readToken (token);
	if (expectToken (token, TOKEN_TYPE)) {
		int newScope = makeTagEx (token, NULL, KIND_ENUMERATION, scope, access);
		readToken (token);
		if (isKeyword (token, KEYWORD_as))
		{
			readToken (token);
			if (expectToken (token, TOKEN_TYPE, TOKEN_KEYWORD) ||
				expectKeyword (token, KEYWORD_TYPE))
				readToken (token);
		}
		if (expectToken (token, TOKEN_OPEN_CURLY))
		{
			vString *capture = vStringNewInit (".");
			readTokenFull (token, capture);
			while (expectToken (token, TOKEN_CLOSE_CURLY, TOKEN_IDENT))
			{
				if (isToken (token, TOKEN_CLOSE_CURLY))
					break;

				makeTag (token, capture, KIND_ENUMERATOR, newScope);
				vStringCopyS (capture, ".");
				LastTokenType = TOKEN_DOT; // prevent capture adding ' '

				readTokenFull (token, capture);
				if (isToken (token, TOKEN_ASSIGN))
				{
					vStringCopyS (capture, ".");
					readToken (token);
					if (expectToken (token, TOKEN_IMMEDIATE))
						readTokenFull (token, capture);
				}
			}
			vStringDelete (capture);

			tagEntryInfo *entry = getEntryInCorkQueue (newScope);
			if (entry)
				entry->extensionFields.endLine = token->lineNumber;
		}
	}
	else
		unreadToken (token);
	PARSER_EPILOGUE ();
}

// 'type' type '=' fqtype
static void parseAlias (tokenInfo *const token, vString *const access, int scope)
{
	PARSER_PROLOGUE ("alias");
	readToken (token);
	if (expectToken (token, TOKEN_TYPE))
	{
		makeTagEx (token, NULL, KIND_ALIAS, scope, access);
		readToken (token);
		if (expectToken (token, TOKEN_ASSIGN))
		{
			readToken (token);
			parseFQType (token, NULL, scope);
		}
	}
	PARSER_EPILOGUE ();
}

static void parseFile (tokenInfo *const token)
{
	PARSER_PROLOGUE ("file");
	int scope = CORK_NIL;
	vString *access = NULL;
	do
	{
		readToken (token);
		if (isAccess (token, &access))
			continue;

		if (isToken (token, TOKEN_OPEN_PAREN, TOKEN_OPEN_CURLY,
		             TOKEN_OPEN_SQUARE)) // attributes
			skipToClose (token->type, NULL);
		else if (isKeyword (token, KEYWORD_module))
			scope = parseModule (token);
		else if (isKeyword (token, KEYWORD_fn))
			parseFn (token, scope, access, KIND_FUNCTION);
		else if (isKeyword (token, KEYWORD_import))
			parseImport (token);
		else if (isKeyword (token, KEYWORD_const))
			parseConst (token, access);
		else if (isKeyword (token, KEYWORD_struct))
			parseStruct (token, access, scope, KIND_STRUCT);
		else if (isKeyword (token, KEYWORD_interface))
			parseStruct (token, access, scope, KIND_INTERFACE);
		else if (isKeyword (token, KEYWORD_enum))
			parseEnum (token, access, scope);
		else if (isKeyword (token, KEYWORD_type))
			parseAlias (token, access, scope);
		else
			expectToken (token, TOKEN_EOF);

		if (access)
			vStringClear (access);
	}
	while (!isToken (token, TOKEN_EOF));
	vStringDelete (access);
	PARSER_EPILOGUE ();
}

// _____________________________________________________________________________
//

static void findVTags (void)
{
	tokenInfo *const token = newToken ();

	parseFile (token);
#ifdef DEBUG_V_PARSER
	vDebugPrintf ("\n");
#endif

	deleteToken (token);
}

static void initialize (const langType language)
{
	Lang_v = language;

	TokenPool = objPoolNew (
		16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
	addKeywordGroup (&VTypeKeywords, Lang_v);
	addKeywordGroup (&VRestKeywords, Lang_v);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	deleteToken (ReplayToken);
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
