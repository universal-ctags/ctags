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
#include "param.h"

#define _NARGS(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
#define nArgs(...) _NARGS (__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))
#define vStringAccumulate(c, a) \
	do{ if (c && a) { vStringCat (c, a); vStringClear (a); } }while(0)
#define isToken(token, ...) \
	_isToken (token,  false, false, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define expectToken(token, ...) \
	_isToken (token,  true, false, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define isKeyword(token, ...) \
	_isToken (token, false, true, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define expectKeyword(token, ...) \
	_isToken (token, true, true, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define isWhitespace(c) (c == ' ' || c == '\t' || c == '\r' || c == '\n')
#define isDigit(c) (c >= '0' && c <= '9')
#define isHexDigit(c) (isDigit (c) || \
                       (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
#define isIdentInitial(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || \
                           c == '_' || c == '$')
#define isIdentSubsequent(c) (isIdentInitial (c) || isDigit (c))
#define isOperator(c) ( \
		isToken (c, TOKEN_OPERATOR, TOKEN_PLUSMINUS, TOKEN_ASTERISK, \
		         TOKEN_AMPERSAND, TOKEN_CHPOP, TOKEN_PIPE) || \
		isKeyword (token, KEYWORD_in) )
#define isTypeInitial(c) ( \
		isToken (c, TOKEN_TYPE, TOKEN_OPEN_SQUARE, TOKEN_QUESTION, \
		         TOKEN_EXCLAMATION, TOKEN_ASTERISK, TOKEN_AMPERSAND) || \
		isKeyword (token, KEYWORD_TYPE, KEYWORD_map, KEYWORD_Smap) )
#define isClose(c) (\
		isToken (c, TOKEN_CLOSE_PAREN, TOKEN_CLOSE_SQUARE, TOKEN_CLOSE_CURLY))
#define unreadToken(t) (unreadTokenFull (t, NULL))

#ifdef DEBUG
#define vDebugPrintf(...) \
	DebugStatement ( if (debug (DEBUG_CPP)) fprintf (stderr, __VA_ARGS__); )
#define vDebugParserPrintf(...) \
	do { if (parserSpew) vDebugPrintf(__VA_ARGS__); }while(0)
#define PARSER_PROLOGUE(c) \
	const char *const _prevParser = CurrentParser; \
	CurrentParser = (c); \
    vDebugParserPrintf ("{%s:", CurrentParser)
#define PARSER_EPILOGUE() \
    vDebugParserPrintf (":%s}", CurrentParser); \
	CurrentParser = _prevParser
#else
#define vDebugPrintf(...)
#define vDebugParserPrintf(...)
#define PARSER_PROLOGUE(c)
#define PARSER_EPILOGUE()
#endif

enum {
	KEYWORD_fn,
	KEYWORD_map,
	KEYWORD_or,
	KEYWORD_module,
	KEYWORD_as,
	KEYWORD_import,
	KEYWORD_if,
	KEYWORD_Sif,
	KEYWORD_else,
	KEYWORD_Selse,
	KEYWORD_const,
	KEYWORD_return,
	KEYWORD_mut,
	KEYWORD_pub,
	KEYWORD_defer,
	KEYWORD_unsafe,
	KEYWORD_for,
	KEYWORD_Sfor,
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
	KEYWORD_asm,
	KEYWORD_chan,
	KEYWORD_match,
	KEYWORD_Smap,
	KEYWORD_Sarray,
	KEYWORD_Ssumtype,
	KEYWORD_Salias,
	KEYWORD_Sstruct,
	KEYWORD_Senum,
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
		"string", "rune", "mapnode", "size_t", "usize", "isize",
		"float_literal", "int_literal", "thread", "IError", NULL
	},
};

const static struct keywordGroup VRestKeywords = {
	.value = KEYWORD_REST,
	.addingUnlessExisting = true,
	.keywords = {
		"shared", "static", "go", "spawn", "union", "lock", "rlock", "select",
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
	{"$if", KEYWORD_Sif},
	{"else", KEYWORD_else},
	{"$else", KEYWORD_Selse},
	{"const", KEYWORD_const},
	{"return", KEYWORD_return},
	{"mut", KEYWORD_mut},
	{"pub", KEYWORD_pub},
	{"defer", KEYWORD_defer},
	{"unsafe", KEYWORD_unsafe},
	{"for", KEYWORD_for},
	{"$for", KEYWORD_Sfor},
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
	{"asm", KEYWORD_asm},
	{"chan", KEYWORD_chan},
	{"match", KEYWORD_match},
	{"$map", KEYWORD_Smap},
	{"$array", KEYWORD_Sarray},
	{"$sumtype", KEYWORD_Ssumtype},
	{"$alias", KEYWORD_Salias},
	{"$struct", KEYWORD_Sstruct},
	{"$enum", KEYWORD_Senum},
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
	TOKEN_PLUSMINUS,
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
	TOKEN_PIPE,
	TOKEN_CHPOP,
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
	"PLUSMINUS",
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
	"PIPE",
	"CHPOP",
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

static kindType vLookupKinds[] = {
	KIND_STRUCT,
	KIND_ENUMERATION,
	KIND_INTERFACE,
};

typedef struct {
	int type;
	keywordId keyword;          /* when type is KEYWORD */
	vString *string;            /* when type is KEYWORD, IDENT or TYPE */
	bool fullyQualified;        /* when type is IDENT or TYPE */
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
static bool parserSpew = false;

static void parseStatement (tokenInfo *const, int);
static void parseExpression (tokenInfo *const, int, vString *const);
static void parseExprList (tokenInfo *const, int, vString *const);
static void parseExprCont (tokenInfo *const, int, bool);
static void parseVType (tokenInfo *const, vString *const, int, bool);

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->string = NULL;
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

	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	vStringDelete (token->string);
	token->string = NULL;
	token->fullyQualified = false;
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringDelete (dest->string);
	dest->string = src->string? vStringNewCopy (src->string) : NULL;
	dest->fullyQualified = src->fullyQualified;
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
}

static tokenInfo *dupToken (tokenInfo *const token)
{
	tokenInfo *newToken = newToken ();
	copyToken (newToken, token);
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
		copyToken (token, ReplayToken);
		deleteToken (ReplayToken);
		ReplayToken = NULL;
		char *tmp = ReplayCapture? vStringValue (ReplayCapture) : "";
		tmp = *tmp == ' '? tmp + 1 : tmp; // replay can include a space
		vDebugParserPrintf ("*%s[%s]", tokenNames[token->type], tmp);
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
	vStringDelete (token->string);
	token->string = NULL;

	if (capture && c != EOF &&
	    (isDigit (c) || isIdentInitial (c) || isOneOf (c, "-+'\\*\"`")) &&
	    (LastTokenType == TOKEN_IDENT || LastTokenType == TOKEN_KEYWORD ||
	     LastTokenType == TOKEN_TYPE || LastTokenType == TOKEN_COMMA))
		vStringPut (capture, ' ');

#ifdef DEBUG
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
	else if (c == '#' && getcFromInputFileIfEq ('[', capture))
		token->type = TOKEN_OPEN_SQUARE; // #[
	else if (isDigit (c) ||
	         (isOneOf (c, "-+.") && isDigit (peekcFromInputFile ())))
	{
		bool more;
		token->type = TOKEN_IMMEDIATE;
		token->string = vStringNew ();
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
		else if (isOneOf (c, "+-"))
			token->type = TOKEN_PLUSMINUS; // + -
		else if (c == '&')
			token->type = TOKEN_AMPERSAND; // &
		else if (c == '|')
			token->type = TOKEN_PIPE; // |
		else
			token->type = TOKEN_OPERATOR; // / % ^
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
		else if (c == '<' && getcFromInputFileIfEq ('-', capture))
			token->type = TOKEN_CHPOP; // <-
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
		token->string = vStringNew ();
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
		token->string = vStringNew ();
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
		token->string = vStringNew ();
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
			else if (initChar == 'r' && vStringLength (token->string) == 1 &&
			         isOneOf (peekcFromInputFile (), "'\"`"))
			{
				int end = getcFromInputFile ();
				vStringPut (capture, end);
				do
				{
					c = getcFromInputFile ();
					vStringPut (capture, c);
				}
				while (c != end);
				token->type = TOKEN_IMMEDIATE; // raw string
			}
			else
				token->type = TOKEN_IDENT;
		}
	}
	else
	{
		vDebugPrintf ("\nUNRECOGNISED CHAR (%s:%lu): %c (%u)\n",
		              getInputFileName (), token->lineNumber, c, c);
	}

	LastTokenType = token->type;

#ifdef DEBUG
	vDebugParserPrintf ("%s[%s]", tokenNames[token->type],
	                    vStringValue (capture));
	if (oldCapture)
		vStringCat (oldCapture, capture);
	vStringDelete (capture);
#endif
}

static void unreadTokenFull (tokenInfo *const token, vString *const acc)
{
	vDebugParserPrintf ("#");
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
                      int lineNum, int nTypes, ...)
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
			if (token->type == TOKEN_KEYWORD)
				got = vStringNewInit ("KEYWORD");
			else
			{
				got = vStringNewInit ("TOKEN (");
				vStringCatS (got, tokenNames[token->type]);
				vStringPut (got, ')');
			}
			if (!keyword)
			{
				exp = vStringNewInit (" (");
				bool addSpace = true;
				for (int i = 0; i < nTypes; i++)
				{
					tokenType type = va_arg (argp, tokenType);
					if (i > 0)
						vStringCatS (exp, ", ");
					else if (type == TOKEN_NONE)
					{
						addSpace = false;
						break;
					}
					else
						vStringCatS (exp, "expected: ");
					vStringCatS (exp, tokenNames[type]);
				}
				if (addSpace)
					vStringPut (exp, ' ');
				char buf[20];
				sprintf (buf, "%d", lineNum);
				vStringCatS (exp, "at v.c:");
				vStringCatS (exp, buf);
				vStringPut (exp, ')');
			}
			else if (token->type == TOKEN_KEYWORD)
				exp = vStringNewInit (" (expected different keyword)");
			else
				exp = vStringNewInit (" (expected KEYWORD)");
			if (token->type == TOKEN_KEYWORD)
				for (int i = 0; i < nTypes; i++)
					found = found || (token->keyword == va_arg (argp, keywordId));
			vDebugPrintf (
				"\nUNEXPECTED %s in {%s} at %s:%lu%s\n",
				vStringValue (got), CurrentParser, getInputFileName (),
				token->lineNumber, vStringValue (exp));
			vStringDelete (got);
			vStringDelete (exp);
			va_end (argp);
		}
	);
	// Although we don't show a debug parser error, we always return failure for
	// expectToken() on a keyword token, so that the call can be chained to an
	// expectKeyword() call to check which keyword it is. E.g.:
	//     expectToken (token, ..., TOKEN_KEYWORD) || expectKeyword (token, ...)
	// Note that isToken() is not affected by this special case.
	return (expected && !keyword && token->type == TOKEN_KEYWORD)? false : found;
}

static int makeTagFull (tokenInfo *const token, const char *name,
                        const kindType kind, const int scope, int role,
                        vString *const argList, vString *retType,
                        const char *access)
{
	tagEntryInfo e;

	Assert (name || token->string);
	const char *const tagName =
		name? name : (token->string? vStringValue (token->string) : "");
	Assert (tagName && tagName[0] != '\0');
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

// NOTE: over-reads!
static void skipAccessAndReadToken (tokenInfo *const token, vString **capture)
{
	while (true)
	{
		const char *add = NULL;
		if (isKeyword (token, KEYWORD_mut))
			add = "mut";
		else if (isKeyword (token, KEYWORD_pub))
			add = "pub";
		else if (isKeyword (token, KEYWORD___global))
			add = "__global";
		if (!add)
			break;
		if (capture)
		{
			if (*capture == NULL)
				*capture = vStringNew ();
			else if(!vStringIsEmpty(*capture))
				vStringPut (*capture, ' ');
			vStringCatS (*capture, add);
		}
		readToken (token);
	}
}

static char *nextCharIdent (char *s, char sep)
{
	if (s)
	{
		while (*s != sep && *s != '\0')
			s++;
		if (*s == sep)
			*(s++) = '\0';
		while (*s == ' ')
			s++;
		if (*s == '\0')
			s = NULL;
	}
	return s;
}

static int lookupName (vString *name, int scope)
{
	vString *tmp = vStringNewCopy (name);
	char *last, *part = vStringValue (tmp);
	int origScope = scope;
	unsigned int nkinds = sizeof (vLookupKinds) / sizeof (kindType);
	while (part)
	{
		last = part;
		part = nextCharIdent (part, '.');
		if (part && *last != '\0') // has one more (i.e. don't lookup last part)
			scope = anyKindsEntryInScope (
				scope, last, vLookupKinds, nkinds, true);
		if (scope == CORK_NIL)
			break;
	}
	if (scope != CORK_NIL)
		vStringCopyS (name, last);
	vStringDelete (tmp);
	return scope == CORK_NIL? origScope : scope;
}

// _____________________________________________________________________________
//                                                                        PARSER

// Note: all parsers expect the caller to have already read the initial token
// prior to calling, but will not themselves over-read on exit. Parsers will
// unread the last token, if is not within their purview.

// fq: [ident | type] ['.' [ident | type | kwtype]]*
// on return, token->type is IDENT/TYPE and ->string is f.q.name if capturing
static void parseFullyQualified (tokenInfo *const token, bool captureInToken)
{
	Assert (isToken (token, TOKEN_IDENT, TOKEN_TYPE));
	if (token->fullyQualified)
		return;
	PARSER_PROLOGUE ("fq");

	tokenType prev = TOKEN_DOT;
	vString *acc = NULL;
	if (captureInToken)
	{
		acc = token->string;
		token->string = NULL;
	}
	while ((isToken (token, TOKEN_DOT, TOKEN_IDENT, TOKEN_TYPE) ||
		       isKeyword (token, KEYWORD_TYPE)) &&
	       isToken (token, TOKEN_DOT) != (prev == TOKEN_DOT))
	{
		prev = token->type;
		readTokenFull (token, acc);
	}
	unreadTokenFull (token, acc);

	// load token with type (and fq name)
    token->type = prev;
    token->fullyQualified = true;
    vStringDelete (token->string);
    token->string = acc;

    expectToken (token, TOKEN_TYPE, TOKEN_IDENT, TOKEN_KEYWORD) ||
	    expectKeyword (token, KEYWORD_TYPE); // should end on one of these

    PARSER_EPILOGUE ();
}

// fqident: [ident | type] fq
static void parseFQIdent (tokenInfo *const token, vString *const capture)
{
	parseFullyQualified (token, !!capture);
	expectToken (token, TOKEN_IDENT);
	if (capture && token->string)
		vStringCat (capture, token->string);
}

// init: '{' ['...' fqident]? [[immediate | ident] ':' expr]* '}'
static void parseInit (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_OPEN_CURLY));
	PARSER_PROLOGUE ("init");

	readToken (token);
	if (isToken (token, TOKEN_SLICE))
	{
		readToken (token);
		if (expectToken (token, TOKEN_IDENT, TOKEN_TYPE))
		{
			parseFQIdent (token, NULL);
			readToken (token);
		}
	}
	while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY))
	{
		if (isToken (token, TOKEN_COMMA))
			readToken (token);
		else if (isToken (token, TOKEN_IDENT, TOKEN_IMMEDIATE))
		{
			tokenInfo *tmpToken = newToken ();
			readToken (tmpToken);
			if (isToken (tmpToken, TOKEN_COLON))
				readToken (token);
			else
				unreadToken (tmpToken);
			parseExpression (token, scope, NULL);
			readToken (token);
			deleteToken (tmpToken);
		}
		else if (!isToken (token, TOKEN_CLOSE_CURLY))
		{
			parseExpression (token, scope, NULL);
			readToken (token);
		}
	}

	PARSER_EPILOGUE ();
}

// fncall: '(' list ')' err-cont
static void parseFnCall (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_OPEN_PAREN));
	PARSER_PROLOGUE ("fncall");

	tokenType close = getClose (token->type);
	readToken (token);
	if (!isToken (token, close))
	{
		parseExprList (token, scope, NULL);
		readToken (token);
	}
	if (expectToken (token, close))
	{
		readToken (token);
		parseExprCont (token, scope, true);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// chain: '.' [ident | type | kwtype] ['.' [ident | type | kwtype]]*
//     ['(' fncall]?
static void parseChain (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_DOT));
	PARSER_PROLOGUE ("chain");

	readToken (token);
	if (expectToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_DOT, TOKEN_KEYWORD) ||
	    expectKeyword (token, KEYWORD_TYPE))
	{
		tokenType last;
		do
		{
			last = isToken (token, TOKEN_DOT);
			readToken (token);
		}
		while ((isToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_DOT) ||
		        isKeyword (token, KEYWORD_TYPE)) &&
		       isToken (token, TOKEN_DOT) != last);
		if (isToken (token, TOKEN_DOT))
			unreadToken (token);
		else if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else
			parseExprCont (token, scope, false);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// block: '{' statement* '}' ['.' chain]?
static void parseBlock (tokenInfo *const token, int scope, bool hasChain)
{
	Assert (isToken (token, TOKEN_OPEN_CURLY));
	PARSER_PROLOGUE ("block");

	tokenType close = getClose (token->type);
	do
	{
		readToken (token);
		if (isToken (token, close))
			break;
		else
			parseStatement (token, scope);
	}
	while (!isToken (token, TOKEN_EOF));
	if (hasChain)
	{
		readToken (token);
		if (isToken (token, TOKEN_DOT))
			parseChain (token, scope);
		else
			unreadToken (token);
	}

	PARSER_EPILOGUE ();
}

// declare: ':=' list
static void parseDeclare (tokenInfo *const token, vString *const names,
                          int scope, vString *const accesses)
{
	Assert (isToken (token, TOKEN_DECLARE, TOKEN_ASSIGN));
	PARSER_PROLOGUE ("decl");

	if (isToken (token, TOKEN_DECLARE))
	{
		char *name = names? vStringValue (names) : NULL;
		char *access = accesses? vStringValue (accesses) : NULL;
		while (name)
		{
			char *n = name, *a = access;
			name = nextCharIdent (name, ',');
			access = nextCharIdent (access, ',');
			makeTagFull (token, n, KIND_VARIABLE, CORK_NIL,
			             ROLE_DEFINITION_INDEX, NULL, NULL, a);
		}
	}
	readToken (token);
	parseExprList (token, scope, NULL);

	PARSER_EPILOGUE ();
}

// if: 'if' list '?'? block ['else' [block | if]]?
static void parseIf (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_if, KEYWORD_Sif));
	PARSER_PROLOGUE ("if");

	readToken (token);
	parseExprList (token, scope, NULL);
	readToken (token);
	if (isToken (token, TOKEN_QUESTION))
		readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, scope, false);
		readToken (token);
		if (isKeyword (token, KEYWORD_else, KEYWORD_Selse))
		{
			readToken (token);
			if (isToken (token, TOKEN_OPEN_CURLY))
				parseBlock (token, scope, false);
			else if (isKeyword (token, KEYWORD_if, KEYWORD_Sif))
				parseIf (token, scope);
			else
			{
				expectToken (token, TOKEN_OPEN_CURLY, TOKEN_KEYWORD) ||
					expectKeyword (token, KEYWORD_if, KEYWORD_Sif);
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

// fn(NONE): 'fn' [fqident | kwtype | clsr-args]? args vtype? block
// fn(FUNCTION): 'fn' receiver? [fqident | kwtype] tmpl-args? args vtype? block
// fn(METHOD): [fqident | kwtype] args vtype?
static void parseFn (tokenInfo *const token, int scope, vString *const access,
                     kindType kind)
{
	Assert (kind == KIND_FUNCTION || kind == KIND_METHOD || kind == KIND_NONE);
	Assert (kind == KIND_METHOD || isKeyword (token, KEYWORD_fn));
	PARSER_PROLOGUE (kind == KIND_FUNCTION? "fndef" :
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
				rxToken = dupToken (token);
				receiver = token->string;
				token->string = NULL;

				readTokenFull (token, acc);
				if (isToken (token, TOKEN_ASTERISK))
					readTokenFull (token, acc);
				if (!isToken (token, TOKEN_CLOSE_PAREN))
				{
					parseVType (token, acc, CORK_NIL, false);
					readTokenFull (token, acc);
				}
			}
		}
		if (expectToken (token, TOKEN_CLOSE_PAREN))
		{
			vStringAccumulate (argList, acc);
			readToken (token);
		}
		else
			skipToClose (TOKEN_OPEN_PAREN, NULL);
	}
	// name
	if ((kind == KIND_NONE && (
		     isToken (token, TOKEN_IDENT, TOKEN_TYPE) ||
		     isKeyword (token, KEYWORD_TYPE))) ||
	    (kind == KIND_FUNCTION && (
		    expectToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_KEYWORD) ||
		    expectKeyword (token, KEYWORD_TYPE))))
	{
		if (isKeyword (token, KEYWORD_TYPE))
		{
			name = token->string;
			token->string = NULL;
		}
		else
		{
			name = vStringNew ();
			parseFQIdent (token, name);
		}
		deleteToken (fnToken);
		fnToken = dupToken (token);
		// lookup parent scope
		scope = lookupName (name, scope);
		readToken (token);
	}
	// closure/template args
	if (isToken (token, TOKEN_OPEN_SQUARE) &&
	    (kind == KIND_FUNCTION || (kind == KIND_NONE && name == NULL)))
	{
		vStringPut (argList, '[');
 		skipToClose (token->type, acc);
		vStringAccumulate (argList, acc);
		readToken (token);
	}
	if (expectToken (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (argList, '(');
		skipToClose (token->type, acc);
		vStringAccumulate (argList, acc);
		readTokenFull (token, acc);
		// type
		if (isTypeInitial (token))
		{
			parseVType (token, acc, scope, false);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		else if (isToken (token, TOKEN_OPEN_PAREN))
		{
			skipToClose (token->type, acc);
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
			parseBlock (token, newScope, false);
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

// chpop: <- ident ['or' block]?
static void parseChanPop (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_CHPOP));
	PARSER_PROLOGUE ("chpop");

	readToken (token);
	if (expectToken (token, TOKEN_IDENT))
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_or))
		{
			readToken (token);
			if (expectToken (token, TOKEN_OPEN_CURLY))
				parseBlock (token, scope, true);
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

// match: 'match' access* fqident cont '{' [
//     [[vtype | expr] [',' [vtype | expr]]* | 'else'] block
// ]* '}'
static void parseMatch (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_match));
	PARSER_PROLOGUE ("match");

	vString *access = NULL;
	readToken (token);
    skipAccessAndReadToken (token, &access);
    if (expectToken (token, TOKEN_TYPE, TOKEN_IDENT))
    {
	    parseFQIdent (token, NULL);
	    readToken (token);
	    parseExprCont (token, scope, false);
	    readToken (token);
	    if (expectToken (token, TOKEN_OPEN_CURLY))
	    {
		    readToken (token);
		    do // match clauses
		    {
			    while (true) // match clause comma-list
			    {
				    if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
					    parseFullyQualified (token, true);

				    if (isToken (token, TOKEN_OPEN_CURLY, TOKEN_CLOSE_CURLY))
					    break;
				    else if (isTypeInitial (token))
					    parseVType (token, NULL, scope, false);
				    else if (isKeyword (token, KEYWORD_else))
				    {
					    readToken (token);
					    break;
				    }
				    else
					    parseExpression (token, scope, NULL);
				    readToken (token);
				    if (!isToken (token, TOKEN_COMMA))
					    break;
				    readToken (token);
			    }
			    if (expectToken (token, TOKEN_OPEN_CURLY))
			    {
				    parseBlock (token, scope, false);
				    readToken (token);
			    }
			    else
				    break;
		    }
		    while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY));
		    if (!expectToken (token, TOKEN_CLOSE_CURLY))
			    skipToClose (TOKEN_OPEN_CURLY, NULL);
	    }
    }

    PARSER_EPILOGUE ();
}

// for: 'for' access* [
//     ident [[',' access? ident]? 'in' expr | expr [';' expr]*]
// ]? block
static void parseFor (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_for, KEYWORD_Sfor));
	PARSER_PROLOGUE ("for");

	tokenInfo *var1Token = NULL;
	tokenInfo *var2Token = NULL;
	vString *var1Access = NULL;
	vString *var2Access = NULL;
	readToken (token);
	skipAccessAndReadToken (token, &var1Access);
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
				skipAccessAndReadToken (token, &var2Access);
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
				parseExpression (token, scope, NULL);
				readToken (token);
			}
		}
		else
		{
			unreadToken (token);
			copyToken (token, var1Token);
			while (true)
			{
				parseExprList (token, scope, var1Access);
				readToken (token);
				if (!isToken (token, TOKEN_SEMICOLON))
					break;
				readToken (token);
			}
		}
	}
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope, false);
	else
		unreadToken (token);
	deleteToken (var1Token);
	deleteToken (var2Token);
	vStringDelete (var1Access);
	vStringDelete (var2Access);

	PARSER_EPILOGUE ();
}

// import: 'import' fqident ['as' ident]? ['{' ident [',' ident]* '}']?
static void parseImport (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_import));
	PARSER_PROLOGUE ("import");

	readToken (token);
	if (expectToken (token, TOKEN_IDENT))
	{
		tokenInfo *moduleToken = dupToken (token);
		vString *moduleName = vStringNew ();
		parseFQIdent (token, moduleName);
		readToken (token);
		if (isKeyword (token, KEYWORD_as))
		{
			readToken (token);
			if (expectToken (token, TOKEN_IDENT))
			{
				moduleName = token->string;
				token->string = NULL;
			}
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

// struct: kw type? ['[' any ']']? '{' [access* fqtype]? [
//     access* ident method |
//     access* ident vtype ['[' any ']']? ['=' expr]?
// ]* '}'
static void parseStruct (tokenInfo *const token, vString *const access,
                         int scope, kindType kind)
{
	Assert (kind == KIND_STRUCT || kind == KIND_INTERFACE || kind == KIND_NONE);
	Assert (isKeyword (token, KEYWORD_struct, KEYWORD_interface));
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
			registerEntry (newScope);
			readToken (token);

			if (isToken (token, TOKEN_OPEN_SQUARE)) // template args
			{
				skipToClose (token->type, NULL);
				readToken (token);
			}
		}
		if (expectToken (token, TOKEN_OPEN_CURLY))
		{
			bool initial = true;
			vString *fieldAccess = NULL;
			while (true)
			{
				vString *tmp = NULL;
				readToken (token);
				skipAccessAndReadToken (token, &tmp);
				if (tmp)
				{
					vStringDelete (fieldAccess);
					fieldAccess = tmp;
					if(expectToken (token, TOKEN_COLON))
						readToken (token);
				}

				if ((initial && expectToken (token, TOKEN_TYPE, TOKEN_IDENT,
				                             TOKEN_CLOSE_CURLY)) ||
					(!initial && expectToken (token, TOKEN_IDENT,
					                          TOKEN_CLOSE_CURLY)))
				{
					if (isToken (token, TOKEN_CLOSE_CURLY))
						break;

					tokenInfo *fieldToken = dupToken (token);

					if (initial)
					{
						initial = false;
						if (isToken (token, TOKEN_IDENT, TOKEN_TYPE))
							parseFullyQualified (token, NULL);
						if (isToken (token, TOKEN_TYPE))
						{
							deleteToken (fieldToken);
							continue; // embedded struct
						}
					}

					readToken (token);
					if (kind == KIND_INTERFACE &&
					    isToken (token, TOKEN_OPEN_PAREN))
					{
						unreadToken (token);
						parseFn (fieldToken, newScope, fieldAccess, KIND_METHOD);
					}
					else
					{
						parseVType (token, NULL, newScope, false);
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
							parseExpression (token, newScope, NULL);
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

// enum: 'enum' type ['as' type]? '{' [ident ['=' immediate]?]* '}'
static void parseEnum (tokenInfo *const token, vString *const access, int scope)
{
	Assert (isKeyword (token, KEYWORD_enum));
	PARSER_PROLOGUE ("enum");

	readToken (token);
	if (expectToken (token, TOKEN_TYPE)) {
		int newScope = makeTagEx (token, NULL, KIND_ENUMERATION, scope, access);
		registerEntry (newScope);
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

// vtype: ['!' | '?' | '[' any ']' | '&']* [
//    fqtype ['[' any ']']? | 'chan' vtype | 'map' '[' any* ']' vtype | kwtype |
//    '$map' | '$struct' | '$enum' | '$array' | '$sumtype' | '$alias' |
//    'struct' struct
// ] init?
static void parseVType (tokenInfo *const token, vString *const capture,
                         int scope, bool hasInit)
{
	PARSER_PROLOGUE ("vtype");

	while (true)
	{
		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION, TOKEN_AMPERSAND))
			;
		else if (isToken (token, TOKEN_OPEN_SQUARE))
			skipToClose (TOKEN_OPEN_SQUARE, capture);
		else
			break;
		readTokenFull (token, capture);
	}

	if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
	{
		parseFullyQualified (token, capture);
		expectToken (token, TOKEN_TYPE);

		readTokenFull (token, capture);
		if (isToken (token, TOKEN_OPEN_SQUARE))
			skipToClose (token->type, capture);
		else
			unreadTokenFull (token, capture);
	}
	else if (isKeyword (token, KEYWORD_TYPE, KEYWORD_Smap, KEYWORD_Sstruct,
	                    KEYWORD_Senum, KEYWORD_Sarray, KEYWORD_Ssumtype,
	                    KEYWORD_Salias))
		;
	else if (isKeyword (token, KEYWORD_chan))
	{
		readTokenFull (token, capture);
		parseVType (token, capture, scope, false);
	}
	else if (isKeyword (token, KEYWORD_map))
	{
		readTokenFull (token, capture);
		if (expectToken (token, TOKEN_OPEN_SQUARE))
		{
			skipToClose (TOKEN_OPEN_SQUARE, capture);
			readTokenFull (token, capture);
			parseVType (token, capture, scope, false);
		}
		else
			unreadTokenFull (token, capture);
	}
	else if (isKeyword (token, KEYWORD_struct))
		parseStruct (token, NULL, scope, KIND_NONE);
	else
		unreadTokenFull (token, capture);

	if (hasInit)
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_CURLY))
			parseInit (token, scope);
		else
			unreadToken (token);
	}

	PARSER_EPILOGUE ();
}

// alias: 'type' type '=' vtype ['|' vtype]*
static void parseAlias (tokenInfo *const token, vString *const access, int scope)
{
	Assert (isKeyword (token, KEYWORD_type));
	PARSER_PROLOGUE ("alias");

	readToken (token);
	if (expectToken (token, TOKEN_TYPE))
	{
		int newScope = makeTagEx (token, NULL, KIND_ALIAS, scope, access);
		registerEntry (newScope);
		readToken (token);
		if (expectToken (token, TOKEN_ASSIGN))
		{
			readToken (token);
			parseVType (token, NULL, scope, false);

			while (true)
			{
				readToken (token);
				if (isToken (token, TOKEN_PIPE))
				{
					readToken (token);
					parseVType (token, NULL, scope, false);
					continue;
				}
				else
					unreadToken (token);
				break;
			}
		}
	}

	PARSER_EPILOGUE ();
}

static void parseExprList (tokenInfo *const token, int scope,
                           vString *const access)
{
	PARSER_PROLOGUE ("list");

	// attempt to consume multi-var declaration (special case)
	bool consumedIdent = true;
	if (isToken (token, TOKEN_IDENT))
	{
		tokenInfo *identToken = dupToken (token);
		vString *accesses = accesses = access? access : vStringNew ();
		vString *idents = token->string;
		token->string = NULL;
		readToken (token);
		if (isToken (token, TOKEN_COMMA))
		{
			do
			{
				vStringPut (accesses, ',');
				vStringPut (idents, ',');
				readToken (token);
				skipAccessAndReadToken (token, &accesses);
				if (!isToken (token, TOKEN_IDENT))
					consumedIdent = false;
				else
				{
					deleteToken (identToken);
					identToken = dupToken (token);
					vStringCat (idents, token->string);
					readToken (token);
				}
			}
			while (consumedIdent && isToken (token, TOKEN_COMMA));
		}
		if (consumedIdent)
		{
			if (isToken (token, TOKEN_DECLARE))
				parseDeclare (token, idents, scope, accesses);
			else if (isToken (token, TOKEN_COLON))
			{
				readToken (token);
				consumedIdent = false;
			}
			else
			{
				unreadToken (token);
				parseFullyQualified (identToken, false);
				readToken (token);
				parseExprCont (token, scope, false);
			}
		}
		deleteToken (identToken);
		vStringDelete (idents);
		if (!access)
			vStringDelete (accesses);
	}
	else
		parseExpression (token, scope, NULL);

    while (true)
	{
		if (!consumedIdent)
			consumedIdent = true;
		else
		{
			readToken (token);
			if (!isToken (token, TOKEN_COMMA))
				break;
			readToken (token);
			skipAccessAndReadToken (token, NULL);
		}
		if (isToken (token, TOKEN_EOF, TOKEN_CLOSE_PAREN, TOKEN_CLOSE_SQUARE,
		             TOKEN_CLOSE_CURLY))
			break;
		parseExpression (token, scope, NULL);
	}
    unreadToken (token);

	PARSER_EPILOGUE ();
}

// err: ['!' | '?' | 'or' block]?
// cont: ['--' | '++']? [
//     '.' chain | op expr | '...' expr? | ['is' | 'as'] vtype cont |
//     '[' expr ']' err-cont | ['=' ':='] expr | '(' fncall
// ]?
static void parseExprCont (tokenInfo *const token, int scope, bool hasErr)
{
	if (hasErr)
	{
		PARSER_PROLOGUE ("err");

		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
			readToken (token);
		else if (isKeyword (token, KEYWORD_or))
		{
			readToken (token);
			if (expectToken (token, TOKEN_OPEN_CURLY))
			{
				parseBlock (token, scope, true);
				readToken (token);
			}
		}

		PARSER_EPILOGUE ();
	}

	if (true)
	{
		PARSER_PROLOGUE ("cont");

		if (isToken (token, TOKEN_INCDECOP))
			readToken (token);
		if (isToken (token, TOKEN_DOT))
			parseChain (token, scope);
		else if (isOperator (token))
		{
			readToken (token);
			parseExpression (token, scope, NULL);
		}
		else if (isToken (token, TOKEN_SLICE))
		{
			readToken (token);
			if (!isClose (token))
				parseExpression (token, scope, NULL);
			else
				unreadToken (token);
		}
		else if (isKeyword (token, KEYWORD_is, KEYWORD_as))
		{
			readToken (token);
			parseVType (token, NULL, scope, false);
			readToken (token);
			parseExprCont (token, scope, false);
		}
		else if (isToken (token, TOKEN_OPEN_SQUARE))
		{
			readToken (token);
			if (!isClose (token))
			{
				parseExpression (token, scope, NULL);
				readToken (token);
			}
			if (expectToken (token, TOKEN_CLOSE_SQUARE))
			{
				readToken (token);
				if (!isClose (token))
					parseExprCont (token, scope, true);
				else
					unreadToken (token);
			}
			else
				unreadToken (token);
		}
		else if (isToken (token, TOKEN_DECLARE, TOKEN_ASSIGN))
		{
			expectToken (token, TOKEN_ASSIGN);
			readToken (token);
			parseExprList (token, scope, NULL);
		}
		else if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else
			unreadToken (token); // put back token that we didn't read!

		PARSER_EPILOGUE ();
	}
}

static void parseExpression (tokenInfo *const token, int scope,
                             vString *const access)
{
	PARSER_PROLOGUE ("expr");

	skipAccessAndReadToken (token, NULL);

	if (isToken (token, TOKEN_IDENT, TOKEN_TYPE))
		parseFullyQualified (token, true);

	if (isToken (token, TOKEN_IDENT))
	{
		tokenInfo *identToken = dupToken (token);
		readToken (token);
		if (isToken (token, TOKEN_COLON))
		{
			readToken (token);
			if (!isClose (token))
				parseExpression (token, scope, NULL);
			else
				unreadToken (token);
		}
		else
			parseExprCont (token, scope, false);
        deleteToken (identToken);
	}
	else if (isToken (token, TOKEN_IMMEDIATE, TOKEN_DOT))
	{
		if (isToken (token, TOKEN_DOT)) // enumerations
		{
			readToken (token);
			if (!expectToken (token, TOKEN_IDENT) && isClose (token))
				unreadToken (token);
		}
		readToken (token);
		if (!isClose (token))
			parseExprCont (token, scope, false);
		else
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_TYPE))
	{
		parseVType (token, NULL, scope, true);
		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN) ||
		    isKeyword (token, KEYWORD_is, KEYWORD_in))
			parseExprCont (token, scope, false);
		else
			unreadToken (token);
	}
	else if (isKeyword (token, KEYWORD_fn))
		parseFn (token, scope, NULL, KIND_NONE);
	else if (isKeyword (token, KEYWORD_if, KEYWORD_Sif))
		parseIf (token, scope);
	else if (isKeyword (token, KEYWORD_match))
		parseMatch (token, scope);
	else if (isToken (token, TOKEN_OPEN_CURLY))
		parseInit (token, scope);
	else if (isKeyword (token, KEYWORD_chan, KEYWORD_map))
		parseVType (token, NULL, scope, true);
	else if (isToken (token, TOKEN_CHPOP))
		parseChanPop (token, scope);
	else if (isKeyword (token, KEYWORD_unsafe))
	{
		readToken (token);
		parseBlock (token, scope, false);
	}
	else if (isToken (token, TOKEN_TILDE, TOKEN_EXCLAMATION, TOKEN_QUESTION,
	                  TOKEN_ASTERISK, TOKEN_AMPERSAND, TOKEN_PLUSMINUS))
	{
		readToken (token);
		parseExpression (token, scope, NULL);
	}
	else if (isKeyword (token, KEYWORD_TYPE))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_OPEN_PAREN))
	{
		readToken (token);
		if (!isClose (token))
		{
			parseExpression (token, scope, NULL);
			readToken (token);
		}
		else
			expectToken (token, TOKEN_NONE);
		if (expectToken (token, TOKEN_CLOSE_PAREN))
		{
			readToken (token);
			if (!isClose (token))
				parseExprCont (token, scope, false);
			else
				unreadToken (token);
		}
		else
			skipToClose (TOKEN_OPEN_PAREN, NULL);
	}
	else if (isToken (token, TOKEN_OPEN_SQUARE))
	{
		tokenInfo *tmpToken = newToken (); // []type or [x,x,x]?
		readToken (tmpToken);
		unreadToken (tmpToken);
		if (isToken (tmpToken, TOKEN_CLOSE_SQUARE))
			parseVType (token, NULL, scope, true);
		else
		{
			readToken (token);
			parseExprList (token, scope, NULL);
			readToken (token);
			if (!expectToken (token, TOKEN_CLOSE_SQUARE))
				unreadToken (token);
		}
		deleteToken (tmpToken);
	}
	else
		expectToken (token, NULL);

	PARSER_EPILOGUE ();
}

// stmt: [
//     'for' for | 'return' list? | 'defer' block | 'assert' expr |
//     'break' ident? | 'asm' '{' any '}' | expr
// ]
static void parseStatement (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("stmt");

	if (isKeyword (token, KEYWORD_for, KEYWORD_Sfor))
		parseFor (token, scope);
	else if (isKeyword (token, KEYWORD_return))
	{
		readToken (token);
		if (!isClose (token))
			parseExprList (token, scope, NULL);
		else
			unreadToken (token);
	}
	else if (isKeyword (token, KEYWORD_defer))
	{
		readToken (token);
		if (expectToken (token, TOKEN_OPEN_CURLY))
			parseBlock (token, scope, false);
		else
			unreadToken (token);
	}
	else if (isKeyword (token, KEYWORD_assert))
	{
		readToken (token);
		parseExpression (token, scope, NULL);
	}
	else if (isKeyword (token, KEYWORD_continue, KEYWORD_break))
	{
		readToken (token);
		if (!isToken (token, TOKEN_IDENT))
			unreadToken (token);
	}
	else if (isKeyword (token, KEYWORD_asm))
	{
		readToken (token);
		if (expectToken (token, TOKEN_OPEN_CURLY))
			skipToClose (token->type, NULL);
	}
	else if (isToken (token, TOKEN_IDENT))
	{
		tokenInfo *tmpToken = dupToken (token);
		readToken (token);
		if (isToken (token, TOKEN_COLON))
			makeTag (tmpToken, NULL, KIND_LABEL, scope);
		else
		{
			unreadToken (token);
			copyToken (token, tmpToken);
			parseExprList (token, scope, NULL);
		}
		deleteToken (tmpToken);
	}
	else
	{
		vString *access = NULL;
		skipAccessAndReadToken (token, &access);
		parseExprList (token, scope, access);
		vStringDelete (access);
	}

	PARSER_EPILOGUE ();
}

// constexpr: ident '=' expr
static void parseConstExpr (tokenInfo *const token, vString *const access)
{
	PARSER_PROLOGUE ("constexpr");

	makeTagEx (token, NULL, KIND_CONST, CORK_NIL, access);
	readToken (token);
	if (expectToken (token, TOKEN_ASSIGN))
	{
		readToken (token);
		parseExpression (token, CORK_NIL, NULL);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// cont: 'const' [constexpr | '(' constexpr* ')' ]
static void parseConst (tokenInfo *const token, vString *const access)
{
	Assert (isKeyword (token, KEYWORD_const));
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

// module: 'module' ident
static int parseModule (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_module));
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

// file: [access* [
//     ['(' any ')'] | ['{' any '}'] | 'module' module | 'fn' fn | 'enum' enum |
//     'import' import | 'const' const | 'struct' struct | 'interface' struct |
//     'type' alias
// ]]*
static void parseFile (tokenInfo *const token)
{
	PARSER_PROLOGUE ("file");

	int scope = CORK_NIL;
	vString *access = NULL;
	do
	{
		readToken (token);
		skipAccessAndReadToken (token, &access);

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
	vDebugParserPrintf ("\n");

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

#ifdef DEBUG
static bool vSetParserSpew (const langType language CTAGS_ATTR_UNUSED,
                            const char *optname CTAGS_ATTR_UNUSED,
                            const char *arg)
{
	parserSpew = arg && (arg[0] == 'y' || arg[0] == 'Y' || arg[0] == 't' ||
	                     arg[0] == 'T' || arg[0] == 'o' || arg[0] == 'O' ||
	                     arg[0] == '1');
	return true;
}

static paramDefinition VParams[] = {
	{
		.name = "parserSpew",
		.desc = "spews debug information about the parser to stderr",
		.handleParam = vSetParserSpew,
	},
};
#endif

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
#ifdef DEBUG
	def->paramTable = VParams;
	def->paramCount = ARRAY_SIZE (VParams);
#endif
	return def;
}
