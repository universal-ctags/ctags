/*
*   Copyright (c) 2023, Tim Marston
*
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
#include "selectors.h"

#define MAX_REPLAYS 2
#define _NARGS(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
#define nArgs(...) _NARGS (__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))
#define vStringAccumulate(a, s) \
	do{ if (a && s) { vStringCat (a, s); vStringClear (s); } }while(0)
#define isToken(token, ...) \
	_isToken (token,  false, false, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define expectToken(token, ...) \
	_isToken (token,  true, false, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define isKeyword(token, ...) \
	_isToken (token, false, true, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define expectKeyword(token, ...) \
	_isToken (token, true, true, __LINE__, nArgs (__VA_ARGS__), __VA_ARGS__)
#define isDigit(c) (c >= '0' && c <= '9')
#define isHexDigit(c) (isDigit (c) || \
					   (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
#define isInitialIdent(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || \
						   c == '_' || c == '$')
#define isSubsequentIdent(c) (isInitialIdent (c) || isDigit (c))
#define isOperator(t) ( \
		isToken (t, TOKEN_OPERATOR, TOKEN_PLUSMINUS, TOKEN_ANDAND, \
				 TOKEN_ASTERISK, TOKEN_AMPERSAND, TOKEN_CHPOP, TOKEN_PIPE) || \
		isKeyword (token, KEYWORD_in))
#define isInitialType(t) ( \
		isToken (t, TOKEN_TYPE, TOKEN_EXTERN, TOKEN_OPEN_SQUARE, \
				 TOKEN_QUESTION, TOKEN_EXCLAMATION, TOKEN_ASTERISK, \
				 TOKEN_AMPERSAND, TOKEN_ANDAND) || \
		isKeyword (t, KEYWORD_TYPE, KEYWORD_map, KEYWORD_chan, KEYWORD_Smap, \
				   KEYWORD_shared, KEYWORD_fn, KEYWORD_none))
#define isClose(t) ( \
		isToken (t, TOKEN_CLOSE_PAREN, TOKEN_CLOSE_SQUARE, TOKEN_CLOSE_CURLY))
#define unreadToken(t) (unreadTokenFull (t, NULL))
#define RED "\033[91m\033[1m"
#define NORM "\033[0m\n"

#ifdef DEBUG
#define vDebugPrintf(...) \
	do { if (debug (DEBUG_CPP)) fprintf (stderr, __VA_ARGS__); }while(0)
#define vDebugParserPrintf(...) \
	do { if (debug (DEBUG_OPTION)) fprintf(stderr, __VA_ARGS__); }while(0)
#define vDebugUnexpected(t, e, l) vDebugPrintf ( \
	RED "\nUNEXPECTED %s%s%s%s in {%s} at %s:%lu (v.c:%i)" NORM, \
	t->keyword != KEYWORD_NONE? "KEYWORD" : tokenNames[t->type], \
	(e)? " (expected " : "", e? e : "", (e)? ")" : "", \
	PS->currentParser, getInputFileName (), t->lineNumber, l? l : __LINE__)
#define PARSER_PROLOGUE(c) \
	const char *const _prevParser = PS->currentParser; \
	PS->currentParser = (c); \
	vDebugParserPrintf ("{%s: ", PS->currentParser)
#define PARSER_EPILOGUE() \
	vDebugParserPrintf (":%s} ", PS->currentParser); \
	PS->currentParser = _prevParser
#else
#define vDebugPrintf(...)
#define vDebugParserPrintf(...)
#define vDebugUnexpected(t, e, l)
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
	KEYWORD_static,
	KEYWORD___global,
	KEYWORD_defer,
	KEYWORD_unsafe,
	KEYWORD_go,
	KEYWORD_spawn,
	KEYWORD_for,
	KEYWORD_Sfor,
	KEYWORD_in,
	KEYWORD_continue,
	KEYWORD_break,
	KEYWORD_assert,
	KEYWORD_struct,
	KEYWORD_interface,
	KEYWORD_union,
	KEYWORD_is,
	KEYWORD_none,
	KEYWORD_enum,
	KEYWORD_type,
	KEYWORD_asm,
	KEYWORD_chan,
	KEYWORD_match,
	KEYWORD_select,
	KEYWORD_lock,
	KEYWORD_rlock,
	KEYWORD_shared,
	KEYWORD_Smap,
	KEYWORD_Sarray,
	KEYWORD_Ssumtype,
	KEYWORD_Salias,
	KEYWORD_Sstruct,
	KEYWORD_Senum,
	COUNT_KEYWORD,
	KEYWORD_TYPE,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

const static struct keywordGroup VTypeKeywords = {
	.value = KEYWORD_TYPE,
	.addingUnlessExisting = true,
	.keywords = {
		"voidptr", "byteptr", "charptr", "i8", "i16", "i32", "int", "i64",
		"byte", "u8", "u16", "u32", "u64", "f32", "f64", "char", "bool",
		"string", "array", "rune", "mapnode", "size_t", "usize", "isize",
		"$int", "float_literal", "int_literal", "thread", "any", "IError",
		"_option", "_result", NULL
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
	{"static", KEYWORD_static},
	{"__global", KEYWORD___global},
	{"defer", KEYWORD_defer},
	{"unsafe", KEYWORD_unsafe},
	{"go", KEYWORD_go},
	{"spawn", KEYWORD_spawn},
	{"for", KEYWORD_for},
	{"$for", KEYWORD_Sfor},
	{"in", KEYWORD_in},
	{"continue", KEYWORD_continue},
	{"break", KEYWORD_break},
	{"assert", KEYWORD_assert},
	{"struct", KEYWORD_struct},
	{"interface", KEYWORD_interface},
	{"union", KEYWORD_union},
	{"is", KEYWORD_is},
	{"none", KEYWORD_none},
	{"enum", KEYWORD_enum},
	{"type", KEYWORD_type},
	{"asm", KEYWORD_asm},
	{"chan", KEYWORD_chan},
	{"match", KEYWORD_match},
	{"select", KEYWORD_select},
	{"lock", KEYWORD_lock},
	{"rlock", KEYWORD_rlock},
	{"shared", KEYWORD_shared},
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
	TOKEN_TYPE, // V type ident (e.g., "Foo"), not KEYWORD_TYPE (e.g. "int")
	TOKEN_IMMEDIATE,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_DECLARE, // :=
	TOKEN_OPERATOR,
	TOKEN_PLUSMINUS, // + -
	TOKEN_ANDAND, // &&
	TOKEN_EXCLAMATION,
	TOKEN_QUESTION,
	TOKEN_DOT, // .
	TOKEN_COMMA,
	TOKEN_SLICE,
	TOKEN_ASSIGN, // =
	TOKEN_ASTERISK,
	TOKEN_AMPERSAND,
	TOKEN_TILDE,
	TOKEN_COLON,
	TOKEN_SEMICOLON,
	TOKEN_INCDECOP, // ++ --
	TOKEN_PIPE,
	TOKEN_CHPOP, // <-
	TOKEN_AT,
	TOKEN_LABEL, // ident-then-colon (e.g., "foo:")
	TOKEN_EXTERN, // symbol in exteral namespace (e.g., "C.foo" or "JS.Object")
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
	"PLUSMINUS",
	"ANDAND",
	"EXCLAMATION",
	"QUESTION",
	"DOT",
	"COMMA",
	"SLICE",
	"ASSIGN",
	"ASTERISK",
	"AMPERSAND",
	"TILDE",
	"COLON",
	"SEMICOLON",
	"INCDECOP",
	"PIPE",
	"CHPOP",
	"AT",
	"LABEL",
	"EXTERN",
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
	ROLE_EXTERNAL_SYMBOL,
	COUNT_EXTERN_ROLE
} VExternRole;

static roleDefinition VExternRoles [COUNT_EXTERN_ROLE] = {
	{ true, "external", "external symbol" },
};

typedef enum {
	KIND_NONE = -1,
	KIND_FUNCTION,
	KIND_MODULE,
	KIND_VARIABLE,
	KIND_CONST,
	//KIND_FNARG,
	KIND_RECEIVER,
	//KIND_CLSARG,
	KIND_LABEL,
	KIND_STRUCT,
	KIND_FIELD,
	KIND_METHOD,
	KIND_ENUMERATOR,
	KIND_ENUMERATION,
	KIND_ALIAS,
	KIND_INTERFACE,
	KIND_UNION,
	KIND_EXTERN,
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
	{true, 'u', "union", "union names"},
	{false, 'x', "external", "external symbols",
	 .referenceOnly = false, ATTACH_ROLES (VExternRoles)},
	{true, 'Y', "unknown", "unknown (imported) variables, types and functions",
	 .referenceOnly = false, ATTACH_ROLES (VUnknownRoles)},
};

static fieldDefinition VFields [] = {
	{
		.name = "module",
		.description = "the name of the module",
		.enabled = true,
	},
};

static int vLookupKinds[] = { // int, compatible with anyKindsEntryInScope()
	KIND_STRUCT,
	KIND_ENUMERATION,
	KIND_INTERFACE,
	KIND_UNION,
};
const static int vLookupNumKinds = sizeof (vLookupKinds) / sizeof (int);

typedef struct {
	int type;
	keywordId keyword;          /* when type is KEYWORD */
	vString *string;            /* when type is KEYWORD, IDENT or TYPE */
	bool fullyQualified;        /* when type is IDENT or TYPE */
	unsigned long lineNumber;	/* line number of tagName */
	MIOPos filePosition;		/* file position of line containing name */
	int captureLen;             /* num chars captured by token */
	bool onNewline;             /* token follows a \n (V is not ws agnostic!) */
#ifdef DEBUG
	int id;
#endif
} tokenInfo;

typedef struct {
	tokenType lastTokenType;
	int numReplays;
	struct {
		tokenInfo *token;
		vString *capture;
		tokenType lastTokenType;
	} replays[MAX_REPLAYS];
	//tokenInfo *replayTokens[MAX_REPLAYS];
	//vString *replayCaptures[MAX_REPLAYS];
	//tokenType replayLastTokenTypes[MAX_REPLAYS];
	bool isBuiltin;
	tokenInfo *sofToken;
	int externCorkIdC;
	int externCorkIdJS;
#ifdef DEBUG
	const char *currentParser;
	int nextTokenId;
#endif
} parserState;

static langType Lang_v;
static objPool *TokenPool = NULL;
static parserState *PS = NULL; // global parser state

static int lookupQualifiedName (vString *, int);
static void parseStatement (tokenInfo *const, int);
static bool parseExpression (tokenInfo *const, int, vString *const);
static void parseExprList (tokenInfo *const, int, vString *const, bool);
static bool parseExprCont (tokenInfo *const, int, bool);
static bool parseVType (tokenInfo *const, vString *const, int, bool, bool);

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->string = NULL;
#ifdef DEBUG
	token->id = PS->nextTokenId++;
#endif
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
	token->captureLen = -1;
	token->onNewline = false;
}

static void copyToken (tokenInfo *const dst, tokenInfo *const src)
{
	dst->type = src->type;
	dst->keyword = src->keyword;
	vStringDelete (dst->string);
	dst->string = src->string? vStringNewCopy (src->string) : NULL;
	dst->fullyQualified = src->fullyQualified;
	dst->lineNumber = src->lineNumber;
	dst->filePosition = src->filePosition;
	dst->captureLen = src->captureLen;
	dst->onNewline = src->onNewline;
}

static tokenInfo *dupToken (tokenInfo *const token)
{
	tokenInfo *newToken = newToken ();
	copyToken (newToken, token);
	return newToken;
}

static parserState *newParserState ()
{
	parserState *st = xMalloc (1, parserState);
	st->sofToken = NULL;
	st->numReplays = 0;
	st->isBuiltin = false;
	st->lastTokenType = TOKEN_IMMEDIATE;
	st->externCorkIdC = 0;
	st->externCorkIdJS = 0;
#ifdef DEBUG
	st->nextTokenId = 1;
#endif
	return st;
}

static void deleteParserState (parserState *st)
{
	deleteToken (st->sofToken);
	for (int i = 0; i < st->numReplays; i++)
	{
		deleteToken (st->replays[i].token);
		vStringDelete (st->replays[i].capture);
	}
	eFree (st);
}

// _____________________________________________________________________________
//                                                                     TOKENISER

static void captureChar (vString *capture, tokenInfo *token, char c)
{
	if (capture)
		vStringPut (capture, c);
	token->captureLen++;
}

static bool getcAndCaptureIfEq (int expect,
								vString *const capture, tokenInfo *token)
{
	int c = getcFromInputFile ();
	if (c != expect)
		ungetcToInputFile (c);
	else
		captureChar (capture, token, c);
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

static void checkLabel (tokenInfo *const token, vString *capture)
{
	int c = getcFromInputFile ();
	if (c == ':')
	{
		int d = getcFromInputFile ();
		ungetcToInputFile (d);
		if (d != '=')
		{
			token->type = TOKEN_LABEL;
			if (capture)
				vStringPut (capture, ':');
		}
		else
			ungetcToInputFile (c);
	}
	else
		ungetcToInputFile (c);
}

static void readTokenFull (tokenInfo *const token, vString *capture)
{
	int c;

	// replay?
	if (PS->numReplays > 0)
	{
		PS->numReplays--;
		copyToken (token, PS->replays[PS->numReplays].token);
		deleteToken (PS->replays[PS->numReplays].token);
		vDebugParserPrintf (
			"˅%s%s%s%s%s ", token->id > 1? "[" : "", tokenNames[token->type],
			(token->string && !vStringIsEmpty (token->string))? ":" : "",
			token->string? vStringValue (token->string) : "",
			token->id > 1? "]" : "");
		if (capture)
		{
			Assert (PS->replays[PS->numReplays].capture);
			vStringCat (capture, PS->replays[PS->numReplays].capture);
		}
		vStringDelete (PS->replays[PS->numReplays].capture);
		PS->lastTokenType = PS->replays[PS->numReplays].lastTokenType;
		return;
	}

	// skip whitespace and comments
	bool skippedNewline = false;
	do
	{
		c = getcFromInputFile ();
		if (isOneOf (c, "\r\n"))
			skippedNewline = true;
		else if (isOneOf (c, " \t"))
			;
		else if (isOneOf (c, "/#"))
		{
			int d = getcFromInputFile ();
			if (c == '/' && d == '*')
				skipInputFileTillCommentEnd (); // /*
			else if ((c == '/' && d == '/') || (c == '#' && d != '['))
			{
				skipInputFileTillEOL (); // // #flag #include
				skippedNewline = true;
			}
			else
			{
				ungetcToInputFile (d);
				break;
			}
		}
		else
			break;
	} while (true);

	// init token
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringDelete (token->string);
	token->string = NULL;
	token->captureLen = 0;
	token->onNewline = skippedNewline;
	token->keyword = KEYWORD_NONE;

	// start capture
	if (c != EOF &&
		(isDigit (c) || isInitialIdent (c) || isOneOf (c, "-+'\\*\"`")) &&
		(PS->lastTokenType == TOKEN_IDENT ||
		 PS->lastTokenType == TOKEN_KEYWORD ||
		 PS->lastTokenType == TOKEN_TYPE ||
		 PS->lastTokenType == TOKEN_COMMA))
		captureChar (capture, token, ' ');
	if (c != EOF)
		captureChar (capture, token, c);

	// match token...
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
	else if (c == '#' && getcAndCaptureIfEq ('[', capture, token))
		token->type = TOKEN_OPEN_SQUARE; // #[
	else if (isDigit (c) ||
			 (isOneOf (c, "-+.") && isDigit (peekcFromInputFile ())))
	{
		bool more;
		token->type = TOKEN_IMMEDIATE;
		do
		{
			c = getcFromInputFile ();
			more = isHexDigit (c) ||
				(isOneOf (c, ".-+xo_") && isHexDigit (peekcFromInputFile ())) ||
				(isOneOf (c, "eE") && (isDigit (peekcFromInputFile ()) ||
									   isOneOf (peekcFromInputFile (), "-+")));
			if (more)
				captureChar (capture, token, c);
		} while (more);
		ungetcToInputFile (c);
		checkLabel (token, capture);
	}
	else if (isOneOf (c, "+-*/%&|^"))
	{
		if (getcAndCaptureIfEq ('=', capture, token))
			token->type = TOKEN_ASSIGN; // += -= *= /= %= &= |= ^=
		else if (c == '*')
			token->type = TOKEN_ASTERISK; // *
		else if (c == '|' && getcAndCaptureIfEq (c, capture, token))
			token->type = TOKEN_OPERATOR; // ||
		else if (c == '&' && getcAndCaptureIfEq (c, capture, token))
			token->type = TOKEN_ANDAND; // &&
		else if (isOneOf (c, "+-") && getcAndCaptureIfEq (c, capture, token))
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
		if (getcAndCaptureIfEq ('=', capture, token))
			token->type = TOKEN_OPERATOR; // == !=
		else if (c == '=')
			token->type = TOKEN_ASSIGN; // =
		else
			token->type = TOKEN_EXCLAMATION; // !
	}
	else if (isOneOf (c, "<>"))
	{
		if (getcAndCaptureIfEq (c, capture, token))
		{
			if (c == '>')
				getcAndCaptureIfEq ('>', capture, token);
			if (getcAndCaptureIfEq ('=', capture, token))
				token->type = TOKEN_ASSIGN; // <<= >>= >>>=
			else
				token->type = TOKEN_OPERATOR; // << >> >>>
		}
		else if (c == '<' && getcAndCaptureIfEq ('-', capture, token))
			token->type = TOKEN_CHPOP; // <-
		else
		{
			getcAndCaptureIfEq ('=', capture, token);
			token->type = TOKEN_OPERATOR; // < > <= >=
		}
	}
	else if (c == '.')
	{
		if (getcAndCaptureIfEq ('.', capture, token))
		{
			getcAndCaptureIfEq ('.', capture, token);
			token->type = TOKEN_SLICE; // .. ...
		}
		else
			token->type = TOKEN_DOT; // .
	}
	else if (c == ':')
	{
		if (getcAndCaptureIfEq ('=', capture, token))
			token->type = TOKEN_DECLARE; // :=
		else
			token->type = TOKEN_COLON; // :
	}
	else if (isOneOf (c, "\"'`"))
	{
		int terminator = c;
		bool inEscape = false;
		bool inEval = false;
		int evalLevel = 0;
		token->type = TOKEN_IMMEDIATE;
		do
		{
			if (inEscape)
			{
				captureChar (capture, token, c);
				inEscape = false;
			}
			else if (inEval) // TODO: this doesn't handle nested strings/evals
			{
				if (c == '{')
					evalLevel++;
				else if (c == '}')
					evalLevel--;
				if (evalLevel == 0)
					inEval = false;
			}
			else if (c == '\\')
				inEscape = true;
			else if (c == '$')
				inEval = true;
			c = getcFromInputFile ();
			captureChar (capture, token, c);
		} while (c != EOF &&
				 (c != terminator || inEscape || (inEval && evalLevel > 0)));
		checkLabel (token, capture);
	}
	else if (c == '@')
	{
		if (isSubsequentIdent (peekcFromInputFile ()))
		{
			token->type = TOKEN_IDENT;
			token->string = vStringNew ();
			bool more;
			do
			{
				vStringPut (token->string, c);
				c = getcFromInputFile ();
				more = isSubsequentIdent (c);
				if (more)
					captureChar (capture, token, c);
			} while (more);
			ungetcToInputFile (c);
			checkLabel (token, capture);
		}
		else
			token->type = TOKEN_AT;
	}
	else if (isInitialIdent (c))
	{
		token->string = vStringNew ();
		bool more;
		do
		{
			vStringPut (token->string, c);
			c = getcFromInputFile ();
			more = isSubsequentIdent (c);
			if (more)
				captureChar (capture, token, c);
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
			else if (isOneOf (initChar, "rc") &&
					 vStringLength (token->string) == 1 &&
					 isOneOf (peekcFromInputFile (), "'\"`"))
			{
				int end = getcFromInputFile ();
				captureChar (capture, token, end);
				do
				{
					c = getcFromInputFile ();
					captureChar (capture, token, c);
				}
				while (c != end);
				vStringDelete (token->string);
				token->string = NULL;
				token->type = TOKEN_IMMEDIATE; // raw string
			}
			else
				token->type = TOKEN_IDENT;
		}

		if (token->type != TOKEN_KEYWORD)
			checkLabel (token, capture);
	}
	else
	{
		vDebugPrintf (RED "\nUNRECOGNISED CHAR at %s:%lu: %c (%u)" NORM,
					  getInputFileName (), token->lineNumber, c, c);
	}

	vDebugParserPrintf (
		"%s%s%s%s%s ", token->id > 1? "[" : "", tokenNames[token->type],
		(token->string && !vStringIsEmpty (token->string))? ":" : "",
		token->string? vStringValue (token->string) : "",
		token->id > 1? "]" : "");

	PS->lastTokenType = token->type;
}

static void unreadTokenFull (tokenInfo *const token, vString *const acc)
{
	Assert (PS->numReplays < MAX_REPLAYS);
	DebugStatement (
		// multiple-replay in only possible where additional tokenInfos are
		// loaded with tokens so different tokens can be unread.
		for (int i = 0; i < PS->numReplays; i++)
			Assert (token->id != PS->replays[i].token->id);
	);
	vDebugParserPrintf ("˄ ");

	PS->replays[PS->numReplays].token = dupToken (token);
	if (acc)
	{
		Assert (token->captureLen > -1);
		Assert (token->captureLen <= vStringLength (acc));
		int off = vStringLength (acc) - token->captureLen;
		PS->replays[PS->numReplays].capture =
			vStringNewInit (vStringValue (acc) + off);
		vStringTruncate (acc, off);
	}
	else
		PS->replays[PS->numReplays].capture = NULL;
	PS->replays[PS->numReplays].lastTokenType = PS->lastTokenType;
	PS->numReplays++;
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, NULL);
}

// _____________________________________________________________________________
//                                                                    HELPER FNS

static bool _isToken (tokenInfo *const token, bool expect, bool keyword,
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
		if (expect && !found)
		{
			if (!keyword)
			{
				vString *exp = NULL;
				va_start (argp, nTypes);
				for (int i = 0; i < nTypes; i++)				{
					tokenType type = va_arg (argp, tokenType);
					if (type == TOKEN_NONE)
						continue;
					else if (exp)
						vStringCatS (exp, ", ");
					else
						exp = vStringNew ();
					vStringCatS (exp, tokenNames[type]);
				}
				va_end (argp);
				vDebugUnexpected (token, exp? vStringValue (exp) : NULL, lineNum);
				vStringDelete (exp);
			}
			else if (token->type == TOKEN_KEYWORD)
				vDebugUnexpected (token, "other KEYWORD", lineNum);
			else
				vDebugUnexpected (token, "KEYWORD", lineNum);
		}
	);
	// Although we don't show a debug parser error, we always return failure for
	// expectToken() on a keyword token, so that the call can be chained to an
	// expectKeyword() call to check which keyword it is. E.g.:
	//     expectToken (token, ..., TOKEN_KEYWORD) || expectKeyword (token, ...)
	// Note that isToken() is not affected by this special case.
	return (expect && !keyword && token->type == TOKEN_KEYWORD)? false : found;
}

static int makeTagFull (tokenInfo *const token, const char *name,
						const kindType kind, const int scope, int role,
						vString *const argList, vString *retType,
						const char *access, bool extraQualified)
{
	Assert (token || kind == KIND_EXTERN);
	Assert (name == NULL || name[0] != '\0');
	Assert (name != NULL || (token && token->string != NULL &&
							 vStringLength (token->string) > 0));
	vDebugParserPrintf ("#%c ", VKinds[kind].letter);

	tagEntryInfo e;

	const char *const tagName =
		name? name : (token->string? vStringValue (token->string) : "");
	if (!strcmp (tagName, "_"))
		return CORK_NIL; // ignore _
	initRefTagEntry (&e, tagName, kind, role);

	e.lineNumber = token? token->lineNumber : 0;
	e.filePosition = (token? token : PS->sofToken)->filePosition;
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
	if (extraQualified)
		markTagExtraBit (&e, XTAG_QUALIFIED_TAGS);

	return makeTagEntry (&e);
}

static int makeFnTag (tokenInfo *const token, vString *const name,
					  kindType kind, int scope, vString *const argList,
					  vString *const retType, vString *const access)
{
	// lookup parent scope
	if (name)
		scope = lookupQualifiedName (name, scope);
	return makeTagFull (token, name? vStringValue (name) : NULL, kind,
						scope, ROLE_DEFINITION_INDEX, argList, retType,
						access? vStringValue (access) : NULL, false);
}

static int makeTag (tokenInfo *const token, vString *const name, kindType kind,
					int scope)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind, scope,
						ROLE_DEFINITION_INDEX, NULL, NULL, NULL, false);
}

static int makeTagEx (tokenInfo *const token, vString *const name, kindType kind,
					  int scope, vString *const access)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind, scope,
						ROLE_DEFINITION_INDEX, NULL, NULL,
						access? vStringValue (access) : NULL, false);
}

static int makeReferenceTag (tokenInfo *const token, vString *const name,
							 kindType kind, int scope, int role)
{
	return makeTagFull (token, name? vStringValue (name) : NULL, kind, scope,
						role, NULL, NULL, NULL, false);
}

static tokenType getOpen (tokenType close)
{
	switch (close)
	{
	case TOKEN_CLOSE_PAREN: return TOKEN_OPEN_PAREN;
	case TOKEN_CLOSE_SQUARE: return TOKEN_OPEN_SQUARE;
	case TOKEN_CLOSE_CURLY: return TOKEN_OPEN_CURLY;
	default: return TOKEN_NONE;
	}
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

static void skipToTokenFull (tokenType type, vString *const capture,
							 tokenInfo *token)
{
	if (type != TOKEN_NONE)
	{
		tokenType open =
			(type == TOKEN_CLOSE_PAREN || type == TOKEN_CLOSE_SQUARE ||
			 type == TOKEN_CLOSE_CURLY)? getOpen (type) : TOKEN_NONE;
		int nest = 1;
		do
		{
			readTokenFull (token, capture);
			if (open != TOKEN_NONE && isToken (token, open))
				nest++;
			else if (isToken (token, type))
				nest--;
			else if (isToken (token, TOKEN_EOF))
				break;
		} while (nest > 0);
	}
}

static void skipToToken (tokenType type, vString *const capture)
{
	tokenInfo *token = newToken ();
	skipToTokenFull (type, capture, token);
	deleteToken (token);
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
		else if (isKeyword (token, KEYWORD_static))
			add = "static";
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

static int lookupQualifiedName (vString *name, int scope)
{
	vString *tmp = vStringNewCopy (name);
	char *part, *next = vStringValue (tmp);
	bool once = true, ongoing = true, external = false;
	while (next)
	{
		part = next;
		next = nextCharIdent (next, '.');
		if (*part != '\0' && next) // has another next (don't lookup last next)
		{
			if (once)
			{
				once = false;
				int *externCorkId = NULL;
				if (!strcmp (part, "C"))
					externCorkId = &PS->externCorkIdC;
				else if (!strcmp (part, "JS"))
					externCorkId = &PS->externCorkIdJS;
				if (externCorkId)
				{
					external = true;
					if (*externCorkId == CORK_NIL)
						*externCorkId = makeTagFull ( // reference
							PS->sofToken, part, KIND_EXTERN, CORK_NIL,
							ROLE_EXTERNAL_SYMBOL, NULL, NULL, NULL, false);
					scope = *externCorkId;
				}
			}
			if (!external && ongoing)
			{
				scope = anyKindsEntryInScope (
					scope, part, vLookupKinds, vLookupNumKinds, true);
				ongoing = scope != CORK_NIL;
			}
		}
	}
	if (scope != CORK_NIL)
		vStringCopyS (name, part);
	vStringDelete (tmp);
	return scope;
}

// _____________________________________________________________________________
//                                                                       PARSERS

// Note: all parsers expect the caller to have already read the initial token
// prior to calling, but will not themselves over-read on exit. Parsers will
// unread the last token, if is not within their purview.

// fq: [ident | type] ['.' [ident | type | kwtype | 'assert']]*
// on return, token->type is IDENT/TYPE/EXTERN, token->string is fully-qualified
static void parseFullyQualified (tokenInfo *const token, bool captureInToken)
{
	Assert (isToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_EXTERN));
	Assert (!captureInToken || token->string);
	if (token->fullyQualified)
		return;
	PARSER_PROLOGUE ("fq");

	bool isExtern = false;
	if (token->string)
	{
		const char *s = vStringValue (token->string);
		isExtern =
			(s[0] == 'C' && (s[1] == '\0' || s[1] == '.')) ||
			(s[0] == 'J' && s[1] == 'S' && (s[2] == '\0' || s[2] == '.'));
	}
	vString *acc = captureInToken? token->string : NULL;
	tokenType prev = TOKEN_NONE, prevprev = TOKEN_NONE;
	tokenInfo *tokens[2] = { NULL, NULL };
	int count = 0, idx = 1;
	while (prev == TOKEN_NONE ||
		   (prev == TOKEN_DOT &&
			(isToken (tokens[idx], TOKEN_IDENT, TOKEN_TYPE) ||
			 isKeyword (tokens[idx], KEYWORD_TYPE, KEYWORD_map, KEYWORD_chan) ||
			 (isExtern && isKeyword (tokens[idx], KEYWORD_assert)))) ||
		   (prev != TOKEN_DOT && isToken (tokens[idx], TOKEN_DOT)))
	{
		prevprev = prev;
		prev = prev == TOKEN_NONE? token->type : tokens[idx]->type;
		idx = 1 - idx;
		if (tokens[idx] == NULL)
			tokens[idx] = newToken ();
		readTokenFull (tokens[idx], acc);
		count++;
	}
	unreadTokenFull (tokens[idx], acc);
	if (prev == TOKEN_DOT && count >= 2)
	{
		unreadTokenFull (tokens[1 - idx], acc);
		prev = prevprev;
	}
	if (prev == TOKEN_KEYWORD)
		prev = TOKEN_IDENT;
	deleteToken (tokens[0]);
	deleteToken (tokens[1]);

	// load token with type (and fq name)
	token->type = isExtern? TOKEN_EXTERN : prev;
	token->fullyQualified = true;

	// should end on one of these
	expectToken (token, TOKEN_TYPE, TOKEN_IDENT, TOKEN_EXTERN, TOKEN_KEYWORD) ||
		expectKeyword (token, KEYWORD_TYPE, KEYWORD_map, KEYWORD_chan);

	PARSER_EPILOGUE ();
}

// fqident: [ident | type] fq
static void parseFQIdent (tokenInfo *const token, vString *const capture)
{
	Assert (isToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_EXTERN));
	parseFullyQualified (token, !!capture);
	expectToken (token, TOKEN_IDENT, TOKEN_EXTERN);
	if (capture && token->string)
		vStringCat (capture, token->string);
}

// init: [
//     ['{' ['...' fqident]? [',' | [ [label | expr ':']? expr ]]* '}'] |
//     ['(' [',' | [[label | expr ':']? expr | '...' expr ]]* ')']
// ]
static void parseInit (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_OPEN_CURLY, TOKEN_OPEN_PAREN));
	PARSER_PROLOGUE ("init");

	bool hasDestruc = isToken (token, TOKEN_OPEN_CURLY);
	bool hasVarargs = isToken (token, TOKEN_OPEN_PAREN);
	tokenType close = getClose (token->type);
	readToken (token);
	if (hasDestruc && isToken (token, TOKEN_SLICE))
	{
		readToken (token);
		if (!isToken (token, close))
		{
			if (parseExpression (token, scope, NULL))
				readToken (token);
		}
		else
			unreadToken (token);
	}
	while (!isToken (token, TOKEN_EOF, close))
	{
		if (isToken (token, TOKEN_COMMA))
			readToken (token);
		else
		{
			if (isToken (token, TOKEN_LABEL))
				readToken (token);
			else if (isToken (token, TOKEN_DOT))
			{
				// enumerator labels
				readToken (token);
				if (isToken (token, TOKEN_LABEL))
					readToken (token);
				else
					unreadToken (token);
			}
			else if (hasVarargs && isToken (token, TOKEN_SLICE))
				readToken (token);
			if (!isToken (token, close))
			{
				if (parseExpression (token, scope, NULL))
				{
					readToken (token);
					if (isToken (token, TOKEN_COLON)) // expr labels
					{
						readToken (token);
						if (!isToken (token, close))
						{
							parseExpression (token, scope, NULL);
							readToken (token);
						}
					}
				}
			}
		}
	}

	PARSER_EPILOGUE ();
}

// fncall: '(' init err-cont
static void parseFnCall (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_OPEN_PAREN));
	PARSER_PROLOGUE ("fncall");

	parseInit (token, scope);
	readToken (token);
	if (!parseExprCont (token, scope, true))
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
		expectKeyword (token, KEYWORD_TYPE, KEYWORD_map))
	{
		tokenType last;
		do
		{
			last = isToken (token, TOKEN_DOT);
			readToken (token);
		}
		while ((isToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_DOT) ||
				isKeyword (token, KEYWORD_TYPE, KEYWORD_map)) &&
			   isToken (token, TOKEN_DOT) != last);
		if (isToken (token, TOKEN_DOT))
			unreadToken (token);
		else if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else if (!parseExprCont (token, scope, false))
			unreadToken (token);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// block: '{' statement* '}' ['.' chain]?
// returns line number of CLOSE_CURLY
static int parseBlock (tokenInfo *const token, int scope, bool hasChain)
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
	int ret = token->lineNumber;
	if (hasChain)
	{
		readToken (token);
		if (isToken (token, TOKEN_DOT))
			parseChain (token, scope);
		else
			unreadToken (token);
	}

	PARSER_EPILOGUE ();
	return ret;
}

// decl: [':=' | 'in'] list
static void parseDeclare (tokenInfo *const token, vString *const names,
						  int scope, vString *const accesses)
{
	Assert (isToken (token, TOKEN_DECLARE) || isKeyword (token, KEYWORD_in));
	PARSER_PROLOGUE ("decl");

	if (isToken (token, TOKEN_DECLARE) || isKeyword (token, KEYWORD_in))
	{
		char *name = names? vStringValue (names) : NULL;
		char *access = accesses? vStringValue (accesses) : NULL;
		while (name)
		{
			char *n = name, *a = access;
			name = nextCharIdent (name, ',');
			access = nextCharIdent (access, ',');
			makeTagFull (token, n, KIND_VARIABLE, scope,
						 ROLE_DEFINITION_INDEX, NULL, NULL, a, false);
		}
	}
	readToken (token);
	parseExprList (token, scope, NULL, false);

	PARSER_EPILOGUE ();
}

// if: 'if' list '?'? block ['else' [block | if]]?
static void parseIf (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_if, KEYWORD_Sif));
	PARSER_PROLOGUE ("if");

	vString *access = NULL;
	readToken (token);
	skipAccessAndReadToken (token, &access);
	parseExprList (token, scope, access, false);
	vStringDelete (access);
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
			{
				parseBlock (token, scope, false);
				readToken (token);
				if (!parseExprCont (token, scope, false))
					unreadToken (token);
			}
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

// fndef: 'fn' receiver? [fqident | kwtype] tmpl-args? args vtype? block?
// method: [fqident | kwtype] args vtype?
// lambda: 'fn' [id | clsr-args]? tmpl-args? args vtype? block fncall?
// fntype: 'fn' tmpl-args args vtype?
//
// receiver: '(' 'mut'? '*'? ident vtype ')'
// tmpl-args: '[' any ']'
// clsr-args: '[' any ']'
// args: '(' any ')'
static void parseFunction (tokenInfo *const token, int scope,
						vString *const access, kindType kind)
{
	Assert (kind == KIND_FUNCTION || kind == KIND_METHOD ||
			kind == KIND_ALIAS || kind == KIND_NONE);
	Assert (kind == KIND_METHOD || isKeyword (token, KEYWORD_fn));
	PARSER_PROLOGUE (kind == KIND_FUNCTION? "fndef" :
					 (kind == KIND_METHOD? "method" :
					  (kind == KIND_ALIAS? "fntype" : "lambda")));

	// TODO: make return false if calling function should not further read token
	// due to failure of this function to process token (like parseExpression)

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
					if (parseVType (token, acc, CORK_NIL, false, false))
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
			skipToToken (TOKEN_CLOSE_PAREN, NULL);
	}
	// name
	if ((kind == KIND_NONE && (
			 isToken (token, TOKEN_IDENT, TOKEN_TYPE) ||
			 isKeyword (token, KEYWORD_TYPE))) ||
		(kind == KIND_FUNCTION && (
			expectToken (token, TOKEN_IDENT, TOKEN_TYPE, TOKEN_KEYWORD,
						 TOKEN_OPERATOR, TOKEN_PLUSMINUS, TOKEN_ANDAND,
						 TOKEN_ASTERISK) ||
			expectKeyword (token, KEYWORD_TYPE, KEYWORD_map))))
	{
		if (isToken (token, TOKEN_KEYWORD))
		{
			name = token->string;
			token->string = NULL;
		}
		else if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
		{
			name = vStringNew ();
			parseFQIdent (token, name);
		}
		deleteToken (fnToken);
		fnToken = dupToken (token);
		readToken (token);
	}
	// closure args
	if (kind == KIND_NONE && name == NULL &&
		isToken (token, TOKEN_OPEN_SQUARE))
	{
		vStringPut (argList, '[');
		skipToToken (TOKEN_CLOSE_SQUARE, acc);
		vStringAccumulate (argList, acc);
		readToken (token);
	}
	// template args
	if ((kind == KIND_FUNCTION ||
		 (kind == KIND_NONE && name == NULL)) &&
		isToken (token, TOKEN_OPEN_SQUARE))
	{
		vStringPut (argList, '[');
		skipToToken (TOKEN_CLOSE_SQUARE, acc);
		vStringAccumulate (argList, acc);
		readToken (token);
	}
	// args
	if (expectToken (token, TOKEN_OPEN_PAREN))
	{
		vStringPut (argList, '(');
		skipToToken (TOKEN_CLOSE_PAREN, acc);
		vStringAccumulate (argList, acc);
		// next token could be ident of next interface method or struct field
		readTokenFull (token, acc);
		if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
		{
			size_t len = vStringLength (token->string);
			parseFullyQualified (token, true);
			vStringCatS (acc, vStringValue (token->string) + len);
		}
		// return type (unless on next line)
		if (isInitialType (token) && !token->onNewline)
		{
			parseVType (token, acc, scope, false, false);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		else if (isToken (token, TOKEN_OPEN_PAREN))
		{
			skipToToken (TOKEN_CLOSE_PAREN, acc);
			vStringAccumulate (retType, acc);
			readToken (token);
		}
		else if ((kind == KIND_METHOD || kind == KIND_ALIAS) &&
				 isToken (token, TOKEN_IDENT))
		{
			vStringDelete (token->string);
			token->string = acc;
			acc = NULL;
		}
		int newScope = CORK_NIL;
		if (name || kind == KIND_METHOD)
		{
			kindType fnKind = kind == KIND_NONE? KIND_FUNCTION : kind;
			newScope = makeFnTag (fnToken, name, fnKind, scope,
								  argList, retType, access);
			if (receiver)
				makeTag (rxToken, NULL, KIND_RECEIVER, newScope);
		}
		// block
		if ((kind == KIND_NONE && expectToken (token, TOKEN_OPEN_CURLY)) ||
			(kind == KIND_FUNCTION && isToken (token, TOKEN_OPEN_CURLY)))
		{
			int lineNumber = parseBlock (token, newScope, false);
			tagEntryInfo *entry = getEntryInCorkQueue (newScope);
			if (entry)
				entry->extensionFields.endLine = lineNumber;
			// fncall
			if (kind == KIND_NONE)
			{
				readToken (token);
				if (isToken (token, TOKEN_OPEN_PAREN))
					parseFnCall (token, newScope);
				else
					unreadToken (token);
			}
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

// chpop: '<-' ident ['or' block]?
static void parseChanPop (tokenInfo *const token, int scope)
{
	Assert (isToken (token, TOKEN_CHPOP));
	PARSER_PROLOGUE ("chpop");

	readToken (token);
	if (expectToken (token, TOKEN_IDENT, TOKEN_TYPE))
	{
		parseFQIdent (token, NULL);
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

	readToken (token);
	skipAccessAndReadToken (token, NULL);
	if (!isClose (token))
	{
		parseExprList (token, scope, NULL, false);
		readToken (token);
	}
	if (!expectToken (token, TOKEN_OPEN_CURLY))
		skipToTokenFull (TOKEN_OPEN_CURLY, NULL, token);
	if (isToken (token, TOKEN_OPEN_CURLY))
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
				else if (isInitialType (token))
					parseVType (token, NULL, scope, false, false);
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
			skipToToken (TOKEN_CLOSE_CURLY, NULL);
	}

	PARSER_EPILOGUE ();
}

// select: 'select' '{' [list block]* '}'
static void parseSelect (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_select));
	PARSER_PROLOGUE ("select");

	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
	{
		readToken (token);
		do // select clauses
		{
			if (!isToken (token, TOKEN_CLOSE_CURLY))
				parseExprList (token, scope, NULL, false);
			readToken (token);
			if (expectToken (token, TOKEN_OPEN_CURLY))
			{
				parseBlock (token, scope, false);
				readToken (token);
			}
		}
		while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY));
		expectToken (token, TOKEN_CLOSE_CURLY);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// for: list? [';' expr]* block
static void parseFor (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_for, KEYWORD_Sfor));
	PARSER_PROLOGUE ("for");

	vString *access = NULL;
	readToken (token);
	skipAccessAndReadToken (token, &access);
	if (!isToken (token, TOKEN_OPEN_CURLY, TOKEN_SEMICOLON, TOKEN_EOF))
	{
		parseExprList (token, CORK_NIL, access, true);
		readToken (token);
	}
	vStringDelete (access);
	while (isToken (token, TOKEN_SEMICOLON))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_CURLY, TOKEN_EOF))
			break;
		else if (!isToken (token, TOKEN_SEMICOLON))
		{
			parseExpression (token, scope, NULL);
			readToken (token);
		}
	}
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope, false);
	else
		unreadToken (token);

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
				vStringDelete (moduleName);
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

// struct: 'struct' [fqtype | extern] ['[' any ']']? struct-rest
// iface: 'interface' [fqtype | extern] ['[' any ']']? struct-rest
// union: 'union' [fqtype | extern] ['[' any ']']? struct-rest
// anon: ['struct' | 'union'] [[fqtype | extern] ['[' any ']']?]? struct-rest
// struct-rest: '{' [
//     [access* ':'] | [
//         fqtype | extern | [ident | kwtype | 'map'] method |
//         [ident | extern] vtype ['@'? '[' any ']']? ['=' expr]?
//     ]*
// ]* '}'
static void parseStruct (tokenInfo *const token, vString *const access,
						 int scope, kindType kind)
{
	Assert (
		(kind == KIND_STRUCT && isKeyword (token, KEYWORD_struct)) ||
		(kind == KIND_INTERFACE && isKeyword (token, KEYWORD_interface)) ||
		(kind == KIND_UNION && isKeyword (token, KEYWORD_union)) ||
		(kind == KIND_NONE && isKeyword (token, KEYWORD_struct, KEYWORD_union)));
	PARSER_PROLOGUE (kind == KIND_INTERFACE? "iface" :
					 (kind == KIND_UNION? "union" :
					  (kind == KIND_STRUCT? "struct" : "anon")));

	int newScope = scope;
	readToken (token);

	// name
	if ((kind == KIND_STRUCT || kind == KIND_INTERFACE) && PS->isBuiltin &&
		scope == CORK_NIL && isKeyword (token, KEYWORD_TYPE, KEYWORD_map))
		readToken (token);
	else if ((kind != KIND_NONE && expectToken (token, TOKEN_TYPE)) ||
			 (kind == KIND_NONE && isToken (token, TOKEN_TYPE)))
	{
		parseFullyQualified (token, true);
		if (expectToken (token, TOKEN_TYPE, TOKEN_EXTERN))
		{
			scope = lookupQualifiedName (token->string, scope);
			kindType realKind = kind == KIND_NONE? KIND_STRUCT : kind;
			newScope = makeTagEx (token, NULL, realKind, scope, access);
			registerEntry (newScope);
			readToken (token);
		}
		if (isToken (token, TOKEN_OPEN_SQUARE)) // template args
		{
			skipToToken (TOKEN_CLOSE_SQUARE, NULL);
			readToken (token);
		}
	}

	if (expectToken (token, TOKEN_OPEN_CURLY))
	{
		vString *fieldAccess = NULL;
		readToken (token);
		while (!isToken (token, TOKEN_CLOSE_CURLY, TOKEN_EOF))
		{
			vString *tmp = NULL;
			skipAccessAndReadToken (token, &tmp);
			if (tmp)
			{
				vStringDelete (fieldAccess);
				fieldAccess = tmp;
				if(expectToken (token, TOKEN_COLON))
					readToken (token);
				continue;
			}

			if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
				parseFullyQualified (token, true);

			// if set, parse field below (with next token already read)
			tokenInfo *fieldToken = NULL;

			if (isToken (token, TOKEN_IDENT))
			{
				fieldToken = dupToken (token);
				readToken (token);
				if (kind == KIND_INTERFACE && isToken (token, TOKEN_OPEN_PAREN))
				{
					unreadToken (token);
					copyToken (token, fieldToken);
					deleteToken (fieldToken);
					parseFunction (token, newScope, fieldAccess, KIND_METHOD);
					readToken (token);
					continue; // method
				}
			}
			else if (kind == KIND_INTERFACE &&
					 isKeyword (token, KEYWORD_TYPE, KEYWORD_map))
			{
				parseFunction (token, newScope, fieldAccess, KIND_METHOD);
				readToken (token);
				continue; // method
			}
			else if (isToken (token, TOKEN_TYPE))
			{
				if (PS->isBuiltin)
				{
					fieldToken = dupToken (token);
					readToken (token);
					if (token->onNewline)
					{
						deleteToken (fieldToken);
						continue; // embed type (after all)
					}
				}
				else
				{
					readToken (token);
					continue; // embedded type
				}
			}
			else if (isToken (token, TOKEN_EXTERN))
			{
				fieldToken = dupToken (token);
				readToken (token);
				if (token->onNewline)
				{
					deleteToken (fieldToken);
					continue; // embedded extern
				}
			}
			else
			{
				expectToken (token, TOKEN_TYPE, TOKEN_IDENT, TOKEN_EXTERN,
							 TOKEN_KEYWORD);
				skipToToken (TOKEN_CLOSE_CURLY, false);
				break;
			}

			if (fieldToken) // parse field
			{
				if (parseVType (token, NULL, newScope, false, false))
				{
					makeTagEx (fieldToken, NULL, KIND_FIELD, newScope,
							   fieldAccess);

					readToken (token);
					if (isToken (token, TOKEN_AT))
					{
						readToken (token);
						if (expectToken (token, TOKEN_OPEN_SQUARE))
						{
							skipToToken (TOKEN_CLOSE_SQUARE, NULL);
							readToken (token);
						}
					}
					else if (isToken (token, TOKEN_OPEN_SQUARE))
					{
						skipToToken (TOKEN_CLOSE_SQUARE, NULL);
						readToken (token);
					}
					if (isToken (token, TOKEN_ASSIGN))
					{
						readToken (token);
						if (parseExpression (token, newScope, NULL))
							readToken (token);
					}
				}
				else
					vDebugUnexpected (token, "vtype", 0);
				deleteToken (fieldToken);
			}
		}
		vStringDelete (fieldAccess);
	}

	if (kind != KIND_NONE)
	{
		tagEntryInfo *entry = getEntryInCorkQueue (newScope);
		if (entry)
			entry->extensionFields.endLine = token->lineNumber;
	}

	PARSER_EPILOGUE ();
}

// enum: 'enum' type ['as' type]? '{' [
//     [ident | kwtype] ['=' [immediate | extern]]? ['@'? '[' any ']']?
// ]* '}'
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
			while (expectToken (token, TOKEN_CLOSE_CURLY, TOKEN_IDENT,
								TOKEN_KEYWORD) ||
				   expectKeyword (token, KEYWORD_TYPE, KEYWORD_map,
								  KEYWORD_chan))
			{
				if (isToken (token, TOKEN_CLOSE_CURLY))
					break;

				makeTag (token, capture, KIND_ENUMERATOR, newScope);
				vStringCopyS (capture, ".");
				PS->lastTokenType = TOKEN_DOT; // prevent capture of ' '

				readTokenFull (token, capture);
				if (isToken (token, TOKEN_ASSIGN))
				{
					readToken (token);
					if (isToken (token, TOKEN_IDENT, TOKEN_TYPE))
						parseFullyQualified (token, true);
					vStringCopyS (capture, ".");
					PS->lastTokenType = TOKEN_DOT; // prevent capture of ' '
					if (expectToken (token, TOKEN_IMMEDIATE, TOKEN_EXTERN))
						readTokenFull (token, capture);
				}

				if (isToken (token, TOKEN_AT))
				{
					readToken (token);
					if (expectToken (token, TOKEN_OPEN_SQUARE))
					{
						skipToToken (TOKEN_CLOSE_SQUARE, NULL);
						vStringCopyS (capture, ".");
						PS->lastTokenType = TOKEN_DOT; // prevent capture of ' '
						readTokenFull (token, capture);
					}
				}
				else if (isToken (token, TOKEN_OPEN_SQUARE))
				{
					skipToToken (TOKEN_CLOSE_SQUARE, NULL);
					vStringCopyS (capture, ".");
					PS->lastTokenType = TOKEN_DOT; // prevent capture of ' '
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
//    [fqtype | extern] ['[' any ']']? | ['chan' | 'shared'] vtype | kwtype |
//    'struct' struct | 'map' '[' any* ']' vtype | 'fn' fntype |
//    '$map' | '$struct' | '$enum' | '$array' | '$sumtype' | '$alias' |
// ] ['{' init]?
// return false if token is rejected
// set letInit false to parse *only* V type (with no {...} initialisation after)
// set canInit false unless tokens already read which would permit it (e.g., [])
static bool parseVType (tokenInfo *const token, vString *const capture,
						int scope, bool canInit, bool letInit)
{
	PARSER_PROLOGUE ("vtype");

	bool ok = true;

	if (isToken (token, TOKEN_OPEN_SQUARE))
	{
		skipToToken (TOKEN_CLOSE_SQUARE, capture);
		readTokenFull (token, capture);
		if (parseVType (token, capture, scope, false, false))
			canInit = true;
		else
			unreadTokenFull (token, capture);
	}
	else if (isToken (token, TOKEN_EXCLAMATION))
	{
		readTokenFull (token, capture);
		if (!token->onNewline &&
			parseVType (token, capture, scope, false, false))
			canInit = true;
		else
			unreadTokenFull (token, capture);
	}
	else if (isToken (token, TOKEN_QUESTION))
	{
		readTokenFull (token, capture);
		if (isToken (token, TOKEN_OPEN_PAREN))
			skipToToken (TOKEN_CLOSE_PAREN, capture);
		else if (!parseVType (token, capture, scope, false, false))
		{
			vDebugUnexpected (token, "vtype", 0);
			unreadTokenFull (token, capture);
		}
	}
	else if (isToken (token, TOKEN_AMPERSAND, TOKEN_ANDAND))
	{
		readTokenFull (token, capture);
		if (parseVType (token, capture, scope, false, false))
			canInit = true;
		else
		{
			vDebugUnexpected (token, "vtype", 0);
			unreadTokenFull (token, capture);
		}

	}
	else if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
	{
		size_t len = capture? vStringLength (token->string) : 0;
		parseFullyQualified (token, !!capture);
		if (capture)
			vStringCatS (capture, vStringValue (token->string) + len);
		if (expectToken (token, TOKEN_TYPE, TOKEN_EXTERN, TOKEN_KEYWORD) ||
			expectKeyword (token, KEYWORD_none))
		{
			canInit = true;
			readTokenFull (token, capture);
			if (isToken (token, TOKEN_OPEN_SQUARE))
				skipToToken (TOKEN_CLOSE_SQUARE, capture);
			else
				unreadTokenFull (token, capture);
		}
	}
	else if (isKeyword (token, KEYWORD_TYPE, KEYWORD_Smap, KEYWORD_Sstruct,
						KEYWORD_Senum, KEYWORD_Sarray, KEYWORD_Ssumtype,
						KEYWORD_Salias, KEYWORD_none))
		;
	else if (isKeyword (token, KEYWORD_chan, KEYWORD_shared))
	{
		readTokenFull (token, capture);
		if (parseVType (token, capture, scope, false, false))
			canInit = true;
		else
			unreadTokenFull (token, capture);
	}
	else if (isKeyword (token, KEYWORD_map))
	{
		readTokenFull (token, capture);
		if ((!PS->isBuiltin && expectToken (token, TOKEN_OPEN_SQUARE)) ||
			(PS->isBuiltin && isToken (token, TOKEN_OPEN_SQUARE)))
		{
			skipToToken (TOKEN_CLOSE_SQUARE, capture);
			readTokenFull (token, capture);
			if (parseVType (token, capture, scope, false, false))
				canInit = true;
			else
				unreadTokenFull (token, capture);
		}
		else
			unreadTokenFull (token, capture);
	}
	else if (isKeyword (token, KEYWORD_struct, KEYWORD_union))
	{
		parseStruct (token, NULL, scope, KIND_NONE);
		canInit = true;
	}
	else if (isKeyword (token, KEYWORD_fn) && !token->onNewline)
		parseFunction (token, scope, NULL, KIND_ALIAS);
	else
		ok = false;

	if (letInit && canInit)
	{
		readTokenFull (token, capture);
		if (isToken (token, TOKEN_OPEN_CURLY))
			parseInit (token, scope);
		else
			unreadTokenFull (token, capture);
	}

	PARSER_EPILOGUE ();
	return ok;
}

// alias: 'type' type ['[' type ']']? '=' vtype ['|' vtype]*
static void parseAlias (tokenInfo *const token, vString *const access, int scope)
{
	Assert (isKeyword (token, KEYWORD_type));
	PARSER_PROLOGUE ("alias");

	readToken (token);
	if ((!PS->isBuiltin && expectToken (token, TOKEN_TYPE)) ||
		(PS->isBuiltin && (expectToken (token, TOKEN_TYPE, TOKEN_KEYWORD) ||
					   expectKeyword (token, KEYWORD_TYPE))))
	{
		int newScope = makeTagEx (token, NULL, KIND_ALIAS, scope, access);
		registerEntry (newScope);
		readToken (token);
		if (isToken (token, TOKEN_OPEN_SQUARE)) // template args
		{
			skipToToken (TOKEN_CLOSE_SQUARE, NULL);
			readToken (token);
		}
		if (expectToken (token, TOKEN_ASSIGN))
		{
			readToken (token);
			if(parseVType (token, NULL, scope, false, false))
				readToken (token);
			while (true)
			{
				if (isToken (token, TOKEN_PIPE))
				{
					readToken (token);
					if (parseVType (token, NULL, scope, false, false))
					{
						readToken (token);
						continue;
					}
					else
						unreadToken (token);
				}
				else
					unreadToken (token);
				break;
			}
		}
	}

	PARSER_EPILOGUE ();
}

// list: [
//     [access* indent [',' access* ident]* [':=' | 'in'] list] |
//     [expr? [',' expr]*]
// ]
static void parseExprList (tokenInfo *const token, int scope,
						   vString *const access, bool hasInDecl)
{
	PARSER_PROLOGUE ("list");

	// attempt to consume multi-var declaration (special case of list)
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
			if (isToken (token, TOKEN_DECLARE) ||
				(hasInDecl && isKeyword (token, KEYWORD_in)))
				parseDeclare (token, idents, scope, accesses);
			else
			{
				if (isToken (token, TOKEN_DOT))
				{
					unreadToken (token);
					parseFullyQualified (identToken, false);
					if (isToken (identToken, TOKEN_TYPE))
						parseVType (identToken, NULL, scope, false, true);
					readToken (token);
				}
				if (!parseExprCont (token, scope, true))
					unreadToken (token);
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
//     '[' '...'? list ']' err-cont | ['=' ':='] expr | '(' fncall
// ]?
// return false if token is rejected
static bool parseExprCont (tokenInfo *const token, int scope, bool hasErr)
{
	bool ok = true;

	if (isClose (token))
		ok = false;

	if (ok && hasErr)
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

	if (ok)
	{
		PARSER_PROLOGUE ("cont");

		if (isToken (token, TOKEN_INCDECOP))
			readToken (token);
		if (isToken (token, TOKEN_DOT))
		{
			// could be enumerator label
			tokenInfo *tmpToken = newToken ();
			readToken (tmpToken);
			unreadToken (tmpToken);
			if (isToken (tmpToken, TOKEN_LABEL))
				unreadToken (token);
			else
				parseChain (token, scope);
			deleteToken (tmpToken);
		}
		else if (isOperator (token))
		{
			readToken (token);
			parseExpression (token, scope, NULL);
		}
		else if (isToken (token, TOKEN_EXCLAMATION))
		{
			readToken (token);
			if (expectKeyword (token, KEYWORD_in, KEYWORD_is))
			{
				keywordId wasIs = isKeyword(token, KEYWORD_is);
				readToken (token);
				if (!isClose (token))
				{
					if (wasIs)
					{
						if(parseVType (token, NULL, scope, false, false))
							readToken (token);
						if (!parseExprCont (token, scope, false))
							unreadToken (token);
					}
					else
						parseExpression (token, scope, NULL);
				}
				else
					unreadToken (token);
			}
			else
				unreadToken (token);
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
			if (parseVType (token, NULL, scope, false, false))
				readToken (token);
			if (!parseExprCont (token, scope, false))
				unreadToken (token);
		}
		else if (isToken (token, TOKEN_OPEN_SQUARE) && !token->onNewline)
		{
			readToken (token);
			if (isToken (token, TOKEN_SLICE))
				readToken (token);
			if (!isClose (token))
			{
				parseExprList (token, scope, NULL, false);
				readToken (token);
			}
			if (expectToken (token, TOKEN_CLOSE_SQUARE))
			{
				readToken (token);
				if (!parseExprCont (token, scope, true))
					unreadToken (token);
			}
			else
				unreadToken (token);
		}
		else if (isToken (token, TOKEN_DECLARE, TOKEN_ASSIGN))
		{
			expectToken (token, TOKEN_ASSIGN);
			readToken (token);
			parseExprList (token, scope, NULL, false);
		}
		else if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, scope);
		else
			ok = false;

		PARSER_EPILOGUE ();
	}

	return ok;
}

// unsafe: 'unsafe' block cont?
static void parseUnsafe (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_unsafe));
	PARSER_PROLOGUE ("unsafe");

	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, scope, false);
		readToken (token);
		if (!parseExprCont (token, scope, false))
			unreadToken (token);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// lock: ['lock' | 'rlock'] fqident? block
static void parseLock (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_lock, KEYWORD_rlock));
	PARSER_PROLOGUE ("lock");

	readToken (token);
	if (isToken (token, TOKEN_IDENT, TOKEN_TYPE))
	{
		parseFQIdent (token, NULL);
		readToken (token);
	}
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope, false);
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

static bool parseExpression (tokenInfo *const token, int scope,
							 vString *const access)
{
	PARSER_PROLOGUE ("expr");

	bool ok = true;

	skipAccessAndReadToken (token, NULL);

	if (isToken (token, TOKEN_LABEL))
		readToken (token);

	if (isToken (token, TOKEN_IDENT, TOKEN_TYPE))
		parseFullyQualified (token, false);
	if (isToken (token, TOKEN_IDENT) ||
		isKeyword (token, KEYWORD_none, KEYWORD_TYPE))
	{
		readToken (token);
		if (!parseExprCont (token, scope, true))
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_EXTERN))
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_CURLY))
		{
			tokenInfo *tmpToken = newToken ();
			readToken (tmpToken);
			if (!isToken (tmpToken, TOKEN_CLOSE_CURLY))
			{
				unreadToken (tmpToken);
				if (!parseExprCont (token, scope, true))
					unreadToken (token);
			}
		}
		else if (!parseExprCont (token, scope, true))
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_IMMEDIATE, TOKEN_DOT))
	{
		if (isToken (token, TOKEN_DOT)) // enumerators
		{
			readToken (token);
			if (!(expectToken (token, TOKEN_IDENT, TOKEN_KEYWORD) ||
					expectKeyword (token, KEYWORD_TYPE, KEYWORD_map,
								   KEYWORD_chan)) &&
				isClose (token))
				unreadToken (token);
		}
		readToken (token);
		if (!parseExprCont (token, scope, false))
			unreadToken (token);
	}
	else if (isToken (token, TOKEN_TYPE))
	{
		parseVType (token, NULL, scope, false, true);
		readToken (token);
		if (!(isToken (token, TOKEN_OPEN_PAREN) ||
			  isKeyword (token, KEYWORD_is, KEYWORD_in)) ||
			!parseExprCont (token, scope, false))
			unreadToken (token);
	}
	else if (isKeyword (token, KEYWORD_lock, KEYWORD_rlock))
		parseLock (token, scope);
	else if (isKeyword (token, KEYWORD_fn))
		parseFunction (token, scope, NULL, KIND_NONE);
	else if (isKeyword (token, KEYWORD_if, KEYWORD_Sif))
		parseIf (token, scope);
	else if (isKeyword (token, KEYWORD_match))
		parseMatch (token, scope);
	else if (isKeyword (token, KEYWORD_select))
		parseSelect (token, scope);
	else if (isToken (token, TOKEN_OPEN_CURLY))
		parseInit (token, scope);
	else if (isKeyword (token, KEYWORD_chan, KEYWORD_map))
		parseVType (token, NULL, scope, false, true);
	else if (isToken (token, TOKEN_CHPOP))
		parseChanPop (token, scope);
	else if (isKeyword (token, KEYWORD_unsafe))
		parseUnsafe (token, scope);
	else if (isToken (token, TOKEN_TILDE, TOKEN_EXCLAMATION, TOKEN_QUESTION,
					  TOKEN_ASTERISK, TOKEN_AMPERSAND, TOKEN_PLUSMINUS,
					  TOKEN_ANDAND) ||
			 isKeyword (token, KEYWORD_spawn, KEYWORD_go))
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
			if (!parseExprCont (token, scope, false))
				unreadToken (token);
		}
		else
			skipToToken (TOKEN_CLOSE_PAREN, NULL);
	}
	else if (isToken (token, TOKEN_OPEN_SQUARE))
	{
		readToken (token);
		if (isToken (token, TOKEN_CLOSE_SQUARE)) // []type or []?
		{
			readToken (token);
			if (isToken (token, TOKEN_TYPE, TOKEN_IDENT))
				parseFullyQualified (token, true);
			if (isToken (token, TOKEN_TYPE, TOKEN_OPEN_SQUARE) ||
				isKeyword (token, KEYWORD_TYPE, KEYWORD_map, KEYWORD_chan))
			{
				parseVType (token, NULL, scope, true, true);
				readToken (token);
				if (isToken (token, TOKEN_OPEN_PAREN)) // cast
					parseFnCall (token, scope);
				else
					unreadToken (token);
			}
			else
				unreadToken (token);
		}
		else
		{
			while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_SQUARE))
			{
				parseExprList (token, scope, NULL, false);
				readToken (token);
			}
			if (expectToken (token, TOKEN_CLOSE_SQUARE))
			{
				readToken (token);
				if (isToken (token, TOKEN_EXCLAMATION))
					readToken (token);
				if (parseExprCont (token, scope, false))
					;
				else if (!token->onNewline && isInitialType (token) &&
						 parseVType (token, NULL, scope, true, true))
					;
				else
					unreadToken (token);
			}
			else
				unreadToken (token);
		}
	}
	else
	{
		vDebugUnexpected (token, "expression", 0);
		ok = false;
	}

	PARSER_EPILOGUE ();
	return ok;
}

// return: 'return' list?
static void parseReturn (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_return));
	PARSER_PROLOGUE ("return");

	readToken (token);
	if (!isClose (token))
		parseExprList (token, scope, NULL, false);
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// defer: 'defer' block
static void parseDefer (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_defer));
	PARSER_PROLOGUE ("defer");

	readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope, false);
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// asm: 'asm' ident? '{' any '}'
static void parseAsm (tokenInfo *const token, int scope)
{
	Assert (isKeyword (token, KEYWORD_asm));
	PARSER_PROLOGUE ("asm");

	readToken (token);
	if (isToken (token, TOKEN_IDENT))
		readToken (token);
	if (expectToken (token, TOKEN_OPEN_CURLY))
		skipToToken (TOKEN_CLOSE_CURLY, NULL);
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
}

// stmt: [
//     'for' for | 'return' list? | 'defer' block | 'assert' expr |
//     'break' ident? | 'asm' '{' any '}' | 'lock' fqident block | expr
// ]
static void parseStatement (tokenInfo *const token, int scope)
{
	PARSER_PROLOGUE ("stmt");

	if (isToken (token, TOKEN_LABEL))
	{
		makeTag (token, NULL, KIND_LABEL, scope);
		readToken (token);
	}

	if (isToken (token, TOKEN_CLOSE_CURLY))
		;
	else if (isToken (token, TOKEN_IDENT))
		parseExprList (token, scope, NULL, false);
	else if (isKeyword (token, KEYWORD_for, KEYWORD_Sfor))
		parseFor (token, scope);
	else if (isKeyword (token, KEYWORD_return))
		parseReturn (token, scope);
	else if (isKeyword (token, KEYWORD_defer))
		parseDefer (token, scope);
	else if (isKeyword (token, KEYWORD_asm))
		parseAsm (token, scope);
	else if (isToken (token, TOKEN_OPEN_CURLY))
		parseBlock (token, scope, false);
	else if (isKeyword (token, KEYWORD_assert))
	{
		readToken (token);
		parseExprList (token, scope, NULL, false);
	}
	else if (isKeyword (token, KEYWORD_continue, KEYWORD_break))
	{
		readToken (token);
		if (!isToken (token, TOKEN_IDENT))
			unreadToken (token);
	}
	else
	{
		vString *access = NULL;
		skipAccessAndReadToken (token, &access);
		parseExprList (token, scope, access, false);
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
	{
		if (!strcmp (vStringValue (token->string), "builtin"))
			PS->isBuiltin = true;
		else
			scope = makeTag (token, NULL, KIND_MODULE, CORK_NIL);
	}
	else
		unreadToken (token);

	PARSER_EPILOGUE ();
	return scope;
}

// global: '__global' ident '=' expr
static void parseGlobal (tokenInfo *token, vString *access, int scope)
{
	Assert (isToken (token, TOKEN_IDENT));
	Assert (!strcmp (vStringValue (access), "__global"));
	PARSER_PROLOGUE ("global");

	tokenInfo *identToken = dupToken (token);
	readToken (token);
	makeTag (identToken, NULL, KIND_VARIABLE, CORK_NIL);
	if (isToken (token, TOKEN_ASSIGN))
		readToken (token);
	if (!isClose (token))
		parseExpression (token, scope, access);
	else
		unreadToken (token);
	deleteToken (identToken);

	PARSER_EPILOGUE ();
}

// file: [access* [
//     ['(' any ')'] | ['{' any '}'] | 'module' module | 'fn' fn | 'enum' enum |
//     'import' import | 'const' const | 'struct' struct | 'interface' iface |
//     'union' union | 'type' alias | '$if' if | 'asm' asm
// ]]*
static void parseFile (tokenInfo *const token)
{
	PARSER_PROLOGUE ("file");

	int scope = CORK_NIL;
	vString *access = NULL;
	do
	{
		readToken (token);
		if (PS->sofToken == NULL)
			PS->sofToken = dupToken (token);
		skipAccessAndReadToken (token, &access);

		if (isToken (token, TOKEN_OPEN_PAREN, TOKEN_OPEN_CURLY,
					 TOKEN_OPEN_SQUARE)) // attributes
			skipToToken (getClose (token->type), NULL);
		else if (isKeyword (token, KEYWORD_module))
			scope = parseModule (token);
		else if (isKeyword (token, KEYWORD_fn))
			parseFunction (token, scope, access, KIND_FUNCTION);
		else if (isKeyword (token, KEYWORD_import))
			parseImport (token);
		else if (isKeyword (token, KEYWORD_const))
			parseConst (token, access);
		else if (isKeyword (token, KEYWORD_struct))
			parseStruct (token, access, scope, KIND_STRUCT);
		else if (isKeyword (token, KEYWORD_interface))
			parseStruct (token, access, scope, KIND_INTERFACE);
		else if (isKeyword (token, KEYWORD_union))
			parseStruct (token, access, scope, KIND_UNION);
		else if (isKeyword (token, KEYWORD_enum))
			parseEnum (token, access, scope);
		else if (isKeyword (token, KEYWORD_type))
			parseAlias (token, access, scope);
		else if (isToken (token, TOKEN_IDENT, TOKEN_TYPE) &&
				 access && !strcmp (vStringValue (access), "__global"))
			parseGlobal (token, access, scope);
		else if (isKeyword (token, KEYWORD_Sif))
			parseIf (token, scope);
		else if (isKeyword (token, KEYWORD_asm))
			parseAsm (token, scope);
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
	PS = newParserState ();
	tokenInfo *const token = newToken ();

	parseFile (token);

	deleteToken (token);
	deleteParserState (PS);

	vDebugParserPrintf ("\n");
}

static void initialize (const langType language)
{
	Lang_v = language;

	TokenPool = objPoolNew (
		16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
	addKeywordGroup (&VTypeKeywords, Lang_v);
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
	static selectLanguage selectors[] = { selectVOrVerilogByKeywords, NULL };
	def->kindTable = VKinds;
	def->kindCount = ARRAY_SIZE (VKinds);
	def->extensions = extensions;
	def->parser = findVTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->selectLanguage  = selectors;
	def->keywordTable = VKeywordTable;
	def->keywordCount = ARRAY_SIZE (VKeywordTable);
	def->fieldTable = VFields;
	def->fieldCount = ARRAY_SIZE (VFields);
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	def->requestAutomaticFQTag = true;
	return def;
}
