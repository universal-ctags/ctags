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

//#define DEBUG_TOKENISER

#define NARGS_SEQ(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
#define NARGS(...) NARGS_SEQ (__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))
#define vStringAccumulate(a, b) \
	do{ vStringCat (a, b); vStringClear (b); }while(0)
#define isToken(token, ...) \
	_isToken (token, false, NARGS (__VA_ARGS__), __VA_ARGS__)
#define expectedToken(token, ...) \
	_isToken (token, true, NARGS (__VA_ARGS__), __VA_ARGS__)
#define isKeyword(a, b) ((a)->keyword == (b))
#define isWhitespace(c) (c == ' ' || c == '\t' || c == '\r' || c == '\n')
#define isIdentInitial(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || \
                          c == '_')
#define isIdentSubsequent(c) (isIdentInitial (c) || (c >= '0' && c <= '9'))
#define unreadToken() (unreadTokenFull (NULL))

enum {
	KEYWORD_fn,
	KEYWORD_map,
	KEYWORD_or,
	KEYWORD_module,
	KEYWORD_as,
	KEYWORD_import,
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
		"isize", "float_literal", "int_literal", "thread", "IError",
		NULL
	},
};

const static struct keywordGroup VRestKeywords = {
	.value = KEYWORD_REST,
	.addingUnlessExisting = true,
	.keywords = {
		"pub", "const", "mut", "shared", "static", "__global", "assert", "as",
		"go", "spawn", "asm", "return", "type", "for", "in", "is", "if", "else",
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
};

typedef enum eTokenType {
	TOKEN_NONE,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
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
	/* TOKEN_SEMICOLON, */
	/* TOKEN_STAR, */
	/* TOKEN_EQUAL, */
	/* TOKEN_3DOTS, */
	TOKEN_EOF,
	COUNT_TOKEN
} tokenType;

static char *const tokenNames[COUNT_TOKEN] = {
	"NONE"
	"keyword",
	"identifier",
	"immediate",
	"open_paren",
	"close_paren",
	"open_curly",
	"close_curly",
	"open_square",
	"close_square",
	"declare",
	"operator",
	"colon",
	"exclamation",
	"question",
	"dot",
	"comma",
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
	KIND_FUNCTION,
	KIND_MODULE,
	/* K_CONST, */
	/* K_TYPE, */
	/* K_VAR, */
	/* K_STRUCT, */
	/* K_INTERFACE, */
	/* K_MODULE, */
	/* K_ALIAS, */
	/* K_RECEIVER, */
	KIND_UNKNOWN,
	COUNT_KIND
} kindType;

static kindDefinition VKinds[COUNT_KIND] = {
	{true, 'f', "fn", "functions"},
	{true, 'm', "module", "modules",
	 .referenceOnly = false, ATTACH_ROLES (VModuleRoles)},
	/* {true, 'c', "const", "constants"}, */
	/* {true, 't', "type", "types"}, */
	/* {true, 'v', "var", "variables"}, */
	/* {true, 's', "struct", "structs"}, */
	/* {true, 'i', "interface", "interfaces"}, */
	/* {true, 'm', "module", "modules"}, */
	/* {true, 'a', "alias", "type aliases"}, */
	/* {false,'R', "receiver", "receivers"}, */
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
	unsigned long lineNumber;	/* line number of tag */
	MIOPos filePosition;		/* file position of line containing name */
} tokenInfo;

static langType Lang_v;
static bool ReplayToken = false;
static vString *ReplayCapture = NULL;
//static tokenInfo *NextToken = NULL;
/* static NestingLevels *VNestingLevels = NULL; */
static objPool *TokenPool = NULL;
//static int currentScope = CORK_NIL;

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

/* static void parseStringOrRune (vString *const string, const int delimiter) */
/* { */
/* 	bool end = false; */
/* 	while (!end) */
/* 	{ */
/* 		int c = getcFromInputFile (); */
/* 		if (c == EOF) */
/* 			end = true; */
/* 		else if (c == '\\' && delimiter != '`') */
/* 		{ */
/* 			c = getcFromInputFile (); */
/* 			if (c != '\'' && c != '\"') */
/* 				vStringPut (string, '\\'); */
/* 			vStringPut (string, c); */
/* 		} */
/* 		else if (c == delimiter) */
/* 			end = true; */
/* 		else */
/* 			vStringPut (string, c); */
/* 	} */
/* } */

static void parseBlock (tokenInfo *const token, int scope);

static int getcFromInputFileAndKeepIfEq (int expect, vString *const capture)
{
	int c = getcFromInputFile ();
	if (c != expect)
		ungetcToInputFile (c);
	else if (capture)
		vStringPut (capture, c);
	return c;
}

static void readTokenFull (tokenInfo *const token, vString *capture)
{
	int c;

	if (ReplayToken)
	{
		ReplayToken = false;
		if (capture && ReplayCapture)
			vStringCat (capture, ReplayCapture);
		vStringDelete (ReplayCapture);
		return;
	}

	do
		c = getcFromInputFile ();
	while (c != EOF && isWhitespace (c));

	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

#ifdef DEBUG_TOKENISER
	vString *const oldCapture = capture;
	capture = vStringNew ();
#endif

	vStringClear (token->string);
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
	else if (c == '.')
		token->type = TOKEN_DOT;
	else if (c == '!')
	{
		if (getcFromInputFileAndKeepIfEq ('=', capture) == '=')
			token->type = TOKEN_OPERATOR; // !=
		else
			token->type = TOKEN_EXCLAMATION; // !
	}
	else if (c == ':')
	{
		if (getcFromInputFileAndKeepIfEq ('=', capture) == '=')
			token->type = TOKEN_DECLARE; // :=
		else
			token->type = TOKEN_COLON; // :
	}
	else if (c == '=' || c == '<' || c == '>')
	{
		getcFromInputFileAndKeepIfEq ('=', capture);
		token->type = TOKEN_OPERATOR; // = == < <= > >=
	}
	else if (c == ',')
    {
	    token->type = TOKEN_COMMA;
	    if (capture)
		    vStringPut (capture, ' ');
    }
	else if (c == '"' || c == '\'' || c == '`')
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
		if (isKeyword (token, KEYWORD_NONE))
			token->type = TOKEN_IDENTIFIER;
		else
			token->type = TOKEN_KEYWORD;
	}

#ifdef DEBUG_TOKENISER
	printf ("%s[%s]", tokenNames[token->type], vStringValue (capture));
	if (oldCapture)
		vStringCat (oldCapture, capture);
	vStringDelete (capture);
#endif
}

static void unreadTokenFull (vString *const acc)
{
	ReplayToken = true;
	ReplayCapture = acc? vStringNewCopy (acc) : ReplayCapture;
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, NULL);
}

static bool _isToken (tokenInfo *const token, bool expected, int nTypes, ...)
{
	va_list argp;
	va_start (argp, nTypes);
	bool found = false;
	for (int i = 0; i < nTypes; i++)
		if (token->type == va_arg (argp, tokenType))
			found = true;
	va_end (argp);
	DebugStatement (
		if (expected && !found)
			debugPrintf (DEBUG_PARSE, "UNEXPECTED TOKEN AT LINE %lu: %s",
			             token->lineNumber, tokenNames[token->type]);
	);
	return found;
}

/* static bool expectedToken (tokenInfo *const token, ...) */
/* { */
/* 	bool found = isToken (token, ...); */
/* 	DebugStatement ( */
/* 		if (!found) */
/* 			debugPrintf (DEBUG_PARSE, "UNEXPECTED TOKEN AT LINE %d: %s", */
/* 			             token->lineNumber, tokenNames[expected]); */
/* 	); */
/* 	return found; */
/* } */

/* static void readExpectedToken (tokenInfo *const token, tokenType expect) */
/* { */
/* 	readToken (token); */
/* 	expectedToken (token, expect); */
/* } */

static int makeTagFull (tokenInfo *const token, vString *const name,
                        const kindType kind, const int scope, int role,
                        vString *const argList, vString *retType)
{
	tagEntryInfo e;

	/* /\* Don't record `_' placeholder variable  *\/ */
	/* if (kind == GOTAG_VAR && name[0] == '_' && name[1] == '\0') */
	/* 	return CORK_NIL; */

	const char *const tag = vStringValue (name? name : token->string);
	initRefTagEntry (&e, tag, kind, role);

	e.lineNumber = token->lineNumber;
	e.filePosition = token->filePosition;
	if (argList && !vStringIsEmpty (argList))
		e.extensionFields.signature = vStringValue (argList);
	if (retType)
	{
		e.extensionFields.typeRef [0] = "typename";
		e.extensionFields.typeRef [1] = vStringValue( retType );
	}
	e.extensionFields.scopeIndex = scope;
	return makeTagEntry (&e);
}

static int makeFunctionTag (tokenInfo *const token, const int scope,
                            vString *const argList, vString *const retType)
{
	return makeTagFull (token, NULL, KIND_FUNCTION, scope,
	                    ROLE_DEFINITION_INDEX, argList, retType);
}

static int makeTag (tokenInfo *const token, vString *const name, kindType kind,
                    int scope)
{
	return makeTagFull (token, name, kind, scope, ROLE_DEFINITION_INDEX, NULL,
	                    NULL);
}

static int makeReferenceTag (tokenInfo *const token, vString *const name,
                             kindType kind, int scope, int role)
{
	return makeTagFull (token, name, kind, scope, role, NULL, NULL);
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
static void parseIdentifier (tokenInfo *const token, vString *const capture,
                             vString *const identifier)
{
	vString *const acc = capture? vStringNew () : NULL;
	do
	{
		if (acc)
			vStringAccumulate (capture, acc);
		if (identifier)
		{
			if (isToken (token, TOKEN_DOT))
				vStringPut (identifier, '.');
			else
				vStringCat (identifier, token->string);
		}
		readTokenFull (token, acc);
	} while (isToken (token, TOKEN_DOT, TOKEN_IDENTIFIER));
	unreadTokenFull (acc);
	vStringDelete (acc);
}

// '(' >*? ')'
static void parseArgs (tokenInfo *const token, vString *const capture)
{
	readTokenFull (token, capture);
	while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_PAREN))
	{
		/* if (isToken (token, TOKEN_IDENTIFIER)) */
		/* { */
		/* 	tokenInfo *const identToken = dupToken (token); */
		/* 	parseFQType (token, capture); */
		/* 	makeTag (identToken) */
		/* } */

		readTokenFull (token, capture);
	}
}

// ['!' | '?']? >[fqident | type ['[' type ']'] | 'map' '[' type ']' fqtype ]
static void parseFQType (tokenInfo *const token, vString *const capture)
{
	vString *const acc = vStringNew ();

	if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
	{
		vStringAccumulate (capture, acc);
		readTokenFull (token, acc);
	}

	if (isToken (token, TOKEN_IDENTIFIER) || isKeyword (token, KEYWORD_TYPE))
	{
		if (isToken (token, TOKEN_IDENTIFIER))
			parseIdentifier (token, acc, NULL);
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
		if (expectedToken (token, TOKEN_OPEN_SQUARE))
		{
			skipToClose (TOKEN_OPEN_SQUARE, acc);
            readTokenFull (token, acc);
            parseFQType (token, acc);
			vStringAccumulate (capture, acc);
		}
	}
	else
		unreadToken ();
}

// open >[expr [',' expr]*]? close
static void parseExpressionList (tokenInfo *const token)
{
	int nest = 1;
	tokenType open = token->type, close = getClose (token->type);
	do
	{
		readToken (token);
		if (isToken (token, open))
			nest++;
		else if (isToken (token, close))
			nest--;
	} while (!isToken (token, TOKEN_EOF) && nest > 0);
}

// fqident '(' >expr-list ')' ['!' | '?' | 'or' block]?
static void parseFnCall (tokenInfo *const token, vString *const name, int scope)
{
	parseExpressionList (token);
	readToken (token);
	if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION))
		readToken (token);
	else if (isKeyword (token, KEYWORD_or))
	{
		readToken (token);
		if (expectedToken (token, TOKEN_OPEN_CURLY))
			parseBlock (token, scope);
		else
			unreadToken ();
	}
}

// [fncall | ]?
static void parseStatement (tokenInfo *const token, int scope)
{
	if (isToken (token, TOKEN_IDENTIFIER))
	{
		vString *const identifier = vStringNew ();
		parseIdentifier (token, NULL, identifier);

		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN))
			parseFnCall (token, identifier, scope);
//			else if (isToken (token, TOKEN_DECLARE))

		vStringDelete (identifier);
	}
}

// '{' >statement* '}'
static void parseBlock (tokenInfo *const token, int scope)
{
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
}

// 'fn' >receiver? ident args? cargs? type? block
static void parseFn (tokenInfo *const token, int scope)
{
	readToken (token);

	/* if (isToken (token, TOKEN_OPEN_PAREN)) */
	/* { */
	/* 	parseReceiver (token); */
	/* 	readToken (token); */
	/* } */

	if (expectedToken (token, TOKEN_IDENTIFIER))
	{
		tokenInfo *fnToken = dupToken (token);
		vString *argList = vStringNew ();
		vString *retType = vStringNew ();
		vString *acc = vStringNew ();
		readTokenFull (token, acc);
		if (isToken (token, TOKEN_OPEN_SQUARE))
		{
			parseArgs (token, acc);
			vStringAccumulate (argList, acc);
			readTokenFull (token, acc);
		}
		if (isToken (token, TOKEN_OPEN_PAREN))
		{
			parseArgs (token, acc);
			vStringAccumulate (argList, acc);
			readTokenFull (token, acc);
		}

		if (isToken (token, TOKEN_EXCLAMATION, TOKEN_QUESTION, TOKEN_IDENTIFIER)
		    || isKeyword (token, KEYWORD_TYPE))
		{
			parseFQType (token, acc);
			vStringAccumulate (retType, acc);
		}
		vStringDelete (acc);

		int newScope = makeFunctionTag (fnToken, CORK_NIL, argList, retType);
		deleteToken (fnToken);
		vStringDelete (argList);
		vStringDelete (retType);

		if (expectedToken (token, TOKEN_OPEN_CURLY))
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
}

// 'module' >ident
static int parseModule (tokenInfo *const token)
{
	int scope = CORK_NIL;
	readToken (token);
	if (expectedToken (token, TOKEN_IDENTIFIER))
		scope = makeTag (token, NULL, KIND_MODULE, CORK_NIL);
	else
		unreadToken ();
	return scope;
}

// 'import' >fqident ['as' ident]? ['{' ident [',' ident]* '}']?
static void parseImport (tokenInfo *const token)
{
	readToken (token);
	if (expectedToken (token, TOKEN_IDENTIFIER))
	{
		vString *name = vStringNew ();
		parseIdentifier (token, NULL, name);
		readToken (token);
		if (isKeyword (token, KEYWORD_as))
		{
			readToken (token);
			if (expectedToken (token, TOKEN_IDENTIFIER))
				vStringCopy (name, token->string);
			readToken (token);
		}
		int moduleScope = makeReferenceTag (token, name, KIND_MODULE, CORK_NIL,
		                                    ROLE_IMPORTED_MODULE);
		if (isToken (token, TOKEN_OPEN_CURLY))
		{
			do
			{
				readToken (token);
				if (expectedToken (token, TOKEN_IDENTIFIER))
					makeReferenceTag (token, NULL, KIND_UNKNOWN, moduleScope,
					         ROLE_IMPORTED_SYMBOL);
			}
			while (!isToken (token, TOKEN_EOF, TOKEN_CLOSE_CURLY));
		}
		else
			unreadToken ();
	}
	else
		unreadToken ();
}

static void parseFile (tokenInfo *const token)
{
	int scope = CORK_NIL;
	do
	{
		readToken (token);
		if (isToken (token, TOKEN_OPEN_PAREN, TOKEN_OPEN_CURLY,
		             TOKEN_OPEN_SQUARE)) // attribtes
			skipToClose (token->type, NULL);
		else if (isKeyword (token, KEYWORD_module))
			scope = parseModule (token);
		else if (isKeyword (token, KEYWORD_fn))
			parseFn (token, scope);
		else if (isKeyword (token, KEYWORD_import))
			parseImport (token);
		/* token->keyword = lookupKeyword (vStringValue (token->string), Lang_python); */
		/* if (isKeyword (token, KEYWORD_NONE)) */
		/* 	token->type = TOKEN_IDENTIFIER; */
		/* else */
		/* 	token->type = TOKEN_KEYWORD; */
	}
	while (!isToken (token, TOKEN_EOF));

#ifdef DEBUG_TOKENISER
	printf ("\n");
#endif
}

static void findVTags (void)
{
	tokenInfo *const token = newToken ();

	parseFile (token);

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
