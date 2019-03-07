/*
 *	 Copyright (c) 2019, Karol Samborski
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating tags for TypeScript language
 *	 files.
 *
 *	 Reference: https://github.com/Microsoft/TypeScript/blob/master/doc/TypeScript%20Language%20Specification.pdf
 *
 */

/*
 *	 INCLUDE FILES
 */
#include <stdio.h>
#include <stdarg.h>

#include "general.h"	/* must always come first */
#include "parse.h"
#include "objpool.h"
#include "keyword.h"
#include "read.h"
#include "numarray.h"
#include "routines.h"
#include "entry.h"

#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '$' || \
		(c) == '@' || (c) == '_' || (c) == '#' || \
		(c) >= 0x80)

#define PARSER_DEF(fname, pfun, word, stype) \
inline static void parse##fname(const char c, tokenInfo *const token, void *state, parserResult *const result) \
{ \
	pfun(c, token, word, stype state, result); \
}

#define SINGLE_CHAR_PARSER_DEF(fname, ch, ttype) \
inline static void parse##fname(const char c, tokenInfo *const token, void *state, parserResult *const result) \
{ \
	parseOneChar(c, token, ch, ttype, result); \
}

#define WORD_TOKEN_PARSER_DEF(fname, w, ttype) \
inline static void parse##fname(const char c, tokenInfo *const token, void *state, parserResult *const result) \
{ \
	parseWordToken(c, token, w, ttype, (int*) state, result); \
}

#define BLOCK_PARSER_DEF(fname, start, end, ttype) \
inline static void parse##fname(const char c, tokenInfo *const token, void *state, parserResult *const result) \
{ \
	parseBlock(c, token, ttype, start, end, (blockState*) state, result); \
}

/*
 *	DATA DEFINITIONS
 */
static langType Lang_ts;
static objPool *TokenPool = NULL;

/*	Used to specify type of keyword.
*/
enum eKeywordId {
	KEYWORD_async,
	KEYWORD_break,
	KEYWORD_case,
	KEYWORD_catch,
	KEYWORD_class,
	KEYWORD_const,
	KEYWORD_constructor,
	KEYWORD_continue,
	KEYWORD_debugger,
	KEYWORD_default,
	KEYWORD_delete,
	KEYWORD_do,
	KEYWORD_else,
	KEYWORD_enum,
	KEYWORD_export,
	KEYWORD_extends,
	KEYWORD_false,
	KEYWORD_finally,
	KEYWORD_for,
	KEYWORD_function,
	KEYWORD_if,
	KEYWORD_implements,
	KEYWORD_import,
	KEYWORD_in,
	KEYWORD_instanceof,
	KEYWORD_interface,
	KEYWORD_let,
	KEYWORD_module,
	KEYWORD_namespace,
	KEYWORD_new,
	KEYWORD_null,
	KEYWORD_package,
	KEYWORD_private,
	KEYWORD_protected,
	KEYWORD_public,
	KEYWORD_return,
	KEYWORD_readonly,
	KEYWORD_static,
	KEYWORD_super,
	KEYWORD_switch,
	KEYWORD_this,
	KEYWORD_throw,
	KEYWORD_true,
	KEYWORD_try,
	KEYWORD_type,
	KEYWORD_typeof,
	KEYWORD_var,
	KEYWORD_void,
	KEYWORD_while,
	KEYWORD_with,
	KEYWORD_yield
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_CHARACTER,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_QUESTION_MARK,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_TEMPLATE,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_EQUAL_SIGN,
	TOKEN_REGEXP,
	TOKEN_STAR,
	TOKEN_NL,
	TOKEN_COMMENT_BLOCK,
	TOKEN_PARENS,
	TOKEN_SQUARES,
	TOKEN_PIPE,
	TOKEN_AMPERSAND,
	TOKEN_ARROW,
	TOKEN_NUMBER,
	TOKEN_AT
} tokenType;

typedef enum {
	TSTAG_FUNCTION,
	TSTAG_CLASS,
	TSTAG_INTERFACE,
	TSTAG_ENUM,
	TSTAG_METHOD,
	TSTAG_NAMESPACE,
	TSTAG_PROPERTY,
	TSTAG_VARIABLE,
	TSTAG_GENERATOR,
	TSTAG_ALIAS
} tsKind;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	tsKind			scopeParentKind;
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
	keywordId		accessKeyword;
} tokenInfo;

typedef struct sCommentState {
	int  parsed;
	int  blockParsed;
	bool isBlock;
} commentState;

typedef struct sBlockState {
	int  parsed;
	int  nestLevel;
} blockState;

static tokenType LastTokenType;
static tokenInfo *NextToken;

static const keywordTable TsKeywordTable [] = {
	/* keyword		keyword ID */
	{ "async"      , KEYWORD_async       },
	{ "break"      , KEYWORD_break       },
	{ "case"       , KEYWORD_case        },
	{ "catch"      , KEYWORD_catch       },
	{ "class"      , KEYWORD_class       },
	{ "const"      , KEYWORD_const       },
	{ "constructor", KEYWORD_constructor },

	{ "continue"   , KEYWORD_continue    },
	{ "debugger"   , KEYWORD_debugger    },
	{ "default"    , KEYWORD_default     },
	{ "delete"     , KEYWORD_delete      },
	{ "do"         , KEYWORD_do          },
	{ "else"       , KEYWORD_else        },
	{ "enum"       , KEYWORD_enum        },
	{ "export"     , KEYWORD_export      },
	{ "extends"    , KEYWORD_extends     },
	{ "false"      , KEYWORD_false       },
	{ "finally"    , KEYWORD_finally     },
	{ "for"        , KEYWORD_for         },
	{ "function"   , KEYWORD_function    },
	{ "if"         , KEYWORD_if          },
	{ "implements" , KEYWORD_implements  },
	{ "import"     , KEYWORD_import      },
	{ "in"         , KEYWORD_in          },
	{ "instanceof" , KEYWORD_instanceof  },
	{ "interface"  , KEYWORD_interface   },
	{ "let"        , KEYWORD_let         },
	{ "module"     , KEYWORD_module      },
	{ "namespace"  , KEYWORD_namespace   },
	{ "new"        , KEYWORD_new         },
	{ "null"       , KEYWORD_null        },
	{ "package"    , KEYWORD_package     },
	{ "private"    , KEYWORD_private     },
	{ "protected"  , KEYWORD_protected   },
	{ "public"     , KEYWORD_public      },
	{ "return"     , KEYWORD_return      },
	{ "static"     , KEYWORD_static      },
	{ "super"      , KEYWORD_super       },
	{ "switch"     , KEYWORD_switch      },
	{ "this"       , KEYWORD_this        },
	{ "throw"      , KEYWORD_throw       },
	{ "true"       , KEYWORD_true        },
	{ "try"        , KEYWORD_try         },
	{ "type"       , KEYWORD_type        },
	{ "typeof"     , KEYWORD_typeof      },
	{ "var"        , KEYWORD_var         },
	{ "void"       , KEYWORD_void        },
	{ "while"      , KEYWORD_while       },
	{ "with"       , KEYWORD_with        },
	{ "yield"      , KEYWORD_yield       }
};

static kindDefinition TsKinds [] = {
	{ true,  'f', "function",	  "functions"		   },
	{ true,  'c', "class",		  "classes"			   },
	{ true,  'i', "interface",	  "interfaces"		   },
	{ true,  'e', "enum",		  "enums"			   },
	{ true,  'm', "method",		  "methods"			   },
	{ true,  'n', "namespace",	  "namespaces"		   },
	{ true,  'p', "property",	  "properties"		   },
	{ true,  'v', "variable",	  "global variables"   },
	{ true,  'g', "generator",	  "generators"		   },
	{ true,	 'a', "alias",		  "aliases",		   }
};

typedef enum eParserResult {
	PARSER_FINISHED,
	PARSER_NEEDS_MORE_INPUT,
	PARSER_FAILED
} parserResultStatus;

typedef struct sParserResult {
	parserResultStatus status;
	unsigned int       unusedChars;
} parserResult;

typedef void (*Parser)(const char c, tokenInfo *const, void *state, parserResult *const);
typedef void* (*ParserStateInit)();
typedef void (*ParserStateFree)(void *);

static bool tryParser(Parser parser, ParserStateInit stInit, ParserStateFree stFree, tokenInfo *const token);

static void emitTag(const tokenInfo *const token, const tsKind kind)
{
	if (!TsKinds [kind].enabled)
		return;

	static const char *const access[3] = {
		"private",
		"protected",
		"public"
	};

	const char *name = vStringValue (token->string);
	tagEntryInfo e;

	initTagEntry (&e, name, kind);
	e.lineNumber   = token->lineNumber;
	e.filePosition = token->filePosition;

	if (token->scope && vStringLength(token->scope) > 0) {
		e.extensionFields.scopeKindIndex = token->scopeParentKind;
		e.extensionFields.scopeName = vStringValue (token->scope);
	}

	switch (token->accessKeyword) {
		case KEYWORD_public:
			e.extensionFields.access = access[2];
			break;
		case KEYWORD_protected:
			e.extensionFields.access = access[1];
			break;
		case KEYWORD_private:
			e.extensionFields.access = access[0];
			break;
	}

	makeTagEntry (&e);
}

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->string = vStringNew ();

	return token;
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type			   = TOKEN_UNDEFINED;
	token->keyword		   = KEYWORD_NONE;
	token->lineNumber      = getInputLineNumber ();
	token->filePosition    = getInputFilePosition ();
	token->scope           = vStringNew ();
	token->scopeParentKind = TSTAG_CLASS;
	token->accessKeyword   = KEYWORD_NONE;
	vStringClear (token->string);
	vStringClear (token->scope);
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	if (token->string) vStringDelete (token->string);
	if (token->scope) vStringDelete (token->scope);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src,
					   bool scope)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy(dest->string, src->string);
	if (scope)
		vStringCopy(dest->scope, src->scope);
}

inline static bool whiteChar(const char c)
{
	return c == ' ' || c == '\r' || c == '\t';
}

inline static void parseOneChar(const char c, tokenInfo *const token, const char expected, const tokenType type, parserResult *const result)
{
	if (whiteChar(c)) {
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (c != expected) {
		result->status = PARSER_FAILED;
		return;
	}

	token->type = type;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	result->status = PARSER_FINISHED;
}

inline static void parseChar(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	if (whiteChar(c)) {
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (c == EOF) {
		result->status = PARSER_FAILED;
		return;
	}

	token->type = TOKEN_CHARACTER;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	result->status = PARSER_FINISHED;
}

inline static void* initParseWordState()
{
	int *num = xMalloc (1, int);
	*num = 0;

	return num;
}

inline static void freeParseWordState(void *state)
{
	eFree ((int *) state);
}

inline static void parseWord(const char c, tokenInfo *const token, const char* word, int* parsed, parserResult *const result)
{
	if (*parsed == 0 && whiteChar (c))
	{
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (word[*parsed] == '\0')
	{
		if (isIdentChar (c))
		{
			result->status = PARSER_FAILED;
			return;
		}

		vStringCatS (token->string, word);
		token->type = TOKEN_KEYWORD;
		token->keyword = lookupKeyword (vStringValue (token->string), Lang_ts);
		token->lineNumber   = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();

		result->unusedChars = 1;
		result->status = PARSER_FINISHED;
		return;
	}

	if (c == word[*parsed])
	{
		*parsed += 1;

		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	result->status = PARSER_FAILED;
}

inline static void parseNumber(const char c, tokenInfo *const token, int* parsed, parserResult *const result)
{
	if (*parsed == 0)
	{
		result->status = PARSER_NEEDS_MORE_INPUT;

		if (whiteChar (c)) return;
		else if (c == '-')
		{
			*parsed += 1;
			return;
		}
	}

	if (isdigit (c))
	{
		result->status = PARSER_NEEDS_MORE_INPUT;
		*parsed += 1;
		return;
	}
	else if (*parsed == 0)
	{
		result->status = PARSER_FAILED;
		return;
	}

	token->type = TOKEN_NUMBER;
	token->keyword = KEYWORD_NONE;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	result->unusedChars = 1;
	result->status = PARSER_FINISHED;
}

inline static void parseWordToken(const char c, tokenInfo *const token, const char* word, const tokenType type, int* parsed, parserResult *const result)
{
	if (*parsed == 0 && whiteChar(c))
	{
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (c == word[*parsed])
	{
		*parsed += 1;

		if (word[*parsed] == '\0') {
			token->type = type;
			token->keyword = KEYWORD_NONE;
			token->lineNumber   = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			result->status = PARSER_FINISHED;
			return;
		}

		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	result->status = PARSER_FAILED;
}

inline static void* initParseCommentState()
{
	commentState *st = xMalloc (1, commentState);
	st->parsed = 0;
	st->blockParsed = 0;
	st->isBlock = false;

	return st;
}

inline static void freeParseCommentState(void *state)
{
	eFree ((commentState *) state);
}

inline static void parseComment(const char c, tokenInfo *const token, commentState* state, parserResult *const result)
{
	if (state->parsed == 0 && whiteChar(c)) {
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (state->parsed < 2) {
		parseWordToken (c, token, "//", TOKEN_COMMENT_BLOCK, &state->parsed, result);

		if (result->status == PARSER_FAILED) {
			parseWordToken (c, token, "/*", TOKEN_COMMENT_BLOCK, &state->parsed, result);
			if (result->status == PARSER_FINISHED) {
				result->status = PARSER_NEEDS_MORE_INPUT;
				state->isBlock = true;
			}
		} else if (result->status == PARSER_FINISHED) {
			result->status = PARSER_NEEDS_MORE_INPUT;
			state->isBlock = false;
		}

		return;
	}

	state->parsed += 1;

	if (c == EOF) result->status = PARSER_FINISHED;
	else if (state->isBlock)
	{
		parseWordToken (c, token, "*/", TOKEN_COMMENT_BLOCK, &state->blockParsed, result);

		if (result->status == PARSER_FAILED) {
			state->blockParsed = c == '*' ? 1 : 0;
			result->status = PARSER_NEEDS_MORE_INPUT;
		}
	}
	else if (c == '\n') result->status = PARSER_FINISHED;

	if (result->status == PARSER_FINISHED) {
		token->type = TOKEN_COMMENT_BLOCK;
		token->keyword = KEYWORD_NONE;
		token->lineNumber   = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();

		return;
	}

	result->status = PARSER_NEEDS_MORE_INPUT;
}

inline static void* initParseStringState()
{
	char *st = xMalloc (1, char);
	*st = '\0';

	return st;
}

inline static void freeParseStringState(void *state)
{
	eFree ((char *) state);
}

inline static void parseString(const char c, tokenInfo *const token, const char quote, char* prev, parserResult *const result)
{
	if (*prev == '\0') {
		if (whiteChar (c)) {
			result->status = PARSER_NEEDS_MORE_INPUT;
			return;
		} else if (c == quote) {
			*prev = c;
			result->status = PARSER_NEEDS_MORE_INPUT;
		} else {
			result->status = PARSER_FAILED;

			return;
		}
	}

	if (c == quote) {
		if (*prev == '\\') {
			*prev = c;
			result->status = PARSER_NEEDS_MORE_INPUT;
		} else {
			result->status = PARSER_FINISHED;

			token->type = TOKEN_STRING;
			token->keyword = KEYWORD_NONE;
			token->lineNumber   = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();

			return;
		}
	}

	*prev = c;
	result->status = PARSER_NEEDS_MORE_INPUT;
}

inline static void* initBlockState()
{
	blockState *st = xMalloc (1, blockState);
	st->parsed = 0;
	st->nestLevel = 0;

	return st;
}

inline static void freeBlockState(void *state)
{
	eFree ((blockState *) state);
}

inline static void parseBlock(const char c, tokenInfo *const token, tokenType const ttype, const char start, const char end, blockState* state, parserResult *const result)
{
	if (state->parsed == 0)
	{
		if (whiteChar (c))
		{
			result->status = PARSER_NEEDS_MORE_INPUT;
			return;
		}

		if (c != start)
		{
			result->status = PARSER_FAILED;
			return;
		}
	}

	if (c == start) state->nestLevel += 1;
	else if (c == end) state->nestLevel -= 1;
	else if (c == ';' || c == EOF)
	{
		result->status = PARSER_FAILED;
		return;
	}

	//skip comments:
	tryParser ((Parser) parseComment, initParseCommentState, freeParseCommentState, token);

	state->parsed += 1;

	if (state->nestLevel <= 0)
	{
		token->type = ttype;
		token->keyword = KEYWORD_NONE;
		token->lineNumber   = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
		result->status = PARSER_FINISHED;

		return;
	}

	result->status = PARSER_NEEDS_MORE_INPUT;
}

inline static void parseIdentifier(const char c, tokenInfo *const token, int *parsed, parserResult *const result)
{
	if (*parsed == 0 && whiteChar(c)) {
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (isIdentChar(c)) {
		vStringPut (token->string, c);
		*parsed = *parsed + 1;
		result->status = PARSER_NEEDS_MORE_INPUT;

		return;
	}

	if (*parsed > 0) {
		token->type = TOKEN_IDENTIFIER;
		token->lineNumber   = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
		result->status = PARSER_FINISHED;
		result->unusedChars = 1;
		return;
	}

	result->status = PARSER_FAILED;
}

PARSER_DEF(AsyncKeyword      , parseWord , "async"      , (int*))
PARSER_DEF(BreakKeyword      , parseWord , "break"      , (int*))
PARSER_DEF(CaseKeyword       , parseWord , "case"       , (int*))
PARSER_DEF(CatchKeyword      , parseWord , "catch"      , (int*))
PARSER_DEF(ClassKeyword      , parseWord , "class"      , (int*))
PARSER_DEF(ConstKeyword      , parseWord , "const"      , (int*))
PARSER_DEF(ConstructorKeyword, parseWord , "constructor", (int*))
PARSER_DEF(ContinueKeyword   , parseWord , "continue"   , (int*))
PARSER_DEF(DebuggerKeyword   , parseWord , "debugger"   , (int*))
PARSER_DEF(DefaultKeyword    , parseWord , "default"    , (int*))
PARSER_DEF(DeleteKeyword     , parseWord , "delete"     , (int*))
PARSER_DEF(DoKeyword         , parseWord , "do"         , (int*))
PARSER_DEF(ElseKeyword       , parseWord , "else"       , (int*))
PARSER_DEF(EnumKeyword       , parseWord , "enum"       , (int*))
PARSER_DEF(ExportKeyword     , parseWord , "export"     , (int*))
PARSER_DEF(ExtendsKeyword    , parseWord , "extends"    , (int*))
PARSER_DEF(FalseKeyword      , parseWord , "false"      , (int*))
PARSER_DEF(FinallyKeyword    , parseWord , "finally"    , (int*))
PARSER_DEF(ForKeyword        , parseWord , "for"        , (int*))
PARSER_DEF(FunctionKeyword   , parseWord , "function"   , (int*))
PARSER_DEF(IfKeyword         , parseWord , "if"         , (int*))
PARSER_DEF(ImplementsKeyword , parseWord , "implements" , (int*))
PARSER_DEF(ImportKeyword     , parseWord , "import"     , (int*))
PARSER_DEF(InKeyword         , parseWord , "in"         , (int*))
PARSER_DEF(InstanceofKeyword , parseWord , "instanceof" , (int*))
PARSER_DEF(InterfaceKeyword  , parseWord , "interface"  , (int*))
PARSER_DEF(LetKeyword        , parseWord , "let"        , (int*))
PARSER_DEF(ModuleKeyword     , parseWord , "module"     , (int*))
PARSER_DEF(NamespaceKeyword  , parseWord , "namespace"  , (int*))
PARSER_DEF(NewKeyword        , parseWord , "new"        , (int*))
PARSER_DEF(NullKeyword       , parseWord , "null"       , (int*))
PARSER_DEF(PackageKeyword    , parseWord , "package"    , (int*))
PARSER_DEF(PrivateKeyword    , parseWord , "private"    , (int*))
PARSER_DEF(ProtectedKeyword  , parseWord , "protected"  , (int*))
PARSER_DEF(PublicKeyword     , parseWord , "public"     , (int*))
PARSER_DEF(ReadonlyKeyword   , parseWord , "readonly"   , (int*))
PARSER_DEF(ReturnKeyword     , parseWord , "return"     , (int*))
PARSER_DEF(StaticKeyword     , parseWord , "static"     , (int*))
PARSER_DEF(SuperKeyword      , parseWord , "super"      , (int*))
PARSER_DEF(SwitchKeyword     , parseWord , "switch"     , (int*))
PARSER_DEF(ThisKeyword       , parseWord , "this"       , (int*))
PARSER_DEF(ThrowKeyword      , parseWord , "throw"      , (int*))
PARSER_DEF(TrueKeyword       , parseWord , "true"       , (int*))
PARSER_DEF(TryKeyword        , parseWord , "try"        , (int*))
PARSER_DEF(TypeKeyword       , parseWord , "type"       , (int*))
PARSER_DEF(TypeofKeyword     , parseWord , "typeof"     , (int*))
PARSER_DEF(VarKeyword        , parseWord , "var"        , (int*))
PARSER_DEF(VoidKeyword       , parseWord , "void"       , (int*))
PARSER_DEF(WhileKeyword      , parseWord , "while"      , (int*))
PARSER_DEF(WithKeyword       , parseWord , "with"       , (int*))
PARSER_DEF(YieldKeyword      , parseWord , "yield"      , (int*))

SINGLE_CHAR_PARSER_DEF(Semicolon    , ';'  , TOKEN_SEMICOLON)
SINGLE_CHAR_PARSER_DEF(Comma        , ','  , TOKEN_COMMA)
SINGLE_CHAR_PARSER_DEF(Colon        , ':'  , TOKEN_COLON)
SINGLE_CHAR_PARSER_DEF(Period       , '.'  , TOKEN_PERIOD)
SINGLE_CHAR_PARSER_DEF(OpenCurly    , '{'  , TOKEN_OPEN_CURLY)
SINGLE_CHAR_PARSER_DEF(CloseCurly   , '}'  , TOKEN_CLOSE_CURLY)
SINGLE_CHAR_PARSER_DEF(OpenParen    , '('  , TOKEN_OPEN_PAREN)
SINGLE_CHAR_PARSER_DEF(CloseParen   , ')'  , TOKEN_CLOSE_PAREN)
SINGLE_CHAR_PARSER_DEF(OpenSquare   , '['  , TOKEN_OPEN_SQUARE)
SINGLE_CHAR_PARSER_DEF(CloseSquare  , ']'  , TOKEN_CLOSE_SQUARE)
SINGLE_CHAR_PARSER_DEF(EqualSign    , '='  , TOKEN_EQUAL_SIGN)
SINGLE_CHAR_PARSER_DEF(Star         , '*'  , TOKEN_STAR)
SINGLE_CHAR_PARSER_DEF(At           , '@'  , TOKEN_AT)
SINGLE_CHAR_PARSER_DEF(NewLine      , '\n' , TOKEN_NL)
SINGLE_CHAR_PARSER_DEF(QuestionMark , '?'  , TOKEN_QUESTION_MARK)
SINGLE_CHAR_PARSER_DEF(EOF          , EOF  , TOKEN_EOF)
SINGLE_CHAR_PARSER_DEF(Pipe         , '|'  , TOKEN_PIPE)
SINGLE_CHAR_PARSER_DEF(Ampersand    , '&'  , TOKEN_AMPERSAND)

WORD_TOKEN_PARSER_DEF(Arrow         , "=>" , TOKEN_ARROW)

PARSER_DEF(StringSQuote   , parseString , '\'' , (char*))
PARSER_DEF(StringDQuote   , parseString , '"'  , (char*))
PARSER_DEF(StringTemplate , parseString , '`'  , (char*))

BLOCK_PARSER_DEF(Parens, '(', ')', TOKEN_PARENS)
BLOCK_PARSER_DEF(Squares, '[', ']', TOKEN_SQUARES)
BLOCK_PARSER_DEF(Template, '<', '>', TOKEN_TEMPLATE)

static bool tryParser(Parser parser, ParserStateInit stInit, ParserStateFree stFree, tokenInfo *const token)
{
	void *currentState = NULL;
	parserResult result;
	result.status = PARSER_NEEDS_MORE_INPUT;
	result.unusedChars = 0;
	charArray *usedC = charArrayNew();
	char c;

	if (stInit)
		currentState = stInit();

	while (result.status == PARSER_NEEDS_MORE_INPUT) {
		c = getcFromInputFile();
		parser(c, token, currentState, &result);
		charArrayAdd(usedC, c);
	}

	if (stFree)
		stFree(currentState);

	if (result.status == PARSER_FAILED) {
		while (charArrayCount(usedC) > 0) {
			ungetcToInputFile(charArrayLast(usedC));
			charArrayRemoveLast(usedC);
		}
	} else {
		while (result.unusedChars > 0) {
			ungetcToInputFile(charArrayLast(usedC));
			charArrayRemoveLast(usedC);
			result.unusedChars--;
		}
	}

	charArrayDelete(usedC);

	return result.status == PARSER_FINISHED;
}

static bool tryParse(tokenInfo *const token, ...)
{
	Parser currentParser = NULL;
	ParserStateInit stInit;
	ParserStateFree stFree;
	bool result = false;

	va_list args;
	va_start(args, token);

	currentParser = va_arg(args, Parser);
	while (!result && currentParser) {
		stInit = va_arg(args, ParserStateInit);
		stFree = va_arg(args, ParserStateFree);
		result = tryParser(currentParser, stInit, stFree, token);
		currentParser = va_arg(args, Parser);
	}

	va_end(args);

	return result;
}

static void readToken (tokenInfo *const token)
{
	clearPoolToken (token);

	bool parsed = tryParse(token,
			parseNumber, initParseWordState, freeParseWordState,
			parseSemicolon, NULL, NULL,
			parseComma, NULL, NULL,
			parseQuestionMark, NULL, NULL,
			parseColon, NULL, NULL,
			parsePeriod, NULL, NULL,
			parseOpenCurly, NULL, NULL,
			parseCloseCurly, NULL, NULL,
			parseArrow, initParseWordState, freeParseWordState,
			parseEqualSign, NULL, NULL,
			parseStar, NULL, NULL,
			parseAt, NULL, NULL,
			parseNewLine, NULL, NULL,
			parseEOF, NULL, NULL,
			parsePipe, NULL, NULL,
			parseAmpersand, NULL, NULL,
			parseSquares, initBlockState, freeBlockState,
			parseParens, initBlockState, freeBlockState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseTemplate, initBlockState, freeBlockState,
			parseStringSQuote, initParseStringState, freeParseStringState,
			parseStringDQuote, initParseStringState, freeParseStringState,
			parseStringTemplate, initParseStringState, freeParseStringState,

			parseBreakKeyword, initParseWordState, freeParseWordState,
			parseCaseKeyword, initParseWordState, freeParseWordState,
			parseCatchKeyword, initParseWordState, freeParseWordState,
			parseClassKeyword, initParseWordState, freeParseWordState,
			parseConstKeyword, initParseWordState, freeParseWordState,
			parseContinueKeyword, initParseWordState, freeParseWordState,
			parseDebuggerKeyword, initParseWordState, freeParseWordState,
			parseDefaultKeyword, initParseWordState, freeParseWordState,
			parseDeleteKeyword, initParseWordState, freeParseWordState,
			parseDoKeyword, initParseWordState, freeParseWordState,
			parseElseKeyword, initParseWordState, freeParseWordState,
			parseEnumKeyword, initParseWordState, freeParseWordState,
			parseExportKeyword, initParseWordState, freeParseWordState,
			parseExtendsKeyword, initParseWordState, freeParseWordState,
			parseFalseKeyword, initParseWordState, freeParseWordState,
			parseFinallyKeyword, initParseWordState, freeParseWordState,
			parseForKeyword, initParseWordState, freeParseWordState,
			parseFunctionKeyword, initParseWordState, freeParseWordState,
			parseIfKeyword, initParseWordState, freeParseWordState,
			parseImplementsKeyword, initParseWordState, freeParseWordState,
			parseImportKeyword, initParseWordState, freeParseWordState,
			parseInstanceofKeyword, initParseWordState, freeParseWordState,
			parseInterfaceKeyword, initParseWordState, freeParseWordState,
			parseInKeyword, initParseWordState, freeParseWordState,
			parseLetKeyword, initParseWordState, freeParseWordState,
			parseModuleKeyword, initParseWordState, freeParseWordState,
			parseNamespaceKeyword, initParseWordState, freeParseWordState,
			parseNewKeyword, initParseWordState, freeParseWordState,
			parseNullKeyword, initParseWordState, freeParseWordState,
			parsePackageKeyword, initParseWordState, freeParseWordState,
			parsePrivateKeyword, initParseWordState, freeParseWordState,
			parseProtectedKeyword, initParseWordState, freeParseWordState,
			parsePublicKeyword, initParseWordState, freeParseWordState,
			parseReadonlyKeyword, initParseWordState, freeParseWordState,
			parseReturnKeyword, initParseWordState, freeParseWordState,
			parseStaticKeyword, initParseWordState, freeParseWordState,
			parseSuperKeyword, initParseWordState, freeParseWordState,
			parseSwitchKeyword, initParseWordState, freeParseWordState,
			parseThisKeyword, initParseWordState, freeParseWordState,
			parseThrowKeyword, initParseWordState, freeParseWordState,
			parseTrueKeyword, initParseWordState, freeParseWordState,
			parseTryKeyword, initParseWordState, freeParseWordState,
			parseTypeofKeyword, initParseWordState, freeParseWordState,
			parseTypeKeyword, initParseWordState, freeParseWordState,
			parseVarKeyword, initParseWordState, freeParseWordState,
			parseVoidKeyword, initParseWordState, freeParseWordState,
			parseWhileKeyword, initParseWordState, freeParseWordState,
			parseWithKeyword, initParseWordState, freeParseWordState,
			parseYieldKeyword, initParseWordState, freeParseWordState,

			parseIdentifier, initParseWordState, freeParseWordState,
			parseChar, NULL, NULL,
			NULL);

	if (parsed)
		LastTokenType = token->type;
}

static void parseDecorator (tokenInfo *const token)
{
	bool parsed = false;

	do {
		clearPoolToken (token);
		parsed = tryParse(token,
			parseAt, NULL, NULL,
			parseNewLine, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			NULL);
	} while (parsed && token->type != TOKEN_PARENS);

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);
	} while (parsed && (token->type != TOKEN_IDENTIFIER || tryParser ((Parser) parsePeriod, NULL, NULL, token)));

	//parse optional parens block
	tryParser ((Parser) parseParens, initBlockState, freeBlockState, token);
}

static void skipBlocksTillType (tokenType type, tokenInfo *const token)
{
	int nestLevel = 0;
	do {
		readToken (token);

		if (isType (token, TOKEN_OPEN_CURLY))
		{
			nestLevel++;
		}
		else if (isType (token, TOKEN_CLOSE_CURLY))
		{
			nestLevel--;
		}
		else if (nestLevel == 0 && isType (token, type)) return;
		else if (nestLevel < 0) return;
	} while (! isType (token, TOKEN_EOF));
}

static void parseInterfaceBody (vString *const scope, tokenInfo *const token)
{
	do {
		readToken (token);
		if (isType (token, TOKEN_EOF)) return;
	} while (! isType (token, TOKEN_OPEN_CURLY));

	tokenInfo *member = NULL;
	do {
		readToken (token);
		if (isType (token, TOKEN_EOF)) break;
		else if (!member && isType (token, TOKEN_IDENTIFIER))
		{
			member = newToken ();
			copyToken(member, token, false);
			vStringCopy (member->scope, scope);
			member->scopeParentKind = TSTAG_INTERFACE;
		}
		else if (!member && isType (token, TOKEN_PARENS))
		{
			skipBlocksTillType (TOKEN_SEMICOLON, token);
		}
		else if (!member && isType (token, TOKEN_SQUARES))
		{
			skipBlocksTillType (TOKEN_SEMICOLON, token);
		}
		else if (member && isType (token, TOKEN_PARENS))
		{
			emitTag (member, TSTAG_METHOD);
			deleteToken (member);
			member = NULL;
			skipBlocksTillType (TOKEN_SEMICOLON, token);
		}
		else if (member)
		{
			if (!isType (token, TOKEN_TEMPLATE)) {
				emitTag (member, TSTAG_PROPERTY);
				deleteToken (member);
				member = NULL;
				skipBlocksTillType (TOKEN_SEMICOLON, token);
			}
		}
	} while (! isType (token, TOKEN_CLOSE_CURLY));

	if (member) {
		emitTag (member, TSTAG_PROPERTY);
		deleteToken (member);
	}
}

static void parseInterface (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	readToken (token);

	if (token->type != TOKEN_IDENTIFIER) return;

	if (scope)
	{
		vStringCopy (token->scope, scope);
		token->scopeParentKind = scopeParentKind;
	}

	emitTag(token, TSTAG_INTERFACE);

	vString *nscope = vStringNew ();
	if (scope)
	{
		vStringCopy (nscope, scope);
		vStringCatS (nscope, ".");
	}
	vStringCat (nscope, token->string);

	parseInterfaceBody (nscope, token);

	vStringDelete(nscope);
}

static void parseType (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	readToken (token);

	if (token->type != TOKEN_IDENTIFIER) return;

	if (scope)
	{
		vStringCopy(token->scope, scope);
		token->scopeParentKind = scopeParentKind;
	}
	emitTag(token, TSTAG_ALIAS);
	skipBlocksTillType (TOKEN_SEMICOLON, token);
}

static void parseEnumBody (vString *const scope, tokenInfo *const token)
{
	do {
		readToken (token);
		if (isType (token, TOKEN_EOF)) return;
	} while (! isType (token, TOKEN_OPEN_CURLY));

	tokenInfo *member = NULL;
	do {
		readToken (token);
		if (isType (token, TOKEN_EOF)) break;
		else if (!member && isType (token, TOKEN_IDENTIFIER))
		{
			member = newToken ();
			copyToken(member, token, false);
			vStringCopy (member->scope, scope);
			member->scopeParentKind = TSTAG_ENUM;
			emitTag (member, TSTAG_PROPERTY);
		}
		else if (isType (token, TOKEN_COMMA))
		{
			deleteToken (member);
			member = NULL;
		}
	} while (! isType (token, TOKEN_CLOSE_CURLY));

	if (member) {
		emitTag (member, TSTAG_PROPERTY);
		deleteToken (member);
	}
}

static void parseEnum (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	readToken (token);

	if (token->type != TOKEN_IDENTIFIER) return;

	if (scope)
	{
		vStringCopy (token->scope, scope);
		token->scopeParentKind = scopeParentKind;
	}
	emitTag(token, TSTAG_ENUM);

	vString *nscope = vStringNew ();
	if (scope)
	{
		vStringCopy (nscope, scope);
		vStringCatS (nscope, ".");
	}
	vStringCat (nscope, token->string);

	parseEnumBody (nscope, token);

	vStringDelete(nscope);
}

static void parseVariable (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	tokenInfo *member = NULL;
	bool parsed = false;
	bool parsingType = false;
	int nestLevel = 0;

	do {
		clearPoolToken (token);
		parsed = tryParse(token,
			parseTemplate, initBlockState, freeBlockState,
			parseStringSQuote, initParseStringState, freeParseStringState,
			parseStringDQuote, initParseStringState, freeParseStringState,
			parseStringTemplate, initParseStringState, freeParseStringState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseParens, initBlockState, freeBlockState,
			parseNumber, initParseWordState, freeParseWordState,
			parsePipe, NULL, NULL,
			parseAmpersand, NULL, NULL,
			parseEqualSign, NULL, NULL,
			parseQuestionMark, NULL, NULL,
			parseOpenSquare, NULL, NULL,
			parseCloseSquare, NULL, NULL,
			parseOpenCurly, NULL, NULL,
			parseCloseCurly, NULL, NULL,
			parseNewLine, NULL, NULL,
			parseColon, NULL, NULL,
			parseSemicolon, NULL, NULL,
			parseComma, NULL, NULL,
			parsePeriod, NULL, NULL,
			parseArrow, initParseWordState, freeParseWordState,
			parseEnumKeyword, initParseWordState, freeParseWordState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);

		if (parsed) {
			switch (token->type) {
				case TOKEN_OPEN_SQUARE:
				case TOKEN_OPEN_CURLY:
					if (parsingType) nestLevel += 1;
					break;
				case TOKEN_CLOSE_SQUARE:
				case TOKEN_CLOSE_CURLY:
					if (parsingType) nestLevel -= 1;
					break;
				case TOKEN_COMMA:
				case TOKEN_SEMICOLON:
					if (nestLevel <= 0)
					{
						parsingType = false;
						nestLevel = 0;
					}
					break;
				case TOKEN_COLON:
				case TOKEN_EQUAL_SIGN:
					parsingType = true;
					break;
				case TOKEN_IDENTIFIER:
					if (!parsingType) {
						member = newToken ();
						copyToken(member, token, false);
						if (scope)
						{
							vStringCopy (member->scope, scope);
							member->scopeParentKind = scopeParentKind;
						}
						emitTag (member, TSTAG_VARIABLE);
						deleteToken (member);
					}
					break;
				case TOKEN_KEYWORD:
					if (token->keyword == KEYWORD_enum) parseEnum (scope, scopeParentKind, token);
					break;
				default:
					break;
			}
		}
	} while (parsed && ! (token->type == TOKEN_SEMICOLON && !parsingType));

	clearPoolToken (token);
}

static void parseFunctionArgs (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	bool parsed = false;
	bool parsingType = false;
	int nestLevel = 0;
	tokenInfo *member = NULL;

	do {
		clearPoolToken (token);
		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseTemplate, initBlockState, freeBlockState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseOpenParen, NULL, NULL,
			NULL);
	} while (parsed && token->type != TOKEN_OPEN_PAREN);

	if (!parsed) return;

	do {
		clearPoolToken (token);
		parsed = tryParse(token,
			parseTemplate, initBlockState, freeBlockState,
			parseStringSQuote, initParseStringState, freeParseStringState,
			parseStringDQuote, initParseStringState, freeParseStringState,
			parseStringTemplate, initParseStringState, freeParseStringState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseParens, initBlockState, freeBlockState,
			parseNumber, initParseWordState, freeParseWordState,
			parsePipe, NULL, NULL,
			parseAmpersand, NULL, NULL,
			parseEqualSign, NULL, NULL,
			parseQuestionMark, NULL, NULL,
			parseOpenSquare, NULL, NULL,
			parseCloseSquare, NULL, NULL,
			parseOpenCurly, NULL, NULL,
			parseCloseCurly, NULL, NULL,
			parseNewLine, NULL, NULL,
			parseColon, NULL, NULL,
			parseComma, NULL, NULL,
			parsePeriod, NULL, NULL,
			parseAt, NULL, NULL,
			parseArrow, initParseWordState, freeParseWordState,
			parseCloseParen, NULL, NULL,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);

		if (parsed) {
			switch (token->type) {
				case TOKEN_AT:
					ungetcToInputFile ('@');
					parseDecorator (token);
					break;
				case TOKEN_OPEN_SQUARE:
				case TOKEN_OPEN_CURLY:
					if (parsingType) nestLevel += 1;
					break;
				case TOKEN_CLOSE_SQUARE:
				case TOKEN_CLOSE_CURLY:
					if (parsingType) nestLevel -= 1;
					break;
				case TOKEN_COMMA:
					if (nestLevel <= 0)
					{
						parsingType = false;
						nestLevel = 0;
					}
					break;
				case TOKEN_COLON:
				case TOKEN_EQUAL_SIGN:
					parsingType = true;
					break;
				case TOKEN_IDENTIFIER:
					if (!parsingType) {
						member = newToken ();
						copyToken(member, token, false);
						vStringCopy (member->scope, scope);
						member->scopeParentKind = scopeParentKind;
						emitTag (member, TSTAG_VARIABLE);
						deleteToken (member);
					}
					break;
				default:
					break;
			}
		}
	} while (parsed && token->type != TOKEN_CLOSE_PAREN);
}


static void parseFunctionBody (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	bool parsed = false;
	int nestLevel = 1;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseOpenCurly, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			parseChar, NULL, NULL,
			NULL);

	} while (parsed && ! isType (token, TOKEN_OPEN_CURLY));

	if (!parsed) return;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseOpenCurly, NULL, NULL,
			parseCloseCurly, NULL, NULL,
			parseStringSQuote, initParseStringState, freeParseStringState,
			parseStringDQuote, initParseStringState, freeParseStringState,
			parseStringTemplate, initParseStringState, freeParseStringState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseTemplate, initBlockState, freeBlockState,
			parseVarKeyword, initParseWordState, freeParseWordState,
			parseLetKeyword, initParseWordState, freeParseWordState,
			parseConstKeyword, initParseWordState, freeParseWordState,
			parseChar, NULL, NULL,
			NULL);

		if (parsed) {
			switch (token->type) {
				case TOKEN_OPEN_CURLY:
					nestLevel += 1;
					break;
				case TOKEN_CLOSE_CURLY:
					nestLevel -= 1;
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword) {
						case KEYWORD_var:
						case KEYWORD_let:
						case KEYWORD_const:
							parseVariable (scope, scopeParentKind, token);
							break;
					}
					break;
				default:
					break;
			}
		}
	} while (parsed && ! (isType (token, TOKEN_CLOSE_CURLY) && nestLevel <= 0));

	clearPoolToken (token);
}

static void parseFunction (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	bool isGenerator = tryParser ((Parser) parseStar, NULL, NULL, token);

	readToken (token);
	if (token->type != TOKEN_IDENTIFIER) return;

	if (scope)
	{
		vStringCopy (token->scope, scope);
		token->scopeParentKind = scopeParentKind;
	}

	if (isGenerator) emitTag(token, TSTAG_GENERATOR);
	else emitTag(token, TSTAG_FUNCTION);

	vString *nscope = vStringNew ();
	if (scope)
	{
		vStringCopy (nscope, scope);
		vStringCatS (nscope, ".");
	}
	vStringCat (nscope, token->string);

	parseFunctionArgs (nscope, isGenerator ? TSTAG_GENERATOR : TSTAG_FUNCTION, token);
	parseFunctionBody (nscope, isGenerator ? TSTAG_GENERATOR : TSTAG_FUNCTION, token);

	vStringDelete(nscope);
}

static void parsePropertyType (tokenInfo *const token)
{
	bool parsed = tryParser ((Parser) parseColon, NULL, NULL, token);

	if (!parsed) return;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseSemicolon, NULL, NULL,
			parsePipe, NULL, NULL,
			parseAmpersand, NULL, NULL,
			parseEqualSign, NULL, NULL,
			parseComma, NULL, NULL,
			parseCloseParen, NULL, NULL,
			parseTemplate, initBlockState, freeBlockState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseSquares, initBlockState, freeBlockState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);
	} while (parsed && !isType (token, TOKEN_CLOSE_PAREN) && ! isType (token, TOKEN_SEMICOLON) && ! isType (token, TOKEN_COMMA));

	if (!parsed) return;

	if (isType (token, TOKEN_CLOSE_PAREN)) ungetcToInputFile (')');
	clearPoolToken (token);
}

static void parsePropertyList (vString *const scope, tokenInfo *const token)
{
	bool parsed = false;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseOpenParen, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			NULL);
	} while (parsed && !isType (token, TOKEN_OPEN_PAREN));

	if (!parsed) return;

	tokenInfo *member = NULL;
	int visibility = 0;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseCloseParen, NULL, NULL,
			parseComma, NULL, NULL,
			parseColon, NULL, NULL,
			parseAt, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			parsePrivateKeyword, initParseWordState, freeParseWordState,
			parseProtectedKeyword, initParseWordState, freeParseWordState,
			parsePublicKeyword, initParseWordState, freeParseWordState,
			parseReadonlyKeyword, initParseWordState, freeParseWordState,
			parseStaticKeyword, initParseWordState, freeParseWordState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);

		if (parsed)
		{
			switch (token->type) {
				case TOKEN_AT:
					ungetcToInputFile ('@');
					parseDecorator (token);
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword) {
						case KEYWORD_private:
						case KEYWORD_public:
						case KEYWORD_protected:
							visibility = token->keyword;
							break;
					}
					break;
				case TOKEN_COLON:
					ungetcToInputFile(':');
					parsePropertyType (token);
					break;
				case TOKEN_IDENTIFIER:
					member = newToken ();
					copyToken(member, token, false);
					vStringCopy (member->scope, scope);
					if (visibility)
					{
						member->accessKeyword = visibility;
						member->scopeParentKind = TSTAG_CLASS;
						emitTag(member, TSTAG_PROPERTY);
					}
					else
					{
						vStringCatS (member->scope, ".constructor");
						member->scopeParentKind = TSTAG_METHOD;
						emitTag(member, TSTAG_VARIABLE);
					}
					deleteToken (member);
					member = NULL;
					visibility = 0;
					break;
				default:
					break;
			}
		}
	} while (parsed && !isType (token, TOKEN_CLOSE_PAREN));

}

static void parseClassBody (vString *const scope, tokenInfo *const token)
{
	bool parsed = false;

	//parse until {
	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseOpenCurly, NULL, NULL,
			parseTemplate, initBlockState, freeBlockState,
			parseComment, initParseCommentState, freeParseCommentState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);
	} while (parsed && token->type != TOKEN_OPEN_CURLY);

	if (!parsed) return;

	tokenInfo *member = NULL;
	bool isGenerator = false;
	int visibility = 0;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseCloseCurly, NULL, NULL,
			parseStar, NULL, NULL,
			parseAt, NULL, NULL,
			parseOpenParen, NULL, NULL,
			parseColon, NULL, NULL,
			parseSemicolon, NULL, NULL,
			parseEqualSign, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			parseAsyncKeyword, initParseWordState, freeParseWordState,
			parseConstructorKeyword, initParseWordState, freeParseWordState,
			parsePrivateKeyword, initParseWordState, freeParseWordState,
			parseProtectedKeyword, initParseWordState, freeParseWordState,
			parsePublicKeyword, initParseWordState, freeParseWordState,
			parseReadonlyKeyword, initParseWordState, freeParseWordState,
			parseStaticKeyword, initParseWordState, freeParseWordState,
			parseNumber, initParseWordState, freeParseWordState,
			parseTemplate, initBlockState, freeBlockState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);

		if (parsed)
		{
			switch (token->type) {
				case TOKEN_AT:
					ungetcToInputFile ('@');
					parseDecorator (token);
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword) {
						case KEYWORD_constructor:
							if (member) deleteToken (member);
							member = newToken ();
							copyToken(member, token, false);
							vStringCopy (member->scope, scope);
							member->scopeParentKind = TSTAG_CLASS;

							if (visibility) member->accessKeyword = visibility;
							else member->accessKeyword = KEYWORD_public;

							emitTag(member, TSTAG_METHOD);
							deleteToken (member);
							member = NULL;
							visibility = 0;

							parsePropertyList (scope, token);

							vString *methodScope = vStringNew ();
							vStringCopy (methodScope, scope);
							vStringCatS (methodScope, ".constructor");
							parseFunctionBody (methodScope, TSTAG_METHOD, token);
							vStringDelete(methodScope);
							break;
						case KEYWORD_private:
						case KEYWORD_public:
						case KEYWORD_protected:
							visibility = token->keyword;
							break;
					}
					isGenerator = false;
					break;
				case TOKEN_EQUAL_SIGN:
					skipBlocksTillType (TOKEN_SEMICOLON, token);
					ungetcToInputFile(';');
					break;
				case TOKEN_COLON:
					ungetcToInputFile(':');
					parsePropertyType (token);
				case TOKEN_SEMICOLON:
					if (member) {
						emitTag(member, TSTAG_PROPERTY);
						deleteToken (member);
						member = NULL;
						isGenerator = false;
						visibility = 0;
					}
					break;
				case TOKEN_STAR:
					isGenerator = true;
					break;
				case TOKEN_OPEN_PAREN:
					ungetcToInputFile('(');

					if (isGenerator) emitTag(member, TSTAG_GENERATOR);
					else emitTag(member, TSTAG_METHOD);

					vString *methodScope = vStringNew ();
					vStringCopy (methodScope, scope);
					vStringCatS (methodScope, ".");
					vStringCat (methodScope, member->string);

					deleteToken (member);
					member = NULL;

					parseFunctionArgs (methodScope, TSTAG_METHOD, token);
					parseFunctionBody (methodScope, TSTAG_METHOD, token);

					vStringDelete(methodScope);
					isGenerator = false;
					visibility = 0;
					break;
				case TOKEN_IDENTIFIER:
					if (member) deleteToken (member);
					member = newToken ();
					copyToken(member, token, false);
					vStringCopy (member->scope, scope);
					member->scopeParentKind = TSTAG_CLASS;
					if (visibility) member->accessKeyword = visibility;
					else member->accessKeyword = KEYWORD_public;
					break;
				default:
					isGenerator = false;
					visibility = 0;
					break;
			}
		}
	} while (parsed && token->type != TOKEN_CLOSE_CURLY);


	if (parsed && member)
	{
		emitTag(member, TSTAG_PROPERTY);
		deleteToken (member);
	}
	else if (member) deleteToken (member);
}

static void parseClass (vString *const scope, tsKind scopeParentKind, tokenInfo *const token)
{
	bool parsed = false;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (!parsed) return;

	if (scope)
	{
		vStringCopy (token->scope, scope);
		token->scopeParentKind = scopeParentKind;
	}
	emitTag(token, TSTAG_CLASS);

	vString *nscope = vStringNew ();
	if (scope)
	{
		vStringCopy (nscope, scope);
		vStringCatS (nscope, ".");
	}
	vStringCat (nscope, token->string);

	parseClassBody (nscope, token);

	vStringDelete(nscope);
}

static void parseNamespaceBody (vString *const scope, tokenInfo *const token)
{
	bool parsed = false;

	//parse until {
	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseOpenCurly, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			NULL);
	} while (parsed && token->type != TOKEN_OPEN_CURLY);

	if (!parsed) return;

	do {
		readToken (token);

		switch (token->type) {
			case TOKEN_KEYWORD:
				switch (token->keyword) {
					case KEYWORD_interface:
						parseInterface (scope, TSTAG_NAMESPACE, token);
						break;
					case KEYWORD_type:
						parseType (scope, TSTAG_NAMESPACE, token);
						break;
					case KEYWORD_enum:
						parseEnum (scope, TSTAG_NAMESPACE, token);
						break;
					case KEYWORD_function:
						parseFunction (scope, TSTAG_NAMESPACE, token);
						break;
					case KEYWORD_class:
						parseClass (scope, TSTAG_NAMESPACE, token);
						break;
					case KEYWORD_var:
					case KEYWORD_let:
					case KEYWORD_const:
						parseVariable (scope, TSTAG_NAMESPACE, token);
						break;
				}
				break;
			case TOKEN_AT:
				ungetcToInputFile ('@');
				parseDecorator (token);
				break;
			default:
				break;
		}
	} while (!isType (token, TOKEN_CLOSE_CURLY) && !isType (token, TOKEN_EOF));
}

static void parseNamespace (tokenInfo *const token)
{
	bool parsed = false;

	do {
		clearPoolToken (token);

		parsed = tryParse(token,
			parseNewLine, NULL, NULL,
			parseComment, initParseCommentState, freeParseCommentState,
			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (!parsed) return;

	emitTag(token, TSTAG_NAMESPACE);

	vString *scope = vStringNew ();
	vStringCopy (scope, token->string);

	parseNamespaceBody (scope, token);

	vStringDelete(scope);
}

static void parseTsFile (tokenInfo *const token)
{
	do {
		readToken (token);

		switch (token->type) {
			case TOKEN_KEYWORD:
				switch (token->keyword) {
					case KEYWORD_interface:
						parseInterface (NULL, TSTAG_CLASS, token);
						break;
					case KEYWORD_type:
						parseType (NULL, TSTAG_CLASS, token);
						break;
					case KEYWORD_enum:
						parseEnum (NULL, TSTAG_CLASS, token);
						break;
					case KEYWORD_function:
						parseFunction (NULL, TSTAG_CLASS, token);
						break;
					case KEYWORD_class:
						parseClass (NULL, TSTAG_CLASS, token);
						break;
					case KEYWORD_namespace:
						parseNamespace (token);
						break;
					case KEYWORD_var:
					case KEYWORD_let:
					case KEYWORD_const:
						parseVariable (NULL, TSTAG_CLASS, token);
						break;
				}
				break;
			case TOKEN_AT:
				ungetcToInputFile ('@');
				parseDecorator (token);
				break;
			default:
				break;
		}
	} while (! isType (token, TOKEN_EOF));
}

static void findTsTags (void)
{
	tokenInfo *const token = newToken ();

	NextToken = NULL;
	LastTokenType = TOKEN_UNDEFINED;

	parseTsFile (token);

	deleteToken (token);
}


static void initialize (const langType language)
{
	Lang_ts = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

/* Create parser definition structure */
extern parserDefinition* TypeScriptParser (void)
{
	static const char *const extensions [] = { "ts", NULL };
	parserDefinition *const def = parserNew ("TypeScript");
	def->extensions = extensions;
	def->kindTable	= TsKinds;
	def->kindCount	= ARRAY_SIZE (TsKinds);
	def->parser		= findTsTags;
	def->initialize = initialize;
	def->finalize   = finalize;
	def->keywordTable = TsKeywordTable;
	def->keywordCount = ARRAY_SIZE (TsKeywordTable);

	return def;
}
