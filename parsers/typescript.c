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

/*
 *	DATA DEFINITIONS
 */
static langType Lang_ts;
static objPool *TokenPool = NULL;

/*	Used to specify type of keyword.
*/
enum eKeywordId {
	KEYWORD_break,
	KEYWORD_case,
	KEYWORD_catch,
	KEYWORD_class,
	KEYWORD_const,
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
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_TEMPLATE_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_EQUAL_SIGN,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_REGEXP,
	TOKEN_POSTFIX_OPERATOR,
	TOKEN_STAR,
	TOKEN_BINARY_OPERATOR
} tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
} tokenInfo;

static tokenType LastTokenType;
static tokenInfo *NextToken;

static const keywordTable TsKeywordTable [] = {
	/* keyword		keyword ID */
	{ "break"      , KEYWORD_break      },
	{ "case"       , KEYWORD_case       },
	{ "catch"      , KEYWORD_catch      },
	{ "class"      , KEYWORD_class      },
	{ "const"      , KEYWORD_const      },
	{ "continue"   , KEYWORD_continue   },
	{ "debugger"   , KEYWORD_debugger   },
	{ "default"    , KEYWORD_default    },
	{ "delete"     , KEYWORD_delete     },
	{ "do"         , KEYWORD_do         },
	{ "else"       , KEYWORD_else       },
	{ "enum"       , KEYWORD_enum       },
	{ "export"     , KEYWORD_export     },
	{ "extends"    , KEYWORD_extends    },
	{ "false"      , KEYWORD_false      },
	{ "finally"    , KEYWORD_finally    },
	{ "for"        , KEYWORD_for        },
	{ "function"   , KEYWORD_function   },
	{ "if"         , KEYWORD_if         },
	{ "implements" , KEYWORD_implements },
	{ "import"     , KEYWORD_import     },
	{ "in"         , KEYWORD_in         },
	{ "instanceof" , KEYWORD_instanceof },
	{ "interface"  , KEYWORD_interface  },
	{ "let"        , KEYWORD_let        },
	{ "module"     , KEYWORD_module     },
	{ "namespace"  , KEYWORD_namespace  },
	{ "new"        , KEYWORD_new        },
	{ "null"       , KEYWORD_null       },
	{ "package"    , KEYWORD_package    },
	{ "private"    , KEYWORD_private    },
	{ "protected"  , KEYWORD_protected  },
	{ "public"     , KEYWORD_public     },
	{ "return"     , KEYWORD_return     },
	{ "static"     , KEYWORD_static     },
	{ "super"      , KEYWORD_super      },
	{ "switch"     , KEYWORD_switch     },
	{ "this"       , KEYWORD_this       },
	{ "throw"      , KEYWORD_throw      },
	{ "true"       , KEYWORD_true       },
	{ "try"        , KEYWORD_try        },
	{ "type"       , KEYWORD_type       },
	{ "typeof"     , KEYWORD_typeof     },
	{ "var"        , KEYWORD_var        },
	{ "void"       , KEYWORD_void       },
	{ "while"      , KEYWORD_while      },
	{ "with"       , KEYWORD_with       },
	{ "yield"      , KEYWORD_yield      }
};

typedef enum {
	TSTAG_FUNCTION,
	TSTAG_CLASS,
	TSTAG_INTERFACE,
	TSTAG_ENUM,
	TSTAG_METHOD,
	TSTAG_PROPERTY,
	TSTAG_CONSTANT,
	TSTAG_VARIABLE,
	TSTAG_GENERATOR,
	TSTAG_ALIAS
} tsKind;

static kindDefinition TsKinds [] = {
	{ true,  'f', "function",	  "functions"		   },
	{ true,  'c', "class",		  "classes"			   },
	{ true,  'i', "interface",	  "interfaces"		   },
	{ true,  'e', "enum",		  "enums"			   },
	{ true,  'm', "method",		  "methods"			   },
	{ true,  'p', "property",	  "properties"		   },
	{ true,  'C', "constant",	  "constants"		   },
	{ true,  'v', "variable",	  "global variables"   },
	{ true,  'g', "generator",	  "generators"		   },
	{ true,	 'a', "alias",		  "aliases",		   }
};

static void emitTag(const tokenInfo *const token, const tsKind kind)
{
	if (!TsKinds [kind].enabled)
		return;

	const char *name = vStringValue (token->string);
	tagEntryInfo e;

	initTagEntry (&e, name, kind);
	e.lineNumber   = token->lineNumber;
	e.filePosition = token->filePosition;
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

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	vStringDelete (token->string);
	eFree (token);
}

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

inline static bool whiteChar(const char c)
{
	return c == ' ' || c == '\n' || c == '\r' || c == '\t';
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
	if (*parsed == 0 && whiteChar(c)) {
		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	if (c == word[*parsed]) {
		*parsed = *parsed + 1;

		if (word[*parsed] == '\0') {
			vStringCatS (token->string, word);
			token->type = TOKEN_KEYWORD;
			token->keyword = lookupKeyword (vStringValue (token->string), Lang_ts);
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
		result->status = PARSER_FINISHED;
		result->unusedChars = 1;
		return;
	}

	result->status = PARSER_FAILED;
}

inline static void parseBreakKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "break", (int*) state, result);
}

inline static void parseCaseKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "case", (int*) state, result);
}

inline static void parseCatchKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "catch", (int*) state, result);
}

inline static void parseClassKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "class", (int*) state, result);
}

inline static void parseConstKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "const", (int*) state, result);
}

inline static void parseContinueKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "continue", (int*) state, result);
}

inline static void parseDebuggerKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "debugger", (int*) state, result);
}

inline static void parseDefaultKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "default", (int*) state, result);
}

inline static void parseDeleteKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "delete", (int*) state, result);
}

inline static void parseDoKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "do", (int*) state, result);
}

inline static void parseElseKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "else", (int*) state, result);
}

inline static void parseEnumKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "enum", (int*) state, result);
}

inline static void parseExportKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "export", (int*) state, result);
}

inline static void parseExtendsKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "extends", (int*) state, result);
}

inline static void parseFalseKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "false", (int*) state, result);
}

inline static void parseFinallyKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "finally", (int*) state, result);
}

inline static void parseForKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "for", (int*) state, result);
}

inline static void parseFunctionKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "function", (int*) state, result);
}

inline static void parseIfKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "if", (int*) state, result);
}

inline static void parseImplementsKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "implements", (int*) state, result);
}

inline static void parseImportKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "import", (int*) state, result);
}

inline static void parseInKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "in", (int*) state, result);
}

inline static void parseInstanceofKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "instanceof", (int*) state, result);
}

inline static void parseInterfaceKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "interface", (int*) state, result);
}

inline static void parseLetKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "let", (int*) state, result);
}

inline static void parseModuleKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "module", (int*) state, result);
}

inline static void parseNamespaceKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "namespace", (int*) state, result);
}

inline static void parseNewKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "new", (int*) state, result);
}

inline static void parseNullKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "null", (int*) state, result);
}

inline static void parsePackageKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "package", (int*) state, result);
}

inline static void parsePrivateKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "private", (int*) state, result);
}

inline static void parseProtectedKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "protected", (int*) state, result);
}

inline static void parsePublicKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "public", (int*) state, result);
}

inline static void parseReturnKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "return", (int*) state, result);
}

inline static void parseStaticKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "static", (int*) state, result);
}

inline static void parseSuperKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "super", (int*) state, result);
}

inline static void parseSwitchKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "switch", (int*) state, result);
}

inline static void parseThisKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "this", (int*) state, result);
}

inline static void parseThrowKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "throw", (int*) state, result);
}

inline static void parseTrueKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "true", (int*) state, result);
}

inline static void parseTryKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "try", (int*) state, result);
}

inline static void parseTypeKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "type", (int*) state, result);
}

inline static void parseTypeofKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "typeof", (int*) state, result);
}

inline static void parseVarKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "var", (int*) state, result);
}

inline static void parseVoidKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "void", (int*) state, result);
}

inline static void parseWhileKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "while", (int*) state, result);
}

inline static void parseWithKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "with", (int*) state, result);
}

inline static void parseYieldKeyword(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseWord(c, token, "yield", (int*) state, result);
}

inline static void parseCloseParen(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, ')', TOKEN_CLOSE_PAREN, result);
}

inline static void parseSemicolon(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, ';', TOKEN_SEMICOLON, result);
}

inline static void parseComma(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, ',', TOKEN_COMMA, result);
}

inline static void parseColon(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, ':', TOKEN_COLON, result);
}

inline static void parseOpenParen(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '(', TOKEN_OPEN_PAREN, result);
}

inline static void parsePeriod(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '.', TOKEN_PERIOD, result);
}

inline static void parseOpenCurly(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '{', TOKEN_OPEN_CURLY, result);
}

inline static void parseCloseCurly(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '}', TOKEN_CLOSE_CURLY, result);
}

inline static void parseEqualSign(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '=', TOKEN_EQUAL_SIGN, result);
}

inline static void parseOpenSquare(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '[', TOKEN_OPEN_SQUARE, result);
}

inline static void parseCloseSquare(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, ']', TOKEN_CLOSE_SQUARE, result);
}

inline static void parseStar(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, '*', TOKEN_STAR, result);
}

inline static void parseEOF(const char c, tokenInfo *const token, void *state, parserResult *const result)
{
	parseOneChar(c, token, EOF, TOKEN_EOF, result);
}

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
			parseCloseParen, NULL, NULL,
			parseSemicolon, NULL, NULL,
			parseComma, NULL, NULL,
			parseColon, NULL, NULL,
			parseOpenParen, NULL, NULL,
			parsePeriod, NULL, NULL,
			parseOpenCurly, NULL, NULL,
			parseCloseCurly, NULL, NULL,
			parseEqualSign, NULL, NULL,
			parseOpenSquare, NULL, NULL,
			parseCloseSquare, NULL, NULL,
			parseStar, NULL, NULL,
			parseEOF, NULL, NULL,

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
			parseReturnKeyword, initParseWordState, freeParseWordState,
			parseStaticKeyword, initParseWordState, freeParseWordState,
			parseSuperKeyword, initParseWordState, freeParseWordState,
			parseSwitchKeyword, initParseWordState, freeParseWordState,
			parseThisKeyword, initParseWordState, freeParseWordState,
			parseThrowKeyword, initParseWordState, freeParseWordState,
			parseTrueKeyword, initParseWordState, freeParseWordState,
			parseTryKeyword, initParseWordState, freeParseWordState,
			parseTypeKeyword, initParseWordState, freeParseWordState,
			parseTypeofKeyword, initParseWordState, freeParseWordState,
			parseVarKeyword, initParseWordState, freeParseWordState,
			parseVoidKeyword, initParseWordState, freeParseWordState,
			parseWhileKeyword, initParseWordState, freeParseWordState,
			parseWithKeyword, initParseWordState, freeParseWordState,
			parseYieldKeyword, initParseWordState, freeParseWordState,

			parseIdentifier, initParseWordState, freeParseWordState,
			NULL);

	if (parsed)
		LastTokenType = token->type;
}

static void parseTsFile (tokenInfo *const token)
{
	do {
		readToken (token);

		switch (token->keyword) {
			case KEYWORD_interface:
				emitTag(token, TSTAG_INTERFACE);
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
