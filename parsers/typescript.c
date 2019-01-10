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

#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))

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
} parserResult;

typedef parserResult (*Parser)(const char c, tokenInfo *const, void *state);
typedef void* (*ParserStateInit)();
typedef void (*ParserStateFree)(void *);

inline static bool whiteChar(const char c)
{
	return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

inline static parserResult parseOneChar(const char c, tokenInfo *const token, const char expected, const tokenType type)
{
	if (whiteChar(c))
		return PARSER_NEEDS_MORE_INPUT;

	if (c != expected)
		return PARSER_FAILED;

	token->type = type;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	return PARSER_FINISHED;
}

inline static void* initParseWordState()
{
	return xMalloc (1, int);
}

inline static void freeParseWordState(void *state)
{
	eFree ((int *) state);
}

inline static parserResult parseWord(const char c, tokenInfo *const token, const char* word, int* parsed)
{
	if (whiteChar(c))
		return PARSER_NEEDS_MORE_INPUT;

	if (c == word[*parsed]) {
		*parsed = *parsed + 1;

		if (word[*parsed] == '\0') {
			vStringCatS (token->string, word);
			token->type = TOKEN_KEYWORD;
			token->keyword = lookupKeyword (vStringValue (token->string), Lang_ts);
			token->lineNumber   = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();

			return PARSER_FINISHED;
		}

		return PARSER_NEEDS_MORE_INPUT;
	}

	return PARSER_FAILED;
}

inline static parserResult parseBreakKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "break", (int*) state);
}

inline static parserResult parseCaseKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "case", (int*) state);
}

inline static parserResult parseCatchKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "catch", (int*) state);
}

inline static parserResult parseClassKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "class", (int*) state);
}

inline static parserResult parseConstKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "const", (int*) state);
}

inline static parserResult parseContinueKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "continue", (int*) state);
}

inline static parserResult parseDebuggerKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "debugger", (int*) state);
}

inline static parserResult parseDefaultKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "default", (int*) state);
}

inline static parserResult parseDeleteKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "delete", (int*) state);
}

inline static parserResult parseDoKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "do", (int*) state);
}

inline static parserResult parseElseKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "else", (int*) state);
}

inline static parserResult parseEnumKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "enum", (int*) state);
}

inline static parserResult parseExportKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "export", (int*) state);
}

inline static parserResult parseExtendsKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "extends", (int*) state);
}

inline static parserResult parseFalseKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "false", (int*) state);
}

inline static parserResult parseFinallyKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "finally", (int*) state);
}

inline static parserResult parseForKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "for", (int*) state);
}

inline static parserResult parseFunctionKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "function", (int*) state);
}

inline static parserResult parseIfKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "if", (int*) state);
}

inline static parserResult parseImplementsKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "implements", (int*) state);
}

inline static parserResult parseImportKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "import", (int*) state);
}

inline static parserResult parseInKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "in", (int*) state);
}

inline static parserResult parseInstanceofKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "instanceof", (int*) state);
}

inline static parserResult parseInterfaceKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "interface", (int*) state);
}

inline static parserResult parseLetKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "let", (int*) state);
}

inline static parserResult parseModuleKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "module", (int*) state);
}

inline static parserResult parseNamespaceKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "namespace", (int*) state);
}

inline static parserResult parseNewKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "new", (int*) state);
}

inline static parserResult parseNullKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "null", (int*) state);
}

inline static parserResult parsePackageKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "package", (int*) state);
}

inline static parserResult parsePrivateKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "private", (int*) state);
}

inline static parserResult parseProtectedKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "protected", (int*) state);
}

inline static parserResult parsePublicKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "public", (int*) state);
}

inline static parserResult parseReturnKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "return", (int*) state);
}

inline static parserResult parseStaticKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "static", (int*) state);
}

inline static parserResult parseSuperKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "super", (int*) state);
}

inline static parserResult parseSwitchKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "switch", (int*) state);
}

inline static parserResult parseThisKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "this", (int*) state);
}

inline static parserResult parseThrowKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "throw", (int*) state);
}

inline static parserResult parseTrueKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "true", (int*) state);
}

inline static parserResult parseTryKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "try", (int*) state);
}

inline static parserResult parseTypeKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "type", (int*) state);
}

inline static parserResult parseTypeofKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "typeof", (int*) state);
}

inline static parserResult parseVarKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "var", (int*) state);
}

inline static parserResult parseVoidKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "void", (int*) state);
}

inline static parserResult parseWhileKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "while", (int*) state);
}

inline static parserResult parseWithKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "with", (int*) state);
}

inline static parserResult parseYieldKeyword(const char c, tokenInfo *const token, void *state)
{
	return parseWord(c, token, "yield", (int*) state);
}

inline static parserResult parseCloseParen(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, ')', TOKEN_CLOSE_PAREN);
}

inline static parserResult parseSemicolon(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, ';', TOKEN_SEMICOLON);
}

inline static parserResult parseComma(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, ',', TOKEN_COMMA);
}

inline static parserResult parseColon(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, ':', TOKEN_COLON);
}

inline static parserResult parseOpenParen(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '(', TOKEN_OPEN_PAREN);
}

inline static parserResult parsePeriod(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '.', TOKEN_PERIOD);
}

inline static parserResult parseOpenCurly(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '{', TOKEN_OPEN_CURLY);
}

inline static parserResult parseCloseCurly(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '}', TOKEN_CLOSE_CURLY);
}

inline static parserResult parseEqualSign(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '=', TOKEN_EQUAL_SIGN);
}

inline static parserResult parseOpenSquare(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '[', TOKEN_OPEN_SQUARE);
}

inline static parserResult parseCloseSquare(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, ']', TOKEN_CLOSE_SQUARE);
}

inline static parserResult parseStar(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, '*', TOKEN_STAR);
}

inline static parserResult parseEOF(const char c, tokenInfo *const token, void *state)
{
	return parseOneChar(c, token, EOF, TOKEN_EOF);
}

static bool tryParser(Parser parser, ParserStateInit stInit, ParserStateFree stFree, tokenInfo *const token)
{
	void *currentState = NULL;
	parserResult result = PARSER_NEEDS_MORE_INPUT;
	charArray *usedC = charArrayNew();
	char c;

	if (stInit)
		currentState = stInit();

	while (result == PARSER_NEEDS_MORE_INPUT) {
		c = getcFromInputFile();
		result = parser(c, token, currentState);
		charArrayAdd(usedC, c);
	}

	if (stFree)
		stFree(currentState);

	if (result == PARSER_FAILED) {
		while (charArrayCount(usedC) > 0) {
			ungetcToInputFile(charArrayLast(usedC));
			charArrayRemoveLast(usedC);
		}
	}

	charArrayDelete(usedC);

	return result == PARSER_FINISHED;
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
			parseInKeyword, initParseWordState, freeParseWordState,
			parseInstanceofKeyword, initParseWordState, freeParseWordState,
			parseInterfaceKeyword, initParseWordState, freeParseWordState,
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
			NULL);

	if (parsed)
		LastTokenType = token->type;
}

static void parseTsFile (tokenInfo *const token)
{
	do {
		readToken (token);
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
