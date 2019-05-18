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
#include <string.h>

#include "general.h"    /* must always come first */
#include "parse.h"
#include "objpool.h"
#include "keyword.h"
#include "read.h"
#include "numarray.h"
#include "routines.h"
#include "entry.h"
#include "inline.h"
#include "unwindi.h"

#define isType(token,t)     (bool) ((token)->type == (t))
#define isKeyword(token,k)  (bool) ((token)->keyword == (k))
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '$' || \
	 (c) == '@' || (c) == '_' || (c) == '#' || \
	 (c) >= 0x80)

#define PARSER_DEF(fname, pfun, word, stateField) \
	CTAGS_INLINE void parse ## fname (const int c, tokenInfo * const token, parserState *state, parserResult * const result) \
	{ \
		pfun (c, token, word, &state->stateField, result); \
	}

#define SINGLE_CHAR_PARSER_DEF(fname, ch, ttype) \
	CTAGS_INLINE void parse ## fname (const int c, tokenInfo * const token, parserState *state, parserResult * const result) \
	{ \
		parseOneChar (c, token, ch, ttype, result); \
	}

#define MULTI_CHAR_PARSER_DEF(fname, chs, ...) \
	CTAGS_INLINE void parse ## fname (const int c, tokenInfo * const token, parserState *state, parserResult * const result) \
	{ \
		tokenType types[] = { __VA_ARGS__ }; \
		parseChar (c, token, state, result, chs, types); \
	}

#define WORD_TOKEN_PARSER_DEF(fname, w, ttype) \
	CTAGS_INLINE void parse ## fname (const int c, tokenInfo * const token, parserState *state, parserResult * const result) \
	{ \
		parseWordToken (c, token, w, ttype, &state->num, result); \
	}

#define BLOCK_PARSER_DEF(fname, start, end, ttype) \
	CTAGS_INLINE void parse ## fname (const int c, tokenInfo * const token, parserState *state, parserResult * const result) \
	{ \
		parseBlock (c, token, ttype, start, end, &state->block, result); \
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
	KEYWORD_class,
	KEYWORD_constructor,
	KEYWORD_const,
	KEYWORD_enum,
	KEYWORD_for,
	KEYWORD_function,
	KEYWORD_instanceof,
	KEYWORD_in,
	KEYWORD_interface,
	KEYWORD_let,
	KEYWORD_namespace,
	KEYWORD_of,
	KEYWORD_private,
	KEYWORD_protected,
	KEYWORD_public,
	KEYWORD_return,
	KEYWORD_readonly,
	KEYWORD_static,
	KEYWORD_this,
	KEYWORD_type,
	KEYWORD_var,
	KEYWORD_while
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
	TOKEN_STAR,
	TOKEN_NL,
	TOKEN_COMMENT_BLOCK,
	TOKEN_PARENS,
	TOKEN_SQUARES,
	TOKEN_CURLIES,
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
	TSTAG_ENUMERATOR,
	TSTAG_METHOD,
	TSTAG_NAMESPACE,
	TSTAG_PARAMETER,
	TSTAG_PROPERTY,
	TSTAG_VARIABLE,
	TSTAG_LOCAL,
	TSTAG_CONSTANT,
	TSTAG_GENERATOR,
	TSTAG_ALIAS
} tsKind;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	vString *string;
	int scope;
	unsigned long lineNumber;
	MIOPos filePosition;
	keywordId accessKeyword;
} tokenInfo;

typedef struct sCommentState {
	int parsed;
	int blockParsed;
	bool isBlock;
} commentState;

typedef struct sBlockState {
	int parsed;
	int nestLevel;
	int curlyLevel;
} blockState;

static const keywordTable TsKeywordTable [] = {
	/* keyword		  keyword ID */
	{ "async"       , KEYWORD_async       },
	{ "class"       , KEYWORD_class       },
	{ "const"       , KEYWORD_const       },
	{ "constructor" , KEYWORD_constructor },
	{ "enum"        , KEYWORD_enum        },
	{ "for"         , KEYWORD_for         },
	{ "function"    , KEYWORD_function    },
	{ "in"          , KEYWORD_in          },
	{ "interface"   , KEYWORD_interface   },
	{ "let"         , KEYWORD_let         },
	{ "namespace"   , KEYWORD_namespace   },
	{ "of"          , KEYWORD_of          },
	{ "private"     , KEYWORD_private     },
	{ "protected"   , KEYWORD_protected   },
	{ "public"      , KEYWORD_public      },
	{ "static"      , KEYWORD_static      },
	{ "this"        , KEYWORD_this        },
	{ "type"        , KEYWORD_type        },
	{ "var"         , KEYWORD_var         },
	{ "while"       , KEYWORD_while       }
};

static kindDefinition TsKinds [] = {
	{ true,  'f', "function",     "functions"                                       },
	{ true,  'c', "class",        "classes"                                         },
	{ true,  'i', "interface",    "interfaces"                                      },
	{ true,  'g', "enum",         "enums"                                           },
	{ true,  'e', "enumerator",   "enumerators (values inside an enumeration)"      },
	{ true,  'm', "method",       "methods"                                         },
	{ true,  'n', "namespace",    "namespaces"                                      },
	{ false, 'z', "parameter",    "function parameters inside function definitions" },
	{ true,  'p', "property",     "properties"                                      },
	{ true,  'v', "variable",     "variables"                                       },
	{ false, 'l', "local",        "local variables"                                 },
	{ true,  'C', "constant",     "constants"                                       },
	{ true,  'G', "generator",    "generators"                                      },
	{ true,  'a', "alias",        "aliases",                                        }
};

typedef enum eParserResult {
	PARSER_FINISHED,
	PARSER_NEEDS_MORE_INPUT,
	PARSER_FAILED
} parserResultStatus;

typedef struct sParserResult {
	parserResultStatus status;
	unsigned int unusedChars;
} parserResult;

typedef union uParserState {
	int num;
	blockState block;
	commentState comment;
	char ch;
} parserState;

static struct sUwiStats tsUwiStats;

typedef void (*Parser)(const int c, tokenInfo *const, parserState *state, parserResult *const);

static bool tryParser(Parser parser, tokenInfo *const token, bool skipWhite);

static int emitTag(const tokenInfo *const token, const tsKind kind)
{
	if (! TsKinds [kind].enabled)
		return CORK_NIL;

	static const char *const access [3] = {
		"private",
		"protected",
		"public"
	};

	const char *name = vStringValue (token->string);
	tagEntryInfo e;

	initTagEntry (&e, name, kind);
	e.lineNumber   = token->lineNumber;
	e.filePosition = token->filePosition;
	e.extensionFields.scopeIndex = token->scope;

	switch (token->accessKeyword)
	{
		case KEYWORD_public:
			e.extensionFields.access = access [2];
			break;
		case KEYWORD_protected:
			e.extensionFields.access = access [1];
			break;
		case KEYWORD_private:
			e.extensionFields.access = access [0];
			break;
	}

	return makeTagEntry (&e);
}

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->scope = CORK_NIL;
	token->string = NULL;

	return token;
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type            = TOKEN_UNDEFINED;
	token->keyword         = KEYWORD_NONE;
	token->lineNumber      = uwiGetLineNumber ();
	token->filePosition    = uwiGetFilePosition ();

	token->scope = CORK_NIL;

	token->accessKeyword   = KEYWORD_NONE;

	token->string = vStringNewOrClear (token->string);
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	if (token->string)
		vStringDelete (token->string);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src,
					   bool scope)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy (dest->string, src->string);
	if (scope) dest->scope = src->scope;
}

static void initToken (tokenInfo *const token, tokenType type)
{
	token->type = type;
	token->keyword = KEYWORD_NONE;
	token->lineNumber   = uwiGetLineNumber ();
	token->filePosition = uwiGetFilePosition ();
}

CTAGS_INLINE bool whiteChar(const int c)
{
	return c == ' ' || c == '\r' || c == '\t';
}

CTAGS_INLINE void parseWhiteChars(const int c, tokenInfo *const token, parserState *state, parserResult * const result)
{
	if (whiteChar (c))
	{
		result->status = PARSER_NEEDS_MORE_INPUT;
		state->num += 1;
		return;
	}

	if (state->num == 0)
	{
		result->status = PARSER_FAILED;
		return;
	}

	result->status = PARSER_FINISHED;
	result->unusedChars = 1;
}

CTAGS_INLINE void parseOneChar(const int c, tokenInfo *const token, const char expected, const tokenType type, parserResult *const result)
{
	if (c != expected)
	{
		result->status = PARSER_FAILED;
		return;
	}

	initToken (token, type);
	result->status = PARSER_FINISHED;
}

CTAGS_INLINE void parseChar(const int c, tokenInfo *const token, void *state, parserResult *const result, const char *chars, const tokenType *types)
{
	const char *pos = strchr (chars, c);

	if (pos)
	{
		result->status = PARSER_FINISHED;
		initToken (token, types[pos - chars]);
		return;
	}

	result->status = PARSER_FAILED;
}

CTAGS_INLINE void parseWord(const int c, tokenInfo *const token, const char *word, int *parsed, parserResult *const result)
{
	if (word [*parsed] == '\0')
	{
		if (isIdentChar (c))
		{
			result->status = PARSER_FAILED;
			return;
		}

		vStringCatS (token->string, word);
		initToken (token, TOKEN_KEYWORD);
		token->keyword = lookupKeyword (vStringValue (token->string), Lang_ts);

		result->unusedChars = 1;
		result->status = PARSER_FINISHED;
		return;
	}

	if (c == word [*parsed])
	{
		*parsed += 1;

		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	result->status = PARSER_FAILED;
}

CTAGS_INLINE void parseNumber(const int c, tokenInfo *const token, int *parsed, parserResult *const result)
{
	if (*parsed == 0)
	{
		result->status = PARSER_NEEDS_MORE_INPUT;

		if (c == '-')
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

	initToken (token, TOKEN_NUMBER);

	result->unusedChars = 1;
	result->status = PARSER_FINISHED;
}

CTAGS_INLINE void parseWordToken(const int c, tokenInfo *const token, const char *word, const tokenType type, int *parsed, parserResult *const result)
{
	if (c == word [*parsed])
	{
		*parsed += 1;

		if (word [*parsed] == '\0')
		{
			initToken (token, type);
			result->status = PARSER_FINISHED;
			return;
		}

		result->status = PARSER_NEEDS_MORE_INPUT;
		return;
	}

	result->status = PARSER_FAILED;
}

CTAGS_INLINE void parseComment(const int c, tokenInfo *const token, commentState *state, parserResult *const result)
{
	if (state->parsed < 2)
	{
		parseWordToken (c, token, "//", TOKEN_COMMENT_BLOCK, &state->parsed, result);

		if (result->status == PARSER_FAILED)
		{
			parseWordToken (c, token, "/*", TOKEN_COMMENT_BLOCK, &state->parsed, result);
			if (result->status == PARSER_FINISHED)
			{
				result->status = PARSER_NEEDS_MORE_INPUT;
				state->isBlock = true;
			}
		}
		else if (result->status == PARSER_FINISHED)
		{
			result->status = PARSER_NEEDS_MORE_INPUT;
			state->isBlock = false;
		}

		return;
	}

	state->parsed += 1;

	if (c == EOF)
		result->status = PARSER_FINISHED;
	else if (state->isBlock)
	{
		parseWordToken (c, token, "*/", TOKEN_COMMENT_BLOCK, &state->blockParsed, result);

		if (result->status == PARSER_FAILED)
		{
			state->blockParsed = c == '*' ? 1 : 0;
			result->status = PARSER_NEEDS_MORE_INPUT;
		}
	}
	else if (c == '\n')
		result->status = PARSER_FINISHED;

	if (result->status == PARSER_FINISHED)
	{
		initToken (token, TOKEN_COMMENT_BLOCK);
		return;
	}

	result->status = PARSER_NEEDS_MORE_INPUT;
}

CTAGS_INLINE void parseString(const int c, tokenInfo *const token, const char quote, char *prev, parserResult *const result)
{
	if (*prev == '\0')
	{
		if (c == quote)
		{
			*prev = c;
			result->status = PARSER_NEEDS_MORE_INPUT;
		}
		else
		{
			result->status = PARSER_FAILED;

			return;
		}
	}

	if (c == quote)
	{
		if (*prev == '\\')
		{
			*prev = c;
			result->status = PARSER_NEEDS_MORE_INPUT;
		}
		else
		{
			result->status = PARSER_FINISHED;

			initToken (token, TOKEN_STRING);

			return;
		}
	}

	*prev = c;
	result->status = PARSER_NEEDS_MORE_INPUT;
}

CTAGS_INLINE void parseBlock(const int c, tokenInfo *const token, tokenType const ttype, const char start, const char end, blockState *state, parserResult *const result)
{
	if (state->parsed == 0)
	{
		if (c != start)
		{
			result->status = PARSER_FAILED;
			return;
		}

		state->parsed = 1;
	}

	if (c == '{')
		state->curlyLevel += 1;
	else if (c == '}')
		state->curlyLevel -= 1;

	if (c == start)
		state->nestLevel += 1;
	else if (c == end)
		state->nestLevel -= 1;
	else if ((state->curlyLevel < 1 && c == ';') || c == EOF)
	{
		result->status = PARSER_FAILED;
		return;
	}

	if (state->nestLevel <= 0)
	{
		initToken (token, ttype);
		result->status = PARSER_FINISHED;

		return;
	}

	//skip comments:
	tryParser ((Parser) parseComment, token, false);

	result->status = PARSER_NEEDS_MORE_INPUT;
}


CTAGS_INLINE void parseIdentifierCommon(const int c, tokenInfo *const token, int *parsed, parserResult *const result,
										   bool acceptFqName)
{
	if (isIdentChar (c) || (acceptFqName && c == '.'))
	{
		vStringPut (token->string, c);
		*parsed = *parsed + 1;
		result->status = PARSER_NEEDS_MORE_INPUT;

		return;
	}

	if (*parsed > 0)
	{
		initToken (token, TOKEN_IDENTIFIER);
		result->status = PARSER_FINISHED;
		result->unusedChars = 1;
		return;
	}

	result->status = PARSER_FAILED;
}

CTAGS_INLINE void parseIdentifier(const int c, tokenInfo *const token, int *parsed, parserResult *const result)
{
	parseIdentifierCommon (c, token, parsed, result, false);
}

CTAGS_INLINE void parseFQIdentifier(const int c, tokenInfo *const token, int *parsed, parserResult *const result)
{
	parseIdentifierCommon (c, token, parsed, result, true);
}

PARSER_DEF (AsyncKeyword, parseWord, "async", num)
PARSER_DEF (ClassKeyword, parseWord, "class", num)
PARSER_DEF (ConstKeyword, parseWord, "const", num)
PARSER_DEF (ConstructorKeyword, parseWord, "constructor", num)
PARSER_DEF (EnumKeyword, parseWord, "enum", num)
PARSER_DEF (ForKeyword, parseWord, "for", num)
PARSER_DEF (FunctionKeyword, parseWord, "function", num)
PARSER_DEF (InKeyword, parseWord, "in", num)
PARSER_DEF (InterfaceKeyword, parseWord, "interface", num)
PARSER_DEF (LetKeyword, parseWord, "let", num)
PARSER_DEF (NamespaceKeyword, parseWord, "namespace", num)
PARSER_DEF (OfKeyword, parseWord, "of", num)
PARSER_DEF (PrivateKeyword, parseWord, "private", num)
PARSER_DEF (ProtectedKeyword, parseWord, "protected", num)
PARSER_DEF (PublicKeyword, parseWord, "public", num)
PARSER_DEF (ReadonlyKeyword, parseWord, "readonly", num)
PARSER_DEF (StaticKeyword, parseWord, "static", num)
PARSER_DEF (ThisKeyword, parseWord, "this", num)
PARSER_DEF (TypeKeyword, parseWord, "type", num)
PARSER_DEF (VarKeyword, parseWord, "var", num)
PARSER_DEF (WhileKeyword, parseWord, "while", num)

SINGLE_CHAR_PARSER_DEF (Semicolon, ';', TOKEN_SEMICOLON)
SINGLE_CHAR_PARSER_DEF (Comma, ',', TOKEN_COMMA)
SINGLE_CHAR_PARSER_DEF (Colon, ':', TOKEN_COLON)
SINGLE_CHAR_PARSER_DEF (Period, '.', TOKEN_PERIOD)
SINGLE_CHAR_PARSER_DEF (OpenCurly, '{', TOKEN_OPEN_CURLY)
SINGLE_CHAR_PARSER_DEF (CloseCurly, '}', TOKEN_CLOSE_CURLY)
SINGLE_CHAR_PARSER_DEF (OpenParen, '(', TOKEN_OPEN_PAREN)
SINGLE_CHAR_PARSER_DEF (CloseParen, ')', TOKEN_CLOSE_PAREN)
SINGLE_CHAR_PARSER_DEF (OpenSquare, '[', TOKEN_OPEN_SQUARE)
SINGLE_CHAR_PARSER_DEF (CloseSquare, ']', TOKEN_CLOSE_SQUARE)
SINGLE_CHAR_PARSER_DEF (EqualSign, '=', TOKEN_EQUAL_SIGN)
SINGLE_CHAR_PARSER_DEF (Star, '*', TOKEN_STAR)
SINGLE_CHAR_PARSER_DEF (At, '@', TOKEN_AT)
SINGLE_CHAR_PARSER_DEF (NewLine, '\n', TOKEN_NL)
SINGLE_CHAR_PARSER_DEF (QuestionMark, '?', TOKEN_QUESTION_MARK)
SINGLE_CHAR_PARSER_DEF (EOF, EOF, TOKEN_EOF)
SINGLE_CHAR_PARSER_DEF (Pipe, '|', TOKEN_PIPE)
SINGLE_CHAR_PARSER_DEF (Ampersand, '&', TOKEN_AMPERSAND)

WORD_TOKEN_PARSER_DEF (Arrow, "=>", TOKEN_ARROW)

PARSER_DEF (StringSQuote, parseString, '\'', ch)
PARSER_DEF (StringDQuote, parseString, '"', ch)
PARSER_DEF (StringTemplate, parseString, '`', ch)

BLOCK_PARSER_DEF (Parens, '(', ')', TOKEN_PARENS)
BLOCK_PARSER_DEF (Squares, '[', ']', TOKEN_SQUARES)
BLOCK_PARSER_DEF (Template, '<', '>', TOKEN_TEMPLATE)
BLOCK_PARSER_DEF (Curlies, '{', '}', TOKEN_CURLIES)

CTAGS_INLINE bool tryParser(Parser parser, tokenInfo *const token, bool skipWhite)
{
	parserState currentState;
	parserResult result;
	int c;

	result.status = PARSER_NEEDS_MORE_INPUT;
	result.unusedChars = 0;
	memset(&currentState, 0, sizeof (currentState));

	uwiPushMarker();

	while (result.status == PARSER_NEEDS_MORE_INPUT)
	{
		c = uwiGetC ();
		if (skipWhite && whiteChar (c))
		{
			do {
				c = uwiGetC ();
			} while (whiteChar (c));
			skipWhite = false;
		}

		parser (c, token, &currentState, &result);
	}

	if (result.status == PARSER_FAILED)
		uwiPopMarker (-1, true);
	else if (result.unusedChars > 0)
		uwiPopMarker (result.unusedChars, true);
	else
		uwiDropMaker ();

	return result.status == PARSER_FINISHED;
}

static bool tryInSequence(tokenInfo *const token, bool skipUnparsed, ...)
{
	Parser currentParser = NULL;
	bool result = false;

	tryParser (parseWhiteChars, token, false);

	va_list args;
	va_start (args, skipUnparsed);

	currentParser = va_arg (args, Parser);
	while (! result && currentParser)
	{
		result = tryParser (currentParser, token, false);
		currentParser = va_arg (args, Parser);
	}

	va_end (args);

	if (skipUnparsed && ! result) return uwiGetC () != EOF;

	return result;
}

MULTI_CHAR_PARSER_DEF (DecoratorChars, "@\n", TOKEN_AT, TOKEN_NL)
static void parseDecorator (tokenInfo *const token)
{
	bool parsed = false;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false, parseDecoratorChars, parseComment, NULL);
	} while (parsed && token->type != TOKEN_PARENS);

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false, parseNewLine, parseComment, parseIdentifier, NULL);
	} while (parsed && (token->type != TOKEN_IDENTIFIER || tryParser ((Parser) parsePeriod, token, true)));

	//parse optional parens block
	tryParser ((Parser) parseParens, token, true);
}

MULTI_CHAR_PARSER_DEF (SkipBlockChars, ";,", TOKEN_SEMICOLON, TOKEN_COMMA)
static void skipBlocksTillType (tokenType type, tokenInfo *const token)
{
	bool parsed;

	do
	{
		parsed = tryInSequence (token, true,
								parseSquares,
								parseParens,
								parseComment,
								parseTemplate,
								parseCurlies,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseSkipBlockChars,
								NULL);
	} while (parsed && ! isType (token, type));
}

static void parseInterfaceBody (const int scope, tokenInfo *const token)
{
	bool parsed;

	do
	{
		parsed = tryInSequence (token, true,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseParens,
								parseComment,
								parseSquares,
								parseOpenCurly,
								NULL);
	} while (parsed && ! isType (token, TOKEN_OPEN_CURLY));

	if (! parsed)
		return;

	tokenInfo *member = NULL;
	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, true,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseParens,
								parseComment,
								parseSquares,
								parsePrivateKeyword,
								parseProtectedKeyword,
								parsePublicKeyword,
								parseReadonlyKeyword,
								parseStaticKeyword,
								parseCloseCurly,
								parseIdentifier,
								NULL);

		if (parsed)
		{
			if (! member && isType (token, TOKEN_IDENTIFIER))
			{
				member = newToken ();
				copyToken (member, token, false);
				member->scope = scope;
			}
			else if (! member && isType (token, TOKEN_PARENS))
			{
				skipBlocksTillType (TOKEN_SEMICOLON, token);
			}
			else if (! member && isType (token, TOKEN_SQUARES))
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
				if (! isType (token, TOKEN_TEMPLATE))
				{
					emitTag (member, TSTAG_PROPERTY);
					deleteToken (member);
					member = NULL;
					skipBlocksTillType (TOKEN_SEMICOLON, token);
				}
			}
		}
	} while (parsed && ! isType (token, TOKEN_CLOSE_CURLY));

	if (member)
	{
		emitTag (member, TSTAG_PROPERTY);
		deleteToken (member);
	}
}

static void parseInterface (const int scope, tokenInfo *const token)
{
	bool parsed;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false,
								parseNewLine,
								parseComment,
								parseIdentifier,
								NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (! parsed)
		return;

	token->scope = scope;

	const int nscope = emitTag (token, TSTAG_INTERFACE);

	parseInterfaceBody (nscope, token);
}

static void parseType (const int scope, tokenInfo *const token)
{
	bool parsed;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false,
								parseComment,
								parseIdentifier,
								NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (! parsed)
		return;

	token->scope = scope;
	emitTag (token, TSTAG_ALIAS);
	skipBlocksTillType (TOKEN_SEMICOLON, token);
}

MULTI_CHAR_PARSER_DEF (EnumBodyChars, "},", TOKEN_CLOSE_CURLY, TOKEN_COMMA)
static void parseEnumBody (const int scope, tokenInfo *const token)
{
	bool parsed;

	do
	{
		parsed = tryInSequence (token, true,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseParens,
								parseComment,
								parseSquares,
								parseOpenCurly,
								NULL);
	} while (parsed && token->type != TOKEN_OPEN_CURLY);

	if (! parsed)
		return;

	tokenInfo *member = NULL;
	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, true,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseParens,
								parseComment,
								parseSquares,
								parseEnumBodyChars,
								parseIdentifier,
								NULL);

		if (parsed)
		{
			if (! member && isType (token, TOKEN_IDENTIFIER))
			{
				member = newToken ();
				copyToken (member, token, false);
				member->scope = scope;
				emitTag (member, TSTAG_ENUMERATOR);
			}
			else if (isType (token, TOKEN_COMMA))
			{
				deleteToken (member);
				member = NULL;
			}
		}
	} while (parsed && ! isType (token, TOKEN_CLOSE_CURLY));

	if (member)
	{
		deleteToken (member);
	}
}

static void parseEnum (const int scope, tokenInfo *const token)
{
	bool parsed;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false,
								parseComment,
								parseIdentifier,
								NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (! parsed)
		return;

	token->scope = scope;
	const int nscope = emitTag (token, TSTAG_ENUM);

	parseEnumBody (nscope, token);
}

MULTI_CHAR_PARSER_DEF (VariableChars, "|&=?[]{})\n:;,.",
		TOKEN_PIPE, TOKEN_AMPERSAND, TOKEN_EQUAL_SIGN, TOKEN_QUESTION_MARK,
		TOKEN_OPEN_SQUARE, TOKEN_CLOSE_SQUARE, TOKEN_OPEN_CURLY, TOKEN_CLOSE_CURLY,
		TOKEN_CLOSE_PAREN, TOKEN_NL, TOKEN_COLON, TOKEN_SEMICOLON, TOKEN_COMMA, TOKEN_PERIOD)
static void parseVariable (bool constVar, bool localVar, const int scope, tokenInfo *const token)
{
	tokenInfo *member = NULL;
	bool parsed = false;
	bool parsingType = false;
	int nestLevel = 0;
	tsKind varKind = constVar ? TSTAG_CONSTANT : (localVar ? TSTAG_LOCAL : TSTAG_VARIABLE);

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, true,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseComment,
								parseParens,
								parseNumber,
								parseVariableChars,
								parseArrow,
								parseForKeyword,
								parseWhileKeyword,
								parseThisKeyword,
								parseEnumKeyword,
								parseOfKeyword,
								parseInKeyword,
								parseIdentifier,
								NULL);

		if (parsed)
		{
			switch (token->type)
			{
				case TOKEN_OPEN_SQUARE:
				case TOKEN_OPEN_CURLY:
					if (parsingType)
						nestLevel += 1;
					break;
				case TOKEN_CLOSE_SQUARE:
				case TOKEN_CLOSE_CURLY:
					if (parsingType)
						nestLevel -= 1;
					break;
				case TOKEN_COMMA:
				case TOKEN_SEMICOLON:
				case TOKEN_CLOSE_PAREN:
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
					if (! parsingType)
					{
						member = newToken ();
						copyToken (member, token, false);
						member->scope = scope;
						emitTag (member, varKind);
						deleteToken (member);
					}
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword)
					{
						case KEYWORD_enum:
							parseEnum (scope, token);
							break;
						case KEYWORD_of:
						case KEYWORD_in:
							parsingType = true;
							break;
					}
					break;
				default:
					break;
			}
		}
	} while (parsed && ! ((token->type == TOKEN_SEMICOLON || token->type == TOKEN_CLOSE_PAREN) && ! parsingType));

	clearPoolToken (token);
}

MULTI_CHAR_PARSER_DEF (FunctionArgsChars, "\n(", TOKEN_NL, TOKEN_OPEN_PAREN)
MULTI_CHAR_PARSER_DEF (FunctionArgsAfterParenChars, "|&=?[]{})\n:,.@",
		TOKEN_PIPE, TOKEN_AMPERSAND, TOKEN_EQUAL_SIGN, TOKEN_QUESTION_MARK,
		TOKEN_OPEN_SQUARE, TOKEN_CLOSE_SQUARE, TOKEN_OPEN_CURLY, TOKEN_CLOSE_CURLY,
		TOKEN_CLOSE_PAREN, TOKEN_NL, TOKEN_COLON, TOKEN_COMMA, TOKEN_PERIOD, TOKEN_AT)
static void parseFunctionArgs (const int scope, tokenInfo *const token)
{
	bool parsed = false;
	bool parsingType = false;
	int nestLevel = 0;
	tokenInfo *member = NULL;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false,
								parseTemplate,
								parseComment,
								parseFunctionArgsChars,
								NULL);
	} while (parsed && token->type != TOKEN_OPEN_PAREN);

	if (! parsed)
		return;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseComment,
								parseParens,
								parseNumber,
								parseFunctionArgsAfterParenChars,
								parseArrow,
								parseIdentifier,
								NULL);

		if (parsed)
		{
			switch (token->type)
			{
				case TOKEN_AT:
					uwiUngetC ('@');
					parseDecorator (token);
					break;
				case TOKEN_OPEN_SQUARE:
				case TOKEN_OPEN_CURLY:
					if (parsingType)
						nestLevel += 1;
					break;
				case TOKEN_CLOSE_SQUARE:
				case TOKEN_CLOSE_CURLY:
					if (parsingType)
						nestLevel -= 1;
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
					if (! parsingType)
					{
						member = newToken ();
						copyToken (member, token, false);
						member->scope = scope;
						emitTag (member, TSTAG_PARAMETER);
						deleteToken (member);
					}
					break;
				default:
					break;
			}
		}
	} while (parsed && token->type != TOKEN_CLOSE_PAREN);
}

MULTI_CHAR_PARSER_DEF (FunctionBodyChars, "{}", TOKEN_OPEN_CURLY, TOKEN_CLOSE_CURLY)
static void parseFunctionBody (const int scope, tokenInfo *const token)
{
	bool parsed = false;
	int nestLevel = 1;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, true,
								parseOpenCurly,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseTemplate,
								parseComment,
								NULL);

	} while (parsed && ! isType (token, TOKEN_OPEN_CURLY));

	if (! parsed)
		return;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, true,
								parseFunctionBodyChars,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseComment,
								parseTemplate,
								parseVarKeyword,
								parseLetKeyword,
								parseConstKeyword,
								NULL);

		if (parsed)
		{
			switch (token->type)
			{
				case TOKEN_OPEN_CURLY:
					nestLevel += 1;
					break;
				case TOKEN_CLOSE_CURLY:
					nestLevel -= 1;
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword)
					{
						case KEYWORD_var:
						case KEYWORD_let:
							parseVariable (false, true, scope, token);
							break;
						case KEYWORD_const:
							parseVariable (true, true, scope, token);
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

static void parseFunction (const int scope, tokenInfo *const token)
{
	bool isGenerator = false;
	bool parsed;

	do
	{
		clearPoolToken (token);
		parsed = tryInSequence (token, false,
								parseComment,
								parseStar,
								parseIdentifier,
								NULL);

		if (parsed && isType (token, TOKEN_STAR))
			isGenerator = true;
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (! parsed)
		return;

	token->scope = scope;

	const int nscope = emitTag (token, isGenerator ? TSTAG_GENERATOR : TSTAG_FUNCTION);

	parseFunctionArgs (nscope, token);
	parseFunctionBody (nscope, token);
}

MULTI_CHAR_PARSER_DEF (PropertyTypeChars, "\n;|&=,)",
		TOKEN_NL, TOKEN_SEMICOLON, TOKEN_PIPE, TOKEN_AMPERSAND,
		TOKEN_EQUAL_SIGN, TOKEN_COMMA, TOKEN_CLOSE_PAREN)
static void parsePropertyType (tokenInfo *const token)
{
	bool parsed = tryParser ((Parser) parseColon, token, true);

	if (! parsed)
		return;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, true,
								parsePropertyTypeChars,
								parseArrow,
								parseTemplate,
								parseParens,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseComment,
								parseSquares,
								parseCurlies,
								parseIdentifier,
								NULL);
	} while (parsed && ! isType (token, TOKEN_CLOSE_PAREN) && ! isType (token, TOKEN_SEMICOLON) && ! isType (token, TOKEN_COMMA));

	if (! parsed)
		return;

	if (isType (token, TOKEN_CLOSE_PAREN))
		uwiUngetC (')');
	clearPoolToken (token);
}

MULTI_CHAR_PARSER_DEF (ConstructorParamsChars, "\n(", TOKEN_NL, TOKEN_OPEN_PAREN)
MULTI_CHAR_PARSER_DEF (ConstructorParamsAfterParenChars, "\n:,)@",
		TOKEN_NL, TOKEN_COLON, TOKEN_COMMA, TOKEN_CLOSE_PAREN, TOKEN_AT)
static void parseConstructorParams (const int classScope, const int constrScope, tokenInfo *const token)
{
	bool parsed = false;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false,
								parseConstructorParamsChars,
								parseComment,
								NULL);
	} while (parsed && ! isType (token, TOKEN_OPEN_PAREN));

	if (! parsed)
		return;

	tokenInfo *member = NULL;
	int visibility = 0;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false,
								parseConstructorParamsAfterParenChars,
								parseComment,
								parsePrivateKeyword,
								parseProtectedKeyword,
								parsePublicKeyword,
								parseReadonlyKeyword,
								parseStaticKeyword,
								parseIdentifier,
								NULL);

		if (parsed)
		{
			switch (token->type)
			{
				case TOKEN_AT:
					uwiUngetC ('@');
					parseDecorator (token);
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword)
					{
						case KEYWORD_private:
						case KEYWORD_public:
						case KEYWORD_protected:
							visibility = token->keyword;
							break;
					}
					break;
				case TOKEN_COLON:
					uwiUngetC (':');
					parsePropertyType (token);
					break;
				case TOKEN_IDENTIFIER:
					member = newToken ();
					copyToken (member, token, false);
					if (visibility)
					{
						member->accessKeyword = visibility;
						member->scope = classScope;
						emitTag (member, TSTAG_PROPERTY);
					}
					else
					{
						member->scope = constrScope;
						emitTag (member, TSTAG_PARAMETER);
					}
					deleteToken (member);
					member = NULL;
					visibility = 0;
					break;
				default:
					break;
			}
		}
	} while (parsed && ! isType (token, TOKEN_CLOSE_PAREN));

}

MULTI_CHAR_PARSER_DEF (ClassBodyChars, "\n{", TOKEN_NL, TOKEN_OPEN_CURLY)
MULTI_CHAR_PARSER_DEF (ClassBodyAfterCurlyChars, "\n}*@(:;=",
		TOKEN_NL, TOKEN_CLOSE_CURLY, TOKEN_STAR, TOKEN_AT, TOKEN_OPEN_PAREN,
		TOKEN_COLON, TOKEN_SEMICOLON, TOKEN_EQUAL_SIGN)
static void parseClassBody (const int scope, tokenInfo *const token)
{
	bool parsed = false;

	//parse until {
	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false,
								parseClassBodyChars,
								parseTemplate,
								parseComment,
								parseIdentifier,
								NULL);
	} while (parsed && token->type != TOKEN_OPEN_CURLY);

	if (! parsed)
		return;

	tokenInfo *member = NULL;
	bool isGenerator = false;
	int visibility = 0;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false,
								parseClassBodyAfterCurlyChars,
								parseComment,
								parseAsyncKeyword,
								parseConstructorKeyword,
								parsePrivateKeyword,
								parseProtectedKeyword,
								parsePublicKeyword,
								parseReadonlyKeyword,
								parseStaticKeyword,
								parseNumber,
								parseTemplate,
								parseIdentifier,
								NULL);

		if (parsed)
		{
			switch (token->type)
			{
				case TOKEN_AT:
					uwiUngetC ('@');
					parseDecorator (token);
					break;
				case TOKEN_KEYWORD:
					switch (token->keyword)
					{
						case KEYWORD_constructor:
							if (member)
								deleteToken (member);
							member = newToken ();
							copyToken (member, token, false);
							member->scope = scope;

							if (visibility)
								member->accessKeyword = visibility;
							else
								member->accessKeyword = KEYWORD_public;

							const int nscope = emitTag (member, TSTAG_METHOD);
							deleteToken (member);
							member = NULL;
							visibility = 0;

							parseConstructorParams (scope, nscope, token);

							parseFunctionBody (nscope, token);
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
					uwiUngetC (';');
					break;
				case TOKEN_COLON:
					uwiUngetC (':');
					parsePropertyType (token);
				case TOKEN_SEMICOLON:
					if (member)
					{
						emitTag (member, TSTAG_PROPERTY);
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
					if (! member)
						break;
					uwiUngetC ('(');

					const int nscope = emitTag (member, isGenerator ? TSTAG_GENERATOR : TSTAG_METHOD);

					deleteToken (member);
					member = NULL;

					parseFunctionArgs (nscope, token);
					parseFunctionBody (nscope, token);

					isGenerator = false;
					visibility = 0;
					break;
				case TOKEN_IDENTIFIER:
					if (member)
						deleteToken (member);
					member = newToken ();
					copyToken (member, token, false);
					member->scope = scope;
					if (visibility)
						member->accessKeyword = visibility;
					else
						member->accessKeyword = KEYWORD_public;
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
		emitTag (member, TSTAG_PROPERTY);
		deleteToken (member);
	}
	else if (member)
		deleteToken (member);
}

static void parseClass (const int scope, tokenInfo *const token)
{
	bool parsed = false;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false,
								parseNewLine,
								parseComment,
								parseIdentifier,
								NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (! parsed)
		return;

	token->scope = scope;
	const int nscope = emitTag (token, TSTAG_CLASS);

	parseClassBody (nscope, token);
}

MULTI_CHAR_PARSER_DEF (NamespaceBodyChars, "\n{", TOKEN_NL, TOKEN_OPEN_CURLY)
MULTI_CHAR_PARSER_DEF (NamespaceBodyAfterCurlyChars, "@{}()[]",
		TOKEN_AT, TOKEN_OPEN_CURLY, TOKEN_CLOSE_CURLY, TOKEN_OPEN_PAREN,
		TOKEN_CLOSE_PAREN, TOKEN_OPEN_SQUARE, TOKEN_CLOSE_SQUARE)
static void parseNamespaceBody (const int scope, tokenInfo *const token)
{
	bool parsed = false;

	//parse until {
	do
	{
		parsed = tryInSequence (token, false,
								parseNamespaceBodyChars,
								parseComment,
								NULL);
	} while (parsed && token->type != TOKEN_OPEN_CURLY);

	if (! parsed)
		return;

	int parenLvl = 0;
	int squareLvl = 0;
	int curlyLvl = 1;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, true,
								parseComment,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseInterfaceKeyword,
								parseTypeKeyword,
								parseEnumKeyword,
								parseFunctionKeyword,
								parseClassKeyword,
								parseVarKeyword,
								parseLetKeyword,
								parseConstKeyword,
								parseNamespaceBodyAfterCurlyChars,
								NULL);

		switch (token->type)
		{
			case TOKEN_KEYWORD:
				switch (token->keyword)
				{
					case KEYWORD_interface:
						parseInterface (scope, token);
						break;
					case KEYWORD_type:
						parseType (scope, token);
						break;
					case KEYWORD_enum:
						parseEnum (scope, token);
						break;
					case KEYWORD_function:
						parseFunction (scope, token);
						break;
					case KEYWORD_class:
						parseClass (scope, token);
						break;
					case KEYWORD_var:
					case KEYWORD_let:
						parseVariable (false, false, scope, token);
						break;
					case KEYWORD_const:
						parseVariable (true, false, scope, token);
						break;
				}
				break;
			case TOKEN_AT:
				uwiUngetC ('@');
				parseDecorator (token);
				break;
			case TOKEN_OPEN_CURLY:
				curlyLvl += 1;
				break;
			case TOKEN_OPEN_SQUARE:
				squareLvl += 1;
				break;
			case TOKEN_OPEN_PAREN:
				parenLvl += 1;
				break;
			case TOKEN_CLOSE_CURLY:
				curlyLvl -= 1;
				break;
			case TOKEN_CLOSE_SQUARE:
				squareLvl -= 1;
				break;
			case TOKEN_CLOSE_PAREN:
				parenLvl -= 1;
				break;
			default:
				break;
		}
	} while (parsed && ! (isType (token, TOKEN_CLOSE_CURLY) && parenLvl <= 0 && squareLvl <= 0 && curlyLvl <= 0));
}

static void parseNamespace (tokenInfo *const token)
{
	bool parsed = false;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, false,
								parseNewLine,
								parseComment,
								parseFQIdentifier,
								NULL);
	} while (parsed && token->type != TOKEN_IDENTIFIER);

	if (! parsed)
		return;

	const int scope = emitTag (token, TSTAG_NAMESPACE);

	parseNamespaceBody (scope, token);
}

static void parseTsFile (tokenInfo *const token)
{
	bool parsed;

	do
	{
		clearPoolToken (token);

		parsed = tryInSequence (token, true,
								parseComment,
								parseTemplate,
								parseStringSQuote,
								parseStringDQuote,
								parseStringTemplate,
								parseInterfaceKeyword,
								parseTypeKeyword,
								parseEnumKeyword,
								parseFunctionKeyword,
								parseClassKeyword,
								parseNamespaceKeyword,
								parseVarKeyword,
								parseLetKeyword,
								parseConstKeyword,
								parseAt,
								NULL);

		switch (token->type)
		{
			case TOKEN_KEYWORD:
				switch (token->keyword)
				{
					case KEYWORD_interface:
						parseInterface (CORK_NIL, token);
						break;
					case KEYWORD_type:
						parseType (CORK_NIL, token);
						break;
					case KEYWORD_enum:
						parseEnum (CORK_NIL, token);
						break;
					case KEYWORD_function:
						parseFunction (CORK_NIL, token);
						break;
					case KEYWORD_class:
						parseClass (CORK_NIL, token);
						break;
					case KEYWORD_namespace:
						parseNamespace (token);
						break;
					case KEYWORD_var:
					case KEYWORD_let:
						parseVariable (false, false, CORK_NIL, token);
						break;
					case KEYWORD_const:
						parseVariable (true, false, CORK_NIL, token);
						break;
				}
				break;
			case TOKEN_AT:
				uwiUngetC ('@');
				parseDecorator (token);
				break;
			default:
				break;
		}
	} while (parsed);
}

static void findTsTags (void)
{
	uwiActivate (256);

	tokenInfo *const token = newToken ();

	parseTsFile (token);

	deleteToken (token);

	uwiDeactivate (&tsUwiStats);
}

static void initialize (const langType language)
{
	Lang_ts = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (! initialized)
		return;

	objPoolDelete (TokenPool);
}

static void initStats (langType language CTAGS_ATTR_UNUSED)
{
	uwiStatsInit (&tsUwiStats);
}
static void printStats (langType language CTAGS_ATTR_UNUSED)
{
	uwiStatsPrint (&tsUwiStats);
}

/* Create parser definition structure */
extern parserDefinition *TypeScriptParser (void)
{
	static const char *const extensions [] = { "ts", NULL };
	parserDefinition *const def = parserNew ("TypeScript");
	def->extensions = extensions;
	def->kindTable  = TsKinds;
	def->kindCount  = ARRAY_SIZE (TsKinds);
	def->parser     = findTsTags;
	def->initialize = initialize;
	def->finalize   = finalize;
	def->keywordTable = TsKeywordTable;
	def->keywordCount = ARRAY_SIZE (TsKeywordTable);
	def->useCork = true;
	def->requestAutomaticFQTag = true;

	def->initStats = initStats;
	def->printStats = printStats;

	return def;
}
