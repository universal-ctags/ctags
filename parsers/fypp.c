	/*
 *   Copyright (c) 2018, Peter Vitt
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Fypp, a Python powered
 *   Fortran preprocessor.
 *   See: https://github.com/aradi/fypp
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>

#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "xtag.h"

/*
 *   MACROS
 */
#define isident(c)              (isalnum(c) || (c) == '_')
#define isBlank(c)              (bool) (c == ' ' || c == '\t')
#define isType(token,t)         (bool) ((token)->type == (t))
#define isKeyword(token,k)      (bool) ((token)->keyword == (k))
#define isSecondaryKeyword(token,k)  (bool) ((token)->secondary == NULL ? \
	false : (token)->secondary->keyword == (k))

/*
 *   DATA DECLARATIONS
 */
enum eKeywordID {
	KEYWORD_def,
	KEYWORD_enddef,
	KEYWORD_for,
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_elif,
	KEYWORD_endif,
	KEYWORD_del,
	KEYWORD_set,
	KEYWORD_mute,
	KEYWORD_stop,
	KEYWORD_assert,
	KEYWORD_include,
	KEYWORD_getvar,
	KEYWORD_setvar,
	KEYWORD_delvar,
	KEYWORD_globalvar,
	KEYWORD_defined,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_CONTROL,
	TOKEN_DIRECTCALL,
	TOKEN_EVAL,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_INLINESTART,
	TOKEN_INLINEEND,
	TOKEN_LINE,
	TOKEN_LINECONTINUE,
	TOKEN_NUMERIC,
	TOKEN_PAREN_OPEN,
	TOKEN_PAREN_CLOSE,
	TOKEN_COMMA,
	TOKEN_STATEMENT_END,
} tokenType;

typedef enum eTagType {
	TAG_UNDEFINED = -1,
	TAG_MACRO_DEFINITION,
	TAG_CONDITION,
	TAG_VAR_DEFINITION,
	TAG_VAR_REMOVAL,
	TAG_ITERATOR,
	TAG_INCLUDE,
	TAG_COMMENT,
	TAG_MUTE,
	TAG_STOP,
	TAG_ASSERT,
	TAG_COUNT /* must be last */
} tagType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	tagType tag;
	vString* string;
	vString* parentType;
	vString* signature;
	bool isMethod;
	struct sTokenInfo* secondary; /* TODO: make use of this */
	unsigned long lineNumber;
	MIOPos filePosition;
} tokenInfo;

static langType Lang_fypp;
static int Ungetc;

static kindDefinition FyppKinds [] = {
	{true,  'd', "def",        "definition"},
	{false, 'f', "for",        "for loop"},
	{false, 'i', "if",         "condition"},
	{false, 'D', "del",        "remove variable"},
	{false, 'S', "set",        "set variable"},
	{false, 'e', "eval",       "evaluate"},
	{false, 'c', "call",       "call"},
	{false, 'C', "directcall", "direct call"},
	{false, 'g', "global",     "global"},
	{false, 'I', "include",    "include"},
	{false, 'm', "mute",       "mute"},
	{false, 's', "stop",       "stop"},
	{false, 'a', "assert",     "assert"},
};

static const keywordTable FyppKeywordTable [] = {
	{"def",        KEYWORD_def},
	{"enddef",     KEYWORD_enddef},
	{"for",        KEYWORD_for},
	{"if",         KEYWORD_if},
	{"else",       KEYWORD_else},
	{"elif",       KEYWORD_elif},
	{"endif",      KEYWORD_endif},
	{"del",        KEYWORD_del},
	{"set",        KEYWORD_set},
	{"mute",       KEYWORD_mute},
	{"stop",       KEYWORD_stop},
	{"assert",     KEYWORD_assert},
	{"include",    KEYWORD_include},
	{"getvar",     KEYWORD_getvar},
	{"setvar",     KEYWORD_setvar},
	{"delvar",     KEYWORD_delvar},
	{"globalvar",  KEYWORD_globalvar},
	{"defined",    KEYWORD_defined},
};

/*
 *   Tag generation functions
 */

static tokenInfo* newToken (void)
{
	tokenInfo* const token = xMalloc (1, tokenInfo);

	token->type         = TOKEN_UNDEFINED;
	token->keyword      = KEYWORD_NONE;
	token->tag          = TAG_UNDEFINED;
	token->string       = vStringNew ();
	token->secondary    = NULL;
	token->parentType   = NULL;
	token->signature    = NULL;
	token->isMethod     = false;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static tokenInfo *newTokenFrom (tokenInfo *const token)
{
	tokenInfo *result = xMalloc (1, tokenInfo);
	*result = *token;
	result->string = vStringNewCopy (token->string);
	token->secondary = NULL;
	token->parentType = NULL;
	token->signature = NULL;
	return result;
}


static void deleteToken (tokenInfo *const token)
{
	if (token != NULL)
	{
		vStringDelete (token->string);
		vStringDelete (token->parentType);
		vStringDelete (token->signature);
		deleteToken (token->secondary);
		token->secondary = NULL;
		eFree (token);
	}
}

static bool includeTag (const tagType type)
{
	bool include;
	Assert (type != TAG_UNDEFINED);
	include = FyppKinds [(int) type].enabled;
	if (include)
		include = isXtagEnabled(XTAG_FILE_SCOPE);
	return include;
}

static void makeFyppTag (tokenInfo *const token, tagType tag)
{
	token->tag = tag;
	if (includeTag (token->tag))
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;

		initTagEntry (&e, name, token->tag);

		e.lineNumber	= token->lineNumber;
		e.filePosition	= token->filePosition;
		e.truncateLineAfterTag = true;
		if (token->signature
				&& vStringLength (token->signature) > 0
				&& token->tag == TAG_MACRO_DEFINITION)
			e.extensionFields.signature = vStringValue (token->signature);
		makeTagEntry (&e);
	}
}

static int getChar (void)
{
	bool advanceLine = false;
	int c;

	if (Ungetc != '\0')
	{
		c = Ungetc;
		Ungetc = '\0';
	}
	else
	{
		c = getcFromInputFile ();

		/* If the last nonblank, non-comment character of a Fypp text line is an
		 * ampersand, then the next non-comment line is a continuation line.
		 */
		if (c == '&')
		{
			do
				c = getcFromInputFile ();
			while (isspace (c)  &&  c != '\n');
			if (c == '\n')
			{
				advanceLine = true;
			}
			else
			{
				ungetcToInputFile (c);
				c = '&';
			}
		}
		while (advanceLine)
		{
			while (isspace (c))
				c = getcFromInputFile ();
			if (c == '&')
				c = getcFromInputFile ();
			else
				advanceLine = false;
		}
	}
	return c;
}

static void ungetChar (const int c)
{
	Ungetc = c;
}

/*  If a numeric is passed in 'c', this is used as the first digit of the
 *  numeric being parsed.
 */
static vString *parseInteger (int c)
{
	vString *string = vStringNew ();

	if (c == '-')
	{
		vStringPut (string, c);
		c = getChar ();
	}
	else if (! isdigit (c))
		c = getChar ();
	while (c != EOF  &&  isdigit (c))
	{
		vStringPut (string, c);
		c = getChar ();
	}

	if (c == '_')
	{
		do
			c = getChar ();
		while (c != EOF  &&  isalpha (c));
	}
	ungetChar (c);

	return string;
}

static vString *parseNumeric (int c)
{
	vString *string = parseInteger (c);

	c = getChar ();
	if (c == '.')
	{
		vString *integer = parseInteger ('\0');
		vStringPut (string, c);
		vStringCat (string, integer);
		vStringDelete (integer);
		c = getChar ();
	}
	if (tolower (c) == 'e')
	{
		vString *integer = parseInteger ('\0');
		vStringPut (string, c);
		vStringCat (string, integer);
		vStringDelete (integer);
	}
	else
		ungetChar (c);

	return string;
}

/*
 *  Read a C identifier beginning with "firstChar" and places it into "string".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;

	do
	{
		vStringPut (string, c);
		c = getChar ();
	} while (isident (c));

	ungetChar (c);  /* unget non-identifier character */
}

static void readIdentifier (tokenInfo *const token, const int c)
{
	parseIdentifier (token->string, c);
	token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_fypp);
	if (! isKeyword (token, KEYWORD_NONE))
		token->type = TOKEN_KEYWORD;
	else
	{
		token->type = TOKEN_IDENTIFIER;
	}
}

/*
 * Reads from the input and tries to find known tokens.
 */
static void readToken (tokenInfo *const token)
{
	int c;

	deleteToken (token->secondary);
	token->type = TOKEN_UNDEFINED;
	token->tag = TAG_UNDEFINED;
	token->keyword = KEYWORD_NONE;
	token->secondary = NULL;
	vStringClear (token->string);
	vStringDelete (token->parentType);
	vStringDelete (token->signature);
	token->parentType = NULL;
	token->isMethod = false;
	token->signature = NULL;

getNextChar:
	c = getChar ();

	token->lineNumber	= getInputLineNumber ();
	token->filePosition	= getInputFilePosition ();

	switch (c)
	{
		case EOF:  token->type = TOKEN_EOF;           break;
		case ' ':  goto getNextChar;
		case '\t': goto getNextChar;
		case '\n': token->type = TOKEN_STATEMENT_END; break;
		case '#':  token->type = TOKEN_CONTROL;       break;
		case '$':  token->type = TOKEN_EVAL;          break;
		case '@':  token->type = TOKEN_DIRECTCALL;    break;
		case ':':  token->type = TOKEN_LINE;          break;
		case '{':  token->type = TOKEN_INLINESTART;   break;
		case '}':  token->type = TOKEN_INLINEEND;     break;
		case '&':  token->type = TOKEN_LINECONTINUE;  break;
		case '(':  token->type = TOKEN_PAREN_OPEN;    break;
		case ')':  token->type = TOKEN_PAREN_CLOSE;   break;
		case ',':  token->type = TOKEN_COMMA;         break;

		default:
			if (isalpha (c))
				readIdentifier (token, c);
			else if (isdigit (c))
			{
				vString *numeric = parseNumeric (c);
				vStringCat (token->string, numeric);
				vStringDelete (numeric);
				token->type = TOKEN_NUMERIC;
			}
			else
				token->type = TOKEN_UNDEFINED;
			break;
	}
}

/* ########## PARSE TOKENS ########## */

/*
 * Parses a macro definition and creates a tag out of it.
 */
static void parseDef (tokenInfo* const token)
{
	Assert (isKeyword (token, KEYWORD_def));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		tokenInfo* name = newTokenFrom (token);
		makeFyppTag (name, TAG_MACRO_DEFINITION);
		deleteToken (name);
	}
}

/*
 * Tries to read one token after another from the file until the end is reached.
 */
static void parse (tokenInfo *const token)
{
	readToken (token);

	do
	{
		switch (token->keyword)
		{
			case KEYWORD_def: parseDef (token); break;
			/* TODO Add more keyword handlers */
			default:
				readToken (token);
				break;
		}
	} while (! isType (token, TOKEN_EOF) );
}

/*
 * Callback for ctags to parse a Fypp file.
 * This routine is called from ctags when a fypp file needs to be parsed. It
 * simply calls parse, where the actual work is done.
 */
static rescanReason parseFypp (const unsigned int passCount)
{
	tokenInfo *token;

	token = newToken ();

	parse (token);
	deleteToken (token);

	return RESCAN_NONE;
}

/*
 * Initialize the parser.
 * This basically means to store the langType provided by ctags for looging up
 * some keywords.
 */
static void initialize (const langType language)
{
	Lang_fypp = language;
}

/*
 * Setup the parser definition for ctags to know what this parser is
 * capable of
 */
extern parserDefinition* FyppParser (void)
{
	static const char* const extensions [] = {
		"fy",
#ifndef CASE_INSENSITIVE_FILENAMES
		"FY",
#endif
		NULL
	};
	parserDefinition* def = parserNew("Fypp");
	def->kindTable = FyppKinds;
	def->kindCount = ARRAY_SIZE (FyppKinds);
	def->extensions = extensions;
	def->parser2 = parseFypp;
	def->initialize = initialize;
	def->keywordTable = FyppKeywordTable;
	def->keywordCount = ARRAY_SIZE (FyppKeywordTable);
	return def;
}
