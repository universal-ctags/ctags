/*
*   $Id$
*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for PL/SQL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <ctype.h>  /* to define isalpha () */
#include <setjmp.h>

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   On-line PL/SQL Reference Guide:
*   http://info-it.umsystem.edu/oradocs/doc/server/doc/PLS23/toc.htm
*
*   Sample PL/SQL code is available from:
*   http://www.orafaq.com/faqscrpt.htm#GENPLSQL
*/

/*
*   MACROS
*/
#define isType(token,t)     (boolean) ((token)->type == (t))
#define isKeyword(token,k)  (boolean) ((token)->keyword == (k))

/*
*   DATA DECLARATIONS
*/

typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

/*  Used to specify type of keyword.
 */
typedef enum eKeywordId {
	KEYWORD_NONE = -1,
	KEYWORD_is,
	KEYWORD_begin,
	KEYWORD_body,
	KEYWORD_cursor,
	KEYWORD_declare,
	KEYWORD_end,
	KEYWORD_function,
	KEYWORD_if,
	KEYWORD_loop,
	KEYWORD_package,
	KEYWORD_pragma,
	KEYWORD_procedure,
	KEYWORD_record,
	KEYWORD_ref,
	KEYWORD_rem,
	KEYWORD_return,
	KEYWORD_subtype,
	KEYWORD_table,
	KEYWORD_trigger,
	KEYWORD_type
} keywordId;

/*  Used to determine whether keyword is valid for the token language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
	const char *name;
	keywordId id;
} keywordDesc;

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_BLOCK_LABEL_BEGIN,
	TOKEN_BLOCK_LABEL_END,
	TOKEN_CHARACTER,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COMMA,
	TOKEN_IDENTIFIER,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_OPERATOR,
	TOKEN_OTHER,
	TOKEN_STRING
} tokenType;

typedef struct sTokenInfo {
	tokenType  type;
	keywordId  keyword;
	vString*   string;
	unsigned long lineNumber;
	fpos_t     filePosition;
} tokenInfo;

/*
*   DATA DEFINITIONS
*/

static langType Lang_sql;

static jmp_buf Exception;

typedef enum {
	SQLTAG_CURSOR,
	SQLTAG_PROTOTYPE,
	SQLTAG_FUNCTION,
	SQLTAG_FIELD,
	SQLTAG_LOCAL_VARIABLE,
	SQLTAG_BLOCK_LABEL,
	SQLTAG_PACKAGE,
	SQLTAG_PROCEDURE,
	SQLTAG_RECORD,
	SQLTAG_SUBTYPE,
	SQLTAG_TABLE,
	SQLTAG_TRIGGER,
	SQLTAG_VARIABLE,
	SQLTAG_COUNT
} sqlKind;

static kindOption SqlKinds [] = {
	{ TRUE,  'c', "cursor",    "cursors"         },
	{ FALSE, 'd', "prototype", "prototypes"      },
	{ TRUE,  'f', "function",  "functions"       },
	{ TRUE,  'F', "field",     "record fields"   },
	{ FALSE, 'l', "local",     "local variables" },
	{ TRUE,  'L', "label",     "block label"     },
	{ TRUE,  'P', "package",   "packages"        },
	{ TRUE,  'p', "procedure", "procedures"      },
	{ TRUE,  'r', "record",    "records"         },
	{ TRUE,  's', "subtype",   "subtypes"        },
	{ TRUE,  't', "table",     "tables"          },
	{ TRUE,  'T', "trigger",   "triggers"        },
	{ TRUE,  'v', "variable",  "variables"       },
};

static const keywordDesc SqlKeywordTable [] = {
	/* keyword          keyword ID */
	{ "as",         KEYWORD_is         },
	{ "begin",      KEYWORD_begin      },
	{ "body",       KEYWORD_body       },
	{ "cursor",     KEYWORD_cursor     },
	{ "declare",    KEYWORD_declare    },
	{ "end",        KEYWORD_end        },
	{ "function",   KEYWORD_function   },
	{ "if",         KEYWORD_if         },
	{ "is",         KEYWORD_is         },
	{ "loop",       KEYWORD_loop       },
	{ "package",    KEYWORD_package    },
	{ "pragma",     KEYWORD_pragma     },
	{ "procedure",  KEYWORD_procedure  },
	{ "record",     KEYWORD_record     },
	{ "ref",        KEYWORD_ref        },
	{ "rem",        KEYWORD_rem        },
	{ "return",     KEYWORD_return     },
	{ "subtype",    KEYWORD_subtype    },
	{ "table",      KEYWORD_table      },
	{ "trigger",    KEYWORD_trigger    },
	{ "type",       KEYWORD_type       }
};

/*
*   FUNCTION DECLARATIONS
*/

static void parseBlock (tokenInfo *const token, const boolean local);

/*
*   FUNCTION DEFINITIONS
*/

static boolean isIdentChar1 (const int c)
{
	return (boolean) isalpha (c);
}

static boolean isIdentChar (const int c)
{
	return (boolean)
		(isalpha (c) || isdigit (c) || c == '$' || c == '_' || c == '#');
}

static void buildSqlKeywordHash (void)
{
	const size_t count = sizeof (SqlKeywordTable) /
						 sizeof (SqlKeywordTable [0]);
	size_t i;
	for (i = 0  ;  i < count  ;  ++i)
	{
		const keywordDesc* const p = &SqlKeywordTable [i];
		addKeyword (p->name, Lang_sql, (int) p->id);
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type         = TOKEN_UNDEFINED;
	token->keyword      = KEYWORD_NONE;
	token->string       = vStringNew ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	eFree (token);
}

/*
*   Tag generation functions
*/

static void makeSqlTag (tokenInfo *const token, const sqlKind kind)
{
	if (SqlKinds [kind].enabled)
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;
		e.kindName     = SqlKinds [kind].name;
		e.kind         = SqlKinds [kind].letter;

		makeTagEntry (&e);
	}
}

/*
*   Parsing functions
*/

static int skipToCharacter (const int c)
{
	int d;
	do
	{
		d = fileGetc ();
	} while (d != EOF  &&  d != c);
	return d;
}

static void parseString (vString *const string, const int delimiter)
{
	boolean end = FALSE;
	int c;
	while (! end)
	{
		c = fileGetc ();
		if (c == EOF)
			end = TRUE;
		else if (c == delimiter)
			end = TRUE;
		else
			vStringPut (string, c);
	}
	vStringTerminate (string);
}

/*  Read a C identifier beginning with "firstChar" and places it into "name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar1 (c));
	do
	{
		vStringPut (string, c);
		c = fileGetc ();
	} while (isIdentChar (c));
	vStringTerminate (string);
	if (!isspace (c))
		fileUngetc (c);  /* unget non-identifier character */
}

static keywordId analyzeToken (vString *const name)
{
	static vString *keyword = NULL;
	if (keyword == NULL)
		keyword = vStringNew ();
	vStringCopyToLower (keyword, name);
	return (keywordId) lookupKeyword (vStringValue (keyword), Lang_sql);
}

static void readToken (tokenInfo *const token)
{
	int c;

	token->type         = TOKEN_UNDEFINED;
	token->keyword      = KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:
	do
		c = fileGetc ();
	while (c == '\t'  ||  c == ' '  ||  c == '\n');

	switch (c)
	{
		case EOF: longjmp (Exception, (int)ExceptionEOF);  break;
		case '(': token->type = TOKEN_OPEN_PAREN;          break;
		case ')': token->type = TOKEN_CLOSE_PAREN;         break;
		case ';': token->type = TOKEN_SEMICOLON;           break;
		case ',': token->type = TOKEN_COMMA;               break;

		case '\'':
		case '"':
			token->type = TOKEN_STRING;
			parseString (token->string, c);
			break;

		case '-':
			c = fileGetc ();
			if (c == '-')  /* is this the start of a comment? */
			{
				skipToCharacter ('\n');
				goto getNextChar;
			}
			else
			{
				if (!isspace (c))
					fileUngetc (c);
				token->type = TOKEN_OPERATOR;
			}
			break;

		case '<':
		case '>':
		{
			const int initial = c;
			int d = fileGetc ();
			if (d == initial)
			{
				if (initial == '<')
					token->type = TOKEN_BLOCK_LABEL_BEGIN;
				else
					token->type = TOKEN_BLOCK_LABEL_END;
			}
			else
			{
				fileUngetc (d);
				token->type = TOKEN_UNDEFINED;
			}
			break;
		}

		case '/':
		{
			int d = fileGetc ();
			if (d != '*')  /* is this the start of a comment? */
				fileUngetc (d);
			else
			{
				do
				{
					skipToCharacter ('*');
					c = fileGetc ();
					if (c == '/')
						break;
					else
						fileUngetc (c);
				} while (c != '\0');
				goto getNextChar;
			}
			break;
		}

		default:
			if (! isIdentChar1 (c))
				token->type = TOKEN_UNDEFINED;
			else
			{
				parseIdentifier (token->string, c);
				token->lineNumber = getSourceLineNumber ();
				token->filePosition = getInputFilePosition ();
				token->keyword = analyzeToken (token->string);
				if (isKeyword (token, KEYWORD_rem))
				{
					vStringClear (token->string);
					skipToCharacter ('\n');
					goto getNextChar;
				}
				else if (isKeyword (token, KEYWORD_NONE))
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}
}

/*
*   Scanning functions
*/

static void findToken (tokenInfo *const token, const tokenType type)
{
	while (! isType (token, type))
		readToken (token);
}

static void skipArgumentList (tokenInfo *const token)
{
	if (isType (token, TOKEN_OPEN_PAREN))  /* arguments? */
	{
		findToken (token, TOKEN_CLOSE_PAREN);
		readToken (token);
	}
}

static void parseSubProgram (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	const sqlKind kind = isKeyword (token, KEYWORD_function) ?
			SQLTAG_FUNCTION : SQLTAG_PROCEDURE;
	Assert (isKeyword (token, KEYWORD_function) ||
			isKeyword (token, KEYWORD_procedure));
	readToken (name);
	readToken (token);
	skipArgumentList (token);
	if (isKeyword (token, KEYWORD_return))
	{
		do
			readToken (token);  /* read return type */
		while (!(isKeyword (token, KEYWORD_is) ||
					isType (token, TOKEN_SEMICOLON)));
	}
	if (isKeyword (token, KEYWORD_is))
	{
		if (isType (name, TOKEN_IDENTIFIER))
			makeSqlTag (name, kind);
		readToken (token);
		parseBlock (token, TRUE);
	}
	else if (isType (token, TOKEN_SEMICOLON))
		makeSqlTag (name, SQLTAG_PROTOTYPE);
	deleteToken (name);
}

static void parseRecord (tokenInfo *const token)
{
	Assert (isType (token, TOKEN_OPEN_PAREN));
	do
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			makeSqlTag (token, SQLTAG_FIELD);
		while (!(isType (token, TOKEN_COMMA) ||
				 isType (token, TOKEN_CLOSE_PAREN)))
			readToken (token);
	} while (! isType (token, TOKEN_CLOSE_PAREN));
}

static void parseType (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	readToken (name);
	if (isType (name, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_is))
		{
			readToken (token);
			switch (token->keyword)
			{
				case KEYWORD_record:
					makeSqlTag (name, SQLTAG_RECORD);
					parseRecord (token);
					break;

				case KEYWORD_table:
					makeSqlTag (name, SQLTAG_TABLE);
					break;

				case KEYWORD_ref:
					readToken (token);
					if (isKeyword (token, KEYWORD_cursor))
						makeSqlTag (name, SQLTAG_CURSOR);
					break;

				default: break;
			}
		}
	}
	deleteToken (name);
}

static void parseSimple (tokenInfo *const token, const sqlKind kind)
{
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
		makeSqlTag (token, kind);
}

static void parseDeclare (tokenInfo *const token, const boolean local)
{
	if (isKeyword (token, KEYWORD_declare))
		readToken (token);
	while (! isKeyword (token, KEYWORD_begin) && ! isKeyword (token, KEYWORD_end))
	{
		switch (token->keyword)
		{
			case KEYWORD_cursor:    parseSimple (token, SQLTAG_CURSOR); break;
			case KEYWORD_function:  parseSubProgram (token); break;
			case KEYWORD_procedure: parseSubProgram (token); break;
			case KEYWORD_subtype:   parseSimple (token, SQLTAG_SUBTYPE); break;
			case KEYWORD_trigger:   parseSimple (token, SQLTAG_TRIGGER); break;
			case KEYWORD_type:      parseType (token); break;

			default:
				if (isType (token, TOKEN_IDENTIFIER))
				{
					if (local)
						makeSqlTag (token, SQLTAG_LOCAL_VARIABLE);
					else
						makeSqlTag (token, SQLTAG_VARIABLE);
				}
				break;
		}
		findToken (token, TOKEN_SEMICOLON);
		readToken (token);
	}
}

static void parseLabel (tokenInfo *const token)
{
	Assert (isType (token, TOKEN_BLOCK_LABEL_BEGIN));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		makeSqlTag (token, SQLTAG_BLOCK_LABEL);
		readToken (token);  /* read end of label */
	}
}

static void parseStatements (tokenInfo *const token)
{
	do
	{
		if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
			parseLabel (token);
		else
		{
			switch (token->keyword)
			{
				case KEYWORD_if:
				case KEYWORD_loop:
					readToken (token);
					parseStatements (token);
					break;

				case KEYWORD_declare:
				case KEYWORD_begin:
					parseBlock (token, TRUE);
					break;

				default:
					readToken (token);
					break;
			}
			findToken (token, TOKEN_SEMICOLON);
		}
		readToken (token);
	} while (! isKeyword (token, KEYWORD_end));
}

static void parseBlock (tokenInfo *const token, const boolean local)
{
	if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
	{
		parseLabel (token);
		readToken (token);
	}
	if (! isKeyword (token, KEYWORD_begin))
		parseDeclare (token, local);
	if (isKeyword (token, KEYWORD_begin))
	{
		readToken (token);
		while (! isKeyword (token, KEYWORD_end))
			parseStatements (token);
		findToken (token, TOKEN_SEMICOLON);
	}
}

static void parsePackage (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	readToken (name);
	if (isKeyword (name, KEYWORD_body))
		readToken (name);
	readToken (token);
	if (isKeyword (token, KEYWORD_is))
	{
		if (isType (name, TOKEN_IDENTIFIER))
			makeSqlTag (name, SQLTAG_PACKAGE);
		readToken (token);
		parseBlock (token, FALSE);
	}
	findToken (token, TOKEN_SEMICOLON);
	deleteToken (name);
}

static void parseTable (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		if (isType (name, TOKEN_IDENTIFIER))
		{
			makeSqlTag (name, SQLTAG_TABLE);
			parseRecord (token);
		}
	}
	findToken (token, TOKEN_SEMICOLON);
	deleteToken (name);
}

static void parseSqlFile (tokenInfo *const token)
{
	do
	{
		readToken (token);
		if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
			parseLabel (token);
		else switch (token->keyword)
		{
			case KEYWORD_begin:     parseBlock (token, FALSE); break;
			case KEYWORD_cursor:    parseSimple (token, SQLTAG_CURSOR); break;
			case KEYWORD_declare:   parseBlock (token, FALSE); break;
			case KEYWORD_function:  parseSubProgram (token); break;
			case KEYWORD_package:   parsePackage (token); break;
			case KEYWORD_procedure: parseSubProgram (token); break;
			case KEYWORD_subtype:   parseSimple (token, SQLTAG_SUBTYPE); break;
			case KEYWORD_table:     parseTable (token); break;
			case KEYWORD_trigger:   parseSimple (token, SQLTAG_TRIGGER); break;
			case KEYWORD_type:      parseType (token); break;
			default:                break;
		}
	} while (! isKeyword (token, KEYWORD_end));
}

static void initialize (const langType language)
{
	Assert (sizeof (SqlKinds) / sizeof (SqlKinds [0]) == SQLTAG_COUNT);
	Lang_sql = language;
	buildSqlKeywordHash ();
}

static void findSqlTags (void)
{
	tokenInfo *const token = newToken ();
	exception_t exception = (exception_t) (setjmp (Exception));
	while (exception == ExceptionNone)
		parseSqlFile (token);
	deleteToken (token);
}

extern parserDefinition* SqlParser (void)
{
	static const char *const extensions [] = { "sql", NULL };
	parserDefinition* def = parserNew ("SQL");
	def->kinds      = SqlKinds;
	def->kindCount  = KIND_COUNT (SqlKinds);
	def->extensions = extensions;
	def->parser     = findSqlTags;
	def->initialize = initialize;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
