/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for COBOL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */
#include "entry.h"
#include "keyword.h"
#include "nestlevel.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

typedef enum {
	K_FILE,
	K_GROUP,
	K_PROGRAM,
	K_SECTION,
	K_DIVISION,
	K_PARAGRAPH,
	K_DATA,
	K_SOURCEFILE,
} cobolKind;

typedef enum {
	COBOL_SOURCEFILE_COPIED,
} cobolSourcefileRole;

static roleDefinition CobolSourcefileRoles [] = {
	{ true, "copied", "copied in source file" },
};

static kindDefinition CobolKinds[] = {
	{ true, 'f', "fd", "file descriptions (FD, SD, RD)" },
	{ true, 'g', "group", "group items" },
	{ true, 'P', "program", "program ids" },
	{ true, 's', "section", "sections" },
	{ true, 'D', "division", "divisions" },
	{ true, 'p', "paragraph", "paragraphs" },
	{ true, 'd', "data", "data items"      },
	{ true, 'S', "sourcefile", "source code file",
	  .referenceOnly = true, ATTACH_ROLES(CobolSourcefileRoles)},
};

static langType Lang_cobol;
static bool AtLineStart = true;

typedef enum {
	TOKEN_UNDEFINED = 256,
	TOKEN_EOF,
	TOKEN_WORD,
	TOKEN_NUMBER,
	TOKEN_KEYWORD,
	TOKEN_LITERAL,
	TOKEN_PICTURE
} tokenType;

enum {
	KEYWORD_FD,
	KEYWORD_SD,
	KEYWORD_RD,
	KEYWORD_SECTION,
	KEYWORD_DIVISION,
	KEYWORD_CONTINUE,
	KEYWORD_END_EXEC,
	KEYWORD_FILLER,
	KEYWORD_BLANK,
	KEYWORD_OCCURS,
	KEYWORD_IS,
	KEYWORD_JUST,
	KEYWORD_PIC,
	KEYWORD_REDEFINES,
	KEYWORD_RENAMES,
	KEYWORD_SIGN,
	KEYWORD_SYNC,
	KEYWORD_USAGE,
	KEYWORD_VALUE,
	KEYWORD_PROGRAM_ID,
	KEYWORD_EXIT,
	KEYWORD_COPY,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

static const keywordTable cobolKeywordTable[] = {
#define DEFINE_KEYWORD(n) { #n, KEYWORD_##n }
	DEFINE_KEYWORD (FD),
	DEFINE_KEYWORD (SD),
	DEFINE_KEYWORD (RD),
	DEFINE_KEYWORD (SECTION),
	DEFINE_KEYWORD (DIVISION),
	DEFINE_KEYWORD (CONTINUE),
	{ "END-EXEC", KEYWORD_END_EXEC },
	DEFINE_KEYWORD (EXIT),
	DEFINE_KEYWORD (FILLER),
	DEFINE_KEYWORD (BLANK),
	DEFINE_KEYWORD (OCCURS),
	DEFINE_KEYWORD (IS),
	DEFINE_KEYWORD (JUST),
	DEFINE_KEYWORD (PIC),
	DEFINE_KEYWORD (REDEFINES),
	//~ { "REDEFINE", KEYWORD_REDEFINES },
	DEFINE_KEYWORD (RENAMES),
	//~ { "RENAME", KEYWORD_RENAMES },
	DEFINE_KEYWORD (SIGN),
	DEFINE_KEYWORD (SYNC),
	DEFINE_KEYWORD (USAGE),
	DEFINE_KEYWORD (VALUE),
	{ "PROGRAM-ID", KEYWORD_PROGRAM_ID },
	DEFINE_KEYWORD (COPY),
};

typedef struct {
	int				type;
	keywordId		keyword;
	vString *		string;
	unsigned long 	lineNumber;
	MIOPos			filePosition;
} tokenInfo;

#define isIdentifierChar(c) (isalnum(c) || (c) == '-')

static void readToken (tokenInfo *const token)
{
	int c;
	int i;

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	i = AtLineStart ? 0 : 80;
	do
	{
		c = getcFromInputFile ();
		if (i == 6 && (c == '*' || c == '/'))
		{
			do
				c = getcFromInputFile ();
			while (c != EOF && c != '\r' && c != '\n');
		}
		if (c == '\r' || c == '\n')
		{
			AtLineStart = true;
			i = 0;
		}
		else
			i++;
	}
	while ((AtLineStart && i < 7 && c != EOF) || isspace(c));
	AtLineStart = false;

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
		case EOF:
			token->type = TOKEN_EOF;
			break;

		case '\'':
		case '"':
		{
			const int q = c;
			int d = getcFromInputFile ();
			token->type = TOKEN_LITERAL;
			vStringPut (token->string, c);
			while (d != EOF && (d != q || c == q))
			{
				vStringPut (token->string, d);
				c = d;
				d = getcFromInputFile ();
			}
			vStringPut (token->string, d);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			break;
		}

		case '*': /* maybe comment start */
		{
			int d = getcFromInputFile ();
			if (d != '>')
			{
				ungetcToInputFile (d);
				vStringPut (token->string, c);
				token->type = c;
			}
			else
			{
				d = getcFromInputFile ();
				do
				{
					d = getcFromInputFile ();
				}
				while (d != EOF && d != '\r' && d != '\n');
				if (d != EOF) /* let the newline magic happen */
					ungetcToInputFile (d);
				goto getNextChar;
			}
			break;
		}

		case '-':
		case '+':
		{
			int d = getcFromInputFile ();
			vStringPut (token->string, c);
			if (isdigit (d))
			{
				c = d;
				goto readNumber;
			}
			else
			{
				ungetcToInputFile (d);
				token->type = c;
			}
			break;
		}

		default:
			if (isdigit (c) || c == '.')
			{
				bool seenPeriod;
			readNumber:
				if (c == '.')
				{
					int d = getcFromInputFile ();
					ungetcToInputFile (d);
					if (! isdigit (d))
					{
						vStringPut (token->string, c);
						token->type = c;
						break;
					}
					seenPeriod = true;
				}
				do
				{
					vStringPut (token->string, c);
					c = getcFromInputFile ();
					if (c == '.')
					{
						if (seenPeriod)
							break;
						else
						{
							int d = getcFromInputFile ();
							ungetcToInputFile (d);
							if (! isdigit (d))
								break;
							seenPeriod = true;
						}
					}
				}
				while (isdigit(c) || c == '.');
				if (c != EOF)
					ungetcToInputFile (c);
				token->type = TOKEN_NUMBER;
			}
			else if (! isIdentifierChar (c))
			{
				vStringPut (token->string, c);
				token->type = c;
			}
			else
			{
				do
				{
					vStringPut (token->string, c);
					c = getcFromInputFile ();
				}
				while (isIdentifierChar (c));
				if (c != EOF)
					ungetcToInputFile (c);
				token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_cobol);
				if (token->keyword == KEYWORD_NONE)
					token->type = TOKEN_WORD;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}
}

static int makeCOBOLTag (const tokenInfo *const token, const cobolKind kind)
{
	if (CobolKinds[kind].enabled)
		return makeSimpleTag (token->string, kind);
	else
		return CORK_NIL;
}

static void clearToken (tokenInfo *token)
{
	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
}

#define CBL_NL(nl) (*((int *) (nestingLevelGetUserData (nl))))

static NestingLevel *popNestingLevelsToLevelNumber (NestingLevels *levels, const int levelNumber)
{
	NestingLevel *nl;

	while (true)
	{
		nl = nestingLevelsGetCurrent (levels);
		if (! nl || CBL_NL (nl) < levelNumber)
			break;
		nestingLevelsPop (levels);
	}

	return nl;
}

static void findCOBOLTags (void)
{
	tokenInfo tokens[2];
	unsigned int curToken = 0;
	tokenInfo *token = NULL;
	const tokenInfo *prevToken = NULL;
	tokenInfo *sentenceStart = NULL;
	NestingLevels *levels;

	for (curToken = 0; curToken < ARRAY_SIZE (tokens); curToken++)
	{
		tokens[curToken].string = vStringNew ();
		clearToken (&tokens[curToken]);
	}

	levels = nestingLevelsNew (sizeof (int));

#define readToken() \
	do {																	\
		curToken ++;														\
		if (curToken >= ARRAY_SIZE (tokens)) curToken = 0;					\
		prevToken = token;													\
		token = &tokens[curToken];											\
		readToken (token);													\
		if (! prevToken || prevToken->type == '.')							\
			sentenceStart = token;											\
		else if (sentenceStart != prevToken)								\
			sentenceStart = NULL;											\
	} while (0)

	AtLineStart = true;

	do
	{
		readToken ();
		fprintf(stderr, "token(%d) = %s\n", token->type, vStringValue (token->string));

		if (token->keyword == KEYWORD_DIVISION)
		{
			if (prevToken && prevToken->type == TOKEN_WORD)
				makeCOBOLTag (prevToken, K_DIVISION);
		}
		else if (token->keyword == KEYWORD_SECTION)
		{
			if (prevToken && prevToken->type == TOKEN_WORD)
				makeCOBOLTag (prevToken, K_SECTION);
		}
		else if (token->keyword == KEYWORD_PROGRAM_ID)
		{
			readToken ();
			if (token->type == '.')
			{
				/* this is a hack to prevent the period in
				 * "PROGRAM-ID. program-name" from being considered as a
				 * sentence separator.  Yeah it's ugly. */
				token->type = TOKEN_UNDEFINED;

				readToken ();
			}
			if (token->type == TOKEN_WORD)
				makeCOBOLTag (token, K_PROGRAM);
		}
		else if (token->keyword == KEYWORD_FD ||
		         token->keyword == KEYWORD_SD ||
		         token->keyword == KEYWORD_RD)
		{
			readToken ();
			if (token->type == TOKEN_WORD)
				makeCOBOLTag (token, K_FILE);
		}
		else if (token->keyword == KEYWORD_COPY)
		{
			readToken ();
			if (token->type == TOKEN_WORD ||
			    token->type == TOKEN_NUMBER ||
			    token->type == TOKEN_LITERAL)
			{
				makeSimpleRefTag (token->string, K_SOURCEFILE, COBOL_SOURCEFILE_COPIED);
			}
		}
		/* paragraph-name "." */
		else if (token->type == '.' && prevToken == sentenceStart && prevToken->type == TOKEN_WORD)
		{
			makeCOBOLTag (prevToken, K_PARAGRAPH);
		}
		else if (token == sentenceStart && token->type == TOKEN_NUMBER)
		{
			int thisLevelNumber = strtol (vStringValue (token->string), NULL, 10);

			readToken ();
			if (token->type == TOKEN_WORD)
			{
				int kind = KIND_GHOST_INDEX;

				readToken ();
				if (token->type == '.')
					kind = K_GROUP;
				else if (token->type == TOKEN_KEYWORD)
				{
					switch (token->keyword)
					{
						case KEYWORD_BLANK:
						case KEYWORD_OCCURS:
						case KEYWORD_IS:
						case KEYWORD_JUST:
						case KEYWORD_PIC:
						case KEYWORD_REDEFINES:
						case KEYWORD_RENAMES:
						case KEYWORD_SIGN:
						case KEYWORD_SYNC:
						case KEYWORD_USAGE:
						case KEYWORD_VALUE:
							kind = K_DATA;
							break;
					}
				}

				if (kind != KIND_GHOST_INDEX)
				{
					NestingLevel *nl;
					tagEntryInfo entry;
					int r;

					/* FIXME: handle level 77 (standalone) specifically */
					nl = popNestingLevelsToLevelNumber (levels, thisLevelNumber);
					initTagEntry (&entry, vStringValue (prevToken->string), kind);
					if (nl && CBL_NL (nl) < thisLevelNumber)
						entry.extensionFields.scopeIndex = nl->corkIndex;
					r = makeTagEntry (&entry);
					nl = nestingLevelsPush (levels, r);
					CBL_NL (nl) = thisLevelNumber;

					while (token->type != TOKEN_EOF && token->type != '.')
						readToken ();
				}
			}
		}
	}
	while (token->type != TOKEN_EOF);

	nestingLevelsFree (levels);

	for (curToken = 0; curToken < ARRAY_SIZE (tokens); curToken++)
		vStringDelete (tokens[curToken].string);
}

static void initializeCobolParser (langType language)
{
	Lang_cobol = language;
}

extern parserDefinition* CobolParser (void)
{
	static const char *const extensions [] = {
			"cbl", "cob", "CBL", "COB", NULL };
	parserDefinition* def = parserNew ("Cobol");
	def->extensions = extensions;
	def->initialize = initializeCobolParser;
	def->parser = findCOBOLTags;
	def->kindTable = CobolKinds;
	def->kindCount = ARRAY_SIZE(CobolKinds);
	def->keywordTable = cobolKeywordTable;
	def->keywordCount = ARRAY_SIZE(cobolKeywordTable);
	def->useCork = CORK_QUEUE;
	return def;
}
