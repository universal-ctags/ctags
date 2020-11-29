/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for COBOL language
*   files.
*/

/* Some references:
 * - https://www.cs.vu.nl/grammarware/browsable/cobol/
 * - https://www.cs.vu.nl/grammarware/browsable/vs-cobol-ii/
 * - https://open-cobol.sourceforge.io/guides/grammar.pdf
 * - http://mapage.noos.fr/~bpinon/a_cobol_parser.htm
 * - https://en.wikipedia.org/wiki/COBOL
 */

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */
#include "debug.h"
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
	{ "PICTURE", KEYWORD_PIC },
	DEFINE_KEYWORD (REDEFINES),
	DEFINE_KEYWORD (RENAMES),
	DEFINE_KEYWORD (SIGN),
	DEFINE_KEYWORD (SYNC),
	DEFINE_KEYWORD (USAGE),
	DEFINE_KEYWORD (VALUE),
	{ "VALUES", KEYWORD_VALUE },
	{ "PROGRAM-ID", KEYWORD_PROGRAM_ID },
	DEFINE_KEYWORD (COPY),
};

#define INDICATOR_COLUMN 7
#define PROGRAM_NAME_AREA_COLUMN 73

#define isIdentifierChar(c) (isalnum(c) || (c) == '-')
#define isQuote(c) ((c) == '\'' || (c) == '"')

typedef enum {
	/* Fixed: program starts at column 8, ends at column 72 */
	FORMAT_FIXED	= 0x1,
	/* Free: program starts at column 1, no specific end */
	FORMAT_FREE		= 0x2,
	/* Variable: program starts at column 8, no specific end */
	FORMAT_VARIABLE	= FORMAT_FIXED | FORMAT_FREE
} CobolFormat;

static struct {
	vString *line;
	unsigned long int lineNumber;
	MIOPos filePosition;
	const char *nextLine;
	CobolFormat format;
} CblInputState;

static void cblppInit (const CobolFormat format)
{
	CblInputState.line = vStringNew ();
	CblInputState.lineNumber = 0;
	CblInputState.nextLine = NULL;
	CblInputState.format = format;
}

static void cblppDeinit (void)
{
	vStringDelete (CblInputState.line);
}

static const char *cblppGetColumn (const char *line,
								   const unsigned int column)
{
	unsigned int col = 0;

	for (; *line; line++)
	{
		col += (*line == '\t') ? 8 : 1;
		if (col >= column)
			return line;
	}

	return NULL;
}

static void cblppAppendLine (vString *buffer,
							 const char *line)
{
	if (CblInputState.format & FORMAT_FIXED)
	{
		const char *indicator = cblppGetColumn (line, INDICATOR_COLUMN);

		if (indicator && *indicator && *indicator != '*' && *indicator != '/')
		{
			const char *lineStart = indicator + 1;
			const char *lineEnd = cblppGetColumn (line, PROGRAM_NAME_AREA_COLUMN);

			if (*indicator == '-')
			{
				vStringStripTrailing (buffer);
				while (isspace (*lineStart))
					lineStart++;
			}

			if (CblInputState.format == FORMAT_FIXED)
				vStringNCatS (buffer, lineStart, lineEnd - lineStart);
			else
				vStringCatS (buffer, lineStart);
		}
	}
	else if (line[0] != '*' && line[0] != '/')
		vStringCatS (buffer, line);
}

/* TODO: skip *> comments */
static const char *cblppGetLine (void)
{
	const char *line;

	if (CblInputState.nextLine)
	{
		line = CblInputState.nextLine;
		CblInputState.nextLine = NULL;
	}
	else
		line = (const char *) readLineFromInputFile ();

	CblInputState.lineNumber = getInputLineNumber ();
	CblInputState.filePosition = getInputFilePosition ();

	if (!line)
		return NULL;

	vStringClear (CblInputState.line);
	cblppAppendLine (CblInputState.line, line);

	/* check for continuation lines */
	if (CblInputState.format & FORMAT_FIXED)
	{
		while (true)
		{
			const char *indicator;
			line = (const char *) readLineFromInputFile ();
			if (! line)
				break;
			indicator = cblppGetColumn (line, INDICATOR_COLUMN);
			if (indicator && *indicator == '-')
				cblppAppendLine (CblInputState.line, line);
			else
				break;
		}

		CblInputState.nextLine = line;
	}

	return vStringValue (CblInputState.line);
}

static void initCOBOLRefTagEntry (tagEntryInfo *e, const char *name,
								  const cobolKind kind, const int role)
{
	initRefTagEntry (e, name, kind, role);
	e->lineNumber = CblInputState.lineNumber;
	e->filePosition = CblInputState.filePosition;
}

static void initCOBOLTagEntry (tagEntryInfo *e, const char *name, const cobolKind kind)
{
	initCOBOLRefTagEntry (e, name, kind, ROLE_DEFINITION_INDEX);
}

static int makeCOBOLRefTag (const char *name, const cobolKind kind, const int role)
{
	if (CobolKinds[kind].enabled)
	{
		tagEntryInfo e;

		initCOBOLRefTagEntry (&e, name, kind, role);

		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static int makeCOBOLTag (const char *name, const cobolKind kind)
{
	return makeCOBOLRefTag (name, kind, ROLE_DEFINITION_INDEX);
}

#define CBL_NL(nl) (*((unsigned int *) (nestingLevelGetUserData (nl))))

static NestingLevel *popNestingLevelsToLevelNumber (NestingLevels *levels, const unsigned int levelNumber)
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

static bool isNumeric (const char *nptr, unsigned long int *num)
{
	char *endptr;
	unsigned long int v;

	v = strtoul (nptr, &endptr, 10);
	if (nptr != endptr && *endptr == 0)
	{
		if (num)
			*num = v;
		return true;
	}
	return false;
}

static void findCOBOLTags (const CobolFormat format)
{
	NestingLevels *levels;
	const char *line;

	cblppInit (format);

	levels = nestingLevelsNew (sizeof (unsigned int));

	while ((line = cblppGetLine ()) != NULL)
	{
		char word[64];
		int keyword;
		unsigned long int levelNumber;

#define READ_WHILE(word, cond) \
	do { \
		unsigned int i; \
		for (i = 0; i < (ARRAY_SIZE (word) - 1) && *line && (cond); line++) \
			word[i++] = *line; \
		word[i] = 0; \
	} while (0)
#define READ_LITERAL(word) \
	do { \
		const char READ_LITERAL__q = isQuote (*line) ? *line++ : 0; \
		READ_WHILE (word, (READ_LITERAL__q && READ_LITERAL__q != *line) || \
		                   isIdentifierChar (*line)); \
		if (READ_LITERAL__q && READ_LITERAL__q == *line) \
			line++; \
		keyword = lookupCaseKeyword (word, Lang_cobol); \
	} while (0)
#define READ_WORD(word, keyword) \
	do { \
		READ_WHILE (word, isIdentifierChar (*line)); \
		keyword = lookupCaseKeyword (word, Lang_cobol); \
	} while (0)
#define READ_KEYWORD(keyword) \
	do { \
		char READ_KEYWORD__word[64]; \
		READ_WORD (READ_KEYWORD__word, keyword); \
	} while (0)
#define SKIP_SPACES() do { while (isspace (*line)) line++; } while (0)

		SKIP_SPACES ();
		READ_WORD (word, keyword);
		SKIP_SPACES ();

		switch (keyword)
		{
		case KEYWORD_FD:
		case KEYWORD_SD:
		case KEYWORD_RD:
			READ_WORD (word, keyword);
			SKIP_SPACES ();
			if (*word && *line == '.')
				makeCOBOLTag (word, K_FILE);
			break;

		case KEYWORD_PROGRAM_ID:
			if (*line == '.')
			{
				line++;
				SKIP_SPACES ();
			}
			READ_LITERAL (word);
			if (*word)
				makeCOBOLTag (word, K_PROGRAM);
			break;

		case KEYWORD_COPY:
			READ_WORD (word, keyword); // FIXME: also allow LITERAL
			if (*word)
				makeCOBOLRefTag (word, K_SOURCEFILE, COBOL_SOURCEFILE_COPIED);
			break;

		case KEYWORD_CONTINUE:
		case KEYWORD_END_EXEC:
		case KEYWORD_EXIT:
		case KEYWORD_FILLER:
			/* nothing, just ignore those in following cases */;
			break;

		default:
			if (isNumeric (word, &levelNumber))
			{
				READ_WORD (word, keyword);
				SKIP_SPACES ();

				if (*word && keyword != KEYWORD_FILLER)
				{
					int kind = KIND_GHOST_INDEX;

					if (*line == '.')
						kind = K_GROUP;
					else
					{
						int keyword2;

						READ_KEYWORD (keyword2);
						switch (keyword2)
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
						}
					}

					if (kind != KIND_GHOST_INDEX)
					{
						NestingLevel *nl;
						tagEntryInfo entry;
						int r;
						unsigned int nestingLevelNumber;

						/* for nesting purposes, level 77 is identical to 1,
						 * and 66 to 2 */
						switch (levelNumber)
						{
						default: nestingLevelNumber = levelNumber; break;
						case 77: nestingLevelNumber = 1; break;
						case 66: nestingLevelNumber = 2; break;
						}

						nl = popNestingLevelsToLevelNumber (levels, nestingLevelNumber);
						initCOBOLTagEntry (&entry, word, kind);
						if (nl && CBL_NL (nl) < nestingLevelNumber)
							entry.extensionFields.scopeIndex = nl->corkIndex;
						r = makeTagEntry (&entry);
						if (levelNumber < 50 /* exclude special levels */)
						{
							nl = nestingLevelsPush (levels, r);
							CBL_NL (nl) = levelNumber;
						}
					}
				}
			}
			else if (*word && *line == '.')
				makeCOBOLTag (word, K_PARAGRAPH);
			else
			{
				int keyword2;

				READ_KEYWORD (keyword2);
				SKIP_SPACES ();

				if (keyword2 == KEYWORD_DIVISION && *line == '.')
					makeCOBOLTag (word, K_DIVISION);
				else if (keyword2 == KEYWORD_SECTION && *line == '.')
					makeCOBOLTag (word, K_SECTION);
			}
		}
	}

	nestingLevelsFree (levels);
	cblppDeinit ();
}

static void findCOBOLFixedTags (void)
{
	findCOBOLTags (FORMAT_FIXED);
}

static void findCOBOLFreeTags (void)
{
	findCOBOLTags (FORMAT_FREE);
}

static void findCOBOLVariableTags (void)
{
	findCOBOLTags (FORMAT_VARIABLE);
}

static void initializeCobolParser (langType language)
{
	Lang_cobol = language;
}

static parserDefinition* commonCobolParserDefinition (const char *name,
													  simpleParser parser)
{
	parserDefinition* def = parserNew (name);
	def->initialize = initializeCobolParser;
	def->parser = parser;
	def->kindTable = CobolKinds;
	def->kindCount = ARRAY_SIZE(CobolKinds);
	def->keywordTable = cobolKeywordTable;
	def->keywordCount = ARRAY_SIZE(cobolKeywordTable);
	def->useCork = CORK_QUEUE;
	return def;
}

extern parserDefinition* CobolParser (void)
{
	static const char *const extensions [] = {
			"cbl", "cob", "CBL", "COB", NULL };
	parserDefinition* def = commonCobolParserDefinition ("Cobol",
														 findCOBOLFixedTags);
	def->extensions = extensions;
	return def;
}

extern parserDefinition* CobolFreeParser (void)
{
	return commonCobolParserDefinition ("CobolFree", findCOBOLFreeTags);
}

extern parserDefinition* CobolVariableParser (void)
{
	return commonCobolParserDefinition ("CobolVariable", findCOBOLVariableTags);
}
