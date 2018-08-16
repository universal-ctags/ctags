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
#include "keyword.h"
#include "parse.h"
#include "routines.h"

/*
 * ANS85 can have letters in the first column
 */
#define COBOL_REGEX_PREFIX_FIXED "[0-9a-Z][0-9a-Z][0-9a-Z][0-9a-Z][0-9a-Z][0-9a-Z][ \t]+"

/*
 * Sourceformat free/variable just needs the line to start with optional spaces/tabs
*/
#define COBOL_REGEX_PREFIX_RELAXED "[ \t]+"

typedef enum {
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
	{ true, 'p', "paragraph", "paragraphs" },
	{ true, 'd', "data", "data items"      },
	{ true, 'S', "sourcefile", "source code file",
	  .referenceOnly = true, ATTACH_ROLES(CobolSourcefileRoles)},
};

static tagRegexTable cobolTagRegexTable[] = {
	{ "......\\*.*", "", "", "{exclusive}" },
	{ COBOL_REGEX_PREFIX_FIXED
	  "[FSR]D[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "f,fd,file descriptions (FD, SD, RD)", "i"},
	{ COBOL_REGEX_PREFIX_FIXED
	  "[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "g,group,group items", "i"},
	{ COBOL_REGEX_PREFIX_FIXED
	  "PROGRAM-ID\\.[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "P,program,program ids", "i"},
	{ COBOL_REGEX_PREFIX_FIXED
	  "([A-Z0-9][A-Z0-9-]*)[ \t]+SECTION\\.", "\\1",
	 "s,section,sections", "i"},
	{ COBOL_REGEX_PREFIX_FIXED
	  "([A-Z0-9][A-Z0-9-]*)[ \t]+DIVISION\\.", "\\1",
	  "D,division,divisions", "i"},
	{ COBOL_REGEX_PREFIX_RELAXED
	"[FSR]D[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	"f,fd,file descriptions (FD, SD, RD)", "i" },
	{ COBOL_REGEX_PREFIX_RELAXED
	"[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	"g,group,group items", "i" },
	{ COBOL_REGEX_PREFIX_RELAXED
	"PROGRAM-ID\\.[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	"P,program,program ids", "i" },
	{ COBOL_REGEX_PREFIX_RELAXED
	"([A-Z0-9][A-Z0-9-]*)[ \t]+SECTION\\.", "\\1",
	"s,section,sections", "i" },
	{ COBOL_REGEX_PREFIX_RELAXED
	"([A-Z0-9][A-Z0-9-]*)[ \t]+DIVISION\\.", "\\1",
	"D,division,divisions", "i" },
};

typedef enum {
	K,
} cobolKeyword;

static const keywordTable cobolKeywordTable[] = {
	{ "ACCEPT", K },{ "ACCESS", K },{ "ADD", K },{ "ADDRESS", K },{ "ADVANCING", K },{ "AFTER", K },
{ "ALL", K },{ "ALPHABET", K },{ "ALPHABETIC", K },{ "ALPHABETIC-LOWER", K },{ "ALPHABETIC-UPPER", K },
{ "ALPHANUMERIC", K },{ "ALPHANUMERIC-EDITED", K },{ "ALSO", K },{ "ALTER", K },{ "ALTERNATE", K },
{ "AND", K },{ "ANY", K },{ "APPLY", K },{ "ARE", K },{ "AREA", K },{ "AREAS", K },{ "ASCENDING", K },
{ "ASSIGN", K },{ "AT", K },{ "AUTHOR", K },{ "BASIS", K },{ "BEFORE", K },{ "BEGINNING", K },{ "BINARY", K },
{ "BLANK", K },{ "BLOCK", K },{ "BOTTOM", K },{ "BY", K },{ "CALL", K },{ "CANCEL", K },{ "CBL", K },
{ "CD", K },{ "CF", K },{ "CH", K },{ "CHARACTER", K },{ "CHARACTERS", K },{ "CLASS", K },{ "CLASS-ID", K },
{ "CLOCK-UNITS", K },{ "CLOSE", K },{ "COBOL", K },{ "CODE", K },{ "CODE-SET", K },{ "COLLATING", K },
{ "COLUMN", K },{ "COM-REG", K },{ "COMMA", K },{ "COMMON", K },{ "COMMUNICATION", K },{ "COMP", K },
{ "COMP-1", K },{ "COMP-2", K },{ "COMP-3", K },{ "COMP-4", K },{ "COMP-5", K },{ "COMPUTATIONAL", K },
{ "COMPUTATIONAL-1", K },{ "COMPUTATIONAL-2", K },{ "COMPUTATIONAL-3", K },{ "COMPUTATIONAL-4", K },
{ "COMPUTATIONAL-5", K },{ "COMPUTE", K },{ "CONFIGURATION", K },{ "CONTAINS", K },{ "CONTENT", K },
{ "CONTINUE", K },{ "CONTROL", K },{ "CONTROLS", K },{ "CONVERTING", K },{ "COPY", K },{ "CORR", K },
{ "CORRESPONDING", K },{ "COUNT", K },{ "CURRENCY", K },{ "DATA", K },{ "DATE-COMPILED", K },
{ "DATE-WRITTEN", K },{ "DAY", K },{ "DAY-OF-WEEK", K },{ "DBCS", K },{ "DE", K },{ "DEBUG-CONTENTS", K },
{ "DEBUG-ITEM", K },{ "DEBUG-LINE", K },{ "DEBUG-NAME", K },{ "DEBUG-SUB-1", K },{ "DEBUG-SUB-2", K },
{ "DEBUG-SUB-3", K },{ "DEBUGGING", K },{ "DECIMAL-POINT", K },{ "DECLARATIVES", K },{ "DELETE", K },
{ "DELIMITED", K },{ "DELIMITER", K },{ "DEPENDING", K },{ "DESCENDING", K },{ "DESTINATION", K },
{ "DETAIL", K },{ "DISPLAY", K },{ "DISPLAY-1", K },{ "DIVIDE", K },{ "DIVISION", K },{ "DOWN", K },
{ "DUPLICATES", K },{ "DYNAMIC", K },{ "EGCS", K },{ "EGI", K },{ "EJECT", K },{ "ELSE", K },{ "EMI", K },
{ "ENABLE", K },{ "END", K },{ "END-ADD", K },{ "END-CALL", K },{ "END-COMPUTE", K },{ "END-DELETE", K },
{ "END-DIVIDE", K },{ "END-EVALUATE", K },{ "END-IF", K },{ "END-INVOKE", K },{ "END-MULTIPLY", K },
{ "END-OF-PAGE", K },{ "END-PERFORM", K },{ "END-READ", K },{ "END-RECEIVE", K },{ "END-RETURN", K },
{ "END-REWRITE", K },{ "END-SEARCH", K },{ "END-START", K },{ "END-STRING", K },{ "END-SUBTRACT", K },
{ "END-UNSTRING", K },{ "END-WRITE", K },{ "ENDING", K },{ "ENTER", K },{ "ENTRY", K },{ "ENVIRONMENT", K },
{ "EOP", K },{ "EQUAL", K },{ "ERROR", K },{ "ESI", K },{ "EVALUATE", K },{ "EVERY", K },{ "EXCEPTION", K },
{ "EXIT", K },{ "EXTEND", K },{ "EXTERNAL", K },{ "FALSE", K },{ "FD", K },{ "FILE", K },{ "FILE-CONTROL", K },
{ "FILLER", K },{ "FINAL", K },{ "FIRST", K },{ "FOOTING", K },{ "FOR", K },{ "FROM", K },{ "FUNCTION", K },
{ "GENERATE", K },{ "GIVING", K },{ "GLOBAL", K },{ "GO", K },{ "GOBACK", K },{ "GREATER", K },{ "GROUP", K },
{ "HEADING", K },{ "HIGH-VALUE", K },{ "HIGH-VALUES", K },{ "I-O", K },{ "I-O-CONTROL", K },{ "ID", K },
{ "IDENTIFICATION", K },{ "IF", K },{ "IN", K },{ "INDEX", K },{ "INDEXED", K },{ "INDICATE", K },
{ "INHERITS", K },{ "INITIAL", K },{ "INITIALIZE", K },{ "INITIATE", K },{ "INPUT", K },{ "INPUT-OUTPUT", K },
{ "INSERT", K },{ "INSPECT", K },{ "INSTALLATION", K },{ "INTO", K },{ "INVALID", K },{ "INVOKE", K },
{ "IS", K },{ "JUST", K },{ "JUSTIFIED", K },{ "KANJI", K },{ "KEY", K },{ "LABEL", K },{ "LAST", K },
{ "LEADING", K },{ "LEFT", K },{ "LENGTH", K },{ "LESS", K },{ "LIMIT", K },{ "LIMITS", K },
{ "LINAGE", K },{ "LINAGE-COUNTER", K },{ "LINE", K },{ "LINE-COUNTER", K },{ "LINES", K },
{ "LINKAGE", K },{ "LOCAL-STORAGE", K },{ "LOCK", K },{ "LOW-VALUE", K },{ "LOW-VALUES", K },
{ "MEMORY", K },{ "MERGE", K },{ "MESSAGE", K },{ "METACLASS", K },{ "METHOD", K },{ "METHOD-ID", K },
{ "MODE", K },{ "MODULES", K },{ "MORE-LABELS", K },{ "MOVE", K },{ "MULTIPLE", K },{ "MULTIPLY", K },
{ "NATIVE", K },{ "NATIVE_BINARY", K },{ "NEGATIVE", K },{ "NEXT", K },{ "NO", K },{ "NOT", K },
{ "NULL", K },{ "NULLS", K },{ "NUMBER", K },{ "NUMERIC", K },{ "NUMERIC-EDITED", K },{ "OBJECT", K },
{ "OBJECT-COMPUTER", K },{ "OCCURS", K },{ "OF", K },{ "OFF", K },{ "OMITTED", K },
{ "ON", K },{ "OPEN", K },{ "OPTIONAL", K },{ "OR", K },{ "ORDER", K },
{ "ORGANIZATION", K },{ "OTHER", K },{ "OUTPUT", K },{ "OVERFLOW", K },{ "OVERRIDE", K },
{ "PACKED-DECIMAL", K },{ "PADDING", K },{ "PAGE", K },{ "PAGE-COUNTER", K },{ "PASSWORD", K },
{ "PERFORM", K },{ "PF", K },{ "PH", K },{ "PIC", K },{ "PICTURE", K },
{ "PLUS", K },{ "POINTER", K },{ "POSITION", K },{ "POSITIVE", K },{ "PRINTING", K },
{ "PROCEDURE", K },{ "PROCEDURE-POINTER", K },{ "PROCEDURES", K },{ "PROCEED", K },{ "PROCESSING", K },
{ "PROGRAM", K },{ "PROGRAM-ID", K },{ "PURGE", K },{ "QUEUE", K },{ "QUOTE", K },
{ "QUOTES", K },{ "RANDOM", K },{ "RD", K },{ "READ", K },{ "READY", K },
{ "RECEIVE", K },{ "RECORD", K },{ "RECORDING", K },{ "RECORDS", K },{ "RECURSIVE", K },
{ "REDEFINES", K },{ "REEL", K },{ "REFERENCE", K },{ "REFERENCES", K },{ "RELATIVE", K },
{ "RELEASE", K },{ "RELOAD", K },{ "REMAINDER", K },{ "REMOVAL", K },{ "RENAMES", K },
{ "REPLACE", K },{ "REPLACING", K },{ "REPORT", K },{ "REPORTING", K },{ "REPORTS", K },
{ "REPOSITORY", K },{ "RERUN", K },{ "RESERVE", K },{ "RESET", K },{ "RETURN", K },
{ "RETURN-CODE", K },{ "RETURNING", K },{ "REVERSED", K },{ "REWIND", K },{ "REWRITE", K },
{ "RF", K },{ "RH", K },{ "RIGHT", K },{ "ROUNDED", K },{ "RUN", K },
{ "SAME", K },{ "SD", K },{ "SEARCH", K },{ "SECTION", K },{ "SECURITY", K },
{ "SEGMENT", K },{ "SEGMENT-LIMIT", K },{ "SELECT", K },{ "SELF", K },{ "SEND", K },
{ "SENTENCE", K },{ "SEPARATE", K },{ "SEQUENCE", K },{ "SEQUENTIAL", K },{ "SERVICE", K },
{ "SET", K },{ "SHIFT-IN", K },{ "SHIFT-OUT", K },{ "SIGN", K },{ "SIZE", K },
{ "SKIP1", K },{ "SKIP2", K },{ "SKIP3", K },{ "SORT", K },{ "SORT-CONTROL", K },
{ "SORT-CORE-SIZE", K },{ "SORT-FILE-SIZE", K },{ "SORT-MERGE", K },{ "SORT-MESSAGE", K },{ "SORT-MODE-SIZE", K },
{ "SORT-RETURN", K },{ "SOURCE", K },{ "SOURCE-COMPUTER", K },{ "SPACE", K },{ "SPACES", K },
{ "SPECIAL-NAMES", K },{ "STANDARD", K },{ "STANDARD-1", K },{ "STANDARD-2", K },{ "START", K },
{ "STATUS", K },{ "STOP", K },{ "STRING", K },{ "SUB-QUEUE-1", K },{ "SUB-QUEUE-2", K },
{ "SUB-QUEUE-3", K },{ "SUBTRACT", K },{ "SUM", K },{ "SUPER", K },{ "SUPPRESS", K },
{ "SYMBOLIC", K },{ "SYNC", K },{ "SYNCHRONIZED", K },{ "TABLE", K },{ "TALLY", K },
{ "TALLYING", K },{ "TAPE", K },{ "TERMINAL", K },{ "TERMINATE", K },{ "TEST", K },
{ "TEXT", K },{ "THAN", K },{ "THEN", K },{ "THROUGH", K },{ "THRU", K },
{ "TIME", K },{ "TIMES", K },{ "TITLE", K },{ "TO", K },{ "TOP", K },
{ "TRACE", K },{ "TRAILING", K },{ "TRUE", K },{ "TYPE", K },{ "UNIT", K },
{ "UNSTRING", K },{ "UNTIL", K },{ "UP", K },{ "UPON", K },{ "USAGE", K },
{ "USE", K },{ "USING", K },{ "VALUE", K },{ "VALUES", K },{ "VARYING", K },
{ "WHEN", K },{ "WHEN-COMPILED", K },{ "WITH", K },{ "WORDS", K },{ "WORKING-STORAGE", K },
{ "WRITE", K },{ "WRITE-ONLY", K },{ "ZERO", K },{ "ZEROES", K },{ "ZEROS", K }
	};

/*
*   FUNCTION DEFINITIONS
*/

static void cobol_make_tag_maybe (const char *line,
								  const regexMatch *matches,
								  unsigned int count,
								  langType cobol,
								  int matchIndex,
								  int kindIndex)
{
	if (count > 0)
	{
		vString *name = vStringNew ();

		vStringNCopyS (name, line + matches[matchIndex].start, matches[matchIndex].length);
		if (lookupCaseKeyword (vStringValue (name), cobol) == KEYWORD_NONE)
			makeSimpleTag (name, kindIndex);
		vStringDelete (name);
	}
}

static bool make_tag_for_data_maybe (const char *line,
									 const regexMatch *matches,
									 unsigned int count,
									 void *data)
{
	cobol_make_tag_maybe (line, matches, count, *(langType *)data, 1, K_DATA);
	return true;
}

static bool make_tag_for_paragraph_maybe (const char *line,
										  const regexMatch *matches,
										  unsigned int count,
										  void *data)
{
	cobol_make_tag_maybe (line, matches, count, *(langType *)data, 1, K_PARAGRAPH);
	return true;
}

static bool make_tag_for_copied_in_sourcefile (const char *line,
											   const regexMatch *matches,
											   unsigned int count,
											   void *data CTAGS_ATTR_UNUSED)
{
	if (count > 0)
	{
		vString *name = vStringNew ();

		vStringNCopyS (name, line + matches[1].start, matches[1].length);
		makeSimpleRefTag (name, K_SOURCEFILE, COBOL_SOURCEFILE_COPIED);
		vStringDelete (name);
	}
	return true;
}

static void initializeCobolParser (langType language)
{
	static langType cobol;

	cobol = language;

	addLanguageCallbackRegex (cobol,
		COBOL_REGEX_PREFIX_FIXED
		"[0-9]+[ \t]+([A-Z0-9][A-Z0-9_-]*)[ \t]+("
		"BLANK|OCCURS|IS|JUST|PIC|REDEFINES|RENAMES|SIGN|SYNC|USAGE|VALUE|COMP\\-1|COMP\\-2|COMP\\-3|COMP\\-X|COMP"
		")",
		"{icase}",
		make_tag_for_data_maybe, NULL, &cobol);

	addLanguageCallbackRegex(cobol,
		COBOL_REGEX_PREFIX_RELAXED
		"[0-9]+[ \t]+([A-Z0-9][A-Z0-9_-]*)[ \t]+("
		"BLANK|OCCURS|IS|JUST|PIC|REDEFINES|RENAMES|SIGN|SYNC|USAGE|VALUE|COMP\\-1|COMP\\-2|COMP\\-3|COMP\\-X|COMP"
		")",
		"{icase}",
		make_tag_for_data_maybe, NULL, &cobol);

	addLanguageCallbackRegex (cobol,
		COBOL_REGEX_PREFIX_FIXED
		"([A-Z0-9][A-Z0-9-]*)\\.",
		"{icase}",
		make_tag_for_paragraph_maybe, NULL, &cobol);

	addLanguageCallbackRegex(cobol,
		COBOL_REGEX_PREFIX_RELAXED
		"([A-Z0-9][A-Z0-9-]*)\\.",
		"{icase}",
		make_tag_for_paragraph_maybe, NULL, &cobol);

	addLanguageCallbackRegex (cobol,
		"^[ \t]*COPY[ \t]+([A-Z0-9][A-Z0-9-]*)\\.",
		"{icase}",
		make_tag_for_copied_in_sourcefile, NULL, NULL);
}

extern parserDefinition* CobolParser (void)
{
	static const char *const extensions [] = {
			"cbl", "cob", "CBL", "COB", NULL };
	parserDefinition* def = parserNew ("Cobol");
	def->extensions = extensions;
	def->initialize = initializeCobolParser;
	def->tagRegexTable = cobolTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (cobolTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->kindTable = CobolKinds;
	def->kindCount = ARRAY_SIZE(CobolKinds);
	def->keywordTable = cobolKeywordTable;
	def->keywordCount = ARRAY_SIZE(cobolKeywordTable);
	return def;
}
