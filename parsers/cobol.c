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

#define COBOL_REGEX_PREFIX "^.......[ \t]*"

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
	{ COBOL_REGEX_PREFIX
	  "[FSR]D[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "f,fd,file descriptions (FD, SD, RD)", "i"},
	{ COBOL_REGEX_PREFIX
	  "[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "g,group,group items", "i"},
	{ COBOL_REGEX_PREFIX
	  "PROGRAM-ID\\.[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "P,program,program ids", "i"},
	{ COBOL_REGEX_PREFIX
	  "([A-Z0-9][A-Z0-9-]*)[ \t]+SECTION\\.", "\\1",
	 "s,section,sections", "i"},
	{ COBOL_REGEX_PREFIX
	  "([A-Z0-9][A-Z0-9-]*)[ \t]+DIVISION\\.", "\\1",
	  "D,division,divisions", "i"},
};

typedef enum {
	K,
} cobolKeyword;

static const keywordTable cobolKeywordTable[] = {
	{ "CONTINUE", K },
	{ "END-EXEC", K },
	{ "EXIT", K },
	{ "FILLER", K },
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
					  COBOL_REGEX_PREFIX
					  "[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)[ \t]+("
					  "BLANK|OCCURS|IS|JUST|PIC|REDEFINES|RENAMES|SIGN|SYNC|USAGE|VALUE"
					  ")",
					  "{icase}",
					  make_tag_for_data_maybe, NULL, &cobol);
	addLanguageCallbackRegex (cobol,
					  COBOL_REGEX_PREFIX
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
