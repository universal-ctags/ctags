/*
*   Copyright (c) 2009, Eric Forgeot
*   Copyright (c) 2014, Colomban Wendling <colomban@geany.org>
*
*   Based on work by Jon Strait
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your opinion) any later version.
*
*   This module contains functions for generating tags for Txt2tags files
*   (https://en.wikipedia.org/wiki/Txt2tags).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "parse.h"
#include "read.h"
#include "nestlevel.h"
#include "vstring.h"
#include "routines.h"
#include "entry.h"


#define SCOPE_SEPARATOR "\"\""

/*
*   DATA DEFINITIONS
*/

typedef enum {
	K_SECTION = 0
} Txt2tagsKind;

static scopeSeparator Txt2TagsSeparators [] = {
	{ KIND_WILDCARD_INDEX, SCOPE_SEPARATOR }
};

static kindDefinition Txt2tagsKinds[] = {
	{ true, 's', "section", "sections",
	  ATTACH_SEPARATORS(Txt2TagsSeparators) },
};

struct nestingLevelUserData {
	int indentation;
};
#define NL_INDENTATION(nl) ((struct nestingLevelUserData *)nestingLevelGetUserData(nl))->indentation

/*
*   FUNCTION DEFINITIONS
*/

static int makeTxt2tagsTag (const vString* const name,
                            const NestingLevels *const nls,
                            Txt2tagsKind type)
{
	tagEntryInfo e;
	NestingLevel *nl;
	initTagEntry (&e, vStringValue(name), type);

	nl = nestingLevelsGetCurrent (nls);
	if (nl)
		e.extensionFields.scopeIndex = nl->corkIndex;

	return makeTagEntry(&e);
}

/* matches: ^ *[=_-]{20,} *$ */
static bool isTxt2tagsLine (const unsigned char *line)
{
	unsigned int len;

	while (isspace(*line)) line++;
	for (len = 0; *line == '=' || *line == '-' || *line == '_'; len++)
		line++;
	while (isspace(*line)) line++;

	return len >= 20 && *line == 0;
}

static bool parseTxt2tagsTitle (const unsigned char *line,
                                vString *const title,
                                int *const depth_)
{
	const int MAX_TITLE_DEPTH = 5; /* maximum length of a title delimiter */
	unsigned char delim;
	int delim_delta = 0;
	const unsigned char *end;

	/* skip leading spaces, but no tabs (probably because they create quotes) */
	while (*line == ' ') line++;

	/* normal/numbered titles */
	if (*line != '=' && *line != '+')
		return false;

	delim = *line;

	/* find the start delimiter length */
	while (*line == delim && delim_delta < MAX_TITLE_DEPTH+1)
	{
		line++;
		delim_delta++;
	}
	while (isspace(*line))
		line++;

	if (delim_delta > MAX_TITLE_DEPTH) /* invalid */
		return false;

	*depth_ = delim_delta;

	/* find the end delimiter */
	end = line + strlen((const char *) line) - 1;
	while (end > line && isspace(*end)) end--;
	/* skip a possible label: \[[A-Za-z0-9_-]+\] */
	if (*end == ']')
	{
		end--;
		while (end > line && (isalnum(*end) || *end == '_' || *end == '-'))
			end--;
		if (*end != '[') /* invalid */
			return false;
		end--;
	}
	while (end > line && *end == delim && delim_delta >= 0)
	{
		delim_delta--;
		end--;
	}
	while (end > line && isspace(*end)) end--;
	end++;

	/* if start and end delimiters are not identical, or the the name is empty */
	if (delim_delta != 0 || (end - line) <= 0)
		return false;

	vStringNCopyS(title, (const char *) line, end - line);
	return true;
}

static void findTxt2tagsTags (void)
{
	NestingLevels *nls = nestingLevelsNew(sizeof(struct nestingLevelUserData));
	vString *name = vStringNew();
	const unsigned char *line;

	while ((line = readLineFromInputFile()) != NULL)
	{
		int depth;

		if (isTxt2tagsLine(line))
			; /* skip not to improperly match titles */
		else if (parseTxt2tagsTitle(line, name, &depth))
		{
			NestingLevel *nl = nestingLevelsGetCurrent(nls);
			int r;

			while (nl && NL_INDENTATION(nl) >= depth)
			{
				nestingLevelsPop(nls);
				nl = nestingLevelsGetCurrent(nls);
			}

			r = makeTxt2tagsTag(name, nls, K_SECTION);
			nestingLevelsPush(nls, r);
			nl = nestingLevelsGetCurrent(nls);
			NL_INDENTATION(nl) = depth;
		}
	}
	vStringDelete (name);
	nestingLevelsFree(nls);
}

extern parserDefinition* Txt2tagsParser (void)
{
	static const char *const patterns [] = { "*.t2t", NULL };
	static const char *const extensions [] = { "t2t", NULL };
	parserDefinition* const def = parserNew ("Txt2tags");

	def->kindTable = Txt2tagsKinds;
	def->kindCount = ARRAY_SIZE (Txt2tagsKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findTxt2tagsTags;
	def->useCork = CORK_QUEUE;
	return def;
}

