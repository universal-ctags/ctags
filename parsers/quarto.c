/*
 *
 *  Copyright (c) 2023, Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 * This module contains functions for generating tags for Quarto files.
 * https://quarto.org/docs/guide/
 * https://quarto.org/docs/reference/
 * https://www.jaysong.net/RBook/quarto.html#sec-quarto-howtouse <Japanese>
 *
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include "markdown.h"

#include "entry.h"
#include "parse.h"
#include "read.h"

#include <ctype.h>
#include <string.h>

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_CHUNK_LABEL = 0,
} quartoKind;

static kindDefinition QuartoKinds[] = {
	{ true, 'l', "chunklabel",       "chunk labels"},
};

struct sQuartoSubparser {
	markdownSubparser markdown;
	int lastChunkLabel;
};

/*
*   FUNCTION DEFINITIONS
*/

static void findQuartoTags (void)
{
	scheduleRunningBaseparser (0);
}

#define skip_space(CP) 	while (*CP == ' ' || *CP == '\t') CP++;

static int makeQuartoTag (vString *name, int kindIndex, bool anonymous)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kindIndex);
	if (anonymous)
		markTagExtraBit (&e, XTAG_ANONYMOUS);
	return makeTagEntry (&e);
}

static bool extractLanguageForCodeBlock (markdownSubparser *s,
										 const char *langMarker,
										 vString *langName)
{
	struct sQuartoSubparser *quarto = (struct sQuartoSubparser *)s;
	const char *cp = langMarker;
	bool unexecutedBlock = false;

	if (*cp != '{')
		return false;
	cp++;

	/* Handle unexecuted blocks like ```{{python}} */
	if (*cp == '{') {
		unexecutedBlock = true;
		cp++;
	}

	const char *end = strpbrk(cp, " \t,}");
	if (!end)
		return false;

	if (end - cp == 0)
		return false;

	vStringNCatS (langName, cp, end - cp);

	if (unexecutedBlock) {
		end = strpbrk(cp, " \t,}");
		if (!end)
		{
			vStringClear (langName);
			return false;
		}
	}

	cp = end;
	if (*cp == ',' || *cp == '}')
	{
		vString *name = anonGenerateNew("__anon", K_CHUNK_LABEL);
		quarto->lastChunkLabel = makeQuartoTag (name,
												K_CHUNK_LABEL,
												true);
		vStringDelete (name);
		return true;
	}

	skip_space(cp);

	vString *chunk_label  = vStringNew ();
	bool anonymous = false;
	while (isalnum((unsigned char)*cp) || *cp == '-')
		vStringPut (chunk_label, *cp++);

	if (vStringLength (chunk_label) == 0)
	{
		anonGenerate (chunk_label, "__anon", K_CHUNK_LABEL);
		anonymous = true;
	}

	skip_space(cp);
	if (*cp == ',' || *cp == '}')
		quarto->lastChunkLabel = makeQuartoTag (chunk_label,
												K_CHUNK_LABEL,
												anonymous);

	vStringDelete (chunk_label);
	return true;
}

static void notifyCodeBlockLine (markdownSubparser *s,
								 const unsigned char *line)
{
	struct sQuartoSubparser *quarto = (struct sQuartoSubparser *)s;

	if (strncmp ((const char *)line, "#| ", 3))
		return;

	line += 3;
	skip_space (line);

	if (strncmp ((const char *)line, "label:", 6))
		return;

	line += 6;
	skip_space (line);

	if (!*line)
		return;

	vString *label = vStringNewInit ((const char *)line);
	vStringStripTrailing (label);
	if (!vStringIsEmpty (label))
	{
		/* If an anonymous tag is made for the label of this code chunk,
		 * it becomes unnecessary; the real one can be made from
		 * "#! label: ..." */
		if (quarto->lastChunkLabel != CORK_NIL)
		{
			tagEntryInfo *e = getEntryInCorkQueue (quarto->lastChunkLabel);
			if (e && isTagExtraBitMarked (e, XTAG_ANONYMOUS))
				markTagAsPlaceholder (e, true);
		}

		quarto->lastChunkLabel = makeQuartoTag (label,
												K_CHUNK_LABEL,
												false);
	}
	vStringDelete (label);
}

static void notifyEndOfCodeBlock (markdownSubparser *s)
{
	struct sQuartoSubparser *quarto = (struct sQuartoSubparser *)s;

	if (quarto->lastChunkLabel == CORK_NIL)
		return;

	tagEntryInfo *e = getEntryInCorkQueue (quarto->lastChunkLabel);
	if (e)
		e->extensionFields.endLine = getInputLineNumber ();

	quarto->lastChunkLabel = CORK_NIL;
}

static void inputStart (subparser *s)
{
	struct sQuartoSubparser *quatro = (struct sQuartoSubparser*)s;

	quatro->lastChunkLabel = CORK_NIL;
}

extern parserDefinition* QuartoParser (void)
{
	static const char *const extensions [] = { "qmd", NULL };
	static struct sQuartoSubparser quartoSubparser = {
		.markdown = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
				.inputStart = inputStart,
			},
			.extractLanguageForCodeBlock = extractLanguageForCodeBlock,
			.notifyCodeBlockLine = notifyCodeBlockLine,
			.notifyEndOfCodeBlock = notifyEndOfCodeBlock,
		},
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Markdown", &quartoSubparser },
	};

	parserDefinition* const def = parserNew ("Quarto");


	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable      = QuartoKinds;
	def->kindCount  = ARRAY_SIZE (QuartoKinds);
	def->extensions = extensions;
	def->parser     = findQuartoTags;
	return def;
}
