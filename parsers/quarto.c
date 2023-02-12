/*
 *
 *  Copyright (c) 2022, Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 * This module contains functions for generating tags for Quarto Markdown files.
 * https://quarto.org/docs/get-started/hello/rstudio.html
 *
 * Maintainer: Anish S. Shah, https://github.com/asshah4
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include "markdown.h"

#include "entry.h"
#include "parse.h"

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
};

/*
*   FUNCTION DEFINITIONS
*/

static void findQuartoTags (void)
{
	scheduleRunningBaseparser (0);
}

#define skip_space(CP) 	while (*CP == ' ' || *CP == '\t') CP++;

static void makeQuartoTag (vString *name, int kindIndex, bool anonymous)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kindIndex);
	if (anonymous)
		markTagExtraBit (&e, XTAG_ANONYMOUS);
	makeTagEntry (&e);
}

static bool extractLanguageForCodeBlock (markdownSubparser *s,
										 const char *langMarker,
										 vString *langName)
{
	const char *cp = langMarker;

	if (*cp != '{')
		return false;
	cp++;

	const char *end = strpbrk(cp, " \t,}");
	if (!end)
		return false;

	if (end - cp == 0)
		return false;

	vStringNCatS (langName, cp, end - cp);

	cp = end;
	if (*cp == ',' || *cp == '}')
	{
		vString *name = anonGenerateNew("__anon", K_CHUNK_LABEL);
		makeQuartoTag (name, K_CHUNK_LABEL, true);
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
		makeQuartoTag (chunk_label, K_CHUNK_LABEL, anonymous);

	vStringDelete (chunk_label);
	return true;
}

extern parserDefinition* QuartoParser (void)
{
	static const char *const extensions [] = { "qmd", NULL };
	static struct sQuartoSubparser quartoSubparser = {
		.markdown = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
			},
			.extractLanguageForCodeBlock = extractLanguageForCodeBlock,
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
