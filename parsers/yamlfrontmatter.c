/*
*
*   Copyright (c) 2022, Masatake YAMATO
*   Copyright (c) 2022, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
* This module contains functions for extracting language objects in FrontMatter
* using Yaml.
*
* https://gohugo.io/content-management/front-matter
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include "frontmatter.h"
#include "yaml.h"

#include "entry.h"
#include "gcc-attr.h"
#include "parse.h"
#include "read.h"
#include "subparser.h"
#include "trace.h"


/*
*   DATA DECLARATIONS
*/
enum yamlfrontmatterDetectingState {
	DSTAT_LAST_KEY,
	DSTAT_LAST_VALUE,
	DSTAT_INITIAL,
};

struct sYamlFrontMatterSubparser {
	yamlSubparser yaml;
	enum yamlfrontmatterDetectingState detection_state;
};


/*
*   FUNCTION PROTOTYPES
*/
static bool yamlFrontmattterInitTagEntry (tagEntryInfo *e, char *name, void *data);


/*
*   DATA DEFINITIONS
*/

static langType frontMatterLang;

static tagYpathTable ypathTables [] = {
	{
		"title",
		DSTAT_LAST_VALUE,
		.initTagEntry = yamlFrontmattterInitTagEntry,
	},
};


/*
*   FUNCTION DEFINITIONS
*/

static void	yamlfrontmatterStateMachine (struct sYamlFrontMatterSubparser *yamlfrontmatter,
								 yaml_token_t *token)
{
#ifdef DO_TRACING
	ypathPrintTypeStack (YAML(yamlfrontmatter));
#endif

	switch (token->type)
	{
	case YAML_KEY_TOKEN:
		yamlfrontmatter->detection_state = DSTAT_LAST_KEY;
		break;
	case YAML_SCALAR_TOKEN:
		switch (yamlfrontmatter->detection_state)
		{
		case DSTAT_LAST_KEY:
			ypathFillKeywordOfTokenMaybe (YAML(yamlfrontmatter), token, getInputLanguage ());
			/* FALL THROUGH */
		case DSTAT_LAST_VALUE:
			TRACE_PRINT("token-callback: %s: %s",
						(yamlfrontmatter->detection_state == DSTAT_LAST_KEY)? "key": "value",
						(char*)token->data.scalar.value);
			ypathHandleToken (YAML(yamlfrontmatter), token, yamlfrontmatter->detection_state,
							  ypathTables, ARRAY_SIZE (ypathTables));
			break;
		default:
			break;
		}

		yamlfrontmatter->detection_state = DSTAT_INITIAL;

		break;
	case YAML_VALUE_TOKEN:
		yamlfrontmatter->detection_state = DSTAT_LAST_VALUE;
		break;

	default:
		yamlfrontmatter->detection_state = DSTAT_INITIAL;
		break;
	}
}

static void newTokenCallback (yamlSubparser *s, yaml_token_t *token)
{
	if (token->type == YAML_BLOCK_SEQUENCE_START_TOKEN
		|| token->type == YAML_BLOCK_MAPPING_START_TOKEN)
		ypathPushType (s, token);

	yamlfrontmatterStateMachine ((struct sYamlFrontMatterSubparser *)s, token);

	if (token->type == YAML_BLOCK_END_TOKEN)
		ypathPopType (s);
	else if (token->type == YAML_STREAM_END_TOKEN)
		ypathPopAllTypes (s);
}

static bool yamlFrontmattterInitTagEntry (tagEntryInfo *e, char *name, void * data CTAGS_ATTR_UNUSED)
{
	initForeignTagEntry (e, name, frontMatterLang, FRONTMATTER_TITLE_KIND);
	return true;
}

static void yamlFrontMatterInputStart(subparser *s)
{
	((struct sYamlFrontMatterSubparser*)s)->detection_state = DSTAT_INITIAL;
	((yamlSubparser*)s)->ypathTypeStack = NULL;
}

static void yamlFrontMatterInputEnd(subparser *s)
{
	Assert (((yamlSubparser*)s)->ypathTypeStack == NULL);
}

static void findYamlFrontMatterTags (void)
{
	scheduleRunningBaseparser (0);
}

static void yamlFrontMatterInitialize (langType language)
{
	ypathCompileTables (language, ypathTables, ARRAY_SIZE (ypathTables), 0);
	frontMatterLang = getNamedLanguage ("FrontMatter", 0);
}

static void yamlFrontMatterFinalize (langType language, bool initialized)
{
	if (initialized)
		ypathCompiledCodeDelete (ypathTables, ARRAY_SIZE (ypathTables));
}

extern parserDefinition* YamlFrontMatter (void)
{
	static struct sYamlFrontMatterSubparser yamlfrontmatterSubparser = {
		.yaml = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
				.inputStart = yamlFrontMatterInputStart,
				.inputEnd = yamlFrontMatterInputEnd,
			},
			.newTokenNotfify = newTokenCallback
		},
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "Yaml", &yamlfrontmatterSubparser },
		{ DEPTYPE_FOREIGNER, "FrontMatter", NULL },
	};

	parserDefinition* const def = parserNew ("YamlFrontMatter");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable	= NULL;
	def->kindCount = 0;
	def->parser	= findYamlFrontMatterTags;
	def->initialize = yamlFrontMatterInitialize;
	def->finalize = yamlFrontMatterFinalize;

	/* This parser runs ONLY as a part of FrontMatter parser.
	 * User may not want to enable/disable this parser directly. */
	def->invisible = true;

	return def;
}
