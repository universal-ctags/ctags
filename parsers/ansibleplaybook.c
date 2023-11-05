/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "kind.h"
#include "yaml.h"
#include "parse.h"
#include "subparser.h"

typedef enum {
	K_PLAY
} ansiblePlaybookKind;

static kindDefinition AnsiblePlaybookKinds [] = {
	{ true,  'p', "play", "plays" },
};

struct sAnsiblePlaybookSubparser {
	yamlSubparser yaml;
	int nameIndex;
};

static tagYpathTable ypathTables [] = {
	{ "*/name",
	  YPATH_DSTAT_LAST_VALUE, K_PLAY, },
};

static void
findAnsiblePlaybookTags (void)
{
	scheduleRunningBaseparser (0);
}

static void inputStart(subparser *s)
{
	((struct sAnsiblePlaybookSubparser*)s)->nameIndex = CORK_NIL;
}

static void makeTagEntryCallbackViaYpath(yamlSubparser *s, int corkIndex)
{
	/* a mapping in a sequence */
	if (ypathGetTypeStackDepth(s) == 2)
		((struct sAnsiblePlaybookSubparser *)s)->nameIndex = corkIndex;
}

static void leaveBlockCallback(yamlSubparser *s, yaml_token_t *token)
{
	struct sAnsiblePlaybookSubparser *ansible = (struct sAnsiblePlaybookSubparser *)s;

	if (token
		&& ansible->nameIndex != CORK_NIL
		&& ypathGetTypeStackDepth(s) == 2)
	{
		tagEntryInfo *tag = getEntryInCorkQueue (ansible->nameIndex);
		if (tag)
			attachYamlPosition (tag, token, true);
		ansible->nameIndex = CORK_NIL;
	}
}

extern parserDefinition* AnsiblePlaybookParser (void)
{
	static struct sAnsiblePlaybookSubparser ansiblePlaybookSubparser = {
		.yaml = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
				.inputStart = inputStart,
			},
			.ypathTables = ypathTables,
			.ypathTableCount = ARRAY_SIZE (ypathTables),
			.leaveBlockNotify = leaveBlockCallback,
			.makeTagEntryNotifyViaYpath = makeTagEntryCallbackViaYpath,
		},
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "Yaml", &ansiblePlaybookSubparser },
	};

	parserDefinition* const def = parserNew ("AnsiblePlaybook");


	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable         = AnsiblePlaybookKinds;
	def->kindCount     = ARRAY_SIZE (AnsiblePlaybookKinds);
	def->parser        = findAnsiblePlaybookTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
