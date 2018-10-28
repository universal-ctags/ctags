/*
 *   yumrepo.c
 *
 *   Copyright (c) 2016, Masatake YAMATO <yamato@redhat.com>
 *   Copyright (c) 2016, Red Hat, K.K.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for yum repo file
 *
 */

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "iniconf.h"
#include "kind.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include <string.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_REPO_ID,
} yumRepoKind;

static kindDefinition YumRepoKinds [] = {
	{ true, 'r', "repoid", "repository id" },
};


static void newDataCallback (iniconfSubparser *s CTAGS_ATTR_UNUSED,
							 const char *section, const char *key, const char *value)
{
	tagEntryInfo e;

	if (section && key == NULL && value == NULL)
	{
		initTagEntry (&e, section, K_REPO_ID);
		makeTagEntry (&e);
	}
}

static void findYumRepoTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* YumRepoParser (void)
{
	static const char *const extensions [] = { "repo", NULL };
	static iniconfSubparser yumRepoSubparser = {
		.subparser = {
			.direction = SUBPARSER_SUB_RUNS_BASE,
		},
		.newDataNotify = newDataCallback,
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Iniconf", &yumRepoSubparser },
	};

	parserDefinition* const def = parserNew ("YumRepo");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable      = YumRepoKinds;
	def->kindCount  = ARRAY_SIZE (YumRepoKinds);
	def->extensions = extensions;
	def->parser     = findYumRepoTags;

	return def;
}
