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
#include "read.h"
#include "routines.h"
#include <string.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_REPO_ID,
} yumRepoKind;

static kindOption YumRepoKinds [] = {
	{ TRUE, 'r', "repoid", "repository id" },
};


static void makeYumRepoTag (const char *section, const char *key, const char *value,
			       void *userData)
{
	tagEntryInfo e;

	if (section && key == NULL && value == NULL)
	{
		initTagEntry (&e, section, YumRepoKinds + K_REPO_ID);
		makeTagEntry (&e);
	}
}

static void findYumRepoTags (void)
{
	runIniconfParser (makeYumRepoTag, NULL);
}

extern parserDefinition* YumRepoParser (void)
{
	static const char *const extensions [] = { "repo", NULL };
	parserDefinition* const def = parserNew ("YumRepo");

	def->kinds      = YumRepoKinds;
	def->kindCount  = ARRAY_SIZE (YumRepoKinds);
	def->extensions = extensions;
	def->parser     = findYumRepoTags;

	return def;
}
