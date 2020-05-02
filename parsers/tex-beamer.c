/*
 *	 Copyright (c) 2020, Masatake YAMATO
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating tags for Beamer class of TeX.
 *   - https://github.com/josephwright/beamer
 *   - http://mirrors.ibiblio.org/CTAN/macros/latex/contrib/beamer/doc/beameruserguide.pdf
 *
 */

/*
 *	 INCLUDE FILES
 */

#include "general.h"  /* must always come first */

#include "entry.h"
#include "kind.h"
#include "parse.h"
#include "read.h"
#include "tex.h"

#include <string.h>


/*
 *	 DATA DECLARATIONS
 */

struct beamerSubparser {
	texSubparser tex;
	int lastTitleCorkIndex;
};

enum BamerKind {
	K_FRAMETITLE,
	K_FRAMESUBTITLE,
};


/*
 *	 DATA DEFINITIONS
 */

static kindDefinition beamerKinds[] = {
	{ true, 'f', "frametitle", "frametitles" },
	{ true, 'g', "framesubtitle", "framesubtitles"},
};

/* \frametitle<overlay specification>[short frame title]{frame title text} */
static struct TexParseStrategy frametitle_strategy[] = {
	{
		.type = '<',
		.flags = TEX_NAME_FLAG_OPTIONAL,
		.kindIndex = KIND_GHOST_INDEX,
		.name = NULL,
	},
	{
		.type = '[',
		.flags = TEX_NAME_FLAG_OPTIONAL|TEX_NAME_FLAG_EXCLUSIVE,
		.kindIndex = K_FRAMETITLE,
		.roleIndex = ROLE_DEFINITION_INDEX,
		.name = NULL,
		.unique = false,
	},
	{
		.type = '{',
		.flags = TEX_NAME_FLAG_INCLUDING_WHITESPACE,
		.kindIndex = K_FRAMETITLE,
		.roleIndex = ROLE_DEFINITION_INDEX,
		.name = NULL,
		.unique = true,
		.scopeIndex = CORK_NIL,	/* root scope */
	},
	{
		.type = 0
	}
};

/* \framesubtitle<overlay specification>{frame subtitle text} */
static struct TexParseStrategy framesubtitle_strategy[] = {
	{
		.type = '<',
		.flags = TEX_NAME_FLAG_OPTIONAL,
		.kindIndex = KIND_GHOST_INDEX,
		.name = NULL,
	},
	{
		.type = '{',
		.flags = TEX_NAME_FLAG_INCLUDING_WHITESPACE,
		.kindIndex = K_FRAMESUBTITLE,
		.roleIndex = ROLE_DEFINITION_INDEX,
		.name = NULL,
		.unique = false,
	},
	{
		.type = 0
	}
};

/*  \begin{frame}<overlay specification>[<default overlay specification>][options]{title}{subtitle}*/
static struct TexParseStrategy frame_env_strategy [] = {
	{
		.type = '<',
		.flags = TEX_NAME_FLAG_OPTIONAL,
		.kindIndex = KIND_GHOST_INDEX,
		.name = NULL,
	},
	{
		.type = '[',
		.flags = TEX_NAME_FLAG_OPTIONAL,
		.kindIndex = KIND_GHOST_INDEX,
		.name = NULL,
	},
	{
		.type = '[',
		.flags = TEX_NAME_FLAG_OPTIONAL,
		.kindIndex = KIND_GHOST_INDEX,
		.name = NULL,
	},
	{
		.type = '{',
		.flags = TEX_NAME_FLAG_INCLUDING_WHITESPACE,
		.kindIndex = K_FRAMETITLE,
		.roleIndex = ROLE_DEFINITION_INDEX,
		.unique = true,
		.scopeIndex = CORK_NIL,	/* root scope */
	},
	{
		/* This should not be optoinal. */
		.type = '{',
		.flags = TEX_NAME_FLAG_INCLUDING_WHITESPACE,
		.kindIndex = K_FRAMESUBTITLE,
		.roleIndex = ROLE_DEFINITION_INDEX,
		.name = NULL,
		.unique = false,
	},
	{
		.type = 0
	}
};


/*
 *	 FUNCTION DEFINITIONS
 */

static  struct TexParseStrategy *readIdentifierNotify (texSubparser *s,
													   vString *identifier)
{
	/* Reject uninteresting identifiers early. */
	if (! (vStringLength (identifier) > 10
		   && strncmp (vStringValue(identifier), "\\frame", 6) == 0))
		return NULL;
	else if (vStringLength (identifier) == 11
		&& strcmp (vStringValue(identifier), "\\frametitle") == 0)
		return frametitle_strategy;
	/* strlen("\\framesubtitle") = 14 */
	else if (vStringLength (identifier) == 14
		&& strcmp (vStringValue(identifier), "\\framesubtitle") == 0)
		return framesubtitle_strategy;
	else
		return NULL;
}

static void inputStart (subparser *s)
{
	struct beamerSubparser *b = (struct beamerSubparser *)s;
	b->lastTitleCorkIndex = CORK_NIL;
}

static void reportStrategicParsing (texSubparser *s,
									const struct TexParseStrategy *strategy)
{
	struct beamerSubparser *b = (struct beamerSubparser *)s;

	if (strategy == frametitle_strategy)
	{
		if (strategy [1].corkIndex != CORK_NIL)
			b->lastTitleCorkIndex = strategy [1].corkIndex;
		else if (strategy [2].corkIndex != CORK_NIL)
			b->lastTitleCorkIndex = strategy [2].corkIndex;
	}
	else if (strategy == framesubtitle_strategy)
	{
		tagEntryInfo *e = getEntryInCorkQueue (strategy [1].corkIndex);
		if (e)
			e->extensionFields.scopeIndex = b->lastTitleCorkIndex;
	}
	else if (strategy == frame_env_strategy)
	{
		b->lastTitleCorkIndex = CORK_NIL;

		if (strategy [3].corkIndex != CORK_NIL)
			b->lastTitleCorkIndex = strategy [3].corkIndex;

		tagEntryInfo *e = getEntryInCorkQueue (strategy [4].corkIndex);
		if (e && b->lastTitleCorkIndex != CORK_NIL)
			e->extensionFields.scopeIndex = b->lastTitleCorkIndex;
	}
}

static struct TexParseStrategy *readEnviromentBeginNotify (texSubparser *s,
														   vString *env)
{
	if (strcmp (vStringValue (env), "frame") == 0)
       return frame_env_strategy;
	return NULL;
}

static bool readEnviromentEndNotify (texSubparser *s,
									 vString *env)
{
	if (strcmp (vStringValue (env), "frame") == 0)
	{
		struct beamerSubparser *b = (struct beamerSubparser *)s;
		tagEntryInfo *e = getEntryInCorkQueue (b->lastTitleCorkIndex);
		if (e)
			e->extensionFields.endLine = getInputLineNumber ();
		return true;
	}
	return false;
}

static struct beamerSubparser beamerSubparser = {
	.tex = {
		.subparser = {
			.direction = SUBPARSER_BI_DIRECTION,
			.inputStart = inputStart,
		},
		.readIdentifierNotify = readIdentifierNotify,
		.readEnviromentBeginNotify = readEnviromentBeginNotify,
		.readEnviromentEndNotify = readEnviromentEndNotify,
		.reportStrategicParsing = reportStrategicParsing,
	},
};

static void findBeamerTags(void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

extern parserDefinition* TexBeamerParser (void)
{
	parserDefinition* const def = parserNew("TeXBeamer");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Tex", &beamerSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = beamerKinds;
	def->kindCount = ARRAY_SIZE(beamerKinds);

	def->parser = findBeamerTags;
	def->useCork = CORK_QUEUE;

	return def;
}
