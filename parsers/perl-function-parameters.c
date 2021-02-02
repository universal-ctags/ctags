/*
 *   Copyright (c) 2021, Masatake YAMATO
 *   Copyright (c) 2021, Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for Function::Parameters perl extension.
 *   https://metacpan.org/pod/Function::Parameters
 *
 *   This parser is inspired by the pull request submitted by Jim Butler (@jimmygoogle).
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include "entry.h"
#include "kind.h"
#include "parse.h"
#include "perl.h"

#include <string.h>

/*
 *   DATA DECLARATIONS
 */

enum FParamsKind {
	K_METHOD,
	K_FUN,
};

static kindDefinition FParamsKinds[] = {
	{ true, 'm', "method", "methods" },
	{ true, 'f', "fun",    "functions" },
};

struct FParamsSubparser {
	perlSubparser perl;
	bool notInFParams;
	bool inPod;
};

/*
 *   FUNCTION PROTOTYPES
 */

static void inputStart (subparser *s);
static void makeTagEntryNotify (subparser *s, const tagEntryInfo *tag, int corkIndex);
static void enterFParams (struct FParamsSubparser *fparms);
static void leaveFParams (struct FParamsSubparser *fparams);
static void enteringPodNotify (perlSubparser *perl);
static void leavingPodNotify  (perlSubparser *perl);

/*
 *   DATA DEFINITIONS
 */

static struct FParamsSubparser fparamsSubparser = {
	.perl = {
		.subparser = {
			.direction  = SUBPARSER_BI_DIRECTION,
			.inputStart = inputStart,
			.makeTagEntryNotify = makeTagEntryNotify,
		},
		.enteringPodNotify = enteringPodNotify,
		.leavingPodNotify  = leavingPodNotify,
	}
};

/*
 *   FUNCTION DEFINITIONS
 */

static void inputStart (subparser *s)
{
	struct FParamsSubparser *fparams = (struct FParamsSubparser *)s;

	fparams->notInFParams = true;
	fparams->inPod = false;
}

static void makeTagEntryNotify (subparser *s, const tagEntryInfo *tag, int corkIndex)
{
	perlSubparser *perl = (perlSubparser *)s;
	struct FParamsSubparser *fparams = (struct FParamsSubparser *)perl;

	if (isTagExtraBitMarked(tag, XTAG_QUALIFIED_TAGS))
		return;

	if (tag->kindIndex == KIND_PERL_MODULE)
	{
		if (isRoleAssigned(tag, ROLE_PERL_MODULE_USED)
			&& strcmp (tag->name, "Function::Parameters") == 0)
			enterFParams (fparams);
		else if (isRoleAssigned(tag, ROLE_PERL_MODULE_UNUSED)
				 && strcmp (tag->name, "Function::Parameters") == 0)
			leaveFParams (fparams);
	}
}

static void enterFParams (struct FParamsSubparser *fparams)
{
	fparams->notInFParams = false;
}

static void leaveFParams (struct FParamsSubparser *fparams)
{
	fparams->notInFParams = true;
}

static void enteringPodNotify (perlSubparser *perl)
{
	struct FParamsSubparser *fparams = (struct FParamsSubparser *)perl;
	fparams->inPod = true;
}

static void leavingPodNotify  (perlSubparser *perl)
{
	struct FParamsSubparser *fparams = (struct FParamsSubparser *)perl;
	fparams->inPod = false;
}

static bool findFParamsObject (const char * line, const regexMatch *matches, unsigned int count,
							   void *userData)
{
	struct FParamsSubparser *fparams = (struct FParamsSubparser *)userData;

	if (fparams->inPod)
		return false;


	const char *kindHint = line + matches[1].start;
	int kind = kindHint [0] == 'm'? K_METHOD: K_FUN;

	char *name = eStrndup (line + matches[2].start, matches[2].length);
	tagEntryInfo e;
	initTagEntry (&e, name, kind);

	makeTagEntry (&e);
	eFree (name);
	return true;
}

static void findFParamsTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void initializeFParamsParser (langType language)
{
	addLanguageCallbackRegex (language, "^[ \t]*(method|fun)[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)[ \t]*\\(",
							  NULL,
							  findFParamsObject, &fparamsSubparser.notInFParams,
							  &fparamsSubparser);
}

extern parserDefinition* FunctionParametersParser (void)
{
	parserDefinition* const def = parserNew("FunctionParameters");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Perl", &fparamsSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = FParamsKinds;
	def->kindCount = ARRAY_SIZE(FParamsKinds);

	def->initialize = initializeFParamsParser;
	def->parser = findFParamsTags;

	return def;
}
