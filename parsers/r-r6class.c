/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for R6Class.
*   https://www.rdocumentation.org/packages/R6/versions/2.4.1/topics/R6Class
*/


/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include "r.h"
#include "kind.h"
#include "parse.h"
#include "entry.h"
#include "tokeninfo.h"
#include "read.h"

#include <string.h>


/*
*   DATA DECLARATIONS
*/

struct r6Subparser {
	rSubparser r;
	const char * access;
	bool activeBinding;
};

/*
* FUNCTION PROTOTYPES
*/

static int r6ReadRightSideSymbol (rSubparser *s,
								  tokenInfo *const symbol,
								  const char *const assignmentOperator,
								  int parent,
								  tokenInfo *const token);
static int r6MakeTagWithTranslation (rSubparser *s,
									 tokenInfo *const token,
									 int parent,
									 bool in_func,
									 int kindInR,
									 const char *const assignmentOperator);
static bool r6AskTagAcceptancy (rSubparser *s, tagEntryInfo *pe);
static bool r6HasFunctionAlikeKind (rSubparser *s, tagEntryInfo *e);

static void parseClass (rSubparser *s, tokenInfo *const token, int classIndex);


/*
*   DATA DEFINITIONS
*/

typedef enum {
	R6_K_CLASS,
	R6_K_METHOD,
	R6_K_FIELD,
	R6_K_ACTIVE_BINDING_FUNCTION,
} r6Kind;

static kindDefinition R6Kinds[] = {
	{ true, 'c', "class",  "classes" },
	{ true, 'm', "method", "methods" },
	{ true, 'f', "field",  "fields" },
	{ true, 'a', "activeBindingFunc", "active binding functions" },
};

static struct r6Subparser r6Subparser = {
	.r = {
		.subparser = {
			.direction = SUBPARSER_BI_DIRECTION,
		},
		.readRightSideSymbol = r6ReadRightSideSymbol,
		.makeTagWithTranslation = r6MakeTagWithTranslation,
		.askTagAcceptancy = r6AskTagAcceptancy,
		.hasFunctionAlikeKind = r6HasFunctionAlikeKind,
	},
	.access = NULL,
	.activeBinding = false,
};


/*
*   FUNCTION DEFINITIONS
*/

/*
 *       parse this area:              V======V
 * klass = R6Class("klass", ..., inherit=parent)
 */
static void parseInherit (rSubparser *s,
							tokenInfo *const token, int classIndex)
{
	rTokenReadNoNewline (token);
	if (tokenIsTypeVal (token, '='))
	{
		rTokenReadNoNewline (token);
		if (tokenIsType (token, R_SYMBOL))
		{
			tagEntryInfo *e = getEntryInCorkQueue (classIndex);
			e->extensionFields.inheritance = vStringStrdup (token->string);
		}
		else
			tokenUnread (token);
	}
	else
		tokenUnread (token);
}

/*
 *       parse this area:              V===V              V===V             V===V
 * klass = R6Class("klass", public=list(...), private=list(...), active=list(...), ...)
 */
static void parseListMain (rSubparser *s,
							 tokenInfo *const token, int classIndex, const char *access,
							 bool activeBinding)
{
	const char *last_access = ((struct r6Subparser *)s)->access;
	((struct r6Subparser *)s)->access = access;

	bool last_active_binding = ((struct r6Subparser *)s)->activeBinding;
	((struct r6Subparser *)s)->activeBinding = activeBinding;

	rTokenReadNoNewline (token);

	while (! tokenIsTypeVal (token, ')'))
	{
		if (!rParseStatement (token, classIndex, false))
			break;
		else if (tokenIsTypeVal (token, '\n'))
			rTokenReadNoNewline (token);
	}
	((struct r6Subparser *)s)->access = last_access;
	((struct r6Subparser *)s)->activeBinding = last_active_binding;
}

/*
 *       parse this area:        V=====|---V        V=====|---V       V=====|---V
 * klass = R6Class("klass", public=list(...), private=list(...), active=list(...), ...)
 */
static void parseList (rSubparser *s,
					   tokenInfo *const token, int classIndex, const char *access,
					   bool activeBinding)
{
	rTokenReadNoNewline (token);
	if (tokenIsTypeVal (token, '='))
	{
		rTokenReadNoNewline (token);
		if (tokenIsKeyword (token, R_LIST))
		{
			rTokenReadNoNewline (token);
			if (tokenIsTypeVal (token, '('))
				parseListMain (s, token, classIndex, access, activeBinding);
			else
				tokenUnread (token);
		}
		else
			tokenUnread (token);
	}
	else
		tokenUnread (token);
}

/*
 *      parse this area: V=======|----V=======|----V======|----V=======|----V
 * klass = R6Class("klass", public=..., private=..., active=..., inherit=...)
 */
static void parseClass (rSubparser *s,
						  tokenInfo *const token, int classIndex)
{
	do
	{
		rTokenReadNoNewline (token);

		if (tokenIsTypeVal (token, ')'))
			break;
		else if (tokenIsTypeVal (token, '('))
			tokenSkipOverPair (token);
		else if (tokenIsTypeVal (token, '{'))
			tokenSkipOverPair (token);
		else if (tokenIsTypeVal (token, '['))
			tokenSkipOverPair (token);
		else if (tokenIsType (token, R_SYMBOL))
		{
			const char *str = tokenString (token);
			if (strcmp (str, "inherit") == 0)
				parseInherit (s, token, classIndex);
			else if (strcmp (str, "public") == 0)
				parseList (s, token, classIndex, "public", false);
			else if (strcmp (str, "private") == 0)
				parseList (s, token, classIndex, "private", false);
			else if (strcmp (str, "active") == 0)
				parseList (s, token, classIndex, "public", true);
		}
	}
	while (!tokenIsEOF (token));
}

/*
 *         V=============|-----V: parse this area
 * klass = R6Class("klass", ...)
 */
static int r6ReadRightSideSymbol (rSubparser *s,
								  tokenInfo *const symbol,
								  const char *const assignmentOperator,
								  int parent,
								  tokenInfo *const token)
{
	tokenInfo * token0 = NULL;
	tokenInfo * token1 = NULL;
	if (strcmp (tokenString (token), "R6") == 0)
	{
		tokenInfo * token0 = rNewToken ();
		tokenRead (token0);
		if (!tokenIsType (token0, R_SCOPE))
			goto reject;
		if (strcmp (tokenString (token0), "::"))
			goto reject;

		tokenInfo * token1 = rNewToken ();
		tokenRead (token1);
		if (!tokenIsType (token1, R_SYMBOL))
			goto reject;
		if (strcmp (tokenString (token1), "R6Class"))
			goto reject;
		tokenCopy (token, token1);
		tokenDelete (token1);
		tokenDelete (token0);
		token0 = token1 = NULL;
	}
	else if (strcmp (tokenString (token), "R6Class") != 0)
		return CORK_NIL;

	rTokenReadNoNewline (token);
	if (tokenIsTypeVal (token, '('))
	{
		int corkIndex = makeSimpleTag (symbol->string, R6_K_CLASS);
		tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
		if (e)
			e->extensionFields.scopeIndex = parent;
		parseClass (s, token, corkIndex);
		return corkIndex;
	}
	return CORK_NIL;
 reject:
	if (token1)
		tokenUnread (token1);
	if (token0)
		tokenUnread (token0);
	/* tokenDelete accepts NULL. */
	tokenDelete (token1);
	tokenDelete (token0);

	return CORK_NIL;
}

static int r6MakeTagWithTranslation (rSubparser *s,
									 tokenInfo *const token,
									 int parent,
									 bool in_func,
									 int kindInR,
									 const char *const assignmentOperator)
{
	tagEntryInfo e;

	initTagEntry (&e, tokenString (token),
				  in_func
				  ? (((struct r6Subparser*)s)->activeBinding
					 ? R6_K_ACTIVE_BINDING_FUNCTION
					 : R6_K_METHOD)
				  : R6_K_FIELD);
	e.extensionFields.scopeIndex = parent;
	e.extensionFields.access = ((struct r6Subparser*)s)->access;

	return makeTagEntry (&e);
}

static bool r6AskTagAcceptancy (rSubparser *s, tagEntryInfo *pe)
{
	return (pe->kindIndex == R6_K_CLASS);
}

static bool r6HasFunctionAlikeKind (rSubparser *s,
									tagEntryInfo *e)
{
	return e->kindIndex == R6_K_METHOD ||
		e->kindIndex == R6_K_ACTIVE_BINDING_FUNCTION;
}

static void findR6Tags(void)
{
	r6Subparser.access = NULL;
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

extern parserDefinition* R6ClassParser (void)
{
	parserDefinition* const def = parserNew("R6Class");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "R", &r6Subparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = R6Kinds;
	def->kindCount = ARRAY_SIZE(R6Kinds);

	def->parser =  findR6Tags;
	def->useCork = CORK_QUEUE;

	return def;
}
