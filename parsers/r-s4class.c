/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for S4 Classes and Methods.
*   https://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf
*/


/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include "r.h"
#include "kind.h"
#include "tokeninfo.h"
#include "parse.h"
#include "subparser.h"

#include <string.h>


/*
*   DATA DECLARATIONS
*/

struct s4Subparser {
	rSubparser r;
};


/*
* FUNCTION PROTOTYPES
*/

static int s4ReadFuncall (rSubparser *s,
						  tokenInfo *const func, tokenInfo *const token,
						  int parent);
static int s4ReadRightSideSymbol (rSubparser *s,
								  tokenInfo *const symbol,
								  const char *const assignmentOperator,
								  int parent,
								  tokenInfo *const token);
static bool s4AskTagAcceptancy (rSubparser *s, tagEntryInfo *pe);
static bool s4HasFunctionAlikeKind (rSubparser *s, tagEntryInfo *e);


/*
*   DATA DEFINITIONS
*/

typedef enum {
	S4_K_CLASS,
	S4_K_REPRESENTATION,
	S4_K_GENERIC,
	S4_K_METHOD,
} s4Kind;

static kindDefinition S4Kinds[] = {
	{ true, 'c', "class",   "classes" },
	{ true, 'r', "repr",    "representations" },
	{ true, 'g', "generic", "generics" },
	{ true, 'm', "method",  "methods" },
};

static struct s4Subparser s4Subparser = {
	.r = {
		.subparser = {
			.direction = SUBPARSER_BI_DIRECTION,
		},
		.readFuncall = s4ReadFuncall,
		.readRightSideSymbol = s4ReadRightSideSymbol,
		.askTagAcceptancy = s4AskTagAcceptancy,
		.hasFunctionAlikeKind = s4HasFunctionAlikeKind,
	},
};


/*
*   FUNCTION DEFINITIONS
*/

/*
 * V=================V: parse this area
 * representation(...)) ...
 * representation(...), ...
 */
static void parseRepresentation (rSubparser *s, tokenInfo *const token, int parent)
{
	rTokenReadNoNewline (token);
	if (tokenIsTypeVal (token, '('))
	{
		rTokenReadNoNewline (token);
		while (!(tokenIsTypeVal (token, ')') || tokenIsTypeVal (token, ',')))
		{
			if (!rParseStatement (token, parent, false))
				break;
			else if (tokenIsTypeVal (token, '\n'))
				rTokenReadNoNewline (token);
		}
	}
	else
		tokenUnread (token);
}

/*
 * V==============V: parse this area
 * contains = "..."
 */
static void parseContains (rSubparser *s, tokenInfo *const token, int parent)
{
	rTokenReadNoNewline (token);
	if (tokenIsTypeVal (token, '='))
	{
		rTokenReadNoNewline (token);
		if (tokenIsType (token, R_STRING))
		{
			tagEntryInfo *e = getEntryInCorkQueue (parent);
			if (e)
			{
				vString *n = rExtractNameFromString (token->string);
				e->extensionFields.inheritance = vStringDeleteUnwrap (n);
			}
		}
		else
			tokenUnread (token);
	}
	else
		tokenUnread (token);
}

/*
 *                  V====V: parse this area
 * setClass ("class", ...)
 */
static bool parseClassArgs (rSubparser *s, tokenInfo *const token, int parent,
							tokenInfo *const open_paren CTAGS_ATTR_UNUSED)
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
			if (strcmp (str, "representation") == 0)
				parseRepresentation (s, token, parent);
			else if (strcmp (str, "contains") == 0)
				parseContains (s, token, parent);
		}
	}
	while (!tokenIsEOF (token));

	return false;
}

/*
 *                    V============V: parse this area
 * setMethod ("method", c(...), ...)
 *                    V====================V: parse this area
 * setMethod ("method", signature(...), ...)
 *
 * Attaching c(...) or signature(...) to "signature:" field of
 * the tag for "method" specified by parent.
 */
static bool parseMethodArgs (rSubparser *s, tokenInfo *const token, int parent,
							 tokenInfo *const open_paren)
{
	rTokenReadNoNewline (token);
	if (tokenIsTypeVal (token, ','))
	{
		rTokenReadNoNewline (token);
		if (tokenIsKeyword (token, R_C)
			|| (tokenIsType (token, R_SYMBOL)
				&& (strcmp (tokenString (token), "signature") == 0)))
		{
			rTokenReadNoNewline (token);
			if (tokenIsTypeVal (token, '('))
			{
				vString *signature = vStringNewInit("(");
				rSetupCollectingSignature (token, signature);
				tokenSkipOverPair (token);
				rTeardownCollectingSignature (token);
				tagEntryInfo *e = getEntryInCorkQueue (parent);
				if (e)
				{
					e->extensionFields.signature = vStringDeleteUnwrap (signature);
					signature = NULL;
				}
				vStringDelete (signature); /* NULL is acceptable */
			}
			rTokenReadNoNewline (token);
			if (tokenIsTypeVal (token, ','))
			{
				rTokenReadNoNewline (token);
				/* anonymous function for implementing this method may be here.*/
				while (!tokenIsTypeVal (token, ')'))
				{
					if (!rParseStatement (token, parent, true))
						break;
					else if (tokenIsTypeVal (token, '\n'))
						rTokenReadNoNewline (token);
				}
			}
			return false;
		}
		else
			tokenUnread (token);
	}
	else
		tokenUnread (token);
	return true;
}

/*
 *                      V====V: parse this area
 * setGeneric ("generic", ...)
 */
static bool parseGenericArgs (rSubparser *s, tokenInfo *const token, int parent,
							  tokenInfo *const open_paren)
{
	while (!tokenIsTypeVal (token, ')'))
	{
		if (!rParseStatement (token, parent, true))
			break;
		else if (tokenIsTypeVal (token, '\n'))
			rTokenReadNoNewline (token);
	}
	return false;
}

/* parse
 * this
 * area ---\          /--- parseArgs parses this area.
 *         V====VV----V
 *   setXXX("...", ...)
 */
static int parseSetCommon (rSubparser *s, tokenInfo *const token, int parent,
						   int kind,
						   bool (* parseArgs) (rSubparser *, tokenInfo *const, int, tokenInfo *const))
{
	int q = CORK_NIL;
	tokenInfo *const open_paren = newTokenByCopying (token);

	rTokenReadNoNewline (token);
	if (tokenIsType (token, R_STRING))
	{
		vString *n = rExtractNameFromString (token->string);
		if (n)
		{
			q = makeSimpleTag (n, kind);
			vStringDelete (n);
		}
	}

	bool skip = true;
	if (q != CORK_NIL && parseArgs)
		skip = (* parseArgs) (s, token, q, open_paren);

	if (skip)
	{
		tokenCopy (token, open_paren);
		tokenSkipOverPair (token);
	}

	tokenDelete (open_paren);
	return q;
}

static int parseSetClass (rSubparser *s, tokenInfo *const token, int parent)
{
	return parseSetCommon (s, token, parent, S4_K_CLASS, parseClassArgs);
}

static int parseSetGeneric (rSubparser *s, tokenInfo *const token, int parent)
{
	return parseSetCommon (s, token, parent, S4_K_GENERIC, parseGenericArgs);
}

static int parseSetMethod (rSubparser *s, tokenInfo *const token, int parent)
{
	return parseSetCommon (s, token, parent, S4_K_METHOD, parseMethodArgs);
}

static int s4ReadFuncall (rSubparser *s,
						  tokenInfo *const func, tokenInfo *const token,
						  int parent)
{
	if (strcmp (tokenString (func), "setClass") == 0)
		return parseSetClass (s, token, parent);
	else if (strcmp (tokenString (func), "setGeneric") == 0)
		return parseSetGeneric (s, token, parent);
	else if (strcmp (tokenString (func), "setMethod") == 0)
		return parseSetMethod (s, token, parent);
	else
		return CORK_NIL;
}

static bool s4AskTagAcceptancy (rSubparser *s, tagEntryInfo *pe)
{
	return (pe->kindIndex == S4_K_CLASS);
}

static bool s4HasFunctionAlikeKind (rSubparser *s, tagEntryInfo *e)
{
	return e->kindIndex == S4_K_METHOD ||
		e->kindIndex == S4_K_GENERIC;
}

/*
 * setClass("class", representation(r0 = "t0", r1 = "t1", ...
 *
 * Attaching t as "typeref:typename:" field of r.
 *
 * the tag for "class" -> parent.
 * r0, r1, ... -> symbol
 */
static int s4ReadRightSideSymbol (rSubparser *s,
								  tokenInfo *const symbol,
								  const char *const assignmentOperator,
								  int parent,
								  tokenInfo *const token)
{
	tagEntryInfo *pe = getEntryInCorkQueue (parent);
	if (! (pe
		   && pe->langType == s->subparser.slaveParser->id
		   && pe->kindIndex == S4_K_CLASS))
		return CORK_NIL;

	tagEntryInfo e;
	int q;
	vString *t = NULL;

	if (tokenIsType (token, R_STRING))
		t = rExtractNameFromString (token->string);

	initTagEntry (&e, tokenString (symbol), S4_K_REPRESENTATION);
	e.extensionFields.scopeIndex = parent;
	if (t)
	{
		e.extensionFields.typeRef[0] = "typename";
		e.extensionFields.typeRef[1] = vStringValue (t);
	}
	q = makeTagEntry (&e);
	vStringDelete (t);			/* NULL is acceptable. */

	return q;
}

static void findS4Tags(void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

extern parserDefinition* S4ClassParser (void)
{
	parserDefinition* const def = parserNew("S4Class");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "R", &s4Subparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = S4Kinds;
	def->kindCount = ARRAY_SIZE(S4Kinds);

	def->parser =  findS4Tags;
	def->useCork = CORK_QUEUE;

	return def;
}
