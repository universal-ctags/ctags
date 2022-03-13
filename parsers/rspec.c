/*
*
* Copyright (c) 2017, Red Hat, Inc.
* Copyright (c) 2017, Masatake YAMATO
*
* Author: Masatake YAMATO <yamato@redhat.com>
*
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
* USA.
*
* Inspired by the following commit but written from scratch:
* ==========================================================
*
* commit 76dbed4de88875d8c8409dfd65da4f94f901c94a
* Author: Ram Singla <ram.singla@gmail.com>
* Date:   Tue Jan 18 13:24:46 2011 +0800
*
*    RSpec Code added. Courtesy: mortice
*
* ==========================================================
*
* Reference:
* - https://rubydoc.info/gems/rspec-core/frames
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "kind.h"
#include "numarray.h"
#include "parse.h"
#include "subparser.h"

#include "ruby.h"

#include <string.h>

/*
* DATA STRUCTURES
*/
struct sRSpecSubparser {
	rubySubparser ruby;
	int scope;
};

typedef enum {
	K_DESCRIBE,
	K_CONTEXT,
} rspecKind;

static kindDefinition RSpecKinds [] = {
	{ true, 'd', "describe", "describes" },
	{ true, 'c', "context", "contexts" },
};

/*
* FUNCTIONS
*/
static int makeSimpleRSpecTag (vString *vstr, int kindIndex, rubySubparser *subparser)
{
	int r = makeSimpleTag (vstr, kindIndex);
	tagEntryInfo *e = getEntryInCorkQueue (r);
	if (e)
	{
		struct sRSpecSubparser *rspec = (struct sRSpecSubparser *)subparser;
		e->extensionFields.scopeIndex = rspec->scope;
	}
	return r;
}

static vString *readRest (const unsigned char **cp)
{
	vString *vstr = NULL;
	unsigned char b;

	switch (**cp)
	{
	case '\'':
	case '"':
		b = **cp;
		++*cp;
		vstr = vStringNew ();
		if (!rubyParseString (cp, b, vstr))
		{
			vStringDelete (vstr);
			vstr = NULL;
		}
		break;
	case ':':
		/* symbol, Should this be part of the Ruby parser side? */
		break;
	default:
		vstr = vStringNew ();
		if (!rubyParseModuleName (cp, vstr))
		{
			vStringDelete (vstr);
			vstr = NULL;
		}
		else if (strcmp (vStringValue (vstr), "do") == 0)
		{
			vStringDelete (vstr);
			vstr = NULL;
			*cp += -2;			/* push back "do" */
		}

		break;
	}

	if (vstr)
	{
		rubySkipWhitespace (cp);
		if (**cp == ',')
		{
			++*cp;
			rubySkipWhitespace (cp);
			vString *vstr_rest = readRest (cp);
			if (vstr_rest)
			{
				vStringPut (vstr, ' ');
				vStringCat (vstr, vstr_rest);
				vStringDelete (vstr_rest);
			}
		}
	}

	return vstr;
}

static int lineNotify (rubySubparser *s, const unsigned char **cp)
{
	if (rubyCanMatchKeywordWithAssign (cp, "describe")
		|| rubyCanMatchKeywordWithAssign (cp, "RSpec.describe"))
	{
		vString *vstr = NULL;
		rubySkipWhitespace (cp);
		vstr = readRest (cp);
		if (vstr)
		{
			int r = makeSimpleRSpecTag (vstr, K_DESCRIBE, s);
			vStringDelete (vstr);
			return r;
		}
	}
	else if (rubyCanMatchKeywordWithAssign (cp, "context"))
	{
		vString *vstr = NULL;
		rubySkipWhitespace (cp);
		vstr = readRest (cp);
		if (vstr)
		{
			int r = makeSimpleRSpecTag (vstr, K_CONTEXT, s);
			vStringDelete (vstr);
			return r;
		}
	}
	return CORK_NIL;
}

static void enterBlockNotify (rubySubparser *s, int corkIndex)
{
	struct sRSpecSubparser *rspec = (struct sRSpecSubparser *)s;
	rspec->scope = corkIndex;
}

static void leaveBlockNotify (rubySubparser *s, int corkIndex)
{
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	if (e)
	{
		struct sRSpecSubparser *rspec = (struct sRSpecSubparser *)s;
		rspec->scope = e->extensionFields.scopeIndex;
	}
}

static void findRSpecTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void inputStart (subparser *s)
{
	struct sRSpecSubparser *rspec = (struct sRSpecSubparser *)s;
	rspec->scope = CORK_NIL;
}

extern parserDefinition* RSpecParser (void)
{
	static struct sRSpecSubparser rspecSubparser = {
		.ruby = {
			.subparser = {
				.direction = SUBPARSER_BASE_RUNS_SUB,
				.inputStart = inputStart,
			},
			.lineNotify = lineNotify,
			.enterBlockNotify = enterBlockNotify,
			.leaveBlockNotify = leaveBlockNotify,
		},
		.scope = CORK_NIL,
	};

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Ruby", &rspecSubparser },
	};

	parserDefinition* const def = parserNew ("RSpec");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable  = RSpecKinds;
	def->kindCount  = ARRAY_SIZE (RSpecKinds);
	def->parser     = findRSpecTags;
	def->useCork    = CORK_QUEUE;

	return def;
}
