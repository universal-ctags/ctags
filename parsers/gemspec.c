/*
* A parser for RUBYGEM's specification
*
* Copyright (c) 2021, Red Hat, Inc.
* Copyright (c) 2021, Masatake YAMATO
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
* Reference:
* - https://guides.rubygems.org/specification-reference/
*
* See also:
* - https://yehudakatz.com/2010/12/16/clarifying-the-roles-of-the-gemspec-and-gemfile/
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "parse.h"
#include "subparser.h"

#include "ruby.h"

#include <string.h>

/*
* DATA STRUCTURES
*/
struct sGemSpecSubparser {
	rubySubparser ruby;
	vString *var_name;
};

typedef enum {
	K_GEM,
} gemspecKind;

typedef enum {
	R_GEM_RUNTIME_DEP,
	R_GEM_DEVEL_DEP,
} gemspecGemRole;

static roleDefinition GemSpecGemRoles [] = {
	{ true, "runtimeDep", "specifying runtime dependency" },
	{ true, "develDep",   "specifying development dependency" },
};

static kindDefinition GemSpecKinds [] = {
	{ true, 'g', "gem", "gems",
	  .referenceOnly = false, ATTACH_ROLES(GemSpecGemRoles)},
};

/*
* FUNCTIONS
*/
static void findGemSpecTags (void)
{
	scheduleRunningBaseparser (0);
}

static int lineNotify (rubySubparser *s, const unsigned char **cp)
{
	struct sGemSpecSubparser *gemspec = (struct sGemSpecSubparser *)s;
	const unsigned char *p = *cp;

	if (vStringLength (gemspec->var_name) > 0
		&& (strncmp ((char *)p, vStringValue (gemspec->var_name),
					 vStringLength (gemspec->var_name)) == 0))
	{
		int kind = K_GEM;
		int role = ROLE_DEFINITION_INDEX;
		bool is_attr = true;

		p += vStringLength (gemspec->var_name);

		if (strncmp ((const char *)p, "name", 4) == 0)
			p += 4;
		else if (strncmp ((const char *)p, "add_runtime_dependency", 22) == 0)
		{
			p += 22;
			role = R_GEM_RUNTIME_DEP;
			is_attr = false;
		}
		else if (strncmp ((const char *)p, "add_development_dependency", 26) == 0)
		{
			p += 26;
			role = R_GEM_DEVEL_DEP;
			is_attr = false;
		}
		else
			p = NULL;

		if (p)
		{
			rubySkipWhitespace (&p);
			if ((!is_attr) || *p == '=')
			{
				if (is_attr)
				{
					p++;
					rubySkipWhitespace (&p);
				}
				if (*p == '"' || *p == '\'')
				{
					unsigned char b = *p;
					vString *gem = vStringNew ();
					p++;
					if (rubyParseString (&p, b, gem))
					{
						if (role == ROLE_DEFINITION_INDEX)
							makeSimpleTag (gem, kind);
						else
							makeSimpleRefTag (gem, kind, role);
					}
					vStringDelete (gem);
				}
			}
		}
	}
	else if (rubyCanMatchKeywordWithAssign (&p, "Gem::Specification.new"))
	{
		vString *vstr = vStringNew ();
		bool curly_bracket = false;

		rubySkipWhitespace (&p);

		curly_bracket = (*p == '{');
		if (curly_bracket
			|| (rubyParseMethodName (&p, vstr)
				&& vStringLength (vstr) == 2
				&& strcmp (vStringValue(vstr), "do") == 0))
		{
			if (curly_bracket)
				p++;

			rubySkipWhitespace (&p);
			if (*p == '|')
			{
				p++;
				rubySkipWhitespace (&p);
				vStringClear (vstr);
				if (rubyParseMethodName (&p, vstr))
				{
					vStringPut (vstr, '.');
					vStringCopy(gemspec->var_name, vstr);
				}
			}
		}
		vStringDelete (vstr);
	}
	return CORK_NIL;
}

static void inputStart (subparser *s)
{
	struct sGemSpecSubparser *gemspec = (struct sGemSpecSubparser *)s;

	gemspec->var_name = vStringNew ();
}

static void inputEnd (subparser *s)
{
	struct sGemSpecSubparser *gemspec = (struct sGemSpecSubparser *)s;

	vStringDelete (gemspec->var_name); /* NULL is acceptable. */
	gemspec->var_name = NULL;
}

extern parserDefinition* GemSpecParser (void)
{
	static const char *const extensions [] = { "gemspec", NULL };
	static struct sGemSpecSubparser gemspecSubparser = {
		.ruby = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
				.inputStart = inputStart,
				.inputEnd = inputEnd,
			},
			.lineNotify = lineNotify,
		},
		.var_name = NULL,
	};

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Ruby", &gemspecSubparser },
	};

	parserDefinition* const def = parserNew ("GemSpec");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable  = GemSpecKinds;
	def->kindCount  = ARRAY_SIZE (GemSpecKinds);
	def->extensions = extensions;
	def->parser     = findGemSpecTags;
	def->useCork    = CORK_QUEUE;

	return def;
}
