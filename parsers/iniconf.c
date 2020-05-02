/*
*
*   Copyright (c) 2000-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for ini/config files.
*/

/*
 *  This is based on geany's conf.c:
 * --------------------------------
 * commit 3af538fa65f8b17897259080db8144b1edc43470
 * Author: Enrico Tröger <enrico.troeger@uvena.de>
 * Date:   Sun Nov 27 20:39:57 2005 +0000
 *
 * added tag support for filetype Conf
 *
 *
 * git-svn-id: https://geany.svn.sourceforge.net/svnroot/geany/trunk@15 ea778897-0a13-0410-b9d1-a72fbfd435f5
 *
 */

#include "general.h"  /* must always come first */

#include "entry.h"
#include "htable.h"
#include "iniconf.h"
#include "parse.h"
#include "read.h"
#include "subparser.h"
#include "vstring.h"

static bool isIdentifier (int c)
{
    /* allow whitespace within keys and sections */
    return (bool)(isalnum (c) || isspace (c) ||  c == '_');
}

static bool isValue (int c)
{
	return (c != '\0');
}

static iniconfSubparser *maySwitchLanguage (const char *section, const char *key, const char *value)
{
	iniconfSubparser *iniconf_subparser = NULL;
	subparser *sub;

	foreachSubparser (sub, false)
	{
		iniconfSubparser *s = (iniconfSubparser *)sub;
		if ((sub->direction & SUBPARSER_BASE_RUNS_SUB)
			&& s->probeLanguage)
		{
			bool r;

			enterSubparser ((subparser *)s);
			r = s->probeLanguage(section, key, value);
			leaveSubparser ();
			if (r)
			{
				iniconf_subparser = s;
				chooseExclusiveSubparser (sub, NULL);
				break;
			}
		}
	}

	return iniconf_subparser;
}

typedef enum {
	K_SECTION,
	K_KEY,
} makeKind;

static kindDefinition IniconfKinds [] = {
	{ true, 's', "section",  "sections"},
	{ true, 'k', "key",      "keys"},
};

static void makeIniconfTagMaybe (const char *section, const char *key, const char *value CTAGS_ATTR_UNUSED, int *index)
{
	tagEntryInfo e;

	if (!isLanguageEnabled (getInputLanguage ()))
		return;

	if (key)
	{
		initTagEntry (&e, key, K_KEY);
		e.extensionFields.scopeIndex = *index;
		makeTagEntry (&e);
	}
	else
	{
		tagEntryInfo *last = getEntryInCorkQueue (*index);
		if (last)
			last->extensionFields.endLine = getInputLineNumber ();

		initTagEntry (&e, section, K_SECTION);
		*index = makeTagEntry (&e);
	}
}

static void findIniconfTags (void)
{
	const unsigned char *line;
	vString *val   = vStringNew ();
	vString *name  = vStringNew ();
	vString *scope = vStringNew ();
	iniconfSubparser *sub;
	int sectionCorkIndex = CORK_NIL;


	sub = (iniconfSubparser *)getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser ((subparser *)sub, NULL);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;
		bool possible = true;

		if (isspace ((int) *cp) || *cp == '#' || *cp == ';' || *cp == '\0'
		    || (*cp == '/' && *(cp+1) == '/'))
			continue;

		/* look for a section */
		if (*cp == '[')
		{
			++cp;
			while (*cp != '\0' && *cp != ']')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}

			makeIniconfTagMaybe (vStringValue (name), NULL, NULL,
								 &sectionCorkIndex);


			if (!sub)
				sub = maySwitchLanguage (vStringValue (name), NULL, NULL);

			if (sub)
			{
				enterSubparser((subparser *)sub);
				sub->newDataNotify (sub, vStringValue (name), NULL, NULL);
				leaveSubparser ();
			}

			vStringCopy (scope, name);
			vStringClear (name);
			continue;
		}

		while (*cp != '\0')
		{
			/*  We look for any sequence of identifier characters following a white space */
			if (possible && isIdentifier ((int) *cp))
			{
				while (isIdentifier ((int) *cp))
				{
					vStringPut (name, (int) *cp);
					++cp;
				}
				vStringStripTrailing (name);
				while (isspace ((int) *cp))
					++cp;
				if (*cp == '=' || *cp == ':')
				{

					cp++;
					while (isspace ((int) *cp))
						++cp;
					while (isValue ((int) *cp))
					{
						vStringPut (val, (int) *cp);
						++cp;
					}
					vStringStripTrailing (val);

					makeIniconfTagMaybe ((vStringLength (scope) > 0)
										 ? vStringValue (scope)
										 : NULL,
										 vStringValue (name),
										 vStringValue (val),
										 &sectionCorkIndex);
					if (!sub)
						sub = maySwitchLanguage ((vStringLength (scope) > 0)
												 ? vStringValue (scope)
												 : NULL,
												 vStringValue (name),
												 vStringValue (val));
					if (sub)
					{
						enterSubparser ((subparser *)sub);
						sub->newDataNotify (sub,
											(vStringLength (scope) > 0)
											? vStringValue (scope)
											: NULL,
											vStringValue (name),
											vStringValue (val));
						leaveSubparser ();
					}
					vStringClear (val);
				}
				vStringClear (name);
			}
			else
				possible = !!(isspace ((int) *cp));

			if (*cp != '\0')
				++cp;
		}
	}

	vStringDelete (name);
	vStringDelete (scope);
	vStringDelete (val);
}

extern parserDefinition* IniconfParser (void)
{
	static const char *const extensions [] = { "ini", "conf", NULL };
	parserDefinition* const def = parserNew ("Iniconf");

	def->kindTable      = IniconfKinds;
	def->kindCount  = ARRAY_SIZE (IniconfKinds);
	def->extensions = extensions;
	def->parser     = findIniconfTags;
	def->useCork   = CORK_QUEUE;

	return def;
}
