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

#include "debug.h"
#include "htable.h"
#include "iniconf.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"


static void makeIniconfTagMaybe (langType iniconf, const char *section, const char *key, const char *value, void *userData);

static boolean isIdentifier (int c)
{
    /* allow whitespace within keys and sections */
    return (boolean)(isalnum (c) || isspace (c) ||  c == '_');
}

static boolean isValue (int c)
{
	return (c != '\0');
}

static hashTable *iniconfParserClients;
extern void registerIniconfParserClient (struct iniconfParserClient *client)
{
	if (!iniconfParserClients)
		iniconfParserClients = hashTableNew (5, hashInthash, hashInteq,
						     NULL, NULL);
	hashTablePutItem (iniconfParserClients, &client->lang, client);
}


struct probeData {
	struct iniconfParserClient *client;
	const char *section, *key, *value;

};

static void mayRunProbe (void *key CTAGS_ATTR_UNUSED, void *value, void *user_data)
{
	struct probeData *probe_data = user_data;
	struct iniconfParserClient *client = value;

	if (probe_data->client)
		return;

	if (client->probeLanguage && client->probeLanguage (probe_data->section,
							    probe_data->key,
							    probe_data->value))
		probe_data->client = client;
}

static struct iniconfParserClient *maySwitchLanguage (const char *section, const char *key, const char *value)
{
	struct probeData probe_data = {
		.client = NULL,
		.section = section, .key = key, .value = value };

	hashTableForeachItem (iniconfParserClients, mayRunProbe, &probe_data);
	if (probe_data.client)
	{
		probe_data.client->data = probe_data.client->prepareForNewInput ();
		pushLanguage (probe_data.client->lang);
	}
	return probe_data.client;
}

extern void runIniconfParser (const iniconfCallback callback, void* userData)
{
	iniconfCallback cb = callback;
	static langType iniconf = LANG_IGNORE;
	int sectionCorkIndex = CORK_NIL;

	vString *val   = vStringNew ();
	vString *name  = vStringNew ();
	vString *scope = vStringNew ();
	const unsigned char *line;

	if (iniconf == LANG_IGNORE)
		iniconf = getNamedLanguage ("Iniconf", 0);

	initializeParser (iniconf);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;
		boolean possible = TRUE;

		if (isspace ((int) *cp) || *cp == '#' || *cp == ';'
		    || (*cp != '\0' && *cp == '/' && *(cp+1) == '/'))
			continue;

		/* look for a section */
		if (*cp != '\0' && *cp == '[')
		{
			++cp;
			while (*cp != '\0' && *cp != ']')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			vStringTerminate (name);

			if (isLanguageEnabled (iniconf))
				makeIniconfTagMaybe (iniconf, vStringValue (name), NULL, NULL,
						     &sectionCorkIndex);

			if (!cb)
			{
				struct iniconfParserClient *client;
				client = maySwitchLanguage (vStringValue (name), NULL, NULL);
				if (client)
				{
					cb = client->handleInputData;
					userData = client->data;
				}
			}
			if (cb)
				cb (vStringValue (name), NULL, NULL, userData);

			vStringCopy (scope, name);
			vStringTerminate (scope);
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
				vStringTerminate (name);
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
					vStringTerminate (val);
					vStringStripTrailing (val);

					if (isLanguageEnabled (iniconf))
						makeIniconfTagMaybe (iniconf,
								     (vStringLength (scope) > 0)
								     ? vStringValue (scope)
								     : NULL,
								     vStringValue (name),
								     vStringValue (val),
								     &sectionCorkIndex);
					if (!cb)
					{
						struct iniconfParserClient *client;
						client = maySwitchLanguage ((vStringLength (scope) > 0)
									    ? vStringValue (scope)
									    : NULL,
									    vStringValue (name),
									    vStringValue (val));
						if (client)
						{
							cb = client->handleInputData;
							userData = client->data;
						}
					}
					if (cb)
						cb ((vStringLength (scope) > 0)
							  ? vStringValue (scope)
							  : NULL,
							  vStringValue (name),
							  vStringValue (val),
							  userData);
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


typedef enum {
	K_SECTION,
	K_KEY,
} makeKind;

static kindOption IniconfKinds [] = {
	{ TRUE, 's', "section",  "sections"},
	{ TRUE, 'k', "key",      "keys"},
};

static void makeIniconfTagMaybe (langType iniconf, const char *section, const char *key, const char *value, void *userData)
{
	tagEntryInfo e;

	pushLanguage (iniconf);
	if (key)
	{
		initTagEntry (&e, key, &(IniconfKinds [K_KEY]));
		e.extensionFields.scopeIndex = *(int *)userData;
		makeTagEntry (&e);
	}
	else
	{
		if (*(int *)userData != CORK_NIL)
		{
			tagEntryInfo *last;

			last = getEntryInCorkQueue (*(int *)userData);
			if (last)
				last->extensionFields.endLine = getInputLineNumber ();
		}

		initTagEntry (&e, section, &(IniconfKinds [K_SECTION]));
		*(int *)userData = makeTagEntry (&e);
	}
	popLanguage ();
}

static void findIniconfTags (void)
{
	runIniconfParser (NULL, NULL);
}

extern parserDefinition* IniconfParser (void)
{
	static const char *const extensions [] = { "ini", "conf", NULL };
	parserDefinition* const def = parserNew ("Iniconf");

	def->kinds      = IniconfKinds;
	def->kindCount  = ARRAY_SIZE (IniconfKinds);
	def->extensions = extensions;
	def->parser     = findIniconfTags;
	def->useCork   = TRUE;

	return def;
}
