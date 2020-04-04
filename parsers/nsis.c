/*
*   Copyright (c) 2000-2002, Darren Hiebert
*   Copyright (c) 2009-2011, Enrico Tröger
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for NSIS scripts
*   (https://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System).
*
*   Based on sh.c.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_SECTION,
	K_FUNCTION,
	K_VARIABLE,
	K_DEFINITION,
	K_MACRO,
	K_SECTION_GROUP,
	K_MACRO_PARAM,
	K_LANGSTR,
	K_SCRIPT,
} NsisKind;

typedef enum {
	NSIS_SCRIPT_INCLUDED,
} nsisScriptRole;

static roleDefinition NsisScriptRoles [] = {
	{ true, "included",  "included with !include" },
};

static kindDefinition NsisKinds [] = {
	{ true, 's', "section", "sections"},
	{ true, 'f', "function", "functions"},
	{ true, 'v', "variable", "variables"},
	{ true, 'd', "definition", "definitions"},
	{ true, 'm', "macro", "macros"},
	{ true, 'S', "sectionGroup", "section groups"},
	{ false, 'p', "macroparam", "macro parameters"},
	{ true, 'l', "langstr", "language strings"},
	{ true, 'i', "script", "NSIS scripts",
	  .referenceOnly = true, ATTACH_ROLES(NsisScriptRoles)},
};

typedef enum {
	F_LANGID,
} nsisField;

static fieldDefinition NsisFields[] = {
	{ .name = "langid",
	  .description = "language identifier specified in (License)LangString commands",
	  .enabled = true },
};

/*
*   FUNCTION DEFINITIONS
*/

static const unsigned char* skipWhitespace (const unsigned char* cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

static const unsigned char* skipFlags (const unsigned char* cp)
{
	while (*cp == '/')
	{
		++cp;
		while (! isspace ((int) *cp))
			++cp;
		while (isspace ((int) *cp))
			++cp;
	}
	return cp;
}

static int makeSimpleTagWithScope(vString *name, int kindIndex, int parentCorkIndex)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kindIndex);
	e.extensionFields.scopeIndex = parentCorkIndex;
	return makeTagEntry (&e);
}

#define lineStartingWith(CP,EXPECTED,EOL)								\
	(strncasecmp ((const char*) CP, EXPECTED, strlen(EXPECTED)) == 0	\
		&& (EOL ? (isspace ((int) CP [strlen(EXPECTED)]) || CP [strlen(EXPECTED)] == '\0') \
			: isspace ((int) CP [strlen(EXPECTED)])))

#define fillName(NAME,CP,CONDITION)				\
	while (CONDITION)							\
	{											\
		vStringPut ((NAME), (int) *(CP));		\
		++(CP);									\
	}											\
	do {} while (0)

static const unsigned char* parseSection (const unsigned char* cp, vString *name,
										  int kindIndex, int scopeIndex, int *corkIndex)
{
	cp = skipWhitespace (cp);
	cp = skipFlags (cp);
	cp = skipWhitespace (cp);

	if (corkIndex)
		*corkIndex = CORK_NIL;

	if (strpbrk((const char *)cp, "'`\""))
	{
		const unsigned char terminator = *cp;

		cp++;
		if (*cp == terminator)
		{
			/* An empty section.
			 * See https://nsis.sourceforge.io/Docs/Chapter4.html#sectionsettext
			 */
			anonGenerate (name,
						  (kindIndex == K_SECTION
						   ? "AnonymousSection"
						   : "AnonymousSectionGroup"),
						  kindIndex);
			cp++;
		}
		else if (*cp == '\0')
			return cp;
		else
		{
			int in_escape = 0;
			do
			{
				vStringPut (name, (int) *cp);
				++cp;

				if (*cp == '\0')
					break;

				/*
				 * Ignore `"' in `$\"' as the terminator of quotation.
				 */
				if (*cp == '$' && in_escape == 0)
					in_escape++;
				else if (*cp == '\\' && in_escape == 1)
					in_escape++;
				else if (*cp == terminator && in_escape == 2)
					/*
					 * This `"' is not a terminator of quotation;
					 * set in_escape to 3.
					 */
					in_escape++;
				else
					in_escape = 0;

				if ((in_escape != 3) && *cp == terminator)
				{
					++cp;
					break;
				}
			}
			while (1);
		}
	}
	else
	{
		while (isalnum ((int) *cp)
			   || *cp == '_' || *cp == '-' || *cp == '.' || *cp == '!'
			   || *cp == '$' || *cp == '{' || *cp == '}' || *cp == '(' || *cp == ')')
		{
			vStringPut (name, (int) *cp);
			++cp;
		}
	}
	int r = makeSimpleTagWithScope (name, kindIndex, scopeIndex);
	if (corkIndex)
		*corkIndex = r;
	if (vStringLength (name) > 0)
	{
		/*
		 * Try to capture section_index_output.
		 */
		vStringClear (name);
		cp = skipWhitespace (cp);

		fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

		if (vStringLength (name) > 0)
		{
			makeSimpleTag (name, K_DEFINITION);
			vStringClear (name);
		}
	}
	return cp;
}

static const unsigned char* parseLangString (const unsigned char* cp, vString *name)
{
	cp = skipWhitespace (cp);

	/* `^' is not explained in the nsis reference manual. However, it is used
	 * in gvim.
	 * e.g.
	 * https://github.com/vim/vim/blob/3dabd718f4b2d8e09de9e2ec73832620b91c2f79/nsis/lang/english.nsi
	 */
	fillName (name, cp, (isalnum ((int) *cp) || *cp == '_' || *cp == '^'));

	if (vStringLength (name) > 0)
	{
		int r = makeSimpleTag (name, K_LANGSTR);
		if (r == CORK_NIL)
			goto out;
		vStringClear (name);

		cp = skipWhitespace (cp);
		fillName (name, cp, ((*cp != '\0') && (!isspace ((int) *cp))));
		if (vStringLength (name) > 0)
		{
			attachParserFieldToCorkEntry (r, NsisFields[F_LANGID].ftype,
										  vStringValue (name));
			vStringClear (name);
		}
	}
 out:
	return cp;
}

static void findNsisTags (void)
{
	int sectionGroupIndex = CORK_NIL;
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;

		while (isspace (*cp))
			cp++;

		if (*cp == '#' || *cp == ';')
			continue;

		/* functions */
		if (lineStartingWith (cp, "function", false))
		{
			cp += 8;
			cp = skipWhitespace (cp);

			fillName (name, cp,
					  (isalnum ((int) *cp) || *cp == '_' || *cp == '-' || *cp == '.' || *cp == '!'));

			makeSimpleTag (name, K_FUNCTION);
			vStringClear (name);
		}
		/* variables */
		else if (lineStartingWith (cp, "var", false))
		{
			cp += 3;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

			makeSimpleTag (name, K_VARIABLE);
			vStringClear (name);
		}
		/* section groups */
		else if (lineStartingWith (cp, "sectiongroup", false))
		{
			cp += 12;
			cp = parseSection (cp, name, K_SECTION_GROUP, CORK_NIL, &sectionGroupIndex);
		}
		else if (lineStartingWith (cp, "sectiongroupend", true))
		{
			cp += 15;
			sectionGroupIndex = CORK_NIL;
		}
		/* sections */
		else if (lineStartingWith (cp, "section", false))
		{
			cp += 7;
			cp = parseSection (cp, name, K_SECTION, sectionGroupIndex, NULL);
		}
		/* LangString */
		else if (lineStartingWith (cp, "langstring", false))
		{
			cp += 10;
			cp = parseLangString (cp, name);
		}
		/* LicenseLangString */
		else if (lineStartingWith (cp, "licenselangstring", false))
		{
			cp += 17;
			cp = parseLangString (cp, name);
		}
		/* definitions */
		else if (lineStartingWith (cp, "!define", false))
		{
			cp += 7;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

			makeSimpleTag (name, K_DEFINITION);
			vStringClear (name);
		}
		/* macro */
		else if (lineStartingWith (cp, "!macro", false))
		{
			cp += 6;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

			int index = makeSimpleTag (name, K_MACRO);
			if (vStringLength (name) > 0)
			{
				while (1)
				{
					vStringClear (name);
					cp = skipWhitespace (cp);
					fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));
					if (vStringLength (name) == 0)
						break;
					makeSimpleTagWithScope (name, K_MACRO_PARAM, index);
				}
			}
		}
		/* include */
		else if (lineStartingWith (cp, "!include", false))
		{
			cp += 8;

			/* !include [/NONFATAL] [/CHARSET=ACP|OEM|CP#|UTF8|UTF16LE|UTF16BE] file */
			cp = skipWhitespace (cp);

			/* /NONFATAL */
			cp = skipFlags (cp);
			cp = skipWhitespace (cp);

			/* /CHARSET */
			cp = skipFlags (cp);
			cp = skipWhitespace (cp);

			fillName (name, cp, (*cp != '\0' && *cp != ';' && *cp != '#'));
			vStringStripTrailing (name);

			if (vStringLength (name) > 0)
			{
				makeSimpleRefTag (name, K_SCRIPT, NSIS_SCRIPT_INCLUDED);
				vStringClear (name);
			}
			/* TODO: capture !addincludedir */
		}
	}
	vStringDelete (name);
}

extern parserDefinition* NsisParser (void)
{
	static const char *const extensions [] = {
		"nsi", "nsh", NULL
	};
	parserDefinition* def = parserNew ("NSIS");
	def->kindTable  = NsisKinds;
	def->kindCount  = ARRAY_SIZE (NsisKinds);
	def->extensions = extensions;
	def->fieldTable = NsisFields;
	def->fieldCount = ARRAY_SIZE (NsisFields);
	def->parser     = findNsisTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
