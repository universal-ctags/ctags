/*
 *   Copyright (c) 2000-2001, Jérôme Plût
 *   Copyright (c) 2006, Enrico Tröger
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains functions for generating tags for source files
 *   for docbook files(based on TeX parser from Jérôme Plût).
 */

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_CHAPTER,
	K_SECTION,
	K_SECT1,
	K_SECT2,
	K_SECT3,
	K_APPENDIX
} docbookKind;

static kindDefinition DocBookKinds [] = {
	{ true,  'f', "function",   "chapters"},
	{ true,  'c', "class",      "sections"},
	{ true,  'm', "member",     "sect1"},
	{ true,  'd', "macro",      "sect2"},
	{ true,  'v', "variable",   "sect3"},
	{ true,  's', "struct",     "appendix"}
};

/*
*   FUNCTION DEFINITIONS
*/

static int getWord(const char *ref, const char **ptr)
{
	const char *p = *ptr;

	while ((*ref != '\0') && (*p != '\0') && (*ref == *p)) ref++, p++;

	if (*ref) return false;

	*ptr = p;
	return true;
}


static void createTag(docbookKind kind, const char *buf)
{
	vString *name;

	if (*buf == '>') return;

	buf = strstr(buf, "id=\"");
	if (buf == NULL) return;
	buf += 4;
	if (*buf == '"') return;
	name = vStringNew();

	do
	{
		vStringPut(name, (int) *buf);
		++buf;
	} while ((*buf != '\0') && (*buf != '"'));
	makeSimpleTag(name, kind);
}


static void findDocBookTags(void)
{
	const char *line;

	while ((line = (const char*)readLineFromInputFile()) != NULL)
	{
		const char *cp = line;

		for (; *cp != '\0'; cp++)
		{
			if (*cp == '<')
			{
				cp++;

				/* <section id="..."> */
				if (getWord("section", &cp))
				{
					createTag(K_SECTION, cp);
					continue;
				}
				/* <sect1 id="..."> */
				if (getWord("sect1", &cp))
				{
					createTag(K_SECT1, cp);
					continue;
				}
				/* <sect2 id="..."> */
				if (getWord("sect2", &cp))
				{
					createTag(K_SECT2, cp);
					continue;
				}
				/* <sect3 id="..."> */
				if (getWord("sect3", &cp) ||
					getWord("sect4", &cp) ||
					getWord("sect5", &cp))
				{
					createTag(K_SECT3, cp);
					continue;
				}
				/* <chapter id="..."> */
				if (getWord("chapter", &cp))
				{
					createTag(K_CHAPTER, cp);
					continue;
				}
				/* <appendix id="..."> */
				if (getWord("appendix", &cp))
				{
					createTag(K_APPENDIX, cp);
					continue;
				}
			}
		}
	}
}

extern parserDefinition* DocBookParser (void)
{
	static const char *const extensions [] = { "sgml", "docbook", NULL };
	parserDefinition* def = parserNew ("Docbook");
	def->extensions = extensions;
	def->kindTable  = DocBookKinds;
	def->kindCount  = ARRAY_SIZE (DocBookKinds);
	def->parser     = findDocBookTags;
	return def;
}

