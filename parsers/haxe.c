/*
 *       Copyright (c) 2007, Ritchie Turner
 *
 *       This source code is released for free distribution under the terms of the
 *       GNU General Public License version 2 or (at your opinion) any later version.
 *
 *       This module contains functions for generating tags for Haxe files
 *       (https://en.wikipedia.org/wiki/Haxe).
 *
 *       Borrowed from PHP.
 */

/*
 *       INCLUDE FILES
 */
#include "general.h"    /* must always come first */
#include <ctype.h>      /* to define isalpha () */
#ifdef DEBUG
#include <stdio.h>
#endif
#include <string.h>
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"

/*
 *       MACROS
 */
#define isType(token,t)         (bool) ((token)->type == (t))
#define isKeyword(token,k)      (bool) ((token)->keyword == (k))

/*
 *      DATA DEFINITIONS
 */

/*static jmp_buf Exception;*/

typedef enum {
	HXTAG_METHODS,
	HXTAG_CLASS,
	HXTAG_ENUM,
	HXTAG_VARIABLE,
	HXTAG_INTERFACE,
	HXTAG_TYPEDEF,
	HXTAG_COUNT
} hxKind;

static kindDefinition HxKinds [] = {
	{ true,  'm', "method",         "methods" },
	{ true,  'c', "class",          "classes" },
	{ true,  'e', "enum",           "enumerations" },
	{ true,  'v', "variable",       "variables" },
	{ true,  'i', "interface",      "interfaces" },
	{ true,  't', "typedef",        "typedefs" },
};

static void findHxTags (void)
{
	vString *name = vStringNew ();
	vString *clsName = vStringNew();
	vString *scope2 = vStringNew();
	vString *laccess = vStringNew();
	const char *const priv = "private";
	const char *const pub = "public";

	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char *cp = line;
another:
		while (isspace (*cp))
			cp++;

		vStringCopyS(laccess,priv);

		if (strncmp ((const char*) cp, "var", (size_t) 3) == 0  &&
			isspace ((int) cp [3]))
		{
			cp += 3;

			while (isspace ((int) *cp))
				++cp;

			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, HXTAG_VARIABLE);

			vStringClear (name);
		}
		else if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  &&
			isspace ((int) cp [8]))
		{
			cp += 8;

			while (isspace ((int) *cp))
				++cp;

			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, HXTAG_METHODS);

			vStringClear (name);
		}
		else if (strncmp ((const char*) cp, "class", (size_t) 5) == 0 &&
				 isspace ((int) cp [5]))
		{
			cp += 5;

			while (isspace ((int) *cp))
				++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, HXTAG_CLASS);
			vStringCopy(clsName,name);
			vStringClear (name);
		}
		else if (strncmp ((const char*) cp, "enum", (size_t) 4) == 0 &&
				  isspace ((int) cp [4]))
		{
			cp += 4;

			while (isspace ((int) *cp))
				++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, HXTAG_ENUM);
			vStringClear (name);
		} else if (strncmp ((const char*) cp, "public", (size_t) 6) == 0 &&
				 isspace((int) cp [6]))
		{
			cp += 6;
			while (isspace ((int) *cp))
				++cp;
			vStringCopyS(laccess,pub);
			goto another;
		} else if (strncmp ((const char*) cp, "static", (size_t) 6) == 0 &&
				 isspace((int) cp [6]))
		{
			cp += 6;
			while (isspace ((int) *cp))
				++cp;
			goto another;
		} else if (strncmp ((const char*) cp, "interface", (size_t) 9) == 0 &&
			isspace((int) cp [9]))
		{
			cp += 9;

			while (isspace ((int) *cp))
				++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_') {
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, HXTAG_INTERFACE);
			vStringClear (name);
		} else if (strncmp ((const char *) cp,"typedef",(size_t) 7) == 0 && isspace(((int) cp[7]))) {
			cp += 7;

			while (isspace ((int) *cp))
				++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_') {
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, HXTAG_TYPEDEF);
			vStringClear (name);
		}


	}

	vStringDelete(name);
	vStringDelete(clsName);
	vStringDelete(scope2);
	vStringDelete(laccess);
}


/* Create parser definition structure */
extern parserDefinition* HaxeParser (void)
{
	static const char *const extensions [] = { "hx", NULL };

	parserDefinition *const def = parserNew ("Haxe");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kindTable          = HxKinds;
	def->kindCount  = ARRAY_SIZE (HxKinds);
	def->parser             = findHxTags;
	/*def->initialize = initialize;*/
	return def;
}
