/*
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains functions for generating tags for source files
 *   for inp files (Abaqus).
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
	K_PART,
	K_ASSEMBLY,
	K_STEP
} AbaqusKind;

static kindDefinition AbaqusKinds[] = {
     { true, 'c', "class",      "Parts" },
     { true, 'm', "member",      "Assembly" },
     { true, 'n', "namespace",      "Steps" }
};

/*
*   FUNCTION DEFINITIONS
*/

static int getWord(const char *ref, const char **ptr)
{
	const char *p = *ptr;

	while ((*ref != '\0') && (*p != '\0') && (tolower(*ref) == tolower(*p))) ref++, p++;

	if (*ref) return false;

	*ptr = p;
	return true;
}


static void createTag(AbaqusKind kind, const char *buf)
{
	vString *name;

	if (*buf == '\0') return;

	buf = strstr(buf, "=");
	if (buf == NULL) return;

	buf += 1;

	if (*buf == '\0') return;

	name = vStringNew();

	do
	{
		vStringPut(name, (int) *buf);
		++buf;
	} while ((*buf != '\0') && (*buf != ','));
	makeSimpleTag(name, kind);
	vStringDelete(name);
}


static void findAbaqusTags(void)
{
	const char *line;

	while ((line = (const char*)readLineFromInputFile()) != NULL)
	{
		const char *cp = line;

		for (; *cp != '\0'; cp++)
		{
			if (*cp == '*')
			{
				cp++;

				/* Parts*/
				if (getWord("part", &cp))
				{
					createTag(K_PART, cp);
					continue;
				}
				/* Assembly */
				if (getWord("assembly", &cp))
				{
					createTag(K_ASSEMBLY, cp);
					continue;
				}
				/* Steps */
				if (getWord("step", &cp))
				{
					createTag(K_STEP, cp);
					continue;
				}
			}
		}
	}
}


extern parserDefinition* AbaqusParser (void)
{
	static const char *const extensions [] = { "inp", NULL };
	parserDefinition * def = parserNew ("Abaqus");
	def->kindTable  = AbaqusKinds;
	def->kindCount  = ARRAY_SIZE (AbaqusKinds);
	def->extensions = extensions;
	def->parser     = findAbaqusTags;
	return def;
}
