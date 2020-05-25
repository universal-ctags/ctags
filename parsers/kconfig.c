/*
 *   Copyright (c) 2020, Maxime Chretien <maxime.chretien@bootlin.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for the Kconfig language
 *
 *   Reference
 *      https://www.kernel.org/doc/html/latest/kbuild/kconfig-language.html
 */

/*
*   INCLUDE FILES
*/
#include "general.h"  /* always include first */
#include "parse.h"    /* always include */
#include "routines.h"
#include "selectors.h"

static tagRegexTable kconfigTagRegexTable [] = {
	{"^\\s*(menu)?config\\s+([A-Za-z0-9_]+)\\s*$", "\\2",
	 "c,config,configs", NULL},
};

/*
*   FUNCTION DEFINITIONS
*/

extern parserDefinition* KconfigParser (void)
{
	static const char *const patterns [] = { "Kconfig*", NULL };

	parserDefinition* const def = parserNew ("Kconfig");
	def->patterns   = patterns;
	def->tagRegexTable = kconfigTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (kconfigTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}
