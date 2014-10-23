/*
*   $Id$
*
*   Copyright (c) 2008, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for MATLAB language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"

static unsigned char matlab_m_tg_table [65536] =
{
	[    2597] =       55, /* N % */
	[    2600] =        9, /* N ( */
	[    2606] =        2, /* N . */
	[    2620] =        3, /* N < */
	[    2621] =       11, /* N = */
	[    2622] =        3, /* N > */
	[    2651] =        2, /* N [ */
	[    9482] =       29, /* % N */
	[    9512] =        2, /* % ( */
	[    9516] =        3, /* % , */
	[    9518] =        6, /* % . */
	[    9530] =        2, /* % : */
	[    9533] =       12, /* % = */
	[    9534] =        1, /* % > */
	[    9566] =        1, /* % ^ */
	[    9994] =        3, /* ' N */
	[   10023] =        1, /* ' ' */
	[   10024] =        1, /* ' ( */
	[   10025] =        6, /* ' ) */
	[   10030] =        1, /* ' . */
	[   10279] =        6, /* ( ' */
	[   10281] =        5, /* ( ) */
	[   10283] =        1, /* ( + */
	[   10284] =        3, /* ( , */
	[   10285] =        3, /* ( - */
	[   10288] =        4, /* ( 0 */
	[   10506] =        8, /* ) N */
	[   10539] =        1, /* ) + */
	[   10540] =        3, /* ) , */
	[   10542] =        2, /* ) . */
	[   10555] =        3, /* ) ; */
	[   10556] =        3, /* ) < */
	[   10588] =        1, /* ) \ */
	[   10622] =        1, /* ) ~ */
	[   10762] =        1, /* * N */
	[   10793] =        1, /* * ) */
	[   10798] =        1, /* * . */
	[   10811] =        3, /* * ; */
	[   10846] =        1, /* * ^ */
	[   11050] =        1, /* + * */
	[   11052] =        1, /* + , */
	[   11057] =        1, /* + 1 */
	[   11067] =        2, /* + ; */
	[   11102] =        2, /* + ^ */
	[   11274] =       20, /* , N */
	[   11305] =        3, /* , ) */
	[   11308] =        8, /* , , */
	[   11310] =        1, /* , . */
	[   11313] =        5, /* , 1 */
	[   11324] =        1, /* , < */
	[   11357] =        2, /* , ] */
	[   11561] =        3, /* - ) */
	[   11786] =       10, /* . N */
	[   11815] =        5, /* . ' */
	[   11816] =        1, /* . ( */
	[   11822] =        6, /* . . */
	[   11823] =        1, /* . / */
	[   11837] =        1, /* . = */
	[   12078] =        2, /* / . */
	[   12079] =        3, /* / / */
	[   12081] =        1, /* / 1 */
	[   12158] =        1, /* / ~ */
	[   12332] =        8, /* 0 , */
	[   12412] =        2, /* 0 | */
	[   12554] =        1, /* 1 N */
	[   12585] =        4, /* 1 ) */
	[   12588] =        3, /* 1 , */
	[   12591] =        1, /* 1 / */
	[   12603] =        2, /* 1 ; */
	[   14858] =        1, /* : N */
	[   14895] =        1, /* : / */
	[   15114] =       10, /* ; N */
	[   15148] =        1, /* ; , */
	[   15408] =        1, /* < 0 */
	[   15421] =        6, /* < = */
	[   15626] =        3, /* = N */
	[   15656] =        8, /* = ( */
	[   15658] =        3, /* = * */
	[   15659] =        4, /* = + */
	[   15660] =        8, /* = , */
	[   15662] =        3, /* = . */
	[   15664] =        5, /* = 0 */
	[   15665] =        4, /* = 1 */
	[   15675] =        1, /* = ; */
	[   15915] =        1, /* > + */
	[   15933] =        5, /* > = */
	[   23340] =        2, /* [ , */
	[   23592] =        1, /* \ ( */
	[   23869] =        2, /* ] = */
	[   24106] =        3, /* ^ * */
	[   24125] =        1, /* ^ = */
	[   31806] =        2, /* | > */
	[   32303] =        1, /* ~ / */
	[   32317] =        1, /* ~ = */
};

/*
*   FUNCTION DEFINITIONS
*/

static void installMatLabRegex (const langType language)
{
    /* function [x,y,z] = asdf */
    addTagRegex (language, "^function[ \t]*\\[.*\\][ \t]*=[ \t]*([a-zA-Z0-9_]+)", "\\1", "f,function", NULL);
    /* function x = asdf */
    addTagRegex (language, "^function[ \t]*[a-zA-Z0-9_]+[ \t]*=[ \t]*([a-zA-Z0-9_]+)", "\\1", "f,function", NULL);
    /* function asdf */
    addTagRegex (language, "^function[ \t]*([a-zA-Z0-9_]+)[^=]*$", "\\1", "f,function", NULL);

    addTgEntryForExtension (language, "m", matlab_m_tg_table);
}

extern parserDefinition* MatLabParser (void)
{
	static const char *const extensions [] = { "m", NULL };
	parserDefinition* const def = parserNew ("MatLab");
	def->extensions = extensions;
	def->initialize = installMatLabRegex;
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
