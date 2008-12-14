/*
*   $Id: yacc.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2001-2002, Nick Hibma <n_hibma@van-laarhoven.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for YACC language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"

/*
*   FUNCTION DEFINITIONS
*/

static void installDosBatchRegex (const langType language)
{
	addTagRegex (language,
		"^:([A-Za-z_0-9]+)", "\\1", "l,label,labels", NULL);
	addTagRegex (language,
		"set[ \t]+([A-Za-z_0-9]+)[ \t]*=", "\\1", "v,variable,variables", NULL);
}

extern parserDefinition* DosBatchParser ()
{
	static const char *const extensions [] = { "bat", "cmd", NULL };
	parserDefinition* const def = parserNew ("DosBatch");
	def->extensions = extensions;
	def->initialize = installDosBatchRegex;
	def->regex      = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
