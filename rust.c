/*
*   $Id$
*
*   Copyright (c) 2014, Passw 
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Rust language files.
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

static void installRustRegex (const langType language)
{
	addTagRegex (language,"^[ \t]*(#\[[^\]]\][ \t]*)*(pub[ \t]+)?(extern[ \t]+)?(\"[^\"]+\"[ \t]+)?(unsafe[ \t]+)?fn[ \t]+([a-zA-Z0-9_]+)", "\\6", "f,functions,function definitions", NULL);
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?type[ \t]+([a-zA-Z0-9_]+)", "\\2", "T,types,type definitions", NULL);  
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?enum[ \t]+([a-zA-Z0-9_]+)", "\\2", "g,enum,enumeration names", NULL);
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?struct[ \t]+([a-zA-Z0-9_]+)", "\\2", "s,structure names", NULL);
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?mod[ \t]+([a-zA-Z0-9_]+)", "\\2", "m,modules,module names", NULL);
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?static[ \t]+([a-zA-Z0-9_]+)", "\\2", "c,consts,static constants", NULL);
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?trait[ \t]+([a-zA-Z0-9_]+)", "\\2", "t,traits,traits", NULL);
	addTagRegex (language, "^[ \t]*(pub[ \t]+)?impl([ \t\n]*<[^>]*>)?[ \t]+(([a-zA-Z0-9_:]+)[ \t]*(<[^>]*>)?[ \t]+(for)[ \t]+)?([a-zA-Z0-9_]+)", "\\4 \\6 \\7", "i,impls,trait implementations", NULL);
	addTagRegex (language, "^[ \t]*macro_rules![ \t]+([a-zA-Z0-9_]+)", "\\1", "d,macros,macro definitions", NULL);
}

extern parserDefinition* RustParser ()
{
	static const char *const extensions [] = { "rs", NULL };
	parserDefinition* const def = parserNew ("Rust");
	def->extensions = extensions;
	def->initialize = installRustRegex;
	def->regex      = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
