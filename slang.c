/*
 *   $Id$
 *
 *   Copyright (c) 2000-2001, Francesc Rocher
 *
 *   Author: Francesc Rocher <f.rocher@computer.org>.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains functions for generating tags for S-Lang files.
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"
#include "parse.h"

/*
 *   FUNCTION DEFINITIONS
 */
static void installSlangRegex (const langType language)
{
   addLanguageRegex (language, "/^.*define[ \t]+([A-Z_][A-Z0-9_]*)[^;]*$/\\1/f,function/i");
   addLanguageRegex (language, "/^[ \t]*implements[ \t]+\\([ \t]*\"([^\"]*)\"[ \t]*\\)[ \t]*;/\\1/n,namespace/");
}

extern parserDefinition* SlangParser (void)
{
   static const char *const extensions [] = { "sl", NULL };
   parserDefinition* const def = parserNew ("SLang");
   def->extensions = extensions;
   def->initialize = installSlangRegex;
   def->regex      = TRUE;
   return def;
}
