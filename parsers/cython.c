/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2014-2016, Colomban Wendling <ban@herbesfolles.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Cython language
*   files.
*
*   References:
*   - https://cython.readthedocs.io/en/latest/src/userguide/language_basics.html
*/

#include "general.h"  /* must always come first */

extern parserDefinition* CythonParser (void)
{
	static const char *const extensions[] = { "pyx", "pxd", "pxi", NULL };
	parserDefinition *def = parserNew ("Cython");
	def->kindTable = CythonKinds;
	def->kindCount = ARRAY_SIZE (CythonKinds);
	def->extensions = extensions;
	def->parser = findCythonTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->keywordTable = CythonKeywordTable;
	def->keywordCount = ARRAY_SIZE (CythonKeywordTable);
	def->fieldTable = CythonFields;
	def->fieldCount = ARRAY_SIZE (CythonFields);
	def->useCork = CORK_QUEUE;
	def->requestAutomaticFQTag = true;
	return def;
}
