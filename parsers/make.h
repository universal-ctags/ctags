/*
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for makefiles.
*/

#ifndef CTAGS_PARSER_MAKE_H
#define CTAGS_PARSER_MAKE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"
#include "vstring.h"

typedef struct sMakeSubparser makeSubparser;

struct sMakeSubparser {
	subparser subparser;

	void (* valueNotify) (makeSubparser *s, char* name);
	void (* directiveNotify) (makeSubparser *s, char* name);
	void (* newMacroNotify) (makeSubparser *s,
							 char* name,
							 bool withDefineDirective,
							 bool appending);
};

#endif
