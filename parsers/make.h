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
#include "vstring.h"

struct makeParserClient {
	void (* valuesFound) (struct makeParserClient*, vString *name, void *data);
	void (* directiveFound) (struct makeParserClient*, vString *name, void *data);
	void (* newMacro) (struct makeParserClient*,
			   vString *const name,
			   bool withDefineDirective,
			   bool appending, void *data);
};

extern void runMakeParser (struct makeParserClient *client, void *data);

#endif
