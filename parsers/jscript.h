/*
*   Copyright (c) 2017, Masatake YAMATO <yamato@redhat.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Interface for JavaScript parser
*/

#ifndef CTAGS_PARSER_JSCRIPT_H
#define CTAGS_PARSER_JSCRIPT_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "subparser.h"

typedef enum {
	JSTAG_FUNCTION,
	JSTAG_CLASS,
	JSTAG_METHOD,
	JSTAG_PROPERTY,
	JSTAG_CONSTANT,
	JSTAG_VARIABLE,
	JSTAG_GENERATOR,
	JSTAG_COUNT
} jsKind;

typedef struct sJavaScriptSubparser javaScriptSubparser;

struct sJavaScriptSubparser {
	subparser subparser;

	void (* tagEntryNotify) (javaScriptSubparser *s, tagEntryInfo *tag);
};

#endif
