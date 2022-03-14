/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   The interface for subparsers of Markdown
*/
#ifndef CTAGS_PARSER_MARKDOWN_H
#define CTAGS_PARSER_MARKDOWN_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"
#include "vstring.h"

typedef struct sMarkdownSubparser markdownSubparser;

struct sMarkdownSubparser {
	subparser subparser;
	bool (* extractLanguageForCodeBlock) (markdownSubparser *s,
										  const char *langMarker,
										  vString *langName);
};

#endif
