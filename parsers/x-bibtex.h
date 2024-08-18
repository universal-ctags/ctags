/*
*   Copyright (c) 2023, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/
#ifndef CTAGS_PARSER_BIBTEX_H
#define CTAGS_PARSER_BIBTEX_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"

typedef struct sBibTexSubparser bibTexSubparser;

struct sBibTexSubparser {
	subparser subparser;
	int (* isKeywordForTagging) (bibTexSubparser *,
								 const char *string);
};

#endif	/* CTAGS_PARSER_BIBTEX_H */
