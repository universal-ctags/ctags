/*
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for makefiles.
*/

#ifndef CTAGS_PARSER_M4_H
#define CTAGS_PARSER_M4_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "types.h"
#include "subparser.h"
#include "vstring.h"

typedef struct sM4Subparser m4Subparser;
struct sM4Subparser {
	subparser subparser;

	bool (* probeLanguage) (m4Subparser *m4, const char* token);

	/* return value: Cork index */
	int  (* newMacroNotify) (m4Subparser *m4, const char* token);

	bool (* doesLineCommentStart)   (m4Subparser *m4, int c, const char *token);
	bool (* doesStringLiteralStart) (m4Subparser *m4, int c);
};

/* Helpers functions */
extern bool readM4MacroArgument(vString *const arg);
extern void setM4Quotes(char openQuote, char closeQuote);

#endif
