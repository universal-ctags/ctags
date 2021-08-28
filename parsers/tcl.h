/*
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef CTAGS_PARSER_TCL_H
#define CTAGS_PARSER_TCL_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"
#include "tokeninfo.h"

typedef struct sTclSubparser tclSubparser;

enum TclTokenType {
	/* 0..255 are the byte's value */
	TOKEN_TCL_EOF = 256,
	TOKEN_TCL_UNDEFINED,
	TOKEN_TCL_KEYWORD,
	TOKEN_TCL_IDENTIFIER,
	TOKEN_TCL_VARIABLE,
	TOKEN_TCL_EOL,
	TOKEN_TCL_STRING,
};

struct sTclSubparser {
	subparser subparser;

	/* `pstate' is needed to call newTclToken(). */
	void (* namespaceImportNotify) (tclSubparser *s, char *namespace,
									void *pstate);
	/* Return CORK_NIL if the command line is NOT consumed.
	   If a positive integer is returned, end: field may
	   be attached by tcl base parser.
	   Return CORK_NIL - 1 if the command line is consumed
	   but not tag is made. */
	int (* commandNotify) (tclSubparser *s, char *command,
						   int parentIndex,
						   void *pstate);
};

extern tokenInfo *newTclToken (void *pstate);
extern void skipToEndOfTclCmdline (tokenInfo *const token);

#endif	/* CTAGS_PARSER_TCL_H */
