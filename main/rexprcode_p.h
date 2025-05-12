/*
*   Copyright (c) 2025, Red Hat, Inc.
*   Copyright (c) 2025, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.

*/

#ifndef CTAGS_MAIN_REXPRCODE_H
#define CTAGS_MAIN_REXPRCODE_H

/*
*   INCLUDE FILES
*/
#include "general.h"

#include "vstring.h"

/*
*   DATA DECLARATIONS
*/

struct rExprCode;

/*
*   FUNCTION DECLARATIONS
*/
extern const char *rExprCodeGetSource (const struct rExprCode *rxcode);
extern vString *rExprCodeNewEncodedSource (const struct rExprCode *rxcode);
extern bool rExprCodeGetICase (const struct rExprCode *rxcode);
extern struct rExprCode *rExprCodeNew(const char *rxsrc, bool iCase);
extern void rExprCodeDelete (struct rExprCode *rxcode);
extern bool rExprCodeMatch (struct rExprCode *rxcode, const char *fname);

#endif	/* CTAGS_MAIN_REXPRCODE_H */
