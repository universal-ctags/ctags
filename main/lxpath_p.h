/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Xpath based parer API for the main part
*/
#ifndef CTAGS_LXPATH_PARSE_PRIVATE_H
#define CTAGS_LXPATH_PARSE_PRIVATE_H

/*
*   INCLUDE FILES
*/

#include "general.h"  /* must always come first */
#include "types.h"


/*
*   FUNCTION PROTOTYPES
*/

extern void addTagXpath (const langType language, tagXpathTable *xpathTable);
extern void removeTagXpath (const langType language, tagXpathTable *xpathTable);

#endif  /* CTAGS_LXPATH_PARSE_PRIVATE_H */
