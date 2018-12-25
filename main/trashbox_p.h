/*
*
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef CTAGS_MAIN_TRASH_PRIVATE_H
#define CTAGS_MAIN_TRASH_PRIVATE_H

/*
*   INCLUDE FILES
*/

#include "general.h"  /* must always come first */


/*
*   FUNCTION PROTOTYPES
*/

extern void initDefaultTrashBox (void);
extern void finiDefaultTrashBox  (void);

extern void initParserTrashBox (void);
extern void finiParserTrashBox  (void);

#endif	/* CTAGS_MAIN_TRASH_PRIVATE_H */
