/*
 *
 *  Copyright (c) 2020, Red Hat, Inc.
 *  Copyright (c) 2020, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_HINT_PRIVATE_H
#define CTAGS_MAIN_HINT_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

extern void processHintFileOption (const char *const option,
								   const char *const parameter);
extern void proprocessHints (void);

#endif	/* CTAGS_MAIN_HINT_PRIVATE_H */
