/*
 *
 *  Copyright (c) 2026, Red Hat, Inc.
 *  Copyright (c) 2026, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_STACK_GUARD_H
#define CTAGS_MAIN_STACK_GUARD_H

/*
*   INCLUDE FILES
*/
#include "general.h"
#include "parse.h"

/*
*   FUNCTION PROTOTYPES
*/
extern size_t stackGuardGetLimit (void);
extern void stackGuardSetLimit (const size_t limit);
extern void stackGuardPrepare (discardInputFn gotoEOF);
extern void stackGuardRelease (void);
extern size_t stackGuardObservedPeak (const char **input);

#endif	/* CTAGS_MAIN_STACK_GUARD_H */
