/*
 *
 *  Copyright (c) 2016, Red Hat, Inc.
 *  Copyright (c) 2016, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_PROMISE_H
#define CTAGS_MAIN_PROMISE_H

#include "general.h"
#include "mio.h"
#include "parse.h"

int  makePromise   (const char *parser,
		    unsigned long startLine, int startCharOffset,
		    unsigned long endLine, int endCharOffset,
		    unsigned long sourceLineOffset);
bool forcePromises (void);
void breakPromisesAfter (int promise);
int getLastPromise (void);

#endif	/* CTAGS_MAIN_PROMISE_H */
