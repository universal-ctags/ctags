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
#ifndef CTAGS_MAIN_PROMISE_PRIVATE_H
#define CTAGS_MAIN_PROMISE_PRIVATE_H

#include "general.h"

bool forcePromises (void);
void breakPromisesAfter (int promise);
int getLastPromise (void);
void runModifiers (int promise,
				   unsigned long startLine, long startCharOffset,
				   unsigned long endLine, long endCharOffset,
				   unsigned char *input,
				   size_t size);

#endif	/* CTAGS_MAIN_PROMISE_PRIVATE_H */
