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
#include "numarray.h"

/* parser can be NULL; give a name with promiseUpdateLanguage()
 * when the name can be determined. */
int  makePromise   (const char *parser,
		    unsigned long startLine, long startCharOffset,
		    unsigned long endLine, long endCharOffset,
		    unsigned long sourceLineOffset);

/* Fill the line with white spaces.
   The callee takes the ownership of lines. */
void promiseAttachLineFiller (int promise, ulongArray *lines);

void promiseUpdateLanguage  (int promise, langType lang);

#endif	/* CTAGS_MAIN_PROMISE_H */
