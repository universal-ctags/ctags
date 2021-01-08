/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains ctags specific optscript objects
*/

#ifndef CTAGS_MAIN_SCRIPT_PRIVATE_H
#define CTAGS_MAIN_SCRIPT_PRIVATE_H

#include "general.h"  /* must always come first */

#include "optscript.h"

extern OptVM *optscriptInit (void);

extern void optscriptInstallProcs (EsObject *dict);

extern void optscriptSetup (OptVM *vm, EsObject *dict, int corkIndex);
extern void optscriptTeardown (OptVM *vm, EsObject *dict);

/* If len is 0, strlen (src) is used instead of 0. */
extern EsObject *optscriptRead (OptVM *vm, const char *src, size_t len);
extern EsObject *optscriptEval (OptVM *vm, EsObject *code);

#endif	/* CTAGS_MAIN_SCRIPT_PRIVATE_H */
