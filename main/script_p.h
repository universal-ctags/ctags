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

extern EsObject *OPTSCRIPT_ERR_NOTAGENTRY;

extern OptVM *optscriptInit (void);

extern void optscriptInstallProcs (EsObject *dict, OptOperatorFn matchResultAccessor);

extern void optscriptSetup (OptVM *vm, EsObject *dict, int corkIndex);
extern void optscriptTeardown (OptVM *vm, EsObject *dict);

/* If len is 0, strlen (src) is used instead of 0. */
extern EsObject *optscriptRead (OptVM *vm, const char *src, size_t len);
extern EsObject *optscriptEval (OptVM *vm, EsObject *code);
extern EsObject *optscriptDefine (EsObject *dict, const char *name, EsObject *obj);

extern EsObject *optscriptReadAndEval   (OptVM *vm, const char *src, size_t len);
extern EsObject *optscriptReadAndDefine (OptVM *vm, EsObject *dict, const char *name,
										 const char *src, size_t len);

extern void      optscriptHelp          (OptVM *vm, FILE *fp);

#endif	/* CTAGS_MAIN_SCRIPT_PRIVATE_H */
