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
#ifndef CTAGS_MAIN_PARAM_H
#define CTAGS_MAIN_PARAM_H

#include "general.h"
#include "types.h"

typedef struct sParameterHandlerTable {
	const char *name;
	const char *desc;
	void  (* handleParameter) (langType lang, const char *name, const char *arg);
} parameterHandlerTable;

extern void applyParameter (const langType language, const char *name, const char *args);

extern void printParameterListHeader (bool indent, bool tabSeparated);
extern void printParameter (const parameterHandlerTable *const paramHandler, bool indent, bool tabSeparated);


#define PR_PARAM_STR(X) PR_PARAM_WIDTH_##X
#define PR_PARAM_FMT(X,T) "%-" STRINGIFY(PR_PARAM_STR(X)) STRINGIFY(T)

#define PR_PARAM_WIDTH_LANG 15

#endif	/* CTAGS_MAIN_PARAM_H */
