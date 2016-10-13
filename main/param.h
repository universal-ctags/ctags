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

#endif	/* CTAGS_MAIN_PARAM_H */
