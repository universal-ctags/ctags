/*
*
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   Author: Masatake YAMATO <yamato@redhat.com>
*           https://ctags.io
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains infrastructure to realize Multi pass parsing over
*   Multi source files (MM).
*/

#ifndef CTAGS_MM_H
#define CTAGS_MM_H

#include "general.h"  /* must always come first */
#include "types.h"

void mmSchedule (const char*fname, langType lang);

bool mmRun (void);

int mmCurrentPass (void);
bool inMMPass (void);

void pourEntryToBarrel (tagEntryInfo *e);

struct sBarrel;
typedef struct sBarrel Barrel;

unsigned int countEntryInBarrel (Barrel *b);
tagEntryInfo* getEntryInBarrel (Barrel *b, unsigned int index);


#endif
