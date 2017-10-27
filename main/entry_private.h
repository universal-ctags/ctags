/*
*   Copyright (c) 2017, Red Hat, INc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   main part private interface to entry.c
*/
#ifndef CTAGS_PRIVATE_ENTRY_H
#define CTAGS_PRIVATE_ENTRY_H

#include "general.h"
#include "entry.h"
#include "types.h"

extern kindDefinition* getTagKind(const tagEntryInfo *const tag);
extern char getTagKindLetter(const tagEntryInfo *const tag);
extern char* getTagKindName(const tagEntryInfo *const tag);

#endif	/* CTAGS_PRIVATE_ENTRY_H */
