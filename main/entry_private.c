/*
*   Copyright (c) 2017, Red Hat, INc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   main part private interface to entry.c
*/

#include "entry_private.h"
#include "parse.h"

extern kindDefinition* getTagKind(const tagEntryInfo *const tag)
{
	return getLanguageKind(tag->langType, tag->kindIndex);
}

extern char getTagKindLetter(const tagEntryInfo *const tag)
{
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);
	return kdef->letter;
}

extern char* getTagKindName(const tagEntryInfo *const tag)
{
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);
	return kdef->name;
}
