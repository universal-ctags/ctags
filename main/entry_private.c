/*
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   main part private interface to entry.c
*/

#include "general.h"
#include "entry_p.h"
#include "parse_p.h"

extern const kindDefinition* getTagKind(const tagEntryInfo *const tag)
{
	return getLanguageKind(tag->langType, tag->kindIndex);
}

extern char getTagKindLetter(const tagEntryInfo *const tag)
{
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);
	return kdef->letter;
}

extern const char* getTagKindName(const tagEntryInfo *const tag)
{
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);
	return kdef->name;
}

extern const roleDefinition* getTagRole(const tagEntryInfo *const tag,
										int roleIndex)
{
	if (roleIndex == ROLE_DEFINITION_INDEX)
		return NULL;
	return getLanguageRole(tag->langType, tag->kindIndex, roleIndex);
}
