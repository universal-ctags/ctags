/*
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   main part private interface to entry.c
*/
#ifndef CTAGS_PRIVATE_ENTRY_H
#define CTAGS_PRIVATE_ENTRY_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "entry.h"
#include "types.h"

/*
*   FUNCTION PROTOTYPES
*/
extern const kindDefinition* getTagKind(const tagEntryInfo *const tag);
extern char getTagKindLetter(const tagEntryInfo *const tag);
extern const char* getTagKindName(const tagEntryInfo *const tag);

extern const roleDefinition* getTagRole(const tagEntryInfo *const tag, int roleIndex);

extern void freeTagFileResources (void);
extern const char *tagFileName (void);
extern void openTagFile (void);
extern void closeTagFile (const bool resize);
extern void  setupWriter (void *writerClientData);
extern bool  teardownWriter (const char *inputFilename);

extern unsigned long numTagsAdded(void);
extern void setNumTagsAdded (unsigned long nadded);
extern unsigned long numTagsTotal(void);
extern unsigned long maxTagsLine(void);
extern void invalidatePatternCache(void);
extern void tagFilePosition (MIOPos *p);
extern void setTagFilePosition (MIOPos *p, bool truncation);
extern const char* getTagFileDirectory (void);
extern void getTagScopeInformation (tagEntryInfo *const tag,
				    const char **kind, const char **name);

/* Getting line associated with tag */
extern char *readLineFromBypassForTag (vString *const vLine, const tagEntryInfo *const tag,
				   long *const pSeekValue);

/* Generating pattern associated tag, caller must do eFree for the returned value. */
extern char* makePatternString (const tagEntryInfo *const tag);


/* language is optional: can be NULL. */
extern bool writePseudoTag (const ptagDesc *pdesc,
			       const char *const fileName,
			       const char *const pattern,
			       const char *const parserName);

void          corkTagFile(unsigned int corkFlags);
void          uncorkTagFile(void);

extern void makeFileTag (const char *const fileName);

extern const tagField* getParserFieldForIndex (const tagEntryInfo * tag, int index);


CTAGS_INLINE roleBitsType makeRoleBit(int roleIndex)
{
	if (roleIndex == ROLE_DEFINITION_INDEX)
		return 0;
	else
		return ((roleBitsType)1) << roleIndex;
}

#endif	/* CTAGS_PRIVATE_ENTRY_H */
