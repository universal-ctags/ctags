/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/
#ifndef CTAGS_MAIN_ENTRY_H
#define CTAGS_MAIN_ENTRY_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>

#include "kind.h"
#include "vstring.h"

/*
*   MACROS
*/
#define WHOLE_FILE  -1L

/*
*   DATA DECLARATIONS
*/

typedef struct sTagFields {
	unsigned int count;        /* number of additional extension flags */
	const char *const *label;  /* list of labels for extension flags */
	const char *const *value;  /* list of values for extension flags */
} tagFields;

/*  Information about the current tag candidate.
 */
typedef struct sTagEntryInfo {
	unsigned int lineNumberEntry:1;  /* pattern or line number entry */
	unsigned int isFileScope    :1;  /* is tag visible only within input file? */
	unsigned int isFileEntry    :1;  /* is this just an entry for a file name? */
	unsigned int truncateLine   :1;  /* truncate tag line at end of tag name? */
	unsigned int placeholder    :1;	 /* This is just a part of scope context.
					    Put this entry to cork queue but
					    don't print it to tags file. */

	unsigned long lineNumber;     /* line number of tag */
	const char* pattern;	      /* pattern for locating input line
				       * (may be NULL if not present) *//*  */
	fpos_t      filePosition;     /* file position of line containing tag */
	const char* language;         /* language of input file */
	const char *inputFileName;   /* name of input file */
	const char *name;             /* name of the tag */
	const kindOption *kind;	      /* kind descriptor */
	struct {
		const char* access;
		const char* fileScope;
		const char* implementation;
		const char* inheritance;

		const kindOption* scopeKind;
		const char* scopeName;
#define SCOPE_NIL 0
		int         scopeIndex;   /* cork queue entry for upper scope tag.
					     This field is meaningful if the value
					     is not SCOPE_NIL and scope[0]  and scope[1] are
					     NULL. */

		const char* signature;

		/* type (union/struct/etc.) and name for a variable or typedef. */
		const char* typeRef [2];  /* e.g., "struct" and struct name */

#define ROLE_INDEX_DEFINITION -1
		int roleIndex; /* for role of reference tag */
	} extensionFields;  /* list of extension fields*/

	/* Following source* fields are used only when #line is found
	   in input and --line-directive is given in ctags command line. */
	const char* sourceLanguage;
	const char *sourceFileName;
	unsigned long sourceLineNumberDifference;
} tagEntryInfo;

/*  Maintains the state of the tag file.
 */
typedef struct eTagFile {
	char *name;
	char *directory;
	FILE *fp;
	struct sNumTags { unsigned long added, prev; } numTags;
	struct sMax { size_t line, tag; } max;
	struct sEtags {
		char *name;
		FILE *fp;
		size_t byteCount;
	} etags;
	vString *vLine;

	unsigned int cork;
	struct sCorkQueue {
		struct sTagEntryInfo* queue;
		unsigned int length;
		unsigned int count;
	} corkQueue;

	boolean patternCacheValid;
} tagFile;

/*
*   GLOBAL VARIABLES
*/
extern tagFile TagFile;

/*
*   FUNCTION PROTOTYPES
*/
extern void freeTagFileResources (void);
extern const char *tagFileName (void);
extern void copyBytes (FILE* const fromFp, FILE* const toFp, const long size);
extern void copyFile (const char *const from, const char *const to, const long size);
extern void openTagFile (void);
extern void closeTagFile (const boolean resize);
extern void beginEtagsFile (void);
extern void endEtagsFile (const char *const name);
extern int makeTagEntry (const tagEntryInfo *const tag);
extern void initTagEntry (tagEntryInfo *const e, const char *const name,
			  const kindOption *kind);
extern void initRefTagEntry (tagEntryInfo *const e, const char *const name,
			     const kindOption *kind, int roleIndex);
extern void initTagEntryFull (tagEntryInfo *const e, const char *const name,
			      unsigned long lineNumber,
			      const char* language,
			      fpos_t      filePosition,
			      const char *inputFileName,
			      const kindOption *kind,
			      int roleIndex,
			      const char *sourceFileName,
			      const char* sourceLanguage,
			      long sourceLineNumberDifference);

/* Getting line associated with tag */
extern char *readLineFromBypassAnyway (vString *const vLine, const tagEntryInfo *const tag,
				   long *const pSeekValue);

/* Generating pattern associated tag, caller must do eFree for the returned value. */
extern char* makePatternString (const tagEntryInfo *const tag);


/* language is optional: can be NULL. */
struct sPtagDesc;
extern void writePseudoTag (const struct sPtagDesc *pdesc,
			    const char *const fileName,
			    const char *const pattern,
			    const char *const language);

void          corkTagFile(void);
void          uncorkTagFile(void);
tagEntryInfo *getEntryInCorkQueue   (unsigned int n);
size_t        countEntryInCorkQueue (void);

extern void makeFileTag (const char *const fileName);

#endif  /* CTAGS_MAIN_ENTRY_H */

/* vi:set tabstop=4 shiftwidth=4: */
