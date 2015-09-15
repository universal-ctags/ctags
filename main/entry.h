/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to entry.c
*/
#ifndef _ENTRY_H
#define _ENTRY_H

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

/*  Maintains the state of the tag file.
 */
struct sTagEntryInfo;
typedef struct eTagFile {
	char *name;
	char *directory;
	FILE *fp;
	struct sNumTags { unsigned long added, prev; } numTags;
	struct sMax { size_t line, tag, file; } max;
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
} tagFile;

typedef struct sTagFields {
	unsigned int count;        /* number of additional extension flags */
	const char *const *label;  /* list of labels for extension flags */
	const char *const *value;  /* list of values for extension flags */
} tagFields;

/*  Information about the current tag candidate.
 */
typedef struct sTagEntryInfo {
	boolean     lineNumberEntry;  /* pattern or line number entry */
	unsigned long lineNumber;     /* line number of tag */
	const char* pattern;	      /* pattern for locating source line
				       * (may be NULL if not present) *//*  */
	fpos_t      filePosition;     /* file position of line containing tag */
	const char* language;         /* language of source file */
	boolean     isFileScope;      /* is tag visible only within source file? */
	boolean     isFileEntry;      /* is this just an entry for a file name? */
	boolean     truncateLine;     /* truncate tag line at end of tag name? */
	const char *sourceFileName;   /* name of source file */
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

	} extensionFields;  /* list of extension fields*/
} tagEntryInfo;

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
extern void initTagEntryFull (tagEntryInfo *const e, const char *const name,
			      unsigned long lineNumber,
			      const char* language,
			      fpos_t      filePosition,
			      const char *sourceFileName,
			      const kindOption *kind);

/* language is optional: can be NULL. */
extern void writePseudoTag (const char *const tagName,
			    const char *const fileName,
			    const char *const pattern,
			    const char *const language);

void          corkTagFile(void);
void          uncorkTagFile(void);
tagEntryInfo *getEntryInCorkQueue   (unsigned int n);
size_t        countEntryInCorkQueue (void);

#endif  /* _ENTRY_H */

/* vi:set tabstop=4 shiftwidth=4: */
