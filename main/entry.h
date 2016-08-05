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
#include "types.h"

#include <stdio.h>

#include "field.h"
#include "kind.h"
#include "vstring.h"
#include "xtag.h"
#include "mio.h"
#include "nestlevel.h"

/*
*   MACROS
*/
#define WHOLE_FILE  -1L
#define includeExtensionFlags()         (Option.tagFileFormat > 1)

/*
*   DATA DECLARATIONS
*/
typedef struct sTagField {
	fieldType  ftype;
	const char* value;
} tagField;

/*  Information about the current tag candidate.
 */
struct sTagEntryInfo {
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
	unsigned int boundaryInfo;    /* info about nested input stream */
	MIOPos      filePosition;     /* file position of line containing tag */
	const char* language;         /* language of input file */
	const char *inputFileName;   /* name of input file */
	const char *name;             /* name of the tag */
	const kindOption *kind;	      /* kind descriptor */
	unsigned char extra[ ((XTAG_COUNT) / 8) + 1 ];

	struct {
		const char* access;
		const char* fileScope;
		const char* implementation;
		const char* inheritance;

		const kindOption* scopeKind;
		const char* scopeName;
		int         scopeIndex;   /* cork queue entry for upper scope tag.
					     This field is meaningful if the value
					     is not CORK_NIL and scope[0]  and scope[1] are
					     NULL. */

		const char* signature;

		/* type (union/struct/etc.) and name for a variable or typedef. */
		const char* typeRef [2];  /* e.g., "struct" and struct name */

#define ROLE_INDEX_DEFINITION -1
		int roleIndex; /* for role of reference tag */

#ifdef HAVE_LIBXML
		const char* xpath;
#endif
		unsigned long endLine;
	} extensionFields;  /* list of extension fields*/

#define PRE_ALLOCATED_PARSER_FIELDS 5
#define NO_PARSER_FIELD -1
	unsigned int usedParserFields;
	tagField     parserFields [PRE_ALLOCATED_PARSER_FIELDS];

	/* Following source* fields are used only when #line is found
	   in input and --line-directive is given in ctags command line. */
	const char* sourceLanguage;
	const char *sourceFileName;
	unsigned long sourceLineNumberDifference;
};


/*
*   GLOBAL VARIABLES
*/


/*
*   FUNCTION PROTOTYPES
*/
extern void freeTagFileResources (void);
extern const char *tagFileName (void);
extern void openTagFile (void);
extern void closeTagFile (const boolean resize);
extern void  setupWriter (void);
extern void  teardownWriter (const char *inputFilename);
extern int makeTagEntry (const tagEntryInfo *const tag);
extern void initTagEntry (tagEntryInfo *const e, const char *const name,
			  const kindOption *kind);
extern void initRefTagEntry (tagEntryInfo *const e, const char *const name,
			     const kindOption *kind, int roleIndex);
extern void initTagEntryFull (tagEntryInfo *const e, const char *const name,
			      unsigned long lineNumber,
			      const char* language,
			      MIOPos      filePosition,
			      const char *inputFileName,
			      const kindOption *kind,
			      int roleIndex,
			      const char *sourceFileName,
			      const char* sourceLanguage,
			      long sourceLineNumberDifference);
extern int makeQualifiedTagEntry (const tagEntryInfo *const e);

extern unsigned long numTagsAdded(void);
extern void setNumTagsAdded (unsigned long nadded);
extern unsigned long numTagsTotal(void);
extern unsigned long maxTagsLine(void);
extern void invalidatePatternCache(void);
extern void tagFilePosition (MIOPos *p);
extern void setTagFilePosition (MIOPos *p);
extern const char* getTagFileDirectory (void);
extern void getTagScopeInformation (tagEntryInfo *const tag,
				    const char **kind, const char **name);

/* Getting line associated with tag */
extern char *readLineFromBypassAnyway (vString *const vLine, const tagEntryInfo *const tag,
				   long *const pSeekValue);

/* Generating pattern associated tag, caller must do eFree for the returned value. */
extern char* makePatternString (const tagEntryInfo *const tag);


/* language is optional: can be NULL. */
extern boolean writePseudoTag (const ptagDesc *pdesc,
			       const char *const fileName,
			       const char *const pattern,
			       const char *const parserName);

#define CORK_NIL 0
void          corkTagFile(void);
void          uncorkTagFile(void);
tagEntryInfo *getEntryInCorkQueue   (unsigned int n);
tagEntryInfo *getEntryOfNestingLevel (const NestingLevel *nl);
size_t        countEntryInCorkQueue (void);

extern void makeFileTag (const char *const fileName);

extern void    markTagExtraBit     (tagEntryInfo *const tag, xtagType extra);
extern boolean isTagExtraBitMarked (const tagEntryInfo *const tag, xtagType extra);

extern void attachParserField (tagEntryInfo *const tag, fieldType ftype, const char* value);
extern void attachParserFieldToCorkEntry (int index, fieldType ftype, const char* value);

#endif  /* CTAGS_MAIN_ENTRY_H */
