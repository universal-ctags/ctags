/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Private definitions for parsing support.
*/
#ifndef CTAGS_MAIN_PARSE_H
#define CTAGS_MAIN_PARSE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "types.h"

#include <stdint.h>

#include "kind.h"
#include "lregex.h"
#include "lxpath.h"
#include "vstring.h"

/*
*   MACROS
*/
#define LANG_AUTO   (-1)
#define LANG_IGNORE (-2)

/*
*   DATA DECLARATIONS
*/
typedef enum {
	RESCAN_NONE,   /* No rescan needed */
	RESCAN_FAILED, /* Scan failed, clear out tags added, rescan */
	RESCAN_APPEND  /* Scan succeeded, rescan */
} rescanReason;

typedef void (*createRegexTag) (const vString* const name);
typedef void (*simpleParser) (void);
typedef rescanReason (*rescanParser) (const unsigned int passCount);
typedef void (*parserInitialize) (langType language);
typedef void (*initStatistics) (langType language);
typedef void (*printStatistics) (langType langType);

/* Used only when CORK_TABLE_REVERSE_SCOPE_MAP is set.
 * This helps you to make the reverse scope map smaller.
 *
 * Return true for accepting an entry as the child of the parent.  The
 * entry is specified with childCandidateIndex and childCandidateTag.
 * The parent is specified with parentIndex. As the result of the
 * acceptance, the entry is linked to the reverse scope map of the
 * parent.
 *
 * forEachChildForCorkEntry() and forEachNamedChildForCorkEntry() are
 * for the function accessing the revese scope map of a given entry.
 */
typedef bool (*filterChildCandidate) (langType langType, int parentIndex, int childCandidateIndex,
									  const tagEntryInfo *childCandidateTag);

/* Used only when CORK_TABLE_REVERSE_SCOPE_MAP and
   CORK_TABLE_REVERSE_NAME_MAP are set.

   Return true if the newEntry and the preExistingEntry represent a same
   language object. */
typedef bool (* detectGroupFunc) (langType language,
								  const tagEntryInfo *newEntry,
								  const tagEntryInfo *preExistingEntry);

/* Per language finalizer is called anytime when ctags exits.
   (Exceptions are a kind of options are given when invoked. Here
   options are: --version, --help, --list-*, and so on.)

   The finalizer is called even when the initializer of the
   same parser is called or not. However, the finalizer can know
   whether the associated initializer is invoked or not with the
   second parameter: INITIALIZED. If it is true, the initializer
   is called. */
typedef void (*parserFinalize) (langType language, bool initialized);

typedef enum {
	METHOD_NOT_CRAFTED    = 1 << 0,
	METHOD_REGEX          = 1 << 1,
	METHOD_XPATH          = 1 << 2,
} parsingMethod;

typedef struct {
	const char *name;
	const int id;
} keywordTable;

typedef enum {
	/* The fundamental queue.
	 * All makeTagEntry'ed entries are stored to the cork queue, and
	 * indexes are assigned to the entries.
	 * The value returned from the makeTagEntry is the index.
	 * You can access the entry after calling makeTagEntry by
	 * calling getEntryInCorkQueue with the index.
	 * This must be enabled to use the reverse map enabled with
	 * CORK_TABLE_REVERSE_SCOPE_MAP.
	 */
	CORK_TABLE_QUEUE = 1 << 0,

	/* All the names are stored this flat map.
	 */
	CORK_TABLE_REVERSE_NAME_MAP       = 1 << 1,
	/* If the bit is not set, the accessing the flat map in
	 * case sensitive way.
	 */
	CORK_TABLE_REVERSE_NAME_MAP_ICASE = 1 << 2,

	/* To use forEachChildForCorkEntry and
	 * forEachNamedChildForCorkEntry, use following flags.
	 */
	CORK_TABLE_REVERSE_SCOPE_MAP       = 1 << 3,

	/* If the bit is not set, the accessing the map in
	 * case sensitive way.
	 */
	CORK_TABLE_REVERSE_SCOPE_MAP_ICASE = 1 << 4,
} corkTableSpec;

struct sParserDefinition {
	/* defined by parser */
	char* name;                    /* name of language */
	kindDefinition* kindTable;	   /* tag kinds handled by parser */
	unsigned int kindCount;        /* size of `kinds' list */
	const char *const *extensions; /* list of default extensions */
	const char *const *patterns;   /* list of default file name patterns */
	const char *const *aliases;    /* list of default aliases (alternative names) */
	parserInitialize initialize;   /* initialization routine, if needed */
	parserFinalize finalize;       /* finalize routine, if needed */
	simpleParser parser;           /* simple parser (common case) */
	rescanParser parser2;          /* rescanning parser (unusual case) */
	selectLanguage* selectLanguage; /* may be used to resolve conflicts */

	unsigned int method;           /* See METHOD_ definitions above */

	unsigned int useCork;		   /* true means ues CORK_TABLE_QUEUE */
	filterChildCandidate filterChild;
	detectGroupFunc detectGroup;

	bool useMemoryStreamInput;
	bool allowNullTag;
	bool requestAutomaticFQTag;
	tagRegexTable *tagRegexTable;
	unsigned int tagRegexCount;
	const keywordTable *keywordTable;
	unsigned int keywordCount;
	tagXpathTableTable *tagXpathTableTable;
	unsigned int tagXpathTableCount;
	bool invisible;
	fieldDefinition *fieldTable;
	unsigned int fieldCount;
	xtagDefinition *xtagTable;
	unsigned int xtagCount;

	parserDependency * dependencies;
	unsigned int dependencyCount;

	parameterHandlerTable  *parameterHandlerTable;
	unsigned int parameterHandlerCount;

	xpathFileSpec *xpathFileSpecs;
	unsigned int xpathFileSpecCount;

	/* Following two fields are used in a parser using cork. */
	const char *defaultScopeSeparator;
	const char *defaultRootScopeSeparator;

	initStatistics initStats;
	printStatistics printStats;

	/* used internally */
	langType id;		    /* id assigned to language */
	unsigned int enabled:1;	       /* currently enabled? */
	unsigned int traced:1;
};

typedef parserDefinition* (parserDefinitionFunc) (void);

/*
*   FUNCTION PROTOTYPES
*/

/* Language processing and parsing */
extern int makeSimpleTag (const vString* const name, const int kindIndex);
extern int makeSimpleRefTag (const vString* const name, const int kindIndex,
			     int roleIndex);
extern int makeSimplePlaceholder(const vString* const name);
extern parserDefinition* parserNew (const char* name);

extern const char *getLanguageName (const langType language);
extern const char *getLanguageKindName (const langType language, const int kindIndex);

extern langType getNamedLanguage (const char *const name, size_t len);
extern langType getLanguageForFilenameAndContents (const char *const fileName);
extern langType getLanguageForCommand (const char *const command, langType startFrom);
extern langType getLanguageForFilename (const char *const filename, langType startFrom);
extern bool isLanguageEnabled (const langType language);
extern bool isLanguageKindEnabled (const langType language, int kindIndex);
extern bool isLanguageRoleEnabled (const langType language, int kindIndex, int roleIndex);

extern kindDefinition* getLanguageKindForLetter (const langType language, char kindLetter);

extern void initializeParser (langType language);

#ifdef HAVE_ICONV
extern const char *getLanguageEncoding (const langType language);
#endif


/* Regex interface */
extern void addLanguageCallbackRegex (const langType language, const char *const regex, const char *const flags,
									  const regexCallback callback, bool *disabled, void *userData);
extern void findRegexTagsMainloop (int (* driver)(void));
extern void findRegexTags (void);

/* Multiline Regex Interface */
extern void addLanguageRegexTable (const langType language, const char *name);
extern void addLanguageTagMultiTableRegex(const langType language,
										  const char* const table_name,
										  const char* const regex,
										  const char* const name, const char* const kinds, const char* const flags,
										  bool *disabled);

extern void anonGenerate (vString *buffer, const char *prefix, int kind);
extern vString *anonGenerateNew (const char *prefix, int kind);
extern void anonHashString (const char *filename, char buf[9]);

#endif  /* CTAGS_MAIN_PARSE_H */
