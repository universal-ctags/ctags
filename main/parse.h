/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Private definitions for parsing support.
*/
#ifndef _PARSE_H
#define _PARSE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "parsers.h"  /* contains list of parsers */
#include "strlist.h"

/*
*   MACROS
*/
#define KIND_COUNT(kindTable) (sizeof(kindTable)/sizeof(kindOption))

#define LANG_AUTO   (-1)
#define LANG_IGNORE (-2)

/*
*   DATA DECLARATIONS
*/
typedef int langType;

typedef enum {
	RESCAN_NONE,   /* No rescan needed */
	RESCAN_FAILED, /* Scan failed, clear out tags added, rescan */
	RESCAN_APPEND  /* Scan succeeded, rescan */
} rescanReason;

typedef void (*createRegexTag) (const vString* const name);
typedef void (*simpleParser) (void);
typedef rescanReason (*rescanParser) (const unsigned int passCount);
typedef void (*parserInitialize) (langType language);
typedef void (*parserFinalize) (langType language);
typedef const char * (*selectLanguage) (FILE *);

/*
 * Predefined kinds
 */
#define KIND_REGEX_DEFAULT 'r'
#define KIND_REGEX_DEFAULT_LONG "regex"
/* We treat ' ' as a ghost kind.
   It will never be listed up in --list-kinds. */

#define KIND_NULL    '\0'

#define KIND_GHOST   ' '
#define KIND_GHOST_LONG "ghost"

#define KIND_FILE_DEFAULT 'F'
#define KIND_FILE_DEFAULT_LONG "file"

#define KIND_FILE_ALT '!'

typedef struct sKindOption {
	boolean enabled;          /* are tags for kind enabled? */
	int letter;               /* kind letter */
	const char* name;         /* kind name */
	const char* description;  /* displayed in --help output */
} kindOption;

typedef struct stgTableEntry{
	/* two gram table which represents the
	   characteristic of its language.
	   This can be used when file extension
	   is conflicted another parser. */
	vString* spec;
	unsigned char* tgTable;
	vString* corpusFile;
	struct stgTableEntry *next;
} tgTableEntry;

typedef enum  {
  METHOD_NOT_CRAFTED    = 1 << 0,
  METHOD_REGEX          = 1 << 1,
  METHOD_XCMD           = 1 << 2,
  METHOD_XCMD_AVAILABLE = 1 << 3,
} parsingMethod;

typedef struct {
	/* defined by parser */
	char* name;                    /* name of language */
	kindOption* kinds;             /* tag kinds handled by parser */
	unsigned int kindCount;        /* size of `kinds' list */
	char fileKind;		           /* override letter for file kind */
	const char *const *extensions; /* list of default extensions */
	const char *const *patterns;   /* list of default file name patterns */
	const char *const *aliases;    /* list of default aliases (alternative names) */
	parserInitialize initialize;   /* initialization routine, if needed */
	parserFinalize finalize;       /* finalize routine, if needed */
	simpleParser parser;           /* simple parser (common case) */
	rescanParser parser2;          /* rescanning parser (unusual case) */
	selectLanguage* selectLanguage; /* may be used to resolve conflicts */
	unsigned int method;           /* See PARSE__... definitions above */
	tgTableEntry *tgEntries;
	boolean useCork;

	/* used internally */
	unsigned int id;               /* id assigned to language */
	boolean enabled;               /* currently enabled? */
	stringList* currentPatterns;   /* current list of file name patterns */
	stringList* currentExtensions; /* current list of extensions */
	stringList* currentAliaes;     /* current list of aliases */
} parserDefinition;

typedef parserDefinition* (parserDefinitionFunc) (void);

typedef struct {
	size_t start;   /* character index in line where match starts */
	size_t length;  /* length of match */
} regexMatch;

typedef void (*regexCallback) (const char *line, const regexMatch *matches, unsigned int count);

/*
*   FUNCTION PROTOTYPES
*/

/* Each parsers' definition function is called. The routine is expected to
 * return a structure allocated using parserNew(). This structure must,
 * at minimum, set the `parser' field.
 */
extern parserDefinitionFunc PARSER_LIST;

/* Language processing and parsing */
extern int makeSimpleTag (const vString* const name, kindOption* const kinds, const int kind);
extern void makeFileTag (const char *const fileName);
extern parserDefinition* parserNew (const char* name);
extern const char *getLanguageName (const langType language);
extern char getLanguageFileKind (const langType language);
extern langType getNamedLanguage (const char *const name);
extern langType getFileLanguage (const char *const fileName);
extern boolean isLanguageEnabled (const langType language);
extern boolean isLanguageKindEnabled (const langType language, char kind);

extern void installLanguageMapDefault (const langType language);
extern void installLanguageMapDefaults (void);
extern void clearLanguageMap (const langType language);
extern boolean removeLanguageExtensionMap (const char *const extension);
extern void addLanguageExtensionMap (const langType language, const char* extension, boolean exclusive);
extern void addLanguagePatternMap (const langType language, const char* ptrn, boolean exclusive);

extern void installLanguageAliasesDefault (const langType language);
extern void installLanguageAliasesDefaults (void);
extern void clearLanguageAliases (const langType language);
extern void addLanguageAlias (const langType language, const char* alias);

extern void addCorpusFile (const langType language, const char* const spec, vString* const corpus_file, boolean pattern_p);
extern void addTgEntryForExtension (const langType language, const char* const ext, unsigned char* const tg_table);
extern void addTgEntryForPattern (const langType language, const char* const pattern, unsigned char* const tg_table);

extern void printLanguageMap (const langType language, FILE *fp);
extern void printLanguageMaps (const langType language);
extern void unifyLanguageMaps (void);
extern void enableLanguages (const boolean state);
extern void enableLanguage (const langType language, const boolean state);
extern void initializeParsing (void);
extern void freeParserResources (void);
extern void printLanguageFileKind (const langType language);
extern void printLanguageKinds (const langType language);
extern void printLanguageCorpus (langType language, const char *const spec);
extern void printLanguageAliases (const langType language);
extern void printLanguageList (void);
extern boolean parseFile (const char *const fileName);

#ifdef HAVE_ICONV
extern void freeEncodingResources (void);
#endif

/* Regex interface */
#ifdef HAVE_REGEX
extern void findRegexTags (void);
extern boolean matchRegex (const vString* const line, const langType language);
#endif
extern void addLanguageRegex (const langType language, const char* const regex);
extern void addTagRegex (const langType language, const char* const regex, const char* const name, const char* const kinds, const char* const flags);
extern void addCallbackRegex (const langType language, const char *const regex, const char *const flags, const regexCallback callback);
extern void resetRegexKinds (const langType language, boolean mode);
extern boolean enableRegexKind (const langType language, const int kind, const boolean mode);
extern boolean isRegexKindEnabled (const langType language, const int kind);
extern boolean hasRegexKind (const langType language, const int kind);
extern void printRegexKinds (const langType language, boolean indent);
extern void freeRegexResources (void);
extern boolean checkRegex (void);
extern void useRegexMethod (const langType language);

#ifdef HAVE_COPROC
extern boolean invokeXcmd (const char* const fileName, const langType language);
#endif
extern void addLanguageXcmd (const langType language, const char* const path);
extern void addTagXcmd (const langType language, vString* pathvstr, const char* flaggs);
extern void resetXcmdKinds (const langType language, boolean mode);
extern boolean enableXcmdKind (const langType language, const int kind, const boolean mode);
extern boolean isXcmdKindEnabled (const langType language, const int kind);
extern boolean hasXcmdKind (const langType language, const int kind);
extern void printXcmdKinds (const langType language, boolean indent);
extern void freeXcmdResources (void);
extern void useXcmdMethod (const langType language);
extern void notifyAvailabilityXcmdMethod (const langType language);

#endif  /* _PARSE_H */

/* vi:set tabstop=4 shiftwidth=4: */
