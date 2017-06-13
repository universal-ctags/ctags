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

#include "dependency.h"
#include "field.h"
#include "kind.h"
#include "lregex.h"
#include "lxpath.h"
#include "param.h"
#include "parsers.h"  /* contains list of parsers */
#include "strlist.h"
#include "xtag.h"

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
	bool useCork;
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

	/* used internally */
	langType id;		    /* id assigned to language */
	unsigned int enabled:1;	       /* currently enabled? */
};

typedef parserDefinition* (parserDefinitionFunc) (void);

typedef enum {
	LMAP_PATTERN   = 1 << 0,
	LMAP_EXTENSION = 1 << 1,
	LMAP_ALL       = LMAP_PATTERN | LMAP_EXTENSION,
} langmapType;

/*
*   FUNCTION PROTOTYPES
*/

/* Each parsers' definition function is called. The routine is expected to
 * return a structure allocated using parserNew(). This structure must,
 * at minimum, set the `parser' field.
 */
extern parserDefinitionFunc PARSER_LIST;
#ifdef HAVE_LIBXML
extern parserDefinitionFunc XML_PARSER_LIST;
#endif
#ifdef HAVE_LIBYAML
extern parserDefinitionFunc YAML_PARSER_LIST;
#endif


/* Language processing and parsing */
extern int makeSimpleTag (const vString* const name, kindDefinition* const kinds, const int kind);
extern int makeSimpleRefTag (const vString* const name, kindDefinition* const kinds, const int kind,
			     int roleIndex);
extern parserDefinition* parserNew (const char* name);
extern bool doesLanguageAllowNullTag (const langType language);
extern bool doesLanguageRequestAutomaticFQTag (const langType language);
extern const char *getLanguageName (const langType language);
/* kindIndex has to be explicitly signed because char is not signed in all platforms. */
extern kindDefinition* getLanguageKindForLetter (const langType language, char kindLetter);
extern kindDefinition* getLanguageKind(const langType language, signed char kindIndex);
extern int defineLanguageKind (const langType language, kindDefinition *def,
							   freeKindDefFunc freeKindDef);
extern langType getNamedLanguage (const char *const name, size_t len);
extern langType getFileLanguage (const char *const fileName);
extern bool isLanguageEnabled (const langType language);
extern bool isLanguageKindEnabled (const langType language, char kind);

extern void installLanguageMapDefault (const langType language);
extern void installLanguageMapDefaults (void);
extern void clearLanguageMap (const langType language);
extern bool removeLanguageExtensionMap (const langType language, const char *const extension);
extern void addLanguageExtensionMap (const langType language, const char* extension,
				     bool exclusiveInAllLanguages);
extern bool removeLanguagePatternMap (const langType language, const char *const pattern);
extern void addLanguagePatternMap (const langType language, const char* ptrn,
				   bool exclusiveInAllLanguages);

extern void installLanguageAliasesDefault (const langType language);
extern void installLanguageAliasesDefaults (void);
extern void clearLanguageAliases (const langType language);
extern void addLanguageAlias (const langType language, const char* alias);

extern void printLanguageMap (const langType language, FILE *fp);
extern void printLanguageMaps (const langType language, langmapType type);
extern void enableLanguages (const bool state);
extern void enableLanguage (const langType language, const bool state);
extern void initializeParsing (void);
extern void initializeParser (langType language);
extern unsigned int countParsers (void);
extern void freeParserResources (void);
extern void printLanguageKinds (const langType language, bool allKindFields);
extern void printLanguageRoles (const langType language, const char* letters);
extern void printLanguageAliases (const langType language);
extern void printLanguageList (void);
extern void printLanguageParameters (const langType language);
extern void printLanguageSubparsers (const langType language);
extern bool doesParserRequireMemoryStream (const langType language);
extern bool parseFile (const char *const fileName);
extern bool parseFileWithMio (const char *const fileName, MIO *mio);
extern bool runParserInNarrowedInputStream (const langType language,
					       unsigned long startLine, long startCharOffset,
					       unsigned long endLine, long endCharOffset,
					       unsigned long sourceLineOffset);

#ifdef HAVE_ICONV
extern void freeEncodingResources (void);
#endif


/* Regex interface */
extern bool processLanguageRegexOption (langType language, const char *const parameter);
extern void findRegexTags (void);
extern void findRegexTagsMainloop (int (* driver)(void));
extern void matchLanguageRegex (const langType language, const vString* const line);
extern void addLanguageCallbackRegex (const langType language, const char *const regex, const char *const flags,
									  const regexCallback callback, bool *disabled, void *userData);
extern void freeRegexResources (void);
extern bool checkRegex (void);
extern void useRegexMethod (const langType language);
extern void printRegexFlags (void);
extern bool hasLanguageScopeActionInRegex (const langType language);

/* Multiline Regex Interface */
extern bool hasLanguageMultilineRegexPatterns (const langType language);
extern void matchLanguageMultilineRegex (const langType language, const vString* const allLines);

extern unsigned int   getXpathFileSpecCount (const langType language);
extern xpathFileSpec* getXpathFileSpec (const langType language, unsigned int nth);

extern bool makeKindSeparatorsPseudoTags (const langType language,
					     const ptagDesc *pdesc);
extern bool makeKindDescriptionsPseudoTags (const langType language,
					       const ptagDesc *pdesc);

extern void anonGenerate (vString *buffer, const char *prefix, int kind);
extern void anonHashString (const char *filename, char buf[9]);

#endif  /* CTAGS_MAIN_PARSE_H */
