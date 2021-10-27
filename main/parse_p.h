/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Private definitions for parsing support.
*/
#ifndef CTAGS_MAIN_PARSE_PRIVATE_H
#define CTAGS_MAIN_PARSE_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "kind.h"
#include "lregex_p.h"
#include "parse.h"
#include "parsers_p.h"  /* contains list of parsers */
#include "strlist.h"
#ifdef EXTERNAL_PARSER_LIST_FILE
#include EXTERNAL_PARSER_LIST_FILE
#endif

/*
*   MACROS
*/
#define LANG_FALLBACK   (1)

/*
*   DATA DECLARATIONS
*/
typedef enum {
	LMAP_PATTERN   = 1 << 0,
	LMAP_EXTENSION = 1 << 1,
	LMAP_ALL       = LMAP_PATTERN | LMAP_EXTENSION,
	LMAP_TABLE_OUTPUT = 1 << 2,
} langmapType;

/*
*   FUNCTION PROTOTYPES
*/

/* Each parsers' definition function is called. The routine is expected to
 * return a structure allocated using parserNew(). This structure must,
 * at minimum, set the `parser' field.
 */
#ifdef EXTERNAL_PARSER_LIST
extern parserDefinitionFunc EXTERNAL_PARSER_LIST;
#else /* ! EXTERNAL_PARSER_LIST */
extern parserDefinitionFunc PARSER_LIST;
#ifdef HAVE_LIBXML
extern parserDefinitionFunc XML_PARSER_LIST;
#endif
#ifdef HAVE_LIBYAML
extern parserDefinitionFunc YAML_PARSER_LIST;
#endif
#ifdef HAVE_PACKCC
extern parserDefinitionFunc PEG_PARSER_LIST;
#endif
#endif /* EXTERNAL_PARSER_LIST */

extern bool doesLanguageAllowNullTag (const langType language);
extern bool doesLanguageRequestAutomaticFQTag (const langType language);

extern langType getNamedLanguageFull (const char *const name, size_t len, bool noPretending, bool include_aliases);

extern kindDefinition* getLanguageKind(const langType language, int kindIndex);
extern kindDefinition* getLanguageKindForName (const langType language, const char *kindName);
extern roleDefinition* getLanguageRole(const langType language, int kindIndex, int roleIndex);
extern roleDefinition* getLanguageRoleForName (const langType language, int kindIndex,
											   const char *roleName);


extern int defineLanguageKind (const langType language, kindDefinition *def,
							   freeKindDefFunc freeKindDef);

extern unsigned int countLanguageKinds (const langType language);
extern unsigned int countLanguageRoles (const langType language, int kindIndex);

extern bool isLanguageKindRefOnly (const langType language, int kindIndex);

extern bool isLanguageVisible (const langType language);


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

extern void printLanguageMaps (const langType language, langmapType type,
							   bool withListHeader, bool machinable, FILE *fp);
extern void enableLanguages (const bool state);
extern void enableLanguage (const langType language, const bool state);
extern void initializeParsing (void);

extern unsigned int countParsers (void);
extern void freeParserResources (void);
extern void enableDefaultFileKind (bool state);
extern void printLanguageKinds (const langType language, bool allKindFields,
								bool withListHeader, bool machinable, FILE *fp);
extern void printLanguageRoles (const langType language, const char* letters,
								bool withListHeader, bool machinable, FILE *fp);
extern void printLanguageAliases (const langType language,
								  bool withListHeader, bool machinable, FILE *fp);
extern void printLanguageList (void);
extern void printLanguageParameters (const langType language,
									 bool withListHeader, bool machinable, FILE *fp);
extern void printLanguageSubparsers (const langType language,
									 bool withListHeader, bool machinable, FILE *fp);
extern void printLangdefFlags (bool withListHeader, bool machinable, FILE *fp);
extern void printKinddefFlags (bool withListHeader, bool machinable, FILE *fp);
extern bool doesParserRequireMemoryStream (const langType language);
extern bool parseFile (const char *const fileName);
extern bool parseFileWithMio (const char *const fileName, MIO *mio, void *clientData);
extern bool parseRawBuffer(const char *fileName, unsigned char *buffer,
			    size_t bufferSize, const langType language, void *clientData);

extern bool runParserInNarrowedInputStream (const langType language,
					       unsigned long startLine, long startCharOffset,
					       unsigned long endLine, long endCharOffset,
					       unsigned long sourceLineOffset,
					       int promise);

#ifdef HAVE_ICONV
extern void freeEncodingResources (void);
#endif

/* Regex interface */
extern bool processLanguageRegexOption (langType language, enum regexParserType regptype, const char *const parameter);
extern void notifyLanguageRegexInputStart (langType language);
extern void notifyLanguageRegexInputEnd (langType language);

extern void matchLanguageRegex (const langType language, const vString* const line);
extern void freeRegexResources (void);
extern bool checkRegex (void);
extern void useRegexMethod (const langType language);
extern void printRegexFlags (bool withListHeader, bool machinable, const char *flags, FILE *fp);
extern void printMultilineRegexFlags (bool withListHeader, bool machinable, const char *flags, FILE *fp);
extern void printMultitableRegexFlags (bool withListHeader, bool machinable, const char *flags, FILE *fp);
extern bool doesLanguageExpectCorkInRegex (const langType language);

/* Multiline Regex Interface */
extern bool hasLanguageMultilineRegexPatterns (const langType language);
extern void matchLanguageMultilineRegex (const langType language, const vString* const allLines);
extern void matchLanguageMultitableRegex (const langType language, const vString* const allLines);

extern void processLanguageMultitableExtendingOption (langType language, const char *const parameter);

extern unsigned int   getXpathFileSpecCount (const langType language);
extern xpathFileSpec* getXpathFileSpec (const langType language, unsigned int nth);

const tagXpathTableTable *getXpathTableTable (const langType language, unsigned int nth);

extern bool makeKindSeparatorsPseudoTags (const langType language,
					     const ptagDesc *pdesc);
extern bool makeKindDescriptionsPseudoTags (const langType language,
					       const ptagDesc *pdesc);
extern bool makeFieldDescriptionsPseudoTags (const langType language,
					       const ptagDesc *pdesc);
extern bool makeExtraDescriptionsPseudoTags (const langType language,
					       const ptagDesc *pdesc);
extern bool makeRoleDescriptionsPseudoTags (const langType language,
					       const ptagDesc *pdesc);

extern void printLanguageMultitableStatistics (langType language);
extern void printParserStatisticsIfUsed (langType lang);

#endif	/* CTAGS_MAIN_PARSE_PRIVATE_H */
