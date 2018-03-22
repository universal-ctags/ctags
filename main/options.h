/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to option processing.
*/
#ifndef CTAGS_MAIN_OPTIONS_H
#define CTAGS_MAIN_OPTIONS_H

#if defined(OPTION_WRITE)
# define CONST_OPTION
#else
# define CONST_OPTION const
#endif

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdarg.h>

#include "args.h"
#include "field.h"
#include "fmt.h"
#include "parse.h"
#include "strlist.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/

typedef enum { OPTION_NONE, OPTION_SHORT, OPTION_LONG } optionType;

typedef struct sCookedArgs {
	/* private */
	Arguments* args;
	char *shortOptions;
	char simple[2];
	bool isOption;
	bool longOption;
	const char* parameter;
	/* public */
	char* item;
} cookedArgs;

typedef enum eLocate {
	EX_MIX,      /* line numbers for defines, patterns otherwise */
	EX_LINENUM,  /* -n  only line numbers in tag file */
	EX_PATTERN   /* -N  only patterns in tag file */
} exCmd;

typedef enum sortType {
	SO_UNSORTED,
	SO_SORTED,
	SO_FOLDSORTED
} sortType;

typedef enum eTagRelative {
	TREL_NO,
	TREL_YES,
	TREL_ALWAYS,
	TREL_NEVER,
} tagRelative;

/*  This stores the command line options.
 */
typedef struct sOptionValues {
	bool append;         /* -a  append to "tags" file */
	bool backward;       /* -B  regexp patterns search backwards */
	bool etags;          /* -e  output Emacs style tags file */
	exCmd locate;           /* --excmd  EX command used to locate tag */
	bool recurse;        /* -R  recurse into directories */
	sortType sorted;        /* -u,--sort  sort tags */
	bool verbose;        /* -V  verbose */
	bool xref;           /* -x  generate xref output instead */
	fmtElement *customXfmt;	/* compiled code for --xformat=XFMT */
	char *fileList;         /* -L  name of file containing names of files */
	char *tagFileName;      /* -o  name of tags file */
	stringList* headerExt;  /* -h  header extensions */
	stringList* etagsInclude;/* --etags-include  list of TAGS files to include*/
	unsigned int tagFileFormat;/* --format  tag file format (level) */
#ifdef HAVE_ICONV
	char *inputEncoding;	/* --input-encoding	convert text into --output-encoding */
	char *outputEncoding;	/* --output-encoding	write tags file as this encoding */
#endif
	langType language;      /* --lang specified language override */
	bool followLinks;    /* --link  follow symbolic links? */
	bool filter;         /* --filter  behave as filter: files in, tags out */
	char* filterTerminator; /* --filter-terminator  string to output */
	tagRelative tagRelative;    /* --tag-relative file paths relative to tag file */
	bool printTotals;    /* --totals  print cumulative statistics */
	bool lineDirectives; /* --linedirectives  process #line directives */
	bool printLanguage;  /* --print-language */
	bool guessLanguageEagerly; /* --guess-language-eagerly|-G */
	bool quiet;		      /* --quiet */
	bool fatalWarnings;	/* --_fatal-warnings */
	unsigned int patternLengthLimit; /* --pattern-length-limit=N */
	bool putFieldPrefix;		 /* --put-field-prefix */
	unsigned int maxRecursionDepth; /* --maxdepth=<max-recursion-depth> */
	enum interactiveMode { INTERACTIVE_NONE = 0,
						   INTERACTIVE_DEFAULT,
						   INTERACTIVE_SANDBOX, } interactive; /* --interactive */
#ifdef DEBUG
	long debugLevel;        /* -d  debugging output */
	unsigned long breakLine;/* -b  input line at which to call lineBreak() */
#endif
} optionValues;

typedef enum eOptionLoadingStage {
	OptionLoadingStageNone,
	OptionLoadingStageCustom,
	OptionLoadingStageDosCnf,
	OptionLoadingStageEtc,
	OptionLoadingStageLocalEtc,
	OptionLoadingStageHomeRecursive,
	OptionLoadingStageCurrentRecursive,
	OptionLoadingStagePreload,
	OptionLoadingStageEnvVar,
	OptionLoadingStageCmdline,
} OptionLoadingStage;

/*
*   GLOBAL VARIABLES
*/
extern CONST_OPTION optionValues		Option;

/*
*   FUNCTION PROTOTYPES
*/
extern void notice (const char *const format, ...) CTAGS_ATTR_PRINTF (1, 2);
extern void verbose (const char *const format, ...) CTAGS_ATTR_PRINTF (1, 2);
#define BEGIN_VERBOSE(VFP) do { if (Option.verbose) { \
                                FILE* VFP = stderr
#define END_VERBOSE()      } } while (0)

extern void freeList (stringList** const pString);
extern void setDefaultTagFileName (void);
extern void checkOptions (void);
extern bool filesRequired (void);
extern void testEtagsInvocation (void);

extern cookedArgs* cArgNewFromString (const char* string);
extern cookedArgs* cArgNewFromArgv (char* const* const argv);
extern cookedArgs* cArgNewFromFile (FILE* const fp);
extern cookedArgs* cArgNewFromLineFile (FILE* const fp);
extern void cArgDelete (cookedArgs* const current);
extern bool cArgOff (cookedArgs* const current);
extern bool cArgIsOption (cookedArgs* const current);
extern const char* cArgItem (cookedArgs* const current);
extern void cArgForth (cookedArgs* const current);

extern bool isExcludedFile (const char* const name);
extern bool isIncludeFile (const char *const fileName);
extern void parseCmdlineOptions (cookedArgs* const cargs);
extern void previewFirstOption (cookedArgs* const cargs);
extern void readOptionConfiguration (void);
extern void initOptions (void);
extern void freeOptionResources (void);

extern langType getLanguageComponentInOption (const char *const option,
					      const char *const prefix);

extern void processLanguageDefineOption (const char *const option, const char *const parameter);
extern bool processMapOption (const char *const option, const char *const parameter);
extern bool processParamOption (const char *const option, const char *const value);
extern bool processKinddefOption (const char *const option, const char *const parameter);
extern bool processKindsOption (const char *const option, const char *const parameter);
extern bool processExtradefOption (const char *const option, const char *const parameter);
extern bool processFielddefOption (const char *const option, const char *const parameter);
extern bool processAliasOption (const char *const option, const char *const parameter);
extern bool processTabledefOption (const char *const option, const char *const parameter);
#ifdef HAVE_ICONV
extern bool processLanguageEncodingOption (const char *const option, const char *const parameter);
#endif
extern bool processRoledefOption (const char *const option, const char *const parameter);

typedef void (* mainLoopFunc) (cookedArgs *args, void *data);
extern void setMainLoop (mainLoopFunc func, void *data);

#endif  /* CTAGS_MAIN_OPTIONS_H */
