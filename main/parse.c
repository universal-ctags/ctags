/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for managing input languages and
*   dispatching files to the appropriate language parser.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "flags.h"
#include "keyword.h"
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parsers.h"
#include "promise.h"
#include "ptag.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#ifdef HAVE_ICONV
# include "mbcs.h"
#endif
#include "xtag.h"

/*
 * DATA TYPES
 */
enum specType {
	SPEC_NONE,
	SPEC_NAME,
	SPEC_ALIAS = SPEC_NAME,
	SPEC_EXTENSION,
	SPEC_PATTERN,
};
const char *specTypeName [] = {
	"none", "name", "extension", "pattern"
};

typedef struct {
	langType lang;
	const char* spec;
	enum specType specType;
}  parserCandidate;

/*
 * FUNCTION PROTOTYPES
 */

static void addParserPseudoTags (langType language);
static void installKeywordTable (const langType language);
static void installTagRegexTable (const langType language);
static void installTagXpathTable (const langType language);

/*
*   DATA DEFINITIONS
*/
static parserDefinition *CTagsSelfTestParser (void);
static parserDefinitionFunc* BuiltInParsers[] = {
	CTagsSelfTestParser,
	PARSER_LIST,
	XML_PARSER_LIST
};
static parserDefinition** LanguageTable = NULL;
static unsigned int LanguageCount = 0;
static kindOption defaultFileKind = {
	.enabled     = FALSE,
	.letter      = KIND_FILE_DEFAULT,
	.name        = KIND_FILE_DEFAULT_LONG,
	.description = KIND_FILE_DEFAULT_LONG,
};

/*
*   FUNCTION DEFINITIONS
*/

extern unsigned int countParsers (void)
{
	return LanguageCount;
}

extern int makeSimpleTag (
		const vString* const name, kindOption* const kinds, const int kind)
{
	int r = CORK_NIL;

	if (kinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
	    tagEntryInfo e;
	    initTagEntry (&e, vStringValue (name), & kinds [kind]);

	    r = makeTagEntry (&e);
	}
	return r;
}

extern int makeSimpleRefTag (const vString* const name, kindOption* const kinds, const int kind,
			     int roleIndex)
{
	int r = CORK_NIL;

	if (! isXtagEnabled (XTAG_REFERENCE_TAGS))
		return r;

	Assert (roleIndex < kinds[kind].nRoles);

	if (kinds[kind].roles[roleIndex].enabled)
	{
	    tagEntryInfo e;
	    initRefTagEntry (&e, vStringValue (name), & kinds [kind], roleIndex);

	    r = makeTagEntry (&e);
	}
	return r;
}

extern boolean isLanguageEnabled (const langType language)
{
	const parserDefinition* const lang = LanguageTable [language];

	if (!lang->enabled)
		return FALSE;

	if (lang->method & METHOD_XCMD)
		initializeParser (language);

	if ((lang->method & METHOD_XCMD) &&
		 (!(lang->method & METHOD_XCMD_AVAILABLE)) &&
		 (lang->kinds == NULL) &&
		 (!(lang->method & METHOD_REGEX)) &&
	         (!(lang->method & METHOD_XPATH)))
		return FALSE;
	else
		return TRUE;
}

/*
*   parserDescription mapping management
*/

extern parserDefinition* parserNew (const char* name)
{
	return parserNewFull (name, 0);
}

static kindOption* fileKindNew (char letter)
{
	kindOption *fileKind;

	fileKind = xMalloc (1, kindOption);
	*(fileKind) = defaultFileKind;
	fileKind->letter = letter;
	return fileKind;
}

extern parserDefinition* parserNewFull (const char* name, char fileKind)
{
	parserDefinition* result = xCalloc (1, parserDefinition);
	result->name = eStrdup (name);

	if (fileKind)
		result->fileKind = fileKindNew(fileKind);
	else
		result->fileKind = &defaultFileKind;
	result->enabled = TRUE;
	return result;
}

extern boolean doesLanguageAllowNullTag (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	return LanguageTable [language]->allowNullTag;
}

extern boolean doesLanguageRequestAutomaticFQTag (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	return LanguageTable [language]->requestAutomaticFQTag;
}

extern const char *getLanguageName (const langType language)
{
	const char* result;
	if (language == LANG_IGNORE)
		result = "unknown";
	else
	{
		Assert (0 <= language  &&  language < (int) LanguageCount);
		result = LanguageTable [language]->name;
	}
	return result;
}

extern kindOption* getLanguageFileKind (const langType language)
{
	kindOption* kind;

	Assert (0 <= language  &&  language < (int) LanguageCount);

	kind = LanguageTable [language]->fileKind;

	Assert (kind != KIND_NULL);

	return kind;
}

extern langType getNamedLanguage (const char *const name, size_t len)
{
	langType result = LANG_IGNORE;
	unsigned int i;
	Assert (name != NULL);

	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		const parserDefinition* const lang = LanguageTable [i];
		if (lang->name != NULL)
		{
			if (len == 0)
			{
				if (strcasecmp (name, lang->name) == 0)
					result = i;
			}
			else
			{
				vString* vstr = vStringNewInit (name);
				vStringTruncate (vstr, len);

				if (strcasecmp (vStringValue (vstr), lang->name) == 0)
					result = i;
				vStringDelete (vstr);
			}
		}
	}
	return result;
}

static langType getNameOrAliasesLanguageAndSpec (const char *const key, langType start_index,
						 const char **const spec, enum specType *specType)
{
	langType result = LANG_IGNORE;
	unsigned int i;


	if (start_index == LANG_AUTO)
	        start_index = 0;
	else if (start_index == LANG_IGNORE || start_index >= (int) LanguageCount)
		return result;

	for (i = start_index  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		const parserDefinition* const lang = LanguageTable [i];
		stringList* const aliases = lang->currentAliaes;
		vString* tmp;

		/* isLanguageEnabled is not used here.
		   It calls initializeParser which takes
		   cost. */
		if (! lang->enabled)
			continue;

		if (lang->name != NULL && strcasecmp (key, lang->name) == 0)
		{
			result = i;
			*spec = lang->name;
			*specType = SPEC_NAME;
		}
		else if (aliases != NULL  &&  (tmp = stringListFileFinds (aliases, key)))
		{
			result = i;
			*spec = vStringValue(tmp);
			*specType = SPEC_ALIAS;
		}
	}
	return result;
}

static langType getPatternLanguageAndSpec (const char *const baseName, langType start_index,
					   const char **const spec, enum specType *specType)
{
	langType result = LANG_IGNORE;
	unsigned int i;

	if (start_index == LANG_AUTO)
	        start_index = 0;
	else if (start_index == LANG_IGNORE || start_index >= (int) LanguageCount)
		return result;

	*spec = NULL;
	for (i = start_index  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const ptrns = LanguageTable [i]->currentPatterns;
		vString* tmp;

		/* isLanguageEnabled is not used here.
		   It calls initializeParser which takes
		   cost. */
		if (! LanguageTable [i]->enabled)
			continue;

		if (ptrns != NULL && (tmp = stringListFileFinds (ptrns, baseName)))
		{
			result = i;
			*spec = vStringValue(tmp);
			*specType = SPEC_PATTERN;
			goto found;
		}
	}

	for (i = start_index  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		vString* tmp;

		/* isLanguageEnabled is not used here.
		   It calls initializeParser which takes
		   cost. */
		if (! LanguageTable [i]->enabled)
			continue;

		if (exts != NULL && (tmp = stringListExtensionFinds (exts,
								     fileExtension (baseName))))
		{
			result = i;
			*spec = vStringValue(tmp);
			*specType = SPEC_EXTENSION;
			goto found;
		}
	}
found:
	return result;
}

static parserCandidate* parserCandidateNew(unsigned int count CTAGS_ATTR_UNUSED)
{
	parserCandidate* candidates;
	unsigned int i;

	candidates= xMalloc(LanguageCount, parserCandidate);
	for (i = 0; i < LanguageCount; i++)
	{
		candidates[i].lang = LANG_IGNORE;
		candidates[i].spec = NULL;
		candidates[i].specType = SPEC_NONE;
	}
	return candidates;
}

/* If multiple parsers are found, return LANG_AUTO */
static unsigned int nominateLanguageCandidates (const char *const key, parserCandidate** candidates)
{
	unsigned int count;
	langType i;
	const char* spec = NULL;
	enum specType specType = SPEC_NONE;

	*candidates = parserCandidateNew(LanguageCount);

	for (count = 0, i = LANG_AUTO; i != LANG_IGNORE; )
	{
		i = getNameOrAliasesLanguageAndSpec (key, i, &spec, &specType);
		if (i != LANG_IGNORE)
		{
			(*candidates)[count].lang = i++;
			(*candidates)[count].spec = spec;
			(*candidates)[count++].specType = specType;
		}
	}

	return count;
}

static unsigned int
nominateLanguageCandidatesForPattern(const char *const baseName, parserCandidate** candidates)
{
	unsigned int count;
	langType i;
	const char* spec;
	enum specType specType = SPEC_NONE;

	*candidates = parserCandidateNew(LanguageCount);

	for (count = 0, i = LANG_AUTO; i != LANG_IGNORE; )
	{
		i = getPatternLanguageAndSpec (baseName, i, &spec, &specType);
		if (i != LANG_IGNORE)
		{
			(*candidates)[count].lang = i++;
			(*candidates)[count].spec = spec;
			(*candidates)[count++].specType = specType;
		}
	}
	return count;
}

static vString* extractEmacsModeAtFirstLine(MIO* input);

/*  The name of the language interpreter, either directly or as the argument
 *  to "env".
 */
static vString* determineInterpreter (const char* const cmd)
{
	vString* const interpreter = vStringNew ();
	const char* p = cmd;
	do
	{
		vStringClear (interpreter);
		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		for ( ;  *p != '\0'  &&  ! isspace ((int) *p)  ;  ++p)
			vStringPut (interpreter, (int) *p);
		vStringTerminate (interpreter);
	} while (strcmp (vStringValue (interpreter), "env") == 0);
	return interpreter;
}

static vString* extractInterpreter (MIO* input)
{
	vString* const vLine = vStringNew ();
	const char* const line = readLineRaw (vLine, input);
	vString* interpreter = NULL;

	if (line != NULL  &&  line [0] == '#'  &&  line [1] == '!')
	{
		/* "48.2.4.1 Specifying File Variables" of Emacs info:
		   ---------------------------------------------------
		   In shell scripts, the first line is used to
		   identify the script interpreter, so you
		   cannot put any local variables there.  To
		   accommodate this, Emacs looks for local
		   variable specifications in the _second_
		   line if the first line specifies an
		   interpreter.  */

		interpreter = extractEmacsModeAtFirstLine(input);
		if (!interpreter)
		{
			const char* const lastSlash = strrchr (line, '/');
			const char *const cmd = lastSlash != NULL ? lastSlash+1 : line+2;
			interpreter = determineInterpreter (cmd);
		}
	}
	vStringDelete (vLine);
	return interpreter;
}

static vString* determineEmacsModeAtFirstLine (const char* const line)
{
	vString* mode = vStringNew ();

	const char* p = strstr(line, "-*-");
	if (p == NULL)
		goto out;
	p += strlen("-*-");

	for ( ;  isspace ((int) *p)  ;  ++p)
		;  /* no-op */

	if (strncmp(p, "mode:", strlen("mode:")) == 0)
	{
		/* -*- mode: MODE; -*- */
		p += strlen("mode:");
		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		for ( ;  *p != '\0'  &&  (isalnum ((int) *p)  || *p == '-')  ;  ++p)
			vStringPut (mode, (int) *p);
		vStringTerminate (mode);
	}
	else
	{
		/* -*- MODE -*- */
		const char* end = strstr (p, "-*-");

		if (end == NULL)
			goto out;

		for ( ;  p < end &&  (isalnum ((int) *p) || *p == '-')  ;  ++p)
			vStringPut (mode, (int) *p);
		vStringTerminate (mode);

		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		if (strncmp(p, "-*-", strlen("-*-")) != 0)
			vStringClear (mode);
	}

	vStringLower (mode);

out:
	return mode;

}

static vString* extractEmacsModeAtFirstLine(MIO* input)
{
	vString* const vLine = vStringNew ();
	const char* const line = readLineRaw (vLine, input);
	vString* mode = NULL;
	if (line != NULL)
		mode = determineEmacsModeAtFirstLine (line);
	vStringDelete (vLine);

	if (mode && (vStringLength(mode) == 0))
	{
		vStringDelete(mode);
		mode = NULL;
	}
	return mode;
}

static vString* determineEmacsModeAtEOF (MIO* const fp)
{
	vString* const vLine = vStringNew ();
	const char* line;
	boolean headerFound = FALSE;
	const char* p;
	vString* mode = vStringNew ();

	while ((line = readLineRaw (vLine, fp)) != NULL)
	{
		if (headerFound && ((p = strstr (line, "mode:")) != NULL))
		{
			vStringClear (mode);
			headerFound = FALSE;

			p += strlen ("mode:");
			for ( ;  isspace ((int) *p)  ;  ++p)
				;  /* no-op */
			for ( ;  *p != '\0'  &&  (isalnum ((int) *p)  || *p == '-')  ;  ++p)
				vStringPut (mode, (int) *p);
			vStringTerminate (mode);
		}
		else if (headerFound && (p = strstr(line, "End:")))
			headerFound = FALSE;
		else if (strstr (line, "Local Variables:"))
			headerFound = TRUE;
	}
	vStringDelete (vLine);
	return mode;
}

static vString* extractEmacsModeLanguageAtEOF (MIO* input)
{
	vString* mode;

	/* "48.2.4.1 Specifying File Variables" of Emacs info:
	   ---------------------------------------------------
	   you can define file local variables using a "local
	   variables list" near the end of the file.  The start of the
	   local variables list should be no more than 3000 characters
	   from the end of the file, */
	mio_seek(input, -3000, SEEK_END);

	mode = determineEmacsModeAtEOF (input);
	if (mode && (vStringLength (mode) == 0))
	{
		vStringDelete (mode);
		mode = NULL;
	}

	return mode;
}

static vString* determineVimFileType (const char *const modeline)
{
	/* considerable combinations:
	   --------------------------
	   ... filetype=
	   ... ft= */

	unsigned int i;
	const char* p;

	const char* const filetype_prefix[] = {"filetype=", "ft="};
	vString* const filetype = vStringNew ();

	for (i = 0; i < ARRAY_SIZE(filetype_prefix); i++)
	{
		if ((p = strrstr(modeline, filetype_prefix[i])) == NULL)
			continue;

		p += strlen(filetype_prefix[i]);
		for ( ;  *p != '\0'  &&  isalnum ((int) *p)  ;  ++p)
			vStringPut (filetype, (int) *p);
		vStringTerminate (filetype);
		break;
	}
	return filetype;
}

static vString* extractVimFileType(MIO* input)
{
	/* http://vimdoc.sourceforge.net/htmldoc/options.html#modeline

	   [text]{white}{vi:|vim:|ex:}[white]se[t] {options}:[text]
	   options=> filetype=TYPE or ft=TYPE

	   'modelines' 'mls'	number	(default 5)
			global
			{not in Vi}
	    If 'modeline' is on 'modelines' gives the number of lines that is
	    checked for set commands. */

	vString* filetype = NULL;
#define RING_SIZE 5
	vString* ring[RING_SIZE];
	int i, j;
	unsigned int k;
	const char* const prefix[] = {
		"vim:", "vi:", "ex:"
	};

	for (i = 0; i < RING_SIZE; i++)
		ring[i] = vStringNew ();

	i = 0;
	while ((readLineRaw (ring[i++], input)) != NULL)
		if (i == RING_SIZE)
			i = 0;

	j = i;
	do
	{
		const char* p;

		j--;
		if (j < 0)
			j = RING_SIZE - 1;

		for (k = 0; k < ARRAY_SIZE(prefix); k++)
			if ((p = strstr (vStringValue (ring[j]), prefix[k])) != NULL)
			{
				p += strlen(prefix[k]);
				for ( ;  isspace ((int) *p)  ;  ++p)
					;  /* no-op */
				filetype = determineVimFileType(p);
				break;
			}
	} while (((i == RING_SIZE)? (j != RING_SIZE - 1): (j != i)) && (!filetype));

	for (i = RING_SIZE - 1; i >= 0; i--)
		vStringDelete (ring[i]);
#undef RING_SIZE

	if (filetype && (vStringLength (filetype) == 0))
	{
		vStringDelete (filetype);
		filetype = NULL;
	}
	return filetype;

	/* TODO:
	   [text]{white}{vi:|vim:|ex:}[white]{options} */
}

static vString* determineZshAutoloadTag (const char *const modeline)
{
	/* See "Autoloaded files" in zsh info.
	   -------------------------------------
	   #compdef ...
	   #autoload [ OPTIONS ] */

	if (((strncmp (modeline, "#compdef", 8) == 0) && isspace (*(modeline + 8)))
	    || ((strncmp (modeline, "#autoload", 9) == 0)
		&& (isspace (*(modeline + 9)) || *(modeline + 9) == '\0')))
		return vStringNewInit ("zsh");
	else
		return NULL;
}

static vString* extractZshAutoloadTag(MIO* input)
{
	vString* const vLine = vStringNew ();
	const char* const line = readLineRaw (vLine, input);
	vString* mode = NULL;

	if (line)
		mode = determineZshAutoloadTag (line);

	vStringDelete (vLine);
	return mode;
}

struct getLangCtx {
    const char *fileName;
    MIO        *input;
    boolean     err;
};

#define GLC_FOPEN_IF_NECESSARY0(_glc_, _label_) do {        \
    if (!(_glc_)->input) {                                  \
	    (_glc_)->input = getMio((_glc_)->fileName, "rb", FALSE);	\
        if (!(_glc_)->input) {                              \
            (_glc_)->err = TRUE;                            \
            goto _label_;                                   \
        }                                                   \
    }                                                       \
} while (0)                                                 \

#define GLC_FOPEN_IF_NECESSARY(_glc_, _label_, _doesParserRequireMemoryStream_) \
	do {								\
		if (!(_glc_)->input)					\
			GLC_FOPEN_IF_NECESSARY0 (_glc_, _label_);	\
		if ((_doesParserRequireMemoryStream_) &&		\
		    (mio_memory_get_data((_glc_)->input, NULL) == NULL)) \
		{							\
			MIO *tmp_ = (_glc_)->input;			\
			(_glc_)->input = mio_new_mio (tmp_, 0, 0);	\
			mio_free (tmp_);				\
			if (!(_glc_)->input) {				\
				(_glc_)->err = TRUE;			\
				goto _label_;				\
			}						\
		}							\
	} while (0)

#define GLC_FCLOSE(_glc_) do {                              \
    if ((_glc_)->input) {                                   \
        mio_free((_glc_)->input);                             \
        (_glc_)->input = NULL;                              \
    }                                                       \
} while (0)

static const struct taster {
	vString* (* taste) (MIO *);
        const char     *msg;
} eager_tasters[] = {
        {
		.taste  = extractInterpreter,
		.msg    = "interpreter",
        },
	{
		.taste  = extractZshAutoloadTag,
		.msg    = "zsh autoload tag",
	},
        {
		.taste  = extractEmacsModeAtFirstLine,
		.msg    = "emacs mode at the first line",
        },
        {
		.taste  = extractEmacsModeLanguageAtEOF,
		.msg    = "emacs mode at the EOF",
        },
        {
		.taste  = extractVimFileType,
		.msg    = "vim modeline",
        },
};
static langType tasteLanguage (struct getLangCtx *glc, const struct taster *const tasters, int n_tasters,
			      langType *fallback);

/* If all the candidates have the same specialized language selector, return
 * it.  Otherwise, return NULL.
 */
static boolean
hasTheSameSelector (langType lang, selectLanguage candidate_selector)
{
	selectLanguage *selector;

	selector = LanguageTable[ lang ]->selectLanguage;
	if (selector == NULL)
		return FALSE;

	while (*selector)
	{
		if (*selector == candidate_selector)
			return TRUE;
		selector++;
	}
	return FALSE;
}

static selectLanguage
commonSelector (const parserCandidate *candidates, int n_candidates)
{
    Assert (n_candidates > 1);
    selectLanguage *selector;
    int i;

    selector = LanguageTable[ candidates[0].lang ]->selectLanguage;
    if (selector == NULL)
	    return NULL;

    while (*selector)
    {
	    for (i = 1; i < n_candidates; ++i)
		    if (! hasTheSameSelector (candidates[i].lang, *selector))
			    break;
	    if (i == n_candidates)
		    return *selector;
	    selector++;
    }
    return NULL;
}


/* Calls the selector and returns the integer value of the parser for the
 * language associated with the string returned by the selector.
 */
static int
pickLanguageBySelection (selectLanguage selector, MIO *input)
{
    const char *lang = selector(input);
    if (lang)
    {
        verbose ("		selection: %s\n", lang);
        return getNamedLanguage(lang, 0);
    }
    else
    {
	verbose ("		no selection\n");
        return LANG_IGNORE;
    }
}

static int compareParsersByName (const void *a, const void* b)
{
	parserDefinition *const *la = a, *const *lb = b;
	return strcasecmp ((*la)->name, (*lb)->name);
}

static int sortParserCandidatesBySpecType (const void *a, const void *b)
{
	const parserCandidate *ap = a, *bp = b;
	if (ap->specType > bp->specType)
		return -1;
	else if (ap->specType == bp->specType)
	{
		/* qsort, the function calling this function,
		   doesn't do "stable sort". To make the result of
		   sorting predictable, compare the names of parsers
		   when their specType is the same. */
		parserDefinition *la = LanguageTable [ap->lang];
		parserDefinition *lb = LanguageTable [bp->lang];
		return compareParsersByName (&la, &lb);
	}
	else
		return 1;
}

static unsigned int sortAndFilterParserCandidates (parserCandidate  *candidates,
						   unsigned int n_candidates)
{
	enum specType highestSpecType;
	unsigned int i;
	unsigned int r;

	if (n_candidates < 2)
		return n_candidates;

	qsort (candidates, n_candidates, sizeof(*candidates),
	       sortParserCandidatesBySpecType);

	highestSpecType = candidates [0].specType;
	r = 1;
	for (i = 1; i < n_candidates; i++)
	{
		if (candidates[i].specType == highestSpecType)
			r++;
	}
	return r;
}

static void verboseReportCandidate (const char *header,
				    parserCandidate  *candidates,
				    unsigned int n_candidates)
{
	unsigned int i;
	verbose ("		#%s: %u\n", header, n_candidates);
	for (i = 0; i < n_candidates; i++)
		verbose ("			%u: %s (%s: \"%s\")\n",
			 i,
			 LanguageTable[candidates[i].lang]->name,
			 specTypeName [candidates[i].specType],
			 candidates[i].spec);
}

static boolean doesCandidatesRequireMemoryStream(const parserCandidate *candidates,
						 int n_candidates)
{
	int i;

	for (i = 0; i < n_candidates; i++)
		if (doesParserRequireMemoryStream (candidates[i].lang))
			return TRUE;

	return FALSE;
}

static langType getSpecLanguageCommon (const char *const spec, struct getLangCtx *glc,
				       unsigned int nominate (const char *const, parserCandidate**),
				       langType *fallback)
{
	langType language;
	parserCandidate  *candidates;
	unsigned int n_candidates;

	if (fallback)
		*fallback = LANG_IGNORE;

	n_candidates = (*nominate)(spec, &candidates);
	verboseReportCandidate ("candidates",
				candidates, n_candidates);

	n_candidates = sortAndFilterParserCandidates (candidates, n_candidates);
	verboseReportCandidate ("candidates after sorting and filtering",
				candidates, n_candidates);

	if (n_candidates == 1)
	{
		language = candidates[0].lang;
	}
	else if (n_candidates > 1)
	{
		selectLanguage selector = commonSelector(candidates, n_candidates);
		boolean memStreamRequired = doesCandidatesRequireMemoryStream (candidates,
									       n_candidates);

		GLC_FOPEN_IF_NECESSARY(glc, fopen_error, memStreamRequired);
		if (selector) {
			verbose ("	selector: %p\n", selector);
			language = pickLanguageBySelection(selector, glc->input);
		} else {
			verbose ("	selector: NONE\n");
		fopen_error:
			language = LANG_IGNORE;
		}

		Assert(language != LANG_AUTO);

		if (fallback)
			*fallback = candidates[0].lang;
	}
	else
	{
		language = LANG_IGNORE;
	}

	eFree(candidates);
	candidates = NULL;

	return language;
}

static langType getSpecLanguage (const char *const spec,
                                 struct getLangCtx *glc,
				 langType *fallback)
{
	return getSpecLanguageCommon(spec, glc, nominateLanguageCandidates,
				     fallback);
}

static langType getPatternLanguage (const char *const baseName,
                                    struct getLangCtx *glc,
				    langType *fallback)
{
	return getSpecLanguageCommon(baseName, glc,
				     nominateLanguageCandidatesForPattern,
				     fallback);
}

/* This function tries to figure out language contained in a file by
 * running a series of tests, trying to find some clues in the file.
 */
static langType
tasteLanguage (struct getLangCtx *glc, const struct taster *const tasters, int n_tasters,
	      langType *fallback)
{
    int i;

    if (fallback)
	    *fallback = LANG_IGNORE;
    for (i = 0; i < n_tasters; ++i) {
        langType language;
        vString* spec;

        mio_rewind(glc->input);
	spec = tasters[i].taste(glc->input);

        if (NULL != spec) {
            verbose ("	%s: %s\n", tasters[i].msg, vStringValue (spec));
            language = getSpecLanguage (vStringValue (spec), glc,
					(fallback && (*fallback == LANG_IGNORE))? fallback: NULL);
            vStringDelete (spec);
            if (language != LANG_IGNORE)
                return language;
        }
    }

    return LANG_IGNORE;
}

static langType
getFileLanguageInternal (const char *const fileName, MIO **mio)
{
    langType language;

    /* ctags tries variety ways(HINTS) to choose a proper language
       for given fileName. If multiple candidates are chosen in one of
       the hint, a SELECTOR common between the candidate languages
       is called.

       "selection failure" means a selector common between the
       candidates doesn't exist or the common selector returns NULL.

       "hint failure" means the hint finds no candidate or
       "selection failure" occurs though the hint finds multiple
       candidates.

       If a hint chooses multiple candidates, and selection failure is
       occured, the hint records one of the candidates as FALLBACK for
       the hint. (The candidates are stored in an array. The first
       element of the array is recorded. However, there is no
       specification about the order of elements in the array.)

       If all hints are failed, FALLBACKs of the hints are examined.
       Which fallbacks should be chosen?  `enum hint' defines the order. */
    enum hint {
	    HINT_INTERP,
	    HINT_OTHER,
	    HINT_FILENAME,
	    HINT_TEMPLATE,
	    N_HINTS,
    };
    langType fallback[N_HINTS];
    int i;
    struct getLangCtx glc = {
        .fileName = fileName,
        .input    = NULL,
        .err      = FALSE,
    };
    const char* const baseName = baseFilename (fileName);
    char *templateBaseName = NULL;
    fileStatus *fstatus = NULL;

    for (i = 0; i < N_HINTS; i++)
	fallback [i] = LANG_IGNORE;

    verbose ("Get file language for %s\n", fileName);

    verbose ("	pattern: %s\n", baseName);
    language = getPatternLanguage (baseName, &glc,
				   fallback + HINT_FILENAME);
    if (language != LANG_IGNORE || glc.err)
        goto cleanup;

    {
        const char* const tExt = ".in";
        templateBaseName = baseFilenameSansExtensionNew (fileName, tExt);
        if (templateBaseName)
        {
            verbose ("	pattern + template(%s): %s\n", tExt, templateBaseName);
            GLC_FOPEN_IF_NECESSARY(&glc, cleanup, FALSE);
            mio_rewind(glc.input);
            language = getPatternLanguage(templateBaseName, &glc,
					  fallback + HINT_TEMPLATE);
            if (language != LANG_IGNORE)
                goto cleanup;
        }
    }

    fstatus = eStat (fileName);
    if (fstatus && fstatus->exists)
    {
	    if (fstatus->isExecutable || Option.guessLanguageEagerly)
	    {
		    GLC_FOPEN_IF_NECESSARY (&glc, cleanup, FALSE);
		    language = tasteLanguage(&glc, eager_tasters, 1,
					    fallback + HINT_INTERP);
	    }
	    if (language != LANG_IGNORE)
		    goto cleanup;

	    if (Option.guessLanguageEagerly)
	    {
		    GLC_FOPEN_IF_NECESSARY(&glc, cleanup, FALSE);
		    language = tasteLanguage(&glc,
					     eager_tasters + 1,
					     ARRAY_SIZE(eager_tasters) - 1,
					     fallback + HINT_OTHER);
	    }
    }


  cleanup:
    if (mio && glc.input)
	    *mio = mio_ref (glc.input);
    GLC_FCLOSE(&glc);
    if (fstatus)
	    eStatFree (fstatus);
    if (templateBaseName)
        eFree (templateBaseName);

    for (i = 0;
	 language == LANG_IGNORE && i < N_HINTS;
	 i++)
    {
        language = fallback [i];
	if (language != LANG_IGNORE)
        verbose ("	fallback[hint = %d]: %s\n", i, getLanguageName (language));
    }

    return language;
}

static langType getFileLanguageAndKeepMIO (const char *const fileName, MIO **mio)
{
	langType l = Option.language;

	if (mio)
		*mio = NULL;

	if (l == LANG_AUTO)
		return getFileLanguageInternal(fileName, mio);
	else if (! isLanguageEnabled (l))
	{
		error (FATAL,
		       "%s parser specified with --language-force is disabled or not available(xcmd)",
		       getLanguageName (l));
		/* For suppressing warnings. */
		return LANG_AUTO;
	}
	else
		return Option.language;
}

extern langType getFileLanguage (const char *const fileName)
{
	return getFileLanguageAndKeepMIO(fileName, NULL);
}

typedef void (*languageCallback)  (langType language, void* user_data);
static void foreachLanguage(languageCallback callback, void *user_data)
{
	langType result = LANG_IGNORE;

	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		const parserDefinition* const lang = LanguageTable [i];
		if (lang->name != NULL)
			callback(i, user_data);
	}
}

extern void printLanguageMap (const langType language, FILE *fp)
{
	boolean first = TRUE;
	unsigned int i;
	stringList* map = LanguageTable [language]->currentPatterns;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	for (i = 0  ;  map != NULL  &&  i < stringListCount (map)  ;  ++i)
	{
		fprintf (fp, "%s(%s)", (first ? "" : " "),
			 vStringValue (stringListItem (map, i)));
		first = FALSE;
	}
	map = LanguageTable [language]->currentExtensions;
	for (i = 0  ;  map != NULL  &&  i < stringListCount (map)  ;  ++i)
	{
		fprintf (fp, "%s.%s", (first ? "" : " "),
			 vStringValue (stringListItem (map, i)));
		first = FALSE;
	}
}

extern void installLanguageMapDefault (const langType language)
{
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentPatterns != NULL)
		stringListDelete (lang->currentPatterns);
	if (lang->currentExtensions != NULL)
		stringListDelete (lang->currentExtensions);

	if (lang->patterns == NULL)
		lang->currentPatterns = stringListNew ();
	else
	{
		lang->currentPatterns =
			stringListNewFromArgv (lang->patterns);
	}
	if (lang->extensions == NULL)
		lang->currentExtensions = stringListNew ();
	else
	{
		lang->currentExtensions =
			stringListNewFromArgv (lang->extensions);
	}
	BEGIN_VERBOSE(vfp);
	{
	printLanguageMap (language, vfp);
	putc ('\n', vfp);
	}
	END_VERBOSE();
}

extern void installLanguageMapDefaults (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		verbose ("    %s: ", getLanguageName (i));
		installLanguageMapDefault (i);
	}
}

static void printAliases (const langType language, FILE *fp);
extern void installLanguageAliasesDefault (const langType language)
{
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentAliaes != NULL)
		stringListDelete (lang->currentAliaes);

	if (lang->aliases == NULL)
		lang->currentAliaes = stringListNew ();
	else
	{
		lang->currentAliaes =
			stringListNewFromArgv (lang->aliases);
	}
	BEGIN_VERBOSE(vfp);
	printAliases (language, vfp);
	putc ('\n', vfp);
	END_VERBOSE();
}
extern void installLanguageAliasesDefaults (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		verbose ("    %s: ", getLanguageName (i));
		installLanguageAliasesDefault (i);
	}
}

extern void clearLanguageMap (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	stringListClear (LanguageTable [language]->currentPatterns);
	stringListClear (LanguageTable [language]->currentExtensions);
}

extern void clearLanguageAliases (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	stringListClear (LanguageTable [language]->currentAliaes);
}

static boolean removeLanguagePatternMap1(const langType language, const char *const pattern)
{
	boolean result = FALSE;
	stringList* const ptrn = LanguageTable [language]->currentPatterns;

	if (ptrn != NULL && stringListDeleteItemExtension (ptrn, pattern))
	{
		verbose (" (removed from %s)", getLanguageName (language));
		result = TRUE;
	}
	return result;
}

extern boolean removeLanguagePatternMap (const langType language, const char *const pattern)
{
	boolean result = FALSE;

	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
			result = removeLanguagePatternMap1 (i, pattern) || result;
	}
	else
		result = removeLanguagePatternMap1 (language, pattern);
	return result;
}

extern void addLanguagePatternMap (const langType language, const char* ptrn,
				   boolean exclusiveInAllLanguages)
{
	vString* const str = vStringNewInit (ptrn);
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (exclusiveInAllLanguages)
		removeLanguagePatternMap (LANG_AUTO, ptrn);
	stringListAdd (lang->currentPatterns, str);
}

static boolean removeLanguageExtensionMap1 (const langType language, const char *const extension)
{
	boolean result = FALSE;
	stringList* const exts = LanguageTable [language]->currentExtensions;

	if (exts != NULL  &&  stringListDeleteItemExtension (exts, extension))
	{
		verbose (" (removed from %s)", getLanguageName (language));
		result = TRUE;
	}
	return result;
}

extern boolean removeLanguageExtensionMap (const langType language, const char *const extension)
{
	boolean result = FALSE;

	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount ;  ++i)
			result = removeLanguageExtensionMap1 (i, extension) || result;
	}
	else
		result = removeLanguageExtensionMap1 (language, extension);
	return result;
}

extern void addLanguageExtensionMap (
		const langType language, const char* extension,
		boolean exclusiveInAllLanguages)
{
	vString* const str = vStringNewInit (extension);
	Assert (0 <= language  &&  language < (int) LanguageCount);
	if (exclusiveInAllLanguages)
		removeLanguageExtensionMap (LANG_AUTO, extension);
	stringListAdd (LanguageTable [language]->currentExtensions, str);
}

extern void addLanguageAlias (const langType language, const char* alias)
{
	vString* const str = vStringNewInit (alias);
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentAliaes == NULL)
		lang->currentAliaes = stringListNew ();
	stringListAdd (lang->currentAliaes, str);
}

extern void enableLanguage (const langType language, const boolean state)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	LanguageTable [language]->enabled = state;
}

extern void enableLanguages (const boolean state)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		enableLanguage (i, state);
}

#ifdef DEBUG
static boolean doesParserUseKind (const parserDefinition *const parser, char letter)
{
	unsigned int k;

	for (k = 0; k < parser->kindCount; k++)
		if (parser->kinds [k].letter == letter)
			return TRUE;
	return FALSE;
}
#endif

static void installFieldSpec (const langType language)
{
	unsigned int i;
	parserDefinition * parser;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	parser = LanguageTable [language];
	if (parser->fieldSpecCount > PRE_ALLOCATED_PARSER_FIELDS)
		error (FATAL,
		       "INTERNAL ERROR: in a parser, fields are defined more than PRE_ALLOCATED_PARSER_FIELDS\n");

	if (parser->fieldSpecs != NULL)
	{
		for (i = 0; i < parser->fieldSpecCount; i++)
			defineField (& parser->fieldSpecs [i], language);
	}
}

static void initializeParserOne (langType lang)
{
	parserDefinition *const parser = LanguageTable [lang];

	if (parser->initialized)
		return;

	verbose ("Initialize parser: %s\n", parser->name);
	parser->initialized = TRUE;

	installKeywordTable (lang);
	installTagRegexTable (lang);
	installTagXpathTable (lang);
	installFieldSpec     (lang);

	if (hasScopeActionInRegex (lang)
	    || parser->requestAutomaticFQTag)
		parser->useCork = TRUE;

	if (parser->initialize != NULL)
		parser->initialize (lang);

	initializeSubparsers (parser);

	Assert (parser->fileKind != KIND_NULL);
	Assert (!doesParserUseKind (parser, parser->fileKind->letter));
}

extern void initializeParser (langType lang)
{
	if (lang == LANG_AUTO)
	{
		int i;
		for (i = 0; i < countParsers(); i++)
			initializeParserOne (i);
	}
	else
		initializeParserOne (lang);
}

static void linkDependenciesAtInitializeParsing (parserDefinition *const parser)
{
	unsigned int i;
	parserDependency *d;
	langType upper;
	parserDefinition *upperParserDef;

	for (i = 0; i < parser->dependencyCount; i++)
	{
		d = parser->dependencies + i;
		upper = getNamedLanguage (d->upperParser, 0);
		upperParserDef = LanguageTable [upper];

		linkDependencyAtInitializeParsing (d->type, upperParserDef, parser);
	}
}

extern void initializeParsing (void)
{
	unsigned int builtInCount;
	unsigned int i;

	builtInCount = ARRAY_SIZE (BuiltInParsers);
	LanguageTable = xMalloc (builtInCount, parserDefinition*);

	verbose ("Installing parsers: ");
	for (i = 0  ;  i < builtInCount  ;  ++i)
	{
		parserDefinition* const def = (*BuiltInParsers [i]) ();
		if (def != NULL)
		{
			boolean accepted = FALSE;
			if (def->name == NULL  ||  def->name[0] == '\0')
				error (FATAL, "parser definition must contain name\n");
			else if (def->method & METHOD_NOT_CRAFTED)
			{
				def->parser = findRegexTags;
				accepted = TRUE;
			}
			else if ((!def->invisible) && (((!!def->parser) + (!!def->parser2)) != 1))
				error (FATAL,
		"%s parser definition must define one and only one parsing routine\n",
					   def->name);
			else
				accepted = TRUE;
			if (accepted)
			{
				verbose ("%s%s", i > 0 ? ", " : "", def->name);
				def->id = LanguageCount++;
				LanguageTable [def->id] = def;
			}
		}
	}
	verbose ("\n");

	for (i = 0; i < builtInCount  ;  ++i)
		linkDependenciesAtInitializeParsing (LanguageTable [i]);
}

extern void freeParserResources (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		parserDefinition* const lang = LanguageTable [i];

		if (lang->finalize)
			(lang->finalize)((langType)i, (boolean)lang->initialized);

		finalizeSubparsers (lang);

		if (lang->fileKind != &defaultFileKind)
		{
			eFree (lang->fileKind);
			lang->fileKind = NULL;
		}

		freeList (&lang->currentPatterns);
		freeList (&lang->currentExtensions);
		freeList (&lang->currentAliaes);

		eFree (lang->name);
		lang->name = NULL;
		eFree (lang);
	}
	if (LanguageTable != NULL)
		eFree (LanguageTable);
	LanguageTable = NULL;
	LanguageCount = 0;
}

static void doNothing (void)
{
}

static void lazyInitialize (langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	lang->parser = doNothing;

	if (lang->method & METHOD_REGEX)
		lang->parser = findRegexTags;
}

/*
*   Option parsing
*/
static void lang_def_flag_file_kind_long (const char* const optflag, const char* const param, void* data)
{
	parserDefinition*  def = data;

	Assert (def);
	Assert (param);
	Assert (optflag);


	if (param[0] == '\0')
		error (WARNING, "No letter specified for \"%s\" flag of --langdef option", optflag);
	else if (param[1] != '\0')
		error (WARNING, "Specify just a letter for \"%s\" flag of --langdef option", optflag);

	if (def->fileKind != &defaultFileKind)
		eFree (def->fileKind);

	def->fileKind = fileKindNew (param[0]);
}

static flagDefinition LangDefFlagDef [] = {
	{ '\0',  "fileKind", NULL, lang_def_flag_file_kind_long },
};

extern void processLanguageDefineOption (
		const char *const option, const char *const parameter CTAGS_ATTR_UNUSED)
{
	if (parameter [0] == '\0')
		error (WARNING, "No language specified for \"%s\" option", option);
	else if (getNamedLanguage (parameter, 0) != LANG_IGNORE)
		error (WARNING, "Language \"%s\" already defined", parameter);
	else
	{
		char *name;
		char *flags;
		unsigned int i;
		parserDefinition*  def;

		flags = strchr (parameter, LONG_FLAGS_OPEN);
		if (flags)
			name = eStrndup (parameter, flags - parameter);
		else
			name = eStrdup (parameter);

		i = LanguageCount++;
		def = parserNew (name);
		def->initialize        = lazyInitialize;
		def->currentPatterns   = stringListNew ();
		def->currentExtensions = stringListNew ();
		def->method            = METHOD_NOT_CRAFTED;
		def->id                = i;
		LanguageTable = xRealloc (LanguageTable, i + 1, parserDefinition*);
		LanguageTable [i] = def;

		flagsEval (flags, LangDefFlagDef, ARRAY_SIZE (LangDefFlagDef), def);

		eFree (name);
	}
}

static kindOption *langKindOption (const langType language, const int flag)
{
	unsigned int i;
	kindOption* result = NULL;
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	for (i=0  ;  i < lang->kindCount  &&  result == NULL  ;  ++i)
		if (lang->kinds [i].letter == flag)
			result = &lang->kinds [i];
	return result;
}

static kindOption *langKindLongOption (const langType language, const char *kindLong)
{
	unsigned int i;
	kindOption* result = NULL;
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	for (i=0  ;  i < lang->kindCount  &&  result == NULL  ;  ++i)
		if (strcmp (lang->kinds [i].name, kindLong) == 0)
			result = &lang->kinds [i];
	return result;
}

extern boolean isLanguageKindEnabled (const langType language, char kind)
{
	const kindOption *kindOpt;

	if (hasRegexKind (language, kind))
		return isRegexKindEnabled (language, kind);
	else if (hasXcmdKind (language, kind))
		return isXcmdKindEnabled (language, kind);

	kindOpt = langKindOption (language, kind);
	Assert (kindOpt);

	return kindOpt->enabled;
}


static void resetLanguageKinds (const langType language, const boolean mode)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	resetRegexKinds (language, mode);
	resetXcmdKinds (language, mode);
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			enableKind (lang->kinds + i, mode);
	}
}

static boolean enableLanguageKind (
		const langType language, const int kind, const boolean mode)
{
	boolean result = FALSE;
	kindOption* const opt = langKindOption (language, kind);
	if (opt != NULL)
	{
		enableKind (opt, mode);
		result = TRUE;
	}
	result = enableRegexKind (language, kind, mode)? TRUE: result;
	result = enableXcmdKind (language, kind, mode)? TRUE: result;
	return result;
}

static boolean enableLanguageKindLong (
	const langType language, const char * const kindLong, const boolean mode)
{
	boolean result = FALSE;
	kindOption* const opt = langKindLongOption (language, kindLong);
	if (opt != NULL)
	{
		enableKind (opt, mode);
		result = TRUE;
	}
	result = enableRegexKindLong (language, kindLong, mode)? TRUE: result;
	result = enableXcmdKindLong (language, kindLong, mode)? TRUE: result;
	return result;
}

static void processLangKindOption (
		const langType language, const char *const option,
		const char *const parameter)
{
	const char *p = parameter;
	boolean mode = TRUE;
	int c;
	static vString *longName;
	boolean inLongName = FALSE;
	const char *k;
	boolean r;

	Assert (0 <= language  &&  language < (int) LanguageCount);

	initializeParser (language);
	if (*p == '*')
	{
		resetLanguageKinds (language, TRUE);
		p++;
	}
	else if (*p != '+'  &&  *p != '-')
		resetLanguageKinds (language, FALSE);

	longName = vStringNewOrClear (longName);

	while ((c = *p++) != '\0')
	{
		switch (c)
		{
		case '+':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = TRUE;
			break;
		case '-':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = FALSE;
			break;
		case '{':
			if (inLongName)
				error(FATAL,
				      "unexpected character in kind specification: \'%c\'",
				      c);
			inLongName = TRUE;
			break;
		case '}':
			if (!inLongName)
				error(FATAL,
				      "unexpected character in kind specification: \'%c\'",
				      c);
			k = vStringValue (longName);
			r = enableLanguageKindLong (language, k, mode);
			if (! r)
				error (WARNING, "Unsupported kind: '%s' for --%s option",
				       k, option);

			inLongName = FALSE;
			vStringClear (longName);
			break;
		default:
			if (inLongName)
				vStringPut (longName, c);
			else
			{
				r = enableLanguageKind (language, c, mode);
				if (! r)
					error (WARNING, "Unsupported kind: '%c' for --%s option",
					       c, option);
			}
			break;
		}
	}
}

struct langKindOptionStruct {
	const char *const option;
	const char *const parameter;
};
static void processLangKindOptionEach(
	langType lang, void* user_data)
{
	struct langKindOptionStruct *arg = user_data;
	processLangKindOption (lang, arg->option, arg->parameter);
}

extern boolean processKindOption (
		const char *const option, const char *const parameter)
{
#define PREFIX "kinds-"
#define PREFIX_LEN strlen(PREFIX)

	boolean handled = FALSE;
	struct langKindOptionStruct arg = {
		.option = option,
		.parameter = parameter,
	};
	langType language;

	const char* const dash = strchr (option, '-');
	if (dash != NULL  &&
		(strcmp (dash + 1, "kinds") == 0  ||  strcmp (dash + 1, "types") == 0))
	{
		size_t len = dash - option;

		if ((len == 1) && (*option == '*'))
			foreachLanguage(processLangKindOptionEach, &arg);
		else
		{
			vString* langName = vStringNew ();
			vStringNCopyS (langName, option, len);
			language = getNamedLanguage (vStringValue (langName), 0);
			if (language == LANG_IGNORE)
				error (WARNING, "Unknown language \"%s\" in \"%s\" option", vStringValue (langName), option);
			else
				processLangKindOption (language, option, parameter);
			vStringDelete (langName);
		}
		handled = TRUE;
	}
	else if ( strncmp (option, PREFIX, PREFIX_LEN) == 0 )
	{
		const char* lang;
		size_t len;

		lang = option + PREFIX_LEN;
		len = strlen (lang);
		if (len == 0)
			error (WARNING, "No language given in \"%s\" option", option);
		else if (len == 1 && lang[0] == '*')
		{
			foreachLanguage(processLangKindOptionEach, &arg);
			handled = TRUE;
		}
		else
		{
			language = getNamedLanguage (lang, 0);
			if (language == LANG_IGNORE)
				error (WARNING, "Unknown language \"%s\" in \"%s\" option", lang, option);
			else
			{
				processLangKindOption (language, option, parameter);
				handled = TRUE;
			}
		}

	}
	return handled;
#undef PREFIX
#undef PREFIX_LEN
}

static void printRoles (const langType language, const char* letters, boolean allowMissingKind)
{
	const parserDefinition* const lang = LanguageTable [language];
	const char *c;

	if (lang->invisible)
		return;

	for (c = letters; *c != '\0'; c++)
	{
		unsigned int i;
		const kindOption *k;

		for (i = 0; i < lang->kindCount; ++i)
		{
			k = lang->kinds + i;
			if (*c == KIND_WILDCARD || k->letter == *c)
			{
				int j;
				const roleDesc *r;

				for (j = 0; j < k->nRoles; j++)
				{
					r = k->roles + j;
					printf ("%s\t%c\t", lang->name, k->letter);
					printRole (r);
				}
				if (*c != KIND_WILDCARD)
					break;
			}
		}
		if ((i == lang->kindCount) && (*c != KIND_WILDCARD) && (!allowMissingKind))
			error (FATAL, "No such letter kind in %s: %c\n", lang->name, *c);
	}
}

extern void printLanguageRoles (const langType language, const char* letters)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
			printRoles (i, letters, TRUE);
	}
	else
		printRoles (language, letters, FALSE);

}

extern void printLanguageFileKind (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];
			printf ("%s %c\n", lang->name, lang->fileKind->letter);
		}
	}
	else
		printf ("%c\n", LanguageTable [language]->fileKind->letter);
}

static void printKinds (langType language, boolean allKindFields, boolean indent)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);

	initializeParser (language);
	lang = LanguageTable [language];
	if (lang->kinds != NULL)
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
		{
			if (allKindFields && indent)
				printf (Option.machinable? "%s": PR_KIND_FMT (LANG,s), lang->name);
			printKind (lang->kinds + i, allKindFields, indent, Option.machinable);
		}
	}
	printRegexKinds (language, allKindFields, indent, Option.machinable);
	printXcmdKinds (language, allKindFields, indent, Option.machinable);
}

extern void printLanguageKinds (const langType language, boolean allKindFields)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;

		if (Option.withListHeader && allKindFields)
			printKindListHeader (TRUE, Option.machinable);

		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];

			if (lang->invisible)
				continue;

			if (!allKindFields)
				printf ("%s%s\n", lang->name, isLanguageEnabled (i) ? "" : " [disabled]");
			printKinds (i, allKindFields, TRUE);
		}
	}
	else
	{
		if (Option.withListHeader && allKindFields)
			printKindListHeader (FALSE, Option.machinable);

		printKinds (language, allKindFields, FALSE);
	}
}

static void processLangAliasOption (const langType language,
				    const char *const parameter)
{
	const char* alias;
	const parserDefinition * lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	Assert (parameter);
	Assert (parameter[0]);
	lang = LanguageTable [language];

	if (parameter[0] == '+')
	{
		alias = parameter + 1;
		addLanguageAlias(language, alias);
		verbose ("add alias %s to %s\n", alias, lang->name);
	}
	else if (parameter[0] == '-')
	{
		if (lang->currentAliaes)
		{
			alias = parameter + 1;
			if (stringListDeleteItemExtension (lang->currentAliaes, alias))
			{
				verbose ("remove alias %s from %s\n", alias, lang->name);
			}
		}
	}
	else
	{
		alias = parameter;
		clearLanguageAliases (language);
		addLanguageAlias(language, alias);
		verbose ("set alias %s to %s\n", alias, lang->name);
	}

}

extern boolean processAliasOption (
		const char *const option, const char *const parameter)
{
	langType language;

	language = getLanguageComponentInOption (option, "alias-");
	if (language == LANG_IGNORE)
		return FALSE;

	processLangAliasOption (language, parameter);
	return TRUE;
}

static void printMaps (const langType language, langmapType type)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	printf ("%-8s", lang->name);
	if (lang->currentExtensions != NULL && (type & LMAP_EXTENSION))
		for (i = 0  ;  i < stringListCount (lang->currentExtensions)  ;  ++i)
			printf (" *.%s", vStringValue (
						stringListItem (lang->currentExtensions, i)));
	if (lang->currentPatterns != NULL && (type & LMAP_PATTERN))
		for (i = 0  ;  i < stringListCount (lang->currentPatterns)  ;  ++i)
			printf (" %s", vStringValue (
						stringListItem (lang->currentPatterns, i)));
	putchar ('\n');
}

static void printAliases (const langType language, FILE *fp)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	if (lang->currentAliaes != NULL)
		for (i = 0  ;  i < stringListCount (lang->currentAliaes)  ;  ++i)
			fprintf (fp, " %s", vStringValue (
					stringListItem (lang->currentAliaes, i)));
}

extern void printLanguageMaps (const langType language, langmapType type)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
			printMaps (i, type);
	}
	else
		printMaps (language, type);
}

extern void printLanguageAliases (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
			printLanguageAliases (i);
	}
	else
	{
		const parserDefinition* lang;

		Assert (0 <= language  &&  language < (int) LanguageCount);
		lang = LanguageTable [language];
		printf ("%-8s", lang->name);
		printAliases (language, stdout);
		putchar ('\n');
	}
}

static void printLanguage (const langType language, parserDefinition** ltable)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = ltable [language];

	if (lang->invisible)
		return;

	if (lang->method & METHOD_XCMD)
		initializeParser (lang->id);

	if (lang->kinds != NULL  ||  (lang->method & METHOD_REGEX) || (lang->method & METHOD_XCMD))
		printf ("%s%s\n", lang->name, isLanguageEnabled (lang->id) ? "" : " [disabled]");
}

extern void printLanguageList (void)
{
	unsigned int i;
	parserDefinition **ltable;

	ltable = xMalloc (LanguageCount, parserDefinition*);
	memcpy (ltable, LanguageTable, sizeof (parserDefinition*) * LanguageCount);
	qsort (ltable, LanguageCount, sizeof (parserDefinition*), compareParsersByName);

	for (i = 0  ;  i < LanguageCount  ;  ++i)
		printLanguage (i, ltable);

	eFree (ltable);
}

/*
*   File parsing
*/

static rescanReason createTagsForFile (const langType language,
				       const unsigned int passCount)
{
	parserDefinition *const lang = LanguageTable [language];
	rescanReason rescan = RESCAN_NONE;

	resetInputFile (language);

	Assert (lang->parser || lang->parser2);

	if (lang->parser != NULL)
		lang->parser ();
	else if (lang->parser2 != NULL)
		rescan = lang->parser2 (passCount);

	return rescan;
}

static boolean createTagsWithFallback1 (const langType language)
{
	boolean tagFileResized = FALSE;
	unsigned long numTags	= numTagsAdded ();
	MIOPos tagfpos;
	int lastPromise = getLastPromise ();
	unsigned int passCount = 0;
	rescanReason whyRescan;

	initializeParser (language);
	if (LanguageTable [language]->useCork)
		corkTagFile();

	addParserPseudoTags (language);
	tagFilePosition (&tagfpos);

	while ( ( whyRescan =
		  createTagsForFile (language, ++passCount) )
		!= RESCAN_NONE)
	{
		if (LanguageTable [language]->useCork)
		{
			uncorkTagFile();
			corkTagFile();
		}


		if (whyRescan == RESCAN_FAILED)
		{
			/*  Restore prior state of tag file.
			*/
			setTagFilePosition (&tagfpos);
			setNumTagsAdded (numTags);
			tagFileResized = TRUE;
			breakPromisesAfter(lastPromise);
		}
		else if (whyRescan == RESCAN_APPEND)
		{
			tagFilePosition (&tagfpos);
			numTags = numTagsAdded ();
			lastPromise = getLastPromise ();
		}
	}

	if (LanguageTable [language]->useCork)
		uncorkTagFile();

	return tagFileResized;
}

extern boolean runParserInNarrowedInputStream (const langType language,
					       unsigned long startLine, int startCharOffset,
					       unsigned long endLine, int endCharOffset,
					       unsigned long sourceLineOffset)
{
	boolean tagFileResized;
	pushNarrowedInputStream (language,
				 startLine, startCharOffset,
				 endLine, endCharOffset,
				 sourceLineOffset);
	tagFileResized = createTagsWithFallback1 (language);
	popNarrowedInputStream  ();
	return tagFileResized;

}

static boolean createTagsWithFallback (
	const char *const fileName, const langType language,
	MIO *mio)
{
	boolean tagFileResized = FALSE;

	Assert (0 <= language  &&  language < (int) LanguageCount);

	if (!openInputFile (fileName, language, mio))
		return FALSE;

	tagFileResized = createTagsWithFallback1 (language);
	tagFileResized = forcePromises()? TRUE: tagFileResized;

	makeFileTag (fileName);
	closeInputFile ();

	return tagFileResized;
}

#ifdef HAVE_COPROC
static boolean createTagsWithXcmd (
		const char *const fileName, const langType language,
		MIO *mio)
{
	boolean tagFileResized = FALSE;

	if (openInputFile (fileName, language, mio))
	{
		tagFileResized = invokeXcmd (fileName, language);

		/* TODO: File.lineNumber must be adjusted for the case
		 *  Option.printTotals is non-zero. */
		closeInputFile ();
	}

	return tagFileResized;
}
#endif

static void printGuessedParser (const char* const fileName, langType language)
{
	const char *parserName;

	if (language == LANG_IGNORE)
	{
		Option.printLanguage = ((int)TRUE) + 1;
		parserName = "NONE";
	}
	else
		parserName = LanguageTable [language]->name;

	printf("%s: %s\n", fileName, parserName);
}

#ifdef HAVE_ICONV
static char **EncodingMap;
static unsigned int EncodingMapMax;

static void addLanguageEncoding (const langType language,
									const char *const encoding)
{
	if (language > EncodingMapMax || EncodingMapMax == 0)
	{
		int i;
		int istart = (EncodingMapMax == 0)? 0: EncodingMapMax + 1;
		EncodingMap = xRealloc (EncodingMap, (language + 1), char*);
		for (i = istart;  i <= language  ;  ++i)
		{
			EncodingMap [i] = NULL;
		}
		EncodingMapMax = language;
	}
	if (EncodingMap [language])
		eFree (EncodingMap [language]);
	EncodingMap [language] = eStrdup(encoding);
	if (!Option.outputEncoding)
		Option.outputEncoding = eStrdup("UTF-8");
}

extern boolean processLanguageEncodingOption (const char *const option, const char *const parameter)
{
	langType language;

	language = getLanguageComponentInOption (option, "input-encoding-");
	if (language == LANG_IGNORE)
		return FALSE;

	addLanguageEncoding (language, parameter);
	return TRUE;
}

extern void freeEncodingResources (void)
{
	if (EncodingMap)
	{
		int i;
		for (i = 0  ;  i <= EncodingMapMax  ; ++i)
		{
			if (EncodingMap [i])
				eFree (EncodingMap [i]);
		}
		free(EncodingMap);
	}
	if (Option.inputEncoding)
		eFree (Option.inputEncoding);
	if (Option.outputEncoding)
		eFree (Option.outputEncoding);
}
#endif

static void addParserPseudoTags (langType language)
{
	if (!LanguageTable[language]->pseudoTagPrinted)
	{
		makePtagIfEnabled (PTAG_KIND_DESCRIPTION, &language);
		makePtagIfEnabled (PTAG_KIND_SEPARATOR, &language);

		LanguageTable[language]->pseudoTagPrinted = 1;
	}
}

extern boolean doesParserRequireMemoryStream (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	parserDefinition *const lang = LanguageTable [language];

	return (lang->tagXpathTableCount > 0)? TRUE: FALSE;
}

extern boolean parseFile (const char *const fileName)
{
	boolean tagFileResized = FALSE;
	langType language;
	MIO *mio;

	language = getFileLanguageAndKeepMIO (fileName, &mio);
	Assert (language != LANG_AUTO);

	if (Option.printLanguage)
	{
		printGuessedParser (fileName, language);
		return tagFileResized;
	}

	if (language == LANG_IGNORE)
		verbose ("ignoring %s (unknown language/language disabled)\n",
			 fileName);
	else if (! isLanguageEnabled (language))
	{
		/* This block is needed. In the parser choosing stage, each
		   parser is not initialized for making ctags starting up faster.
		   So the chooser can choose a XCMD based parser.
		   However, at the stage the chooser cannot know whether
		   the XCMD is available or not. This isLanguageEnabled
		   invocation verify the availability. */
		verbose ("ignoring %s (language disabled)\n", fileName);
	}
	else
	{
		if (Option.filter)
			openTagFile ();

#ifdef HAVE_ICONV
		openConverter (EncodingMap && language <= EncodingMapMax &&
				EncodingMap [language] ?
					EncodingMap[language] : Option.inputEncoding, Option.outputEncoding);
#endif

		setupWriter ();

		tagFileResized = createTagsWithFallback (fileName, language, mio);
#ifdef HAVE_COPROC
		if (LanguageTable [language]->method & METHOD_XCMD_AVAILABLE)
			tagFileResized = createTagsWithXcmd (fileName, language, mio)? TRUE: tagFileResized;
#endif

		teardownWriter (fileName);

		if (Option.filter)
			closeTagFile (tagFileResized);
		addTotals (1, 0L, 0L);

#ifdef HAVE_ICONV
		closeConverter ();
#endif
		if (mio)
			mio_free (mio);
		return tagFileResized;
	}

	if (mio)
		mio_free (mio);
	return tagFileResized;
}

extern void useRegexMethod (const langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	lang->method |= METHOD_REGEX;
}

extern void useXcmdMethod (const langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	lang->method |= METHOD_XCMD;
}

extern void useXpathMethod (const langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	lang->method |= METHOD_XPATH;
}

extern void notifyAvailabilityXcmdMethod (const langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	lang->method |= METHOD_XCMD_AVAILABLE;
}

static void installTagRegexTable (const langType language)
{
	parserDefinition* lang;
	unsigned int i;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];


	if (lang->tagRegexTable != NULL)
	{
	    for (i = 0; i < lang->tagRegexCount; ++i)
		    addTagRegex (language,
				 lang->tagRegexTable [i].regex,
				 lang->tagRegexTable [i].name,
				 lang->tagRegexTable [i].kinds,
				 lang->tagRegexTable [i].flags,
				 (lang->tagRegexTable [i].disabled));
	}
}

static void installKeywordTable (const langType language)
{
	parserDefinition* lang;
	unsigned int i;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	if (lang->keywordTable != NULL)
	{
		for (i = 0; i < lang->keywordCount; ++i)
			addKeyword (lang->keywordTable [i].name,
				    language,
				    lang->keywordTable [i].id);
	}
}

static void installTagXpathTable (const langType language)
{
	parserDefinition* lang;
	unsigned int i, j;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	if (lang->tagXpathTableTable != NULL)
	{
		for (i = 0; i < lang->tagXpathTableCount; ++i)
			for (j = 0; j < lang->tagXpathTableTable[i].count; ++j)
				addTagXpath (language, lang->tagXpathTableTable[i].table + j);
		useXpathMethod (language);
	}
}

extern boolean makeKindSeparatorsPseudoTags (const langType language,
					     const ptagDesc *pdesc)
{
	parserDefinition* lang;
	kindOption *kinds;
	unsigned int kindCount;
	unsigned int i, j;

	boolean r = FALSE;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	kinds = lang->kinds;
	kindCount = lang->kindCount;

	if (kinds == NULL)
		return r;

	for (i = 0; i < kindCount; ++i)
	{
		static vString *sepval;

		if (!sepval)
			sepval = vStringNew ();

		for (j = 0; j < kinds[i].separatorCount; ++j)
		{
			char name[5] = {[0] = '/', [3] = '/', [4] = '\0'};
			const kindOption *upperKind;
			const scopeSeparator *sep;

			sep = kinds[i].separators + j;

			if (sep->parentLetter == KIND_WILDCARD)
			{
				name[1] = KIND_WILDCARD;
				name[2] = kinds[i].letter;
			}
			else if (sep->parentLetter == KIND_NULL)
			{
				/* This is root separator: no upper item is here. */
				name[1] = kinds[i].letter;
				name[2] = name[3];
				name[3] = '\0';
			}
			else
			{
				upperKind = langKindOption (language,
							    sep->parentLetter);
				if (!upperKind)
					continue;

				name[1] = upperKind->letter;
				name[2] = kinds[i].letter;
			}


			vStringClear (sepval);
			vStringCatSWithEscaping (sepval, sep->separator);

			r = writePseudoTag (pdesc, vStringValue (sepval),
					    name, lang->name) || r;
		}
	}

	return r;
}

struct makeKindDescriptionPseudoTagData {
	const char* langName;
	const ptagDesc *pdesc;
	boolean written;
};

static boolean makeKindDescriptionPseudoTag (kindOption *kind,
					     void *user_data)
{
	struct makeKindDescriptionPseudoTagData *data = user_data;
	vString *letter_and_name;
	vString *description;
	const char *d;

	letter_and_name = vStringNew ();
	description = vStringNew ();

	vStringPut (letter_and_name, kind -> letter);
	vStringPut (letter_and_name, ',');
	vStringCatS (letter_and_name, kind -> name);

	d = kind->description? kind->description: kind->name;
	vStringPut (description, '/');
	vStringCatSWithEscapingAsPattern (description, d);
	vStringPut (description, '/');
	data->written |=  writePseudoTag (data->pdesc, vStringValue (letter_and_name),
					  vStringValue (description),
					  data->langName);

	vStringDelete (description);
	vStringDelete (letter_and_name);

	return FALSE;
}

extern boolean makeKindDescriptionsPseudoTags (const langType language,
					    const ptagDesc *pdesc)
{

	parserDefinition* lang;
	kindOption *kinds;
	unsigned int kindCount, i;
	struct makeKindDescriptionPseudoTagData data;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	kinds = lang->kinds;
	kindCount = lang->kindCount;

	data.langName = lang->name;
	data.pdesc = pdesc;
	data.written = FALSE;

	for (i = 0; i < kindCount; ++i)
		makeKindDescriptionPseudoTag (kinds + i, &data);

	foreachRegexKinds (language, makeKindDescriptionPseudoTag, &data);
	foreachXcmdKinds (language, makeKindDescriptionPseudoTag, &data);

	return data.written;
}

/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Anonymous name generator
*/

extern void anonReset (void)
{
	parserDefinition* lang = LanguageTable [getInputLanguage ()];
	lang -> anonumousIdentiferId = 0;
}

static unsigned int anonHash(const unsigned char *str)
{
	unsigned int hash = 5381;
	int c;

	while((c = *str++))
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash ;
}

extern void anonGenerate (vString *buffer, const char *prefix, int kind)
{
	parserDefinition* lang = LanguageTable [getInputLanguage ()];
	lang -> anonumousIdentiferId ++;

	char szNum[32];

	vStringCopyS(buffer, prefix);

	unsigned int uHash = anonHash((const unsigned char *)getInputFileName());
	sprintf(szNum,"%08x%02x%02x",uHash,lang -> anonumousIdentiferId, kind);
	vStringCatS(buffer,szNum);
}

/*
 * A parser for CTagsSelfTest (CTST)
 */
typedef enum {
	K_BROKEN,
	KIND_COUNT
} CTST_Kind;

typedef enum {
	R_BROKEN_REF,
} CTST_BrokenRole;

static roleDesc CTST_BrokenRoles [] = {
	{TRUE, "broken", "broken" },
};

static kindOption CTST_Kinds[KIND_COUNT] = {
	{TRUE, 'b', "broken tag", "name with unwanted characters",
	 .referenceOnly = FALSE, ATTACH_ROLES (CTST_BrokenRoles) },
};

static void createCTSTTags (void)
{
	int i;
	const unsigned char *line;
	tagEntryInfo e;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		int c = line[0];

		for (i = 0; i < KIND_COUNT; i++)
			if (c == CTST_Kinds[i].letter)
			{
				switch (i)
				{
					case K_BROKEN:
						initTagEntry (&e, "one\nof\rbroken\tname", &CTST_Kinds[i]);
						e.extensionFields.scopeKind = & (CTST_Kinds [K_BROKEN]);
						e.extensionFields.scopeName = "\\Broken\tContext";
						makeTagEntry (&e);
						break;
				}
			}
	}

}

static parserDefinition *CTagsSelfTestParser (void)
{
	static const char *const extensions[] = { NULL };
	parserDefinition *const def = parserNew ("CTagsSelfTest");
	def->extensions = extensions;
	def->kinds = CTST_Kinds;
	def->kindCount = KIND_COUNT;
	def->parser = createCTSTTags;
	def->invisible = TRUE;
	return def;
}
