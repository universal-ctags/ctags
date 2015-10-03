/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for managing source languages and
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
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parsers.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#ifdef HAVE_ICONV
# include "mbcs.h"
#endif

/*
*   DATA DEFINITIONS
*/
static parserDefinitionFunc* BuiltInParsers[] = { PARSER_LIST };
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

extern int makeSimpleTag (
		const vString* const name, kindOption* const kinds, const int kind)
{
	int r = SCOPE_NIL;

	if (kinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
	    tagEntryInfo e;
	    initTagEntry (&e, vStringValue (name), & kinds [kind]);

	    r = makeTagEntry (&e);
	}
	return r;
}

static vString* ext2ptrnNew (const char *const ext)
{
	vString * ptrn = vStringNewInit ("*.");
	vStringCatS (ptrn, ext);
	return ptrn;
}

extern boolean isLanguageEnabled (const langType language)
{
	const parserDefinition* const lang = LanguageTable [language];
	if (!lang->enabled)
		return FALSE;
	else if ((lang->method & METHOD_XCMD) &&
		 (!(lang->method & METHOD_XCMD_AVAILABLE)) &&
		 (lang->kinds == NULL) &&
		 (!(lang->method & METHOD_REGEX)))
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

extern boolean getLanguageAllowNullTag (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	return LanguageTable [language]->allowNullTag;
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

extern langType getNamedLanguage (const char *const name)
{
	langType result = LANG_IGNORE;
	unsigned int i;
	Assert (name != NULL);
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		const parserDefinition* const lang = LanguageTable [i];
		if (lang->name != NULL)
			if (strcasecmp (name, lang->name) == 0)
				result = i;
	}
	return result;
}

static langType getNameOrAliasesLanguageAndSpec (const char *const key, langType start_index, const char **const spec)
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

		if (lang->name != NULL && strcasecmp (key, lang->name) == 0)
		{
			result = i;
			*spec = lang->name;
		}
		else if (aliases != NULL  &&  (tmp = stringListFileFinds (aliases, key)))
		{
			result = i;
			*spec = vStringValue(tmp);
		}
	}
	return result;
}

static langType getPatternLanguageAndSpec (const char *const baseName, langType start_index, const char **const spec)
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

		if (ptrns != NULL && (tmp = stringListFileFinds (ptrns, baseName)))
		{
			result = i;
			*spec = vStringValue(tmp);
		}
	}
	return result;
}

typedef struct {
	langType lang;
	const char* spec;
}  parserCandidate;
static parserCandidate* parserCandidateNew(unsigned int count __unused__)
{
	parserCandidate* candidates;
	unsigned int i;

	candidates= xMalloc(LanguageCount, parserCandidate);
	for (i = 0; i < LanguageCount; i++)
	{
		candidates[i].lang = LANG_IGNORE;
		candidates[i].spec = NULL;
	}
	return candidates;
}

/* If multiple parsers are found, return LANG_AUTO */
static unsigned int nominateLanguageCandidates (const char *const key, parserCandidate** candidates)
{
	unsigned int count;
	langType i;
	const char* spec = NULL;

	*candidates = parserCandidateNew(LanguageCount);

	for (count = 0, i = LANG_AUTO; i != LANG_IGNORE; )
	{
		i = getNameOrAliasesLanguageAndSpec (key, i, &spec);
		if (i != LANG_IGNORE)
		{
			(*candidates)[count].lang = i++;
			(*candidates)[count++].spec = spec;
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

	*candidates = parserCandidateNew(LanguageCount);

	for (count = 0, i = LANG_AUTO; i != LANG_IGNORE; )
	{
		i = getPatternLanguageAndSpec (baseName, i, &spec);
		if (i != LANG_IGNORE)
		{
			(*candidates)[count].lang   = i++;
			(*candidates)[count++].spec = spec;
		}
	}
	return count;
}

static vString* extractEmacsModeAtFirstLine(FILE* input);

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

static vString* extractInterpreter (FILE* input)
{
	vString* const vLine = vStringNew ();
	const char* const line = readLine (vLine, input);
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
		return mode;
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
		for ( ;  *p != '\0'  &&  (isalnum ((int) *p) || *p == '-')  ;  ++p)
			vStringPut (mode, (int) *p);
		vStringTerminate (mode);

		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		if (strncmp(p, "-*-", strlen("-*-")) != 0)
			vStringClear (mode);
	}

	return mode;

}

static vString* extractEmacsModeAtFirstLine(FILE* input)
{
	vString* const vLine = vStringNew ();
	const char* const line = readLine (vLine, input);
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

static vString* determineEmacsModeAtEOF (FILE* const fp)
{
	vString* const vLine = vStringNew ();
	const char* line;
	boolean headerFound = FALSE;
	const char* p;
	vString* mode = vStringNew ();

	while ((line = readLine (vLine, fp)) != NULL)
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

static vString* extractEmacsModeLanguageAtEOF (FILE* input)
{
	vString* mode;

	/* "48.2.4.1 Specifying File Variables" of Emacs info:
	   ---------------------------------------------------
	   you can define file local variables using a "local
	   variables list" near the end of the file.  The start of the
	   local variables list should be no more than 3000 characters
	   from the end of the file, */
	fseek(input, -3000, SEEK_END);

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

static vString* extractVimFileType(FILE* input)
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
	while ((readLine (ring[i++], input)) != NULL)
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

static vString* extractZshAutoloadTag(FILE* input)
{
	vString* const vLine = vStringNew ();
	const char* const line = readLine (vLine, input);
	vString* mode = NULL;

	if (line)
		mode = determineZshAutoloadTag (line);

	vStringDelete (vLine);
	return mode;
}

struct getLangCtx {
    const char *fileName;
    FILE       *input;
    boolean     err;
};

#define GLC_FOPEN_IF_NECESSARY(_glc_, _label_) do {         \
    if (!(_glc_)->input) {                                  \
        (_glc_)->input = fopen((_glc_)->fileName, "rb");    \
        if (!(_glc_)->input) {                              \
            (_glc_)->err = TRUE;                            \
            goto _label_;                                   \
        }                                                   \
    }                                                       \
} while (0)                                                 \

#define GLC_FCLOSE(_glc_) do {                              \
    if ((_glc_)->input) {                                   \
        fclose((_glc_)->input);                             \
        (_glc_)->input = NULL;                              \
    }                                                       \
} while (0)

static const struct taster {
	vString* (* taste) (FILE *);
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
static langType tasteLanguage (struct getLangCtx *glc, const struct taster *const tasters, int n_tasters);

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
pickLanguageBySelection (selectLanguage selector, FILE *input)
{
    const char *lang = selector(input);
    if (lang)
    {
        verbose ("	selection: %s\n", lang);
        return getNamedLanguage(lang);
    }
    else
    {
	verbose ("	no selection");
        return LANG_IGNORE;
    }
}

static langType getSpecLanguageCommon (const char *const spec, struct getLangCtx *glc,
				       unsigned int nominate (const char *const, parserCandidate**))
{
	langType language;
	parserCandidate  *candidates;
	unsigned int n_candidates;

	n_candidates = (*nominate)(spec, &candidates);

	verbose ("		#candidates: %u\n", n_candidates);
	if (n_candidates == 1)
	{
		language = candidates[0].lang;
	}
	else if (n_candidates > 1)
	{
		GLC_FOPEN_IF_NECESSARY(glc, fopen_error);
		selectLanguage selector = commonSelector(candidates, n_candidates);
		if (selector) {
			verbose ("Selector: %p\n", selector);
			language = pickLanguageBySelection(selector, glc->input);
		} else {
			verbose ("Selector: NONE\n");
			language = LANG_IGNORE;
		}

		Assert(language != LANG_AUTO);

		if (language == LANG_IGNORE)
			language = candidates[0].lang;
	}
	else
	{
fopen_error:
		language = LANG_IGNORE;
	}

	eFree(candidates);
	candidates = NULL;

	return language;
}

static langType getSpecLanguage (const char *const spec,
                                 struct getLangCtx *glc)
{
	return getSpecLanguageCommon(spec, glc, nominateLanguageCandidates);
}

static langType getPatternLanguage (const char *const baseName,
                                    struct getLangCtx *glc)
{
	return getSpecLanguageCommon(baseName, glc,
				     nominateLanguageCandidatesForPattern);
}

/* This function tries to figure out language contained in a file by
 * running a series of tests, trying to find some clues in the file.
 */
static langType
tasteLanguage (struct getLangCtx *glc, const struct taster *const tasters, int n_tasters)
{
    int i;

    for (i = 0; i < n_tasters; ++i) {
        langType language;
        vString* spec;
        rewind(glc->input);
        if (NULL != (spec = tasters[i].taste(glc->input))) {
            verbose ("	%s: %s\n", tasters[i].msg, vStringValue (spec));
            language = getSpecLanguage (vStringValue (spec), glc);
            vStringDelete (spec);
            if (language != LANG_IGNORE)
                return language;
        }
    }

    return LANG_IGNORE;
}

static langType
getFileLanguageInternal (const char *const fileName)
{
    langType language;
    struct getLangCtx glc = {
        .fileName = fileName,
        .input    = NULL,
        .err      = FALSE,
    };
    const char* const baseName = baseFilename (fileName);
    char *templateBaseName = NULL;
    fileStatus *fstatus = NULL;

    verbose ("Get file language for %s\n", fileName);

    verbose ("	pattern: %s\n", baseName);
    language = getPatternLanguage (baseName, &glc);
    if (language != LANG_IGNORE || glc.err)
        goto cleanup;

    {
        const char* const tExt = ".in";
        templateBaseName = baseFilenameSansExtensionNew (fileName, tExt);
        if (templateBaseName)
        {
            verbose ("	pattern + template(%s): %s\n", tExt, templateBaseName);
            GLC_FOPEN_IF_NECESSARY(&glc, cleanup);
            rewind(glc.input);
            language = getPatternLanguage(templateBaseName, &glc);
            if (language != LANG_IGNORE)
                goto cleanup;
        }
    }

    fstatus = eStat (fileName);
    if (fstatus && fstatus->exists)
    {
	    if (fstatus->isExecutable || Option.guessLanguageEagerly)
	    {
		    GLC_FOPEN_IF_NECESSARY (&glc, cleanup);
		    language = tasteLanguage(&glc, eager_tasters, 1);
	    }
	    if (language != LANG_IGNORE)
		    goto cleanup;

	    if (Option.guessLanguageEagerly)
	    {
		    GLC_FOPEN_IF_NECESSARY(&glc, cleanup);
		    language = tasteLanguage(&glc, 
					     eager_tasters + 1,
					     ARRAY_SIZE(eager_tasters) - 1);
	    }
    }


  cleanup:
    GLC_FCLOSE(&glc);
    if (fstatus)
	    eStatFree (fstatus);
    if (templateBaseName)
        eFree (templateBaseName);
    return language;
}

extern langType getFileLanguage (const char *const fileName)
{
    if (LANG_AUTO == Option.language)
        return getFileLanguageInternal(fileName);
    else
        return Option.language;
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

static boolean removeLanguagePatternMap (const char *const pattern)
{
	boolean result = FALSE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
	{
		stringList* const ptrn = LanguageTable [i]->currentPatterns;
		if (ptrn != NULL && stringListDeleteItemExtension (ptrn, pattern))
		{
			verbose (" (removed from %s)", getLanguageName (i));
			result = TRUE;
		}
	}
	return result;
}

extern void addLanguagePatternMap (const langType language, const char* ptrn, boolean exclusive)
{
	vString* const str = vStringNewInit (ptrn);
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (exclusive)
		removeLanguagePatternMap (ptrn);
	stringListAdd (lang->currentPatterns, str);
}

extern boolean removeLanguageExtensionMap (const char *const extension)
{
	boolean result = FALSE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		if (exts != NULL && stringListDeleteItemExtension (exts, extension))
		{
			verbose (" (removed from %s)", getLanguageName (i));
			result = TRUE;
		}
	}
	return result;
}

extern void addLanguageExtensionMap (
		const langType language, const char* extension, boolean exclusive)
{
	vString* const str = vStringNewInit (extension);
	Assert (0 <= language  &&  language < (int) LanguageCount);
	if (exclusive)
		removeLanguageExtensionMap (extension);
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

static boolean doesParserUseKind (const parserDefinition *const parser, char letter)
{
	unsigned int k;

	for (k = 0; k < parser->kindCount; k++)
		if (parser->kinds [k].letter == letter)
			return TRUE;
	return FALSE;
}

static void initializeParserTopHalf (parserDefinition *const parser, langType lang)
{
	if (parser->initialize != NULL)
	{
		parser->initialize (lang);
		parser->initialize = NULL;
	}

	Assert (parser->fileKind != KIND_NULL);
	Assert (!doesParserUseKind (parser, parser->fileKind->letter));
}

static void initializeParserBottomHalf (parserDefinition *const parser, langType lang)
{
	installTagRegexTable (lang);

	if (hasScopeActionInRegex (lang))
		parser->useCork = TRUE;

	Assert (parser->fileKind != KIND_NULL);
	Assert (!doesParserUseKind (parser, parser->fileKind->letter));
}


static void initializeParsersTopHalf (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		initializeParserTopHalf (LanguageTable [i], i);
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
			const unsigned int regex_only = ( METHOD_REGEX | METHOD_NOT_CRAFTED );
			if (def->name == NULL  ||  def->name[0] == '\0')
				error (FATAL, "parser definition must contain name\n");
			else if ((def->method & regex_only) == regex_only)
			{
#ifdef HAVE_REGEX
				def->parser = findRegexTags;
				accepted = TRUE;
#endif
			}
			else if (((!!def->parser) + (!!def->parser2)) != 1)
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
	initializeParsersTopHalf ();
}

static void initializeParser (parserDefinition *const parser, langType lang)
{
	initializeParserTopHalf (parser, lang);
	initializeParserBottomHalf (parser, lang);
}

extern void freeParserResources (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		parserDefinition* const lang = LanguageTable [i];

		if (lang->finalize)
			(lang->finalize)((langType)i);
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

#ifdef HAVE_REGEX
static void doNothing (void)
{
}

static void lazyInitializeTopHalf (langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	lang->parser = doNothing;

	if (lang->method & METHOD_REGEX)
		lang->parser = findRegexTags;
}
#endif

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
		const char *const option, const char *const parameter __unused__)
{
#ifdef HAVE_REGEX
	if (parameter [0] == '\0')
		error (WARNING, "No language specified for \"%s\" option", option);
	else if (getNamedLanguage (parameter) != LANG_IGNORE)
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
		def->initialize        = lazyInitializeTopHalf;
		def->currentPatterns   = stringListNew ();
		def->currentExtensions = stringListNew ();
		def->method            = METHOD_NOT_CRAFTED;
		def->id                = i;
		LanguageTable = xRealloc (LanguageTable, i + 1, parserDefinition*);
		LanguageTable [i] = def;

		flagsEval (flags, LangDefFlagDef, COUNT_ARRAY (LangDefFlagDef), def);

		eFree (name);
	}
#else
	error (WARNING, "regex support not available; required for --%s option",
		   option);
#endif
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
			lang->kinds [i].enabled = mode;
	}
}

static boolean enableLanguageKind (
		const langType language, const int kind, const boolean mode)
{
	boolean result = enableRegexKind (language, kind, mode);
	kindOption* const opt = langKindOption (language, kind);
	if (opt != NULL)
	{
		opt->enabled = mode;
		result = TRUE;
	}
	result = enableXcmdKind (language, kind, mode)? TRUE: result;
	return result;
}

static void processLangKindOption (
		const langType language, const char *const option,
		const char *const parameter)
{
	const char *p = parameter;
	boolean mode = TRUE;
	int c;

	Assert (0 <= language  &&  language < (int) LanguageCount);

	if (*p == '*')
	{
		resetLanguageKinds (language, TRUE);
		p++;
	}
	else if (*p != '+'  &&  *p != '-')
		resetLanguageKinds (language, FALSE);

	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;  break;
		case '-': mode = FALSE; break;
		default:
			if (! enableLanguageKind (language, c, mode))
				error (WARNING, "Unsupported parameter '%c' for --%s option",
					c, option);
			break;
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
			language = getNamedLanguage (vStringValue (langName));
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
			language = getNamedLanguage (lang);
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

extern void printKind (const kindOption* const kind, boolean allKindFields, boolean indent)
{
	if (allKindFields)
	{
		printf ("%s%c\t%s\t%s\t%s\n", indent ? "\t"           : "",
			kind->letter,
			kind->name        != NULL ? kind->name        : "",
			kind->description != NULL ? kind->description : "",
			kind->enabled             ? "on"              : "off");
	}
	else
	{
		printf ("%s%c  %s%s\n", indent ? "    " : "", kind->letter,
			kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
			kind->enabled ? "" : " [off]");
	}
}

static void printKinds (langType language, boolean allKindFields, boolean indent)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL)
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
		{
			if (allKindFields && indent)
				printf ("%s", lang->name);
			printKind (lang->kinds + i, allKindFields, indent);
		}
	}
	printRegexKinds (language, allKindFields, indent);
	printXcmdKinds (language, allKindFields, indent);
}

extern void printLanguageKinds (const langType language, boolean allKindFields)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];

			if (!allKindFields)
				printf ("%s%s\n", lang->name, isLanguageEnabled (i) ? "" : " [disabled]");
			printKinds (i, allKindFields, TRUE);
		}
	}
	else
		printKinds (language, allKindFields, FALSE);
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

static void printMaps (const langType language)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	printf ("%-8s", lang->name);
	if (lang->currentExtensions != NULL)
		for (i = 0  ;  i < stringListCount (lang->currentExtensions)  ;  ++i)
			printf (" *.%s", vStringValue (
						stringListItem (lang->currentExtensions, i)));
	if (lang->currentPatterns != NULL)
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

extern void printLanguageMaps (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
			printMaps (i);
	}
	else
		printMaps (language);
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

static void printLanguage (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  (lang->method & METHOD_REGEX) || (lang->method & METHOD_XCMD))
		printf ("%s%s\n", lang->name, isLanguageEnabled (language) ? "" : " [disabled]");
}

extern void printLanguageList (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		printLanguage (i);
}

/*
*   File parsing
*/
extern void makeFileTag (const char *const fileName)
{
	boolean via_line_directive = (strcmp (fileName, getInputFileName()) != 0);
	if (Option.include.fileNames || Option.include.fileNamesWithTotalLines)
	{
		tagEntryInfo tag;
		kindOption  *kind;

		kind = getSourceLanguageFileKind();
		Assert (kind);
		kind->enabled = Option.include.fileNames;

		/* TODO: you can return here if enabled == FALSE. */

		initTagEntry (&tag, baseFilename (fileName), kind);

		tag.isFileEntry     = TRUE;
		tag.lineNumberEntry = TRUE;

		if (via_line_directive || Option.include.fileNames)
		{
			tag.lineNumber = 1;
		}
		else
		{
			while (fileReadLine () != NULL)
				;		/* Do nothing */
			tag.lineNumber = getInputLineNumber ();
		}

		makeTagEntry (&tag);
	}
}

static rescanReason createTagsForFile (
		const char *const fileName, const langType language,
		const unsigned int passCount)
{
	rescanReason rescan = RESCAN_NONE;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	if (fileOpen (fileName, language))
	{
		parserDefinition *const lang = LanguageTable [language];

		initializeParser (lang, language);

		Assert (lang->parser || lang->parser2);

		if (LanguageTable [language]->useCork)
			corkTagFile();

		if (lang->parser != NULL)
			lang->parser ();
		else if (lang->parser2 != NULL)
			rescan = lang->parser2 (passCount);

		makeFileTag (fileName);

		if (LanguageTable [language]->useCork)
			uncorkTagFile();

		fileClose ();
	}

	return rescan;
}

static boolean createTagsWithFallback (
		const char *const fileName, const langType language)
{
	unsigned long numTags	= TagFile.numTags.added;
	fpos_t tagFilePosition;
	unsigned int passCount = 0;
	boolean tagFileResized = FALSE;
	rescanReason whyRescan;

	fgetpos (TagFile.fp, &tagFilePosition);
	while ( ( whyRescan =
	            createTagsForFile (fileName, language, ++passCount) )
	                != RESCAN_NONE)
	{
		if (whyRescan == RESCAN_FAILED)
		{
			/*  Restore prior state of tag file.
			*/
			fsetpos (TagFile.fp, &tagFilePosition);
			TagFile.numTags.added = numTags;
			tagFileResized = TRUE;
		}
		else if (whyRescan == RESCAN_APPEND)
		{
			fgetpos(TagFile.fp, &tagFilePosition);
			numTags = TagFile.numTags.added;
		}
	}
	return tagFileResized;
}

#ifdef HAVE_COPROC
static boolean createTagsWithXcmd (
		const char *const fileName, const langType language)
{
	boolean tagFileResized = FALSE;

	if (fileOpen (fileName, language))
	{
		tagFileResized = invokeXcmd (fileName, language);

		/* TODO: File.lineNumber must be adjusted for the case
		 *  Option.printTotals is non-zero. */
		fileClose ();
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

extern boolean parseFile (const char *const fileName)
{
	boolean tagFileResized = FALSE;
	langType language = Option.language;
	if (Option.language == LANG_AUTO)
		language = getFileLanguage (fileName);
	Assert (language != LANG_AUTO);

	if (Option.printLanguage)
	{
		printGuessedParser (fileName, language);
		return tagFileResized;
	}

	if (language == LANG_IGNORE)
		verbose ("ignoring %s (unknown language)\n", fileName);
	else if (! isLanguageEnabled (language))
		verbose ("ignoring %s (language disabled)\n", fileName);
	else
	{
		if (Option.filter)
			openTagFile ();

#ifdef HAVE_ICONV
		openConverter (EncodingMap && language <= EncodingMapMax &&
				EncodingMap [language] ?
					EncodingMap[language] : Option.inputEncoding, Option.outputEncoding);
#endif

		if (Option.etags)
			beginEtagsFile ();

		tagFileResized = createTagsWithFallback (fileName, language);
#ifdef HAVE_COPROC
		tagFileResized = createTagsWithXcmd (fileName, language)? TRUE: tagFileResized;
#endif

		if (Option.etags)
			endEtagsFile (fileName);
		if (Option.filter)
			closeTagFile (tagFileResized);
		addTotals (1, 0L, 0L);

#ifdef HAVE_ICONV
		closeConverter ();
#endif

		return tagFileResized;
	}
	return tagFileResized;
}

static void unifyMaps (const langType language)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (!lang->currentExtensions)
		return;

	for (i = 0 ; i < stringListCount (lang->currentExtensions)  ;  ++i)
	{
		const char* const ext = vStringValue (
			stringListItem (lang->currentExtensions, i));
		vString *const ptrn = ext2ptrnNew (ext);
		addLanguagePatternMap (language, vStringValue (ptrn), FALSE);
		vStringDelete (ptrn);
	}
}

extern void unifyLanguageMaps (void)
{
	unsigned int i;
	for (i = 0; i < LanguageCount  ;  ++i)
		unifyMaps (i);
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

extern void notifyAvailabilityXcmdMethod (const langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	lang->method |= METHOD_XCMD_AVAILABLE;
}

extern void installTagRegexTable (const langType language)
{
	parserDefinition* lang;
	unsigned int i;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	if (lang->tagRegexTable == NULL)
		return;

	for (i = 0; i < lang->tagRegexCount; ++i)
		addTagRegex (language,
			     lang->tagRegexTable [i].regex,
			     lang->tagRegexTable [i].name,
			     lang->tagRegexTable [i].kinds,
			     lang->tagRegexTable [i].flags);
	lang->tagRegexTable = NULL;
}

/* vi:set tabstop=4 shiftwidth=4 nowrap: */
