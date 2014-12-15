/*
*   $Id$
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
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
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parsers.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "tg.h"

/*
*   DATA DEFINITIONS
*/
static parserDefinitionFunc* BuiltInParsers[] = { PARSER_LIST };
static parserDefinition** LanguageTable = NULL;
static unsigned int LanguageCount = 0;

/*
*   FUNCTION DEFINITIONS
*/

extern void makeSimpleTag (
		const vString* const name, kindOption* const kinds, const int kind)
{
	if (kinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
	    tagEntryInfo e;
	    initTagEntry (&e, vStringValue (name));

	    e.kindName = kinds [kind].name;
	    e.kind     = kinds [kind].letter;

	    makeTagEntry (&e);
	}
}

static vString* ext2ptrnNew (const char *const ext)
{
	vString * ptrn = vStringNewInit ("*.");
	vStringCatS (ptrn, ext);
	return ptrn;
}

static boolean enabled_p (const langType language)
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
	parserDefinition* result = xCalloc (1, parserDefinition);
	result->name = eStrdup (name);
	return result;
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
	const char* spec;

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

static vString* extracEmacsModeAtFirstLine(FILE* input);

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

		interpreter = extracEmacsModeAtFirstLine(input);
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
		for ( ;  *p != '\0'  &&  isalnum ((int) *p)  ;  ++p)
			vStringPut (mode, (int) *p);
		vStringTerminate (mode);
	}
	else
	{
		/* -*- MODE -*- */
		for ( ;  *p != '\0'  &&  isalnum ((int) *p)  ;  ++p)
			vStringPut (mode, (int) *p);
		vStringTerminate (mode);

		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		if (strncmp(p, "-*-", strlen("-*-")) != 0)
			vStringClear (mode);
	}

	return mode;

}

static vString* extracEmacsModeAtFirstLine(FILE* input)
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
			for ( ;  *p != '\0'  &&  isalnum ((int) *p)  ;  ++p)
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

static const tgTableEntry* findTgTableEntry(const parserDefinition* const lang, const char* const spec)
{
	const tgTableEntry *entry;

	for (entry = lang->tg_entries;
	     entry != NULL;
	     entry = entry->next)
		if (strcmp (vStringValue (entry->spec), spec) == 0)
			break;
	return entry;
}

static langType determineTwoGramLanguage(const unsigned char *const t,
					 const parserCandidate * const candidates, unsigned int n_candidates)
{
	unsigned int i, winner;

	for (winner = 0, i = 1; i < n_candidates; i++)
	{
		int r;
		const tgTableEntry *const winner_entry = findTgTableEntry(LanguageTable[candidates[winner].lang],
									 candidates[winner].spec);
		const tgTableEntry *const i_entry = findTgTableEntry(LanguageTable[candidates[i].lang],
								     candidates[i].spec);

		r = tg_compare(winner_entry->tg_table, i_entry->tg_table, t);

		verbose ("tg tournament %s:%s v.s. %s:%s => %d\n",
			 LanguageTable[candidates[winner].lang]->name, candidates[winner].spec,
			 LanguageTable[candidates[i].lang]->name, candidates[i].spec,
			 r);

		if (r > 0)
			winner = i;
	}
	return candidates[winner].lang;
}

static langType getTwoGramLanguage (FILE* input,
				    const parserCandidate  *const candidates, unsigned int n_candidates)
{
	langType result;
	unsigned int i;

	for (result = LANG_AUTO, i = 0; candidates[i].lang != LANG_IGNORE; i++)
		if (LanguageTable [candidates[i].lang]->tg_entries == NULL
		    || findTgTableEntry(LanguageTable [candidates[i].lang], candidates[i].spec) == NULL)
		{
			result = LANG_IGNORE;
			break;
		}

	if (result == LANG_AUTO)
	{

		unsigned char* t;

		t = tg_create();
		tg_load(t, input);

		result = determineTwoGramLanguage(t, candidates, n_candidates);

		verbose("winner of tg tournament: %s\n", LanguageTable[result]->name);

		tg_destroy(t);

	}
	return result;
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
		.taste  = extracEmacsModeAtFirstLine,
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
		language = tasteLanguage(glc,
					 eager_tasters,
					 ARRAY_SIZE(eager_tasters));
		if (language == LANG_IGNORE)
		{
			rewind(glc->input);
			language = getTwoGramLanguage(glc->input, candidates, n_candidates);
			if (language == LANG_IGNORE)
				language = candidates[0].lang;
		}
		/* At this point we are guaranteed that a language has been
		 * selected:
		 */
		Assert(language != LANG_IGNORE && language != LANG_AUTO);
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

extern void printLanguageMap (const langType language)
{
	boolean first = TRUE;
	unsigned int i;
	stringList* map = LanguageTable [language]->currentPatterns;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	for (i = 0  ;  map != NULL  &&  i < stringListCount (map)  ;  ++i)
	{
		printf ("%s(%s)", (first ? "" : " "),
				vStringValue (stringListItem (map, i)));
		first = FALSE;
	}
	map = LanguageTable [language]->currentExtensions;
	for (i = 0  ;  map != NULL  &&  i < stringListCount (map)  ;  ++i)
	{
		printf ("%s.%s", (first ? "" : " "),
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
	if (Option.verbose)
		printLanguageMap (language);
	verbose ("\n");
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

static void printAliases (const langType language);
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
	if (Option.verbose)
		printAliases (language);
	verbose ("\n");
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

extern void addLanguagePatternMap (const langType language, const char* ptrn)
{
	vString* const str = vStringNewInit (ptrn);
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentPatterns == NULL)
		lang->currentPatterns = stringListNew ();
	stringListAdd (lang->currentPatterns, str);
}

extern boolean removeLanguageExtensionMap (const char *const extension)
{
	boolean result = FALSE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		if (exts != NULL  &&  stringListRemoveExtension (exts, extension))
		{
			verbose (" (removed from %s)", getLanguageName (i));
			result = TRUE;
		}
	}
	return result;
}

extern void addLanguageExtensionMap (
		const langType language, const char* extension)
{
	vString* const str = vStringNewInit (extension);
	Assert (0 <= language  &&  language < (int) LanguageCount);
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
static void addTgEntryFull (const langType language, vString* const spec, unsigned char* const tg_table,
			    vString* corpus_file)
{
	tgTableEntry *entry;

	entry = xMalloc (1, tgTableEntry);
	entry->spec = spec;
	entry->tg_table = tg_table;
	entry->corpus_file = corpus_file;
	entry->next = LanguageTable [language]->tg_entries;
	LanguageTable [language]->tg_entries = entry;

	if (corpus_file)
		verbose ("Compile corpus %s for %s of %s\n",
			 vStringValue (corpus_file), vStringValue (spec), LanguageTable [language]->name);
	else
		verbose ("Install tg for %s of %s\n", vStringValue (spec), LanguageTable [language]->name);
}

extern void addCorpusFile (const langType language,
			   const char* const spec, vString* const corpus_file, boolean pattern_p)
{
	FILE *input;
	unsigned char* tg_table;
	vString* vspec;

	input = fopen (vStringValue (corpus_file), "rb");
	if (input == NULL)
		error (FATAL,
		       "failed in open %s as corpus", vStringValue (corpus_file));

	tg_table = tg_create ();
	if (!tg_table)
		error (FATAL,
		       "failed allocating memory for tg entry");

	tg_load (tg_table, input);
	fclose (input);

	vspec = pattern_p? vStringNewInit (spec): ext2ptrnNew (spec);
	addTgEntryFull (language, vspec, tg_table, corpus_file);
}

extern void addTgEntryForPattern (const langType language, const char* const ptrn, unsigned char* const tg_table)
{
	addTgEntryFull (language, vStringNewInit (ptrn), tg_table, NULL);
}

extern void addTgEntryForExtension (const langType language, const char* const ext, unsigned char* const tg_table)
{
	vString *const ptrn = ext2ptrnNew (ext);
	addTgEntryFull (language, ptrn, tg_table, NULL);
}

static tgTableEntry* freeTgEntry(tgTableEntry *entry)
{
	tgTableEntry* r;

	if (entry->corpus_file)
	{
		eFree (entry->tg_table);
		entry->tg_table = NULL;
		vStringDelete (entry->corpus_file);
		entry->corpus_file = NULL;
	}

	vStringDelete (entry->spec);
	entry->spec = NULL;

	r = entry->next;
	entry->next = NULL;
	eFree(entry);

	return r;
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

static void initializeParsers (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		if (LanguageTable [i]->initialize != NULL)
			(LanguageTable [i]->initialize) ((langType) i);
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
			else if (def->method & ( METHOD_REGEX | METHOD_NOT_CRAFTED ))
			{
#ifdef HAVE_REGEX
				def->parser = findRegexTags;
				accepted = TRUE;
#endif
			}
			else if (((!!def->parser) + (!!def->parser2) + (!!def->parser_with_gc)) != 1)
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
	enableLanguages (TRUE);
	initializeParsers ();
}

extern void freeParserResources (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		parserDefinition* const lang = LanguageTable [i];

		if (lang->finalize)
			(lang->finalize)((langType)i);

		freeList (&lang->currentPatterns);
		freeList (&lang->currentExtensions);
		freeList (&lang->currentAliaes);

		while (lang->tg_entries)
			lang->tg_entries = freeTgEntry(lang->tg_entries);

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

static void lazyInitialize (langType language)
{
	parserDefinition* lang;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	lang->parser = doNothing;

	if (lang->method & (METHOD_NOT_CRAFTED|METHOD_REGEX))
		lang->parser = findRegexTags;

}
#endif

/*
*   Option parsing
*/

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
		unsigned int i = LanguageCount++;
		parserDefinition* const def = parserNew (parameter);
		def->initialize        = lazyInitialize;
		def->currentPatterns   = stringListNew ();
		def->currentExtensions = stringListNew ();
		def->method            = METHOD_NOT_CRAFTED;
		def->enabled           = TRUE;
		def->id                = i;
		LanguageTable = xRealloc (LanguageTable, i + 1, parserDefinition*);
		LanguageTable [i] = def;
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
	boolean handled = FALSE;
	const char* const dash = strchr (option, '-');
	if (dash != NULL  &&
		(strcmp (dash + 1, "kinds") == 0  ||  strcmp (dash + 1, "types") == 0))
	{
		langType language;
		size_t len = dash - option;

		if ((len == 1) && (*option == '*'))
		{
			struct langKindOptionStruct arg = {
				.option = option,
				.parameter = parameter,
			};

			foreachLanguage(processLangKindOptionEach, &arg);
		}
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
	return handled;
}

static void printLanguageKind (const kindOption* const kind, boolean indent)
{
	const char *const indentation = indent ? "    " : "";
	printf ("%s%c  %s%s\n", indentation, kind->letter,
		kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
		kind->enabled ? "" : " [off]");
}

static void printKinds (langType language, boolean indent)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL)
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			printLanguageKind (lang->kinds + i, indent);
	}
	printRegexKinds (language, indent);
	printXcmdKinds (language, indent);
}

extern void printLanguageKinds (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];
			printf ("%s%s\n", lang->name, enabled_p (i) ? "" : " [disabled]");
			printKinds (i, TRUE);
		}
	}
	else
		printKinds (language, FALSE);
}

static void processLangAliasOption (
		const langType language, const char *const option __unused__,
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
		vString* tmp;
		if (lang->currentAliaes)
		{
			alias = parameter + 1;
			tmp = stringListExtensionFinds(lang->currentAliaes, alias);
			if (tmp)
			{
				verbose ("remove alias %s from %s\n", alias, lang->name);
				stringListRemoveExtension (lang->currentAliaes, alias);
				vStringDelete (tmp);
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
	const char* const dash = strchr (option, '-');
	langType language;
	vString* langName;
	size_t len;

	if (dash == NULL ||
	    (strcmp (dash + 1, "alias") != 0))
		return FALSE;
	len = dash - option;

	langName = vStringNew ();
	vStringNCopyS (langName, option, len);
	language = getNamedLanguage (vStringValue (langName));
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", vStringValue (langName), option);
	vStringDelete(langName);

	processLangAliasOption (language, option, parameter);
	return TRUE;
}

static void printCorpus (langType language, const char* const spec, boolean indent)
{
	const parserDefinition* lang;
	const tgTableEntry* entry;
	const char *const indentation = indent ? "    " : "";

	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	for (entry = lang->tg_entries;
	     entry != NULL;
	     entry = entry->next)
	{
		const char* corpus_file = entry->corpus_file? vStringValue (entry->corpus_file): "ctags";

		if (spec == NULL)
			printf("%s%s: %s\n", indentation, vStringValue (entry->spec), corpus_file);
		else if (strcmp (vStringValue (entry->spec), spec) == 0)
			printf("%s%s: %s\n", indentation, vStringValue (entry->spec), corpus_file);
	}
}

extern void printLanguageCorpus (langType language,
				 const char *const spec)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];
			printf ("%s%s\n", lang->name, enabled_p (i) ? "" : " [disabled]");
			printCorpus (i, spec, TRUE);
		}
	}
	else
		printCorpus (language, spec, FALSE);
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

static void printAliases (const langType language)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];

	if (lang->currentAliaes != NULL)
		for (i = 0  ;  i < stringListCount (lang->currentAliaes)  ;  ++i)
			printf (" %s", vStringValue (
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
		printAliases (language);
		putchar ('\n');
	}
}

static void printLanguage (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  (lang->method & METHOD_REGEX) || (lang->method & METHOD_XCMD))
		printf ("%s%s\n", lang->name, enabled_p (language) ? "" : " [disabled]");
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

static void makeFileTag (const char *const fileName)
{
	if (Option.include.fileNames)
	{
		tagEntryInfo tag;
		initTagEntry (&tag, baseFilename (fileName));

		tag.isFileEntry     = TRUE;
		tag.lineNumberEntry = TRUE;
		tag.lineNumber      = 1;
		tag.kindName        = "file";
		tag.kind            = 'F';

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
		const parserDefinition* const lang = LanguageTable [language];
		if (Option.etags)
			beginEtagsFile ();

		makeFileTag (fileName);

	  retry:
		if (lang->parser != NULL)
			lang->parser ();
		else if (lang->parser2 != NULL)
			rescan = lang->parser2 (passCount);
		else if (lang->parser_with_gc != NULL)
		{
			TrashBox* trash_box = trashBoxNew ();
			jmp_buf jbuf;

			while (!setjmp (jbuf))
			{
				trashBoxDelete (trash_box);
				trash_box = trashBoxNew ();
				rescan = lang->parser_with_gc (passCount, &jbuf, trash_box);
				break;
			}
			trashBoxDelete (trash_box);
		}
		else if (lang->initialize != NULL)
		{
			parserInitialize init = lang->initialize;
			init(language);
			Assert (lang->parser || lang->parser2);
			goto retry;
		}

		if (Option.etags)
			endEtagsFile (getSourceFileTagPath ());

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
	else if (! enabled_p (language))
		verbose ("ignoring %s (language disabled)\n", fileName);
	else
	{
		if (Option.filter)
			openTagFile ();

		tagFileResized = createTagsWithFallback (fileName, language);
#ifdef HAVE_COPROC
		tagFileResized = invokeXcmd (fileName, language)? TRUE: tagFileResized;
#endif

		if (Option.filter)
			closeTagFile (tagFileResized);
		addTotals (1, 0L, 0L);

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
		addLanguagePatternMap (language, vStringValue (ptrn));
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

/* vi:set tabstop=4 shiftwidth=4 nowrap: */
