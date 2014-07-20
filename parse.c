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

static langType getExtensionLanguage (const char *const extension)
{
	langType result = LANG_IGNORE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		if (exts != NULL  &&  stringListExtensionMatched (exts, extension))
			result = i;
	}
	return result;
}

static langType getExtensionOrNameLanguage (const char *const key, langType start_index)
{
	langType result = LANG_IGNORE;
	unsigned int i;


	if (start_index == LANG_AUTO)
	        start_index = 0;
	else if (start_index == LANG_IGNORE || start_index >= LanguageCount)
		return result;

	for (i = start_index  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		const parserDefinition* const lang = LanguageTable [i];
		stringList* const exts = lang->currentExtensions;

		if (exts != NULL  &&  stringListExtensionMatched (exts, key))
			result = i;
		else if (lang->name != NULL && strcasecmp (key, lang->name) == 0)
			result = i;
	}
	return result;
}

/* If multiple parsers are found, return LANG_AUTO */
static unsigned int nominateLanguageCandidates (const char *const key, langType** candidates)
{
	unsigned int count;
	langType i;

	*candidates = xMalloc(LanguageCount, langType);
	for (i = 0; i < LanguageCount; i++)
		(*candidates)[i] = LANG_IGNORE;

	for (count = 0, i = LANG_AUTO; i != LANG_IGNORE; )
	{
		i = getExtensionOrNameLanguage(key, i);
		if (i != LANG_IGNORE)
			(*candidates)[count++] = i++;
	}

	return count;
}

static langType getPatternLanguage (const char *const fileName)
{
	langType result = LANG_IGNORE;
	const char* base = baseFilename (fileName);
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const ptrns = LanguageTable [i]->currentPatterns;
		if (ptrns != NULL  &&  stringListFileMatched (ptrns, base))
			result = i;
	}
	return result;
}

#ifdef SYS_INTERPRETER
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

#endif

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

static vString* determineVimFileType (const char *const line)
{
	/* considerable combinations:
	   --------------------------
	   set ... filetype=
	   se ... filetype=
	   set ... ft=
	   se ... ft= */

	int i, j;
	const char* p;
	const char* q;

	const char* const set_prefix[] = {"set ", "se "};
	const char* const filetype_prefix[] = {"filetype=", "ft="};
	vString* const filetype = vStringNew ();

	for (i = 0; i < sizeof(set_prefix)/sizeof(set_prefix[0]); i++)
	{
		if ((p = strstr(line, set_prefix[i])) == NULL)
			continue;
		p += strlen(set_prefix[i]);
		for (j = 0; j < sizeof(filetype_prefix)/sizeof(filetype_prefix[0]); j++)
		{
			if ((q = strstr(p, filetype_prefix[j])) == NULL)
				continue;
			q += strlen(filetype_prefix[j]);
			for ( ;  *q != '\0'  &&  isalnum ((int) *q)  ;  ++q)
				vStringPut (filetype, (int) *q);
			vStringTerminate (filetype);
			goto out;
		}
	}
  out:
	return filetype;
}

static langType getVimFileTypeLanguage (const char *const fileName)
{
	/* http://vimdoc.sourceforge.net/htmldoc/options.html#modeline

	   [text]{white}{vi:|vim:|ex:}[white]se[t] {options}:[text]
	   options=> filetype=TYPE or ft=TYPE

	   'modelines' 'mls'	number	(default 5)
			global
			{not in Vi}
	    If 'modeline' is on 'modelines' gives the number of lines that is
	    checked for set commands. */

	langType result = LANG_IGNORE;
	FILE* const fp = fopen (fileName, "r");
#define RING_SIZE 5
	vString* ring[RING_SIZE];
	int i, j, k;
	const char* const prefix[] = {
		"vim:", "vi:", "ex:"
	};

	if (fp == NULL)
		return result;

	for (i = 0; i < RING_SIZE; i++)
		ring[i] = vStringNew ();

	i = 0;
	while ((readLine (ring[i++], fp)) != NULL)
		if (i == RING_SIZE)
			i = 0;

	j = i;
	do
	{
		const char* p;
		vString* filetype = NULL;

		j--;
		if (j < 0)
			j = RING_SIZE - 1;

		for (k = 0; k < (sizeof(prefix)/sizeof(prefix[0])); k++)
			if ((p = strstr (vStringValue (ring[j]), prefix[k])) != NULL)
			{
				p += strlen(prefix[k]);
				for ( ;  isspace ((int) *p)  ;  ++p)
					;  /* no-op */
				filetype = determineVimFileType(p);
				break;
			}

		if (filetype != NULL)
		{
			result = getExtensionOrNameLanguage (vStringValue (filetype), LANG_AUTO);
			vStringDelete (filetype);
		}
	} while (((i == RING_SIZE)? (j != RING_SIZE - 1): (j != i)) && result == LANG_IGNORE);

	for (i = RING_SIZE - 1; i >= 0; i--)
		vStringDelete (ring[i]);

	fclose(fp);
#undef RING_SIZE
	return result;

	/* TODO:
	   [text]{white}{vi:|vim:|ex:}[white]{options} */
}

static const tgTableEntry* const findTgTableEntry(const parserDefinition* const lang, const char* const extension)
{
	const tgTableEntry *entry;

	for (entry = lang->tg_entries;
	     entry->extension != NULL;
	     entry++)
		if (strcmp (entry->extension, extension) == 0)
			break;
	if (entry->extension == NULL)
		entry = NULL;

	return entry;
}

static langType determineTwoGramLanguage(const unsigned char *const t,
					 const langType  * const candidates, unsigned int n_candidates,
					 const char* const extension)
{
	unsigned int i, winner;

	for (winner = 0, i = 1; i < n_candidates; i++)
	{
		int r;
		const tgTableEntry *const winner_entry = findTgTableEntry(LanguageTable[candidates[winner]],
									  extension);
		const tgTableEntry *const i_entry = findTgTableEntry(LanguageTable[candidates[i]],
								     extension);

		r = tg_compare(winner_entry->tg_table, i_entry->tg_table, t);

		verbose ("tg tournament %s:%s v.s. %s:%s => %d\n",
			 LanguageTable[candidates[winner]]->name, winner_entry->extension,
			 LanguageTable[candidates[i]]->name, i_entry->extension,
			 r);

		if (r > 0)
			winner = i;
	}
	return candidates[winner];
}

static langType getTwoGramLanguage (const char *const fileName, const char *const spec,
				    const langType  *const candidates, unsigned int n_candidates)
{
	langType result;
	unsigned int i;

	for (result = LANG_AUTO, i = 0; candidates[i] != LANG_IGNORE; i++)
		if (LanguageTable [candidates[i]]->tg_entries == NULL
		    || findTgTableEntry(LanguageTable [candidates[i]], spec) == NULL)
		{
			result = LANG_IGNORE;
			break;
		}

	if (result == LANG_AUTO)
	{
		FILE *fp;

		fp = fopen(fileName, "rb");
		if (fp)
		{
			unsigned char* t;

			t = tg_create();
			tg_load(t, fp);
			fclose(fp);

			result = determineTwoGramLanguage(t, candidates, n_candidates, spec);

			verbose("tg tournament filename %s winner: %s\n",
				fileName, LanguageTable[result]->name);

			tg_destroy(t);

		}
		else
			result = LANG_IGNORE;
	}
	return result;
}

static langType getSpecLanguage (const char *const fileName, const char *const spec)
{
	langType language;
	langType  *candidates;
	unsigned int n_candidates;

	n_candidates = nominateLanguageCandidates(spec, &candidates);

	if (n_candidates == 1)
		language = candidates[0];
	else if (n_candidates > 1)
	{
		language = getTwoGramLanguage(fileName, spec, candidates, n_candidates);
		if (language == LANG_IGNORE)
			language = candidates[0];
	}
	else
		language = LANG_IGNORE;

	eFree(candidates);
	candidates = NULL;

	return language;
}

extern langType getFileLanguage (const char *const fileName)
{
	langType language = Option.language;
	if (language == LANG_AUTO)
	{
		vString* spec;
		FILE* input;

		language = LANG_IGNORE;

		input = fopen (fileName, "rb");
		if (!input)
			goto nofp;

		if ((spec = extracEmacsModeAtFirstLine (input)))
		{
			language = getSpecLanguage (fileName, vStringValue(spec));
			vStringDelete (spec);
		}
		rewind(input);

#ifdef SYS_INTERPRETER
		if (language == LANG_IGNORE && (spec = extractInterpreter (input)))
		{
			language = getSpecLanguage (fileName, vStringValue(spec));
			vStringDelete (spec);
		}
		rewind(input);
#endif

		if (language == LANG_IGNORE && (spec = extractEmacsModeLanguageAtEOF (input)))
		{
			language = getSpecLanguage (fileName, vStringValue(spec));
			vStringDelete (spec);
		}
		rewind(input);

		fclose (input);

	  nofp:
		if (language == LANG_IGNORE)
			language = getVimFileTypeLanguage (fileName);
		if (language == LANG_IGNORE)
		{
			const char* const ext = fileExtension (fileName);
			language = getSpecLanguage(fileName, ext);
		}

		if (language == LANG_IGNORE)
			language = getPatternLanguage (fileName);
	}
	return language;
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

extern void clearLanguageMap (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	stringListClear (LanguageTable [language]->currentPatterns);
	stringListClear (LanguageTable [language]->currentExtensions);
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

	builtInCount = sizeof (BuiltInParsers) / sizeof (BuiltInParsers [0]);
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
			else if (def->regex)
			{
#ifdef HAVE_REGEX
				def->parser = findRegexTags;
				accepted = TRUE;
#endif
			}
			else if ((def->parser == NULL)  ==  (def->parser2 == NULL))
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
		eFree (lang->name);
		lang->name = NULL;
		eFree (lang);
	}
	if (LanguageTable != NULL)
		eFree (LanguageTable);
	LanguageTable = NULL;
	LanguageCount = 0;
}

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
		def->parser            = findRegexTags;
		def->currentPatterns   = stringListNew ();
		def->currentExtensions = stringListNew ();
		def->regex             = TRUE;
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
	if (lang->regex)
		resetRegexKinds (language, mode);
	else
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			lang->kinds [i].enabled = mode;
	}
}

static boolean enableLanguageKind (
		const langType language, const int kind, const boolean mode)
{
	boolean result = FALSE;
	if (LanguageTable [language]->regex)
		result = enableRegexKind (language, kind, mode);
	else
	{
		kindOption* const opt = langKindOption (language, kind);
		if (opt != NULL)
		{
			opt->enabled = mode;
			result = TRUE;
		}
	}
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
	if (lang->kinds != NULL  ||  lang->regex)
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			printLanguageKind (lang->kinds + i, indent);
		printRegexKinds (language, indent);
	}
}

extern void printLanguageKinds (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];
			printf ("%s%s\n", lang->name, lang->enabled ? "" : " [disabled]");
			printKinds (i, TRUE);
		}
	}
	else
		printKinds (language, FALSE);
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

static void printLanguage (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  lang->regex)
		printf ("%s%s\n", lang->name, lang->enabled ? "" : " [disabled]");
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

		if (lang->parser != NULL)
			lang->parser ();
		else if (lang->parser2 != NULL)
			rescan = lang->parser2 (passCount);

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

extern boolean parseFile (const char *const fileName)
{
	boolean tagFileResized = FALSE;
	langType language = Option.language;
	if (Option.language == LANG_AUTO)
		language = getFileLanguage (fileName);
	Assert (language != LANG_AUTO);
	if (language == LANG_IGNORE)
		verbose ("ignoring %s (unknown language)\n", fileName);
	else if (! LanguageTable [language]->enabled)
		verbose ("ignoring %s (language disabled)\n", fileName);
	else
	{
		if (Option.filter)
			openTagFile ();

		tagFileResized = createTagsWithFallback (fileName, language);

		if (Option.filter)
			closeTagFile (tagFileResized);
		addTotals (1, 0L, 0L);

		return tagFileResized;
	}
	return tagFileResized;
}

/* vi:set tabstop=4 shiftwidth=4 nowrap: */
