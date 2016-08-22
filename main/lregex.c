/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include <ctype.h>
#include <stddef.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>  /* declare off_t (not known to regex.h on FreeBSD) */
#endif
#include <regex.h>

#include "debug.h"
#include "entry.h"
#include "flags.h"
#include "htable.h"
#include "kind.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

static boolean regexAvailable = FALSE;
static unsigned long currentScope = CORK_NIL;

/*
*   MACROS
*/

/* Back-references \0 through \9 */
#define BACK_REFERENCE_COUNT 10


#define REGEX_NAME "Regex"

/*
*   DATA DECLARATIONS
*/

union cptr {
	char c;
	void *p;
};

enum pType { PTRN_TAG, PTRN_CALLBACK };

enum scopeAction {
	SCOPE_REF     = 1UL << 0,
	SCOPE_POP     = 1UL << 1,
	SCOPE_PUSH    = 1UL << 2,
	SCOPE_CLEAR   = 1UL << 3,
	SCOPE_PLACEHOLDER = 1UL << 4,
};

typedef struct {
	regex_t *pattern;
	enum pType type;
	boolean exclusive;
	boolean accept_empty_name;
	union {
		struct {
			char *name_pattern;
			kindOption *kind;
		} tag;
		struct {
			regexCallback function;
			void *userData;
		} callback;
	} u;
	unsigned int scopeActions;
	boolean *disabled;
} regexPattern;


typedef struct {
	regexPattern *patterns;
	unsigned int count;
	hashTable   *kinds;
} patternSet;

/*
*   DATA DEFINITIONS
*/

/* Array of pattern sets, indexed by language */
static patternSet* Sets = NULL;
static int SetUpper = -1;  /* upper language index in list */

/*
*   FUNCTION DEFINITIONS
*/


static void clearPatternSet (const langType language)
{
	if (language <= SetUpper)
	{
		patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
		{
			regexPattern *p = &set->patterns [i];
			regfree (p->pattern);
			eFree (p->pattern);
			p->pattern = NULL;

			if (p->type == PTRN_TAG)
			{
				eFree (p->u.tag.name_pattern);
				p->u.tag.name_pattern = NULL;
				p->u.tag.kind = NULL;
			}
		}
		if (set->patterns != NULL)
			eFree (set->patterns);
		set->patterns = NULL;
		set->count = 0;
		hashTableDelete (set->kinds);
		set->kinds = NULL;
	}
}

/*
*   Regex pseudo-parser
*/

static int makeRegexTag (
		const vString* const name, const kindOption* const kind, int scopeIndex, int placeholder)
{
	if (kind->enabled)
	{
		tagEntryInfo e;
		Assert (name != NULL  &&  ((vStringLength (name) > 0) || placeholder));
		Assert (kind != NULL);
		initTagEntry (&e, vStringValue (name), kind);
		e.extensionFields.scopeIndex = scopeIndex;
		e.placeholder = !!placeholder;
		return makeTagEntry (&e);
	}
	else
		return CORK_NIL;
}

/*
*   Regex pattern definition
*/

/* Take a string like "/blah/" and turn it into "blah", making sure
 * that the first and last characters are the same, and handling
 * quoted separator characters.  Actually, stops on the occurrence of
 * an unquoted separator.  Also turns "\t" into a Tab character.
 * Returns pointer to terminating separator.  Works in place.  Null
 * terminates name string.
 */
static char* scanSeparators (char* name)
{
	char sep = name [0];
	char *copyto = name;
	boolean quoted = FALSE;

	for (++name ; *name != '\0' ; ++name)
	{
		if (quoted)
		{
			if (*name == sep)
				*copyto++ = sep;
			else if (*name == 't')
				*copyto++ = '\t';
			else
			{
				/* Something else is quoted, so preserve the quote. */
				*copyto++ = '\\';
				*copyto++ = *name;
			}
			quoted = FALSE;
		}
		else if (*name == '\\')
			quoted = TRUE;
		else if (*name == sep)
		{
			break;
		}
		else
			*copyto++ = *name;
	}
	*copyto = '\0';
	return name;
}

/* Parse `regexp', in form "/regex/name/[k,Kind/]flags" (where the separator
 * character is whatever the first character of `regexp' is), by breaking it
 * up into null terminated strings, removing the separators, and expanding
 * '\t' into tabs. When complete, `regexp' points to the line matching
 * pattern, a pointer to the name matching pattern is written to `name', a
 * pointer to the kinds is written to `kinds' (possibly NULL), and a pointer
 * to the trailing flags is written to `flags'. If the pattern is not in the
 * correct format, a false value is returned.
 */
static boolean parseTagRegex (
		char* const regexp, char** const name,
		char** const kinds, char** const flags)
{
	boolean result = FALSE;
	const int separator = (unsigned char) regexp [0];

	*name = scanSeparators (regexp);
	if (*regexp == '\0')
		error (WARNING, "empty regexp");
	else if (**name != separator)
		error (WARNING, "%s: incomplete regexp", regexp);
	else
	{
		char* const third = scanSeparators (*name);
		if (**name != '\0' && (*name) [strlen (*name) - 1] == '\\')
			error (WARNING, "error in name pattern: \"%s\"", *name);
		if (*third != separator)
			error (WARNING, "%s: regexp missing final separator", regexp);
		else
		{
			char* const fourth = scanSeparators (third);
			if (*fourth == separator)
			{
				*kinds = third;
				scanSeparators (fourth);
				*flags = fourth;
			}
			else
			{
				*flags = third;
				*kinds = NULL;
			}
			result = TRUE;
		}
	}
	return result;
}


static void pre_ptrn_flag_exclusive_short (char c CTAGS_ATTR_UNUSED, void* data)
{
	boolean *exclusive = data;
	*exclusive = TRUE;
}

static void pre_ptrn_flag_exclusive_long (const char* const s CTAGS_ATTR_UNUSED, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	pre_ptrn_flag_exclusive_short ('x', data);
}

static flagDefinition prePtrnFlagDef[] = {
	{ 'x',  "exclusive", pre_ptrn_flag_exclusive_short, pre_ptrn_flag_exclusive_long ,
	  NULL, "skip testing the other patterns if a line is matched to this pattern"},
};

static void scope_ptrn_flag_eval (const char* const f  CTAGS_ATTR_UNUSED,
				  const char* const v, void* data)
{
	unsigned long *bfields = data;

	if (strcmp (v, "ref") == 0)
		*bfields |= SCOPE_REF;
	else if (strcmp (v, "push") == 0)
		*bfields |= (SCOPE_PUSH | SCOPE_REF);
	else if (strcmp (v, "pop") == 0)
		*bfields |= SCOPE_POP;
	else if (strcmp (v, "clear") == 0)
		*bfields |= SCOPE_CLEAR;
	else if (strcmp (v, "set") == 0)
		*bfields |= (SCOPE_CLEAR | SCOPE_PUSH);
	else
		error (FATAL, "Unexpected value for scope flag in regex definition: scope=%s", v);
}

static void placeholder_ptrn_flag_eval (const char* const f  CTAGS_ATTR_UNUSED,
				     const char* const v  CTAGS_ATTR_UNUSED, void* data)
{
	unsigned long *bfields = data;
	*bfields |= SCOPE_PLACEHOLDER;
}

static flagDefinition scopePtrnFlagDef[] = {
	{ '\0', "scope",     NULL, scope_ptrn_flag_eval,
	  "ACTION", "use scope stack: ACTION = ref|push|pop|clear|set"},
	{ '\0', "placeholder",  NULL, placeholder_ptrn_flag_eval,
	  NULL, "don't put this tag to tags file."},
};

static kindOption *kindNew ()
{
	kindOption *kind = xCalloc (1, kindOption);
	kind->letter        = '\0';
	kind->name = NULL;
	kind->description = NULL;
	kind->enabled = FALSE;
	kind->referenceOnly = FALSE;
	kind->nRoles = 0;
	kind->roles = NULL;
	return kind;
}

static void kindFree (void *data)
{
	kindOption *kind = data;
	kind->letter = '\0';
	if (kind->name)
	{
		eFree ((void *)kind->name);
		kind->name = NULL;
	}
	if (kind->description)
	{
		eFree ((void *)kind->description);
		kind->description = NULL;
	}
	eFree (kind);
}



static regexPattern* addCompiledTagCommon (const langType language,
					   regex_t* const pattern,
					   char kind_letter)
{
	patternSet* set;
	regexPattern *ptrn;
	kindOption *kind = NULL;

	if (language > SetUpper)
	{
		int i;
		Sets = xRealloc (Sets, (language + 1), patternSet);
		for (i = SetUpper + 1  ;  i <= language  ;  ++i)
		{
			Sets [i].patterns = NULL;
			Sets [i].count = 0;
			Sets [i].kinds = hashTableNew (11,
						       hashPtrhash,
						       hashPtreq,
						       NULL,
						       kindFree);
		}
		SetUpper = language;
	}
	set = Sets + language;
	set->patterns = xRealloc (set->patterns, (set->count + 1), regexPattern);

	if (kind_letter)
	{
		union cptr c = { .p = NULL };

		c.c = kind_letter;
		kind = hashTableGetItem (set->kinds, c.p);
		if (!kind)
		{
			kind = kindNew ();
			hashTablePutItem (set->kinds, c.p, (void *)kind);
		}
	}
	ptrn = &set->patterns [set->count];
	memset (ptrn, 0, sizeof (*ptrn));
	ptrn->pattern = pattern;
	ptrn->exclusive = FALSE;
	ptrn->accept_empty_name = FALSE;
	if (kind_letter)
		ptrn->u.tag.kind = kind;
	set->count += 1;
	useRegexMethod(language);
	return ptrn;
}

static regexPattern *addCompiledTagPattern (const langType language, regex_t* const pattern,
					    const char* const name, char kind, const char* kindName,
					    char *const description, const char* flags,
					    boolean *disabled)
{
	regexPattern * ptrn;
	boolean exclusive = FALSE;
	unsigned long scopeActions = 0UL;

	flagsEval (flags, prePtrnFlagDef, ARRAY_SIZE(prePtrnFlagDef), &exclusive);
	flagsEval (flags, scopePtrnFlagDef, ARRAY_SIZE(scopePtrnFlagDef), &scopeActions);
	if (*name == '\0' && exclusive && kind == KIND_REGEX_DEFAULT)
	{
		kind = KIND_GHOST;
		kindName = KIND_GHOST_LONG;
	}
	ptrn  = addCompiledTagCommon(language, pattern, kind);
	ptrn->type    = PTRN_TAG;
	ptrn->u.tag.name_pattern = eStrdup (name);
	ptrn->exclusive = exclusive;
	ptrn->scopeActions = scopeActions;
	ptrn->disabled = disabled;
	if (ptrn->u.tag.kind->letter == '\0')
	{
		/* This is a newly registered kind. */
		ptrn->u.tag.kind->letter  = kind;
		ptrn->u.tag.kind->enabled = TRUE;
		ptrn->u.tag.kind->name    = kindName? eStrdup (kindName): NULL;
		ptrn->u.tag.kind->description = description? eStrdup (description): NULL;
	}
	else if (ptrn->u.tag.kind->name && kindName && strcmp(ptrn->u.tag.kind->name, kindName))
	{
		/* When using a same kind letter for multiple regex patterns, the name of kind
		   should be the same. */
		error  (WARNING, "Don't reuse the kind letter `%c' in a language %s (old: \"%s\", new: \"%s\")",
			ptrn->u.tag.kind->letter, getLanguageName (language),
			ptrn->u.tag.kind->name, kindName);
	}

	return ptrn;
}

static void addCompiledCallbackPattern (const langType language, regex_t* const pattern,
					const regexCallback callback, const char* flags,
					boolean *disabled,
					void *userData)
{
	regexPattern * ptrn;
	boolean exclusive = FALSE;
	flagsEval (flags, prePtrnFlagDef, ARRAY_SIZE(prePtrnFlagDef), &exclusive);
	ptrn  = addCompiledTagCommon(language, pattern, '\0');
	ptrn->type    = PTRN_CALLBACK;
	ptrn->u.callback.function = callback;
	ptrn->u.callback.userData = userData;
	ptrn->exclusive = exclusive;
	ptrn->disabled = disabled;
}


static void regex_flag_basic_short (char c CTAGS_ATTR_UNUSED, void* data)
{
	int* cflags = data;
	*cflags &= ~REG_EXTENDED;
}
static void regex_flag_basic_long (const char* const s CTAGS_ATTR_UNUSED, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	regex_flag_basic_short ('b', data);
}

static void regex_flag_extend_short (char c CTAGS_ATTR_UNUSED, void* data)
{
	int* cflags = data;
	*cflags |= REG_EXTENDED;
}

static void regex_flag_extend_long (const char* const c CTAGS_ATTR_UNUSED, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	regex_flag_extend_short('e', data);
}

static void regex_flag_icase_short (char c CTAGS_ATTR_UNUSED, void* data)
{
	int* cflags = data;
	*cflags |= REG_ICASE;
}

static void regex_flag_icase_long (const char* s CTAGS_ATTR_UNUSED, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	regex_flag_icase_short ('i', data);
}


static flagDefinition regexFlagDefs[] = {
	{ 'b', "basic",  regex_flag_basic_short,  regex_flag_basic_long,
	  NULL, "interpreted as a Posix basic regular expression."},
	{ 'e', "extend", regex_flag_extend_short, regex_flag_extend_long,
	  NULL, "interpreted as a Posix extended regular expression (default)"},
	{ 'i', "icase",  regex_flag_icase_short,  regex_flag_icase_long,
	  NULL, "applied in a case-insensitive manner"},
};

static regex_t* compileRegex (const char* const regexp, const char* const flags)
{
	int cflags = REG_EXTENDED | REG_NEWLINE;
	regex_t *result = NULL;
	int errcode;

	flagsEval (flags,
		   regexFlagDefs,
		   ARRAY_SIZE(regexFlagDefs),
		   &cflags);

	result = xMalloc (1, regex_t);
	errcode = regcomp (result, regexp, cflags);
	if (errcode != 0)
	{
		char errmsg[256];
		regerror (errcode, result, errmsg, 256);
		error (WARNING, "regcomp %s: %s", regexp, errmsg);
		regfree (result);
		eFree (result);
		result = NULL;
	}
	return result;
}


static void parseKinds (
		const char* const kinds, char* const kind, char** const kindName,
		char **description)
{
	*kind = '\0';
	*kindName = NULL;
	*description = NULL;
	if (kinds == NULL  ||  kinds [0] == '\0')
	{
		*kind = KIND_REGEX_DEFAULT;
		*kindName = eStrdup (KIND_REGEX_DEFAULT_LONG);
	}
	else if (kinds [0] != '\0')
	{
		const char* k = kinds;
		if (k [0] != ','  &&  (k [1] == ','  ||  k [1] == '\0'))
			*kind = *k++;
		else
			*kind = KIND_REGEX_DEFAULT;
		if (*k == ',')
			++k;
		if (k [0] == '\0')
			*kindName = eStrdup (KIND_REGEX_DEFAULT_LONG);
		else
		{
			const char *const comma = strchr (k, ',');
			if (comma == NULL)
				*kindName = eStrdup (k);
			else
			{
				*kindName = (char*) eMalloc (comma - k + 1);
				strncpy (*kindName, k, comma - k);
				(*kindName) [comma - k] = '\0';
				k = comma + 1;
				if (k [0] != '\0')
					*description = eStrdup (k);
			}
		}
	}
}

static void processLanguageRegex (const langType language,
		const char* const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		clearPatternSet (language);
	else if (parameter [0] != '@')
		addLanguageRegex (language, parameter);
	else if (! doesFileExist (parameter + 1))
		error (WARNING, "cannot open regex file");
	else
	{
		const char* regexfile = parameter + 1;
		MIO* const mio = mio_new_file (regexfile, "r");
		if (mio == NULL)
			error (WARNING | PERROR, "%s", regexfile);
		else
		{
			vString* const regex = vStringNew ();
			while (readLineRaw (regex, mio))
				addLanguageRegex (language, vStringValue (regex));
			mio_free (mio);
			vStringDelete (regex);
		}
	}
}

/*
*   Regex pattern matching
*/


static vString* substitute (
		const char* const in, const char* out,
		const int nmatch, const regmatch_t* const pmatch)
{
	vString* result = vStringNew ();
	const char* p;
	for (p = out  ;  *p != '\0'  ;  p++)
	{
		if (*p == '\\'  &&  isdigit ((int) *++p))
		{
			const int dig = *p - '0';
			if (0 < dig  &&  dig < nmatch  &&  pmatch [dig].rm_so != -1)
			{
				const int diglen = pmatch [dig].rm_eo - pmatch [dig].rm_so;
				vStringNCatS (result, in + pmatch [dig].rm_so, diglen);
			}
		}
		else if (*p != '\n'  &&  *p != '\r')
			vStringPut (result, *p);
	}
	vStringTerminate (result);
	return result;
}

static void matchTagPattern (const vString* const line,
		const regexPattern* const patbuf,
		const regmatch_t* const pmatch)
{
	vString *const name = substitute (vStringValue (line),
			patbuf->u.tag.name_pattern, BACK_REFERENCE_COUNT, pmatch);
	boolean placeholder = !!((patbuf->scopeActions & SCOPE_PLACEHOLDER) == SCOPE_PLACEHOLDER);
	unsigned long scope = CORK_NIL;
	int n;

	vStringStripLeading (name);
	vStringStripTrailing (name);

	if (patbuf->scopeActions & SCOPE_REF)
	{
		tagEntryInfo *entry;

		scope = currentScope;
		while ((entry = getEntryInCorkQueue (scope)) && entry->placeholder)
			/* Look at parent */
			scope = entry->extensionFields.scopeIndex;
	}
	if (patbuf->scopeActions & SCOPE_CLEAR)
		currentScope = CORK_NIL;
	if (patbuf->scopeActions & SCOPE_POP)
	{
		tagEntryInfo *entry = getEntryInCorkQueue (currentScope);
		currentScope = entry? entry->extensionFields.scopeIndex: CORK_NIL;
	}

	if (vStringLength (name) == 0 && (placeholder == FALSE))
	{
		if (patbuf->accept_empty_name == FALSE)
			error (WARNING, "%s:%ld: null expansion of name pattern \"%s\"",
			       getInputFileName (), getInputLineNumber (),
			       patbuf->u.tag.name_pattern);
		n = CORK_NIL;
	}
	else
		n = makeRegexTag (name, patbuf->u.tag.kind, scope, placeholder);

	if (patbuf->scopeActions & SCOPE_PUSH)
		currentScope = n;

	vStringDelete (name);
}

static void matchCallbackPattern (
		const vString* const line, const regexPattern* const patbuf,
		const regmatch_t* const pmatch)
{
	regexMatch matches [BACK_REFERENCE_COUNT];
	unsigned int count = 0;
	int i;
	for (i = 0  ;  i < BACK_REFERENCE_COUNT  ;  ++i)
	{
		matches [i].start  = pmatch [i].rm_so;
		matches [i].length = pmatch [i].rm_eo - pmatch [i].rm_so;
		/* a valid match may have both offsets == -1,
		 * e.g. (foo)*(bar) matching "bar" - see CTags bug 271.
		 * As POSIX regex doesn't seem to have a way to count matches,
		 * we return the count up to the last non-empty match. */
		if (pmatch [i].rm_so != -1)
			count = i + 1;
	}
	patbuf->u.callback.function (vStringValue (line), matches, count,
				     patbuf->u.callback.userData);
}

static boolean matchRegexPattern (const vString* const line,
				  const regexPattern* const patbuf)
{
	boolean result = FALSE;
	regmatch_t pmatch [BACK_REFERENCE_COUNT];
	int match;

	if (patbuf->disabled && *(patbuf->disabled))
		return FALSE;

	match = regexec (patbuf->pattern, vStringValue (line),
			 BACK_REFERENCE_COUNT, pmatch, 0);
	if (match == 0)
	{
		result = TRUE;
		if (patbuf->type == PTRN_TAG)
			matchTagPattern (line, patbuf, pmatch);
		else if (patbuf->type == PTRN_CALLBACK)
			matchCallbackPattern (line, patbuf, pmatch);
		else
		{
			Assert ("invalid pattern type" == NULL);
			result = FALSE;
		}
	}
	return result;
}


/* PUBLIC INTERFACE */

/* Match against all patterns for specified language. Returns true if at least
 * on pattern matched.
 */
extern boolean matchRegex (const vString* const line, const langType language)
{
	boolean result = FALSE;
	if (language != LANG_IGNORE  &&  language <= SetUpper  &&
		Sets [language].count > 0)
	{
		const patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
		{
			regexPattern* ptrn = set->patterns + i;
			if (matchRegexPattern (line, ptrn))
			{
				result = TRUE;
				if (ptrn->exclusive)
					break;
			}
		}
	}
	return result;
}

extern void findRegexTagsMainloop (int (* driver)(void))
{
	currentScope = CORK_NIL;
	/* merely read all lines of the file */
	while (driver () != EOF)
		;
}

static int fileReadLineDriver(void)
{
	return (readLineFromInputFile () == NULL)? EOF: 1;
}

extern void findRegexTags (void)
{
	findRegexTagsMainloop (fileReadLineDriver);
}

extern boolean hasScopeActionInRegex (const langType language)
{
	boolean r = FALSE;
	unsigned int i;

	if (language <= SetUpper  &&  Sets [language].count > 0)
		for (i = 0; i < Sets [language].count; i++)
			if (Sets[language].patterns[i].scopeActions)
				r= TRUE;

	return r;
}

static regexPattern *addTagRegexInternal (const langType language,
					  const char* const regex,
					  const char* const name,
					  const char* const kinds,
					  const char* const flags,
					  boolean *disabled)
{
	regexPattern *rptr = NULL;
	Assert (regex != NULL);
	Assert (name != NULL);
	if (regexAvailable)
	{
		regex_t* const cp = compileRegex (regex, flags);
		if (cp != NULL)
		{
			char kind;
			char* kindName;
			char* description;

			parseKinds (kinds, &kind, &kindName, &description);
			if (kind == getLanguageFileKind (language)->letter)
				error (FATAL,
				       "Kind letter \'%c\' used in regex definition \"%s\" of %s language is reserved in ctags main",
				       kind,
				       regex,
				       getLanguageName (language));

			rptr = addCompiledTagPattern (language, cp, name,
						      kind, kindName, description, flags,
						      disabled);
			if (kindName)
				eFree (kindName);
			if (description)
				eFree (description);
		}
	}

	if (*name == '\0')
	{
		if (rptr->exclusive || rptr->scopeActions & SCOPE_PLACEHOLDER)
			rptr->accept_empty_name = TRUE;
		else
			error (WARNING, "%s: regexp missing name pattern", regex);
	}

	return rptr;
}

extern void addTagRegex (const langType language CTAGS_ATTR_UNUSED,
			 const char* const regex CTAGS_ATTR_UNUSED,
			 const char* const name CTAGS_ATTR_UNUSED,
			 const char* const kinds CTAGS_ATTR_UNUSED,
			 const char* const flags CTAGS_ATTR_UNUSED,
			 boolean *disabled)
{
	addTagRegexInternal (language, regex, name, kinds, flags, disabled);
}

extern void addCallbackRegex (const langType language CTAGS_ATTR_UNUSED,
			      const char* const regex CTAGS_ATTR_UNUSED,
			      const char* const flags CTAGS_ATTR_UNUSED,
			      const regexCallback callback CTAGS_ATTR_UNUSED,
			      boolean *disabled,
			      void * userData)
{
	Assert (regex != NULL);
	if (regexAvailable)
	{
		regex_t* const cp = compileRegex (regex, flags);
		if (cp != NULL)
			addCompiledCallbackPattern (language, cp, callback, flags,
						    disabled, userData);
	}
}

extern void addLanguageRegex (
		const langType language CTAGS_ATTR_UNUSED, const char* const regex CTAGS_ATTR_UNUSED)
{
	if (regexAvailable)
	{
		char *const regex_pat = eStrdup (regex);
		char *name, *kinds, *flags;
		if (parseTagRegex (regex_pat, &name, &kinds, &flags))
		{
			addTagRegexInternal (language, regex_pat, name, kinds, flags,
					     NULL);
			eFree (regex_pat);
		}
	}
}

/*
*   Regex option parsing
*/

extern boolean processRegexOption (const char *const option,
				   const char *const parameter CTAGS_ATTR_UNUSED)
{
	langType language;

	language = getLanguageComponentInOption (option, "regex-");
	if (language == LANG_IGNORE)
		return FALSE;

	processLanguageRegex (language, parameter);

	return TRUE;
}

extern void foreachRegexKinds (const langType language,
			       boolean (*func) (kindOption *, void *),
			       void *data)
{
	initializeParser (language);
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
			if ((set->patterns [i].type == PTRN_TAG)
			    && (func (set->patterns [i].u.tag.kind, data)))
				break;
	}
}


static boolean kind_reset_cb (kindOption *kind, void *data)
{
	kind->enabled = *(boolean *)data;
	return FALSE;		/* continue */
}

extern void resetRegexKinds (const langType language, boolean mode)
{
	foreachRegexKinds (language, kind_reset_cb, &mode);
}

struct kind_and_mode_and_result
{
	int kind;
	const char *kindLong;
	boolean mode;
	boolean result;
};

static boolean enable_kind_cb (kindOption *kind, void *data)
{
	struct kind_and_mode_and_result *kmr = data;
	if ((kmr->kind != KIND_NULL
	     && kind->letter == kmr->kind)
	    || (kmr->kindLong && kind->name
		&& (strcmp (kmr->kindLong, kind->name) == 0)))
	{
		kind->enabled = kmr->mode;
		kmr->result = TRUE;
	}
	/* continue:
	   There can be more than one patterns which represents this kind. */
	return FALSE;
}

extern boolean enableRegexKind (const langType language, const int kind, const boolean mode)
{
	struct kind_and_mode_and_result kmr;

	kmr.kind = kind;
	kmr.kindLong = NULL;
	kmr.mode = mode;
	kmr.result = FALSE;

	foreachRegexKinds (language, enable_kind_cb, &kmr);
	return kmr.result;
}

extern boolean enableRegexKindLong (const langType language, const char *kindLong, const boolean mode)
{
	struct kind_and_mode_and_result kmr;

	kmr.kind = KIND_NULL;
	kmr.kindLong = kindLong;
	kmr.mode = mode;
	kmr.result = FALSE;

	foreachRegexKinds (language, enable_kind_cb, &kmr);
	return kmr.result;
}

struct kind_and_result
{
	int kind;
	boolean result;
};

static boolean is_kind_enabled_cb (kindOption *kind, void *data)
{
	boolean r = FALSE;
	struct kind_and_result *kr = data;

	if (kind->letter == kr->kind)
	{
		kr->result = kind->enabled;
		r = TRUE;
	}

	return r;
}

static boolean does_kind_exist_cb (kindOption *kind, void *data)
{
	boolean r = FALSE;
	struct kind_and_result *kr = data;

	if (kind->letter == kr->kind)
	{
		kr->result = TRUE;
		r = TRUE;
	}

	return r;
}

extern boolean isRegexKindEnabled (const langType language, const int kind)
{
	struct kind_and_result d;

	d.kind = kind;
	d.result = FALSE;

	foreachRegexKinds (language, is_kind_enabled_cb, &d);

	return d.result;
}

extern boolean hasRegexKind (const langType language, const int kind)
{
	struct kind_and_result d;

	d.kind = kind;
	d.result = FALSE;

	foreachRegexKinds (language, does_kind_exist_cb, &d);

	return d.result;
}

struct printRegexKindCBData{
	const char* const langName;
	boolean allKindFields;
	boolean indent;
	boolean tabSeparated;
};

static boolean printRegexKind (kindOption *kind, void *user_data)
{
	struct printRegexKindCBData *data = user_data;
	if (kind->letter != KIND_GHOST)
	{
		if (data->allKindFields && data->indent)
			printf (Option.machinable? "%s": PR_KIND_FMT (LANG,s), data->langName);
		printKind (kind, data->allKindFields, data->indent,
			   data->tabSeparated);
	}
	return FALSE;
}

extern void printRegexKinds (const langType language,
			     boolean allKindFields,
			     boolean indent,
			     boolean tabSeparated)
{
	const char* const langName = getLanguageName (language);
	struct printRegexKindCBData data = {
		.langName      = langName,
		.allKindFields = allKindFields,
		.indent        = indent,
		.tabSeparated  = tabSeparated,
	};
	foreachRegexKinds (language, printRegexKind, &data);
}

extern void printRegexFlags (void)
{
	flagPrintHelp (regexFlagDefs,  ARRAY_SIZE (regexFlagDefs));
	flagPrintHelp (prePtrnFlagDef, ARRAY_SIZE (prePtrnFlagDef));
	flagPrintHelp (scopePtrnFlagDef, ARRAY_SIZE (scopePtrnFlagDef));
}

extern void freeRegexResources (void)
{
	int i;
	for (i = 0  ;  i <= SetUpper  ;  ++i)
		clearPatternSet (i);
	if (Sets != NULL)
		eFree (Sets);
	Sets = NULL;
	SetUpper = -1;
}

/* Return TRUE if available. */
extern boolean checkRegex (void)
{
#if defined (CHECK_REGCOMP)
	{
		/* Check for broken regcomp() on Cygwin */
		regex_t patbuf;
		int errcode;
		if (regcomp (&patbuf, "/hello/", 0) != 0)
			error (WARNING, "Disabling broken regex");
		else
			regexAvailable = TRUE;
	}
#else
	/* We are using bundled regex engine. */
	regexAvailable = TRUE;
#endif
	return regexAvailable;
}
