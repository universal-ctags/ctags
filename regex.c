/*
*   $Id$
*
*   Copyright (c) 2000-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utlizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#if defined (HAVE_REGCOMP) || defined (HAVE_RE_COMPILE_PATTERN)
# include <ctype.h>
# include <stddef.h>
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h> /* declare off_t (not known to regex.h on FreeBSD) */
# endif
# include "regex.h"
#endif

#include "debug.h"
#include "entry.h"
#include "main.h"
#include "parse.h"
#include "read.h"

#ifdef HAVE_REGEX

/*
*   MACROS
*/
#if defined (HAVE_REGCOMP) && !defined (REGCOMP_BROKEN)
# define POSIX_REGEX
# undef GNU_REGEX
#elif defined (HAVE_RE_COMPILE_PATTERN)
# define GNU_REGEX
#endif

#define REGEX_NAME "Regex"

/*
*   DATA DECLARATIONS
*/
#if defined (POSIX_REGEX) || defined (GNU_REGEX)

struct sKind {
    boolean enabled;
    char letter;
    char* name;
} kind;

typedef struct {
    regex_t *pattern;
    char *name_pattern;
    struct sKind kind;
#if defined (GNU_REGEX)
    struct re_registers regs;
    boolean error_signaled;
#endif
} regexPattern;

#endif

typedef struct {
    regexPattern *patterns;
    unsigned int count;
} patternSet;

/*
*   DATA DEFINITIONS
*/

static boolean regexBroken = FALSE;

/* Array of pattern sets, indexed by language */
static patternSet* Sets = NULL;
static int SetUpper = -1;	/* upper language index in list */

#ifdef GNU_REGEX
static char* CaseFold = NULL;
#endif

/*
*   FUNCTION DEFINITIONS
*/

static void clearPatternSet (const langType language)
{
    if (language < SetUpper)
    {
	patternSet* const set = Sets + language;
	unsigned int i;
	for (i = 0  ;  i < set->count  ;  ++i)
	{
#if defined (POSIX_REGEX) || defined (GNU_REGEX)
	    regfree (set->patterns [i].pattern);
#endif
	    eFree (set->patterns [i].pattern);
	    set->patterns [i].pattern = NULL;

	    eFree (set->patterns [i].name_pattern);
	    set->patterns [i].name_pattern = NULL;

	}
	if (set->patterns != NULL)
	    eFree (set->patterns);
	set->patterns = NULL;
	set->count = 0;
    }
}

/*
*   Regex psuedo-parser
*/

static void makeRegexTag (const vString* const name, struct sKind* const kind)
{
    if (kind->enabled)
    {
	tagEntryInfo e;
	Assert (name != NULL  &&  vStringLength (name) > 0);
	Assert (kind != NULL);
	initTagEntry (&e, vStringValue (name));
	e.kind     = kind->letter;
	e.kindName = kind->name;
	makeTagEntry (&e);
    }
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
static boolean parseRegex (char* const regexp, char** const name,
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
	if (**name == '\0')
	    error (WARNING, "%s: regexp missing name pattern", regexp);
	if ((*name) [strlen (*name) - 1] == '\\')
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

static void addCompiledPattern (const langType language,
				regex_t* const pattern, char* const name,
				const char kind, char* const kindName)
{
    patternSet* set;
    if (language > SetUpper)
    {
	int i;
	Sets = xRealloc (Sets, (language + 1), patternSet);
	for (i = SetUpper + 1  ;  i <= language  ;  ++i)
	{
	    Sets [i].patterns = NULL;
	    Sets [i].count = 0;
	}
	SetUpper = language;
    }
    set = Sets + language;
    set->patterns = xRealloc (set->patterns, (set->count + 1), regexPattern);
    set->patterns [set->count].pattern = pattern;
    set->patterns [set->count].name_pattern = name;
    set->patterns [set->count].kind.enabled = TRUE;
    set->patterns [set->count].kind.letter = kind;
    set->patterns [set->count].kind.name = kindName;
    set->count += 1;
}

#if defined (POSIX_REGEX)

static regex_t* compileRegex (char* const regexp, char** const pname,
			      char** const pkinds)
{
    int cflags = REG_EXTENDED | REG_NEWLINE;
    regex_t *patbuf = NULL;
    char *name, *kinds, *flags;

    if (parseRegex (regexp, &name, &kinds, &flags))
    {
	int errcode;
	for ( ; *flags != '\0' ; ++flags) switch ((int) *flags)
	{
	    case 'b': cflags &= ~REG_EXTENDED; break;
	    case 'e': cflags |= REG_EXTENDED;  break;
	    case 'i': cflags |= REG_ICASE;     break;
	    default: error (WARNING, "unknown regex flag: '%c'", *flags); break;
	}
	patbuf = xMalloc (1, regex_t);
	errcode = regcomp (patbuf, regexp, cflags);
	if (errcode == 0)
	{
	    *pname = eStrdup (name);
	    *pkinds = kinds;
	}
	else
	{
	    char errmsg[256];
	    regerror (errcode, patbuf, errmsg, 256);
	    error (WARNING, "%s", errmsg);
	    regfree (patbuf);
	    eFree (patbuf);
	    patbuf = NULL;
	}
    }
    return patbuf;
}

#elif defined (GNU_REGEX)

static char* caseFoldTable (void)
{
    if (CaseFold == NULL)
    {
	int i;
	CaseFold = xMalloc (257, char);
	for (i = 0  ;  i < 256  ;  ++i)
	{
	    if (isalpha (i))
		CaseFold [i] = toupper (i);
	    else
		CaseFold [i] = i;
	}
	CaseFold [i] = '\0';	/* make string for debugging */
    }
    return CaseFold;
}

static regex_t* compileRegex (char* const regexp, char** const pname,
			      char** const pkinds)
{
    regex_t *patbuf = NULL;
    char *name, *kinds, *flags;

    if (parseRegex (regexp, &namep, &kinds, &flags))
    {
	const char *err;

	re_syntax_options = RE_SYNTAX_POSIX_EXTENDED;

	patbuf = xMalloc (1, regex_t);
	patbuf->translate = NULL;
	patbuf->fastmap   = NULL;
	patbuf->buffer    = NULL;
	patbuf->allocated = 0;

	for ( ; *flags != '\0' ; ++flags) switch ((int) *flags)
	{
	    case 'b': re_syntax_options = RE_SYNTAX_POSIX_BASIC;    break;
	    case 'e': re_syntax_options = RE_SYNTAX_POSIX_EXTENDED; break;
	    case 'i': patbuf->translate = caseFoldTable ();         break;
	    default: error (WARNING, "unknown regex flag: '%c'", *flags); break;
	}
	err = re_compile_pattern (regexp, strlen (regexp), patbuf);
	if (err == NULL)
	{
	    *pname = eStrdup (name);
	    *pkinds = eStrdup (kinds);
	}
	else
	{
	    error (WARNING, "%s while compiling pattern", err);
	    xFree (patbuf);
	    patbuf = NULL;
	}
    }
    return patbuf;
}

#endif

static void parseKinds (const char* const kinds,
			char* const kind, char** const kindName)
{
    *kind = '\0';
    *kindName = NULL;
    if (kinds == NULL)
    {
	*kind = 'r';
	*kindName = eStrdup ("regex");
    }
    else if (kinds [0] != '\0')
    {
	const char* k = kinds;
	if (k [1] == ','  ||  k [1] == '\0')
	    *kind = *k++;
	if (*k == ',')
	    ++k;
	if (*k != '\0')
	    *kindName = eStrdup (k);
    }
}

static void printRegexKindOption (const regexPattern *pat, unsigned int i)
{
    printf ("          %c  %s (regex %d)%s\n",
	    pat [i].kind.letter != '\0' ? pat [i].kind.letter : '?',
	    pat [i].kind.name != NULL ? pat [i].kind.name : "Regex pattern",
	    i + 1, pat [i].kind.enabled ? "" : " [off]");
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
	FILE* const fp = fopen (regexfile, "r");
	if (fp == NULL)
	    error (WARNING | PERROR, regexfile);
	else
	{
	    vString* const regex = vStringNew ();
	    while (readLine (regex, fp))
		addLanguageRegex (language, vStringValue (regex));
	    fclose (fp);
	    vStringDelete (regex);
	}
    }
}

/*
*   Regex pattern matching
*/

#if defined (POSIX_REGEX)

static vString* substitute (const char* const in, const char* out,
			    const int nmatch, regmatch_t* const pmatch)
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

static void matchRegexPattern (const vString* const line,
			       regexPattern* const patbuf)
{
#   define SUBSTRING_COUNT 9
    regmatch_t pmatch [SUBSTRING_COUNT];
    const int match = regexec (patbuf->pattern, vStringValue (line),
			       SUBSTRING_COUNT, pmatch, 0);
    if (match == 0)
    {
	vString *const name = substitute (vStringValue (line),
		    patbuf->name_pattern, SUBSTRING_COUNT, pmatch);
	vStringStripLeading (name);
	vStringStripTrailing (name);
	if (vStringLength (name) == 0)
	    error (WARNING, "%s:%ld: null expansion of name pattern \"%s\"",
		   getInputFileName (), getInputLineNumber (),
		   patbuf->name_pattern);
	else
	{
	    makeRegexTag (name, &patbuf->kind);
	    vStringDelete (name);
	}
    }
}

#elif defined (GNU_REGEX)

static vString* substitute (const char* const in, const char* const out,
			    const struct re_registers* const regs)
{
    vString* result = vStringNew ();
    const char* p;
    for (p = out  ;  *p != '\0'  ;   p++)
    {
	if (*p == '\\'  &&  isdigit (*++p))
	{
	    const int dig = *p - '0';
	    const int diglen = regs->end [dig] - regs->start [dig];
	    vStringNCatS (result, in + regs->start [dig], diglen);
	}
	else
	    vStringPut (result, *p);
    }
    vStringTerminate (result);
    return result;
}

static void matchRegexPattern (const vString* const line,
			       regexPattern* const patbuf)
{
    const int match = re_match (
	patbuf->pattern, vStringValue (line),
	vStringLength (line), 0, &patbuf->regs);
    switch (match)
    {
	case -2: /* Some error. */
	    if (! patbuf->error_signaled)
	    {
		error (WARNING, "error while matching pattern \"%s\"",
		       patbuf->pattern);
		patbuf->error_signaled = TRUE;
	    }
	    break;

	case -1: /* No match. */
	    break;

	default: /* Match occurred.  Construct a tag. */
	    {
		vString *const name = substitute (vStringValue (line),
			    patbuf->name_pattern, &patbuf->regs);
		if (vStringLength (name) == 0)
		    error (WARNING,
			"%s:%ld: null expansion of name pattern \"%s\"",
			getInputFileName (), getInputLineNumber (),
			patbuf->name_pattern);
		else
		{
		    makeRegexTag (name, &patbuf->kind);
		    vStringDelete (name);
		}
	    }
	    break;
    }
}

#endif

/* Public interface */

/* Match against all patterns for specified language. */
extern void matchRegex (const vString* const line, const langType language)
{
    if (language != LANG_IGNORE  &&  language <= SetUpper  &&
	Sets [language].count > 0)
    {
	patternSet* const set = Sets + language;
	unsigned int i;
	for (i = 0  ;  i < set->count  ;  ++i)
	    matchRegexPattern (line, set->patterns + i);
    }
}

extern void findRegexTags (void)
{
    /* merely read all lines of the file */
    while (fileReadLine () != NULL)
	;
}

#endif /* HAVE_REGEX */

/*
*   Regex option parsing
*/

extern boolean processRegexOption (const char *const option,
				   const char *const __unused__ parameter)
{
    boolean handled = FALSE;
    const char* const dash = strchr (option, '-');
    if (dash != NULL  &&  strncmp (option, "regex", dash - option) == 0)
    {
#ifdef HAVE_REGEX
	langType language;
	language = getNamedLanguage (dash + 1);
	if (language == LANG_IGNORE)
	    error (WARNING, "unknown language in --%s option", option);
	else
	    processLanguageRegex (language, parameter);
#else
	error (WARNING, "regex support not available; required for --%s option",
	   option);
#endif
	handled = TRUE;
    }
    return handled;
}

extern void addLanguageRegex (const langType __unused__ language,
			      const char* const __unused__ regex)
{
#ifdef HAVE_REGEX
    if (! regexBroken)
    {
	char *const regex_pat = eStrdup (regex);
	char* name;
	char* kinds;
	regex_t* const cp = compileRegex (regex_pat, &name, &kinds);
	if (cp != NULL)
	{
	    char kind;
	    char* kindName;
	    parseKinds (kinds, &kind, &kindName);
	    addCompiledPattern (language, cp, name, kind, kindName);
	}
	eFree (regex_pat);
    }
#endif
}

extern void disableRegexKinds (const langType __unused__ language)
{
#ifdef HAVE_REGEX
    if (language <= SetUpper  &&  Sets [language].count > 0)
    {
	patternSet* const set = Sets + language;
	unsigned int i;
	for (i = 0  ;  i < set->count  ;  ++i)
	    set->patterns [i].kind.enabled = FALSE;
    }
#endif
}

extern boolean enableRegexKind (const langType __unused__ language,
				const int __unused__ kind,
				const boolean __unused__ mode)
{
    boolean result = FALSE;
#ifdef HAVE_REGEX
    if (language <= SetUpper  &&  Sets [language].count > 0)
    {
	patternSet* const set = Sets + language;
	unsigned int i;
	for (i = 0  ;  i < set->count  ;  ++i)
	    if (set->patterns [i].kind.letter == kind)
	    {
		set->patterns [i].kind.enabled = mode;
		result = TRUE;
	    }
    }
#endif
    return result;
}

extern void printRegexKindOptions (const langType __unused__ language)
{
#ifdef HAVE_REGEX
    if (language <= SetUpper  &&  Sets [language].count > 0)
    {
	patternSet* const set = Sets + language;
	unsigned int i;
	for (i = 0  ;  i < set->count  ;  ++i)
	    printRegexKindOption (set->patterns, i);
    }
#endif
}

extern void freeRegexResources (void)
{
#ifdef HAVE_REGEX
    int i;
    for (i = 0  ;  i <= SetUpper  ;  ++i)
	clearPatternSet (i);
    if (Sets != NULL)
	eFree (Sets);
    Sets = NULL;
    SetUpper = -1;
# ifdef GNU_REGEX
    if (CaseFold != NULL)
    {
	eFree (CaseFold);
	CaseFold = NULL;
    }
# endif
#endif
}

/* Check for broken regcomp() on Cygwin */
extern void checkRegex (void)
{
#if defined (HAVE_REGEX) && defined (CHECK_REGCOMP)
    regex_t patbuf;
    int errcode;
    if (regcomp (&patbuf, "/hello/", 0) != 0)
    {
	error (WARNING, "Disabling broken regex");
	regexBroken = TRUE;
    }
#endif
}

/* vi:set tabstop=8 shiftwidth=4: */
