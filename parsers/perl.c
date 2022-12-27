/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for PERL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "perl.h"
#include "promise.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "subparser.h"
#include "trace.h"
#include "vstring.h"
#include "xtag.h"

/*
*   DATA DEFINITIONS
*/
typedef enum PerlKindType perlKind;
typedef enum PerlModuleRoleType perlModuleRole;

static roleDefinition PerlModuleRoles [] = {
	{ true, "used",   "specified in `use' built-in function" },
	{ true, "unused", "specified in `no' built-in function" },
};

typedef enum {
	R_HEREDOC_ENDLABEL,
} perlHeredocRole;

static roleDefinition PerlHeredocRoles [] = {
	{ true, "endmarker", "end marker" },
};

static kindDefinition PerlKinds [] = {
	{ true,  'c', "constant",               "constants" },
	{ true,  'f', "format",                 "formats" },
	{ true,  'l', "label",                  "labels" },
	{ true,  'p', "package",                "packages" },
	{ true,  's', "subroutine",             "subroutines" },
	{ false, 'd', "subroutineDeclaration",  "subroutine declarations" },
	{ false, 'M', "module",                 "modules",
	  .referenceOnly = true,  ATTACH_ROLES(PerlModuleRoles)},
	{ false, 'h', "heredoc", "marker for here document",
	  .referenceOnly = false, ATTACH_ROLES (PerlHeredocRoles) },
};

struct hereDocMarker {
	vString *marker;
	bool indented;
	int corkIndex;
};

struct hereDocMarkerManager {
	ptrArray *markers;
	size_t current;
};

/*
*   FUNCTION DEFINITIONS
*/

static void notifyEnteringPod ()
{
	subparser *sub;

	foreachSubparser (sub, false)
	{
		perlSubparser *perlsub = (perlSubparser *)sub;
		if (perlsub->enteringPodNotify)
		{
			enterSubparser (sub);
			perlsub->enteringPodNotify (perlsub);
			leaveSubparser ();
		}
	}
}

static void notifyLeavingPod ()
{
	subparser *sub;

	foreachSubparser (sub, false)
	{
		perlSubparser *perlsub = (perlSubparser *)sub;
		if (perlsub->leavingPodNotify)
		{
			enterSubparser (sub);
			perlsub->leavingPodNotify (perlsub);
			leaveSubparser ();
		}
	}
}

static void notifyFindingQuotedWord (int moduleIndex,
									 const char *qwd)
{
	subparser *sub;

	foreachSubparser (sub, false)
	{
		perlSubparser *perlsub = (perlSubparser *)sub;
		if (perlsub->findingQuotedWordNotify)
		{
			enterSubparser (sub);
			perlsub->findingQuotedWordNotify (perlsub,
											  moduleIndex,
											  qwd);
			leaveSubparser ();
		}
	}
}

static bool isIdentifier1 (int c)
{
	return (bool) (isalpha (c) || c == '_');
}

static bool isIdentifier (int c)
{
	return (bool) (isalnum (c) || c == '_');
}

static bool isPodWord (const char *word)
{
	/* Perl POD words are three to eight characters in size.  We use this
	 * fact to find (or not find) the right side of the word and then
	 * perform comparisons, if necessary, of POD words of that size.
	 */
	size_t len;
	for (len = 0; len < 9; ++len)
		if ('\0' == word[len] || ' ' == word[len] || '\t' == word[len])
			break;
	switch (len) {
		case 3:
			return 0 == strncmp(word, "end", 3)
				|| 0 == strncmp(word, "for", 3)
				|| 0 == strncmp(word, "pod", 3);
		case 4:
			return 0 == strncmp(word, "back", 4)
				|| 0 == strncmp(word, "item", 4)
				|| 0 == strncmp(word, "over", 4);
		case 5:
			return 0 == strncmp(word, "begin", 5)
				|| 0 == strncmp(word, "head1", 5)
				|| 0 == strncmp(word, "head2", 5)
				|| 0 == strncmp(word, "head3", 5)
				|| 0 == strncmp(word, "head4", 5);
		case 8:
			return 0 == strncmp(word, "encoding", 8);
		default:
			return false;
	}
}

/*
 * Perl subroutine declaration may look like one of the following:
 *
 *  sub abc;
 *  sub abc :attr;
 *  sub abc (proto);
 *  sub abc (proto) :attr;
 *
 * Note that there may be more than one attribute.  Attributes may
 * have things in parentheses (they look like arguments).  Anything
 * inside of those parentheses goes.  Prototypes may contain semi-colons.
 * The matching end when we encounter (outside of any parentheses) either
 * a semi-colon (that'd be a declaration) or an left curly brace
 * (definition).
 *
 * This is pretty complicated parsing (plus we all know that only perl can
 * parse Perl), so we are only promising best effort here.
 *
 * If we can't determine what this is (due to a file ending, for example),
 * we will return false.
 */
static bool isSubroutineDeclaration (const unsigned char *cp)
{
	bool attr = false;
	int nparens = 0;

	do {
		for ( ; *cp; ++cp) {
SUB_DECL_SWITCH:
			switch (*cp) {
				case ':':
					if (nparens)
						break;
					else if (true == attr)
						return false;    /* Invalid attribute name */
					else
						attr = true;
					break;
				case '(':
					++nparens;
					break;
				case ')':
					--nparens;
					break;
				case ' ':
				case '\t':
					break;
				case ';':
					if (!nparens)
						return true;
				case '{':
					if (!nparens)
						return false;
				default:
					if (attr) {
						if (isIdentifier1(*cp)) {
							cp++;
							while (isIdentifier (*cp))
								cp++;
							attr = false;
							goto SUB_DECL_SWITCH; /* Instead of --cp; */
						} else {
							return false;
						}
					} else if (nparens) {
						break;
					} else {
						return false;
					}
			}
		}
	} while (NULL != (cp = readLineFromInputFile ()));

	return false;
}

/* `end' points to the equal sign.  Parse from right to left to get the
 * identifier.  Assume we're dealing with something of form \s*\w+\s*=>
 */
static void makeTagFromLeftSide (const char *begin, const char *end,
	vString *name, vString *package)
{
	tagEntryInfo entry;
	const char *b, *e;
	if (! PerlKinds[KIND_PERL_CONSTANT].enabled)
		return;
	for (e = end - 1; e > begin && isspace(*e); --e)
		;
	if (e < begin)
		return;
	for (b = e; b >= begin && isIdentifier(*b); --b)
		;
	/* Identifier must be either beginning of line of have some whitespace
	 * on its left:
	 */
	if (b < begin || isspace(*b) || ',' == *b)
		++b;
	else if (b != begin)
		return;
	if (e - b + 1 <= 0)
		return;			/* Left side of => has an invalid identifier. */
	vStringClear(name);
	vStringNCatS(name, b, e - b + 1);
	initTagEntry(&entry, vStringValue(name), KIND_PERL_CONSTANT);
	makeTagEntry(&entry);
	if (isXtagEnabled (XTAG_QUALIFIED_TAGS) && package && vStringLength(package)) {
		vStringClear(name);
		vStringCopy(name, package);
		vStringNCatS(name, b, e - b + 1);
		initTagEntry(&entry, vStringValue(name), KIND_PERL_CONSTANT);
		markTagExtraBit (&entry, XTAG_QUALIFIED_TAGS);
		makeTagEntry(&entry);
	}
}

static int makeTagForModule (const char *name, int role)
{
	tagEntryInfo entry;
	initRefTagEntry(&entry, name, KIND_PERL_MODULE, role);
	return makeTagEntry(&entry);
}

enum const_state { CONST_STATE_NEXT_LINE, CONST_STATE_HIT_END };

/* Parse a single line, find as many NAME => VALUE pairs as we can and try
 * to detect the end of the hashref.
 */
static enum const_state parseConstantsFromLine (const char *cp,
	vString *name, vString *package)
{
	while (1) {
		const size_t sz = strcspn(cp, "#}=");
		switch (cp[sz]) {
			case '=':
				if ('>' == cp[sz + 1])
					makeTagFromLeftSide(cp, cp + sz, name, package);
				break;
			case '}':	/* Assume this is the end of the hashref. */
				return CONST_STATE_HIT_END;
			case '\0':	/* End of the line. */
			case '#':	/* Assume this is a comment and thus end of the line. */
				return CONST_STATE_NEXT_LINE;
		}
		cp += sz + 1;
	}
}

/* Parse constants declared via hash reference, like this:
 * use constant {
 *   A => 1,
 *   B => 2,
 * };
 * The approach we take is simplistic, but it covers the vast majority of
 * cases well.  There can be some false positives.
 * Returns 0 if found the end of the hashref, -1 if we hit EOF
 */
static int parseConstantsFromHashRef (const unsigned char *cp,
	vString *name, vString *package)
{
	while (1) {
		enum const_state state =
			parseConstantsFromLine((const char *) cp, name, package);
		switch (state) {
			case CONST_STATE_NEXT_LINE:
				cp = readLineFromInputFile();
				if (cp)
					break;
				else
					return -1;
			case CONST_STATE_HIT_END:
				return 0;
		}
	}
}

static void parseQuotedWords(const unsigned char *cp,
							 vString *name, int moduleIndex)
{
	unsigned char end = *cp++;
	switch (end)
	{
	case '[': end = ']'; break;
	case '(': end = ')'; break;
	case '{': end = '}'; break;
	case '<': end = '>'; break;
	}

	do {
		while (*cp && *cp != end)
		{
			if (isspace(*cp))
			{
				notifyFindingQuotedWord (moduleIndex, vStringValue(name));
				vStringClear(name);
				cp++;
				continue;
			}

			if (*cp == '\\')
			{
				cp++;
				if (*cp == '\0')
					break;
			}

			vStringPut(name, *cp);
			cp++;
		}
		if (!vStringIsEmpty(name))
			notifyFindingQuotedWord (moduleIndex, vStringValue(name));

		if (*cp == end)
			break;
	} while ((cp = readLineFromInputFile()) != NULL);
}

/*
 * Extract heredoc markers and skip the heredoc areas.
 *
 * - https://perldoc.perl.org/perlop#%3C%3CEOF
 */
static struct hereDocMarker *hereDocMarkerNew (bool indented)
{
	struct hereDocMarker *marker = xMalloc(1, struct hereDocMarker);

	marker->indented = indented;
	marker->marker = vStringNew();
	marker->corkIndex = CORK_NIL;

	return marker;
}

static void hereDocMarkerDelete (struct hereDocMarker *marker)
{
	vStringDelete (marker->marker);
	eFree (marker);
}

static unsigned char *readHereDocMarker (unsigned char *line,
										 vString *marker,
										 unsigned char quote_char)
{
	unsigned char *cp = line;
	bool backslash = false;

	for (cp = line; *cp != '\0'; cp++)
	{
		if (backslash)
		{
			vStringPut (marker, *cp);
			backslash = false;
			continue;
		}

		if (quote_char == '"' && (*cp == '\\'))
		{
			backslash = true;
			continue;
		}

		if (quote_char && *cp == quote_char)
		{
			cp++;
			break;
		}

		if (!quote_char && !isIdentifier(*cp))
			break;

		vStringPut (marker, *cp);
	}

	return cp;
}

enum stringType {
	STRING_TYPE_NONE = '\0',
	STRING_TYPE_SINGLEQ = '\'',
	STRING_TYPE_DOUBLEQ = '"',
	STRING_TYPE_BACKQ = '`',
};


static const unsigned char *escapeFromString (const unsigned char *line,
											  const unsigned char *end,
											  enum stringType stype)
{
	bool in_escape = false;
	const unsigned char *cp = line;

	switch (stype)
	{
	case STRING_TYPE_NONE:
		return line;
	case STRING_TYPE_SINGLEQ:
	case STRING_TYPE_DOUBLEQ:
	case STRING_TYPE_BACKQ:
		while ((end && cp < end) || (end == NULL && *cp != '\0'))
		{
			if (in_escape)
			{
				cp++;
				in_escape = false;
			}
			else if (*cp == '\\')
			{
				cp++;
				in_escape = true;
			}
			else if (*cp == (unsigned char)stype)
			{
				cp++;
				return cp;
			}
			else
				cp++;
		}
		return NULL;
	default:
		AssertNotReached ();
		return NULL;
	}
}

static enum stringType isInString (const unsigned char *line,
								   const unsigned char *end)
{
	const unsigned char *cp = line;
	enum stringType t = STRING_TYPE_NONE;

	while (cp && cp < end)
	{
		switch (*cp)
		{
		case '\'':
		case '\"':
		case '`':
			t = *cp;
			break;
		default:
			t = STRING_TYPE_NONE;
			break;
		}

		cp++;
		if (t != STRING_TYPE_NONE)
			cp = escapeFromString (cp, end, t);
	}

	return (cp == NULL)? t: STRING_TYPE_NONE;
}


static const unsigned char *collectHereDocMarker (struct hereDocMarkerManager *mgr,
												  const unsigned char *line)
{
	unsigned char *starter = (unsigned char*)strstr((char *)line, "<<");
	unsigned char *cp = NULL;
	bool indented = false;
	unsigned char quote_char = 0;
	bool space_seen = false;

	if (starter == NULL)
		return NULL;

	enum stringType stype;
	if ((stype = isInString(line, starter)) != STRING_TYPE_NONE)
		return escapeFromString (starter + 2, NULL, stype);

	cp = starter + 2;
	while (isspace (*cp))
	{
		/* To avoid confusing with a shift operator, we track
		 * spaces after the starter (<<). */
		space_seen = true;
		cp++;
	}

	if (*cp == '\0')
		return NULL;

	/* Is shift operator? */
	if (isdigit (*cp))
		/* Scan the rest of the string. */
		return cp + 1;

	if (*cp == '~') {
		if (space_seen)
			return cp + 1;
		indented = true;
		cp++;
		if (*cp == '\0')
			return NULL;
		while (isspace (*cp))
			cp++;
		if (*cp == '\0')
			return NULL;
	}

	switch (*cp)
	{
	case '\'':
	case '"':
	case '`':
		quote_char = *cp;
		/* Fall through */
	case '\\':
		cp++;
		if (*cp == '\0')
			return NULL;
		break;
	default:
		if (space_seen)
			return cp;
		break;
	}

	struct hereDocMarker *marker = hereDocMarkerNew (indented);
	const unsigned char *last_cp = cp;
	cp = readHereDocMarker(cp, marker->marker, quote_char);
	if (vStringLength (marker->marker) > 0)
	{
		marker->corkIndex = makeSimpleTag (marker->marker,
										   KIND_PERL_HEREDOCMARKER);
		ptrArrayAdd (mgr->markers, marker);
	}
	else
		hereDocMarkerDelete (marker);

	if (*cp != '\0' && cp != last_cp)
		return cp;
	return NULL;
}

static void collectHereDocMarkers (struct hereDocMarkerManager *mgr,
								   const unsigned char *line)
{
	const unsigned char *cp = line;
	const unsigned char *last = cp;
	while ((cp = collectHereDocMarker(mgr, cp)) != NULL)
		Assert(last < cp);
}

static bool isInHereDoc (struct hereDocMarkerManager *mgr,
						 const unsigned char *line)
{
	if (ptrArrayCount (mgr->markers) == 0)
		return false;

	const unsigned char *cp = line;
	struct hereDocMarker *current = ptrArrayItem (mgr->markers, mgr->current);
	if (current->indented)
	{
		while (isspace(*cp))
			cp++;
	}
	if (strncmp((const char *)cp, vStringValue (current->marker), vStringLength (current->marker)) == 0
		&& (cp [vStringLength (current->marker)] == '\0'
			|| (!isIdentifier (cp [vStringLength (current->marker)]))))
	{
		tagEntryInfo *tag = getEntryInCorkQueue (current->corkIndex);
		if (tag)
			tag->extensionFields.endLine = getInputLineNumber();
		mgr->current++;
		if (mgr->current == ptrArrayCount (mgr->markers))
		{
			ptrArrayClear (mgr->markers);
			mgr->current = 0;
		}
	}
	return true;
}

static void initHereDocMarkerManager(struct hereDocMarkerManager *mgr)
{
	mgr->markers = ptrArrayNew((ptrArrayDeleteFunc)hereDocMarkerDelete);
	mgr->current = 0;
}

static void finiHereDocMarkerManager(struct hereDocMarkerManager *mgr)
{
	ptrArrayDelete (mgr->markers);
	mgr->markers = NULL;
	mgr->current = 0;
}

/* Algorithm adapted from from GNU etags.
 * Perl support by Bart Robinson <lomew@cs.utah.edu>
 * Perl sub names: look for /^ [ \t\n]sub [ \t\n]+ [^ \t\n{ (]+/
 */
static void findPerlTags (void)
{
	vString *name = vStringNew ();
	vString *package = NULL;
	bool skipPodDoc = false;
	const unsigned char *line;
	unsigned long podStart = 0UL;

	/* A pod area can be after __END__ marker.
	 * Perl parser itself doesn't need to parse the area
	 * after the marker. Parsing the area is needed only
	 * if Perl parser runs Pod parser as a guest.
	 * This variable is set true when it is needed.
	 */
	bool parse_only_pod_area = false;

	/* Core modules AutoLoader and SelfLoader support delayed compilation
	 * by allowing Perl code that follows __END__ and __DATA__ tokens,
	 * respectively.  When we detect that one of these modules is used
	 * in the file, we continue processing even after we see the
	 * corresponding token that would usually terminate parsing of the
	 * file.
	 */
	enum {
		RESPECT_END		= (1 << 0),
		RESPECT_DATA	= (1 << 1),
	} respect_token = RESPECT_END | RESPECT_DATA;

	struct hereDocMarkerManager hdoc_mgr;
	initHereDocMarkerManager (&hdoc_mgr);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		bool spaceRequired = false;
		bool qualified = false;
		const unsigned char *cp = line;
		perlKind kind = KIND_PERL_NONE;
		tagEntryInfo e;

		if (isInHereDoc (&hdoc_mgr, line))
			continue;

		if (skipPodDoc)
		{
			if (strncmp ((const char*) line, "=cut", (size_t) 4) == 0)
			{
				skipPodDoc = false;
				if (podStart != 0UL)
				{
					notifyLeavingPod ();
					makePromise ("Pod",
						     podStart, 0,
						     getInputLineNumber(), 0,
						     getSourceLineNumber());
					podStart = 0UL;
				}
			}
			continue;
		}
		else if (line [0] == '=')
		{
			skipPodDoc = isPodWord ((const char*)line + 1);
			if (skipPodDoc)
			{
				podStart = getSourceLineNumber ();
				notifyEnteringPod ();
			}
			continue;
		}
		else if (strcmp ((const char*) line, "__DATA__") == 0)
		{
			if (respect_token & RESPECT_DATA)
			{
				if (isXtagEnabled (XTAG_GUEST))
					parse_only_pod_area = true;
				else
					break;
			}
			else
				continue;
		}
		else if (strcmp ((const char*) line, "__END__") == 0)
		{
			if (respect_token & RESPECT_END)
			{
				if (isXtagEnabled (XTAG_GUEST))
					parse_only_pod_area = true;
				else
					break;
			}
			else
				continue;
		}
		else if (line [0] == '#')
			continue;

		if (parse_only_pod_area)
			continue;

		while (isspace (*cp))
			cp++;

		if (strncmp((const char*) cp, "sub", (size_t) 3) == 0)
		{
			TRACE_PRINT("this looks like a sub");
			cp += 3;
			kind = KIND_PERL_SUBROUTINE;
			spaceRequired = true;
			qualified = true;
		}
		else if (strncmp((const char*) cp, "use", (size_t) 3) == 0)
		{
			cp += 3;
			if (!isspace(*cp))
				continue;
			while (*cp && isspace (*cp))
				++cp;
			if (strncmp((const char*) cp, "AutoLoader", (size_t) 10) == 0) {
				respect_token &= ~RESPECT_END;
				makeTagForModule("AutoLoader", ROLE_PERL_MODULE_USED);
				continue;
			}
			if (strncmp((const char*) cp, "SelfLoader", (size_t) 10) == 0) {
				respect_token &= ~RESPECT_DATA;
				makeTagForModule("SelfLoader", ROLE_PERL_MODULE_USED);
				continue;
			}

			vString *module = NULL;
			while (isalnum(*cp) || *cp == ':' || *cp == '.' || *cp == '_') {
				if (!module)
					module = vStringNew();
				vStringPut(module, *cp);
				++cp;
			}
			if (!module)
				continue;

			int q = makeTagForModule(vStringValue(module), ROLE_PERL_MODULE_USED);
			bool isConstant = (strcmp(vStringValue(module), "constant") == 0);
			vStringDelete(module);
			if (!isConstant)
			{
				while (isspace(*cp))
					cp++;
				if (strncmp("qw", (const char *)cp, 2) != 0)
					continue;
				cp += 2;
				while (isspace(*cp))
					cp++;
				if (*cp == '\0')
					continue;
				vStringClear (name);

				parseQuotedWords(cp, name, q);
				vStringClear (name);
				continue;
			}

			/* Skip up to the first non-space character, skipping empty
			 * and comment lines.
			 */
			while (isspace(*cp))
				cp++;
			while (!*cp || '#' == *cp) {
				cp = readLineFromInputFile ();
				if (!cp)
					goto END_MAIN_WHILE;
				while (isspace (*cp))
					cp++;
			}
			if ('{' == *cp) {
				++cp;
				if (0 == parseConstantsFromHashRef(cp, name, package)) {
					vStringClear(name);
					continue;
				} else
					goto END_MAIN_WHILE;
			}
			kind = KIND_PERL_CONSTANT;
			spaceRequired = false;
			qualified = true;
		}
		else if (strncmp((const char*) cp, "no", (size_t) 2) == 0 && isspace(cp[2]))
		{
			cp += 3;
			while (isspace (*cp))
				cp++;
			vString *module = NULL;
			while (isalnum(*cp) || *cp == ':' || *cp == '.' || *cp == '_') {
				if (!module)
					module = vStringNew();
				vStringPut(module, *cp);
				++cp;
			}
			if (module) {
				makeTagForModule(vStringValue(module), ROLE_PERL_MODULE_UNUSED);
				vStringDelete(module);
			}
			continue;
		}
		else if (strncmp((const char*) cp, "package", (size_t) 7) == 0 &&
				 ('\0' == cp[7] || isspace(cp[7])))
		{
			cp += 7;
			while (isspace (*cp))
				cp++;
			while (!*cp || '#' == *cp) {
				cp = readLineFromInputFile ();
				if (!cp)
					goto END_MAIN_WHILE;
				while (isspace (*cp))
					cp++;
			}
			if (package == NULL)
				package = vStringNew ();
			else
				vStringClear (package);
			const unsigned char *const first = cp;
			while (*cp && (int) *cp != ';'  &&  !isspace ((int) *cp))
			{
				vStringPut (package, (int) *cp);
				cp++;
			}
			vStringCatS (package, "::");

			cp = first;	 /* Rewind */
			kind = KIND_PERL_PACKAGE;
			spaceRequired = false;
			qualified = true;
		}
		else if (strncmp((const char*) cp, "format", (size_t) 6) == 0)
		{
			cp += 6;
			kind = KIND_PERL_FORMAT;
			spaceRequired = true;
			qualified = true;
		}
		else
		{
			if (isIdentifier1 (*cp))
			{
				const unsigned char *p = cp;
				while (isIdentifier (*p))
					++p;
				while (isspace (*p))
					++p;
				if ((int) *p == ':' && (int) *(p + 1) != ':')
					kind = KIND_PERL_LABEL;
			}
			if (kind != KIND_PERL_LABEL)
				collectHereDocMarkers (&hdoc_mgr, cp);
		}
		if (kind != KIND_PERL_NONE)
		{
			TRACE_PRINT("cp0: %s", (const char *) cp);
			if (spaceRequired && *cp && !isspace (*cp))
				continue;

			TRACE_PRINT("cp1: %s", (const char *) cp);
			while (isspace (*cp))
				cp++;

			while (!*cp || '#' == *cp) { /* Gobble up empty lines
				                            and comments */
				cp = readLineFromInputFile ();
				if (!cp)
					goto END_MAIN_WHILE;
				while (isspace (*cp))
					cp++;
			}

			while (isIdentifier (*cp) || (KIND_PERL_PACKAGE == kind && ':' == *cp))
			{
				vStringPut (name, (int) *cp);
				cp++;
			}

			if (KIND_PERL_FORMAT == kind &&
				vStringLength (name) == 0 && /* cp did not advance */
				'=' == *cp)
			{
				/* format's name is optional.  If it's omitted, 'STDOUT'
				   is assumed. */
				vStringCatS (name, "STDOUT");
			}

			TRACE_PRINT("name: %s", vStringValue (name));

			if (0 == vStringLength(name)) {
				vStringClear(name);
				continue;
			}

			if (KIND_PERL_SUBROUTINE == kind)
			{
				/*
				 * isSubroutineDeclaration() may consume several lines.  So
				 * we record line positions.
				 */
				initTagEntry(&e, vStringValue(name), KIND_GHOST_INDEX);

				if (true == isSubroutineDeclaration(cp)) {
					if (true == PerlKinds[KIND_PERL_SUBROUTINE_DECLARATION].enabled) {
						kind = KIND_PERL_SUBROUTINE_DECLARATION;
					} else {
						vStringClear (name);
						continue;
					}
				} else if (! PerlKinds[kind].enabled) {
					continue;
				}

				e.kindIndex = kind;

				makeTagEntry(&e);

				if (isXtagEnabled (XTAG_QUALIFIED_TAGS) && qualified &&
					package != NULL  && vStringLength (package) > 0)
				{
					vString *const qualifiedName = vStringNew ();
					vStringCopy (qualifiedName, package);
					vStringCat (qualifiedName, name);
					e.name = vStringValue(qualifiedName);
					markTagExtraBit (&e, XTAG_QUALIFIED_TAGS);
					makeTagEntry(&e);
					vStringDelete (qualifiedName);
				}
			} else if (vStringLength (name) > 0)
			{
				makeSimpleTag (name, kind);
				if (isXtagEnabled(XTAG_QUALIFIED_TAGS) && qualified &&
					KIND_PERL_PACKAGE != kind &&
					package != NULL  && vStringLength (package) > 0)
				{
					tagEntryInfo fqe;
					vString *const qualifiedName = vStringNew ();
					vStringCopy (qualifiedName, package);
					vStringCat (qualifiedName, name);
					initTagEntry (&fqe, vStringValue (qualifiedName), kind);
					markTagExtraBit (&fqe, XTAG_QUALIFIED_TAGS);
					makeTagEntry (&fqe);
					vStringDelete (qualifiedName);
				}
			}
			vStringClear (name);
		}
	}

END_MAIN_WHILE:
	vStringDelete (name);
	finiHereDocMarkerManager (&hdoc_mgr);
	if (package != NULL)
		vStringDelete (package);
}

extern parserDefinition* PerlParser (void)
{
	static const char *const extensions [] = { "pl", "pm", "ph", "plx", "perl", NULL };
	static const char *const aliases [] = {
		/* cperl is an Emacs' editing mode for Perl source code  */
		"cperl",
		NULL };
	static selectLanguage selectors [] = { selectByPickingPerlVersion,
					       NULL };
	parserDefinition* def = parserNew ("Perl");
	def->kindTable      = PerlKinds;
	def->kindCount  = ARRAY_SIZE (PerlKinds);
	def->extensions = extensions;
	def->parser     = findPerlTags;
	def->selectLanguage = selectors;
	def->aliases    = aliases;

	/* Subparsers need this */
	def->useCork = CORK_QUEUE;

	return def;
}
