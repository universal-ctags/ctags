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
#include "ptrarray.h"
#include "trashbox.h"

static bool regexAvailable = false;
static unsigned long currentScope = CORK_NIL;

/*
*   MACROS
*/

/* Back-references \0 through \9 */
#define BACK_REFERENCE_COUNT 10

/*
*   DATA DECLARATIONS
*/

enum pType { PTRN_TAG, PTRN_CALLBACK };

enum scopeAction {
	SCOPE_REF     = 1UL << 0,
	SCOPE_POP     = 1UL << 1,
	SCOPE_PUSH    = 1UL << 2,
	SCOPE_CLEAR   = 1UL << 3,
	SCOPE_PLACEHOLDER = 1UL << 4,
};

struct fieldPattern {
	fieldType ftype;
	const char *template;
};

typedef struct {
	regex_t *pattern;
	enum pType type;
	bool exclusive;
	bool accept_empty_name;
	union {
		struct {
			int kindIndex;
			char *name_pattern;
		} tag;
		struct {
			regexCallback function;
			void *userData;
		} callback;
	} u;
	unsigned int scopeActions;
	bool *disabled;
	int   multiline;
	int   xtagType;
	ptrArray *fieldPatterns;
} regexPattern;


struct lregexControlBlock {
	regexPattern *patterns;
	unsigned int count;
	unsigned int multilinePatternsCount;
	langType owner;
};

/*
*   DATA DEFINITIONS
*/

/*
*   FUNCTION DEFINITIONS
*/

static void clearPatternSet (struct lregexControlBlock *lcb)
{
	unsigned int i;
	for (i = 0  ;  i < lcb->count  ;  ++i)
	{
		regexPattern *p = &lcb->patterns [i];
		regfree (p->pattern);
		eFree (p->pattern);
		p->pattern = NULL;

		if (p->type == PTRN_TAG)
		{
			eFree (p->u.tag.name_pattern);
			p->u.tag.name_pattern = NULL;
		}

		if (p->fieldPatterns)
		{
			ptrArrayDelete (p->fieldPatterns);
			p->fieldPatterns = NULL;
		}
	}
	if (lcb->patterns != NULL)
		eFree (lcb->patterns);
	lcb->patterns = NULL;
	lcb->count = 0;
	lcb->multilinePatternsCount = 0;
}

extern struct lregexControlBlock* allocLregexControlBlock (parserDefinition *parser)
{
	struct lregexControlBlock *lcb = xCalloc (1, struct lregexControlBlock);
	lcb->owner = parser->id;
	return lcb;
}

extern void freeLregexControlBlock (struct lregexControlBlock* lcb)
{
	clearPatternSet (lcb);
	eFree (lcb);
}

/*
*   Regex pseudo-parser
*/

static bool initRegexTag (tagEntryInfo *e,
		const vString* const name, const kindDefinition* const kind, int scopeIndex, int placeholder,
		unsigned long line, MIOPos *pos, int xtag_type)
{
	Assert (kind != NULL);
	if (kind->enabled)
	{
		Assert (name != NULL  &&  ((vStringLength (name) > 0) || placeholder));
		initTagEntry (e, vStringValue (name), kind);
		e->extensionFields.scopeIndex = scopeIndex;
		e->placeholder = !!placeholder;
		if (line)
		{
			e->lineNumber = line;
			e->filePosition = *pos;
		}

		if (xtag_type != XTAG_UNKNOWN)
			markTagExtraBit (e, xtag_type);

		return true;
	}
	return false;
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
	bool quoted = false;

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
			quoted = false;
		}
		else if (*name == '\\')
			quoted = true;
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
static bool parseTagRegex (
		char* const regexp, char** const name,
		char** const kinds, char** const flags)
{
	bool result = false;
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
			result = true;
		}
	}
	return result;
}


static void pre_ptrn_flag_exclusive_short (char c CTAGS_ATTR_UNUSED, void* data)
{
	bool *exclusive = data;
	*exclusive = true;
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

static kindDefinition *kindNew (char letter, const char *name, const char *description)
{
	kindDefinition *kdef = xCalloc (1, kindDefinition);
	kdef->letter        = letter;
	kdef->name = eStrdup (name? name: KIND_REGEX_DEFAULT_LONG);
	kdef->description = eStrdup(description? description: kdef->name);
	kdef->enabled = true;
	return kdef;
}

static void kindFree (kindDefinition *kind)
{
	kind->letter = '\0';
	eFree ((void *)kind->name);
	kind->name = NULL;
	eFree ((void *)kind->description);
	kind->description = NULL;
	eFree (kind);
}

static regexPattern* addCompiledTagCommon (struct lregexControlBlock *lcb,
					   regex_t* const pattern)
{
	regexPattern *ptrn;

	lcb->patterns = xRealloc (lcb->patterns, (lcb->count + 1), regexPattern);

	ptrn = &lcb->patterns [lcb->count];
	memset (ptrn, 0, sizeof (*ptrn));
	ptrn->pattern = pattern;
	ptrn->exclusive = false;
	ptrn->accept_empty_name = false;
	lcb->count += 1;
	useRegexMethod(lcb->owner);
	return ptrn;
}

static void pre_ptrn_flag_multiline_long (const char* const s, const char* const v, void* data)
{
	if (!v)
	{
		error (WARNING, "no value is given for: %s", s);
		return;
	}
	if (!strToInt (v, 10, data))
	{
		error (WARNING, "wrong multiline specification: %s", v);
		*((int *)data) = -1;
	}
	else if (*((int *)data) < 0 || *((int *)data) >= BACK_REFERENCE_COUNT)
	{
		error (WARNING, "out of range(0 ~ %d) multiline specification: %s",
		       (BACK_REFERENCE_COUNT - 1), v);
		*((int *)data) = -1;
	}
}

static flagDefinition multilinePtrnFlagDef[] = {
#define EXPERIMENTAL "_"
	{ '\0',  EXPERIMENTAL "multiline", NULL, pre_ptrn_flag_multiline_long ,
	  "NGROUP", "match in multiline mode. cannot be combined with scope, placeholder, and exclusive"},
};

struct extraFlagData {
	int xtype;
	langType owner;
};

static void pre_ptrn_flag_extra_long (const char* const s CTAGS_ATTR_UNUSED, const char* const v, void* data)
{
	struct extraFlagData * xdata = data;

	if (!v)
	{
		error (WARNING, "no value is given for: %s", s);
		return;
	}

	xdata->xtype = getXtagTypeForNameAndLanguage (v, xdata->owner);
	if (xdata->xtype == XTAG_UNKNOWN)
		error (WARNING, "no such extra \"%s\" in %s", v, getLanguageName(xdata->owner));
}

static flagDefinition extraSpecFlagDef[] = {
#define EXPERIMENTAL "_"
	{ '-',  EXPERIMENTAL "extra", NULL, pre_ptrn_flag_extra_long ,
	  "EXTRA", "record the tag only when the extra is enabled"},
};

struct fieldFlagData {
	ptrArray *spec;
	langType owner;
};

static struct fieldPattern * fieldPatternNew (fieldType ftype, const char *template)
{
	struct fieldPattern *fp;

	fp = xMalloc(1, struct fieldPattern);
	fp->ftype = ftype;
	fp->template = eStrdup(template);

	return fp;
}

static void fieldPatternDelete (struct fieldPattern *fp)
{
	eFree ((void *)fp->template);
	eFree (fp);
}

static void pre_ptrn_flag_field_long (const char* const s CTAGS_ATTR_UNUSED, const char* const v, void* data)
{
	struct fieldFlagData *fdata = data;

	struct fieldPattern *fp;
	fieldType ftype;
	char *fname;
	const char* template;
	char *tmp;

	if (!v)
	{
		error (WARNING, "no value is given for: %s", s);
		return;
	}

	tmp = strchr (v, ':');
	if (tmp == NULL || tmp == v)
	{
		error (WARNING, "no field name is given for: %s", s);
		return;
	}

	fname = eStrndup (v, tmp - v);
	ftype = getFieldTypeForNameAndLanguage (fname, fdata->owner);
	if (ftype == FIELD_UNKNOWN)
	{
		error (WARNING, "no such field \"%s\" in %s", fname, getLanguageName(fdata->owner));
		eFree (fname);
		return;
	}

	if (fdata->spec)
	{
		for (unsigned int i = 0; i < ptrArrayCount(fdata->spec); i++)
		{
			fp = ptrArrayItem(fdata->spec, i);
			if (fp->ftype == ftype)
			{
				error (WARNING, "duplicated field specification \"%s\" in %s", fname, getLanguageName(fdata->owner));
				eFree (fname);
				return;
			}
		}
	}
	eFree (fname);

	template = tmp + 1;
	fp = fieldPatternNew (ftype, template);

	if (fdata->spec == NULL)
		fdata->spec = ptrArrayNew((ptrArrayDeleteFunc)fieldPatternDelete);
	ptrArrayAdd(fdata->spec, fp);
}

static flagDefinition fieldSpecFlagDef[] = {
#define EXPERIMENTAL "_"
	{ '\0',  EXPERIMENTAL "field", NULL, pre_ptrn_flag_field_long ,
	  "FIELD:VALUE", "record the matched string(VALUE) to FIELD of the tag"},
};


static regexPattern *addCompiledTagPattern (struct lregexControlBlock *lcb, regex_t* const pattern,
					    const char* const name, char kindLetter, const char* kindName,
					    char *const description, const char* flags,
					    bool *disabled)
{
	regexPattern * ptrn;
	bool exclusive = false;
	unsigned long scopeActions = 0UL;
	int multiline = -1;
	struct extraFlagData extraFlagData = {
		.xtype = XTAG_UNKNOWN,
		.owner = lcb->owner,
	};
	struct fieldFlagData fieldFlagData = {
		.spec  = NULL,
		.owner = lcb->owner,
	};

	flagsEval (flags, prePtrnFlagDef, ARRAY_SIZE(prePtrnFlagDef), &exclusive);
	flagsEval (flags, scopePtrnFlagDef, ARRAY_SIZE(scopePtrnFlagDef), &scopeActions);
	flagsEval (flags, multilinePtrnFlagDef, ARRAY_SIZE(multilinePtrnFlagDef), &multiline);
	flagsEval (flags, extraSpecFlagDef, ARRAY_SIZE(extraSpecFlagDef), &extraFlagData);

	ptrn  = addCompiledTagCommon(lcb, pattern);
	ptrn->type    = PTRN_TAG;
	ptrn->u.tag.name_pattern = eStrdup (name);
	ptrn->exclusive = exclusive;
	ptrn->scopeActions = scopeActions;
	ptrn->disabled = disabled;
	ptrn->multiline = multiline;
	if (multiline >= 0)
		lcb->multilinePatternsCount++;
	ptrn->xtagType = extraFlagData.xtype;

	flagsEval (flags, fieldSpecFlagDef, ARRAY_SIZE(fieldSpecFlagDef), &fieldFlagData);
	ptrn->fieldPatterns = fieldFlagData.spec;

	if (*name == '\0' && exclusive && kindLetter == KIND_REGEX_DEFAULT)
		ptrn->u.tag.kindIndex = KIND_GHOST_INDEX;
	else
	{
		kindDefinition *kdef;

		kdef = getLanguageKindForLetter (lcb->owner, kindLetter);
		if (kdef)
		{
			if (kindName && strcmp (kdef->name, kindName) && (strcmp(kindName, KIND_REGEX_DEFAULT_LONG)))
				/* When using a same kind letter for multiple regex patterns, the name of kind
				   should be the same. */
				error  (WARNING, "Don't reuse the kind letter `%c' in a language %s (old: \"%s\", new: \"%s\")",
						kdef->letter, getLanguageName (lcb->owner),
						kdef->name, kindName);
		}
		else
		{
			kdef = kindNew (kindLetter, kindName, description);
			defineLanguageKind (lcb->owner, kdef, kindFree);
		}

		ptrn->u.tag.kindIndex = kdef->id;
	}
	return ptrn;
}

static void addCompiledCallbackPattern (struct lregexControlBlock *lcb, regex_t* const pattern,
					const regexCallback callback, const char* flags,
					bool *disabled,
					void *userData)
{
	regexPattern * ptrn;
	bool exclusive = false;
	flagsEval (flags, prePtrnFlagDef, ARRAY_SIZE(prePtrnFlagDef), &exclusive);
	ptrn  = addCompiledTagCommon(lcb, pattern);
	ptrn->type    = PTRN_CALLBACK;
	ptrn->u.callback.function = callback;
	ptrn->u.callback.userData = userData;
	ptrn->exclusive = exclusive;
	ptrn->disabled = disabled;
	ptrn->multiline = -1;
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
	regex_t *result;
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

/*
*   Regex pattern matching
*/


static vString* substitute (
		const char* const in, const char* out,
		const int nmatch, const regmatch_t* const pmatch,
		bool allow_multiple)
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
		else if (allow_multiple || (*p != '\n'  &&  *p != '\r'))
			vStringPut (result, *p);
	}
	return result;
}

static void matchTagPattern (struct lregexControlBlock *lcb,
		const char* line,
		const regexPattern* const patbuf,
		const regmatch_t* const pmatch,
			     off_t offset)
{
	vString *const name = substitute (line,
			patbuf->u.tag.name_pattern, BACK_REFERENCE_COUNT, pmatch,
			!!(patbuf->multiline >= 0));
	bool placeholder = !!((patbuf->scopeActions & SCOPE_PLACEHOLDER) == SCOPE_PLACEHOLDER);
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

	if (vStringLength (name) == 0 && (placeholder == false))
	{
		if (patbuf->accept_empty_name == false)
			error (WARNING, "%s:%lu: null expansion of name pattern \"%s\"",
			       getInputFileName (),
			       (patbuf->multiline >= 0)
			       ? getInputLineNumberForFileOffset (offset)
			       : getInputLineNumber (),
			       patbuf->u.tag.name_pattern);
		n = CORK_NIL;
	}
	else
	{
		unsigned long ln = 0;
		MIOPos pos;
		kindDefinition *kdef;
		tagEntryInfo e;

		if (patbuf->multiline >= 0)
		{
			ln = getInputLineNumberForFileOffset (offset);
			pos = getInputFilePositionForLine (ln);
		}

		n = CORK_NIL;
		kdef = getLanguageKind (lcb->owner, patbuf->u.tag.kindIndex);

		if (initRegexTag (&e, name, kdef, scope, placeholder,
						  ln, ln == 0? NULL: &pos, patbuf->xtagType))
		{
			static TrashBox* field_trashbox;
			if (field_trashbox == NULL)
			{
				field_trashbox = trashBoxNew();
				DEFAULT_TRASH_BOX (field_trashbox, trashBoxDelete);
			}

			if (patbuf->fieldPatterns)
			{
				for (unsigned int i = 0; i < ptrArrayCount(patbuf->fieldPatterns); i++)
				{
					struct fieldPattern *fp = ptrArrayItem(patbuf->fieldPatterns, i);
					if (isFieldEnabled (fp->ftype))
					{
						vString * const value = substitute (line, fp->template,
															BACK_REFERENCE_COUNT, pmatch,
															!!(patbuf->multiline >= 0));
						attachParserField (&e, fp->ftype, vStringValue (value));
						trashBoxPut (field_trashbox, value,
									 (TrashBoxDestroyItemProc)vStringDelete);
					}
				}
			}
			n = makeTagEntry (&e);

			trashBoxMakeEmpty(field_trashbox);
		}
	}

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

static bool matchRegexPattern (struct lregexControlBlock *lcb,
				  const vString* const line,
				  const regexPattern* const patbuf)
{
	bool result = false;
	regmatch_t pmatch [BACK_REFERENCE_COUNT];
	int match;

	if (patbuf->disabled && *(patbuf->disabled))
		return false;

	match = regexec (patbuf->pattern, vStringValue (line),
			 BACK_REFERENCE_COUNT, pmatch, 0);
	if (match == 0)
	{
		result = true;
		if (patbuf->type == PTRN_TAG)
			matchTagPattern (lcb, vStringValue (line), patbuf, pmatch, 0);
		else if (patbuf->type == PTRN_CALLBACK)
			matchCallbackPattern (line, patbuf, pmatch);
		else
		{
			Assert ("invalid pattern type" == NULL);
			result = false;
		}
	}
	return result;
}

static bool matchMultilineRegexPattern (struct lregexControlBlock *lcb,
					const vString* const allLines,
					const regexPattern* const patbuf)
{
	const char *start;
	const char *current;

	bool result = false;
	regmatch_t pmatch [BACK_REFERENCE_COUNT];
	int match = 0;

	if (patbuf->disabled && *(patbuf->disabled))
		return false;

	start = vStringValue (allLines);
	for (current = start;
	     match == 0 && current < start + strlen(vStringValue (allLines));
	     current += pmatch [0].rm_eo)
	{
		match = regexec (patbuf->pattern, current,
				 BACK_REFERENCE_COUNT, pmatch, 0);
		if (match == 0)
		{
			if (patbuf->type == PTRN_TAG)
			{
				matchTagPattern (lcb, current, patbuf, pmatch,
						 (current + pmatch [patbuf->multiline].rm_eo) - start);
				result = true;
			}
			else if (patbuf->type == PTRN_CALLBACK)
				;	/* Not implemented yet */
			else
			{
				Assert ("invalid pattern type" == NULL);
				result = false;
				break;
			}
		}
	}
	return result;
}

/* PUBLIC INTERFACE */

/* Match against all patterns for specified language. Returns true if at least
 * on pattern matched.
 */
extern bool matchRegex (struct lregexControlBlock *lcb, const vString* const line)
{
	bool result = false;
	unsigned int i;
	for (i = 0  ;  i < lcb->count  ;  ++i)
	{
		regexPattern* ptrn = lcb->patterns + i;
		if (ptrn->multiline >= 0)
			continue;

		if ((ptrn->xtagType != XTAG_UNKNOWN)
			&& (!isXtagEnabled (ptrn->xtagType)))
				continue;

		if (matchRegexPattern (lcb, line, ptrn))
		{
			result = true;
			if (ptrn->exclusive)
				break;
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

extern bool hasScopeActionInRegex (struct lregexControlBlock *lcb)
{
	bool r = false;
	unsigned int i;

	for (i = 0; i < lcb->count; i++)
			if (lcb->patterns[i].scopeActions)
				r= true;

	return r;
}

static regexPattern *addTagRegexInternal (struct lregexControlBlock *lcb,
					  const char* const regex,
					  const char* const name,
					  const char* const kinds,
					  const char* const flags,
					  bool *disabled)
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
			if (kind == getLanguageKind (lcb->owner, KIND_FILE_INDEX)->letter)
				error (FATAL,
				       "Kind letter \'%c\' used in regex definition \"%s\" of %s language is reserved in ctags main",
				       kind,
				       regex,
				       getLanguageName (lcb->owner));

			rptr = addCompiledTagPattern (lcb, cp, name,
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
			rptr->accept_empty_name = true;
		else
			error (WARNING, "%s: regexp missing name pattern", regex);
	}

	return rptr;
}

extern void addTagRegex (struct lregexControlBlock *lcb,
			 const char* const regex,
			 const char* const name,
			 const char* const kinds,
			 const char* const flags,
			 bool *disabled)
{
	addTagRegexInternal (lcb, regex, name, kinds, flags, disabled);
}

extern void addCallbackRegex (struct lregexControlBlock *lcb,
			      const char* const regex,
			      const char* const flags,
			      const regexCallback callback,
			      bool *disabled,
			      void * userData)
{
	Assert (regex != NULL);
	if (regexAvailable)
	{
		regex_t* const cp = compileRegex (regex, flags);
		if (cp != NULL)
			addCompiledCallbackPattern (lcb, cp, callback, flags,
						    disabled, userData);
	}
}

static void addTagRegexOption (struct lregexControlBlock *lcb,
							   const char* const pattern)
{
	if (regexAvailable)
	{
		char *const regex_pat = eStrdup (pattern);
		char *name, *kinds, *flags;
		if (parseTagRegex (regex_pat, &name, &kinds, &flags))
			addTagRegexInternal (lcb, regex_pat, name, kinds, flags,
					     NULL);
		eFree (regex_pat);
	}
}

extern void processTagRegexOption (struct lregexControlBlock *lcb,
								   const char* const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		clearPatternSet (lcb);
	else if (parameter [0] != '@')
		addTagRegexOption (lcb, parameter);
	else if (! doesFileExist (parameter + 1))
		error (WARNING, "cannot open regex file");
	else
	{
		const char* regexfile = parameter + 1;

		verbose ("open a regex file: %s\n", regexfile);
		MIO* const mio = mio_new_file (regexfile, "r");
		if (mio == NULL)
			error (WARNING | PERROR, "%s", regexfile);
		else
		{
			vString* const regex = vStringNew ();
			while (readLineRaw (regex, mio))
			{
				if (vStringLength (regex) > 1 && vStringValue (regex)[0] != '\n')
					addTagRegexOption (lcb, vStringValue (regex));
			}
			mio_free (mio);
			vStringDelete (regex);
		}
	}
}

/*
*   Regex option parsing
*/

extern void printRegexFlags (void)
{
	flagPrintHelp (regexFlagDefs,  ARRAY_SIZE (regexFlagDefs));
	flagPrintHelp (prePtrnFlagDef, ARRAY_SIZE (prePtrnFlagDef));
	flagPrintHelp (scopePtrnFlagDef, ARRAY_SIZE (scopePtrnFlagDef));
	flagPrintHelp (multilinePtrnFlagDef, ARRAY_SIZE (multilinePtrnFlagDef));
	flagPrintHelp (extraSpecFlagDef, ARRAY_SIZE (extraSpecFlagDef));
	flagPrintHelp (fieldSpecFlagDef, ARRAY_SIZE (fieldSpecFlagDef));
}

extern void freeRegexResources (void)
{
	/* TODO: SHOULD BE REMOVED */
}

extern bool hasMultilineRegexPatterns (struct lregexControlBlock *lcb)
{
	return lcb->multilinePatternsCount;
}

extern bool matchMultilineRegex (struct lregexControlBlock *lcb, const vString* const allLines)
{
	bool result = false;

	if (lcb->count > 0)
	{
		unsigned int i;
		unsigned int multilinePatternsCount = lcb->multilinePatternsCount;

		for (i = 0; i < lcb->count && 0 < multilinePatternsCount; ++i)
		{
			regexPattern* ptrn = lcb->patterns + i;
			if (ptrn->multiline < 0)
				continue;

			multilinePatternsCount--;

			if ((ptrn->xtagType != XTAG_UNKNOWN)
				&& (!isXtagEnabled (ptrn->xtagType)))
				continue;

			result = matchMultilineRegexPattern (lcb, allLines, ptrn) || result;
		}
	}
	return false;
}


/* Return true if available. */
extern bool checkRegex (void)
{
#if defined (CHECK_REGCOMP)
	{
		/* Check for broken regcomp() on Cygwin */
		regex_t patbuf;
		int errcode;
		if (regcomp (&patbuf, "/hello/", 0) != 0)
			error (WARNING, "Disabling broken regex");
		else
			regexAvailable = true;
	}
#else
	/* We are using bundled regex engine. */
	regexAvailable = true;
#endif
	return regexAvailable;
}
