/*
 *
 *  Copyright (c) 2015, Red Hat, Inc.
 *  Copyright (c) 2015, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"  /* must always come first */

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "ctags.h"
#include "debug.h"
#include "entry.h"
#include "field.h"
#include "kind.h"
#include "options.h"
#include "read.h"
#include "routines.h"


typedef const char* (* renderEscaped) (const tagEntryInfo *const tag,
				       const char *value,
				       vString * buffer);

typedef struct sFieldSpec fieldSpec;
struct sFieldSpec {
	unsigned char letter;
	const char* name;
	const char* description;
	boolean enabled;
	renderEscaped renderEscaped;

	unsigned int ftype;	/* Given from the main part */
};

struct sFieldDesc {
	fieldSpec *spec;
	unsigned int fixed:   1;   /* fields which cannot be disabled. */
	vString     *buffer;
	const char* nameWithPrefix;
};

static const char *renderFieldName (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldInput (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldCompactInputLine (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldSignature (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldScope (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldTyperef (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldInherits (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldKindName (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldLineNumber (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldLanguage (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldAccess (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldKindLetter (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldImplementation (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldFile (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldPattern (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldRole (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldRefMarker (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldExtra (const tagEntryInfo *const tag, const char *value, vString* b);

#define DEFINE_FIELD_SPEC(L, N, V, H, F)	\
	{					\
		.letter        = L,		\
		.name          = N,		\
		.description   = H,		\
		.enabled       = V,		\
		.renderEscaped = F,		\
	}

#define WITH_DEFUALT_VALUE(str) ((str)?(str):"-")

static fieldSpec fieldSpecsFixed [] = {
        /* FIXED FIELDS */
	DEFINE_FIELD_SPEC ('N', "name",     TRUE,
			  "tag name (fixed field)",
			  renderFieldName),
	DEFINE_FIELD_SPEC ('F', "input",    TRUE,
			   "input file (fixed field)",
			   renderFieldInput),
	DEFINE_FIELD_SPEC ('P', "pattern",  TRUE,
			   "pattern (fixed field)",
			   renderFieldPattern),
};

static fieldSpec fieldSpecsExuberant [] = {
	DEFINE_FIELD_SPEC ('C', "compact",        FALSE,
			   "compact input line (fixed field, only used in -x option)",
			   renderFieldCompactInputLine),

	/* EXTENSION FIELDS */
	DEFINE_FIELD_SPEC ('a', "access",         FALSE,
		      "Access (or export) of class members",
		      renderFieldAccess),
	DEFINE_FIELD_SPEC ('f', "file",           TRUE,
		      "File-restricted scoping",
		      renderFieldFile),
	DEFINE_FIELD_SPEC ('i', "inherits",       FALSE,
		      "Inheritance information",
		      renderFieldInherits),
	DEFINE_FIELD_SPEC ('K', NULL,             FALSE,
		      "Kind of tag as full name",
		      renderFieldKindName),
	DEFINE_FIELD_SPEC ('k', NULL,             TRUE,
			   "Kind of tag as a single letter",
			   renderFieldKindLetter),
	DEFINE_FIELD_SPEC ('l', "language",       FALSE,
			   "Language of input file containing tag",
			   renderFieldLanguage),
	DEFINE_FIELD_SPEC ('m', "implementation", FALSE,
			   "Implementation information",
			   renderFieldImplementation),
	DEFINE_FIELD_SPEC ('n', "line",           FALSE,
			   "Line number of tag definition",
			   renderFieldLineNumber),
	DEFINE_FIELD_SPEC ('S', "signature",	     FALSE,
			   "Signature of routine (e.g. prototype or parameter list)",
			   renderFieldSignature),
	DEFINE_FIELD_SPEC ('s', NULL,             TRUE,
			   "Scope of tag definition (WARNING: this doesn't work well as a format letter)",
			   renderFieldScope),
	DEFINE_FIELD_SPEC ('t', "typeref",        TRUE,
			   "Type and name of a variable or typedef",
			   renderFieldTyperef),
	DEFINE_FIELD_SPEC ('z', "kind",           FALSE,
			   "Include the \"kind:\" key in kind field (use k or K)",
			   NULL),
};

static fieldSpec fieldSpecsUniversal [] = {
	DEFINE_FIELD_SPEC ('r', "role",    FALSE,
			   "Role",
			   renderFieldRole),
	DEFINE_FIELD_SPEC ('R',  NULL,     FALSE,
			   "Marker (R or D) representing whether tag is definition or reference",
			   renderFieldRefMarker),
	DEFINE_FIELD_SPEC ('Z', "scope",   FALSE,
			  "Include the \"scope:\" key in scope field (use s)",
		      NULL),
	DEFINE_FIELD_SPEC ('E', "extra",   FALSE,
			   "Extra tag type information",
			   renderFieldExtra),
};


static unsigned int       fieldDescUsed = 0;
static unsigned int       fieldDescAllocated = 0;
static fieldDesc* fieldDescs = NULL;

extern void initFieldDescs (void)
{
	int i;
	fieldDesc *fdesc;

	Assert (fieldDescs == NULL);

	fieldDescAllocated
	  = ARRAY_SIZE (fieldSpecsFixed)
	  + ARRAY_SIZE (fieldSpecsExuberant)
	  + ARRAY_SIZE (fieldSpecsUniversal);
	fieldDescs = xMalloc (fieldDescAllocated, fieldDesc);

	fieldDescUsed = 0;

	for (i = 0; i < ARRAY_SIZE (fieldSpecsFixed); i++)
	{
		fdesc = fieldDescs + i + fieldDescUsed;
		fdesc->spec   = fieldSpecsFixed + i;
		fdesc->fixed  = 1;
		fdesc->buffer = NULL;
		fdesc->nameWithPrefix = fdesc->spec->name;
	}
	fieldDescUsed += ARRAY_SIZE (fieldSpecsFixed);

	for (i = 0; i < ARRAY_SIZE (fieldSpecsExuberant); i++)
	{
		fdesc = fieldDescs + i + fieldDescUsed;
		fdesc->spec = fieldSpecsExuberant +i;
		fdesc->fixed = 0;
		fdesc->buffer = NULL;
		fdesc->nameWithPrefix = fdesc->spec->name;
	}
	fieldDescUsed += ARRAY_SIZE (fieldSpecsExuberant);

	for (i = 0; i < ARRAY_SIZE (fieldSpecsUniversal); i++)
	{
		char *nameWithPrefix;

		fdesc = fieldDescs + i + fieldDescUsed;
		fdesc->spec = fieldSpecsUniversal + i;
		fdesc->fixed = 0;
		fdesc->buffer = NULL;

		if (fdesc->spec->name)
		{
			nameWithPrefix = eMalloc (sizeof CTAGS_FIELD_PREFIX + strlen (fdesc->spec->name) + 1);
			nameWithPrefix [0] = '\0';
			strcat (nameWithPrefix, CTAGS_FIELD_PREFIX);
			strcat (nameWithPrefix, fdesc->spec->name);
			fdesc->nameWithPrefix = nameWithPrefix;
		}
		else
			fdesc->nameWithPrefix = NULL;
	}
	fieldDescUsed += ARRAY_SIZE (fieldSpecsUniversal);

	Assert ( fieldDescAllocated == fieldDescUsed );
}

static fieldDesc* getFieldDesc(fieldType type)
{
	Assert ((0 <= type) && (type < fieldDescUsed));
	return fieldDescs + type;
}

extern fieldType getFieldTypeForOption (char letter)
{
	int i;

	for (i = 0; i < fieldDescUsed; i++)
	{
		if (fieldDescs [i].spec->letter == letter)
			return i;
	}
	return FIELD_UNKNOWN;
}

extern const char* getFieldName(fieldType type)
{
	fieldDesc* fdesc;

	fdesc = getFieldDesc (type);
	if (Option.putFieldPrefix)
		return fdesc->nameWithPrefix;
	else
		return fdesc->spec->name;
}

#define PR_FIELD_WIDTH_LETTER     7
#define PR_FIELD_WIDTH_NAME      15
#define PR_FIELD_WIDTH_DESC      30
#define PR_FIELD_WIDTH_XFMTCHAR  8
#define PR_FIELD_WIDTH_ENABLED   7

#define PR_FIELD_STR(X) PR_FIELD_WIDTH_##X
#define PR_FIELD_FMT(X,T) "%-" STRINGIFY(PR_FIELD_STR(X)) STRINGIFY(T)

#define MAKE_FIELD_FMT(LETTER_SPEC)		\
	PR_FIELD_FMT (LETTER,LETTER_SPEC)	\
	" "					\
	PR_FIELD_FMT (NAME,s)			\
	" "					\
	PR_FIELD_FMT (ENABLED,s)		\
	" "					\
	PR_FIELD_FMT (XFMTCHAR,s)		\
	" "					\
	PR_FIELD_FMT (DESC,s)			\
	"\n"

static void printField (fieldType i)
{
	printf((Option.machinable? "%c\t%s\t%s\t%s\t%s\n": MAKE_FIELD_FMT(c)),
	       fieldDescs[i].spec->letter,
	       (fieldDescs[i].spec->name? getFieldName (i): "NONE"),
	       isFieldEnabled (i)? "on": "off",
	       getFieldDesc (i)->spec->renderEscaped? "TRUE": "FALSE",
	       fieldDescs[i].spec->description? fieldDescs[i].spec->description: "NONE");
}

extern void printFields (void)
{
	unsigned int i;

	if (Option.withListHeader)
		printf ((Option.machinable? "%s\t%s\t%s\t%s\t%s\n": MAKE_FIELD_FMT(s)),
			"#LETTER", "NAME", "ENABLED", "XFMTCHAR", "DESCRIPTION");

	for (i = 0; i < fieldDescUsed; i++)
		printField (i);
}

static const char *renderAsIs (vString* b __unused__, const char *s)
{
	return s;
}

static const char *renderEscapedString (const char *s,
					const tagEntryInfo *const tag __unused__,
					vString* b)
{
	vStringCatSWithEscaping (b, s);
	return vStringValue (b);
}

static const char *renderEscapedName (const char* s,
				      const tagEntryInfo *const tag,
				      vString* b)
{
	const char* base = s;

	for (; *s; s++)
	{
		int c = *s;
		if ((c > 0x00 && c <= 0x1F) || c == 0x7F)
		{
			verbose ("Unexpected character (0 < *c && *c < 0x20) included in a tagEntryInfo: %s\n", base);
			verbose ("File: %s, Line: %lu, Lang: %s, Kind: %c\n",
				 tag->inputFileName, tag->lineNumber, tag->language, tag->kind->letter);
			verbose ("Escape the character\n");
			break;
		}
		else if (c == '\\')
			break;
		else
			continue;
	}

	if (*s == '\0')
		return base;

	vStringNCatS (b, base, s - base);

	return renderEscapedString (s, tag, b);
}

static const char *renderFieldName (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	return renderEscapedName (tag->name, tag, b);
}

static const char *renderFieldInput (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	const char *f = tag->inputFileName;

	if (Option.lineDirectives && tag->sourceFileName)
		f = tag->sourceFileName;
	return renderEscapedString (f, tag, b);
}

static const char *renderFieldSignature (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	return renderEscapedString (WITH_DEFUALT_VALUE (tag->extensionFields.signature),
				    tag, b);
}

static const char *renderFieldScope (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	return renderEscapedName (WITH_DEFUALT_VALUE(tag->extensionFields.scopeName), tag, b);
}

static const char *renderFieldInherits (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	return renderEscapedString (WITH_DEFUALT_VALUE (tag->extensionFields.inheritance),
				    tag, b);
}

static const char *renderFieldTyperef (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	return renderEscapedName (WITH_DEFUALT_VALUE (tag->extensionFields.typeRef [1]), tag, b);
}


extern const char* renderFieldEscaped (fieldType type,
				       const tagEntryInfo *tag,
				       int index)
{
	fieldDesc *fdesc = fieldDescs + type;
	const char *value;

	Assert (tag);
	Assert (fdesc->spec->renderEscaped);

	if (fdesc->buffer == NULL)
		fdesc->buffer = vStringNew ();
	else
		vStringClear (fdesc->buffer);

	if (index >= 0)
	{
		Assert ( tag->usedParserFields > index );
		value = tag->parserFields[ index ].value;
	}
	else
		value = NULL;

	return fdesc->spec->renderEscaped (tag, value, fdesc->buffer);
}

/*  Writes "line", stripping leading and duplicate white space.
 */
static const char* renderCompactInputLine (vString *b,  const char *const line)
{
	boolean lineStarted = FALSE;
	const char *p;
	int c;

	/*  Write everything up to, but not including, the newline.
	 */
	for (p = line, c = *p  ;  c != NEWLINE  &&  c != '\0'  ;  c = *++p)
	{
		if (lineStarted  || ! isspace (c))  /* ignore leading spaces */
		{
			lineStarted = TRUE;
			if (isspace (c))
			{
				int next;

				/*  Consume repeating white space.
				 */
				while (next = *(p+1) , isspace (next)  &&  next != NEWLINE)
					++p;
				c = ' ';  /* force space character for any white space */
			}
			if (c != CRETURN  ||  *(p + 1) != NEWLINE)
				vStringPut (b, c);
		}
	}
	return vStringValue (b);
}

static const char *renderFieldKindName (const tagEntryInfo *const tag, const char *value __unused__, vString* b)
{
	return renderAsIs (b, tag->kind->name);
}

static const char *renderFieldCompactInputLine (const tagEntryInfo *const tag,
						const char *value __unused__,
						 vString* b)
{
	const char *line;
	static vString *tmp;

	if (tmp == NULL)
		tmp = vStringNew ();
	else
		vStringClear (tmp);

	line = readLineFromBypassAnyway (tmp, tag, NULL);
	if (line)
		renderCompactInputLine (b, line);
	else
	{
		/* If no associated line for tag is found, we cannot prepare
		 * parameter to writeCompactInputLine(). In this case we
		 * use an empty string as LINE.
		 */
		vStringClear (b);
	}

	return vStringValue (b);
}

static const char *renderFieldLineNumber (const tagEntryInfo *const tag,
					  const char *value __unused__,
					  vString* b)
{
	long ln = tag->lineNumber;
	char buf[32] = {[0] = '\0'};

	if (Option.lineDirectives && (tag->sourceLineNumberDifference != 0))
		ln += tag->sourceLineNumberDifference;
	snprintf (buf, sizeof(buf), "%ld", ln);
	vStringCatS (b, buf);
	return vStringValue (b);
}

static const char *renderFieldRole (const tagEntryInfo *const tag,
				    const char *value __unused__,
				    vString* b)
{
	int rindex = tag->extensionFields.roleIndex;
	const roleDesc * role;

	if (rindex == ROLE_INDEX_DEFINITION)
		vStringClear (b);
	else
	{
		Assert (rindex < tag->kind->nRoles);
		role  = & (tag->kind->roles [rindex]);
		return renderRole (role, b);
	}

	return vStringValue (b);
}

static const char *renderFieldLanguage (const tagEntryInfo *const tag,
					const char *value __unused__,
					vString* b)
{
	const char *l = tag->language;

	if (Option.lineDirectives && tag->sourceLanguage)
		l = tag->sourceLanguage;

	return renderAsIs (b, WITH_DEFUALT_VALUE(l));
}

static const char *renderFieldAccess (const tagEntryInfo *const tag,
				      const char *value,
				      vString* b)
{
	return renderAsIs (b, WITH_DEFUALT_VALUE (tag->extensionFields.access));
}

static const char *renderFieldKindLetter (const tagEntryInfo *const tag,
					  const char *value __unused__,
					  vString* b)
{
	static char c[2] = { [1] = '\0' };

	c [0] = tag->kind->letter;

	return renderAsIs (b, c);
}

static const char *renderFieldImplementation (const tagEntryInfo *const tag,
					      const char *value __unused__,
					      vString* b)
{
	return renderAsIs (b, WITH_DEFUALT_VALUE (tag->extensionFields.implementation));
}

static const char *renderFieldFile (const tagEntryInfo *const tag,
				    const char *value __unused__,
				    vString* b)
{
	return renderAsIs (b, tag->isFileScope? "file": "-");
}

static const char *renderFieldPattern (const tagEntryInfo *const tag,
				       const char *value __unused__,
				       vString* b)
{
	char* tmp = makePatternString (tag);
	vStringCatS (b, tmp);
	eFree (tmp);
	return vStringValue (b);
}

static const char *renderFieldRefMarker (const tagEntryInfo *const tag,
					 const char *value __unused__,
					 vString* b)
{
	static char c[2] = { [1] = '\0' };

	c [0] = tag->extensionFields.roleIndex == ROLE_INDEX_DEFINITION? 'D': 'R';

	return renderAsIs (b, c);
}

static const char *renderFieldExtra (const tagEntryInfo *const tag,
				     const char *value __unused__,
				     vString* b)
{
	int i;
	boolean hasExtra = FALSE;

	for (i = 0; i < XTAG_COUNT; i++)
	{
		const char *name = getXtagName (i);

		if (!name)
			continue;

		if (isTagExtraBitMarked (tag, i))
		{

			if (hasExtra)
				vStringPut (b, ',');
			vStringCatS (b, name);
			hasExtra = TRUE;
		}
	}

	if (hasExtra)
		return vStringValue (b);
	else
		return NULL;
}

extern boolean isFieldEnabled (fieldType type)
{
	return getFieldDesc(type)->spec->enabled? TRUE: FALSE;
}

extern boolean enableField (fieldType type, boolean state)
{
	boolean old = getFieldDesc(type)->spec->enabled? TRUE: FALSE;
	getFieldDesc(type)->spec->enabled = state;
	return old;
}

extern boolean isFieldFixed (fieldType type)
{
	return getFieldDesc(type)->fixed? TRUE: FALSE;
}

extern boolean isFieldRenderable (fieldType type)
{
	return getFieldDesc(type)->spec->renderEscaped? TRUE: FALSE;
}

extern int countFields (void)
{
	return fieldDescUsed;
}

/* vi:set tabstop=4 shiftwidth=4: */
