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


struct sFieldDesc {
	fieldSpec *spec;
	unsigned int fixed:   1;   /* fields which cannot be disabled. */
	vString     *buffer;
	const char* nameWithPrefix;
	langType language;
	fieldType sibling;
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
static const char *renderFieldXpath (const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldScopeKindName(const tagEntryInfo *const tag, const char *value, vString* b);
static const char *renderFieldEnd (const tagEntryInfo *const tag, const char *value, vString* b);

static boolean     isLanguageFieldAvailable  (const tagEntryInfo *const tag);
static boolean     isTyperefFieldAvailable   (const tagEntryInfo *const tag);
static boolean     isFileFieldAvailable      (const tagEntryInfo *const tag);
static boolean     isInheritsFieldAvailable  (const tagEntryInfo *const tag);
static boolean     isAccessFieldAvailable    (const tagEntryInfo *const tag);
static boolean     isImplementationFieldAvailable (const tagEntryInfo *const tag);
static boolean     isSignatureFieldAvailable (const tagEntryInfo *const tag);
static boolean     isRoleFieldAvailable      (const tagEntryInfo *const tag);
static boolean     isExtraFieldAvailable     (const tagEntryInfo *const tag);
static boolean     isXpathFieldAvailable     (const tagEntryInfo *const tag);
static boolean     isEndFieldAvailable       (const tagEntryInfo *const tag);


#define DEFINE_FIELD_SPEC(L, N, V, H, F)		\
	DEFINE_FIELD_SPEC_FULL (L, N, V, H, F, NULL)
#define DEFINE_FIELD_SPEC_FULL(L, N, V, H, F, A)\
	{					\
		.letter        = L,		\
		.name          = N,		\
		.description   = H,		\
		.enabled       = V,		\
		.renderEscaped = F,		\
		.isValueAvailable = A,		\
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
	DEFINE_FIELD_SPEC_FULL ('a', "access",         FALSE,
		      "Access (or export) of class members",
		      renderFieldAccess, isAccessFieldAvailable),
	DEFINE_FIELD_SPEC_FULL ('f', "file",           TRUE,
		      "File-restricted scoping",
		      renderFieldFile, isFileFieldAvailable),
	DEFINE_FIELD_SPEC_FULL ('i', "inherits",       FALSE,
		      "Inheritance information",
		      renderFieldInherits, isInheritsFieldAvailable),
	DEFINE_FIELD_SPEC ('K', NULL,             FALSE,
		      "Kind of tag as full name",
		      renderFieldKindName),
	DEFINE_FIELD_SPEC ('k', NULL,             TRUE,
			   "Kind of tag as a single letter",
			   renderFieldKindLetter),
	DEFINE_FIELD_SPEC_FULL ('l', "language",       FALSE,
			   "Language of input file containing tag",
			   renderFieldLanguage, isLanguageFieldAvailable),
	DEFINE_FIELD_SPEC_FULL ('m', "implementation", FALSE,
			   "Implementation information",
			   renderFieldImplementation, isImplementationFieldAvailable),
	DEFINE_FIELD_SPEC ('n', "line",           FALSE,
			   "Line number of tag definition",
			   renderFieldLineNumber),
	DEFINE_FIELD_SPEC_FULL ('S', "signature",	     FALSE,
			   "Signature of routine (e.g. prototype or parameter list)",
			   renderFieldSignature, isSignatureFieldAvailable),
	DEFINE_FIELD_SPEC ('s', NULL,             TRUE,
			   "Scope of tag definition (`p' can be used for printing its kind)",
			   renderFieldScope),
	DEFINE_FIELD_SPEC_FULL ('t', "typeref",        TRUE,
			   "Type and name of a variable or typedef",
			   renderFieldTyperef, isTyperefFieldAvailable),
	DEFINE_FIELD_SPEC ('z', "kind",           FALSE,
			   "Include the \"kind:\" key in kind field (use k or K) in tags output, kind full name in xref output",
			   /* Following renderer is for handling --_xformat=%{kind};
			      and is not for tags output. */
			   renderFieldKindName),
};

static fieldSpec fieldSpecsUniversal [] = {
	DEFINE_FIELD_SPEC_FULL ('r', "role",    FALSE,
			   "Role",
			   renderFieldRole, isRoleFieldAvailable),
	DEFINE_FIELD_SPEC ('R',  NULL,     FALSE,
			   "Marker (R or D) representing whether tag is definition or reference",
			   renderFieldRefMarker),
	DEFINE_FIELD_SPEC ('Z', "scope",   FALSE,
			  "Include the \"scope:\" key in scope field (use s) in tags output, scope name in xref output",
			   /* Following renderer is for handling --_xformat=%{scope};
			      and is not for tags output. */
			   renderFieldScope),
	DEFINE_FIELD_SPEC_FULL ('E', "extra",   FALSE,
			   "Extra tag type information",
			   renderFieldExtra, isExtraFieldAvailable),
	DEFINE_FIELD_SPEC_FULL ('x', "xpath",   FALSE,
			   "xpath for the tag",
			   renderFieldXpath, isXpathFieldAvailable),
	DEFINE_FIELD_SPEC ('p', "scopeKind", FALSE,
			   "Kind of scope as full name",
			   renderFieldScopeKindName),
	DEFINE_FIELD_SPEC_FULL ('e', "end", FALSE,
			   "end lines of various items",
			   renderFieldEnd, isEndFieldAvailable),
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
		fdesc->language = LANG_IGNORE;
		fdesc->sibling  = FIELD_UNKNOWN;
	}
	fieldDescUsed += ARRAY_SIZE (fieldSpecsFixed);

	for (i = 0; i < ARRAY_SIZE (fieldSpecsExuberant); i++)
	{
		fdesc = fieldDescs + i + fieldDescUsed;
		fdesc->spec = fieldSpecsExuberant +i;
		fdesc->fixed = 0;
		fdesc->buffer = NULL;
		fdesc->nameWithPrefix = fdesc->spec->name;
		fdesc->language = LANG_IGNORE;
		fdesc->sibling  = FIELD_UNKNOWN;
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
		fdesc->language = LANG_IGNORE;
		fdesc->sibling  = FIELD_UNKNOWN;
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
	unsigned int i;

	for (i = 0; i < fieldDescUsed; i++)
	{
		if (fieldDescs [i].spec->letter == letter)
			return i;
	}
	return FIELD_UNKNOWN;
}

extern fieldType getFieldTypeForName (const char *name)
{
	return getFieldTypeForNameAndLanguage (name, LANG_IGNORE);
}

extern fieldType getFieldTypeForNameAndLanguage (const char *fieldName, langType language)
{
	static boolean initialized = FALSE;
	unsigned int i;

	if (fieldName == NULL)
		return FIELD_UNKNOWN;

	if (language == LANG_AUTO && (initialized == FALSE))
	{
		initialized = TRUE;
		initializeParser (LANG_AUTO);
	}
	else if (language != LANG_IGNORE && (initialized == FALSE))
		initializeParser (language);

	for (i = 0; i < fieldDescUsed; i++)
	{
		if (fieldDescs [i].spec->name
		    && strcmp (fieldDescs [i].spec->name, fieldName) == 0
		    && ((language == LANG_AUTO)
			|| (fieldDescs [i].language == language)))
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

extern boolean doesFieldHaveValue (fieldType type, const tagEntryInfo *tag)
{
	if (getFieldDesc(type)->spec->isValueAvailable)
		return getFieldDesc(type)->spec->isValueAvailable(tag);
	else
		return TRUE;
}

#define PR_FIELD_WIDTH_LETTER     7
#define PR_FIELD_WIDTH_NAME      15
#define PR_FIELD_WIDTH_LANGUAGE  16
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
	PR_FIELD_FMT (LANGUAGE,s)		\
	" "					\
	PR_FIELD_FMT (XFMTCHAR,s)		\
	" "					\
	PR_FIELD_FMT (DESC,s)			\
	"\n"

static void printField (fieldType i)
{
	unsigned char letter = fieldDescs[i].spec->letter;
	const char *name;
	const char *language;

	if (letter == FIELD_LETTER_NO_USE)
		letter = '-';

	if (! fieldDescs[i].spec->name)
		name = "NONE";
	else
		name = getFieldName (i);

	if (fieldDescs[i].language == LANG_IGNORE)
		language = "NONE";
	else
		language = getLanguageName (fieldDescs[i].language);

	printf((Option.machinable? "%c\t%s\t%s\t%s\t%s\t%s\n": MAKE_FIELD_FMT(c)),
	       letter,
	       name,
	       isFieldEnabled (i)? "on": "off",
	       language,
	       getFieldDesc (i)->spec->renderEscaped? "TRUE": "FALSE",
	       fieldDescs[i].spec->description? fieldDescs[i].spec->description: "NONE");
}

extern void printFields (int language)
{
	unsigned int i;

	if (Option.withListHeader)
		printf ((Option.machinable? "%s\t%s\t%s\t%s\t%s\t%s\n": MAKE_FIELD_FMT(s)),
			"#LETTER", "NAME", "ENABLED", "LANGUAGE", "XFMTCHAR", "DESCRIPTION");

	for (i = 0; i < fieldDescUsed; i++)
	{
		if (language == LANG_AUTO || getFieldOwner (i) == language)
			printField (i);
	}
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
	const char* scope;

	getTagScopeInformation ((tagEntryInfo *const)tag, NULL, &scope);
	return scope? renderEscapedName (scope, tag, b): NULL;
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

	fdesc->buffer = vStringNewOrClear (fdesc->buffer);

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

	tmp = vStringNewOrClear (tmp);

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

static const char *renderFieldXpath (const tagEntryInfo *const tag,
				     const char *value,
				     vString* b)
{
#ifdef HAVE_LIBXML
	if (tag->extensionFields.xpath)
		return renderEscapedString (tag->extensionFields.xpath,
					    tag, b);
#endif
	return NULL;
}

static const char *renderFieldScopeKindName(const tagEntryInfo *const tag,
					    const char *value,
					    vString* b)
{
	const char* kind;

	getTagScopeInformation ((tagEntryInfo *const)tag, &kind, NULL);
	return kind? renderAsIs (b, kind): NULL;
}

static const char *renderFieldEnd (const tagEntryInfo *const tag,
				   const char *value,
				   vString* b)
{
	static char buf[16];

	if (tag->extensionFields.endLine != 0)
	{
		sprintf (buf, "%ld", tag->extensionFields.endLine);
		return renderAsIs (b, buf);
	}
	else
		return NULL;
}

static boolean     isLanguageFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->language != NULL)? TRUE: FALSE;
}

static boolean     isTyperefFieldAvailable  (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.typeRef [0] != NULL
		&& tag->extensionFields.typeRef [1] != NULL)? TRUE: FALSE;
}

static boolean     isFileFieldAvailable  (const tagEntryInfo *const tag)
{
	return tag->isFileScope? TRUE: FALSE;
}

static boolean     isInheritsFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.inheritance != NULL)? TRUE: FALSE;
}

static boolean     isAccessFieldAvailable   (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.access != NULL)? TRUE: FALSE;
}

static boolean     isImplementationFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.implementation != NULL)? TRUE: FALSE;
}

static boolean     isSignatureFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.signature != NULL)? TRUE: FALSE;
}

static boolean     isRoleFieldAvailable      (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.roleIndex != ROLE_INDEX_DEFINITION)? TRUE: FALSE;
}

static boolean     isExtraFieldAvailable     (const tagEntryInfo *const tag)
{
	int i;
	for (i = 0; i < sizeof (tag->extra); i++)
	{
		if (tag->extra [i])
			return TRUE;
	}

	return FALSE;
}

static boolean     isXpathFieldAvailable      (const tagEntryInfo *const tag)
{
#ifdef HAVE_LIBXML
	return (tag->extensionFields.xpath != NULL)? TRUE: FALSE;
#else
	return FALSE;
#endif
}

static boolean     isEndFieldAvailable       (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.endLine != 0)? TRUE: FALSE;
}

extern boolean isFieldEnabled (fieldType type)
{
	return getFieldDesc(type)->spec->enabled;
}

static boolean isFieldFixed (fieldType type)
{
	return getFieldDesc(type)->fixed? TRUE: FALSE;
}

extern boolean enableField (fieldType type, boolean state, boolean warnIfFixedField)
{
	fieldSpec *spec = getFieldDesc(type)->spec;
	boolean old = spec->enabled? TRUE: FALSE;
	if (isFieldFixed (type))
	{
		if ((!state) && warnIfFixedField)
		{
			if (spec->name && spec->letter != NUL_FIELD_LETTER)
				error(WARNING, "Cannot disable fixed field: '%c'{%s}",
				      spec->letter, spec->name);
			else if (spec->name)
				error(WARNING, "Cannot disable fixed field: {%s}",
				      spec->name);
			else if (spec->letter != NUL_FIELD_LETTER)
				error(WARNING, "Cannot disable fixed field: '%c'",
				      getFieldDesc(type)->spec->letter);
			else
				AssertNotReached();
		}
	}
	else
	{
		getFieldDesc(type)->spec->enabled = state;

		if (isCommonField (type))
			verbose ("enable field \"%s\": %s\n",
				 getFieldDesc(type)->spec->name,
				 (state? "TRUE": "FALSE"));
		else
			verbose ("enable field \"%s\"<%s>: %s\n",
				 getFieldDesc(type)->spec->name,
				 getLanguageName (getFieldOwner(type)),
				 (state? "TRUE": "FALSE"));
	}
	return old;
}

extern boolean isCommonField (fieldType type)
{
	return (FIELD_BUILTIN_LAST < type)? FALSE: TRUE;
}

extern int     getFieldOwner (fieldType type)
{
	return getFieldDesc(type)->language;
}

extern boolean isFieldRenderable (fieldType type)
{
	return getFieldDesc(type)->spec->renderEscaped? TRUE: FALSE;
}

extern int countFields (void)
{
	return fieldDescUsed;
}

extern fieldType nextSiblingField (fieldType type)
{
	fieldDesc *fdesc;

	fdesc = fieldDescs + type;
	return fdesc->sibling;
}

static void updateSiblingField (fieldType type, const char* name)
{
	int i;
	fieldDesc *fdesc;

	for (i = type; i > 0; i--)
	{
		fdesc = fieldDescs + i - 1;
		if (fdesc->spec->name && (strcmp (fdesc->spec->name, name) == 0))
		{
			Assert (fdesc->sibling == FIELD_UNKNOWN);
			fdesc->sibling = type;
			break;
		}
	}
}

static const char* defaultRenderer (const tagEntryInfo *const tag,
				    const char *value,
				    vString * buffer)
{
	return value;
}

extern int defineField (fieldSpec *spec, langType language)
{
	fieldDesc *fdesc;
	char *nameWithPrefix;
	size_t i;

	Assert (spec);
	Assert (spec->name);
	for (i = 0; i < strlen (spec->name); i++)
	{
		Assert ( isalnum (spec->name [i]) );
	}
	spec->letter = NUL_FIELD_LETTER;

	if (fieldDescUsed == fieldDescAllocated)
	{
		fieldDescAllocated *= 2;
		fieldDescs = xRealloc (fieldDescs, fieldDescAllocated, fieldDesc);
	}
	fdesc = fieldDescs + (fieldDescUsed);
	spec->ftype = fieldDescUsed++;

	if (spec->renderEscaped == NULL)
		spec->renderEscaped = defaultRenderer;

	fdesc->spec = spec;

	fdesc->fixed =  0;
	fdesc->buffer = NULL;

	nameWithPrefix = eMalloc (sizeof CTAGS_FIELD_PREFIX + strlen (spec->name) + 1);
	nameWithPrefix [0] = '\0';
	strcat (nameWithPrefix, CTAGS_FIELD_PREFIX);
	strcat (nameWithPrefix, spec->name);
	fdesc->nameWithPrefix = nameWithPrefix;

	fdesc->language = language;
	fdesc->sibling  = FIELD_UNKNOWN;

	updateSiblingField (spec->ftype, spec->name);
	return spec->ftype;
}
