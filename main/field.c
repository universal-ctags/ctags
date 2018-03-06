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
#include "entry_private.h"
#include "field.h"
#include "kind.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "trashbox.h"


typedef struct sFieldObject {
	fieldDefinition *def;
	unsigned int fixed:   1;   /* fields which cannot be disabled. */
	vString     *buffer;
	const char* nameWithPrefix;
	langType language;
	fieldType sibling;
} fieldObject;

static const char *renderFieldName (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldNameNoEscape (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
											bool *rejected);
static const char *renderFieldInput (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldInputNoEscape (const tagEntryInfo *const tag, const char *value, vString* b,
											 bool *rejected);
static const char *renderFieldCompactInputLine (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldSignature (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldScope (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldScopeNoEscape (const tagEntryInfo *const tag, const char *value, vString* b,
											 bool *rejected);
static const char *renderFieldTyperef (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldInherits (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldKindName (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldLineNumber (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldLanguage (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldAccess (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldKindLetter (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldImplementation (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldFile (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldPatternCommon (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldPatternCtags (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldRoles (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldRefMarker (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldExtras (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldXpath (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldScopeKindName(const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);
static const char *renderFieldEnd (const tagEntryInfo *const tag, const char *value, vString* b, bool *rejected);

static bool     isLanguageFieldAvailable  (const tagEntryInfo *const tag);
static bool     isTyperefFieldAvailable   (const tagEntryInfo *const tag);
static bool     isFileFieldAvailable      (const tagEntryInfo *const tag);
static bool     isInheritsFieldAvailable  (const tagEntryInfo *const tag);
static bool     isAccessFieldAvailable    (const tagEntryInfo *const tag);
static bool     isImplementationFieldAvailable (const tagEntryInfo *const tag);
static bool     isSignatureFieldAvailable (const tagEntryInfo *const tag);
static bool     isExtrasFieldAvailable    (const tagEntryInfo *const tag);
static bool     isXpathFieldAvailable     (const tagEntryInfo *const tag);
static bool     isEndFieldAvailable       (const tagEntryInfo *const tag);


#define DEFINE_FIELD(L, N, V, H, DT, ...)				\
	DEFINE_FIELD_FULL (L, N, V, H, NULL, DT, __VA_ARGS__)
#define DEFINE_FIELD_FULL(L, N, V, H, A, DT, ...)	\
	{					\
		.letter        = L,		\
		.name          = N,		\
		.description   = H,		\
		.enabled       = V,		\
		.renderEscaped = { __VA_ARGS__ },		\
		.isValueAvailable = A,		\
		.dataType = DT, \
	}

#define WITH_DEFUALT_VALUE(str) ((str)?(str):"-")

static fieldDefinition fieldDefinitionsFixed [] = {
        /* FIXED FIELDS */
	DEFINE_FIELD ('N', "name",     true,
			  "tag name",
			  FIELDTYPE_STRING,
			  [WRITER_U_CTAGS] = renderFieldName,
			  [WRITER_E_CTAGS] = renderFieldNameNoEscape,
			  [WRITER_JSON]    = renderFieldNameNoEscape,
			  ),
	DEFINE_FIELD ('F', "input",    true,
			   "input file",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldInput,
			   [WRITER_E_CTAGS] = renderFieldInputNoEscape,
			   [WRITER_JSON]    = renderFieldInputNoEscape,
		),
	DEFINE_FIELD ('P', "pattern",  true,
			   "pattern",
			   FIELDTYPE_STRING|FIELDTYPE_BOOL,
			   [WRITER_U_CTAGS] = renderFieldPatternCtags,
			   [WRITER_XREF]    = renderFieldPatternCommon,
			   [WRITER_JSON]    = renderFieldPatternCommon,
		),
};

static fieldDefinition fieldDefinitionsExuberant [] = {
	DEFINE_FIELD ('C', "compact",        false,
			   "compact input line (used only in xref output)",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldCompactInputLine),

	/* EXTENSION FIELDS */
	DEFINE_FIELD_FULL ('a', "access",         false,
		      "Access (or export) of class members",
			  isAccessFieldAvailable,
			  FIELDTYPE_STRING,
		      [WRITER_U_CTAGS] = renderFieldAccess),
	DEFINE_FIELD_FULL ('f', "file",           true,
		      "File-restricted scoping",
			  isFileFieldAvailable,
			  FIELDTYPE_BOOL,
		      [WRITER_U_CTAGS] = renderFieldFile),
	DEFINE_FIELD_FULL ('i', "inherits",       false,
		      "Inheritance information",
			  isInheritsFieldAvailable,
			  FIELDTYPE_STRING|FIELDTYPE_BOOL,
		      [WRITER_U_CTAGS] = renderFieldInherits),
	DEFINE_FIELD ('K', NULL,             false,
		      "Kind of tag as full name",
		      FIELDTYPE_STRING,
		      [WRITER_U_CTAGS] = renderFieldKindName),
	DEFINE_FIELD ('k', NULL,             true,
			   "Kind of tag as a single letter",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldKindLetter),
	DEFINE_FIELD_FULL ('l', "language",       false,
			   "Language of input file containing tag",
			   isLanguageFieldAvailable,
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldLanguage),
	DEFINE_FIELD_FULL ('m', "implementation", false,
			   "Implementation information",
			   isImplementationFieldAvailable,
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldImplementation),
	DEFINE_FIELD ('n', "line",           false,
			   "Line number of tag definition",
			   FIELDTYPE_INTEGER,
			   [WRITER_U_CTAGS] = renderFieldLineNumber),
	DEFINE_FIELD_FULL ('S', "signature",	     false,
			   "Signature of routine (e.g. prototype or parameter list)",
			   isSignatureFieldAvailable,
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldSignature),
	DEFINE_FIELD ('s', NULL,             true,
			   "Scope of tag definition (`p' can be used for printing its kind)",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldScope,
			   [WRITER_E_CTAGS] = renderFieldScopeNoEscape,
			   [WRITER_JSON]    = renderFieldScopeNoEscape),
	DEFINE_FIELD_FULL ('t', "typeref",        true,
			   "Type and name of a variable or typedef",
			   isTyperefFieldAvailable,
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldTyperef),
	DEFINE_FIELD ('z', "kind",           false,
			   "Include the \"kind:\" key in kind field (use k or K) in tags output, kind full name in xref output",
			   FIELDTYPE_STRING,
			   /* Following renderer is for handling --_xformat=%{kind};
			      and is not for tags output. */
			   [WRITER_U_CTAGS] = renderFieldKindName),
};

static fieldDefinition fieldDefinitionsUniversal [] = {
	DEFINE_FIELD ('r', "roles",    false,
			   "Roles",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldRoles),
	DEFINE_FIELD ('R',  NULL,     false,
			   "Marker (R or D) representing whether tag is definition or reference",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldRefMarker),
	DEFINE_FIELD ('Z', "scope",   false,
			  "Include the \"scope:\" key in scope field (use s) in tags output, scope name in xref output",
			   FIELDTYPE_STRING,
			   /* Following renderer is for handling --_xformat=%{scope};
			      and is not for tags output. */
			   [WRITER_U_CTAGS] = renderFieldScope,
			   [WRITER_E_CTAGS] = renderFieldScopeNoEscape,
			   [WRITER_JSON]    = renderFieldScopeNoEscape),
	DEFINE_FIELD_FULL ('E', "extras",   false,
			   "Extra tag type information",
			   isExtrasFieldAvailable,
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldExtras),
	DEFINE_FIELD_FULL ('x', "xpath",   false,
			   "xpath for the tag",
			   isXpathFieldAvailable,
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldXpath),
	DEFINE_FIELD ('p', "scopeKind", false,
			   "Kind of scope as full name",
			   FIELDTYPE_STRING,
			   [WRITER_U_CTAGS] = renderFieldScopeKindName),
	DEFINE_FIELD_FULL ('e', "end", false,
			   "end lines of various items",
			   isEndFieldAvailable,
			   FIELDTYPE_INTEGER,
			   [WRITER_U_CTAGS] = renderFieldEnd),
};


static unsigned int       fieldObjectUsed = 0;
static unsigned int       fieldObjectAllocated = 0;
static fieldObject* fieldObjects = NULL;

extern void initFieldObjects (void)
{
	unsigned int i;
	fieldObject *fobj;

	Assert (fieldObjects == NULL);

	fieldObjectAllocated
	  = ARRAY_SIZE (fieldDefinitionsFixed)
	  + ARRAY_SIZE (fieldDefinitionsExuberant)
	  + ARRAY_SIZE (fieldDefinitionsUniversal);
	fieldObjects = xMalloc (fieldObjectAllocated, fieldObject);
	DEFAULT_TRASH_BOX(&fieldObjects, eFreeIndirect);

	fieldObjectUsed = 0;

	for (i = 0; i < ARRAY_SIZE (fieldDefinitionsFixed); i++)
	{
		fobj = fieldObjects + i + fieldObjectUsed;
		fobj->def = fieldDefinitionsFixed + i;
		fobj->fixed  = 1;
		fobj->buffer = NULL;
		fobj->nameWithPrefix = fobj->def->name;
		fobj->language = LANG_IGNORE;
		fobj->sibling  = FIELD_UNKNOWN;
	}
	fieldObjectUsed += ARRAY_SIZE (fieldDefinitionsFixed);

	for (i = 0; i < ARRAY_SIZE (fieldDefinitionsExuberant); i++)
	{
		fobj = fieldObjects + i + fieldObjectUsed;
		fobj->def = fieldDefinitionsExuberant +i;
		fobj->fixed = 0;
		fobj->buffer = NULL;
		fobj->nameWithPrefix = fobj->def->name;
		fobj->language = LANG_IGNORE;
		fobj->sibling  = FIELD_UNKNOWN;
	}
	fieldObjectUsed += ARRAY_SIZE (fieldDefinitionsExuberant);

	for (i = 0; i < ARRAY_SIZE (fieldDefinitionsUniversal); i++)
	{
		char *nameWithPrefix;

		fobj = fieldObjects + i + fieldObjectUsed;
		fobj->def = fieldDefinitionsUniversal + i;
		fobj->fixed = 0;
		fobj->buffer = NULL;

		if (fobj->def->name)
		{
			nameWithPrefix = eMalloc (sizeof CTAGS_FIELD_PREFIX + strlen (fobj->def->name) + 1);
			nameWithPrefix [0] = '\0';
			strcat (nameWithPrefix, CTAGS_FIELD_PREFIX);
			strcat (nameWithPrefix, fobj->def->name);
			fobj->nameWithPrefix = nameWithPrefix;
			DEFAULT_TRASH_BOX(nameWithPrefix, eFree);
		}
		else
			fobj->nameWithPrefix = NULL;
		fobj->language = LANG_IGNORE;
		fobj->sibling  = FIELD_UNKNOWN;
	}
	fieldObjectUsed += ARRAY_SIZE (fieldDefinitionsUniversal);

	Assert ( fieldObjectAllocated == fieldObjectUsed );
}

static fieldObject* getFieldObject(fieldType type)
{
	Assert ((0 <= type) && ((unsigned int)type < fieldObjectUsed));
	return fieldObjects + type;
}

extern fieldType getFieldTypeForOption (char letter)
{
	unsigned int i;

	for (i = 0; i < fieldObjectUsed; i++)
	{
		if (fieldObjects [i].def->letter == letter)
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
	static bool initialized = false;
	unsigned int i;

	if (fieldName == NULL)
		return FIELD_UNKNOWN;

	if (language == LANG_AUTO && (initialized == false))
	{
		initialized = true;
		initializeParser (LANG_AUTO);
	}
	else if (language != LANG_IGNORE && (initialized == false))
		initializeParser (language);

	for (i = 0; i < fieldObjectUsed; i++)
	{
		if (fieldObjects [i].def->name
		    && strcmp (fieldObjects [i].def->name, fieldName) == 0
		    && ((language == LANG_AUTO)
			|| (fieldObjects [i].language == language)))
			return i;
	}

	return FIELD_UNKNOWN;
}

extern const char* getFieldName(fieldType type)
{
	fieldObject* fobj;

	fobj = getFieldObject (type);
	if (Option.putFieldPrefix)
		return fobj->nameWithPrefix;
	else
		return fobj->def->name;
}

extern bool doesFieldHaveValue (fieldType type, const tagEntryInfo *tag)
{
	if (getFieldObject(type)->def->isValueAvailable)
		return getFieldObject(type)->def->isValueAvailable(tag);
	else
		return true;
}

static const char *renderAsIs (vString* b CTAGS_ATTR_UNUSED, const char *s)
{
	return s;
}

static const char *renderEscapedString (const char *s,
					const tagEntryInfo *const tag CTAGS_ATTR_UNUSED,
					vString* b)
{
	vStringCatSWithEscaping (b, s);
	return vStringValue (b);
}

static const char *renderEscapedName (const bool isTagName,
				      const char* s,
				      const tagEntryInfo *const tag,
				      vString* b)
{
	int unexpected_byte = 0;

	if (isTagName && (*s == ' ' || *s == '!'))
	{
		/* Don't allow a leading space or exclamation mark as it conflicts with
		 * pseudo-tags when sorting.  Anything with a lower byte value is
		 * escaped by renderEscapedString() already. */
		unexpected_byte = *s;
		switch (*s)
		{
			case ' ': vStringCatS (b, "\\x20"); s++; break;
			case '!': vStringCatS (b, "\\x21"); s++; break;
			default: AssertNotReached();
		}
	}
	else
	{
		/* Find the first byte needing escaping for the warning message */
		const char *p = s;

		while (*p > 0x1F && *p != 0x7F)
			p++;
		unexpected_byte = *p;
	}

	if (unexpected_byte)
	{
		const kindDefinition *kdef = getTagKind (tag);
		verbose ("Unexpected character %#04x included in a tagEntryInfo: %s\n", unexpected_byte, s);
		verbose ("File: %s, Line: %lu, Lang: %s, Kind: %c\n",
			 tag->inputFileName, tag->lineNumber, getLanguageName(tag->langType), kdef->letter);
		verbose ("Escape the character\n");
	}

	return renderEscapedString (s, tag, b);
}

static const char *renderFieldName (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
									bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderEscapedName (true, tag->name, tag, b);
}

static const char *renderFieldNameNoEscape (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
											bool *rejected)
{
	if (strpbrk (tag->name, " \t"))
	{
		*rejected = true;
		return NULL;
	}
	return renderAsIs (b, tag->name);
}

static const char *renderFieldInput (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
									 bool *rejected CTAGS_ATTR_UNUSED)
{
	const char *f = tag->inputFileName;

	if (Option.lineDirectives && tag->sourceFileName)
		f = tag->sourceFileName;
	return renderEscapedString (f, tag, b);
}

static const char *renderFieldInputNoEscape (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
											 bool *rejected)
{
	const char *f = tag->inputFileName;

	if (Option.lineDirectives && tag->sourceFileName)
		f = tag->sourceFileName;

	if (strpbrk (f, " \t"))
	{
		*rejected = true;
		return NULL;
	}

	return renderAsIs (b, f);
}

static const char *renderFieldSignature (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
										 bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderEscapedString (WITH_DEFUALT_VALUE (tag->extensionFields.signature),
				    tag, b);
}

static const char *renderFieldScope (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
									 bool *rejected CTAGS_ATTR_UNUSED)
{
	const char* scope;

	getTagScopeInformation ((tagEntryInfo *const)tag, NULL, &scope);
	return scope? renderEscapedName (false, scope, tag, b): NULL;
}

static const char *renderFieldScopeNoEscape (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
											 bool *rejected)
{
	const char* scope;

	getTagScopeInformation ((tagEntryInfo *const)tag, NULL, &scope);
	if (scope && strpbrk (scope, " \t"))
	{
		*rejected = true;
		return NULL;
	}

	return scope? renderAsIs (b, scope): NULL;
}

static const char *renderFieldInherits (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
										bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderEscapedString (WITH_DEFUALT_VALUE (tag->extensionFields.inheritance),
				    tag, b);
}

static const char *renderFieldTyperef (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
									   bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderEscapedName (false, WITH_DEFUALT_VALUE (tag->extensionFields.typeRef [1]), tag, b);
}


extern const char* renderFieldEscaped (writerType writer,
				      fieldType type,
				       const tagEntryInfo *tag,
				       int index,
					   bool *rejected)
{
	fieldObject *fobj = fieldObjects + type;
	const char *value;
	renderEscaped rfn;
	bool stub;

	Assert (tag);
	Assert (fobj->def->renderEscaped);
	Assert (index < 0 || ((unsigned int)index) < tag->usedParserFields);

	fobj->buffer = vStringNewOrClearWithAutoRelease (fobj->buffer);

	if (index >= 0)
	{
		const tagField *f = getParserField (tag, index);

		value = f->value;
	}
	else
		value = NULL;

	rfn = fobj->def->renderEscaped [writer];
	if (rfn == NULL)
		rfn = fobj->def->renderEscaped [WRITER_DEFAULT];

	if (!rejected)
		rejected = &stub;
	return rfn (tag, value, fobj->buffer, rejected);
}

/*  Writes "line", stripping leading and duplicate white space.
 */
static const char* renderCompactInputLine (vString *b,  const char *const line)
{
	bool lineStarted = false;
	const char *p;
	int c;

	/*  Write everything up to, but not including, the newline.
	 */
	for (p = line, c = *p  ;  c != NEWLINE  &&  c != '\0'  ;  c = *++p)
	{
		if (lineStarted  || ! isspace (c))  /* ignore leading spaces */
		{
			lineStarted = true;
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

static const char *renderFieldKindName (const tagEntryInfo *const tag, const char *value CTAGS_ATTR_UNUSED, vString* b,
										bool *rejected CTAGS_ATTR_UNUSED)
{
	const char* name = getTagKindName (tag);
	return renderAsIs (b, name);
}

static const char *renderFieldCompactInputLine (const tagEntryInfo *const tag,
						const char *value CTAGS_ATTR_UNUSED,
						 vString* b,
						bool *rejected CTAGS_ATTR_UNUSED)
{
	const char *line;
	static vString *tmp;

	tmp = vStringNewOrClearWithAutoRelease (tmp);

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
					  const char *value CTAGS_ATTR_UNUSED,
					  vString* b,
					  bool *rejected CTAGS_ATTR_UNUSED)
{
	long ln = tag->lineNumber;
	char buf[32] = {[0] = '\0'};

	if (Option.lineDirectives && (tag->sourceLineNumberDifference != 0))
		ln += tag->sourceLineNumberDifference;
	snprintf (buf, sizeof(buf), "%ld", ln);
	vStringCatS (b, buf);
	return vStringValue (b);
}

static const char *renderFieldRoles (const tagEntryInfo *const tag,
				    const char *value CTAGS_ATTR_UNUSED,
				    vString* b,
					bool *rejected CTAGS_ATTR_UNUSED)
{
	roleBitsType rbits = tag->extensionFields.roleBits;
	const roleDefinition * role;
	if (rbits)
	{
		int roleCount = countLanguageRoles (tag->langType, tag->kindIndex);
		int nRoleWritten = 0;

		for (int roleIndex = 0; roleIndex < roleCount; roleIndex++)
		{
			if (((rbits >> roleIndex) & (roleBitsType)1)
				&& isLanguageRoleEnabled (tag->langType, tag->kindIndex, roleIndex))
			{
				if (nRoleWritten > 0)
					vStringPut(b, ',');

				role = getTagRole(tag, roleIndex);
				renderRole (role, b);
				nRoleWritten++;
			}
		}
	}
	else
		vStringCatS (b, ROLE_NAME_DEFINITION);
	return vStringValue (b);
}

static const char *renderFieldLanguage (const tagEntryInfo *const tag,
					const char *value CTAGS_ATTR_UNUSED,
					vString* b,
					bool *rejected CTAGS_ATTR_UNUSED)
{
	const char *l;

	if (Option.lineDirectives && (tag->sourceLangType != LANG_IGNORE))
		l = getLanguageName(tag->sourceLangType);
	else
		l = getLanguageName(tag->langType);

	return renderAsIs (b, WITH_DEFUALT_VALUE(l));
}

static const char *renderFieldAccess (const tagEntryInfo *const tag,
				      const char *value CTAGS_ATTR_UNUSED,
				      vString* b,
					  bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderAsIs (b, WITH_DEFUALT_VALUE (tag->extensionFields.access));
}

static const char *renderFieldKindLetter (const tagEntryInfo *const tag,
					  const char *value CTAGS_ATTR_UNUSED,
					  vString* b,
					  bool *rejected CTAGS_ATTR_UNUSED)
{
	static char c[2] = { [1] = '\0' };

	c [0] = getTagKindLetter(tag);

	return renderAsIs (b, c);
}

static const char *renderFieldImplementation (const tagEntryInfo *const tag,
					      const char *value CTAGS_ATTR_UNUSED,
					      vString* b,
						  bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderAsIs (b, WITH_DEFUALT_VALUE (tag->extensionFields.implementation));
}

static const char *renderFieldFile (const tagEntryInfo *const tag,
				    const char *value CTAGS_ATTR_UNUSED,
				    vString* b,
					bool *rejected CTAGS_ATTR_UNUSED)
{
	return renderAsIs (b, tag->isFileScope? "file": "-");
}

static const char *renderFieldPatternCommon (const tagEntryInfo *const tag,
				       const char *value CTAGS_ATTR_UNUSED,
				       vString* b,
					   bool *rejected CTAGS_ATTR_UNUSED)
{
	if (tag->isFileEntry)
		return NULL;
	else if (tag->pattern)
		vStringCatS (b, tag->pattern);
	else
	{
		char* tmp;

		tmp = makePatternString (tag);
		vStringCatS (b, tmp);
		eFree (tmp);
	}
	return vStringValue (b);
}

static const char *renderFieldPatternCtags (const tagEntryInfo *const tag,
				       const char *value,
				       vString* b,
					   bool *rejected)
{
	/* This is for handling 'common' of 'fortran'.  See the
	   description of --excmd=mixed in ctags.1.  In tags output, what
	   we call "pattern" is instructions for vi.

	   However, in the other formats, pattern should be pattern as its name. */
	if (tag->lineNumberEntry)
		return NULL;

	return renderFieldPatternCommon(tag, value, b, rejected);
}

static const char *renderFieldRefMarker (const tagEntryInfo *const tag,
					 const char *value CTAGS_ATTR_UNUSED,
					 vString* b,
					 bool *rejected CTAGS_ATTR_UNUSED)
{
	static char c[2] = { [1] = '\0' };

	c [0] = (tag->extensionFields.roleBits)? 'R': 'D';

	return renderAsIs (b, c);
}

static const char *renderFieldExtras (const tagEntryInfo *const tag,
				     const char *value CTAGS_ATTR_UNUSED,
				     vString* b,
					 bool *rejected CTAGS_ATTR_UNUSED)
{
	int i;
	bool hasExtra = false;
	int c = countXtags();

	for (i = 0; i < c; i++)
	{
		const char *name = getXtagName (i);

		if (!name)
			continue;

		if (isTagExtraBitMarked (tag, i))
		{

			if (hasExtra)
				vStringPut (b, ',');
			vStringCatS (b, name);
			hasExtra = true;
		}
	}

	if (hasExtra)
		return vStringValue (b);
	else
		return NULL;
}

static const char *renderFieldXpath (const tagEntryInfo *const tag,
				     const char *value CTAGS_ATTR_UNUSED,
				     vString* b,
					 bool *rejected CTAGS_ATTR_UNUSED)
{
#ifdef HAVE_LIBXML
	if (tag->extensionFields.xpath)
		return renderEscapedString (tag->extensionFields.xpath,
					    tag, b);
#endif
	return NULL;
}

static const char *renderFieldScopeKindName(const tagEntryInfo *const tag,
					    const char *value CTAGS_ATTR_UNUSED,
					    vString* b,
						bool *rejected CTAGS_ATTR_UNUSED)
{
	const char* kind;

	getTagScopeInformation ((tagEntryInfo *const)tag, &kind, NULL);
	return kind? renderAsIs (b, kind): NULL;
}

static const char *renderFieldEnd (const tagEntryInfo *const tag,
				   const char *value CTAGS_ATTR_UNUSED,
				   vString* b,
				   bool *rejected CTAGS_ATTR_UNUSED)
{
	static char buf[16];

	if (tag->extensionFields.endLine != 0)
	{
		sprintf (buf, "%lu", tag->extensionFields.endLine);
		return renderAsIs (b, buf);
	}
	else
		return NULL;
}

static bool     isLanguageFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->langType == LANG_IGNORE)? false: true;
}

static bool     isTyperefFieldAvailable  (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.typeRef [0] != NULL
		&& tag->extensionFields.typeRef [1] != NULL)? true: false;
}

static bool     isFileFieldAvailable  (const tagEntryInfo *const tag)
{
	return tag->isFileScope? true: false;
}

static bool     isInheritsFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.inheritance != NULL)? true: false;
}

static bool     isAccessFieldAvailable   (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.access != NULL)? true: false;
}

static bool     isImplementationFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.implementation != NULL)? true: false;
}

static bool     isSignatureFieldAvailable (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.signature != NULL)? true: false;
}

static bool     isExtrasFieldAvailable     (const tagEntryInfo *const tag)
{
	unsigned int i;
	for (i = 0; i < sizeof (tag->extra); i++)
	{
		if (tag->extra [i])
			return true;
		else if (tag->extraDynamic)
			return true;
	}

	return false;
}

static bool     isXpathFieldAvailable      (const tagEntryInfo *const tag)
{
#ifdef HAVE_LIBXML
	return (tag->extensionFields.xpath != NULL)? true: false;
#else
	return false;
#endif
}

static bool     isEndFieldAvailable       (const tagEntryInfo *const tag)
{
	return (tag->extensionFields.endLine != 0)? true: false;
}

extern bool isFieldEnabled (fieldType type)
{
	return getFieldObject(type)->def->enabled;
}

static bool isFieldFixed (fieldType type)
{
	return getFieldObject(type)->fixed? true: false;
}

extern bool enableField (fieldType type, bool state, bool warnIfFixedField)
{
	fieldDefinition *def = getFieldObject(type)->def;
	bool old = def->enabled;
	if (isFieldFixed (type))
	{
		if ((!state) && warnIfFixedField)
		{
			if (def->name && def->letter != NUL_FIELD_LETTER)
				error(WARNING, "Cannot disable fixed field: '%c'{%s}",
				      def->letter, def->name);
			else if (def->name)
				error(WARNING, "Cannot disable fixed field: {%s}",
				      def->name);
			else if (def->letter != NUL_FIELD_LETTER)
				error(WARNING, "Cannot disable fixed field: '%c'",
				      getFieldObject(type)->def->letter);
			else
				AssertNotReached();
		}
	}
	else
	{
		getFieldObject(type)->def->enabled = state;

		if (isCommonField (type))
			verbose ("enable field \"%s\": %s\n",
				 getFieldObject(type)->def->name,
				 (state? "yes": "no"));
		else
			verbose ("enable field \"%s\"<%s>: %s\n",
				 getFieldObject(type)->def->name,
				 getLanguageName (getFieldOwner(type)),
				 (state? "yes": "no"));
	}
	return old;
}

extern bool isCommonField (fieldType type)
{
	return (FIELD_BUILTIN_LAST < type)? false: true;
}

extern int     getFieldOwner (fieldType type)
{
	return getFieldObject(type)->language;
}

extern unsigned int getFieldDataType (fieldType type)
{
	return getFieldObject(type)->def->dataType;
}

extern bool isFieldRenderable (fieldType type)
{
	return getFieldObject(type)->def->renderEscaped [WRITER_DEFAULT]? true: false;
}

extern int countFields (void)
{
	return fieldObjectUsed;
}

extern fieldType nextSiblingField (fieldType type)
{
	fieldObject *fobj;

	fobj = fieldObjects + type;
	return fobj->sibling;
}

static void updateSiblingField (fieldType type, const char* name)
{
	int i;
	fieldObject *fobj;

	for (i = type; i > 0; i--)
	{
		fobj = fieldObjects + i - 1;
		if (fobj->def->name && (strcmp (fobj->def->name, name) == 0))
		{
			Assert (fobj->sibling == FIELD_UNKNOWN);
			fobj->sibling = type;
			break;
		}
	}
}

static const char* defaultRenderer (const tagEntryInfo *const tag CTAGS_ATTR_UNUSED,
				    const char *value,
				    vString * buffer CTAGS_ATTR_UNUSED,
					bool *rejected CTAGS_ATTR_UNUSED)
{
	return value;
}

extern int defineField (fieldDefinition *def, langType language)
{
	fieldObject *fobj;
	char *nameWithPrefix;
	size_t i;

	Assert (def);
	Assert (def->name);
	for (i = 0; i < strlen (def->name); i++)
	{
		Assert ( isalpha (def->name [i]) );
	}
	def->letter = NUL_FIELD_LETTER;

	if (fieldObjectUsed == fieldObjectAllocated)
	{
		fieldObjectAllocated *= 2;
		fieldObjects = xRealloc (fieldObjects, fieldObjectAllocated, fieldObject);
	}
	fobj = fieldObjects + (fieldObjectUsed);
	def->ftype = fieldObjectUsed++;

	if (def->renderEscaped [WRITER_DEFAULT] == NULL)
		def->renderEscaped [WRITER_DEFAULT] = defaultRenderer;

	if (! def->dataType)
		def->dataType = FIELDTYPE_STRING;

	fobj->def = def;

	fobj->fixed =  0;
	fobj->buffer = NULL;

	nameWithPrefix = eMalloc (sizeof CTAGS_FIELD_PREFIX + strlen (def->name) + 1);
	nameWithPrefix [0] = '\0';
	strcat (nameWithPrefix, CTAGS_FIELD_PREFIX);
	strcat (nameWithPrefix, def->name);
	fobj->nameWithPrefix = nameWithPrefix;
	DEFAULT_TRASH_BOX(nameWithPrefix, eFree);

	fobj->language = language;
	fobj->sibling  = FIELD_UNKNOWN;

	updateSiblingField (def->ftype, def->name);
	return def->ftype;
}

#define FIELD_COL_LETTER      0
#define FIELD_COL_NAME        1
#define FIELD_COL_ENABLED     2
#define FIELD_COL_LANGUAGE    3
#define FIELD_COL_JSTYPE      4
#define FIELD_COL_FIXED       5
#define FIELD_COL_DESCRIPTION 6
extern struct colprintTable * fieldColprintTableNew (void)
{
	return colprintTableNew ("L:LETTER", "L:NAME", "L:ENABLED",
							 "L:LANGUAGE", "L:JSTYPE", "L:FIXED", "L:DESCRIPTION", NULL);
}

static void  fieldColprintAddLine (struct colprintTable *table, int i)
{
	fieldObject *fobj = getFieldObject(i);
	fieldDefinition *fdef = fobj->def;

	struct colprintLine *line = colprintTableGetNewLine(table);

	colprintLineAppendColumnChar (line,
								  (fdef->letter == NUL_FIELD_LETTER)
								  ? '-'
								  : fdef->letter);

	const char *name = getFieldName (i);
	colprintLineAppendColumnCString (line, name? name: RSV_NONE);
	colprintLineAppendColumnBool (line, fdef->enabled);
	colprintLineAppendColumnCString (line,
									 fobj->language == LANG_IGNORE
									 ? RSV_NONE
									 : getLanguageName (fobj->language));

	char  typefields [] = "---";
	{
		unsigned int bmask, offset;
		unsigned int type = getFieldDataType(i);
		for (bmask = 1, offset = 0;
			 bmask < FIELDTYPE_END_MARKER;
			 bmask <<= 1, offset++)
			if (type & bmask)
				typefields[offset] = fieldDataTypeFalgs[offset];
	}
	colprintLineAppendColumnCString (line, typefields);
	colprintLineAppendColumnBool (line, fobj->fixed);
	colprintLineAppendColumnCString (line, fdef->description);
}

extern void fieldColprintAddCommonLines (struct colprintTable *table)
{
	for (int i = 0; i <= FIELD_BUILTIN_LAST; i++)
		fieldColprintAddLine(table, i);
}

extern void fieldColprintAddLanguageLines (struct colprintTable *table, langType language)
{
	for (int i = FIELD_BUILTIN_LAST + 1; i < fieldObjectUsed; i++)
	{
		fieldObject *fobj = getFieldObject(i);
		if (fobj->language == language)
			fieldColprintAddLine (table, i);
	}
}

static int fieldColprintCompareLines (struct colprintLine *a , struct colprintLine *b)
{
	const char *a_fixed  = colprintLineGetColumn (a, FIELD_COL_FIXED);
	const char *b_fixed  = colprintLineGetColumn (b, FIELD_COL_FIXED);
	const char *a_parser = colprintLineGetColumn (a, FIELD_COL_LANGUAGE);
	const char *b_parser = colprintLineGetColumn (b, FIELD_COL_LANGUAGE);

	if ((strcmp (a_fixed, "yes") == 0)
		&& (strcmp (b_fixed, "yes") == 0))
	{
		/* name, input, pattern, compact */
		const char *a_name  = colprintLineGetColumn (a, FIELD_COL_NAME);
		const char *b_name  = colprintLineGetColumn (b, FIELD_COL_NAME);
		const char *ref_name;
		unsigned int a_index = ~0U;
		unsigned int b_index = ~0U;

		for (unsigned int i = 0; i < ARRAY_SIZE(fieldDefinitionsFixed); i++)
		{
			ref_name = fieldDefinitionsFixed [i].name;
			if (strcmp (a_name, ref_name) == 0)
				a_index = i;
			if (strcmp (b_name, ref_name) == 0)
				b_index = i;
			if ((a_index != ~0U) || (b_index != ~0U))
				break;
		}

		if (a_index < b_index)
			return -1;
		else if (a_index == b_index)
			return 0;			/* ??? */
		else
			return 1;
	}
	else if ((strcmp (a_fixed, "yes") == 0)
			  && (strcmp (b_fixed, "yes") != 0))
		return -1;
	else if ((strcmp (a_fixed, "yes") != 0)
			 && (strcmp (b_fixed, "yes") == 0))
		return 1;

	if (strcmp (a_parser, RSV_NONE) == 0
		&& strcmp (b_parser, RSV_NONE) != 0)
		return -1;
	else if (strcmp (a_parser, RSV_NONE) != 0
			 && strcmp (b_parser, RSV_NONE) == 0)
		return 1;
	else if (strcmp (a_parser, RSV_NONE) != 0
			 && strcmp (b_parser, RSV_NONE) != 0)
	{
		int r;
		r = strcmp (a_parser, b_parser);
		if (r != 0)
			return r;

		const char *a_name = colprintLineGetColumn (a, FIELD_COL_NAME);
		const char *b_name = colprintLineGetColumn (b, FIELD_COL_NAME);

		return strcmp(a_name, b_name);
	}
	else
	{
		const char *a_letter = colprintLineGetColumn (a, FIELD_COL_LETTER);
		const char *b_letter = colprintLineGetColumn (b, FIELD_COL_LETTER);

		return strcmp(a_letter, b_letter);
	}
}

extern void fieldColprintTablePrint (struct colprintTable *table,
									 bool withListHeader, bool machinable, FILE *fp)
{
	colprintTableSort (table, fieldColprintCompareLines);
	colprintTablePrint (table, 0, withListHeader, machinable, fp);
}
