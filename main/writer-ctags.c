/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/

#include "general.h"  /* must always come first */

#include "entry_p.h"
#include "field.h"
#include "field_p.h"
#include "mio.h"
#include "options_p.h"
#include "parse_p.h"
#include "ptag_p.h"
#include "read.h"
#include "writer_p.h"
#include "xtag.h"
#include "xtag_p.h"


#define CTAGS_FILE  "tags"


static int writeCtagsEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
							MIO * mio, const tagEntryInfo *const tag,
							void *clientData);
static int writeCtagsPtagEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
								MIO * mio, const ptagDesc *desc,
								const char *const fileName,
								const char *const pattern,
								const char *const parserName,
								void *clientData);
static bool treatFieldAsFixed (int fieldType);
static void checkCtagsOptions (tagWriter *writer, bool fieldsWereReset);

#ifdef WIN32
static enum filenameSepOp overrideFilenameSeparator (enum filenameSepOp currentSetting);
#endif	/* WIN32 */

struct rejection {
	bool rejectionInThisInput;
};

tagWriter uCtagsWriter = {
	.writeEntry = writeCtagsEntry,
	.writePtagEntry = writeCtagsPtagEntry,
	.printPtagByDefault = true,
	.preWriteEntry = NULL,
	.postWriteEntry = NULL,
	.rescanFailedEntry = NULL,
	.treatFieldAsFixed = treatFieldAsFixed,
	.checkOptions = checkCtagsOptions,
#ifdef WIN32
	.overrideFilenameSeparator = overrideFilenameSeparator,
#endif
	.defaultFileName = CTAGS_FILE,
};

static void *beginECtagsFile (tagWriter *writer CTAGS_ATTR_UNUSED, MIO * mio CTAGS_ATTR_UNUSED,
							  void *clientData CTAGS_ATTR_UNUSED)
{
	static struct rejection rej;

	rej.rejectionInThisInput = false;

	return &rej;
}

static bool endECTagsFile (tagWriter *writer, MIO * mio CTAGS_ATTR_UNUSED, const char* filename CTAGS_ATTR_UNUSED,
						   void *clientData CTAGS_ATTR_UNUSED)
{
	struct rejection *rej = writer->private;
	return rej->rejectionInThisInput;
}

#ifdef WIN32
static enum filenameSepOp overrideFilenameSeparator (enum filenameSepOp currentSetting)
{
	if (currentSetting == FILENAME_SEP_UNSET)
		return FILENAME_SEP_USE_SLASH;
	return currentSetting;
}
#endif

tagWriter eCtagsWriter = {
	.writeEntry = writeCtagsEntry,
	.writePtagEntry = writeCtagsPtagEntry,
	.printPtagByDefault = true,
	.preWriteEntry = beginECtagsFile,
	.postWriteEntry = endECTagsFile,
	.rescanFailedEntry = NULL,
	.treatFieldAsFixed = treatFieldAsFixed,
	.defaultFileName = CTAGS_FILE,
	.checkOptions = checkCtagsOptions,
};

static bool hasTagEntryTabOrNewlineChar (const tagEntryInfo * const tag)
{

	if (doesFieldHaveTabOrNewlineChar (FIELD_NAME, tag, NO_PARSER_FIELD)
		|| doesFieldHaveTabOrNewlineChar (FIELD_INPUT_FILE, tag, NO_PARSER_FIELD))
		return true;

	if (tag->lineNumberEntry)
	{
		if (Option.lineDirectives)
		{
			if (doesFieldHaveTabOrNewlineChar (FIELD_LINE_NUMBER, tag, NO_PARSER_FIELD))
				return true;
		}
	}
	else if (doesFieldHaveTabOrNewlineChar (FIELD_PATTERN, tag, NO_PARSER_FIELD))
	{
		/* Pattern may have a tab char. However, doesFieldHaveTabOrNewlineChar returns
		 * false because NO_PARSER_FIELD may not have doesContainAnyChar handler.
		 */
		return true;
	}

	if (includeExtensionFlags ())
	{
		if (isFieldEnabled (FIELD_SCOPE) && doesFieldHaveValue (FIELD_SCOPE, tag)
			&& (doesFieldHaveTabOrNewlineChar (FIELD_SCOPE_KIND_LONG, tag, NO_PARSER_FIELD)
				|| doesFieldHaveTabOrNewlineChar (FIELD_SCOPE, tag, NO_PARSER_FIELD)))
			return true;
		if (isFieldEnabled (FIELD_TYPE_REF) && doesFieldHaveValue (FIELD_TYPE_REF, tag)
			&& doesFieldHaveTabOrNewlineChar (FIELD_TYPE_REF, tag, NO_PARSER_FIELD))
			return true;
		if (isFieldEnabled (FIELD_FILE_SCOPE) && doesFieldHaveValue (FIELD_FILE_SCOPE, tag)
			&& doesFieldHaveTabOrNewlineChar (FIELD_FILE_SCOPE, tag, NO_PARSER_FIELD))
			return true;

		int f[] = { FIELD_INHERITANCE,
					FIELD_ACCESS,
					FIELD_IMPLEMENTATION,
					FIELD_SIGNATURE,
					FIELD_ROLES,
					FIELD_EXTRAS,
					FIELD_XPATH,
					FIELD_END_LINE,
					-1};
		for (unsigned int i = 0; f[i] >= 0; i++)
		{
			if (isFieldEnabled (f[i]) && doesFieldHaveValue (f[i], tag)
				&& doesFieldHaveTabOrNewlineChar (f[i], tag, NO_PARSER_FIELD))
				return true;
		}
	}

	for (unsigned int i = 0; i < tag->usedParserFields; i++)
	{
		const tagField *f = getParserFieldForIndex(tag, i);
		fieldType ftype = f->ftype;
		if (isFieldEnabled (ftype))
		{
			if (doesFieldHaveTabOrNewlineChar (ftype, tag, i))
				return true;
		}
	}
	return false;
}


static const char* escapeFieldValueFull (tagWriter *writer, const tagEntryInfo * tag, fieldType ftype, int fieldIndex)
{
	const char *v;
	if (writer->type == WRITER_E_CTAGS && doesFieldHaveRenderer(ftype, true))
		v = renderFieldNoEscaping (ftype, tag, fieldIndex);
	else
		v = renderField (ftype, tag, fieldIndex);

	return v;
}

static const char* escapeFieldValue (tagWriter *writer, const tagEntryInfo * tag, fieldType ftype)
{
	return escapeFieldValueFull (writer, tag, ftype, NO_PARSER_FIELD);
}

static int renderExtensionFieldMaybe (tagWriter *writer, int xftype, const tagEntryInfo *const tag, char sep[2], MIO *mio)
{
	if (isFieldEnabled (xftype) && doesFieldHaveValue (xftype, tag))
	{
		int len;
		len = mio_printf (mio, "%s\t%s:%s", sep,
				  getFieldName (xftype),
				  escapeFieldValue (writer, tag, xftype));
		sep[0] = '\0';
		return len;
	}
	else
		return 0;
}

static int addParserFields (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag)
{
	unsigned int i;
	int length = 0;

	for (i = 0; i < tag->usedParserFields; i++)
	{
		const tagField *f = getParserFieldForIndex(tag, i);
		fieldType ftype = f->ftype;
		if (! isFieldEnabled (ftype))
			continue;

		length += mio_printf(mio, "\t%s:%s",
							 getFieldName (ftype),
							 escapeFieldValueFull (writer, tag, ftype, i));
	}
	return length;
}

static int writeLineNumberEntry (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag)
{
	if (Option.lineDirectives)
		return mio_printf (mio, "%s", escapeFieldValue (writer, tag, FIELD_LINE_NUMBER));
	else
		return mio_printf (mio, "%lu", tag->lineNumber);
}

static int addExtensionFields (tagWriter *writer, MIO *mio, const tagEntryInfo *const tag)
{
	bool isKindKeyEnabled = isFieldEnabled (FIELD_KIND_KEY);
	bool isScopeEnabled = isFieldEnabled   (FIELD_SCOPE_KEY);

	const char* const kindKey = isKindKeyEnabled
		?getFieldName (FIELD_KIND_KEY)
		:"";
	const char* const kindFmt = isKindKeyEnabled
		?"%s\t%s:%s"
		:"%s\t%s%s";
	const char* const scopeKey = isScopeEnabled
		?getFieldName (FIELD_SCOPE_KEY)
		:"";
	const char* const scopeFmt = isScopeEnabled
		?"%s\t%s:%s:%s"
		:"%s\t%s%s:%s";

	char sep [] = {';', '"', '\0'};
	int length = 0;

	const char *str = NULL;;
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);
	const char kind_letter_str[2] = {kdef->letter, '\0'};

	if (kdef->name != NULL && (isFieldEnabled (FIELD_KIND_LONG)  ||
		 (isFieldEnabled (FIELD_KIND)  && kdef->letter == KIND_NULL_LETTER)))
	{
		/* Use kind long name */
		str = kdef->name;
	}
	else if (kdef->letter != KIND_NULL_LETTER  && (isFieldEnabled (FIELD_KIND) ||
			(isFieldEnabled (FIELD_KIND_LONG) &&  kdef->name == NULL)))
	{
		/* Use kind letter */
		str = kind_letter_str;
	}

	if (str)
	{
		length += mio_printf (mio, kindFmt, sep, kindKey, str);
		sep [0] = '\0';
	}

	if (isFieldEnabled (FIELD_LINE_NUMBER) &&  doesFieldHaveValue (FIELD_LINE_NUMBER, tag))
	{
		length += mio_printf (mio, "%s\t%s:%ld", sep,
				   getFieldName (FIELD_LINE_NUMBER),
				   tag->lineNumber);
		sep [0] = '\0';
	}

	length += renderExtensionFieldMaybe (writer, FIELD_LANGUAGE, tag, sep, mio);

	if (isFieldEnabled (FIELD_SCOPE))
	{
		const char* k, *v;

		k = escapeFieldValue (writer, tag, FIELD_SCOPE_KIND_LONG);
		v = escapeFieldValue (writer, tag, FIELD_SCOPE);
		if (k && v)
		{
			length += mio_printf (mio, scopeFmt, sep, scopeKey, k, v);
			sep [0] = '\0';
		}
	}

	if (isFieldEnabled (FIELD_TYPE_REF) && doesFieldHaveValue (FIELD_TYPE_REF, tag))
	{
		length += mio_printf (mio, "%s\t%s:%s", sep,
				      getFieldName (FIELD_TYPE_REF),
				      escapeFieldValue (writer, tag, FIELD_TYPE_REF));
		sep [0] = '\0';
	}

	if (isFieldEnabled (FIELD_FILE_SCOPE) &&  doesFieldHaveValue (FIELD_FILE_SCOPE, tag))
	{
		length += mio_printf (mio, "%s\t%s:", sep,
				      getFieldName (FIELD_FILE_SCOPE));
		sep [0] = '\0';
	}

	for (int k = FIELD_ECTAGS_LOOP_START; k <= FIELD_ECTAGS_LOOP_LAST; k++)
		length += renderExtensionFieldMaybe (writer, k, tag, sep, mio);
	for (int k = FIELD_UCTAGS_LOOP_START; k <= FIELD_BUILTIN_LAST; k++)
		length += renderExtensionFieldMaybe (writer, k, tag, sep, mio);

	return length;
}

static int writeCtagsEntry (tagWriter *writer,
							MIO * mio, const tagEntryInfo *const tag,
							void *clientData CTAGS_ATTR_UNUSED)
{
	if (writer->private)
	{
		struct rejection *rej = writer->private;
		if (hasTagEntryTabOrNewlineChar (tag))
		{
			rej->rejectionInThisInput = true;
			return 0;
		}
	}

	int length = mio_printf (mio, "%s\t%s\t",
			      escapeFieldValue (writer, tag, FIELD_NAME),
			      escapeFieldValue (writer, tag, FIELD_INPUT_FILE));

	/* This is for handling 'common' of 'fortran'.  See the
	   description of --excmd=mixed in ctags.1.  In tags output, what
	   we call "pattern" is instructions for vi.

	   However, in the other formats, pattern should be pattern as its name. */
	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (writer, mio, tag);
	else
	{
		if (Option.locate == EX_COMBINE)
			length += mio_printf(mio, "%lu;", tag->lineNumber);
		length += mio_puts(mio, escapeFieldValue(writer, tag, FIELD_PATTERN));
	}

	if (includeExtensionFlags ())
	{
		length += addExtensionFields (writer, mio, tag);
		length += addParserFields (writer, mio, tag);
	}

	length += mio_printf (mio, "\n");

	return length;
}

static int writeCtagsPtagEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
				MIO * mio, const ptagDesc *desc,
				const char *const fileName,
				const char *const pattern,
				const char *const parserName,
				void *clientData CTAGS_ATTR_UNUSED)
{

	bool extras = includeExtensionFlags () && isFieldEnabled (FIELD_EXTRAS);
	const char *xsep = extras? ";\"	": "";
	const char *fsep = extras? ":": "";
	const char *fieldx = extras? getFieldName (FIELD_EXTRAS): "";
	const char *xptag = extras? getXtagName (XTAG_PSEUDO_TAGS): "";

	return parserName

#define OPT(X) ((X)?(X):"")
		? mio_printf (mio, "%s%s%s%s\t%s\t/%s/%s%s%s%s\n",
			      PSEUDO_TAG_PREFIX, desc->name, PSEUDO_TAG_SEPARATOR, parserName,
			      OPT(fileName), OPT(pattern),
				  xsep, fieldx, fsep, xptag)
		: mio_printf (mio, "%s%s\t%s\t/%s/%s%s%s%s\n",
			      PSEUDO_TAG_PREFIX, desc->name,
			      OPT(fileName), OPT(pattern),
			      xsep, fieldx, fsep, xptag);
#undef OPT
}

static fieldType fixedFields [] = {
	FIELD_NAME,
	FIELD_INPUT_FILE,
	FIELD_PATTERN,
};

static bool treatFieldAsFixed (int fieldType)
{
	for (int i = 0; i < ARRAY_SIZE(fixedFields); i++)
		if (fixedFields [i] == fieldType)
			return true;
	return false;
}

static void checkCtagsOptions (tagWriter *writer CTAGS_ATTR_UNUSED,
							   bool fieldsWereReset)
{
	if (isFieldEnabled (FIELD_KIND_KEY)
		&& (!(isFieldEnabled (FIELD_KIND_LONG) ||
			  isFieldEnabled (FIELD_KIND))))
	{
		error (WARNING, "though %c/%s field is enabled, neither %c nor %c field is not enabled",
			   getFieldLetter (FIELD_KIND_KEY),
			   getFieldName (FIELD_KIND_KEY),
			   getFieldLetter (FIELD_KIND),
			   getFieldLetter (FIELD_KIND_LONG));
		error (WARNING, "enable the %c field to make the %c/%s field printable",
			   getFieldLetter (FIELD_KIND_LONG),
			   getFieldLetter (FIELD_KIND_KEY),
			   getFieldName (FIELD_KIND_KEY));
		enableField (FIELD_KIND_LONG, true);
	}
	if (isFieldEnabled (FIELD_SCOPE_KEY)
		&& !isFieldEnabled (FIELD_SCOPE))
	{
		error (WARNING, "though %c/%s field is enabled, %c field is not enabled",
			   getFieldLetter (FIELD_SCOPE_KEY),
			   getFieldName (FIELD_SCOPE_KEY),
			   getFieldLetter (FIELD_SCOPE));
		error (WARNING, "enable the %c field to make the %c/%s field printable",
			   getFieldLetter (FIELD_SCOPE),
			   getFieldLetter (FIELD_SCOPE_KEY),
			   getFieldName (FIELD_SCOPE_KEY));
		enableField (FIELD_SCOPE, true);
	}

	for (int i = 0; i < ARRAY_SIZE (fixedFields); i++)
	{
		if (!isFieldEnabled (fixedFields [i]))
		{
			enableField (fixedFields [i], true);

			if (fieldsWereReset)
				continue;

			const char *name = getFieldName (fixedFields [i]);
			unsigned char letter = getFieldLetter (fixedFields [i]);

			if (name && letter != NUL_FIELD_LETTER)
				error(WARNING, "Cannot disable fixed field: '%c'{%s} in ctags output mode",
				      letter, name);
			else if (name)
				error(WARNING, "Cannot disable fixed field: {%s} in ctags output mode",
				      name);
			else if (letter != NUL_FIELD_LETTER)
				error(WARNING, "Cannot disable fixed field: '%c' in ctags output mode",
				      letter);
		}
	}
}
