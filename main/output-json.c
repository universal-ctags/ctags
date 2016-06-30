/*
*   Copyright (c) 2016, Aman Gupta
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/

#include "general.h"  /* must always come first */

#include "entry.h"
#include "mio.h"
#include "options.h"
#include "output.h"
#include "read.h"

#ifdef HAVE_JANSSON
#include <jansson.h>


#define includeExtensionFlags()         (Option.tagFileFormat > 1)

static json_t* escapeFieldValue (const tagEntryInfo * tag, fieldType ftype)
{
	const char *str = renderFieldEscaped (ftype, tag, NO_PARSER_FIELD);
	if (str)
		return json_string (str);
	else
		return NULL;
}

static void addParserFields (json_t *response, const tagEntryInfo *const tag)
{
	unsigned int i;
	unsigned int ftype;

	for (i = 0; i < tag->usedParserFields; i++)
	{
		ftype = tag->parserFields [i].ftype;
		if (! isFieldEnabled (ftype))
			continue;

		json_object_set_new (response, getFieldName (ftype), json_string (tag->parserFields [i].value));
	}
}

static void addExtensionFields (json_t *response, const tagEntryInfo *const tag)
{
	boolean making_fq_tag =  (doesInputLanguageRequestAutomaticFQTag ()
				  && isXtagEnabled (XTAG_QUALIFIED_TAGS));

	if (tag->kind->name != NULL && (isFieldEnabled (FIELD_KIND_LONG)  ||
		 (isFieldEnabled (FIELD_KIND)  && tag->kind == '\0')))
		json_object_set_new (response, getFieldName (FIELD_KIND_KEY), json_string (tag->kind->name));
	else if (tag->kind != '\0'  && (isFieldEnabled (FIELD_KIND) ||
			(isFieldEnabled (FIELD_KIND_LONG) &&  tag->kind->name == NULL)))
	{
		char str[2] = {tag->kind->letter, '\0'};
		json_object_set_new (response, getFieldName (FIELD_KIND_KEY), json_string (str));
	}

	if (isFieldEnabled (FIELD_LINE_NUMBER) && doesFieldHaveValue (FIELD_LINE_NUMBER, tag))
		json_object_set_new (response, getFieldName (FIELD_LINE_NUMBER), json_integer (tag->lineNumber));

	if (isFieldEnabled (FIELD_LANGUAGE)  &&  doesFieldHaveValue (FIELD_LANGUAGE))
		json_object_set_new (response, getFieldName (FIELD_LANGUAGE), escapeFieldValue (tag, FIELD_LANGUAGE));

	if (isFieldEnabled (FIELD_SCOPE) || making_fq_tag)
	{
		if (isFieldEnabled (FIELD_SCOPE))
			json_object_set_new (response, getFieldName (FIELD_SCOPE_KEY), escapeFieldValue (tag, FIELD_SCOPE_KIND_LONG));
	}

	if (isFieldEnabled (FIELD_TYPE_REF) && doesFieldHaveValue (FIELD_TYPE_REF, tag))
		json_object_set_new (response, getFieldName (FIELD_TYPE_REF), escapeFieldValue (tag, FIELD_TYPE_REF));

	if (isFieldEnabled (FIELD_FILE_SCOPE) &&  doesFieldHaveValue (FIELD_FILE_SCOPE, tag))
		json_object_set_new (response, getFieldName (FIELD_FILE_SCOPE), json_boolean(1));

	if (isFieldEnabled (FIELD_INHERITANCE) && doesFieldHaveValue (FIELD_INHERITANCE))
		json_object_set_new (response, getFieldName (FIELD_INHERITANCE), escapeFieldValue (tag, FIELD_INHERITANCE));

	if (isFieldEnabled (FIELD_ACCESS) && doesFieldHaveValue (FIELD_ACCESS))
		json_object_set_new (response, getFieldName (FIELD_ACCESS), escapeFieldValue (tag, FIELD_ACCESS));

	if (isFieldEnabled (FIELD_IMPLEMENTATION) && doesFieldHaveValue (FIELD_IMPLEMENTATION))
		json_object_set_new (response, getFieldName (FIELD_IMPLEMENTATION), escapeFieldValue (tag, FIELD_IMPLEMENTATION));

	if (isFieldEnabled (FIELD_SIGNATURE) && doesFieldHaveValue (tag, FIELD_SIGNATURE))
		json_object_set_new (response, getFieldName (FIELD_SIGNATURE), escapeFieldValue (tag, FIELD_SIGNATURE));

	if (isFieldEnabled (FIELD_ROLE) && doesFieldHaveValue (tag, FIELD_ROLE))
		json_object_set_new (response, getFieldName (FIELD_ROLE), escapeFieldValue (tag, FIELD_ROLE));

	if (isFieldEnabled (FIELD_EXTRA) && doesFieldHaveValue (tag, FIELD_EXTRA))
		json_object_set_new (response, getFieldName (FIELD_EXTRA), escapeFieldValue (tag, FIELD_EXTRA));

	if (isFieldEnabled(FIELD_XPATH) && doesFieldHaveValue (tag, FIELD_XPATH))
		json_object_set_new (response, getFieldName (FIELD_XPATH), escapeFieldValue (tag, FIELD_XPATH));
}

extern int writeJsonEntry (MIO * mio, const tagEntryInfo *const tag, void *data __unused__)
{
	json_t *response = json_pack ("{ss ss ss ss}",
		"_type", "tag",
		"name", tag->name,
		"path", tag->sourceFileName,
		"pattern", tag->pattern
	);

	if (includeExtensionFlags ())
		addExtensionFields (response, tag);

	addParserFields (response, tag);

	char *buf = json_dumps (response, JSON_SORT_KEYS);
	int length = mio_printf (mio, "%s\n", buf);

	free (buf);
	json_decref (response);

	return length;
}

#else /* HAVE_JANSSON */
extern int writeJsonEntry (MIO * mio, const tagEntryInfo *const tag, void *data __unused__)
{
	return 0;
}
#endif
