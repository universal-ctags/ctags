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

static void renderExtensionFieldMaybe (int xftype, const tagEntryInfo *const tag, json_t *response)
{
	if (isFieldEnabled (xftype) && doesFieldHaveValue (xftype, tag))
	{
		switch (xftype)
		{
		case FIELD_LINE_NUMBER:
			json_object_set_new (response, getFieldName (xftype),
					     json_integer (tag->lineNumber));
			break;
		case FIELD_FILE_SCOPE:
			json_object_set_new (response, getFieldName (xftype),
					     json_boolean(1));
			break;
		default:
			json_object_set_new (response, getFieldName (xftype),
					     escapeFieldValue (tag, xftype));
		}
	}
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

	if (isFieldEnabled (FIELD_SCOPE) || making_fq_tag)
	{
		json_t *k = escapeFieldValue (tag, FIELD_SCOPE_KIND_LONG);

		/* The value must be generated even if FIELD_SCOPE is disabled for
		   generating FQN. */
		if (isFieldEnabled (FIELD_SCOPE))
			json_object_set_new (response, getFieldName (FIELD_SCOPE_KEY), k);
	}

	int field_keys [] = {
		FIELD_ACCESS,
		FIELD_INHERITANCE,
		FIELD_LANGUAGE,
		FIELD_IMPLEMENTATION,
		FIELD_LINE_NUMBER,
		FIELD_SIGNATURE,
		FIELD_SCOPE,
		FIELD_TYPE_REF,
		FIELD_ROLE,
		FIELD_EXTRA,
		FIELD_XPATH,
		FIELD_UNKNOWN,
	};
	int *k;
	for (k = field_keys; *k != FIELD_UNKNOWN; k++)
		renderExtensionFieldMaybe (*k, tag, response);
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
	{
		addExtensionFields (response, tag);
		addParserFields (response, tag);
	}

	char *buf = json_dumps (response, JSON_PRESERVE_ORDER);
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
