/*
*   Copyright (c) 2016, Aman Gupta
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/

#include "general.h"  /* must always come first */

#include "debug.h"
#include "entry_p.h"
#include "field_p.h"
#include "mio.h"
#include "options_p.h"
#include "read.h"
#include "routines.h"
#include "ptag_p.h"
#include "writer_p.h"


#include <string.h>

#ifdef HAVE_JANSSON
#include <jansson.h>

#ifndef json_boolean /* compat with jansson < 2.4 */
#define json_boolean(val)      ((val) ? json_true() : json_false())
#endif


static int writeJsonEntry  (tagWriter *writer CTAGS_ATTR_UNUSED,
				MIO * mio, const tagEntryInfo *const tag,
				void *clientData);

static int writeJsonPtagEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
				MIO * mio, const ptagDesc *desc,
				const char *const fileName,
				const char *const pattern,
				const char *const parserName,
				void *clientData);

tagWriter jsonWriter = {
	.writeEntry = writeJsonEntry,
	.writePtagEntry = writeJsonPtagEntry,
	.printPtagByDefault = true,
	.preWriteEntry = NULL,
	.postWriteEntry = NULL,
	.rescanFailedEntry = NULL,
	.treatFieldAsFixed = NULL,
	.defaultFileName = NULL,
};

static const char* escapeFieldValueRaw (const tagEntryInfo * tag, fieldType ftype, int fieldIndex)
{
	const char *v;
	if (doesFieldHaveRenderer(ftype, true))
		v = renderFieldNoEscaping (ftype, tag, fieldIndex);
	else
		v = renderField (ftype, tag, fieldIndex);

	return v;
}

static json_t* escapeFieldValue (const tagEntryInfo * tag, fieldType ftype, bool returnEmptyStringAsNoValue)
{
	const char *str = escapeFieldValueRaw (tag, ftype, NO_PARSER_FIELD);

	if (str)
	{
		unsigned int dt = getFieldDataType(ftype);
		if (dt & FIELDTYPE_STRING)
		{
			if (dt & FIELDTYPE_BOOL && str[0] == '\0')
				return json_false();
			else
				return json_string (str);
		}
		else if (dt & FIELDTYPE_INTEGER)
		{
			long tmp;

			if (strToLong (str, 10, &tmp))
				return json_integer (tmp);
			else
				return NULL;
		}
		else if (dt & FIELDTYPE_BOOL)
		{
			/* TODO: This must be fixed when new boolean field is added.
			   Currently only `file:' field use this. */
			return json_boolean (strcmp ("-", str)); /* "-" -> false */
		}
		AssertNotReached ();
		return NULL;
	}
	else if (returnEmptyStringAsNoValue)
		return json_false();
	else
		return NULL;
}

static void renderExtensionFieldMaybe (int xftype, const tagEntryInfo *const tag, json_t *response)
{
	const char *fname = getFieldName (xftype);

	if (fname && doesFieldHaveRenderer (xftype, false) && isFieldEnabled (xftype) && doesFieldHaveValue (xftype, tag))
	{
		switch (xftype)
		{
		case FIELD_LINE_NUMBER:
			json_object_set_new (response, fname,
					     json_integer (tag->lineNumber));
			break;
		case FIELD_FILE_SCOPE:
			json_object_set_new (response, fname,
					     json_boolean(1));
			break;
		default:
			json_object_set_new (response, fname,
					     escapeFieldValue (tag, xftype, false));
		}
	}
}

static void addParserFields (json_t *response, const tagEntryInfo *const tag)
{
	unsigned int i;

	for (i = 0; i < tag->usedParserFields; i++)
	{
		const tagField *f = getParserFieldForIndex(tag, i);
		fieldType ftype = f->ftype;
		if (! isFieldEnabled (ftype))
			continue;

		unsigned int dt = getFieldDataType (ftype);
		json_t *o;
		if (dt & FIELDTYPE_STRING)
		{
			const char *str = escapeFieldValueRaw (tag, ftype, i);
			if (dt & FIELDTYPE_BOOL && str[0] == '\0')
				o = json_false ();
			else
				o = json_string (str);
		}
		else if (dt & FIELDTYPE_INTEGER)
		{
			/* NOT IMPLEMENTED YET */
			AssertNotReached ();
			o = json_null ();
		}
		else if (dt & FIELDTYPE_BOOL)
			o = json_true ();
		else
		{
			AssertNotReached ();
			o = json_null ();
		}

		json_object_set_new (response, getFieldName (ftype), o);
	}
}

static void addExtensionFields (json_t *response, const tagEntryInfo *const tag)
{
	int k;

	/* FIELD_KIND has no name; getFieldName (FIELD_KIND) returns NULL.
	   FIELD_KIND_LONG does, too.
	   That cannot be changed to keep the compatibility of tags file format.
	   Use FIELD_KIND_KEY instead */
	if (isFieldEnabled (FIELD_KIND) || isFieldEnabled (FIELD_KIND_LONG))
		enableField (FIELD_KIND_KEY, true);

	/* FIELD_SCOPE has no name; getFieldName (FIELD_KIND_KEY) returns NULL.
	   That cannot be changed to keep the compatibility of tags file format.
	   Use FIELD_SCOPE_KEY and FIELD_SCOPE_KIND_LONG instead. */
	if (isFieldEnabled (FIELD_SCOPE))
	{
		enableField (FIELD_SCOPE_KEY, true);
		enableField (FIELD_SCOPE_KIND_LONG, true);
	}

	for (k = FIELD_JSON_LOOP_START; k <= FIELD_BUILTIN_LAST; k++)
		renderExtensionFieldMaybe (k, tag, response);
}

static int writeJsonEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
			       MIO * mio, const tagEntryInfo *const tag,
				   void *clientData CTAGS_ATTR_UNUSED)
{
	int length = 0;
	json_t *response = json_pack ("{ss}", "_type", "tag");

	if (isFieldEnabled (FIELD_NAME))
	{
		json_t *name = json_string (tag->name);
		if (name == NULL)
			goto out;
		json_object_set_new (response, "name", name);
	}
	if (isFieldEnabled (FIELD_INPUT_FILE))
		json_object_set_new (response, "path", json_string (tag->sourceFileName));
	if (isFieldEnabled (FIELD_PATTERN))
	{
		json_t *pat = escapeFieldValue(tag, FIELD_PATTERN, true);
		json_object_set_new (response, "pattern", pat);
	}

	if (includeExtensionFlags ())
	{
		addExtensionFields (response, tag);
		addParserFields (response, tag);
	}

	/* Print nothing if RESPONSE has only "_type" field. */
	if (json_object_size (response) == 1)
		goto out;

	char *buf = json_dumps (response, JSON_PRESERVE_ORDER);
	length = mio_printf (mio, "%s\n", buf);

	free (buf);
 out:
	json_decref (response);

	return length;
}

static int writeJsonPtagEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
			       MIO * mio, const ptagDesc *desc,
			       const char *const fileName,
			       const char *const pattern,
			       const char *const parserName,
				   void *clientData CTAGS_ATTR_UNUSED)
{
#define OPT(X) ((X)?(X):"")
	json_t *response;

	if (parserName)
	{
		response = json_pack ("{ss ss ss ss ss}",
				      "_type", "ptag",
				      "name", desc->name,
				      "parserName", parserName,
				      "path", OPT(fileName),
				      "pattern", OPT(pattern));
	}
	else
	{
		response = json_pack ("{ss ss ss ss}",
				      "_type", "ptag",
				      "name", desc->name,
				      "path", OPT(fileName),
				      "pattern", OPT(pattern));
	}

	char *buf = json_dumps (response, JSON_PRESERVE_ORDER);
	int length = mio_printf (mio, "%s\n", buf);
	free (buf);
	json_decref (response);

	return length;
#undef OPT
}

extern bool ptagMakeJsonOutputVersion (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
									   const void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
			       "0.0",
			       "in development",
			       NULL);
}

#else /* HAVE_JANSSON */

tagWriter jsonWriter = {
	.writeEntry = NULL,
	.writePtagEntry = NULL,
	.preWriteEntry = NULL,
	.postWriteEntry = NULL,
	.defaultFileName = "-",
};

extern bool ptagMakeJsonOutputVersion (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
									   const void *data CTAGS_ATTR_UNUSED)
{
	return false;
}

#endif
