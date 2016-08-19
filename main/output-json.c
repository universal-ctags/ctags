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
#include "ptag.h"

#ifdef HAVE_JANSSON
#include <jansson.h>

#ifndef json_boolean /* compat with jannson < 2.4 */
#define json_boolean(val)      ((val) ? json_true() : json_false())
#endif


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
	const char *fname = getFieldName (xftype);

	if (fname && isFieldRenderable (xftype) && isFieldEnabled (xftype) && doesFieldHaveValue (xftype, tag))
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
	int k;

	/* FIELD_KIND has no name; getFieldName (FIELD_KIND) returns NULL.
	   FIELD_KIND_LONG does, too.
	   That cannot be changed to keep the compatibility of tags file format.
	   Use FIELD_KIND_KEY instead */
	if (isFieldEnabled (FIELD_KIND) || isFieldEnabled (FIELD_KIND_LONG))
		enableField (FIELD_KIND_KEY, TRUE, FALSE);

	/* FIELD_SCOPE has no name; getFieldName (FIELD_KIND_KEY) returns NULL.
	   That cannot be changed to keep the compatibility of tags file format.
	   Use FIELD_SCOPE_KEY and FIELD_SCOPE_KIND_LONG instead. */
	if (isFieldEnabled (FIELD_SCOPE))
	{
		enableField (FIELD_SCOPE_KEY, TRUE, FALSE);
		enableField (FIELD_SCOPE_KIND_LONG, TRUE, FALSE);
	}

	for (k = FIELD_EXTENSION_START; k <= FIELD_BUILTIN_LAST; k++)
		renderExtensionFieldMaybe (k, tag, response);
}

extern int writeJsonEntry (MIO * mio, const tagEntryInfo *const tag, void *data CTAGS_ATTR_UNUSED)
{
	json_t *response = json_pack ("{ss ss ss ss}",
		"_type", "tag",
		"name", tag->name,
		"path", tag->sourceFileName,
		/* --extra=f option can generates a tag with NULL pattern. */
		"pattern", tag->pattern? tag->pattern: ""
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

extern int writeJsonPtagEntry (MIO * mio, const ptagDesc *desc,
			       const char *const fileName,
			       const char *const pattern,
			       const char *const parserName, void *data CTAGS_ATTR_UNUSED)
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

extern boolean ptagMakeJsonOutputVersion (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
			       "0.0",
			       "in development",
			       NULL);
}

#else /* HAVE_JANSSON */
extern int writeJsonEntry (MIO * mio, const tagEntryInfo *const tag, void *data CTAGS_ATTR_UNUSED)
{
	return 0;
}
extern int writeJsonPtagEntry (MIO * mio, const ptagDesc *desc,
			       const char *const fileName,
			       const char *const pattern,
			       const char *const parserName, void *data CTAGS_ATTR_UNUSED)
{
	return 0;
}

extern boolean ptagMakeJsonOutputVersion (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return FALSE;
}

#endif
