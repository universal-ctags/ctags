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

#include "general.h"
#include "debug.h"
#include "fmt.h"
#include "field.h"
#include "options.h"
#include "parse.h"
#include "routines.h"
#include <string.h>
#include <errno.h>

typedef union uFmtSpec {
	char *const_str;
	struct {
		fieldType ftype;
		int width;
	} field;
} fmtSpec;

struct sFmtElement {
	union uFmtSpec spec;
	int (* printer) (fmtSpec*, MIO* fp, const tagEntryInfo *);
	struct sFmtElement *next;
};

static int printLiteral (fmtSpec* fspec, MIO* fp, const tagEntryInfo * tag CTAGS_ATTR_UNUSED)
{
	return mio_puts (fp, fspec->const_str);
}

static boolean isParserFieldCompatibleWithFtype (const tagField *pfield, int baseFtype)
{
	do {
		if (pfield->ftype == baseFtype)
			return TRUE;
		baseFtype = nextSiblingField (baseFtype);
	} while (baseFtype != FIELD_UNKNOWN);
	return FALSE;
}

static int printTagField (fmtSpec* fspec, MIO* fp, const tagEntryInfo * tag)
{
	int i;
	int width = fspec->field.width;
	int ftype;
	const char* str = NULL;

	ftype = fspec->field.ftype;

	if (isCommonField (ftype))
		str = renderFieldEscaped (ftype, tag, NO_PARSER_FIELD);
	else
	{
		unsigned int findex;

		for (findex = 0; findex < tag->usedParserFields; findex++)
		{
			if (isParserFieldCompatibleWithFtype (tag->parserFields + findex, ftype))
				break;
		}

		if (findex == tag->usedParserFields)
			str = "";
		else if (isFieldEnabled (tag->parserFields [findex].ftype))
			str = renderFieldEscaped (tag->parserFields [findex].ftype,
						  tag, findex);
	}

	if (str == NULL)
		str = "";

	if (width < 0)
		i = mio_printf (fp, "%-*s", -1 * width, str);
	else if (width > 0)
		i = mio_printf (fp, "%*s", width, str);
	else
	{
		mio_puts (fp, str);
		i = strlen (str);
	}
	return i;
}

static fmtElement** queueLiteral (fmtElement **last, char *literal)
{
	fmtElement *cur = xMalloc (1, fmtElement);

	cur->spec.const_str = literal;
	cur->printer = printLiteral;
	cur->next = NULL;
	*last = cur;
	return &(cur->next);
}

/* `getLanguageComponentInFieldName' is used as part of the option parameter
   for --_xformat option.

   It splits the value of fullName into a language part and a field name part.
   Here the two parts are combined with `.'.

   If it cannot find a period, it returns LANG_IGNORE and sets
   fullname to *fieldName.

   If lang part if `*', it returns LANG_AUTO and sets the field
   name part to *fieldName.

   Though a period is found but no parser (langType) is found for
   the language parser, this function returns LANG_IGNORE and sets
   NULL to *fieldName.

   A proper parser is found, this function returns langType for the
   parser and sets the field name part to *fieldName. */
static langType getLanguageComponentInFieldName (const char *fullName,
						 const char **fieldName)
{
	const char *tmp;
	langType language;

	tmp = strchr (fullName, '.');
	if (tmp)
	{
		size_t len = tmp - fullName;

		if (len == 1 && fullName[0] == '*')
		{
			language = LANG_AUTO;
			*fieldName = tmp + 1;
		}
		else if (len == 0)
		{
			language = LANG_IGNORE;
			*fieldName = tmp + 1;
		}
		else
		{
			language = getNamedLanguage (fullName, len);
			if (language == LANG_IGNORE)
				*fieldName = NULL;
			else
				*fieldName = tmp + 1;
		}
	}
	else
	{
		language = LANG_IGNORE;
		*fieldName = fullName;
	}
	return language;
}

static fmtElement** queueTagField (fmtElement **last, long width, char field_letter,
				   const char *field_name)
{
	fieldType ftype;
	fmtElement *cur;
	langType language;

	if (field_letter == NUL_FIELD_LETTER)
	{
		const char *f;

		language = getLanguageComponentInFieldName (field_name, &f);
		if (f == NULL)
			error (FATAL, "No suitable parser for field name: %s", field_name);
		ftype = getFieldTypeForNameAndLanguage (f, language);
	}
	else
	{
		language = LANG_IGNORE;
		ftype = getFieldTypeForOption (field_letter);
	}

	if (ftype == FIELD_UNKNOWN)
	{
		if (field_letter == NUL_FIELD_LETTER)
			error (FATAL, "No such field name: %s", field_name);
		else
			error (FATAL, "No such field letter: %c", field_letter);
	}

	if (!isFieldRenderable (ftype))
	{
		Assert (field_letter != NUL_FIELD_LETTER);
		error (FATAL, "The field cannot be printed in format output: %c", field_letter);
	}

	cur = xMalloc (1, fmtElement);

	cur->spec.field.width = width;
	cur->spec.field.ftype = ftype;

	enableField (ftype, TRUE, FALSE);
	if (language == LANG_AUTO)
	{
		fieldType ftype_next = ftype;

		while ((ftype_next = nextSiblingField (ftype_next)) != FIELD_UNKNOWN)
			enableField (ftype_next, TRUE, FALSE);
	}

	cur->printer = printTagField;
	cur->next = NULL;
	*last = cur;
	return &(cur->next);
}

extern fmtElement *fmtNew (const char*  fmtString)
{
	int i;
	vString *literal = NULL;
	fmtElement *code  = NULL;
	fmtElement **last = &code;
	boolean found_percent = FALSE;
	long column_width;
	const char*  cursor;

	cursor = fmtString;

	for (i = 0; cursor[i] != '\0'; ++i)
	{
		if (found_percent)
		{
			found_percent = FALSE;
			if (cursor[i] == '%')
			{
				if (literal == NULL)
					literal = vStringNew ();
				vStringPut (literal, cursor[i]);
			}
			else
			{
				int justification_right = 1;
				vString *width = NULL;
				if (literal)
				{
					char* l = vStringDeleteUnwrap (literal);
					literal = NULL;
					last = queueLiteral (last, l);
				}
				if (cursor [i] == '-')
				{
					justification_right = -1;
					i++;

					if (cursor [i] == '\0')
						error (FATAL, "unexpectedly terminated just after '-': \"%s\"", fmtString);

				}

				while ( '0' <= cursor[i] && cursor[i] <= '9' )
				{
					if (width == NULL)
						width = vStringNew ();
					vStringPut (width, cursor[i]);
					i++;

					if (cursor [i] == '\0')
						error (FATAL, "unexpectedly terminated during parsing column width: \"%s\"", fmtString);
				}

				if (justification_right == -1 && width == NULL)
					error (FATAL, "no column width given after '-': \"%s\"", fmtString);

				column_width = 0;
				if (width)
				{
					if(!strToLong (vStringValue (width), 0, &column_width))
						error (FATAL | PERROR, "coverting failed: %s", vStringValue (width));
					vStringDelete (width);
					width = NULL;
					column_width *= justification_right;
				}

				if (cursor[i] == '{')
				{
					vString *field_name = vStringNew ();

					i++;
					for (; cursor[i] != '}'; i++)
						vStringPut (field_name, cursor[i]);

					last = queueTagField (last, column_width, NUL_FIELD_LETTER,
							      vStringValue (field_name));

					vStringDelete (field_name);
				}
				else
					last = queueTagField (last, column_width, cursor[i], NULL);
			}

		}
		else
		{
			if (cursor[i] == '%')
				found_percent = TRUE;
			else
			{
				if (literal == NULL)
					literal = vStringNew ();

				vStringPut (literal, cursor[i]);
			}
		}
	}
	if (literal)
	{
		char* l = vStringDeleteUnwrap (literal);
		literal = NULL;
		last = queueLiteral (last, l);
	}
	return code;
}

extern int fmtPrint   (fmtElement * fmtelts, MIO* fp, const tagEntryInfo *tag)
{
	fmtElement *f = fmtelts;
	int i = 0;
	while (f)
	{
		i += f->printer (&(f->spec), fp, tag);
		f = f->next;
	}
	return i;
}

extern void fmtDelete  (fmtElement * fmtelts)
{
	fmtElement *f = fmtelts;
	fmtElement *next;

	while (f)
	{
		next = f->next;
		if (f->printer == printLiteral)
		{
			eFree (f->spec.const_str);
			f->spec.const_str = NULL;
		}
		f->next = NULL;
		eFree (f);
		f = next;
	}
}
