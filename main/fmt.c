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
#include "entry_p.h"
#include "fmt_p.h"
#include "field.h"
#include "field_p.h"
#include "parse.h"
#include "routines.h"
#include <string.h>
#include <errno.h>

typedef union uFmtSpec {
	char *const_str;
	struct {
		fieldType ftype;
		int width;
		char *raw_fmtstr;
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

static bool isParserFieldCompatibleWithFtype (const tagField *pfield, int baseFtype)
{
	do {
		if (pfield->ftype == baseFtype)
			return true;
		baseFtype = nextSiblingField (baseFtype);
	} while (baseFtype != FIELD_UNKNOWN);
	return false;
}

static int printTagField (fmtSpec* fspec, MIO* fp, const tagEntryInfo * tag)
{
	int i;
	int width = fspec->field.width;
	int ftype;
	const char* str = NULL;

	ftype = fspec->field.ftype;

	if (isCommonField (ftype))
		str = renderField (ftype, tag, NO_PARSER_FIELD);
	else
	{
		unsigned int findex;
		const tagField *f;

		for (findex = 0; findex < tag->usedParserFields; findex++)
		{
			f = getParserFieldForIndex(tag, findex);
			if (isParserFieldCompatibleWithFtype (f, ftype))
				break;
		}

		if (findex == tag->usedParserFields)
			str = "";
		else if (isFieldEnabled (f->ftype))
		{
			unsigned int dt = getFieldDataType (f->ftype);
			if (dt & FIELDTYPE_STRING)
			{
				str = renderField (f->ftype, tag, findex);
				if ((dt & FIELDTYPE_BOOL) && str[0] == '\0')
				{
					/* TODO: FIELD_NULL_LETTER_STRING */
					str = "-";
				}
			}
			else if (dt & FIELDTYPE_BOOL)
				str = getFieldName (f->ftype);
			else
			{
				/* Not implemented */
				AssertNotReached ();
				str = "CTAGS INTERNAL BUG!";
			}
		}
	}

	if (str == NULL)
		str = "";

	if (width)
		i = mio_printf (fp, fspec->field.raw_fmtstr, width, str);
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

static fmtElement** queueTagField (fmtElement **last, long width, bool truncation,
								   char field_letter, const char *field_name)
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

	if (!doesFieldHaveRenderer (ftype, false))
	{
		Assert (field_letter != NUL_FIELD_LETTER);
		error (FATAL, "The field cannot be printed in format output: %c", field_letter);
	}

	cur = xMalloc (1, fmtElement);

	cur->spec.field.width = width;
	cur->spec.field.ftype = ftype;

	if (width < 0)
	{
		cur->spec.field.width *= -1;
		cur->spec.field.raw_fmtstr = (truncation? "%-.*s": "%-*s");
	}
	else if (width > 0)
		cur->spec.field.raw_fmtstr = (truncation? "%.*s": "%*s");
	else
		cur->spec.field.raw_fmtstr = NULL;

	enableField (ftype, true);
	if (language == LANG_AUTO)
	{
		fieldType ftype_next = ftype;

		while ((ftype_next = nextSiblingField (ftype_next)) != FIELD_UNKNOWN)
			enableField (ftype_next, true);
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
	bool found_percent = false;
	long column_width;
	const char*  cursor;

	cursor = fmtString;

	for (i = 0; cursor[i] != '\0'; ++i)
	{
		if (found_percent)
		{
			found_percent = false;
			if (cursor[i] == '%')
			{
				if (literal == NULL)
					literal = vStringNew ();
				vStringPut (literal, cursor[i]);
			}
			else
			{
				int justification_right = 1;
				bool truncation = false;
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
				if (cursor [i] == '.')
				{
					truncation = true;
					i++;

					if (cursor [i] == '\0')
						error (FATAL, "unexpectedly terminated just after '.': \"%s\"", fmtString);
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
						error (FATAL | PERROR, "converting failed: %s", vStringValue (width));
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

					last = queueTagField (last, column_width, truncation,
										  NUL_FIELD_LETTER, vStringValue (field_name));

					vStringDelete (field_name);
				}
				else
					last = queueTagField (last, column_width, truncation,
										  cursor[i], NULL);
			}

		}
		else
		{
			if (cursor[i] == '%')
				found_percent = true;
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
