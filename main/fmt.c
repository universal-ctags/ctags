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
#include "fmt.h"
#include "field.h"
#include "routines.h"
#include <string.h>
#include <errno.h>

typedef union uFmtSpec {
	char *const_str;
	struct {
		fieldDesc *desc;
		int width;
	} field;
} fmtSpec;

struct sFmtElement {
	union uFmtSpec spec;
	int (* printer) (fmtSpec*, FILE* fp, const tagEntryInfo *);
	struct sFmtElement *next;
};

static int printLiteral (fmtSpec* fspec, FILE* fp, const tagEntryInfo * tag __unused__)
{
	return fputs (fspec->const_str, fp);
}

static int printTagField (fmtSpec* fspec, FILE* fp, const tagEntryInfo * tag)
{
	int i;
	int width = fspec->field.width;
	const char* str = renderFieldEscaped (fspec->field.desc, tag);

	if (width < 0)
		i = fprintf (fp, "%-*s", -1 * width, str);
	else if (width > 0)
		i = fprintf (fp, "%*s", width, str);
	else
	{
		fputs (str, fp);
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

static fmtElement** queueTagField (fmtElement **last, long width, char field_letter)
{
	fieldType ftype;
	fieldDesc* fdesc;

	fmtElement *cur;

	ftype = getFieldTypeForOption (field_letter);
	if (ftype == FIELD_UNKNOWN)
		error (FATAL, "No such field letter: %c", field_letter);

	fdesc = getFieldDesc (ftype);
	if (fdesc->renderEscaped == NULL)
		error (FATAL, "The field cannot be printed in format output: %c", field_letter);

	cur = xMalloc (1, fmtElement);

	cur->spec.field.width = width;
	cur->spec.field.desc  = fdesc;

	fdesc->enabled   = TRUE;	/* TODO */

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
					errno = 0;
					column_width = strtol (vStringValue (width), NULL, 0);
					if (errno != 0)
						error (FATAL | PERROR, "coverting failed: %s", vStringValue (width));
					vStringDelete (width);
					width = NULL;
					column_width *= justification_right;
				}

				last = queueTagField (last, column_width, cursor[i]);
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

extern int fmtPrint   (fmtElement * fmtelts, FILE* fp, const tagEntryInfo *tag)
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
