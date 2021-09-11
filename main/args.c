/*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for reading command line arguments.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "args_p.h"
#include "debug.h"
#include "routines.h"
#include "vstring.h"

/*
*   FUNCTION DEFINITIONS
*/

static char *nextStringArg (const char** const next)
{
	char* result = NULL;
	const char* start;

	Assert (*next != NULL);
	for (start = *next  ;  isspace ((int) *start)  ;  ++start)
		;
	if (*start == '\0')
		*next = start;
	else
	{
		size_t length;
		const char* end;

		for (end = start ;  *end != '\0'  &&  ! isspace ((int) *end)  ;  ++end)
			;
		length = end - start;
		Assert (length > 0);
		result = xMalloc (length + 1, char);
		strncpy (result, start, length);
		result [length] = '\0';
		*next = end;
	}
	return result;
}

static char* nextStringLine (const char** const next)
{
	char* result = NULL;
	size_t length;
	const char* end;

	Assert (*next != NULL);
	for (end = *next ;  *end != '\n'  &&  *end != '\0' ;  ++end)
		;
	length = end - *next;
	if (length > 0)
	{
		result = xMalloc (length + 1, char);
		strncpy (result, *next, length);
		result [length] = '\0';
	}
	if (*end == '\n')
		++end;
	else if (*end == '\r')
	{
		++end;
		if (*end == '\n')
			++end;
	}
	*next = end;
	return result;
}

static char* nextString (const Arguments* const current, const char** const next)
{
	char* result;
	if (current->lineMode)
		result = nextStringLine (next);
	else
		result = nextStringArg (next);
	return result;
}

static char* nextFileArg (FILE* const fp)
{
	char* result = NULL;
	Assert (fp != NULL);
	if (! feof (fp))
	{
		vString* vs = vStringNew ();
		int c;
		do
			c = fgetc (fp);
		while (isspace (c));

		if (c != EOF)
		{
			do
			{
				vStringPut (vs, c);
				c = fgetc (fp);
			} while (c != EOF  &&  ! isspace (c));
			Assert (vStringLength (vs) > 0);
			result = xMalloc (vStringLength (vs) + 1, char);
			strcpy (result, vStringValue (vs));
		}
		vStringDelete (vs);
	}
	return result;
}

static char* nextFileLine (FILE* const fp)
{
	char* result = NULL;
	Assert (fp != NULL);
	if (! feof (fp))
	{
		vString* vs = vStringNew ();
		int c;

		c = fgetc (fp);
		while (c != EOF)
		{
			if (c != '\n'  &&  c != '\r')
				vStringPut (vs, c);
			else if (vStringLength (vs) > 0)
				break;
			c = fgetc (fp);
		}
		if (c != EOF  ||  vStringLength (vs) > 0)
		{
			if (c == '\r')
			{
				c = fgetc (fp);
				if (c != '\n')
					c = ungetc (c, fp);
			}
			vStringStripTrailing (vs);
			vStringStripLeading (vs);
			result = xMalloc (vStringLength (vs) + 1, char);
			strcpy (result, vStringValue (vs));
		}
		vStringDelete (vs);
	}
	return result;
}

static bool isCommentLine (char* line)
{
	while (isspace(*line))
		++line;
	return (*line == '#');
}

static bool isOptscriptLine (char *line)
{
	size_t len = strlen (line);
	if (len < 2)
		return false;
	if (line [len - 1] == '{' && line [len - 2] == '{')
		return true;
	return false;
}

static char* nextOptscriptLines (FILE* const fp, char *line)
{
	vString *vstr = vStringNewInit (line);
	vStringPut (vstr, '\n');
	eFree (line);

	/* \n}}, \n=>1, }=>2, }=>3  */
	int endMarkers = 0;
	int c;
	while (true)
	{
		c = fgetc (fp);
		if (c == EOF)
			break;

		if (c == '\r' || c == '\n')
		{
			if (c == '\r')
			{
				c = fgetc (fp);
				if (c != '\n')
				{
					ungetc(c, fp);
					c = '\n';
				}
			}
			if (c == '\n')
			{
				vStringPut (vstr, c);
				if (endMarkers != 1)
					endMarkers = 1;
			}
		}
		else if (c == '}')
		{
			vStringPut (vstr, c);
			if (endMarkers == 1 || endMarkers == 2)
				endMarkers++;
			if (endMarkers == 3)
				break;
		}
		else
		{
			endMarkers = 0;
			vStringPut (vstr, c);
		}
	}

	if (c == EOF)
	{
		switch (endMarkers)
		{
		case 0:
			vStringPut (vstr, '\n');
			/* Fall through */
		case 1:
			vStringPut (vstr, '}');
			/* Fall through */
		case 2:
			vStringPut (vstr, '}');
		default:
			break;
		}
	}

	c = fgetc (fp);
	while (c != EOF)
	{
		if (c == '\n')
			break;
		if (c == '\r')
		{
			c = fgetc (fp);
			if (c == '\n')
				break;
			ungetc (c, fp);
		}
		c = fgetc (fp);
	}
	return vStringDeleteUnwrap (vstr);
}

static char* nextFileLineSkippingComments (FILE* const fp)
{
	char* result;
	bool comment;
	bool optscript;

	do
	{
		result = nextFileLine (fp);
		comment = false;
		optscript = false;
		if (result)
		{
			comment = isCommentLine (result);
			optscript = isOptscriptLine (result);
		}
		if (comment)
			eFree (result);
		else if (optscript)
			result = nextOptscriptLines (fp, result);
	} while (comment);

	return result;
}

static char* nextFileString (const Arguments* const current, FILE* const fp)
{
	char* result;
	if (current->lineMode)
		result = nextFileLineSkippingComments (fp);
	else
		result = nextFileArg (fp);
	return result;
}

extern Arguments* argNewFromString (const char* const string)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_STRING;
	result->u.stringArgs.next = string;
	result->item = nextString (result, &result->u.stringArgs.next);
	return result;
}

extern Arguments* argNewFromArgv (char* const* const argv)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_ARGV;
	result->u.argvArgs.argv = argv;
	result->u.argvArgs.item = result->u.argvArgs.argv;
	result->item = *result->u.argvArgs.item;
	return result;
}

extern Arguments* argNewFromFile (FILE* const fp)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_FILE;
	result->u.fileArgs.fp = fp;
	result->item = nextFileString (result, result->u.fileArgs.fp);
	return result;
}

extern Arguments* argNewFromLineFile (FILE* const fp)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_FILE;
	result->lineMode = true;
	result->u.fileArgs.fp = fp;
	result->item = nextFileString (result, result->u.fileArgs.fp);
	return result;
}

extern char *argItem (const Arguments* const current)
{
	Assert (current != NULL);
	Assert (! argOff (current));
	return current->item;
}

extern bool argOff (const Arguments* const current)
{
	Assert (current != NULL);
	return (bool) (current->item == NULL);
}

extern void argSetWordMode (Arguments* const current)
{
	Assert (current != NULL);
	current->lineMode = false;
}

extern void argSetLineMode (Arguments* const current)
{
	Assert (current != NULL);
	current->lineMode = true;
}

extern void argForth (Arguments* const current)
{
	Assert (current != NULL);
	Assert (! argOff (current));
	switch (current->type)
	{
		case ARG_STRING:
			if (current->item != NULL)
				eFree (current->item);
			current->item = nextString (current, &current->u.stringArgs.next);
			break;
		case ARG_ARGV:
			++current->u.argvArgs.item;
			current->item = *current->u.argvArgs.item;
			break;
		case ARG_FILE:
			if (current->item != NULL)
				eFree (current->item);
			current->item = nextFileString (current, current->u.fileArgs.fp);
			break;
		default:
			Assert ("Invalid argument type" == NULL);
			break;
	}
}

extern void argDelete (Arguments* const current)
{
	Assert (current != NULL);
	if ((current->type ==  ARG_STRING
		 || current->type ==  ARG_FILE) &&  current->item != NULL)
		eFree (current->item);
	memset (current, 0, sizeof (Arguments));
	eFree (current);
}
