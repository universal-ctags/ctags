/*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions managing resizable string lists.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <fnmatch.h>

#include "debug.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"

/*
*   FUNCTION DEFINITIONS
*/

extern stringList *stringListNew (void)
{
	return ptrArrayNew ((ptrArrayDeleteFunc)vStringDelete);
}

extern void stringListAdd (stringList *const current, vString *string)
{
	ptrArrayAdd (current, string);
}

extern void stringListRemoveLast (stringList *const current)
{
	ptrArrayRemoveLast (current);
}

/* Combine list `from' into `current', deleting `from' */
extern void stringListCombine (
		stringList *const current, stringList *const from)
{
	ptrArrayCombine (current, from);
}

extern stringList* stringListNewFromArgv (const char* const* const argv)
{
	stringList* const result = stringListNew ();
	const char *const *p;
	Assert (argv != NULL);
	for (p = argv  ;  *p != NULL  ;  ++p)
		stringListAdd (result, vStringNewInit (*p));
	return result;
}

extern stringList* stringListNewFromFile (const char* const fileName)
{
	stringList* result = NULL;
	MIO* const mio = mio_new_file (fileName, "r");
	if (mio != NULL)
	{
		result = stringListNew ();
		while (! mio_eof (mio))
		{
			vString* const str = vStringNew ();
			readLineRaw (str, mio);
			vStringStripTrailing (str);
			if (vStringLength (str) > 0)
				stringListAdd (result, str);
			else
				vStringDelete (str);
		}
		mio_unref (mio);
	}
	return result;
}

extern unsigned int stringListCount (const stringList *const current)
{
	return ptrArrayCount (current);
}

extern vString* stringListItem (
		const stringList *const current, const unsigned int indx)
{
	return ptrArrayItem (current, indx);
}

extern vString* stringListLast (const stringList *const current)
{
	return ptrArrayLast (current);
}

extern void stringListClear (stringList *const current)
{
	ptrArrayClear (current);
}

extern void stringListDelete (stringList *const current)
{
	ptrArrayDelete (current);
}

static bool compareString (
		const char *const string, vString *const itm)
{
	return (strcmp (string, vStringValue (itm)) == 0);
}

static bool compareStringInsensitive (
		const char *const string, vString *const itm)
{
	return (strcasecmp (string, vStringValue (itm)) == 0);
}

static int stringListIndex (
		const stringList *const current,
		const char *const string,
		bool (*test)(const char *s, vString *const vs))
{
	int result = -1;
	unsigned int i;
	Assert (current != NULL);
	Assert (string != NULL);
	Assert (test != NULL);
	for (i = 0  ;  result == -1  &&  i < ptrArrayCount (current)  ;  ++i)
		if ((*test)(string, ptrArrayItem (current, i)))
			result = i;
	return result;
}

extern bool stringListHas (
		const stringList *const current, const char *const string)
{
	bool result;
	Assert (current != NULL);
	result = stringListIndex (current, string, compareString) != -1;
	return result;
}

static vString* stringListFinds (
		const stringList *const current, const char *const string,
		bool (*test)(const char *s, vString *const vs))
{
	int i;

	Assert (current != NULL);
	Assert (string != NULL);

	i = stringListIndex (current, string, test);
	if (i == -1)
		return NULL;
	else
		return stringListItem(current, i);
}

extern bool stringListHasInsensitive (
		const stringList *const current, const char *const string)
{
	bool result;
	Assert (current != NULL);
	Assert (string != NULL);
	result = stringListIndex (current, string, compareStringInsensitive) != -1;
	return result;
}

extern bool stringListHasTest (const stringList *const current,
				  bool (*test)(const char *s, void *userData),
				  void *userData)
{
	bool result = false;
	unsigned int i;
	Assert (current != NULL);
	for (i = 0  ;  ! result  &&  i < ptrArrayCount (current)  ;  ++i)
		result = (*test)(vStringValue ((vString *)ptrArrayItem (current, i)), userData);
	return result;
}

extern bool stringListDeleteItemExtension (stringList* const current, const char* const extension)
{
	int where;
#ifdef CASE_INSENSITIVE_FILENAMES
	where = stringListIndex (current, extension, compareStringInsensitive);
#else
	where = stringListIndex (current, extension, compareString);
#endif
	if (where != -1)
		ptrArrayDeleteItem (current, where);
	return where != -1;
}

extern bool stringListExtensionMatched (
		const stringList* const current, const char* const extension)
{
#ifdef CASE_INSENSITIVE_FILENAMES
	return stringListHasInsensitive (current, extension);
#else
	return stringListHas (current, extension);
#endif
}

extern vString* stringListExtensionFinds (
		const stringList* const current, const char* const extension)
{
#ifdef CASE_INSENSITIVE_FILENAMES
	return stringListFinds (current, extension, compareStringInsensitive);
#else
	return stringListFinds (current, extension, compareString);
#endif
}

extern bool stringListCaseMatched (const stringList* const list, const char* const str)
{
	return stringListCaseFinds(list, str)? true: false;
}

extern vString* stringListCaseFinds (const stringList* const list, const char* const str)
{
	return stringListFinds (list, str, compareStringInsensitive);
}

static bool fileNameMatched (
		const vString* const vpattern, const char* const fileName)
{
	const char* const pattern = vStringValue (vpattern);

#ifdef CASE_INSENSITIVE_FILENAMES
	{
		char* const p = newUpperString (pattern);
		char* const f = newUpperString (fileName);
		bool r = (fnmatch (p, f, 0) == 0);
		eFree (f);
		eFree (p);
		return r;
	}
#else
	return (fnmatch (pattern, fileName, 0) == 0);
#endif
}

extern bool stringListFileMatched (
			const stringList* const current, const char* const fileName)
{
	return stringListFileFinds (current, fileName)? true: false;
}

extern vString* stringListFileFinds (
		const stringList* const current, const char* const fileName)
{
	vString* vstr = NULL;
	bool matched = false;
	unsigned int i;
	const char * normalized = fileName;

#if defined (WIN32)
	vString *tmp = vStringNewInit (fileName);
	vStringTranslate (tmp, PATH_SEPARATOR, OUTPUT_PATH_SEPARATOR);
	normalized = vStringValue (tmp);
#endif

	for (i = 0  ;  ! matched  &&  i < stringListCount (current)  ;  ++i)
	{
		vstr = stringListItem (current, i);
		matched = fileNameMatched (vstr, normalized);
	}

#if defined (WIN32)
	vStringDelete (tmp);
#endif

	return matched? vstr: NULL;
}

extern void stringListPrint (const stringList *const current, FILE *fp)
{
	unsigned int i;
	Assert (current != NULL);
	for (i = 0  ;  i < ptrArrayCount (current)  ;  ++i)
		fprintf (fp, "%s%s", (i > 0) ? ", " : "", vStringValue ((vString *)ptrArrayItem (current, i)));
}

extern void stringListReverse (const stringList *const current)
{
	ptrArrayReverse (current);
}
