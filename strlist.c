/*
*   $Id$
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions managing resizable string lists.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>
#ifdef HAVE_FNMATCH_H
# include <fnmatch.h>
#endif

#include "debug.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"

/*
*   FUNCTION DEFINITIONS
*/

extern stringList *stringListNew (void)
{
    stringList* const result = xMalloc (1, stringList);
    result->max   = 0;
    result->count = 0;
    result->list  = NULL;
    return result;
}

extern void stringListAdd (stringList *const current, vString *string)
{
    enum { incrementalIncrease = 10 };
    Assert (current != NULL);
    if (current->list == NULL)
    {
	Assert (current->max == 0);
	current->count = 0;
	current->max   = incrementalIncrease;
	current->list  = xMalloc (current->max, vString*);
    }
    else if (current->count == current->max)
    {
	current->max += incrementalIncrease;
	current->list = xRealloc (current->list, current->max, vString*);
    }
    current->list [current->count++] = string;
}

/* Combine list `from' into `current', deleting `from' */
extern void stringListCombine (stringList *const current, stringList *const from)
{
    unsigned int i;
    Assert (current != NULL);
    Assert (from != NULL);
    for (i = 0  ;  i < from->count  ;  ++i)
    {
	stringListAdd (current, from->list [i]);
	from->list [i] = NULL;
    }
    stringListDelete (from);
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
    FILE* const fp = fopen (fileName, "r");
    if (fp != NULL)
    {
	result = stringListNew ();
	while (! feof (fp))
	{
	    vString* const str = vStringNew ();
	    readLine (str, fp);
	    vStringStripTrailing (str);
	    if (vStringLength (str) > 0)
		stringListAdd (result, str);
	    else
		vStringDelete (str);
	}
    }
    return result;
}

extern unsigned int stringListCount (const stringList *const current)
{
    Assert (current != NULL);
    return current->count;
}

extern vString* stringListItem (const stringList *const current,
				const unsigned int indx)
{
    Assert (current != NULL);
    return current->list [indx];
}

extern void stringListClear (stringList *const current)
{
    unsigned int i;
    Assert (current != NULL);
    for (i = 0  ;  i < current->count  ;  ++i)
    {
	vStringDelete (current->list [i]);
	current->list [i] = NULL;
    }
    current->count = 0;
}

extern void stringListDelete (stringList *const current)
{
    if (current != NULL)
    {
	if (current->list != NULL)
	{
	    stringListClear (current);
	    eFree (current->list);
	    current->list = NULL;
	}
	current->max   = 0;
	current->count = 0;
	eFree (current);
    }
}

extern boolean stringListHas (const stringList *const current,
			      const char *const str)
{
    boolean result = FALSE;
    unsigned int i;
    Assert (current != NULL);
    for (i = 0  ;  ! result  &&  i < current->count  ;  ++i)
	result = (boolean) (strcmp (str, vStringValue (current->list [i]))==0);
    return result;
}

extern boolean stringListHasInsensitive (const stringList *const current,
					 const char *const str)
{
    boolean result = FALSE;
    unsigned int i;
    Assert (current != NULL);
    for (i = 0  ;  ! result  &&  i < current->count  ;  ++i)
	result = (boolean) (strcasecmp (str, vStringValue (current->list [i]))==0);
    return result;
}

extern boolean stringListHasTest (const stringList *const current,
				  boolean (*test)(const char *s))
{
    boolean result = FALSE;
    unsigned int i;
    Assert (current != NULL);
    for (i = 0  ;  ! result  &&  i < current->count  ;  ++i)
	result = (*test)(vStringValue (current->list [i]));
    return result;
}

extern boolean stringListExtensionMatched (const stringList* const list,
					   const char* const extension)
{
#ifdef CASE_INSENSITIVE_FILENAMES
    return stringListHasInsensitive (list, extension);
#else
    return stringListHas (list, extension);
#endif
}

static boolean fileNameMatched (const vString* const vpattern,
				const char* const fileName)
{
    const char* const pattern = vStringValue (vpattern);
#if defined (HAVE_FNMATCH)
    return (boolean) (fnmatch (pattern, fileName, 0) == 0);
#elif defined (CASE_INSENSITIVE_FILENAMES)
    return (boolean) (strcasecmp (pattern, fileName) == 0);
#else
    return (boolean) (strcmp (pattern, fileName) == 0);
#endif
}

extern boolean stringListFileMatched (const stringList* const list,
				      const char* const fileName)
{
    boolean result = FALSE;
    unsigned int i;
    for (i = 0  ;  ! result  &&  i < stringListCount (list)  ;  ++i)
	result = fileNameMatched (stringListItem (list, i), fileName);
    return result;
}

extern void stringListPrint (const stringList *const current)
{
    unsigned int i;
    Assert (current != NULL);
    for (i = 0  ;  i < current->count  ;  ++i)
	printf ("%s%s", (i > 0) ? ", " : "", vStringValue (current->list [i]));
}

/* vi:set tabstop=8 shiftwidth=4: */
