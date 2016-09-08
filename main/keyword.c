/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Manages a keyword hash.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#include "debug.h"
#include "keyword.h"
#include "options.h"
#include "routines.h"

/*
*   DATA DECLARATIONS
*/
typedef struct sHashEntry {
	struct sHashEntry *next;
	const char *string;
	langType language;
	int value;
} hashEntry;

/*
*   DATA DEFINITIONS
*/
static const unsigned int TableSize = 2039;  /* prime */
static hashEntry **HashTable = NULL;

/*
*   FUNCTION DEFINITIONS
*/

static hashEntry **getHashTable (void)
{
	static boolean allocated = FALSE;

	if (! allocated)
	{
		unsigned int i;

		HashTable = xMalloc (TableSize, hashEntry*);

		for (i = 0  ;  i < TableSize  ;  ++i)
			HashTable [i] = NULL;

		allocated = TRUE;
	}
	return HashTable;
}

static hashEntry *getHashTableEntry (unsigned long hashedValue)
{
	hashEntry **const table = getHashTable ();
	hashEntry *entry;

	Assert (hashedValue < TableSize);
	entry = table [hashedValue];

	return entry;
}

static unsigned int hashValue (const char *const string, langType language)
{
	const signed char *p;
	unsigned int h = 5381;

	Assert (string != NULL);

	/* "djb" hash as used in g_str_hash() in glib */
	for (p = (const signed char *)string; *p != '\0'; p++)
		h = (h << 5) + h + tolower (*p);

	/* consider language as an extra "character" and add it to the hash */
	h = (h << 5) + h + language;

	return h;
}

static hashEntry *newEntry (
		const char *const string, langType language, int value)
{
	hashEntry *const entry = xMalloc (1, hashEntry);

	entry->next     = NULL;
	entry->string   = string;
	entry->language = language;
	entry->value    = value;

	return entry;
}

/*  Note that it is assumed that a "value" of zero means an undefined keyword
 *  and clients of this function should observe this. Also, all keywords added
 *  should be added in lower case. If we encounter a case-sensitive language
 *  whose keywords are in upper case, we will need to redesign this.
 */
extern void addKeyword (const char *const string, langType language, int value)
{
	const unsigned int index = hashValue (string, language) % TableSize;
	hashEntry *entry = getHashTableEntry (index);

	if (entry == NULL)
	{
		hashEntry **const table = getHashTable ();
		table [index] = newEntry (string, language, value);
	}
	else
	{
		hashEntry *prev = NULL;

		while (entry != NULL)
		{
			if (language == entry->language  &&
				strcmp (string, entry->string) == 0)
			{
				Assert (("Already in table" == NULL));
			}
			prev = entry;
			entry = entry->next;
		}
		if (entry == NULL)
		{
			Assert (prev != NULL);
			prev->next = newEntry (string, language, value);
		}
	}
}

static int lookupKeywordFull (const char *const string, boolean caseSensitive, langType language)
{
	const unsigned int index = hashValue (string, language) % TableSize;
	hashEntry *entry = getHashTableEntry (index);
	int result = KEYWORD_NONE;

	while (entry != NULL)
	{
		if (language == entry->language &&
			((caseSensitive && strcmp (string, entry->string) == 0) ||
			 (!caseSensitive && strcasecmp (string, entry->string) == 0)))
		{
			result = entry->value;
			break;
		}
		entry = entry->next;
	}
	return result;
}

extern int lookupKeyword (const char *const string, langType language)
{
	return lookupKeywordFull (string, TRUE, language);
}

extern int lookupCaseKeyword (const char *const string, langType language)
{
	return lookupKeywordFull (string, FALSE, language);
}

extern void freeKeywordTable (void)
{
	if (HashTable != NULL)
	{
		unsigned int i;

		for (i = 0  ;  i < TableSize  ;  ++i)
		{
			hashEntry *entry = HashTable [i];

			while (entry != NULL)
			{
				hashEntry *next = entry->next;
				eFree (entry);
				entry = next;
			}
		}
		eFree (HashTable);
	}
}

#ifdef DEBUG

static void printEntry (const hashEntry *const entry)
{
	printf ("  %-15s %-7s\n", entry->string, getLanguageName (entry->language));
}

static unsigned int printBucket (const unsigned int i)
{
	hashEntry **const table = getHashTable ();
	hashEntry *entry = table [i];
	unsigned int measure = 1;
	boolean first = TRUE;

	printf ("%2d:", i);
	if (entry == NULL)
		printf ("\n");
	else while (entry != NULL)
	{
		if (! first)
			printf ("    ");
		else
		{
			printf (" ");
			first = FALSE;
		}
		printEntry (entry);
		entry = entry->next;
		measure = 2 * measure;
	}
	return measure - 1;
}

extern void printKeywordTable (void)
{
	unsigned long emptyBucketCount = 0;
	unsigned long measure = 0;
	unsigned int i;

	for (i = 0  ;  i < TableSize  ;  ++i)
	{
		const unsigned int pass = printBucket (i);

		measure += pass;
		if (pass == 0)
			++emptyBucketCount;
	}

	printf ("spread measure = %ld\n", measure);
	printf ("%ld empty buckets\n", emptyBucketCount);
}

#endif
