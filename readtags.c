/*
*   $Id$
*
*   Copyright (c) 1996-2001, Darren Hiebert
*
*   This source code is released into the public domain.
*
*   This module contains functions for reading tag files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>	/* declare off_t */
#endif
#include <stdio.h>
#include "readtags.h"

/*
*   MACROS
*/
#define TAB '\t'

#define nameComparison(nm)  (strcmp ((nm), File.name.buffer))

/*
*   DATA DECLARATIONS
*/
typedef struct {
    size_t size;
    char *buffer;
} vstring;

/*
*   DATA DEFINITIONS
*/
const char* const EmptyString = "";
const char* const PseudoTagPrefix = "!_";
const char* TagFileName = "tags";

/* Information about current tag file */
static struct {
	/* has the file been opened and this structure initialized? */
    short initialized;
	/* is the tag file sorted? */
    short sorted;
	/* pointer to file structure */
    FILE* fp;
	/* file position of first character of `line' */
    off_t pos;
	/* size of tag file in seekable positions */
    off_t size;
	/* last line read */
    vstring line;
	/* name of tag in last line read */
    vstring name;
    struct {
		/* file position of last match for tag */
	    off_t pos; 
		/* name of tag last searched for */
	    const char *name;
    } find;
	/* miscellaneous extension fields */
    struct {
		/* number of entries in `list' */
	    unsigned short max;
		/* list of key value pairs */
	    tagExtensionField *list;
    } fields;
} File = {
    0, 0,
    NULL, 0, 0,
    { 0, NULL },	/* line */
    { 0, NULL },	/* name */
    { 0, NULL },	/* find */
    { 0, NULL }		/* fields */
};

/*
*   FUNCTION DEFINITIONS
*/

static int growString (vstring *s)
{
    int result = 0;
    size_t newLength;
    char *newLine;
    if (s->size == 0)
    {
	newLength = 128;
	newLine = (char*) malloc (newLength);
	*newLine = '\0';
    }
    else
    {
	newLength = 2 * s->size;
	newLine = (char*) realloc (s->buffer, newLength);
    }
    if (newLine == NULL)
	perror ("string too large");
    else
    {
	s->buffer = newLine;
	s->size = newLength;
	result = 1;
    }
    return result;
}

static void copyName (void)
{
    size_t length;
    const char *end = strchr (File.line.buffer, '\t');
    if (end == NULL)
    {
	end = strchr (File.line.buffer, '\n');
	if (end == NULL)
	    end = strchr (File.line.buffer, '\r');
    }
    if (end != NULL)
	length = end - File.line.buffer;
    else
	length = strlen (File.line.buffer);
    while (length >= File.name.size)
	growString (&File.name);
    strncpy (File.name.buffer, File.line.buffer, length);
    File.name.buffer [length] = '\0';
}

static int readTagLineRaw (void)
{
    int result = 1;
    int reReadLine;

    /*  If reading the line places any character other than a null or a
     *  newline at the last character position in the buffer (one less than
     *  the buffer size), then we must resize the buffer and reattempt to read
     *  the line.
     */
    do
    {
	char *const pLastChar = File.line.buffer + File.line.size - 2;
	char *line;

	File.pos = ftell (File.fp);
	reReadLine = 0;
	*pLastChar = '\0';
	line = fgets (File.line.buffer, (int) File.line.size, File.fp);
	if (line == NULL)
	{
	    /* read error */
	    if (! feof (File.fp))
		perror ("readTagLine");
	    result = 0;
	}
	else if (*pLastChar != '\0'  &&
		    *pLastChar != '\n'  &&  *pLastChar != '\r')
	{
	    /*  buffer overflow */
	    growString (&File.line);
	    fseek (File.fp, File.pos, SEEK_SET);
	    reReadLine = 1;
	}
	else
	{
	    size_t i = strlen (File.line.buffer);
	    while (i > 0  &&
		   (File.line.buffer [i - 1] == '\n' || File.line.buffer [i - 1] == '\r'))
	    {
		File.line.buffer [i - 1] = '\0';
		--i;
	    }
	}
    } while (reReadLine  &&  result);
    if (result)
	copyName ();
    return result;
}

static int readTagLine (void)
{
    int result;
    do
    {
	result = readTagLineRaw ();
    } while (result && *File.name.buffer == '\0');
    return result;
}

static tagResult growFields (void)
{
    tagResult result = TagFailure;
    size_t newCount = 2 * File.fields.max;
    tagExtensionField *newFields = (tagExtensionField*)
	    realloc (File.fields.list, newCount * sizeof (tagExtensionField));
    if (newFields == NULL)
	perror ("too many extension fields");
    else
    {
	File.fields.list = newFields;
	File.fields.max = newCount;
	result = TagSuccess;
    }
    return result;
}

static void parseExtensionFields (tagEntry *const entry, char *const string)
{
    char *p = string;
    while (p != NULL  &&  *p != '\0')
    {
	while (*p == TAB)
	    *p++ = '\0';
	if (*p != '\0')
	{
	    char *colon;
	    char *field = p;
	    p = strchr (p, TAB);
	    if (p != NULL)
		*p++ = '\0';
	    colon = strchr (field, ':');
	    if (colon == NULL)
		entry->kind = field;
	    else
	    {
		const char *key = field;
		const char *value = colon + 1;
		*colon = '\0';
		if (strcmp (key, "kind") == 0)
		    entry->kind = value;
		else if (strcmp (key, "file") == 0)
		    entry->fileScope = 1;
		else if (strcmp (key, "line") == 0)
		    entry->address.lineNumber = atol (value);
		else
		{
		    if (entry->fields.count == File.fields.max)
			growFields ();
		    File.fields.list [entry->fields.count].key = key;
		    File.fields.list [entry->fields.count].value = value;
		    ++entry->fields.count;
		}
	    }
	}
    }
}

static void parseTagLine (tagEntry *const entry)
{
    int i;
    char *p = File.line.buffer;
    char *tab = strchr (p, TAB);
    int fieldsPresent = 0;

    entry->fields.list = NULL;
    entry->fields.count = 0;
    entry->kind = NULL;
    entry->fileScope = 0;

    entry->name = p;
    if (tab != NULL)
    {
	*tab = '\0';
	p = tab + 1;
	entry->file = p;
	tab = strchr (p, TAB);
	if (tab != NULL)
	{
	    *tab = '\0';
	    p = tab + 1;
	    if (*p == '/'  ||  *p == '?')
	    {
		/* parse pattern */
		int delimiter = *(unsigned char*) p;
		entry->address.lineNumber = 0;
		entry->address.pattern = p;
		do
		{
		    p = strchr (p + 1, delimiter);
		} while (p != NULL  &&  *(p - 1) == '\\');
		if (p == NULL)
		{
		    /* invalid pattern */
		}
		else
		    ++p;
	    }
	    else if (isdigit ((int) *(unsigned char*) p))
	    {
		/* parse line number */
		entry->address.pattern = p;
		entry->address.lineNumber = atol (p);
		while (isdigit ((int) *(unsigned char*) p))
		    ++p;
	    }
	    else
	    {
		/* invalid pattern */
	    }
	    fieldsPresent = (strncmp (p, ";\"", 2) == 0);
	    *p = '\0';
	    if (fieldsPresent)
		parseExtensionFields (entry, p + 2);
	}
    }
    if (entry->fields.count > 0)
	entry->fields.list = File.fields.list;
    for (i = entry->fields.count  ;  i < File.fields.max  ;  ++i)
    {
	File.fields.list [i].key = NULL;
	File.fields.list [i].value = NULL;
    }
}

static void readPseudoTags (tagFileInfo *const info)
{
    fpos_t startOfLine;
    size_t prefixLength = strlen (PseudoTagPrefix);
    while (1)
    {
	const char *key, *value;
	tagEntry entry;
	fgetpos (File.fp, &startOfLine);
	if (! readTagLine ())
	    break;
	if (strncmp (File.line.buffer, PseudoTagPrefix, prefixLength) != 0)
	    break;
	else
	{
	    parseTagLine (&entry);
	    key = entry.name + prefixLength;
	    value = entry.file;
	    if (strcmp (key, "TAG_FILE_SORTED") == 0)
		File.sorted = atoi (value);
	    if (info != NULL)
	    {
		if (strcmp (key, "TAG_FILE_FORMAT") == 0)
		    info->file.format = atoi (value);
		else if (strcmp (key, "TAG_FILE_SORTED") == 0)
		    info->file.sorted = atoi (value);
		else if (strcmp (key, "TAG_PROGRAM_AUTHOR") == 0)
		    info->program.author = value;
		else if (strcmp (key, "TAG_PROGRAM_NAME") == 0)
		    info->program.name = value;
		else if (strcmp (key, "TAG_PROGRAM_URL") == 0)
		    info->program.url = value;
		else if (strcmp (key, "TAG_PROGRAM_VERSION") == 0)
		    info->program.version = value;
	    }
	}
    }
    fsetpos (File.fp, &startOfLine);
}

static tagResult initialize (const char *filePath, tagFileInfo *info)
{
    tagResult result = TagFailure;
    if (File.initialized)
    {
	fclose (File.fp);
	File.fp = NULL;
	File.initialized = 0;
    }
    else
    {
	growString (&File.line);
	growString (&File.name);
	File.fields.max = 20;
	File.fields.list = (tagExtensionField*) malloc (
	    File.fields.max * sizeof (tagExtensionField));
    }
    File.fp = fopen (filePath, "r");
    if (File.fp == NULL)
	perror ("cannot open tag file");
    else
    {
	fseek (File.fp, 0, SEEK_END);
	File.size = ftell (File.fp);
	rewind (File.fp);
	if (info != NULL)
	{
	    /* read pseudo tags into 'info' */
	    info->file.format     = 1;
	    info->file.sorted     = 0;
	    info->program.author  = EmptyString;
	    info->program.name    = EmptyString;
	    info->program.url     = EmptyString;
	    info->program.version = EmptyString;
	}
	readPseudoTags (info);
	File.initialized = 1;
	result = TagSuccess;
    }
    return result;
}

static void terminate (void)
{
    File.initialized = 0;
    fclose (File.fp);
    File.fp = NULL;

    free (File.line.buffer);
    File.line.buffer = NULL;
    File.line.size = 0;

    free (File.name.buffer);
    File.name.buffer = NULL;
    File.name.size = 0;

    free (File.fields.list);
    File.fields.list = NULL;
    File.fields.max = 0;
}

static tagResult readNext (tagEntry *const entry)
{
    tagResult result = TagFailure;
    if (! readTagLine ())
	result = TagFailure;
    else
    {
	if (entry != NULL)
	    parseTagLine (entry);
	result = TagSuccess;
    }
    return result;
}

static const char *readFieldValue (
    const tagEntry *const entry, const char *const key)
{
    const char *result = NULL;
    int i;
    if (strcmp (key, "kind") == 0)
	result = entry->kind;
    else if (strcmp (key, "file") == 0)
	result = EmptyString;
    else for (i = 0  ;  i < entry->fields.count  &&  result != NULL  ;  ++i)
	if (strcmp (entry->fields.list [i].key, key) == 0)
	    result = entry->fields.list [i].value;
    return result;
}

static int readTagLineSeek (const off_t pos)
{
    int result = 0;
    if (fseek (File.fp, pos, SEEK_SET) == 0)
    {
	result = readTagLine ();	/* read probable partial line */
	if (pos > 0  &&  result)
	    result = readTagLine ();	/* read complete line */
    }
    return result;
}

static void findFirstNonMatchBefore (const char *const name)
{
#define JUMP_BACK 512
    int more_lines;
    int comp;
    off_t start = File.pos;
    off_t pos = start;
    do
    {
	if (pos < (off_t) JUMP_BACK)
	    pos = 0;
	else
	    pos = pos - JUMP_BACK;
	more_lines = readTagLineSeek (pos);
	comp = nameComparison (name);
    } while (more_lines  &&  comp == 0  &&  pos > 0  &&  pos < start);
}

static tagResult findFirstMatchBefore (const char *const name)
{
    tagResult result = TagFailure;
    int more_lines;
    off_t start = File.pos;
    findFirstNonMatchBefore (name);
    do
    {
	more_lines = readTagLine ();
	if (nameComparison (name) == 0)
	    result = TagSuccess;
    } while (more_lines  &&  result != TagSuccess  &&  File.pos < start);
    return result;
}

static tagResult findBinary (const char *const name)
{
    tagResult result = TagFailure;
    off_t lower_limit = 0;
    off_t upper_limit = File.size;
    off_t last_pos = 0;
    off_t pos = upper_limit / 2;
    while (result != TagSuccess)
    {
	if (! readTagLineSeek (pos))
	{
	    /* in case we fell off end of file */
	    result = findFirstMatchBefore (name);
	    break;
	}
	else if (pos == last_pos)
	{
	    /* prevent infinite loop if we backed up to beginning of file */
	    break;
	}
	else
	{
	    const int comp = nameComparison (name);
	    last_pos = pos;
	    if (comp < 0)
	    {
		upper_limit = pos;
		pos = lower_limit + ((upper_limit - lower_limit) / 2);
	    }
	    else if (comp > 0)
	    {
		lower_limit = pos;
		pos = lower_limit + ((upper_limit - lower_limit) / 2);
	    }
	    else if (pos == 0)
		result = TagSuccess;
	    else
		result = findFirstMatchBefore (name);
	}
    }
    return result;
}

static tagResult findSequential (const char *const name)
{
    tagResult result = TagFailure;
    if (File.initialized)
    {
	while (result == TagFailure  &&  readTagLine ())
	{
	    if (nameComparison (name) == 0)
		result = TagSuccess;
	}
    }
    return result;
}

static tagResult find (tagEntry *const entry, const char *const name)
{
    tagResult result = TagFailure;
    File.find.name = name;
    fseek (File.fp, 0, SEEK_END);
    File.size = ftell (File.fp);
    rewind (File.fp);
    if (File.sorted)
	result = findBinary (name);
    else
	result = findSequential (name);

    if (result != TagSuccess)
	File.find.pos = File.size;
    else
    {
	File.find.pos = File.pos;
	if (entry != NULL)
	    parseTagLine (entry);
    }
    return result;
}

static tagResult findNext (tagEntry *const entry)
{
    tagResult result = TagFailure;
    if (File.sorted)
    {
	result = tagsNext (entry);
	if (result == TagSuccess  && nameComparison (File.find.name) != 0)
	    result = TagFailure;
    }
    else
    {
	result = findSequential (File.find.name);
	if (result == TagSuccess  &&  entry != NULL)
	    parseTagLine (entry);
    }
    return result;
}

/*
*  EXTERNAL INTERFACE
*/

extern tagResult tagsOpen (const char *filePath, tagFileInfo *info)
{
    return initialize (filePath, info);
}

extern tagResult tagsNext (tagEntry *const entry)
{
    tagResult result = TagFailure;
    if (File.initialized)
	result = readNext (entry);
    return result;
}

extern const char *tagsField (const tagEntry *const entry, const char *const key)
{
    const char *result = NULL;
    if (entry != NULL)
	result = readFieldValue (entry, key);
    return result;
}

extern tagResult tagsFind (tagEntry *const entry, const char *const name)
{
    tagResult result = TagFailure;
    if (File.initialized)
	result = find (entry, name);
    return result;
}

extern tagResult tagsFindNext (tagEntry *const entry)
{
    tagResult result = TagFailure;
    if (File.initialized)
	result = findNext (entry);
    return result;
}

extern tagResult tagsClose (void)
{
    tagResult result = TagFailure;
    if (File.initialized)
	terminate ();
    return result;
}

/*
*  TEST FRAMEWORK
*/

#ifdef READTAGS_MAIN

static void printTag (const tagEntry *entry)
{
    int i;
    printf ("%s\t%s\t%s",
	entry->name, entry->file, entry->address.pattern);
    if (entry->kind != NULL  &&  entry->kind [0] != '\0')
	printf ("\tkind:%s", entry->kind);
    if (entry->fileScope)
	printf ("\tfile:");
    if (entry->address.lineNumber > 0)
	printf ("\tline:%lu", entry->address.lineNumber);
    for (i = 0  ;  i < entry->fields.count  ;  ++i)
	printf ("\t%s:%s", entry->fields.list [i].key,
	    entry->fields.list [i].value);
    putchar ('\n');
}

static void findTag (const char *const name)
{
    tagFileInfo info;
    tagEntry entry;
    tagsOpen (TagFileName, &info);
    if (tagsFind (&entry, name) == TagSuccess)
    {
	do
	{
	    printTag (&entry);
	} while (tagsFindNext (&entry) == TagSuccess);
    }
    tagsClose ();
}

static void listTags (void)
{
    tagFileInfo info;
    tagEntry entry;
    tagsOpen (TagFileName, &info);
    while (tagsNext (&entry) == TagSuccess)
	printTag (&entry);
    tagsClose ();
}

const char *const Usage =
    "Usage: %s [-f name(s)] [-l] [-t file]\n"
    "    -f name(s)   Find tags matching specified names.\n"
    "    -l           List all tags.\n"
    "    -t file      Use specified tag file (default: \"tags\").\n"
    "  Note that options are acted upon as encountered, so order is significant.\n";

extern int main (int argc, char **argv)
{
    int i;
    if (argc == 1)
    {
	fprintf (stderr, Usage, argv [0]);
	exit (1);
    }
    for (i = 1  ;  i < argc  ;  ++i)
    {
	if (*argv [i] == '-')
	{
	    const int arg = argv [i][1];
	    switch (arg)
	    {
		case 'f':
		{
		    int j;
		    for (j = i + 1  ;  j < argc  ;  ++j)
			findTag (argv [j]);
		    break;
		}
		case 'l':
		    listTags ();
		    break;
		case 't':
		    ++i;
		    TagFileName = argv [i];
		    break;
		default:
		    fprintf (stderr, "%s: unknown option: %s\n",
				argv[0], argv[i]);
		    exit (1);
		    break;
	    }
	}
    }
    return 0;
}

#endif

/* vi:set tabstop=8 shiftwidth=4: */
