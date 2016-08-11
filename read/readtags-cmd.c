/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released into the public domain.
*
*   This module contains functions for reading tag files.
*/

#include "readtags.h"
#include <string.h>		/* strerror */
#include <stdlib.h>		/* exit */
#include <stdio.h>		/* stderr */

static const char *TagFileName = "tags";
static const char *ProgramName;
static int extensionFields;
static int SortOverride;
static sortType SortMethod;
static int allowPrintLineNumber;
#ifdef QUALIFIER
#include "dsl/qualifier.h"
static QCode *Qualifier;
#endif

static void printTag (const tagEntry *entry)
{
	int i;
	int first = 1;
	const char* separator = ";\"";
	const char* const empty = "";
/* "sep" returns a value only the first time it is evaluated */
#define sep (first ? (first = 0, separator) : empty)
	printf ("%s\t%s\t%s",
		entry->name, entry->file, entry->address.pattern);
	if (extensionFields)
	{
		if (entry->kind != NULL  &&  entry->kind [0] != '\0')
		{
			  printf ("%s\tkind:%s", sep, entry->kind);
			  first = 0;
		}
		if (entry->fileScope)
		{
			printf ("%s\tfile:", sep);
			first = 0;
		}
		if (allowPrintLineNumber && entry->address.lineNumber > 0)
		{
			printf ("%s\tline:%lu", sep, entry->address.lineNumber);
			first = 0;
		}
		for (i = 0  ;  i < entry->fields.count  ;  ++i)
		{
			printf ("%s\t%s:%s", sep, entry->fields.list [i].key,
				entry->fields.list [i].value);
			first = 0;
		}
	}
	putchar ('\n');
#undef sep
}

static void findTag (const char *const name, const int options)
{
	tagFileInfo info;
	tagEntry entry;
	tagFile *const file = tagsOpen (TagFileName, &info);
	if (file == NULL)
	{
		fprintf (stderr, "%s: cannot open tag file: %s: %s\n",
				ProgramName, strerror (info.status.error_number), name);
		exit (1);
	}
	else
	{
		if (SortOverride)
			tagsSetSortType (file, SortMethod);
		if (tagsFind (file, &entry, name, options) == TagSuccess)
		{
			do
			{
#ifdef QUALIFIER
				if (Qualifier)
				{
					int i = q_is_acceptable (Qualifier, &entry);
					switch (i)
					{
					case Q_REJECT:
						continue;
					case Q_ERROR:
						exit (1);
					}
				}
#endif
				printTag (&entry);
			} while (tagsFindNext (file, &entry) == TagSuccess);
		}
		tagsClose (file);
	}
}

static void listTags (void)
{
	tagFileInfo info;
	tagEntry entry;
	tagFile *const file = tagsOpen (TagFileName, &info);
	if (file == NULL)
	{
		fprintf (stderr, "%s: cannot open tag file: %s: %s\n",
				ProgramName, strerror (info.status.error_number), TagFileName);
		exit (1);
	}
	else
	{
		while (tagsNext (file, &entry) == TagSuccess)
		{
#ifdef QUALIFIER
			if (Qualifier)
			{
				int i = q_is_acceptable (Qualifier, &entry);
				switch (i)
				{
				case Q_REJECT:
					continue;
				case Q_ERROR:
					exit (1);
				}
			}
#endif
			printTag (&entry);
		}
		tagsClose (file);
	}
}

static const char *const Usage =
	"Find tag file entries matching specified names.\n\n"
	"Usage: \n"
	"    %s -h\n"
	"    %s [-ilp] [-n] "
#ifdef QUALIFIER
	"[-Q EXP] "
#endif
	"[-s[0|1]] [-t file] [-] [name(s)]\n\n"
	"Options:\n"
	"    -e           Include extension fields in output.\n"
	"    -h           Print this help message.\n"
	"    -i           Perform case-insensitive matching.\n"
	"    -l           List all tags.\n"
	"    -n           Allow print line numbers if -e option is given.\n"
	"    -p           Perform partial matching.\n"
#ifdef QUALIFIER
	"    -Q EXP       Filter the result with EXP.\n"
#endif
	"    -s[0|1|2]    Override sort detection of tag file.\n"
	"    -t file      Use specified tag file (default: \"tags\").\n"
	"    -            Treat arguments after this as NAME even if they start with -.\n"
	"Note that options are acted upon as encountered, so order is significant.\n";

static void printUsage(FILE* stream, int exitCode)
{
	fprintf (stream, Usage, ProgramName, ProgramName);
#ifdef QUALIFIER
	fprintf (stream, "\nFilter expression: \n");
	q_help (stream);
#endif
	exit (exitCode);
}

#ifdef QUALIFIER
static QCode *convertToQualifier(const char* exp)
{
	EsObject *sexp = es_read_from_string (exp, NULL);
	QCode *qcode;

	if (es_error_p (sexp))
	{
		fprintf (stderr,
			 "Failed to read the expression of qualifier: %s\n", exp);
		fprintf (stderr,
			 "Reason: %s\n", es_error_name (sexp));
		exit (1);
	}

	qcode = q_compile (sexp);
	if (qcode == NULL)
	{
		fprintf (stderr,
			 "Failed to compile the expression of qualifier: %s\n", exp);
		exit (1);
	}
	es_object_unref (sexp);
	return qcode;
}
#endif
extern int main (int argc, char **argv)
{
	int options = 0;
	int actionSupplied = 0;
	int i;
	int ignore_prefix = 0;

	ProgramName = argv [0];
	if (argc == 1)
		printUsage(stderr, 1);
	for (i = 1  ;  i < argc  ;  ++i)
	{
		const char *const arg = argv [i];
		if (ignore_prefix || arg [0] != '-')
		{
			findTag (arg, options);
			actionSupplied = 1;
		}
		else if (arg [0] == '-' && arg [1] == '\0')
			ignore_prefix = 1;
		else
		{
			size_t j;
			for (j = 1  ;  arg [j] != '\0'  ;  ++j)
			{
				switch (arg [j])
				{
					case 'h': printUsage (stdout, 0); break;
					case 'e': extensionFields = 1;         break;
					case 'i': options |= TAG_IGNORECASE;   break;
					case 'p': options |= TAG_PARTIALMATCH; break;
					case 'l': listTags (); actionSupplied = 1; break;
					case 'n': allowPrintLineNumber = 1; break;
					case 't':
						if (arg [j+1] != '\0')
						{
							TagFileName = arg + j + 1;
							j += strlen (TagFileName);
						}
						else if (i + 1 < argc)
							TagFileName = argv [++i];
						else
							printUsage(stderr, 1);
						break;
					case 's':
						SortOverride = 1;
						++j;
						if (arg [j] == '\0')
							SortMethod = TAG_SORTED;
						else if (strchr ("012", arg[j]) != NULL)
							SortMethod = (sortType) (arg[j] - '0');
						else
							printUsage(stderr, 1);
						break;
#ifdef QUALIFIER
					case 'Q':
						if (i + 1 == argc)
							printUsage(stderr, 1);
						Qualifier = convertToQualifier (argv[++i]);
						break;
#endif
					default:
						fprintf (stderr, "%s: unknown option: %c\n",
									ProgramName, arg[j]);
						exit (1);
						break;
				}
			}
		}
	}
	if (! actionSupplied)
	{
		fprintf (stderr,
			"%s: no action specified: specify tag name(s) or -l option\n",
			ProgramName);
		exit (1);
	}
	return 0;
}
