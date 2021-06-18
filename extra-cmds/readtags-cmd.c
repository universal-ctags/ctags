/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released into the public domain.
*
*   This module contains functions for reading tag files.
*/

#include "general.h"

#include "readtags.h"
#include "printtags.h"
#include "routines.h"
#include "routines_p.h"
#include <string.h>		/* strerror */
#include <stdlib.h>		/* exit */
#include <stdio.h>		/* stderr */

static const char *TagFileName = "tags";
static const char *ProgramName;
static int extensionFields;
static int SortOverride;
static sortType SortMethod;
static int allowPrintLineNumber;
static int debugMode;
static int escaping;
#ifdef READTAGS_DSL
#include "dsl/qualifier.h"
static QCode *Qualifier;
#include "dsl/sorter.h"
static SCode *Sorter;
#include "dsl/formatter.h"
static FCode *Formatter;
#endif

static const char* tagsStrerror (int err)
{
	if (err > 0)
		return strerror (err);
	else if (err < 0)
	{
		switch (err)
		{
		case TagErrnoUnexpectedSortedMethod:
			return "Unexpected sorted method";
		case TagErrnoUnexpectedFormat:
			return "Unexpected format number";
		case TagErrnoUnexpectedLineno:
			return "Unexpected value for line: field";
		case TagErrnoInvalidArgument:
			return "Unexpected argument passed to the API function";
		default:
			return "Unknown error";
		}
	}
	else
		return "no error";
}

static void printTag (const tagEntry *entry)
{
	tagPrintOptions opts = {
		.extensionFields = extensionFields,
		.lineNumber = allowPrintLineNumber,
		.escaping = escaping,
	};
	tagsPrint (entry, &opts, NULL, stdout);
}

#ifdef READTAGS_DSL
static void printTagWithFormatter (const tagEntry *entry)
{
	f_print (entry, Formatter, stdout);
}
#endif

static void printPseudoTag (const tagEntry *entry)
{
	tagPrintOptions opts = {
		.extensionFields = extensionFields,
		.lineNumber = allowPrintLineNumber,
		.escaping = escaping,
	};
	tagsPrintPseudoTag (entry, &opts, NULL, stdout);
}

#ifdef READTAGS_DSL
static void freeCopiedTag (tagEntry *e)
{
	free ((void *)e->name);
	free ((void *)e->file);
	if (e->address.pattern)
		free ((void *)e->address.pattern);
	if (e->kind)
		free ((void *)e->kind);
	for (unsigned short c = 0; c < e->fields.count; c++)
	{
		free ((void *)e->fields.list[c].key);
		free ((void *)e->fields.list[c].value);
	}
	if (e->fields.count)
		free ((void *)e->fields.list);
	free ((void *)e);
}

static tagEntry *copyTag (tagEntry *o)
{
	tagEntry *n;

	n = calloc (1, sizeof  (*o));
	if (!n)
		perror (__FUNCTION__);

	n->name = strdup (o->name);

	if (!n->name)
		perror (__FUNCTION__);

	if (o->file)
		n->file = strdup (o->file);
	if (o->file && !n->file)
		perror (__FUNCTION__);

	if (o->address.pattern)
	{
		n->address.pattern = strdup (o->address.pattern);
		if (!n->address.pattern)
			perror (__FUNCTION__);
	}

	n->address.lineNumber = o->address.lineNumber;

	if (o->kind)
	{
		n->kind = strdup (o->kind);
		if (!n->kind)
			perror (__FUNCTION__);
	}

	n->fileScope = o->fileScope;
	n->fields.count = o->fields.count;

	if (o->fields.count == 0)
		return n;

	n->fields.list = malloc (o->fields.count *sizeof (*o->fields.list));
	if (!n->fields.list)
		perror (__FUNCTION__);

	for (unsigned short c = 0; c < o->fields.count; c++)
	{
		n->fields.list[c].key = strdup (o->fields.list[c].key);
		if (!n->fields.list[c].key)
			perror (__FUNCTION__);

		n->fields.list[c].value = strdup (o->fields.list[c].value);
		if (!n->fields.list[c].value)
			perror (__FUNCTION__);
	}

	return n;
}

struct tagEntryHolder {
	tagEntry *e;
};
struct tagEntryArray {
	int count;
	int length;
	struct tagEntryHolder *a;
};

struct tagEntryArray *tagEntryArrayNew (void)
{
	struct tagEntryArray * a = malloc (sizeof (struct tagEntryArray));
	if (!a)
		perror(__FUNCTION__);

	a->count = 0;
	a->length = 1024;
	a->a = malloc(a->length * sizeof (a->a[0]));
	if (!a->a)
		perror(__FUNCTION__);

	return a;
}

void tagEntryArrayPush (struct tagEntryArray *a, tagEntry *e)
{
	if (a->count + 1 == a->length)
	{
		if (a->length * 2 < a->length)
			perror("Too large array allocation");

		struct tagEntryHolder *tmp = realloc (a->a, sizeof (a->a[0]) * (a->length * 2));
		if (!tmp)
			perror(__FUNCTION__);

		a->a = tmp;
		a->length *= 2;
	}

	a->a[a->count++].e = e;
}

void tagEntryArrayFree (struct tagEntryArray *a, int freeTags)
{
	if (freeTags)
	{
		for (int i = 0; i < a->count; i++)
			freeCopiedTag (a->a[i].e);
	}
	free (a->a);
	free (a);
}

static int compareTagEntry (const void *a, const void *b)
{
	return s_compare (((struct tagEntryHolder *)a)->e, ((struct tagEntryHolder *)b)->e, Sorter);
}

static void walkTags (tagFile *const file, tagEntry *first_entry,
					  tagResult (* nextfn) (tagFile *const, tagEntry *),
					  void (* actionfn) (const tagEntry *))
{
	struct tagEntryArray *a = NULL;

	if (Sorter)
		a = tagEntryArrayNew ();

	do
	{
		if (Qualifier)
		{
			int i = q_is_acceptable (Qualifier, first_entry);
			switch (i)
			{
			case Q_REJECT:
				continue;
			case Q_ERROR:
				exit (1);
			}
		}

		if (a)
		{
			tagEntry *e = copyTag (first_entry);
			tagEntryArrayPush (a, e);
		}
		else
			(* actionfn) (first_entry);
	} while ( (*nextfn) (file, first_entry) == TagSuccess);

	int err = tagsGetErrno (file);
	if (err != 0)
	{
		fprintf (stderr, "%s: error in walktTags(): %s\n",
				 ProgramName,
				 tagsStrerror (err));
		exit (1);
	}

	if (a)
	{
		qsort (a->a, a->count, sizeof (a->a[0]), compareTagEntry);
		for (int i = 0; i < a->count; i++)
			(* actionfn) (a->a[i].e);
		tagEntryArrayFree (a, 1);
	}
}
#else
static void walkTags (tagFile *const file, tagEntry *first_entry,
					  tagResult (* nextfn) (tagFile *const, tagEntry *),
					  void (* actionfn) (const tagEntry *))
{
	do
		(* actionfn) (first_entry);
	while ( (*nextfn) (file, first_entry) == TagSuccess);

	int err = tagsGetErrno (file);
	if (err != 0)
	{
		fprintf (stderr, "%s: error in walktTags(): %s\n",
				 ProgramName,
				 tagsStrerror (err));
		exit (1);
	}
}
#endif

static int copyFile (FILE *in, FILE *out)
{
#define BUFSIZE (4096 * 10)
	static unsigned char buffer [BUFSIZE];

	while (1)
	{
		size_t r, t;

		r = fread (buffer, 1, BUFSIZE, in);
		if (!r)
		{
			if (ferror(in))
			{
				fprintf (stderr, "%s: error in reading from stdin\n", ProgramName);
				return -1;
			}
			/* EOF */
			break;
		}
		t = fwrite (buffer, 1, r, out);
		if (r != t)
		{
			fprintf (stderr, "%s error in writing to the temporarily file", ProgramName);
			return -1;
		}
	}
	return 0;
}

static void removeTagFile (void)
{
	remove (TagFileName);
	eFree ((char *)TagFileName);
}

static tagFile *openTags (const char *const filePath, tagFileInfo *const info)
{
	if (strcmp (filePath, "-") == 0)
	{
		char *tempName = NULL;
		FILE *tempFP = tempFileFP ("w", &tempName);

		if (tempFP == NULL)
		{
			fprintf (stderr, "%s: failed to make a temporarily file for storing data from stdin\n",
					 ProgramName);
			exit (1);
		}
		TagFileName = tempName;
		atexit (removeTagFile);

		if (copyFile (stdin, tempFP) < 0)
		{
			fclose (tempFP);
			exit (1);
		}

		if (fflush (tempFP) < 0)
		{
			fprintf (stderr, "%s: failed to flush a temporarily file for storing data from stdin\n",
					 ProgramName);
			exit (1);
		}
		fclose (tempFP);
		return tagsOpen (tempName, info);
	}

	return tagsOpen (filePath, info);
}

static void findTag (const char *const name, const int options)
{
	tagFileInfo info;
	tagEntry entry;
	tagFile *const file = openTags (TagFileName, &info);
	if (file == NULL || !info.status.opened)
	{
		fprintf (stderr, "%s: cannot open tag file: %s: %s\n",
				 ProgramName, tagsStrerror (info.status.error_number), TagFileName);
		if (file)
			tagsClose (file);
		exit (1);
	}
	else
	{
		int err = 0;
		if (SortOverride)
		{
			if (tagsSetSortType (file, SortMethod) != TagSuccess)
			{
				err = tagsGetErrno (file);
				fprintf (stderr, "%s: cannot set sort type to %d: %s\n",
						 ProgramName,
						 SortMethod,
						 tagsStrerror (err));
				exit (1);
			}
		}
		if (debugMode)
			fprintf (stderr, "%s: searching for \"%s\" in \"%s\"\n",
					 ProgramName, name, TagFileName);
		if (tagsFind (file, &entry, name, options) == TagSuccess)
			walkTags (file, &entry, tagsFindNext,
#ifdef READTAGS_DSL
					  Formatter? printTagWithFormatter:
#endif
					  printTag);
		else if ((err = tagsGetErrno (file)) != 0)
		{
			fprintf (stderr, "%s: error in tagsFind(): %s\n",
					 ProgramName,
					 tagsStrerror (err));
			exit (1);
		}
		tagsClose (file);
	}
}

static void listTags (int pseudoTags)
{
	tagFileInfo info;
	tagEntry entry;
	tagFile *const file = openTags (TagFileName, &info);
	if (file == NULL || !info.status.opened)
	{
		fprintf (stderr, "%s: cannot open tag file: %s: %s\n",
				 ProgramName,
				 tagsStrerror (info.status.error_number),
				 TagFileName);
		if (file)
			tagsClose (file);
		exit (1);
	}
	else if (pseudoTags)
	{
		int err = 0;
		if (tagsFirstPseudoTag (file, &entry) == TagSuccess)
			walkTags (file, &entry, tagsNextPseudoTag, printPseudoTag);
		else if ((err = tagsGetErrno (file)) != 0)
		{
			fprintf (stderr, "%s: error in tagsFirstPseudoTag(): %s\n",
					 ProgramName,
					 tagsStrerror (err));
			exit (1);
		}
		tagsClose (file);
	}
	else
	{
		int err = 0;
		if (tagsFirst (file, &entry) == TagSuccess)
			walkTags (file, &entry, tagsNext,
#ifdef READTAGS_DSL
					  Formatter? printTagWithFormatter:
#endif
					  printTag);
		else if ((err = tagsGetErrno (file)) != 0)
		{
			fprintf (stderr, "%s: error in tagsFirst(): %s\n",
					 ProgramName,
					 tagsStrerror (err));
			exit (1);
		}
		tagsClose (file);
	}
}

static const char *const Usage =
	"Find tag file entries matching specified names.\n\n"
	"Usage: \n"
	"    %s -h | --help\n"
	"        Print this help message.\n"
#ifdef READTAGS_DSL
	"    %s -H POSTPROCESSOR | --help-expression POSTPROCESSOR\n"
	"        Print available terms that can be used in POSTPROCESSOR expression.\n"
	"        POSTPROCESSOR: filter sorter formatter\n"
#endif
	"    %s [OPTIONS] ACTION\n"
	"        Do the specified action.\n"
	"Actions:\n"
	"    -l | --list\n"
	"        List regular tags.\n"
	"    [-] NAME...\n"
	"        List regular tags matching NAME(s).\n"
	"        \"-\" indicates arguments after this as NAME(s) even if they start with -.\n"
	"    -D | --list-pseudo-tags\n"
	"        List pseudo tags.\n"
	"Options:\n"
	"    -d | --debug\n"
	"        Turn on debugging output.\n"
	"    -E | --escape-output\n"
	"        Escape characters like tabs in output as described in tags(5).\n"
	"    -e | --extension-fields\n"
	"        Include extension fields in output.\n"
	"    -i | --icase-match\n"
	"        Perform case-insensitive matching in the NAME action.\n"
	"    -n | --line-number\n"
	"        Also include the line number field when -e option is given.\n"
	"    -p | --prefix-match\n"
	"        Perform prefix matching in the NAME action.\n"
	"    -t TAGFILE | --tag-file TAGFILE\n"
	"        Use specified tag file (default: \"tags\").\n"
	"        \"-\" indicates taking tag file data from standard input.\n"
	"    -s[0|1|2] | --override-sort-detection METHOD\n"
	"        Override sort detection of tag file.\n"
	"        METHOD: unsorted|sorted|foldcase\n"
#ifdef READTAGS_DSL
	"    -F EXP | --formatter EXP\n"
	"        Format the tags listed by ACTION with EXP when printing.\n"
	"    -Q EXP | --filter EXP\n"
	"        Filter the tags listed by ACTION with EXP before printing.\n"
	"    -S EXP | --sorter EXP\n"
	"        Sort the tags listed by ACTION with EXP before printing.\n"
#endif
	;

static void printUsage(FILE* stream, int exitCode)
{
	fprintf (stream, Usage, ProgramName,
#ifdef READTAGS_DSL
			 ProgramName,
#endif
			 ProgramName);
	exit (exitCode);
}

#ifdef READTAGS_DSL
static void printFilterExpression (FILE *stream, int exitCode)
{
	fprintf (stream, "Filter expression: \n");
	q_help (stream);
	exit (exitCode);
}

static void printSorterExpression (FILE *stream, int exitCode)
{
	fprintf (stream, "Sorter expression: \n");
	s_help (stream);
	exit (exitCode);
}

static void printFormatterExpression (FILE *stream, int exitCode)
{
	fprintf (stream, "Formatter expression: \n");
	f_help (stream);
	exit (exitCode);
}

static void *compileExpression(const char* exp, void * (*compiler) (EsObject *),
							   const char *compiler_name)
{
	EsObject *sexp = es_read_from_string (exp, NULL);
	void *code;

	if (es_error_p (sexp))
	{
		fprintf (stderr,
				 "Failed to read the expression for %s: %s\n", compiler_name, exp);
		fprintf (stderr,
				 "Reason: %s\n", es_error_name (sexp));
		exit (1);
	}

	code = compiler (sexp);
	if (code == NULL)
	{
		fprintf (stderr,
				 "Failed to compile the expression of %s: %s\n", compiler_name, exp);
		exit (1);
	}
	es_object_unref (sexp);
	return code;
}
#endif

extern int main (int argc, char **argv)
{
	int options = 0;
	int actionSupplied = 0;
	int i;
	int ignore_prefix = 0;

	ProgramName = argv [0];
	setExecutableName (ProgramName);
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
		else if (arg [0] == '-' && arg [1] == '-')
		{
			const char *optname = arg + 2;
			if (strcmp (optname, "debug") == 0)
				debugMode++;
			else if (strcmp (optname, "list-pseudo-tags") == 0)
			{
				listTags (1);
				actionSupplied = 1;
			}
			else if (strcmp (optname, "help") == 0)
				printUsage (stdout, 0);
#ifdef READTAGS_DSL
			else if (strcmp (optname, "help-expression") == 0)
			{
				if (i + 1 < argc)
				{
					const char *exp_klass = argv [++i];
					if (strcmp (exp_klass, "filter") == 0)
						printFilterExpression (stdout, 0);
					if (strcmp (exp_klass, "sorter") == 0)
						printSorterExpression (stdout, 0);
					if (strcmp (exp_klass, "formatter") == 0)
						printFormatterExpression (stdout, 0);
					else
					{
						fprintf (stderr, "%s: unknown expression class for --%s option\n",
								 ProgramName, optname);
						exit (1);

					}
				}
				else
				{
					fprintf (stderr, "%s: missing expression class for --%s option\n",
							 ProgramName, optname);
					exit (1);
				}
			}
#endif
			else if (strcmp (optname, "escape-output") == 0)
				escaping = 1;
			else if (strcmp (optname, "extension-fields") == 0)
				extensionFields = 1;
			else if (strcmp (optname, "icase-match") == 0)
				options |= TAG_IGNORECASE;
			else if (strcmp (optname, "prefix-match") == 0)
				options |= TAG_PARTIALMATCH;
			else if (strcmp (optname, "list") == 0)
			{
				listTags (0);
				actionSupplied = 1;
			}
			else if (strcmp (optname, "line-number") == 0)
				allowPrintLineNumber = 1;
			else if (strcmp (optname, "tag-file") == 0)
			{
				if (i + 1 < argc)
					TagFileName = argv [++i];
				else
					printUsage (stderr, 1);
			}
			else if (strcmp (optname, "override-sort-detection") == 0)
			{
				if (i + 1 < argc)
				{
					const char *sort_spec = argv [++i];
					if (strcmp (sort_spec, "0") == 0
						|| strcmp (sort_spec, "unsorted") == 0)
						SortMethod = 0;
					else if (strcmp (sort_spec, "1") == 0
							 || strcmp (sort_spec, "sorted") == 0)
						SortMethod = 1;
					else if (strcmp (sort_spec, "2") == 0
							 || strcmp (sort_spec, "foldcase") == 0)
						SortMethod = 2;
					else
					{
						fprintf (stderr, "%s: unknown sort method for --%s option\n",
								 ProgramName, optname);
						exit (1);
					}
				}
				else
				{
					fprintf (stderr, "%s: missing sort method for --%s option\n",
							 ProgramName, optname);
					exit (1);
				}
			}
#ifdef READTAGS_DSL
			else if (strcmp (optname, "filter") == 0)
			{
				if (i + 1 < argc)
					Qualifier = compileExpression (argv[++i],
												   (void * (*)(EsObject *))q_compile,
												   optname);
				else
				{
					fprintf (stderr, "%s: missing filter expression for --%s option\n",
							 ProgramName, optname);
					exit (1);
				}
			}
			else if (strcmp (optname, "sorter") == 0)
			{
				if (i + 1 < argc)
					Sorter = compileExpression (argv[++i],
												(void * (*)(EsObject *))s_compile,
												optname);
				else
				{
					fprintf (stderr, "%s: missing sorter expression for --%s option\n",
							 ProgramName, optname);
					exit (1);
				}
			}
			else if (strcmp (optname, "formatter") == 0)
			{
				if (i + 1 < argc)
					Sorter = compileExpression (argv[++i],
												(void * (*)(EsObject *))f_compile,
												optname);
				else
				{
					fprintf (stderr, "%s: missing formatter expression for --%s option\n",
							 ProgramName, optname);
					exit (1);
				}
			}
#endif
			else
			{
				fprintf (stderr, "%s: unknown long options: --%s\n",
						 ProgramName, optname);
				exit (1);
				break;
			}
		}
		else
		{
			size_t j;
			for (j = 1  ;  arg [j] != '\0'  ;  ++j)
			{
				switch (arg [j])
				{
					case 'd': debugMode++; break;
					case 'D': listTags (1); actionSupplied = 1; break;
					case 'h': printUsage (stdout, 0); break;
#ifdef READTAGS_DSL
					case 'H':
						if (i + 1 < argc)
						{
							const char *exp_klass = argv [++i];
							if (strcmp (exp_klass, "filter") == 0)
								printFilterExpression (stdout, 0);
							else if (strcmp (exp_klass, "sorter") == 0)
								printSorterExpression (stdout, 0);
							else if (strcmp (exp_klass, "formatter") == 0)
								printFormatterExpression (stdout, 0);
							else
								printUsage(stderr, 1);
						}
						else
							printUsage(stderr, 1);
#endif
					case 'E': escaping = 1; break;
					case 'e': extensionFields = 1;         break;
					case 'i': options |= TAG_IGNORECASE;   break;
					case 'p': options |= TAG_PARTIALMATCH; break;
					case 'l': listTags (0); actionSupplied = 1; break;
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
#ifdef READTAGS_DSL
					case 'Q':
						if (i + 1 == argc)
							printUsage(stderr, 1);
						Qualifier = compileExpression (argv[++i],
													   (void * (*)(EsObject *))q_compile,
													   "filter");
						break;
					case 'S':
						if (i + 1 == argc)
							printUsage(stderr, 1);
						Sorter = compileExpression (argv[++i],
													   (void * (*)(EsObject *))s_compile,
													   "sorter");
						break;
					case 'F':
						if (i + 1 == argc)
							printUsage(stderr, 1);
						Formatter = compileExpression (argv[++i],
													   (void * (*)(EsObject *))f_compile,
													   "formatter");
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
			"%s: no action specified: specify one of NAME, -l or -D\n",
			ProgramName);
		exit (1);
	}
#ifdef READTAGS_DSL
	if (Qualifier)
		q_destroy (Qualifier);
	if (Sorter)
		s_destroy (Sorter);
	if (Formatter)
		f_destroy (Formatter);
#endif
	return 0;
}
