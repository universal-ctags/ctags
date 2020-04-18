/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released into the public domain.
*
*   This module contains functions for reading tag files.
*/

/*
*   INCLUDE FILES
*/

#include "printtags.h"
#include <stdio.h>


/*
*   DATA DEFINITIONS
*/

static tagPrintProcs printFILEProcs = {
	.printStr = (int  (*) (const char *, void *))fputs,
	.printChar = (int  (*) (int, void *))fputc,
};


/*
*   FUNCTION DEFINITIONS
*/

static void ultostr (char dst [21], unsigned long d)
{
	int o [20];
	int i;

	if (d == 0)
	{
		dst [0] = '0';
		dst [1] = '\0';
		return;
	}

	for (i = 0; d != 0; i++, d = d/10)
		o [i] = d % 10;

	for (int j = i - 1; j >= 0; j--)
		dst [i - j - 1] = o[j] + '0';
	dst [i] = '\0';
}

static void printValue (const char *val, int printingWithEscaping,
						int  (* print_str) (const char *, void *),
						int  (* print_char) (int, void *),
						void *outfp)
{
	if (printingWithEscaping)
	{
		for(; *val != '\0'; val++)
		{
			switch (*val)
			{
				case '\t': print_str ("\\t",  outfp); break;
				case '\r': print_str ("\\r",  outfp); break;
				case '\n': print_str ("\\n",  outfp); break;
				case '\\': print_str ("\\\\", outfp); break;
					/* Universal-CTags extensions */
				case '\a': print_str ("\\a", outfp); break;
				case '\b': print_str ("\\b", outfp); break;
				case '\v': print_str ("\\v", outfp); break;
				case '\f': print_str ("\\f", outfp); break;
				default:
					if ((0x01 <= *val && *val <= 0x1F) || *val == 0x7F)
					{
						char c[5] = {
							[0] = '\\',
							[1] = 'x',
						};
						c [2] = (*val / 16) % 16;
#if 0
						if (c [2] == 0)
						{
							c [2] = *val % 16;
							c [2] += ( c [2] < 10 )? '0': 'A' - 9;
							c [3] = '\0';
						}
						else
#endif
						{
							c [2] += ( c [2] < 10 )? '0': 'A' - 9;
							c [3] = *val % 16;
							c [3] += ( c [3] < 10 )? '0': 'A' - 9;
							c [4] = '\0';
						}
						print_str (c, outfp);
					}
					else
						print_char (*val, outfp);
			}
		}
	}
	else
		print_str (val, outfp);
}

static void tagsPrintTag (const tagEntry *entry,
						  int printingExtensionFields,
						  int printingLineNumber,
						  int printingWithEscaping,
						  int pseudoTag,
						  int  (* print_str) (const char *, void *),
						  int  (* print_char) (int, void *),
						  void *outfp)
{
	int i;
	int first = 1;
	const char* separator = ";\"";
	const char* const empty = "";
/* "sep" returns a value only the first time it is evaluated */
#define sep (first ? (first = 0, separator) : empty)

	if (entry->name == NULL
		|| entry->file == NULL
		|| entry->address.pattern == NULL)
		return;
	if (pseudoTag)
		print_str (entry->name, outfp);
	else if (*entry->name == '!' && printingWithEscaping)
	{
		print_str ("\\x21", outfp);
		printValue (entry->name + 1, printingWithEscaping,
					print_str, print_char, outfp);
	}
	else if (*entry->name == ' ' && printingWithEscaping)
	{
		print_str ("\\x20", outfp);
		printValue (entry->name + 1, printingWithEscaping,
					print_str, print_char, outfp);
	}
	else
		printValue (entry->name, printingWithEscaping,
					print_str, print_char, outfp);

	print_char ('\t', outfp);
	printValue  (entry->file, printingWithEscaping,
				 print_str, print_char, outfp);
	print_char ('\t', outfp);
	print_str (entry->address.pattern, outfp);

	if (printingExtensionFields)
	{
		if (entry->kind != NULL  &&  entry->kind [0] != '\0')
		{
			print_str (sep, outfp);
			print_str ("\tkind:", outfp);
			printValue (entry->kind, printingWithEscaping,
						print_str, print_char, outfp);
			first = 0;
		}
		if (entry->fileScope)
		{
			print_str (sep, outfp);
			print_str ("\tfile:", outfp);
			first = 0;
		}
		if (printingLineNumber && entry->address.lineNumber > 0)
		{
			print_str (sep, outfp);
			print_str ("\tline:", outfp);
			char buf [20 + 1];	/* 20 comes from UINNT64_MAX, 1 is for \0. */
			ultostr (buf, entry->address.lineNumber);
			print_str (buf, outfp);
			first = 0;
		}
		for (i = 0  ;  i < entry->fields.count  ;  ++i)
		{
			if (entry->fields.list [i].key)
			{
				print_str (sep, outfp);
				print_char ('\t', outfp);
				print_str (entry->fields.list [i].key, outfp);
				print_char (':', outfp);
				if (entry->fields.list  [i].value)
					printValue (entry->fields.list [i].value,
								printingWithEscaping, print_str, print_char, outfp);
				first = 0;
			}
		}
	}
	print_char ('\n', outfp);
#undef sep
}

extern int tagsPrint (const tagEntry *entry,
					  tagPrintOptions *opt, tagPrintProcs *procs, void *outfp)
{
	if (!procs)
		procs = &printFILEProcs;

	tagsPrintTag (entry,
				  opt->extensionFields,
				  opt->lineNumber,
				  opt->escaping,
				  0,
				  procs->printStr,
				  procs->printChar,
				  outfp);
	return 1;					/* TODO */
}

extern int tagsPrintPseudoTag (const tagEntry *entry,
							   tagPrintOptions *opt, tagPrintProcs *procs, void *outfp)
{
	if (!procs)
		procs = &printFILEProcs;

	tagsPrintTag (entry,
				  opt->extensionFields,
				  opt->lineNumber,
				  opt->escaping,
				  1,
				  procs->printStr,
				  procs->printChar,
				  outfp);
	return 1;					/* TODO */
}
