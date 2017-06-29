/*
*   Copyright (c) 2017, Masatake YAMATO
*
*   Author: Masatake YAMATO <yamato@redhat.com>
*           https://ctags.io
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*   It is provided on an as-is basis and no responsibility is accepted for its
*   failure to perform as expected.
*
*   This module contains infrastructure to realize Multi pass parsing over
*   Multi source files (MM).
*/

#include "general.h"  /* must always come first */

#define OPTION_WRITE
#include "options.h"

#include "mm.h"
#include "ptrarray.h"
#include "routines.h"
#include "types.h"


static ptrArray *mmSourceFiles;

struct mmEntry {
	char *fileName;
	langType lang;
};

static void mmEntryDelete (void *p)
{
	struct mmEntry *m = p;

	eFree (m->fileName);
	m->fileName = NULL;
	eFree (m);
}

extern void mmSchedule (const char*fname, langType lang)
{
	struct mmEntry * mm_entry = xMalloc (1, struct mmEntry);
	mm_entry->fileName = eStrdup (fname);
	mm_entry->lang = lang;

	if (!mmSourceFiles)
		mmSourceFiles = ptrArrayNew (mmEntryDelete);

	ptrArrayAdd (mmSourceFiles, mm_entry);
}

static stringList *mmTakeOverSourceFiles (void)
{
	ptrArray *tmp = mmSourceFiles;

	mmSourceFiles = NULL;
	return tmp;
}

static int currentPass = 0;

static int mmEnterPass (void)
{
	return --currentPass;
}

extern int mmCurrentPass (void)
{
	return currentPass;
}

extern bool inMMPass (void)
{
	return !(currentPass >= 0);
}

static bool mmRun0 (void)
{
	bool resize = false;
	ptrArray *mm_sources;

	while ((mm_sources = mmTakeOverSourceFiles ()) != NULL)
	{
		mmEnterPass ();
		for (unsigned int i = 0; i < ptrArrayCount (mm_sources); i++)
		{
			struct mmEntry *mm_entry = ptrArrayItem (mm_sources, i);
			Option.language = mm_entry->lang;
			resize = parseFile (mm_entry->fileName)? true: resize;
		}
		ptrArrayDelete (mm_sources);
	}

	return resize;
}


extern bool mmRun (void)
{
	bool r;
	langType tmp = Option.language;

	r = mmRun0 ();

	Option.language = tmp;
	return r;
}
