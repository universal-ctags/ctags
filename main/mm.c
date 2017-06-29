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

struct barrelEntry {
	tagEntryInfo e;
};

static void barrelEntryDelete (void *p)
{
	struct barrelEntry *b = p;
	clearTagEntry (&(b->e));
	eFree (b);
}

static ptrArray *barrel;

void pourEntryToBarrel (tagEntryInfo *e)
{
	struct barrelEntry * b_entry = xMalloc (1, struct barrelEntry);
	(b_entry->e) = *e;

	if (!barrel)
		barrel = ptrArrayNew (barrelEntryDelete);

	ptrArrayAdd (barrel, b_entry);
}

static Barrel *barrelRollOut (void)
{
	ptrArray *tmp = barrel;
	barrel = NULL;
	return (Barrel *)tmp;
}

void barrelDelete (Barrel * b)
{
	ptrArray *tmp = (ptrArray *)b;
	ptrArrayDelete (tmp);
}

unsigned int countEntryInBarrel (Barrel *b)
{
	return b? ptrArrayCount ((ptrArray *)b): 0;
}

tagEntryInfo* getEntryInBarrel (Barrel *b, unsigned int index)
{
	ptrArray *tmp = (ptrArray *)b;
	return ptrArrayItem (tmp, index);
}

static bool mmRun0 (void)
{
	bool resize = false;
	ptrArray *mm_sources;

	while ((mm_sources = mmTakeOverSourceFiles ()) != NULL)
	{
		Barrel *barrel = barrelRollOut();
		int pass = mmEnterPass ();

		for (unsigned int i = 0; i < ptrArrayCount (mm_sources); i++)
		{
			struct mmEntry *mm_entry = ptrArrayItem (mm_sources, i);
			Option.language = mm_entry->lang;

			setupParserMM (mm_entry->lang, pass, barrel);
			resize = parseFile (mm_entry->fileName)? true: resize;
		}
		ptrArrayDelete (mm_sources);

		barrelDelete (barrel);
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
