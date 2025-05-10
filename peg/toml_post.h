/*
 *   Copyright (c) 2024 Masatake YAMATO
 *   Copyright (c) 2024 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for TOML.
 */


/*
*   INCLUDE FILES
*/
#include "options.h"
#include "parse.h"
#include "routines.h"

/*
* FUNCTION DEFINITIONS
*/
static void markTableEnd (struct parserCtx *auxil, long offset)
{
	tagEntryInfo *e = getEntryInCorkQueue (auxil->lastTableIndex);
	if (!e)
		return;

	unsigned long lineNumber = (offset == -1)
		? getInputLineNumber()
		: getInputLineNumberForFileOffset (translateFileOffset (offset));

	setTagEndLine(e, ((offset != -1 && lineNumber > 1)
					  ? lineNumber - 1
					  : lineNumber));

	auxil->lastTableIndex = CORK_NIL;
	for (unsigned int i = uintArrayLast (auxil->unwind_depths); i > 0; i--)
		POP_SCOPE (auxil);
	uintArrayRemoveLast (auxil->unwind_depths);
}

static vString *makeVStringFromKeyQueue (ptrArray *keyQueue)
{
	vString *vname = vStringNew ();

	for (size_t i = 0; i < ptrArrayCount (keyQueue); i++)
	{
		const char *key = ptrArrayItem (keyQueue, i);
		vStringCatS (vname, key);
		if (i != ptrArrayCount (keyQueue) - 1)
			vStringPut (vname, '.');
	}

	return vname;
}

static void makeTagsForKeys (struct parserCtx *auxil, unsigned long lineNumber, MIOPos filePosition,
							 int last_key_index0)
{
	int last_key_index = last_key_index0;
	for (size_t i = 0; i < ptrArrayCount (auxil->keyQueue); i++)
	{
		tagEntryInfo e;
		const char *key = ptrArrayItem (auxil->keyQueue, i);
		if (i == ptrArrayCount (auxil->keyQueue) - 1)
			initTagEntry (&e, key, TOML_K_KEY);
		else
			initRefTagEntry (&e, key, TOML_K_KEY, TOML_R_KEY_CHAINELT);
		e.lineNumber = lineNumber;
		e.filePosition = filePosition;
		e.extensionFields.scopeIndex = last_key_index;
		if (i == ptrArrayCount (auxil->keyQueue) - 1)
			;/* TODO: Attach partOf:vStringValue(vname) */
		last_key_index= makeTagEntry (&e);
		if (i == ptrArrayCount (auxil->keyQueue) - 1)
		{
			SET_SCOPE(auxil, last_key_index);
			uintArrayAdd (auxil->unwind_depths, ptrArrayCount (auxil->keyQueue));
		}
	}
}

static void tableKeyStart (struct parserCtx *auxil, bool isArrayTable, long offset)
{
	if (auxil->keyQueue == NULL)
		auxil->keyQueue = ptrArrayNew (eFree);
	auxil->isArrayTable = isArrayTable;
	auxil->keyOffset = offset;

	if (auxil->lastTableIndex != CORK_NIL)
		markTableEnd (auxil, offset);
}

static void tableKeyEnd (struct parserCtx *auxil)
{
	tagEntryInfo e;
	long abs_offset = translateFileOffset (auxil->keyOffset);
	unsigned long lineNumber = getInputLineNumberForFileOffset (abs_offset);
	MIOPos filePosition = getInputFilePositionForLine (lineNumber);

	vString *vname = makeVStringFromKeyQueue (auxil->keyQueue);
	const char *name = vStringValue (vname);

	initTagEntry (&e, name, auxil->isArrayTable? TOML_K_ARRAYTABLE: TOML_K_TABLE);
	e.lineNumber = lineNumber;
	e.filePosition = filePosition;

	if (auxil->isArrayTable)
	{
		if (hashTableHasItem (auxil->arrayTableCounters, name))
		{
			void *tmp = hashTableGetItem (auxil->arrayTableCounters, name);
			int num = HT_PTR_TO_INT(tmp);
			e.extensionFields.nth = num;
			num++;
			hashTableUpdateItem (auxil->arrayTableCounters, name, HT_INT_TO_PTR(num));
		}
		else
		{
			e.extensionFields.nth = 0;
			char *key = eStrdup (name);
			hashTablePutItem (auxil->arrayTableCounters, key, HT_INT_TO_PTR(1));
		}
	}

	int table_index = makeTagEntry (&e);
	auxil->lastTableIndex = table_index;

	makeTagsForKeys (auxil, lineNumber, filePosition, CORK_NIL);
	ptrArrayClear (auxil->keyQueue);

	vStringDelete (vname);		/* NULL is acceptable. */
}

static void keyvalStart (struct parserCtx *auxil, long offset)
{
	if (auxil->keyQueue == NULL)
		auxil->keyQueue = ptrArrayNew (eFree);
	auxil->isArrayTable = false;
	auxil->keyOffset = offset;
}

static void keyvalKeyEnd (struct parserCtx *auxil)
{
	tagEntryInfo e;
	long abs_offset = translateFileOffset (auxil->keyOffset);
	unsigned long lineNumber = getInputLineNumberForFileOffset (abs_offset);
	MIOPos filePosition = getInputFilePositionForLine (lineNumber);

	vString *vname = makeVStringFromKeyQueue (auxil->keyQueue);
	const char *name = vStringValue (vname);

	initTagEntry (&e, name, TOML_K_QKEY);
	e.lineNumber = lineNumber;
	e.filePosition = filePosition;
	e.extensionFields.scopeIndex = auxil->lastTableIndex;
	makeTagEntry (&e);

	makeTagsForKeys (auxil, lineNumber, filePosition, BASE_SCOPE(auxil));
	ptrArrayClear (auxil->keyQueue);

	vStringDelete (vname);		/* NULL is acceptable. */
}

static void keyvalValEnd (struct parserCtx *auxil)
{
	for (unsigned int i = uintArrayLast (auxil->unwind_depths); i > 0; i--)
		POP_SCOPE (auxil);
	uintArrayRemoveLast (auxil->unwind_depths);
}

static void queueKey (struct parserCtx *auxil, const char *name)
{
	if (auxil->keyQueue)
	{
		char *key = eStrdup (name);
		ptrArrayAdd (auxil->keyQueue, key);
	}
}

static void notifyValue (struct parserCtx *auxil, const char *value, long offset)
{
	subparser *sub;
	tomlSubparser *tomlsub = NULL;

	foreachSubparser (sub, false)
	{
		tomlsub = (tomlSubparser *)sub;
		if (tomlsub->valueNotify)
		{
			enterSubparser(sub);
			tomlsub->valueNotify(tomlsub, value, offset, BASE_SCOPE(auxil));
			leaveSubparser();
		}
	}
}

static void ctxInit (struct parserCtx *auxil)
{
	BASE_INIT(auxil, KIND_GHOST_INDEX);
	auxil->keyQueue = NULL;
	auxil->lastTableIndex = CORK_NIL;
	auxil->arrayTableCounters = hashTableNew (7,
											  hashCstrhash,
											  hashCstreq,
											  eFree,
											  NULL);
	auxil->unwind_depths = uintArrayNew ();
}

static void ctxFini (struct parserCtx *auxil)
{
	if (auxil->lastTableIndex != CORK_NIL)
		markTableEnd (auxil, -1);

	BASE_FINI(auxil);
	if (auxil->keyQueue)
		ptrArrayDelete (auxil->keyQueue);
	hashTableDelete (auxil->arrayTableCounters);
	uintArrayDelete (auxil->unwind_depths);
}

static void findTomlTags (void)
{
	struct parserCtx auxil;

	ctxInit (&auxil);
	ptoml_context_t *pctx = ptoml_create(&auxil);

	while (ptoml_parse(pctx, NULL) && (!BASE_ERROR(&auxil)) );

	ptoml_destroy(pctx);
	while (getcFromInputFile() != EOF);
	ctxFini (&auxil);
}

extern parserDefinition* TomlParser (void)
{
	static const char *const extensions [] = { "toml", NULL };
	parserDefinition* def = parserNew ("TOML");
	def->kindTable  = TomlKinds;
	def->kindCount  = ARRAY_SIZE (TomlKinds);
	def->extensions = extensions;
	def->parser     = findTomlTags;
	def->useCork    = true;
	def->requestAutomaticFQTag = false;

	def->enabled    = false;	/* This parser is broken. */

	return def;
}
