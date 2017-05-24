/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*/

#include "general.h"

#include "debug.h"
#include "routines.h"
#include "trashbox.h"

typedef TrashBoxDestroyItemProc TrashDestroyItemProc;
typedef struct sTrash {
	void* item;
	struct sTrash* next;
	TrashDestroyItemProc destrctor;
} Trash;

struct sTrashBox {
	Trash *trash;
};

static TrashBox* defaultTrashBox;
static TrashBox* parserTrashBox;

static Trash* trashPut (Trash* trash, void* item,
			TrashDestroyItemProc destrctor);
static Trash* trashTakeBack (Trash* trash, void* item, TrashDestroyItemProc* destrctor);
static Trash* trashMakeEmpty (Trash* trash);

extern TrashBox* trashBoxNew (void)
{
	TrashBox *t = xMalloc (1, TrashBox);
	t->trash = NULL;
	return t;
}

extern TrashBox* trashBoxStack     (TrashBox* trash_box)
{
	TrashBox *t = trashBoxNew();

	if (!trash_box)
		trash_box = defaultTrashBox;

	trashBoxPut (trash_box, t, (TrashBoxDestroyItemProc) trashBoxDelete);

	return t;
}

extern void trashBoxDelete (TrashBox* trash_box)
{
	if (!trash_box)
		trash_box = defaultTrashBox;

	trashBoxMakeEmpty(trash_box);

	eFree (trash_box);
}

extern void*  trashBoxPut (TrashBox* trash_box, void* item, TrashBoxDestroyItemProc destroy)
{
	if (!trash_box)
		trash_box = defaultTrashBox;

	trash_box->trash = trashPut(trash_box->trash, item, destroy);
	return item;
}

extern TrashBoxDestroyItemProc trashBoxTakeBack (TrashBox* trash_box, void* item)
{
	TrashBoxDestroyItemProc d;

	if (!trash_box)
		trash_box = defaultTrashBox;

	trash_box->trash = trashTakeBack(trash_box->trash, item, &d);
	return d;
}

extern void   trashBoxMakeEmpty (TrashBox* trash_box)
{
	if (!trash_box)
		trash_box = defaultTrashBox;

	trash_box->trash = trashMakeEmpty (trash_box->trash);
}


extern void      trashBoxFree      (TrashBox* trash_box, void* item)
{
	TrashBoxDestroyItemProc d;

	if (!trash_box)
		trash_box = defaultTrashBox;

	d = trashBoxTakeBack (trash_box, item);
	d (item);
}

static Trash* trashPut (Trash* trash, void* item,
			TrashDestroyItemProc destrctor)
{
	Trash* t = xMalloc (1, Trash);
	t->next = trash;
	t->item = item;
	t->destrctor = destrctor? destrctor: eFree;
	return t;
}

static TrashBoxDestroyItemProc trashTakeBack0  (Trash** trash, void* item)
{
	TrashBoxDestroyItemProc removed;
	Trash* tmp;

	removed = NULL;
	while (*trash)
	{
		if ( (*trash)->item ==  item )
		{
			tmp = *trash;
			*trash = (*trash)->next;
			tmp->next = NULL;
			tmp->item = NULL;
			removed = tmp->destrctor;

			eFree (tmp);
			tmp = NULL;
			break;
		}
		else
			trash = &(*trash)->next;
	}

	Assert (removed);
	return removed;
}

static Trash* trashTakeBack (Trash* trash, void* item, TrashDestroyItemProc *destrctor)
{
	TrashDestroyItemProc d;
	d = trashTakeBack0 (&trash, item);
	if (destrctor)
		*destrctor = d;

	return trash;
}

static Trash* trashMakeEmpty (Trash* trash)
{
	Trash* tmp;

	while (trash)
	{
		tmp = trash;
		trash = trash->next;
		tmp->destrctor (tmp->item);
		tmp->item = NULL;
		tmp->destrctor = NULL;
		eFree (tmp);
	}
	return NULL;
}

extern void initDefaultTrashBox (void)
{
	defaultTrashBox = trashBoxNew ();
}

extern void finiDefaultTrashBox (void)
{
	trashBoxDelete (defaultTrashBox);
	defaultTrashBox = NULL;
}

extern void initParserTrashBox (void)
{
	parserTrashBox = trashBoxNew ();
}

extern void finiParserTrashBox  (void)
{
	trashBoxDelete (parserTrashBox);
	parserTrashBox = NULL;
}

extern void* parserTrashBoxPut  (void* item, TrashBoxDestroyItemProc destroy)
{
	return trashBoxPut(parserTrashBox, item, destroy);
}

extern TrashBoxDestroyItemProc parserTrashBoxTakeBack  (void* item)
{
	return trashBoxTakeBack(parserTrashBox, item);
}

#ifdef TRASH_TEST
#include <stdio.h>

int main (void)
{
	Trash* trash = NULL;
	Trash* tmp;
	char* d = eStrdup ("d");
	char* b = eStrdup ("b");

	trash = trashPut (trash, eStrdup ("a"));
	trash = trashPut (trash, b);
	trash = trashPut (trash, eStrdup ("c"));
	trash = trashPut (trash, d);

	trash = trashTakeBack (trash, b, NULL);
	eFree (b);

	fputs("expects: dca\nactual: ", stderr);
	for (tmp = trash; tmp; tmp = tmp->next)
		fputs(tmp->item, stderr);
	fputc('\n', stderr);


	trash = trashTakeBack (trash, d, NULL);
	eFree (d);

	fputs("expects: ca\nactual: ", stderr);
	for (tmp = trash; tmp; tmp = tmp->next)
		fputs(tmp->item, stderr);
	fputc('\n', stderr);

	trash = trashMakeEmpty (trash);

	fputs("expects: \nactual: ", stderr);
	for (tmp = trash; tmp; tmp = tmp->next)
		fputs(tmp->item, stderr);
	fputc('\n', stderr);
	return 0;
}

#include <stdlib.h>
void eFree (void *ptr) { free(ptr); }
void *eMalloc (const size_t size) { return malloc(size); }
char *eStrdup (const char* str) { return strdup(str); }
#endif
