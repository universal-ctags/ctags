/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*/

#include <stdio.h>
#include <string.h>
#include "vstring.h"
#include "routines.h"
#include "trashbox.h"
#include "debug.h"

typedef TrashBoxDestroyItemProc TrashDestroyItemProc;
typedef struct sTrash {
	void* item;
	struct sTrash* next;
	TrashDestroyItemProc destrctor;
} Trash;

struct sTrashBox {
	Trash *trash;
};

static Trash* trashPut (Trash* trash, void* item,
			TrashDestroyItemProc destrctor);
static Trash* trashTakeBack (Trash* trash, void* item);
static Trash* trashMakeEmpty (Trash* trash);



extern TrashBox* trashBoxNew (void)
{
	TrashBox *t = xMalloc (1, TrashBox);
	t->trash = NULL;
	return t;
}

extern void trashBoxDelete (TrashBox* trash_box)
{
	trashBoxMakeEmpty(trash_box);

	eFree (trash_box);
}

extern void   trashBoxPut (TrashBox* trash_box, void* item, TrashBoxDestroyItemProc destroy)
{
	trash_box->trash = trashPut(trash_box->trash, item, destroy);
}

extern void   trashBoxTakeBack (TrashBox* trash_box, void* item)
{
	trash_box->trash = trashTakeBack(trash_box->trash, item);
}

extern void   trashBoxMakeEmpty (TrashBox* trash_box)
{
  trash_box->trash = trashMakeEmpty (trash_box->trash);
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

static boolean trashTakeBack0  (Trash** trash, void* item)
{
	boolean removed;
	Trash* tmp;

	removed = FALSE;
	while (*trash)
	{
		if ( (*trash)->item ==  item )
		{
			tmp = *trash;
			*trash = (*trash)->next;
			tmp->next = NULL;
			tmp->item = NULL;
			eFree (tmp);
			tmp = NULL;
			removed = TRUE;
			break;
		}
		else
			trash = &(*trash)->next;
	}

	Assert (removed);
	return removed;
}

static Trash* trashTakeBack (Trash* trash, void* item)
{
	trashTakeBack0 (&trash, item);
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

#ifdef TRASH_TEST
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

	trash = trashTakeBack (trash, b);
	eFree (b);

	fputs("expects: dca\nactual: ", stderr);
	for (tmp = trash; tmp; tmp = tmp->next)
		fputs(tmp->item, stderr);
	fputc('\n', stderr);


	trash = trashTakeBack (trash, d);
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
