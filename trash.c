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
#include "trash.h"
#include "debug.h"

extern Trash* trashPut (Trash* trash, void* item)
{
	return trashPutFull (trash, item, eFree);
}

extern Trash* trashPutFull (Trash* trash, void* item,
			    trashDestrctorProc destrctor)
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

extern Trash* trashTakeBack (Trash* trash, void* item)
{
	trashTakeBack0 (&trash, item);
	return trash;
}

extern Trash* trashMakeEmpty (Trash* trash)
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
