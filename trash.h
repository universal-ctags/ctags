/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Facility for delayed memory releasing, insptired from AutoreleasePool
*   of OpenStep.
*/

#ifndef _TRASH_H
#define _TRASH_H

#include <setjmp.h>

typedef void (* trashDestrctorProc) (void *);

typedef struct sTrash {
	void* item;
	struct sTrash* next;
	trashDestrctorProc destrctor;
} Trash;

extern Trash* trashPut     (Trash* trash, void* item);
extern Trash* trashPutFull (Trash* trash, void* item,
			    trashDestrctorProc destrctor);
extern Trash* trashTakeBack (Trash* trash, void* item);
extern Trash* trashMakeEmpty (Trash* trash);

#endif /* _TRASH_H */
