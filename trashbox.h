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

typedef void (* TrashBoxDestroyItemProc) (void *);
typedef struct sTrashBox TrashBox;

extern TrashBox* trashBoxNew       (void);
extern void      trashBoxDelete   (TrashBox* trash_box);
extern void      trashBoxPut       (TrashBox* trash_box, void* item, TrashBoxDestroyItemProc destroy);
extern void      trashBoxTakeBack  (TrashBox* trash_box, void* item);
extern void      trashBoxMakeEmpty (TrashBox* trash_box);

#endif /* _TRASH_H */
