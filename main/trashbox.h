/*
*
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Facility for delayed memory releasing, inspired from AutoreleasePool
*   of OpenStep.
*/

#ifndef CTAGS_MAIN_TRASH_H
#define CTAGS_MAIN_TRASH_H

typedef void (* TrashBoxDestroyItemProc) (void *);
typedef struct sTrashBox TrashBox;

extern TrashBox* trashBoxNew       (void);
extern TrashBox* trashBoxStack     (TrashBox* trash_box);
extern void      trashBoxDelete    (TrashBox* trash_box);
extern void*     trashBoxPut       (TrashBox* trash_box, void* item, TrashBoxDestroyItemProc destroy);
extern TrashBoxDestroyItemProc trashBoxTakeBack  (TrashBox* trash_box, void* item);
extern void      trashBoxFree      (TrashBox* trash_box, void* item);
extern void      trashBoxMakeEmpty (TrashBox* trash_box);

#define DEFAULT_TRASH_BOX(PTR,PROC) trashBoxPut(NULL,PTR,(TrashBoxDestroyItemProc)PROC)
#define DEFAULT_TRASH_BOX_TAKE_BACK(PTR) trashBoxTakeBack(NULL,PTR)

#define PARSER_TRASH_BOX(PTR,PROC) parserTrashBoxPut(PTR,(TrashBoxDestroyItemProc)PROC)
#define PARSER_TRASH_BOX_TAKE_BACK(PTR) parserTrashBoxTakeBack(PTR)

extern void initDefaultTrashBox (void);
extern void finiDefaultTrashBox  (void);

extern void initParserTrashBox (void);
extern void finiParserTrashBox  (void);
extern void* parserTrashBoxPut  (void* item, TrashBoxDestroyItemProc destroy);
extern TrashBoxDestroyItemProc parserTrashBoxTakeBack  (void* item);

#endif /* CTAGS_MAIN_TRASH_H */
