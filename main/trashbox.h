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

/*
*   INCLUDE FILES
*/

#include "general.h"  /* must always come first */


/*
*   DATA DECLARATIONS
*/

typedef void (* TrashBoxDestroyItemProc) (void *);
typedef struct sTrashBox TrashBox;

/*
*   MACROS
*/

#define DEFAULT_TRASH_BOX(PTR,PROC) trashBoxPut(NULL,PTR,(TrashBoxDestroyItemProc)PROC)
#define DEFAULT_TRASH_BOX_TAKE_BACK(PTR) trashBoxTakeBack(NULL,PTR)

#define PARSER_TRASH_BOX(PTR,PROC) parserTrashBoxPut(PTR,(TrashBoxDestroyItemProc)PROC)
#define PARSER_TRASH_BOX_TAKE_BACK(PTR) parserTrashBoxTakeBack(PTR)


/*
*   FUNCTION PROTOTYPES
*/

extern TrashBox* trashBoxNew       (void);
extern TrashBox* trashBoxStack     (TrashBox* trash_box);
extern void      trashBoxDelete    (TrashBox* trash_box);
extern void*     trashBoxPut       (TrashBox* trash_box, void* item, TrashBoxDestroyItemProc destroy);
extern TrashBoxDestroyItemProc trashBoxTakeBack  (TrashBox* trash_box, void* item);
extern void      trashBoxFree      (TrashBox* trash_box, void* item);
extern void      trashBoxMakeEmpty (TrashBox* trash_box);

/* A parser trash box is prepared when `parser' method of a parser is called.
 * The parser can register a pair of a memory object and destructor for the object to
 * the trash box with parserTrashBoxPut ().
 *
 * The registered memory objects are destructed after `parser' method is finished in
 * the main side. You can delay the destruction with parserTrashBoxPut ().
 *
 * You can unregister the pair in the `parser' method with parserTrashBoxTakeBack ().
 * In that case, specify the memory object of the pair as the argument.
 */
extern void* parserTrashBoxPut  (void* item, TrashBoxDestroyItemProc destroy);
extern TrashBoxDestroyItemProc parserTrashBoxTakeBack  (void* item);

#endif /* CTAGS_MAIN_TRASH_H */
