/*
*   Copyright (c) 2016, Jiri Techet
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines generic pool for object reuse reducing the amount of allocations
*   and deallocations.
*/
#ifndef CTAGS_MAIN_OBJPOOL_H
#define CTAGS_MAIN_OBJPOOL_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "ptrarray.h"

/*
*   DATA DECLARATIONS
*/
typedef void * (*objPoolCreateFunc) (void *createArg);
typedef void (*objPoolDeleteFunc) (void *data);
typedef void (*objPoolClearFunc) (void *data);

struct sObjPool;
typedef struct sObjPool objPool;

/*
*   FUNCTION PROTOTYPES
*/
extern objPool *objPoolNew (unsigned int size,
	objPoolCreateFunc createFunc, objPoolDeleteFunc deleteFunc, objPoolClearFunc clearFunc,
	void *createArg);
extern void objPoolDelete (objPool *pool);
extern void *objPoolGet (objPool *pool);
extern void objPoolPut (objPool *pool, void *obj);

#endif  /* CTAGS_MAIN_OBJPOOL_H */
