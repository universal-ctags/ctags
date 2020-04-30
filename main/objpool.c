/*
*   Copyright (c) 2016, Jiri Techet
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines generic pool for object reuse reducing the amount of allocations
*   and deallocations.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "debug.h"
#include "routines.h"
#include "objpool.h"

/*
*   DATA DECLARATIONS
*/

struct sObjPool {
	ptrArray *array;
	unsigned int size;
	objPoolCreateFunc createFunc;
	objPoolDeleteFunc deleteFunc;
	objPoolClearFunc clearFunc;
	void *createArg;
};

/*
*   FUNCTION DEFINITIONS
*/
extern objPool *objPoolNew (unsigned int size,
	objPoolCreateFunc createFunc, objPoolDeleteFunc deleteFunc, objPoolClearFunc clearFunc,
	void *createArg)
{
	objPool* const result = xMalloc (1, objPool);
	result->array = ptrArrayNew (deleteFunc);
	result->size = size;
	result->createFunc = createFunc;
	result->deleteFunc = deleteFunc;
	result->clearFunc = clearFunc;
	result->createArg = createArg;
	return result;
}

extern void objPoolDelete (objPool *pool)
{
	ptrArrayDelete (pool->array);
	eFree (pool);
}

extern void *objPoolGet (objPool *pool)
{
	void *obj;

	if (ptrArrayCount (pool->array) > 0)
	{
		obj = ptrArrayLast (pool->array);
		ptrArrayRemoveLast (pool->array);
	}
	else
		obj = pool->createFunc (pool->createArg);

	if (pool->clearFunc)
		pool->clearFunc (obj);

	return obj;
}

extern void objPoolPut (objPool *pool, void *obj)
{
	if (obj == NULL)
		return;

	if (
#ifdef DISABLE_OBJPOOL
		0 &&
#endif
		ptrArrayCount (pool->array) < pool->size
		)
		ptrArrayAdd (pool->array, obj);
	else
		pool->deleteFunc (obj);
}
