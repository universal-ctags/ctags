/*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions managing resizable pointer arrays.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <stdlib.h>

#include "debug.h"
#include "ptrarray.h"
#include "routines.h"

/*
*   DATA DECLARATIONS
*/

struct sPtrArray {
	unsigned int max;
	unsigned int count;
	void **array;
	ptrArrayDeleteFunc deleteFunc;
};

/*
*   FUNCTION DEFINITIONS
*/

extern ptrArray *ptrArrayNew (ptrArrayDeleteFunc deleteFunc)
{
	ptrArray* const result = xMalloc (1, ptrArray);
	result->max = 8;
	result->count = 0;
	result->array = xMalloc (result->max, void*);
	result->deleteFunc = deleteFunc;
	return result;
}

extern unsigned int ptrArrayAdd (ptrArray *const current, void *ptr)
{
	Assert (current != NULL);
	if (current->count == current->max)
	{
		current->max *= 2;
		current->array = xRealloc (current->array, current->max, void*);
	}
	current->array [current->count] = ptr;
	return current->count++;
}

extern bool ptrArrayUpdate (ptrArray *const current,
							unsigned int indx, void *ptr, void *padding)
{
	Assert (current != NULL);
	if (current->count > indx)
	{
		void *r = current->array [indx];
		if (current->deleteFunc)
			current->deleteFunc (r);
		current->array [indx] = ptr;
		return true;
	}
	else
	{
		unsigned int c = indx - current->count;
		for (unsigned int i = 0; i < c; i++)
			ptrArrayAdd (current, padding);
		ptrArrayAdd (current, ptr);
		return false;
	}

}
extern void *ptrArrayRemoveLast (ptrArray *const current)
{
	Assert (current != NULL);
	Assert (current->count > 0);
	void *r = ptrArrayLast (current);
	--current->count;
	return r;
}

extern void  ptrArrayDeleteLastInBatch (ptrArray *const current, unsigned int count)
{
	Assert (current != NULL);
	Assert (current->count >= count);
	while (count > 0)
	{
		void *r = ptrArrayLast (current);
		if (current->deleteFunc)
			current->deleteFunc (r);
		--current->count;
		--count;
	}
}

/* Combine array `from' into `current', deleting `from' */
extern void ptrArrayCombine (ptrArray *const current, ptrArray *const from)
{
	unsigned int i;
	Assert (current != NULL);
	Assert (from != NULL);
	for (i = 0  ;  i < from->count  ;  ++i)
		ptrArrayAdd (current, from->array [i]);
	from->count = 0;
	ptrArrayDelete (from);
}

extern unsigned int ptrArrayCount (const ptrArray *const current)
{
	Assert (current != NULL);
	return current->count;
}

extern void* ptrArrayItem (const ptrArray *const current, const unsigned int indx)
{
	Assert (current != NULL);
	Assert (current->count > indx);
	return current->array [indx];
}

extern void* ptrArrayItemFromLast (const ptrArray *const current, const unsigned int indx)
{
	Assert (current != NULL);
	Assert (current->count > indx);
	return current->array [current->count - 1 - indx];
}

extern void ptrArrayClear (ptrArray *const current)
{
	Assert (current != NULL);
	if (current->deleteFunc)
	{
		unsigned int i;
		for (i = 0  ;  i < current->count  ;  ++i)
			current->deleteFunc (current->array [i]);
	}
	current->count = 0;
}

extern void ptrArrayDelete (ptrArray *const current)
{
	if (current != NULL)
	{
		ptrArrayClear (current);
		eFree (current->array);
		eFree (current);
	}
}

extern bool ptrArrayHasTest (const ptrArray *const current,
				  bool (*test)(const void *ptr, void *userData),
				  void *userData)
{
	bool result = false;
	unsigned int i;
	Assert (current != NULL);
	for (i = 0  ;  ! result  &&  i < current->count  ;  ++i)
		result = (*test)(current->array [i], userData);
	return result;
}

static bool ptrEq (const void *ptr, void *userData)
{
	return (ptr == userData);
}

extern bool ptrArrayHas (const ptrArray *const current, void *ptr)
{
	return ptrArrayHasTest (current, ptrEq, ptr);
}

extern void ptrArrayReverse (const ptrArray *const current)
{
	unsigned int i, j;
	void *tmp;

	Assert (current != NULL);
	for (i = 0, j = current->count - 1 ; i <  (current->count / 2); ++i, --j)
	{
		tmp = current->array[i];
		current->array[i] = current->array[j];
		current->array[j] = tmp;
	}
}

extern void ptrArrayDeleteItem (ptrArray* const current, unsigned int indx)
{
	void *ptr = current->array[indx];

	if (current->deleteFunc)
		current->deleteFunc (ptr);

	memmove (current->array + indx, current->array + indx + 1,
			(current->count - indx) * sizeof (*current->array));
	--current->count;
}

extern void*ptrArrayRemoveItem (ptrArray* const current, unsigned int indx)
{
	void *ptr = current->array[indx];

	memmove (current->array + indx, current->array + indx + 1,
			(current->count - indx) * sizeof (*current->array));
	--current->count;

	return ptr;
}

extern void ptrArrayInsertItem (ptrArray* const current, unsigned int indx, void *ptr)
{
	Assert (current != NULL);
	if (current->count == current->max)
	{
		current->max *= 2;
		current->array = xRealloc (current->array, current->max, void*);
	}

	memmove (current->array + indx + 1, current->array + indx,
			 (current->count - indx) * sizeof (*current->array));
	current->array[indx] = ptr;
	++current->count;
}

static int (*ptrArraySortCompareVar)(const void *, const void *);

static int ptrArraySortCompare(const void *a0, const void *b0)
{
	void *const *a = (void *const *)a0;
	void *const *b = (void *const *)b0;

	return ptrArraySortCompareVar (*a, *b);
}

extern void ptrArraySort (ptrArray *const current, int (*compare)(const void *, const void *))
{
	ptrArraySortCompareVar = compare;
	qsort (current->array, current->count, sizeof (void *), ptrArraySortCompare);
}
