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

#include "debug.h"
#include "numarray.h"
#include "routines.h"

#include <stdlib.h>
#include <string.h>

#define impNumArray(prefix,Prefix,type)									\
																		\
	struct s##Prefix##Array {											\
		unsigned int max;												\
		unsigned int count;												\
		type *array;													\
	};																	\
																		\
	extern prefix##Array *prefix##ArrayNew (void)						\
	{																	\
		prefix##Array* const result = xMalloc (1, prefix##Array);		\
		result->max = 8;												\
		result->count = 0;												\
		result->array = xMalloc (result->max, type);					\
		return result;													\
	}																	\
																		\
	extern unsigned int prefix##ArrayAdd (prefix##Array *const current, type num) \
	{																	\
		Assert (current != NULL);										\
		if (current->count == current->max)								\
		{																\
			current->max *= 2;											\
			current->array = xRealloc (current->array, current->max, type);	\
		}																\
		current->array [current->count] = num;							\
		return current->count++;										\
	}																	\
																		\
	extern void prefix##ArrayRemoveLast (prefix##Array *const current)	\
	{																	\
		Assert (current != NULL);										\
		Assert (current->count > 0);									\
		--current->count;												\
	}																	\
																		\
	extern void prefix##ArrayCombine (prefix##Array *const current, prefix##Array *const from) \
	{																	\
		unsigned int i;													\
		Assert (current != NULL);										\
		Assert (from != NULL);											\
		for (i = 0  ;  i < from->count  ;  ++i)							\
			prefix##ArrayAdd (current, from->array [i]);				\
		from->count = 0;												\
		prefix##ArrayDelete (from);										\
	}																	\
																		\
	extern unsigned int prefix##ArrayCount (const prefix##Array *const current)	\
	{																	\
		Assert (current != NULL);										\
		return current->count;											\
	}																	\
																		\
	extern bool prefix##ArrayIsEmpty (const prefix##Array *const current)	\
	{																	\
		return (prefix##ArrayCount(current) == 0);						\
	}																	\
																		\
	extern type prefix##ArrayItem (const prefix##Array *const current, const unsigned int indx)	\
	{																	\
		Assert (current != NULL);										\
		return current->array [indx];									\
	}																	\
																		\
	extern type prefix##ArrayLast (const prefix##Array *const current)	\
	{																	\
		Assert (current != NULL);										\
		Assert (current->count > 0);									\
		return current->array [current->count - 1];						\
	}																	\
																		\
	extern void prefix##ArrayClear (prefix##Array *const current)		\
	{																	\
		Assert (current != NULL);										\
		current->count = 0;												\
	}																	\
																		\
	extern void prefix##ArrayDelete (prefix##Array *const current)		\
	{																	\
		if (current != NULL)											\
		{																\
			prefix##ArrayClear (current);								\
			eFree (current->array);										\
			eFree (current);											\
		}																\
	}																	\
																		\
	extern bool prefix##ArrayHasTest (const prefix##Array *const current, \
									  bool (*test)(const type num, void *userData),	\
									  void *userData)					\
	{																	\
		bool result = false;											\
		unsigned int i;													\
		Assert (current != NULL);										\
		for (i = 0  ;  ! result  &&  i < current->count  ;  ++i)		\
			result = (*test)(current->array [i], userData);				\
		return result;													\
	}																	\
																		\
	static bool prefix##Eq (const type num, void *userData)				\
	{																	\
		type *num0 = userData;											\
		return (num == *num0);											\
	}																	\
																		\
	extern bool prefix##ArrayHas (const prefix##Array *const current, type num)	\
	{																	\
		return prefix##ArrayHasTest (current, prefix##Eq, &num);		\
	}																	\
																		\
	extern void prefix##ArrayReverse (const prefix##Array *const current) \
	{																	\
		unsigned int i, j;												\
		type tmp;														\
																		\
		Assert (current != NULL);										\
		for (i = 0, j = current->count - 1 ; i <  (current->count / 2); ++i, --j) \
		{																\
			tmp = current->array[i];									\
			current->array[i] = current->array[j];						\
			current->array[j] = tmp;									\
		}																\
	}																	\
																		\
	extern void prefix##ArrayDeleteItem (prefix##Array* const current, unsigned int indx) \
	{																	\
		memmove (current->array + indx, current->array + indx + 1,		\
				 (current->count - indx) * sizeof (*current->array));	\
		--current->count;												\
	}																	\
	static int prefix##GreaterThan(const void *a, const void *b)		\
    {																	\
		type an = *(type *)a;											\
		type bn = *(type *)b;											\
		if (an > bn)													\
			return 1;													\
		else if (an == bn)												\
			return 0;													\
		else															\
			return -1;													\
	}																	\
	static int prefix##LessThan(const void *a, const void *b)			\
	{																	\
		return prefix##GreaterThan (b, a);								\
	}																	\
	extern void prefix##ArraySort (prefix##Array *const current, bool descendingOrder) \
	{																	\
		if (descendingOrder)											\
			qsort (current->array, current->count, sizeof (type), prefix##GreaterThan); \
		else															\
			qsort (current->array, current->count, sizeof (type), prefix##LessThan); \
	}

/* We expect the linker we use is enough clever to delete dead code. */
impNumArray(char, Char, char)
impNumArray(uchar, Uchar, unsigned char)
impNumArray(int, Int, int)
impNumArray(uint, Uint, unsigned int)
impNumArray(long, Long, long)
impNumArray(ulong, Ulong, unsigned long)
