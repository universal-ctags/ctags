/*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to resizable pointer arrays.
*/
#ifndef CTAGS_MAIN_NUMARRAY_H
#define CTAGS_MAIN_NUMARRAY_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */


#define declNumArray(prefix,Prefix,type)								\
																		\
	struct s##Prefix##Array;											\
	typedef struct s##Prefix##Array prefix##Array;						\
																		\
	extern prefix##Array *prefix##ArrayNew (void);						\
	extern unsigned int prefix##ArrayAdd (prefix##Array *const current, type num); \
	extern void prefix##ArrayRemoveLast (prefix##Array *const current);	\
	extern void prefix##ArrayCombine (prefix##Array *const current, prefix##Array *const from);	\
	extern void prefix##ArrayClear (prefix##Array *const current);		\
	extern unsigned int prefix##ArrayCount (const prefix##Array *const current); \
	extern bool prefix##ArrayIsEmpty(const prefix##Array *const current); \
	extern type prefix##ArrayItem (const prefix##Array *const current, const unsigned int indx); \
	extern type prefix##ArrayLast (const prefix##Array *const current); \
	extern void prefix##ArrayDelete (prefix##Array *const current);		\
	extern bool prefix##ArrayHasTest (const prefix##Array *const current, \
									  bool (*test)(const type num, void *userData), \
									  void *userData);					\
	extern bool prefix##ArrayHas (const prefix##Array *const current, type num); \
	extern void prefix##ArrayReverse (const prefix##Array *const current); \
	extern void prefix##ArrayDeleteItem (prefix##Array* const current, unsigned int indx); \
	\
	extern void prefix##ArraySort (prefix##Array *const current, bool descendingOrder);

declNumArray(char, Char, char)
declNumArray(uchar, Uchar, unsigned char)
declNumArray(int, Int, int)
declNumArray(uint, Uint, unsigned int)
declNumArray(long, Long, long)
declNumArray(ulong, Ulong, unsigned long)

#endif  /* CTAGS_MAIN_NUMARRAY_H */
