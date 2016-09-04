/*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to resizable pointer arrays.
*/
#ifndef CTAGS_MAIN_PTRARRAY_H
#define CTAGS_MAIN_PTRARRAY_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

/*
*   DATA DECLARATIONS
*/

typedef void (*ptrArrayDeleteFunc) (void *data);

struct sPtrArray;
typedef struct sPtrArray ptrArray;

/*
*   FUNCTION PROTOTYPES
*/

extern ptrArray *ptrArrayNew (ptrArrayDeleteFunc deleteFunc);
extern void ptrArrayAdd (ptrArray *const current, void *ptr);
extern void ptrArrayRemoveLast (ptrArray *const current);
extern void ptrArrayCombine (ptrArray *const current, ptrArray *const from);
extern void ptrArrayClear (ptrArray *const current);
extern unsigned int ptrArrayCount (const ptrArray *const current);
extern void* ptrArrayItem (const ptrArray *const current, const unsigned int indx);
extern void* ptrArrayLast (const ptrArray *const current);
extern void ptrArrayDelete (ptrArray *const current);
extern bool ptrArrayHasInsensitive (const ptrArray *const current, const void *const ptr);
extern bool ptrArrayHasTest (const ptrArray *const current,
				  bool (*test)(const void *ptr, void *userData),
				  void *userData);
extern void ptrArrayReverse (const ptrArray *const current);
extern void ptrArrayDeleteItem (ptrArray* const current, unsigned int indx);

#endif  /* CTAGS_MAIN_PTRARRAY_H */
