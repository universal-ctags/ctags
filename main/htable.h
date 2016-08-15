/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines hashtable
*/
#ifndef CTAGS_MAIN_HTABLE_H
#define CTAGS_MAIN_HTABLE_H

#include "general.h"

typedef struct sHashTable hashTable;
typedef unsigned int (* hashTableHashFunc)  (void * key);
typedef boolean      (* hashTableEqualFunc) (void* a, void* b);
typedef void         (* hashTableFreeFunc)  (void * ptr);
typedef void         (* hashTableForeachFunc) (void *key, void *value, void* user_data);

unsigned int hashPtrhash (void * x);
boolean hashPtreq (void *a, void *b);

unsigned int hashCstrhash (void * x);
boolean hashCstreq (void *a, void *b);

unsigned int hashCstrhash (void * x);
boolean hashCstreq (void *a, void *b);

unsigned int hashInthash (void *x);
boolean hashInteq (void *a, void *b);

extern hashTable* hashTableNew         (unsigned int size,
					hashTableHashFunc hashfn,
					hashTableEqualFunc equalfn,
					hashTableFreeFunc keyfreefn,
					hashTableFreeFunc valfreefn);
extern void       hashTableDelete      (hashTable *htable);
extern void       hashTablePutItem     (hashTable *htable, void *key, void *value);
extern void*      hashTableGetItem     (hashTable *htable, void *key);
extern boolean    hashTableHasItem     (hashTable *htable, void *key);
extern boolean    hashTableDeleteItem  (hashTable *htable, void *key);
extern void       hashTableForeachItem (hashTable *htable, hashTableForeachFunc proc, void *user_data);
extern int        hashTableCountItem   (hashTable *htable);

#endif	/* CTAGS_MAIN_HTABLE_H */
