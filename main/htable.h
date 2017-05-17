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
typedef unsigned int (* hashTableHashFunc)  (const void * const key);
typedef bool      (* hashTableEqualFunc) (const void* a, const void* b);
typedef void         (* hashTableFreeFunc)  (void * ptr);
typedef void         (* hashTableForeachFunc) (void *key, void *value, void* user_data);

unsigned int hashPtrhash (const void * x);
bool hashPtreq (const void * a, const void * constb);

unsigned int hashCstrhash (const void * x);
bool hashCstreq (const void * a, const void * b);

unsigned int hashCstrcasehash (const void * x);
bool hashCstrcaseeq (const void * a, const void * b);

unsigned int hashInthash (const void * x);
bool hashInteq (const void * a, const void * b);

extern hashTable* hashTableNew         (unsigned int size,
					hashTableHashFunc hashfn,
					hashTableEqualFunc equalfn,
					hashTableFreeFunc keyfreefn,
					hashTableFreeFunc valfreefn);
extern void       hashTableDelete      (hashTable *htable);
extern void       hashTableClear       (hashTable *htable);
extern void       hashTablePutItem     (hashTable *htable, void *key, void *value);
extern void*      hashTableGetItem     (hashTable *htable, const void * key);
extern bool    hashTableHasItem     (hashTable * htable, const void * key);
extern bool    hashTableDeleteItem  (hashTable *htable, void *key);
extern void       hashTableForeachItem (hashTable *htable, hashTableForeachFunc proc, void *user_data);
extern int        hashTableCountItem   (hashTable *htable);

#endif	/* CTAGS_MAIN_HTABLE_H */
