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
#include <stdint.h>

/* This hashtable allows adding multiple items for the same key.
 *
 * hashTablePutItem() adds a key/item pair to the htable even if the
 * htable has an item for the key already.
 *
 * hashTableGetItem() returns the first occurrence item for the given
 * key.
 *
 * hashTableDeleteItem() deletes the first occurrence item for the given
 * key.
 *
 * Use hashTableForeachItemOnChain () to process all items for the same key.
 */
typedef struct sHashTable hashTable;
typedef unsigned int (* hashTableHashFunc)  (const void * const key);
typedef bool      (* hashTableEqualFunc) (const void* a, const void* b);
typedef void         (* hashTableDeleteFunc)  (void * ptr);

/* To continue interation, return true.
 * To break interation, return false. */
typedef bool         (* hashTableForeachFunc) (const void *key, void *value, void *user_data);

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
					hashTableDeleteFunc keyfreefn,
					hashTableDeleteFunc valfreefn);

/* By default, hashTableGetItem() returns NULL for a unknown key.
 * It means you cannot store NULL as a value for a key.
 * With hashTableSetValueForUnknownKey(), you can specific
 * an alternative address representing the value for for unknown
 * keys.
 */
extern void hashTableSetValueForUnknownKey (hashTable *htable,
											void *val,
											hashTableDeleteFunc valfreefn);

extern void       hashTableDelete      (hashTable *htable);
extern void       hashTableClear       (hashTable *htable);
extern void       hashTablePutItem     (hashTable *htable, void *key, void *value);
extern void*      hashTableGetItem     (hashTable *htable, const void * key);
extern bool    hashTableHasItem     (hashTable * htable, const void * key);
extern bool    hashTableDeleteItem  (hashTable *htable, const void *key);
extern bool       hashTableUpdateItem  (hashTable *htable, void *key, void *value);

/* Return true if proc never returns false; proc returns true for all
 * the items, or htable holds no item.
 *
 * Return false if htable holds at least one item and proc returns false
 * for one of the items. */
extern bool       hashTableForeachItem (hashTable *htable, hashTableForeachFunc proc, void *user_data);

/* This function is useful for htable having multiple items for a key.
 * By giving a key, you can traverse all the items associated with the
 * key via proc.  * "Chain" means group of items associated with a
 * key. */
extern bool       hashTableForeachItemOnChain (hashTable *htable, const void *key, hashTableForeachFunc proc, void *user_data);

extern unsigned int hashTableCountItem   (hashTable *htable);

#define HT_PTR_TO_INT(P) ((int)(intptr_t)(P))
#define HT_INT_TO_PTR(P) ((void*)(intptr_t)(P))
#define HT_PTR_TO_UINT(P) ((unsigned int)(uintptr_t)(P))
#define HT_UINT_TO_PTR(P) ((void*)(uintptr_t)(P))

#endif	/* CTAGS_MAIN_HTABLE_H */
