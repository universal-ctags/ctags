/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines hashtable
*/

#include "htable.h"

#ifndef MAIN
#include <stdio.h>
#include "routines.h"
#else
#include <stdlib.h>
#ifndef xCalloc
#define xCalloc(n,Type)    (Type *)calloc((size_t)(n), sizeof (Type))
#endif
#ifndef xMalloc
#define xMalloc(n,Type)    (Type *)malloc((size_t)(n) * sizeof (Type))
#endif
#ifndef eFree
#define eFree(x) free(x)
#endif
#endif	/* MAIN */

typedef struct sHashEntry hentry;
struct sHashEntry {
	void *key;
	void *value;
	hentry *next;
};

struct sHashTable {
	hentry** table;
	unsigned int size;
	hashTableHashFunc hashfn;
	hashTableEqualFunc equalfn;
	hashTableFreeFunc keyfreefn;
	hashTableFreeFunc valfreefn;
};


static hentry* entry_new (void *key, void *value, hentry* next)
{
	hentry* entry = xMalloc (1, hentry);

	entry->key = key;
	entry->value = value;
	entry->next = next;

	return entry;
}

static hentry* entry_destroy (hentry* entry,
			      hashTableFreeFunc keyfreefn,
			      hashTableFreeFunc valfreefn)
{
	hentry* tmp;

	keyfreefn (entry->key);
	valfreefn (entry->value);
	entry->key = NULL;
	entry->value = NULL;
	tmp = entry->next;
	eFree (entry);

	return tmp;
}

static void  entry_reclaim (hentry* entry,
			    hashTableFreeFunc keyfreefn,
			    hashTableFreeFunc valfreefn)
{
	while (entry)
		entry = entry_destroy (entry, keyfreefn, valfreefn);
}

static void *entry_find (hentry* entry, void* key, hashTableEqualFunc equalfn)
{
	while (entry)
	{
		if (equalfn( key, entry->key))
			return entry->value;
	}
	return NULL;
}

static boolean		entry_delete (hentry **entry, void *key, hashTableEqualFunc equalfn,
			      hashTableFreeFunc keyfreefn, hashTableFreeFunc valfreefn)
{
	while (*entry)
	{
		if (equalfn (key, (*entry)->key))
		{
			*entry = entry_destroy (*entry, keyfreefn, valfreefn);
			return TRUE;
		}

	}
	return FALSE;
}

static void  entry_foreach (hentry *entry, hashTableForeachFunc proc, void *user_data)
{
	while (entry)
	{
		proc (entry->key, entry->value, user_data);
		entry = entry->next;
	}
}

extern hashTable *hashTableNew    (unsigned int size,
				   hashTableHashFunc hashfn,
				   hashTableEqualFunc equalfn,
				   hashTableFreeFunc keyfreefn,
				   hashTableFreeFunc valfreefn)
{
	hashTable *htable;

	htable = xMalloc (1, hashTable);
	htable->size = size;
	htable->table = xCalloc (size, hentry*);

	htable->hashfn = hashfn;
	htable->equalfn = equalfn;
	htable->keyfreefn = keyfreefn;
	htable->valfreefn = valfreefn;

	return htable;
}

extern void       hashTableDelete (hashTable *htable)
{
	unsigned int i;
	if (!htable)
		return;

	for (i = 0; i < htable->size; i++)
	{
		hentry *entry;

		entry = htable->table[i];
		entry_reclaim (entry, htable->keyfreefn, htable->valfreefn);
		htable->table[i] = NULL;
	}
	eFree (htable);
}

extern void       hashTablePutItem    (hashTable *htable, void *key, void *value)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	htable->table[i] = entry_new(key, value, htable->table[i]);
}

extern void*      hashTableGetItem   (hashTable *htable, void *key)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	return entry_find(htable->table[i], key, htable->equalfn);
}

extern boolean     hashTableDeleteItem (hashTable *htable, void *key)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	return entry_delete(&htable->table[i], key,
			    htable->equalfn, htable->keyfreefn, htable->valfreefn);
}

extern boolean    hashTableHasItem    (hashTable *htable, void *key)
{
	return hashTableGetItem (htable, key)? TRUE: FALSE;
}

extern void       hashTableForeachItem (hashTable *htable, hashTableForeachFunc proc, void *user_data)
{
	unsigned int i;

	for (i = 0; i < htable->size; i++)
		entry_foreach(htable->table[i], proc, user_data);
}

unsigned int hash_ptrhash (void * x)
{
	union {
		void *ptr;
		unsigned int ui;
	} v;

	v.ui = 0;
	v.ptr = x;
	return v.ui;
}

boolean hash_ptreq (void *a, void *b)
{
	return (a == b)? TRUE: FALSE;
}
