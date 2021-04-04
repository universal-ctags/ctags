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

#include "general.h"
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

#include <string.h>


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
	hashTableDeleteFunc keyfreefn;
	hashTableDeleteFunc valfreefn;
	void *valForNotUnknownKey;
	hashTableDeleteFunc valForNotUnknownKeyfreefn;
};

struct chainTracker {
	const void *const target_key;
	hashTableForeachFunc user_proc;
	void *user_data;
	hashTableEqualFunc equalfn;
};

static hentry* entry_new (void *key, void *value, hentry* next)
{
	hentry* entry = xMalloc (1, hentry);

	entry->key = key;
	entry->value = value;
	entry->next = next;

	return entry;
}

static void entry_reset  (hentry* entry,
						  void *newkey,
						  void *newval,
						  hashTableDeleteFunc keyfreefn,
						  hashTableDeleteFunc valfreefn)
{
	if (keyfreefn)
		keyfreefn (entry->key);
	if (valfreefn)
		valfreefn (entry->value);
	entry->key = newkey;
	entry->value = newval;
}

static hentry* entry_destroy (hentry* entry,
			      hashTableDeleteFunc keyfreefn,
			      hashTableDeleteFunc valfreefn)
{
	hentry* tmp;

	entry_reset (entry, NULL, NULL, keyfreefn, valfreefn);
	tmp = entry->next;
	eFree (entry);

	return tmp;
}

static void  entry_reclaim (hentry* entry,
			    hashTableDeleteFunc keyfreefn,
			    hashTableDeleteFunc valfreefn)
{
	while (entry)
		entry = entry_destroy (entry, keyfreefn, valfreefn);
}

static void *entry_find (hentry* entry, const void* const key, hashTableEqualFunc equalfn,
						 void *valForNotUnknownKey)
{
	while (entry)
	{
		if (equalfn( key, entry->key))
			return entry->value;
		entry = entry->next;
	}
	return valForNotUnknownKey;
}

static bool		entry_delete (hentry **entry, const void *key, hashTableEqualFunc equalfn,
			      hashTableDeleteFunc keyfreefn, hashTableDeleteFunc valfreefn)
{
	while (*entry)
	{
		if (equalfn (key, (*entry)->key))
		{
			*entry = entry_destroy (*entry, keyfreefn, valfreefn);
			return true;
		}
		entry = &((*entry)->next);
	}
	return false;
}

static bool		entry_update (hentry *entry, void *key, void *value, hashTableEqualFunc equalfn,
			      hashTableDeleteFunc keyfreefn, hashTableDeleteFunc valfreefn)
{
	while (entry)
	{
		if (equalfn (key, entry->key))
		{
			entry_reset (entry, key, value, keyfreefn, valfreefn);
			return true;
		}
		entry = entry->next;
	}
	return false;
}

static bool  entry_foreach (hentry *entry, hashTableForeachFunc proc, void *user_data)
{
	while (entry)
	{
		if (!proc (entry->key, entry->value, user_data))
			return false;
		entry = entry->next;
	}
	return true;
}

extern hashTable *hashTableNew    (unsigned int size,
				   hashTableHashFunc hashfn,
				   hashTableEqualFunc equalfn,
				   hashTableDeleteFunc keyfreefn,
				   hashTableDeleteFunc valfreefn)
{
	hashTable *htable;

	htable = xMalloc (1, hashTable);
	htable->size = size;
	htable->table = xCalloc (size, hentry*);

	htable->hashfn = hashfn;
	htable->equalfn = equalfn;
	htable->keyfreefn = keyfreefn;
	htable->valfreefn = valfreefn;
	htable->valForNotUnknownKey = NULL;
	htable->valForNotUnknownKeyfreefn = NULL;

	return htable;
}

extern void hashTableSetValueForUnknownKey (hashTable *htable,
											void *val,
											hashTableDeleteFunc valfreefn)
{
	if (htable->valfreefn)
		htable->valfreefn (htable->valForNotUnknownKey);

	htable->valForNotUnknownKey = val;
	htable->valForNotUnknownKeyfreefn = valfreefn;
}

extern void       hashTableDelete (hashTable *htable)
{
	if (!htable)
		return;

	hashTableClear (htable);

	if (htable->valForNotUnknownKeyfreefn)
		htable->valForNotUnknownKeyfreefn (htable->valForNotUnknownKey);
	eFree (htable->table);
	eFree (htable);
}

extern void       hashTableClear (hashTable *htable)
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
}

extern void       hashTablePutItem    (hashTable *htable, void *key, void *value)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	htable->table[i] = entry_new(key, value, htable->table[i]);
}

extern void*      hashTableGetItem   (hashTable *htable, const void * key)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	return entry_find(htable->table[i], key, htable->equalfn, htable->valForNotUnknownKey);
}

extern bool     hashTableDeleteItem (hashTable *htable, const void *key)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	return entry_delete(&htable->table[i], key,
			    htable->equalfn, htable->keyfreefn, htable->valfreefn);
}

extern bool    hashTableUpdateItem (hashTable *htable, void *key, void *value)
{
	unsigned int i;

	i = htable->hashfn (key) % htable->size;
	bool r = entry_update(htable->table[i], key, value,
						  htable->equalfn, htable->keyfreefn, htable->valfreefn);
	if (!r)
		htable->table[i] = entry_new(key, value, htable->table[i]);
	return r;
}

extern bool    hashTableHasItem    (hashTable *htable, const void *key)
{
	return hashTableGetItem (htable, key) == htable->valForNotUnknownKey? false: true;
}

extern bool       hashTableForeachItem (hashTable *htable, hashTableForeachFunc proc, void *user_data)
{
	unsigned int i;

	for (i = 0; i < htable->size; i++)
		if (!entry_foreach(htable->table[i], proc, user_data))
			return false;
	return true;
}

static bool track_chain (const void *const key, void *value, void *chain_data)
{
	struct chainTracker *chain_tracker = chain_data;

	if (chain_tracker->equalfn (chain_tracker->target_key, key))
	{
		if (! chain_tracker->user_proc (key, value, chain_tracker->user_data))
			return false;
	}
	return true;
}

extern bool       hashTableForeachItemOnChain (hashTable *htable, const void *key, hashTableForeachFunc proc, void *user_data)
{
	unsigned int i;
	struct chainTracker chain_tracker = {
		.target_key = key,
		.user_proc = proc,
		.user_data = user_data,
		.equalfn   = htable->equalfn,
	};

	i = htable->hashfn (key) % htable->size;
	if (!entry_foreach(htable->table[i], track_chain, &chain_tracker))
		return false;
	return true;
}

static bool count (const void *const key CTAGS_ATTR_UNUSED, void *value CTAGS_ATTR_UNUSED, void *data)
{
	int *c = data;
	++*c;
	return true;
}

extern unsigned int hashTableCountItem   (hashTable *htable)
{
	int c = 0;
	hashTableForeachItem (htable, count, &c);
	return c;
}

unsigned int hashPtrhash (const void * const x)
{
	union {
		const void * ptr;
		unsigned int ui;
	} v;
	v.ui = 0;
	v.ptr = x;

	return v.ui;
}

bool hashPtreq (const void *const a, const void *const b)
{
	return (a == b)? true: false;
}


/* http://www.cse.yorku.ca/~oz/hash.html */
static unsigned long
djb2(const unsigned char *str)
{
	unsigned long hash = 5381;
	int c;

	while ((c = *str++))
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash;
}

static unsigned long
casedjb2(const unsigned char *str)
{
	unsigned long hash = 5381;
	int c;

	while ((c = *str++))
	{
		if (('a' <= c) && (c <= 'z'))
			c += ('A' - 'a');
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
	}

	return hash;
}


unsigned int hashCstrhash (const void *const x)
{
	const char *const s = x;
	return (unsigned int)djb2((const unsigned char *)s);
}

bool hashCstreq (const void * const a, const void *const b)
{
	return !!(strcmp (a, b) == 0);
}

unsigned int hashInthash (const void *const x)
{
       union tmp {
               unsigned int u;
               int i;
       } x0;

       x0.u = 0;
       x0.i = *(int *)x;
       return x0.u;
}

bool hashInteq (const void *const a, const void *const b)
{
       int ai = *(int *)a;
       int bi = *(int *)b;

       return !!(ai == bi);
}


unsigned int hashCstrcasehash (const void *const x)
{
	const char *const s = x;
	return (unsigned int)casedjb2((const unsigned char *)s);
}

bool hashCstrcaseeq (const void *const a, const void *const b)
{
	return !!(strcasecmp (a, b) == 0);
}
