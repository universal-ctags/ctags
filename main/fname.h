/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/
#ifndef CTAGS_MAIN_FNAME_UTIL_H
#define CTAGS_MAIN_FNAME_UTIL_H

#include "general.h"

#include "htable.h"

struct canonFnameCacheTable;
extern struct canonFnameCacheTable *canonFnameCacheTableNew (void);
extern void canonFnameCacheTableDelete (struct canonFnameCacheTable *cache_table);

/* Don't free the cstring returned from this function directly.
 * This function stores the cstring to the cache.
 */
extern const char *canonicalizeRelativeFileName(const char *cwd, size_t cwd_len, const char *input,
						struct canonFnameCacheTable *cache_table);

/*
 * Resolve '.', '..', and '//' in FNAME as if the current working
 * directory is at '/'.
 *
 * eFree() is for freeing the cstring returned from this function.
 * This function may modify FNAME[] as a side effect.
 */
extern char *canonicalizeAbsoluteFileName (char *fname);

#endif	/* CTAGS_MAIN_FNAME_UTIL_H */
