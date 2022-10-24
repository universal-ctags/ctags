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
extern struct canonFnameCacheTable *canonFnameCacheTableNew (const char *cwd);
extern void canonFnameCacheTableDelete (struct canonFnameCacheTable *cache_table);

/*
 * If INPUT is an absolute path (starting form '/'), return new absolute path
 * string after resolving '..', '.', and '//' in it. '//' is reduced to '/'.
 *
 * If INPUT is a relative path, resolve '.', '..', and '//' as if the current
 * working directly is the CWD passed canonFnameCacheTableNew(). The return
 * string may absolute or relative path form.
 *
 * In most of all cases, the string return from this function doesn't
 * include ".". There is one exception.
 * If the resolved file name is the same as CWD canonFnameCacheTableNew(),
 * this function returns just ".".
 *
 * Don't free the cstring returned from this function directly.
 * This function stores the cstring to the CACHE_TABLE for processing the
 * same INPUT more than twice. canonFnameCacheTableDelete() destroys all C
 * strings returned from this function.
 */
extern const char *canonicalizeRelativeFileName(struct canonFnameCacheTable *cache_table,
												const char *input);

/*
 * Resolve '.', '..', and '//' in FNAME as if the current working
 * directory is at '/'.
 *
 * eFree() is for freeing the cstring returned from this function.
 * This function may modify FNAME[] as a side effect.
 */
extern char *canonicalizeAbsoluteFileName (char *fname);

#endif	/* CTAGS_MAIN_FNAME_UTIL_H */
