/*
 *
 *  Copyright (c) 2020, Red Hat, Inc.
 *  Copyright (c) 2020, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_HINT_H
#define CTAGS_MAIN_HINT_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#define TAG_NO_COMPAT_SORT_TYPE
#include "readtags.h"

#include "field.h"

/*
*   DATA DECLARATIONS
*/

typedef tagEntry hintEntry;
typedef tagExtensionField hintExtensionField;

typedef bool (* hintForeachFunc) (const char *name,
								  hintEntry *hint,
								  void * data);

/*
*   FUNCTION PROTOTYPES
*/

/* Return true if a hint file is specified (and opened). */
extern bool isHintAvailable (void);

/*
 * If FUNC returns false, this function returns false.
 * If FUNC never returns false, this func returns true.
 * If FUNC is not called because no node for NAME in the symbol table,
 * this function returns true.
 */
extern bool   foreachHintEntries (const char *name,
								  int flags, /* See TAG_ in readtags.h */
								  hintForeachFunc func,
								  void *data);


extern const char* hintFieldForType (const hintEntry *const hint,
									 fieldType ftype);

#endif	/* CTAGS_MAIN_HINT_H */
