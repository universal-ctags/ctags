/*
 *
 *  Copyright (c) 2026, Red Hat, Inc.
 *  Copyright (c) 2026, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "debug.h"
#include "inline.h"
#include "ptrarray.h"
#include "read.h"
#include "routines.h"
#include "stackguard_p.h"
#include "trashbox.h"
#include "vstring.h"

#include <stdint.h>

#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

/*
*   TYPE DECLARATIONS
*/
/* https://github.com/universal+ctags/ctags/pull/4387#discussion_r2839408399
 * A function pointer cannot be stored to a void* variable.
 * You can store only a data object to void* variable.
 * struct discardInputAction is a data type for storing a function
 * point to ptrArray.
 */
struct discardInputAction {
	discardInputFn fn;
};

/*
*   DATA DECLARATIONS
*/
static size_t limit_;
#define MARGIN (16 * 1024)
#define DEFAULT_LIMIT ((2048 * 1024) - MARGIN)
static size_t peakUsage;

static struct stackGuard {
	uintptr_t base;
	ptrArray *gotoEOFs;
} *stackGuard;

static vString *peakStackUsageInput;

/*
*   FUNCTION DEFINITIONS
*/
CTAGS_INLINE size_t getLimit (void)
{
	if (limit_)
		return limit_;

#if HAVE_SYS_RESOURCE_H
	struct rlimit rlim;
	if (getrlimit (RLIMIT_STACK, &rlim) == 0)
	{
		rlim_t cur = rlim.rlim_cur;
		size_t cur_size;

		if (rlim.rlim_cur == RLIM_INFINITY || cur > (rlim_t) SIZE_MAX)
			cur_size = SIZE_MAX;
		else
			cur_size = (size_t) cur;

		if (cur_size > MARGIN)
			limit_ = cur_size - MARGIN;
		else
			limit_ = (cur_size * 3 / 4);
	}
#else
	/* TODO
	 * On windows, you can extract the assumed stack size from the program itself.
	 * https://github.com/cygwin/cygwin/blob/main/winsup/cygwin/resource.cc
	 */
#endif

	if (limit_ == 0)
		limit_ = DEFAULT_LIMIT;

	return limit_;
}

extern char *makeFeatureStackGuardDescription (const char *template)
{
	vString *vstr = vStringNewInit (template);
	char buf[32] = {'\0'};

	vStringCatS (vstr, " (default: ");
	snprintf (buf, sizeof (buf), "%zu", getLimit ());
	vStringCatS (vstr, buf);
	vStringPut (vstr, ')');
	return vStringDeleteUnwrap (vstr);
}

extern size_t stackGuardGetLimit (void)
{
	return getLimit ();
}

extern void stackGuardSetLimit (const size_t limit)
{
	limit_ = limit;
}

extern void stackGuardPrepare (discardInputFn gotoEOF)
{
	if (stackGuard == NULL)
	{
		volatile int base;

		stackGuard = xMalloc (1, struct stackGuard);
		stackGuard->gotoEOFs = ptrArrayNew (eFree);
		stackGuard->base = (uintptr_t)&base;
	}

	struct discardInputAction *a = xMalloc (1, struct discardInputAction);
	a->fn = gotoEOF;
	ptrArrayAdd (stackGuard->gotoEOFs, a);
}

extern void stackGuardRelease (void)
{
	Assert (stackGuard);
	Assert (stackGuard->gotoEOFs);
	Assert (ptrArrayCount(stackGuard->gotoEOFs) > 0);

	ptrArrayDeleteLast (stackGuard->gotoEOFs);
	if (ptrArrayCount (stackGuard->gotoEOFs) == 0)
	{
		ptrArrayDelete (stackGuard->gotoEOFs);
		stackGuard->gotoEOFs = NULL;
		eFree (stackGuard);
		stackGuard = NULL;
	}
}

extern bool stackGuardCheck (void *dataForDiscardFunc)
{
	volatile int inner;
	bool safe = true;

	Assert (stackGuard);
	Assert (stackGuard->gotoEOFs);
	Assert (ptrArrayCount(stackGuard->gotoEOFs) > 0);

	uintptr_t inner_pos = (uintptr_t)&inner;
	size_t used = (inner_pos > stackGuard->base)
		? (inner_pos - stackGuard->base)
		: (stackGuard->base - inner_pos);

	size_t limit = getLimit();
	if (used > limit)
	{
		safe = false;

		notice ("call stack exceeds the limit (%zu) during processing input: %s",
				limit,
				getInputFileName ());

		struct discardInputAction *a = ptrArrayLast (stackGuard->gotoEOFs);
		a->fn (dataForDiscardFunc);
	}

	if (used > peakUsage)
	{
		peakUsage = used;

		if (!peakStackUsageInput)
		{
			peakStackUsageInput = vStringNew();
			DEFAULT_TRASH_BOX (peakStackUsageInput, vStringDelete);
		}
		vStringCopyS (peakStackUsageInput, getInputFileName ());
	}

	return safe;
}

extern size_t stackGuardObservedPeak (const char **input)
{
	if (input)
		*input = peakStackUsageInput ? vStringValue (peakStackUsageInput) : NULL;

	return peakUsage;
}
