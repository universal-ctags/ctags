/*
 *
 *  Copyright (c) 2016, Red Hat, Inc.
 *  Copyright (c) 2016, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"
#include "parse_p.h"
#include "promise.h"
#include "promise_p.h"
#include "ptrarray.h"
#include "debug.h"
#include "read_p.h"
#include "trashbox.h"
#include "xtag.h"
#include "numarray.h"
#include "routines.h"
#include "options.h"

#include <string.h>

struct promise {
	langType lang;
	unsigned long startLine;
	long startCharOffset;
	unsigned long endLine;
	long endCharOffset;
	unsigned long sourceLineOffset;
	int parent_promise;
	ptrArray *modifiers;
};

typedef void ( *promiseInputModifier) (unsigned char * input,
									   size_t size,
									   unsigned long startLine, long startCharOffset,
									   unsigned long endLine, long endCharOffset,
									   void *data);
typedef void ( *promiseDestroyAttachedData) (void *data);

struct modifier {
	promiseInputModifier modifier;
	promiseDestroyAttachedData destroyData;
	void *data;
};

static struct promise *promises;
static int promise_count;
static int promise_allocated;

#define NO_PROMISE -1
static int current_promise = NO_PROMISE;

static void attachPromiseModifier (int promise,
								   promiseInputModifier modifier,
								   promiseDestroyAttachedData destroyData,
								   void *data);


int  makePromise   (const char *parser,
		    unsigned long startLine, long startCharOffset,
		    unsigned long endLine, long endCharOffset,
		    unsigned long sourceLineOffset)
{
	struct promise *p;
	int r;
	langType lang = LANG_IGNORE;

	verbose("makePromise: %s start(line: %lu, offset: %lu, srcline: %lu), end(line: %lu, offset: %lu)\n",
			parser? parser: "*", startLine, startCharOffset, sourceLineOffset,
			endLine, endCharOffset);

	if ((!isThinStreamSpec(startLine,
						   startCharOffset,
						   endLine,
						   endCharOffset,
						   sourceLineOffset))
		&& ( !isXtagEnabled (XTAG_GUEST)))
		return -1;

	if (parser)
	{
		lang = getNamedLanguage (parser, 0);
		if (lang == LANG_IGNORE)
			return -1;
	}

	if ( promise_count == promise_allocated)
	{
		size_t c = promise_allocated? (promise_allocated * 2): 8;
		if (promises)
			DEFAULT_TRASH_BOX_TAKE_BACK(promises);
		promises = xRealloc (promises, c, struct promise);
		DEFAULT_TRASH_BOX(promises, eFree);
		promise_allocated = c;
	}

	p = promises + promise_count;
	p->parent_promise = current_promise;
	p->lang = lang;
	p->startLine = startLine;
	p->startCharOffset = startCharOffset;
	p->endLine = endLine;
	p->endCharOffset = endCharOffset;
	p->sourceLineOffset = sourceLineOffset;
	p->modifiers = NULL;

	r = promise_count;
	promise_count++;
	return r;
}

static void freeModifier (void *data)
{
	struct modifier *m = data;
	m->destroyData(m->data);
	eFree (m);
}

static void attachPromiseModifier (int promise,
								   promiseInputModifier modifier,
								   promiseDestroyAttachedData destroyData,
								   void *data)
{
	struct modifier *m = xMalloc (1, struct modifier);

	m->modifier = modifier;
	m->destroyData = destroyData;
	m->data = data;

	struct promise *p = promises + promise;
	if (!p->modifiers)
		p->modifiers = ptrArrayNew(freeModifier);

	ptrArrayAdd (p->modifiers, m);
}

static void freeModifiers(int promise)
{
	for (int i = promise; i < promise_count; i++)
	{
		struct promise *p = promises + promise;
		if (p->modifiers)
		{
			ptrArrayDelete (p->modifiers);
			p->modifiers = NULL;
		}
	}
}

void breakPromisesAfter (int promise)
{
	Assert (promise_count >= promise);
	if (promise == NO_PROMISE)
		promise = 0;

	freeModifiers(promise);
	promise_count = promise;
}

bool forcePromises (void)
{
	int i;
	bool tagFileResized = false;

	for (i = 0; i < promise_count; ++i)
	{
		current_promise = i;
		struct promise *p = promises + i;

		if (p->lang != LANG_IGNORE && isLanguageEnabled (p->lang))
			tagFileResized = runParserInNarrowedInputStream (p->lang,
															 p->startLine,
															 p->startCharOffset,
															 p->endLine,
															 p->endCharOffset,
															 p->sourceLineOffset,
															 i)
				? true
				: tagFileResized;
	}

	freeModifiers (0);
	current_promise  = NO_PROMISE;
	promise_count = 0;
	return tagFileResized;
}


int getLastPromise (void)
{
	return promise_count - 1;
}

static unsigned char* fill_or_skip (unsigned char *input, unsigned char *input_end, bool filling)
{
	if ( !(input < input_end))
		return NULL;

	unsigned char *next = memchr(input, '\n', input_end - input);
	if (next)
	{
		if (filling)
			memset(input, ' ', next - input);
		input = next + 1;
		if (input == input_end)
			return NULL;
		else
			return input;
	}
	else
	{
		if (filling)
			memset(input, ' ', input_end - input);
		return NULL;
	}
}

static void line_filler (unsigned char *input, size_t size,
						 unsigned long startLine, long startCharOffset,
						 unsigned long endLine, long endCharOffset,
						 void *data)
{
	ulongArray *lines = data;
	unsigned int start_index, end_index;
	unsigned int i;

	for (i = 0; i < ulongArrayCount (lines); i++)
	{
		unsigned long line = ulongArrayItem (lines, i);
		if (line >= startLine)
			break;
	}
	if (i == ulongArrayCount (lines))
		return;
	if (i > endLine)
		return;
	start_index = i;

	for (; i < ulongArrayCount (lines); i++)
	{
		unsigned long line = ulongArrayItem (lines, i);
		if (line > endLine)
			break;
	}
	end_index = i;

	unsigned long input_line = startLine;
	for (i = start_index; i < end_index; i++)
	{
		unsigned long line = ulongArrayItem (lines, i);

		while (1)
		{
			if (input_line == line)
			{
				input = fill_or_skip (input, input + size, true);
				input_line++;
				break;
			}
			else
			{
				input = fill_or_skip (input, input + size, false);
				input_line++;
			}
		}
	}
}

void promiseAttachLineFiller (int promise, ulongArray *lines)
{
	attachPromiseModifier (promise, line_filler,
						   (promiseDestroyAttachedData)ulongArrayDelete,
						   lines);
}


static void collectModifiers(int promise, ptrArray *modifiers)
{
	while (promise != NO_PROMISE)
	{
		struct promise *p = promises + promise;
		if (p->modifiers)
		{
			for (int i = ptrArrayCount(p->modifiers); i > 0; i--)
			{
				struct modifier *m = ptrArrayItem(p->modifiers, i - 1);
				ptrArrayAdd (modifiers, m);
			}
		}
		promise = p->parent_promise;
	}
}

void runModifiers (int promise,
				   unsigned long startLine, long startCharOffset,
				   unsigned long endLine, long endCharOffset,
				   unsigned char *input,
				   size_t size)
{
	ptrArray *modifiers = ptrArrayNew (NULL);

	collectModifiers (promise, modifiers);
	for (int i = ptrArrayCount (modifiers); i > 0 ; i--)
	{
		struct modifier *m = ptrArrayItem (modifiers, i - 1);
		m->modifier (input, size,
					 startLine, startCharOffset,
					 endLine, endCharOffset,
					 m->data);
	}
	ptrArrayDelete (modifiers);
}

void promiseUpdateLanguage  (int promise, langType lang)
{
	Assert (promise >= 0);

	struct promise *p = promises + promise;

	p->lang = lang;
}
