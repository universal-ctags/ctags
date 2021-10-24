/*
 *   Copyright (c) 2021 Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains macros, data decls and prototypes to generate tags for Kotlin.
 */

#ifndef CTAGS_PEG_COMMON
#define CTAGS_PEG_COMMON

#include "debug.h"

#define PCC_GETCHAR(auxil) getcFromInputFile()
#define PCC_MALLCO(auxil,size) eMalloc(size)
#define PCC_REALLOC(auxil,ptr,size) eRealloc(ptr,size)
#define PCC_FREE(auxil,ptr) eFreeNoNullCheck((void *)ptr)
#define PCC_ERROR(auxil) baseReportError(BASE(auxil))
#ifdef DEBUG
#define PCC_DEBUG(auxil, event, rule, level, pos, buffer, length) baseDebug(BASE(auxil), event, rule, level, pos, buffer, length)
#endif

#include "numarray.h"
#include "parse.h"
#include "entry.h"
#include "read.h"
#ifdef DEBUG
#include "htable.h"
#endif

struct parserBaseCtx {
	intArray *kind_stack;
	int scope_cork_index;
	bool found_syntax_error;
#ifdef DEBUG
	hashTable *debug_rules;
#endif
};

#define BASE_STRUCT parserBaseCtx
#define BASE(P) ((struct BASE_STRUCT*)(P))
#define BASE_ERROR(P) ((BASE(P))->found_syntax_error)
#define BASE_SCOPE(P) ((BASE(P))->scope_cork_index)

#define SET_SCOPE(P,S) (BASE_SCOPE(P) = (S))
#define POP_SCOPE(P) (basePopScope(BASE(P)))
#define PUSH_KIND(P,K) (basePushKind(BASE(P),(K)))
#define POP_KIND(P,POP_SCOPE_TOO) (basePopKind(BASE(P),(POP_SCOPE_TOO)))
#define PEEK_KIND(P) (basePeekKind(BASE(P)))

#define BASE_INIT(P,KIND) (baseInit((BASE(P)),KIND))
#define BASE_FINI(P) (baseFini(BASE(P)))

#ifdef DEBUG
#define BASE_DEBUG_RULE(P, R) baseAddDebugRule(BASE(P), R)
#else
#define BASE_DEBUG_RULE(P, R)
#endif

static void basePopScope(struct parserBaseCtx *auxil)
{
    tagEntryInfo *e = getEntryInCorkQueue (auxil->scope_cork_index);
    if (e)
        auxil->scope_cork_index = e->extensionFields.scopeIndex;
}

static void basePushKind (struct parserBaseCtx *auxil, int kind)
{
	intArrayAdd (auxil->kind_stack, kind);
}

static void basePopKind (struct parserBaseCtx *auxil, bool popScopeToo)
{
    intArrayRemoveLast (auxil->kind_stack);

    if (popScopeToo)
    {
        basePopScope(auxil);
    }
}

static int basePeekKind (struct parserBaseCtx *auxil)
{
    return intArrayLast (auxil->kind_stack);
}

static void baseReportError (struct parserBaseCtx *auxil)
{
    auxil->found_syntax_error = true;
    fprintf(stderr, "%s: syntax error in \"%s\"\n",
			getLanguageName (getInputLanguage ()), getInputFileName());
}

static void baseInit(struct parserBaseCtx *auxil, int initial_kind)
{
	auxil->kind_stack = intArrayNew ();
	basePushKind (auxil, initial_kind);
	auxil->scope_cork_index = CORK_NIL;
	auxil->found_syntax_error = false;
#ifdef DEBUG
	auxil->debug_rules = hashTableNew (11,
									   hashCstrhash, hashCstreq,
									   NULL, NULL);
#endif
}

static void baseFini(struct parserBaseCtx *auxil)
{
	basePopKind (auxil, false);
	intArrayDelete (auxil->kind_stack);
#ifdef DEBUG
	hashTableDelete (auxil->debug_rules);
#endif
}

#ifdef DEBUG
static void baseAddDebugRule (struct parserBaseCtx *auxil, char *rule)
{
	hashTablePutItem (auxil->debug_rules, rule, HT_INT_TO_PTR(1));
}

static bool baseIsDebugRule (struct parserBaseCtx *auxil, const char *rule)
{
	return hashTableHasItem (auxil->debug_rules, rule);
}

static void baseDebug(struct parserBaseCtx *auxil, int event, const char *rule, size_t level, size_t pos, const char *buffer, size_t len)
{
	if (!baseIsDebugRule(auxil, rule))
		return;

	fprintf(stderr, "<level:%2lu, len:%4lu>[%7s] %10s: ",
			level, len,
			event == 0 /*PCC_DBG_EVALUATE*/ ? "eval" :
			event == 1 /*PCC_DBG_MATCH*/    ? "match":
			event == 2 /*PCC_DBG_NOMATCH*/  ? "nomatch": "unknown",
			rule);
	for (size_t i = 0; i < len; i++)
	{
		if (buffer[i] == '\n')
			break;
		fputc(buffer[i], stderr);
	}
	fputc('\n', stderr);
}
#endif

#endif	/* !CTAGS_PEG_COMMON */
