/*
*   Copyright (c) 2016, Masatake YAMATO <yamato@redhat.com>
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"
#include "tokeninfo.h"

#include "entry.h"
#include "read.h"
#include "routines.h"

static void* createToken (void *createArg)
{
	struct tokenInfoClass *klass = createArg;
	tokenInfo *token;

	token = eCalloc (1, sizeof (*token) + klass->extraSpace);
	token->klass = klass;
	token->string  = vStringNew ();

	return token;
}

static void clearToken (void *data)
{
	tokenInfo *token = data;

	if (token->klass->clear)
		token->klass->clear (token);

	token->type = token->klass->typeForUndefined;
	token->keyword = token->klass->keywordNone;
	vStringClear (token->string);
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
}

static void deleteToken (void *data)
{
	tokenInfo *token = data;

	if (token->klass->delete)
		token->klass->delete (token);

	vStringDelete (token->string);
	eFree (token);
}

void *newToken (struct tokenInfoClass *klass)
{
	return newTokenFull (klass, NULL);
}

void *newTokenFull (struct tokenInfoClass *klass, void *data)
{
	tokenInfo *token = NULL;

	if (klass->nPreAlloc == 0)
		klass->nPreAlloc = 16;

 retry:
	if (klass->pool)
		token = objPoolGet (klass->pool);
	else
	{
		klass->pool = objPoolNew (klass->nPreAlloc,
					  createToken,
					  deleteToken,
					  clearToken,
					  klass);
		goto retry;
	}

	if (klass->init)
		klass->init (token, data);
	return token;
}

void  flashTokenBacklog (struct tokenInfoClass *klass)
{
	if (klass->backlog)
		ptrArrayClear (klass->backlog);
}

void tokenDelete (tokenInfo *token)
{
	objPoolPut (token->klass->pool, token);
}


void tokenReadFull (tokenInfo *token, void *data)
{
	if (!token->klass->backlog)
		token->klass->backlog = ptrArrayNew ((ptrArrayDeleteFunc)tokenDelete);

	if (ptrArrayCount (token->klass->backlog) > 0)
	{
		tokenInfo *backlog = ptrArrayLast (token->klass->backlog);
		tokenCopyFull (token, backlog, data);
		ptrArrayRemoveLast (token->klass->backlog);
		tokenDelete (backlog);
	}
	else
	{
		token->klass->read (token, data);
		if (!tokenIsEOF (token))
			token->klass->read_counter++;
	}
}

void tokenRead (tokenInfo *token)
{
	tokenReadFull (token, NULL);
}

void tokenCopyFull  (tokenInfo *dest, tokenInfo *src, void *data)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	/* klass */
	vStringCopy(dest->string, src->string);
	if (src->klass->copy)
		src->klass->copy (dest, src, data);
}

void tokenCopy      (tokenInfo *dest, tokenInfo *src)
{
	tokenCopyFull (dest, src, NULL);
}

void *newTokenByCopying (tokenInfo *src)
{
	return newTokenByCopyingFull (src, NULL);
}

void *newTokenByCopyingFull (tokenInfo *src, void *data)
{
	void * dest = newToken (src->klass);
	tokenCopyFull (dest, src, data);
	return dest;
}

bool tokenSkipToTypeFull (tokenInfo *token, tokenType t, void *data)
{
	while (! (tokenIsEOF (token)
			  || token->type == t))
		tokenReadFull (token, data);

	return (token->type == t)? true: false;
}

bool tokenSkipToType (tokenInfo *token, tokenType t)
{
	return tokenSkipToTypeFull (token, t, NULL);
}

void tokenUnreadFull (tokenInfo *token, void *data)
{
	tokenInfo *backlog;

	if (!token->klass->backlog)
		token->klass->backlog = ptrArrayNew ((ptrArrayDeleteFunc)tokenDelete);

	backlog = newToken (token->klass);
	tokenCopyFull (backlog, token, data);
	ptrArrayAdd (token->klass->backlog, backlog);
}

void tokenUnread      (tokenInfo *token)
{
	tokenUnreadFull (token, NULL);
}

bool tokenSkipOverPair (tokenInfo *token)
{
	return tokenSkipOverPairFull(token, NULL);
}

static struct  tokenTypePair *tokenTypeIsStarterOfPairs (tokenType t,
														 struct  tokenTypePair pairs [],
														 size_t count)
{
	for (size_t i = 0; i < count; i++)
		if (t == pairs[i].start)
			return pairs + i;
	return NULL;
}

bool tokenSkipOverPairFull (tokenInfo *token, void *data)
{
	int start = token->type;
	int end = token->klass->typeForUndefined;

	struct tokenTypePair *endp = tokenTypeIsStarterOfPairs (start,
															token->klass->pairs,
															token->klass->pairCount);
	if (endp)
		end = endp->end;

	if (end == token->klass->typeForUndefined)
		return false;

	int depth = 1;
	do {
		tokenReadFull (token, data);
		if (token->type == start)
			depth ++;
		else if (token->type == end)
			depth--;
	} while ((!tokenIsEOF(token)) && (depth > 0));

	return (depth == 0)? true: false;
}

bool tokenSkipToTypes    (tokenInfo *token, const tokenType ts[], size_t count)
{
	return tokenSkipToTypesFull (token, ts, count, NULL);
}

static bool tokenTypeIsMember (tokenType t, const tokenType ts[], size_t count)
{
	for (size_t i = 0; i < count; i++)
		if (ts [i] == t)
			return true;
	return false;
}

bool tokenSkipToTypesFull (tokenInfo *token, const tokenType ts[], size_t count, void *data)
{
	while (! (tokenIsEOF (token)
			  || tokenTypeIsMember (token->type, ts, count)))
		tokenReadFull (token, data);

	return tokenIsEOF (token)? false: true;
}

bool tokenSkipToTypeOverPairs    (tokenInfo *token, tokenType t)
{
	return tokenSkipToTypeOverPairsFull (token, t, NULL);
}

bool tokenSkipToTypeOverPairsFull (tokenInfo *token, tokenType t, void *data)
{

	return tokenSkipToTypesOverPairsFull (token, &t, 1, data);
}

bool tokenSkipToTypesOverPairs     (tokenInfo *token, const tokenType ts[], size_t count)
{
	return tokenSkipToTypesOverPairsFull (token, ts, count, NULL);
}

bool tokenSkipToTypesOverPairsFull (tokenInfo *token, const tokenType ts[], size_t count, void *data)
{
	while (! (tokenIsEOF (token)))
	{
		if (tokenTypeIsMember (token->type, ts, count))
			return true;
		tokenSkipOverPairFull (token, data);
		if (tokenIsEOF (token))
			return false;
		tokenReadFull (token, data);
	}
	return false;
}

void initTagEntryFromToken (tagEntryInfo *e, tokenInfo *const token, int kindIndex,
							int scopeIndex)
{
	initTagEntry (e, tokenString (token), kindIndex);
	e->lineNumber = token->lineNumber;
	e->filePosition = token->filePosition;
	e->extensionFields.scopeIndex = scopeIndex;
}

int makeSimpleTagFromToken (tokenInfo *const token, int kindIndex, int scopeIndex)
{
	tagEntryInfo e;
	initTagEntryFromToken (&e, token, kindIndex, scopeIndex);
	return makeTagEntry (&e);
}
