/*
*   Copyright (c) 2016, Masatake YAMATO <yamato@redhat.com>
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Python language
*   files.
*/

#include "tokeninfo.h"

#include "entry.h"
#include "read.h"
#include "routines.h"

static void* createToken (void *createArg)
{
	struct tokenInfoClass *klass = createArg;
	tokenInfo *token;

	token = eMalloc (sizeof (*token) + klass->extraSpace);
	token->klass = klass;
	token->string  = vStringNew ();

	return token;
}

static void clearToken (void *data)
{
	tokenInfo *token = data;

	if (token->klass->clear)
		token->klass->clear (token);

	token->type = token->klass->typeForKeyword;
	token->keyword = token->klass->keywordNone;
	vStringClear (token->string);
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
}

static void destroyToken (void *data)
{
	tokenInfo *token = data;

	if (token->klass->destroy)
		token->klass->destroy (token);

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
					  destroyToken,
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

void tokenDestroy (tokenInfo *token)
{
	objPoolPut (token->klass->pool, token);
}


void tokenReadFull (tokenInfo *token, void *data)
{
	if (!token->klass->backlog)
		token->klass->backlog = ptrArrayNew ((ptrArrayDeleteFunc)tokenDestroy);

	if (ptrArrayCount (token->klass->backlog) > 0)
	{
		tokenInfo *backlog = ptrArrayLast (token->klass->backlog);
		tokenCopyFull (token, backlog, data);
		ptrArrayRemoveLast (token->klass->backlog);
		tokenDestroy (backlog);
	}
	else
		token->klass->read (token, data);
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
		token->klass->backlog = ptrArrayNew ((ptrArrayDeleteFunc)tokenDestroy);

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
bool tokenSkipOverPairFull (tokenInfo *token, void *data)
{
	int start = token->type;
	int end = token->klass->typeForUndefined;
	unsigned int i;

	for (i = 0; i < token->klass->pairCount; i++)
		if (start == token->klass->pairs[i].start)
			end = token->klass->pairs[i].end;

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
