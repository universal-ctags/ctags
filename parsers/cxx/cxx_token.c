/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_token.h"

#include "routines.h"
#include "vstring.h"
#include "read.h"
#include "objpool.h"

#include "cxx_token_chain.h"
#include "cxx_debug.h"
#include "cxx_keyword.h"
#include "cxx_tag.h"

#define CXX_TOKEN_POOL_MAXIMUM_SIZE 8192

static objPool * g_pTokenPool = NULL;

void cxxTokenForceDestroy(CXXToken * t);

static CXXToken *createToken(void *createArg CTAGS_ATTR_UNUSED)
{
	CXXToken *t = xMalloc(1, CXXToken);
	// we almost always want a string, and since this token
	// is being reused..well.. we always want it
	t->pszWord = vStringNew();
	return t;
}

static void deleteToken(CXXToken *token)
{
	vStringDelete(token->pszWord);
	eFree(token);
}

static void clearToken(CXXToken *t)
{
	CXX_DEBUG_ASSERT(t->pszWord,"The string shouldn't have been destroyed");

	// this won't actually release memory (but we're taking care
	// to do not create very large strings)
	vStringClear(t->pszWord);

	t->bFollowedBySpace = false;

	t->pChain = NULL;
	t->pNext = NULL;
	t->pPrev = NULL;
}

void cxxTokenAPIInit(void)
{
	g_pTokenPool = objPoolNew(CXX_TOKEN_POOL_MAXIMUM_SIZE,
		(objPoolCreateFunc)createToken, (objPoolDeleteFunc)deleteToken,
		(objPoolClearFunc)clearToken,
		NULL);
}

void cxxTokenAPINewFile(void)
{
	/* Stub */
}

void cxxTokenAPIDone(void)
{
	objPoolDelete (g_pTokenPool);
}

CXXToken * cxxTokenCreate(void)
{
	return objPoolGet (g_pTokenPool);
}

void cxxTokenDestroy(CXXToken * t)
{
	if(!t)
		return;

	if(t->pChain)
	{
		cxxTokenChainDestroy(t->pChain);
		t->pChain = NULL;
	}

	objPoolPut (g_pTokenPool, t);
}

void cxxTokenForceDestroy(CXXToken * t)
{
	if(!t)
		return;

	if(t->pChain)
	{
		cxxTokenChainDestroy(t->pChain);
		t->pChain = NULL;
	}

	CXX_DEBUG_ASSERT(t->pszWord,"There should be a word here");

	vStringDelete(t->pszWord);

	eFree(t);
}

CXXToken * cxxTokenCopy(CXXToken * pToken)
{
	CXXToken * pRetToken = cxxTokenCreate();
	pRetToken->iLineNumber = pToken->iLineNumber;
	pRetToken->oFilePosition = pToken->oFilePosition;
	pRetToken->eType = pToken->eType;
	pRetToken->eKeyword = pToken->eKeyword;
	pToken->bFollowedBySpace = pToken->bFollowedBySpace;
	vStringCat(pRetToken->pszWord,pToken->pszWord);

	return pRetToken;
}

CXXToken * cxxTokenCreateKeyword(int iLineNumber,MIOPos oFilePosition,CXXKeyword eKeyword)
{
	CXXToken * pToken = cxxTokenCreate();
	pToken->iLineNumber = iLineNumber;
	pToken->oFilePosition = oFilePosition;
	pToken->eType = CXXTokenTypeKeyword;
	pToken->eKeyword = eKeyword;
	pToken->bFollowedBySpace = true;
	vStringCatS(pToken->pszWord,cxxKeywordName(eKeyword));

	return pToken;
}


CXXToken * cxxTokenCreateAnonymousIdentifier(unsigned int uTagKind)
{
	CXXToken * t = cxxTokenCreate();

	anonGenerate (t->pszWord, "__anon", uTagKind);
	t->eType = CXXTokenTypeIdentifier;
	t->bFollowedBySpace = true;
	t->iLineNumber = getInputLineNumber();
	t->oFilePosition = getInputFilePosition();

	return t;
}

void cxxTokenAppendToString(vString * s,CXXToken * t)
{
	switch(t->eType)
	{
		case CXXTokenTypeParenthesisChain:
		case CXXTokenTypeSquareParenthesisChain:
		case CXXTokenTypeBracketChain:
		case CXXTokenTypeAngleBracketChain:
			CXX_DEBUG_ASSERT(t->pChain,"This token should have a nested chain!");
			cxxTokenChainJoinInString(t->pChain,s,NULL,0);
		break;
		default:
			vStringCat(s,t->pszWord);
		break;
	}
}

void cxxTokenReduceBackward (CXXToken *pStart)
{
	enum CXXTokenType eSentinelType = pStart->eType >> 4;
	CXXToken *pTmp = pStart->pPrev;
	CXXToken *pReducingCandidate;

	while (pTmp && (!cxxTokenTypeIsOneOf (pTmp, eSentinelType)))
	{
		pReducingCandidate = pTmp;
		pTmp = pTmp->pPrev;
		pTmp->pNext = pReducingCandidate->pNext;
		pReducingCandidate->pNext->pPrev = pTmp;
		CXX_DEBUG_PRINT("reduce inner token: %p",pReducingCandidate);
		cxxTokenDestroy (pReducingCandidate);
	}
}
