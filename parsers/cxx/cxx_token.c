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

#include "cxx_token_chain.h"
#include "cxx_debug.h"
#include "cxx_keyword.h"
#include "cxx_tag.h"

#define CXX_TOKEN_POOL_MAXIMUM_SIZE 8192

static CXXTokenChain * g_pTokenPool = NULL;

void cxxTokenForceDestroy(CXXToken * t);

void cxxTokenAPIInit(void)
{
	g_pTokenPool = cxxTokenChainCreate();
}

void cxxTokenAPINewFile(void)
{
	anonReset ();
}

void cxxTokenAPIDone(void)
{
	CXXToken * t;

	while((t = cxxTokenChainTakeFirst(g_pTokenPool)))
		cxxTokenForceDestroy(t);

	cxxTokenChainDestroy(g_pTokenPool);
}

CXXToken * cxxTokenCreate(void)
{
	CXXToken * t;

	if(g_pTokenPool->iCount > 0)
	{
		t = cxxTokenChainTakeFirst(g_pTokenPool);
		CXX_DEBUG_ASSERT(t->pszWord,"The string shouldn't have been destroyed");
	} else {
		t = xMalloc(sizeof(CXXToken),CXXToken);
		// we almost always want a string, and since this token
		// is being reused..well.. we always want it
		t->pszWord = vStringNew();
	}

	t->bFollowedBySpace = FALSE;

	t->pChain = NULL;
	t->pNext = NULL;
	t->pPrev = NULL;

	return t;
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

	CXX_DEBUG_ASSERT(t->pszWord,"There should be a word here");

	if(g_pTokenPool->iCount < CXX_TOKEN_POOL_MAXIMUM_SIZE)
	{
		// this won't actually release memory (but we're taking care
		// to do not create very large strings)
		vStringClear(t->pszWord);
		cxxTokenChainAppend(g_pTokenPool,t);
	} else {
		vStringDelete(t->pszWord);
		eFree(t);
	}
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

CXXToken * cxxTokenCreateKeyword(int iLineNumber,MIOPos oFilePosition,enum CXXKeyword eKeyword)
{
	CXXToken * pToken = cxxTokenCreate();
	pToken->iLineNumber = iLineNumber;
	pToken->oFilePosition = oFilePosition;
	pToken->eType = CXXTokenTypeKeyword;
	pToken->eKeyword = eKeyword;
	pToken->bFollowedBySpace = TRUE;
	vStringCatS(pToken->pszWord,cxxKeywordName(eKeyword));

	return pToken;
}


CXXToken * cxxTokenCreateAnonymousIdentifier(enum CXXTagKind k)
{
	CXXToken * t = cxxTokenCreate();

	anonGenerate (t->pszWord, "__anon", k);
	t->bFollowedBySpace = TRUE;
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
