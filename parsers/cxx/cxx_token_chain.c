/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_token_chain.h"

#include "cxx_debug.h"

#include "vstring.h"
#include "debug.h"
#include "routines.h"

#include <string.h>

void cxxTokenChainInit(CXXTokenChain * tc)
{
	Assert(tc);
	tc->pHead = NULL;
	tc->pTail = NULL;
	tc->iCount = 0;
}

CXXTokenChain * cxxTokenChainCreate(void)
{
	CXXTokenChain * tc = xMalloc(sizeof(CXXTokenChain),CXXTokenChain);
	cxxTokenChainInit(tc);
	return tc;
}

void cxxTokenChainDestroy(CXXTokenChain * tc)
{
	CXXToken * t;
	CXXToken * t2;

	if(!tc)
		return;

	t = tc->pHead;
	while(t)
	{
		t2 = t->pNext;
		cxxTokenDestroy(t);
		t = t2;
	}

	eFree(tc);
}

CXXToken * cxxTokenChainTakeFirst(CXXTokenChain * tc)
{
	CXXToken * t;

	if(!tc)
		return NULL;
	if(!tc->pHead)
		return NULL;

	t = tc->pHead;
	if(t == tc->pTail)
	{
		tc->pHead = NULL;
		tc->pTail = NULL;
		tc->iCount = 0;
		return t;
	}

	tc->iCount--;
	Assert(tc->iCount >= 0);
	Assert(t->pNext);

	t->pNext->pPrev = NULL;
	tc->pHead = t->pNext;

	return t;
}

CXXToken * cxxTokenChainTakeLast(CXXTokenChain * tc)
{
	CXXToken * t;

	if(!tc)
		return NULL;
	if(!tc->pTail)
		return NULL;

	t = tc->pTail;
	if(t == tc->pHead)
	{
		tc->pHead = NULL;
		tc->pTail = NULL;
		tc->iCount = 0;
		return t;
	}

	tc->iCount--;
	Assert(tc->iCount >= 0);

	t->pPrev->pNext = NULL;
	tc->pTail = t->pPrev;

	return t;
}

void cxxTokenChainTake(CXXTokenChain * tc,CXXToken * t)
{
	if(!tc)
		return;
	if(!tc->pHead)
		return;

	/*
	Debug with this:

	CXXToken * t2 = tc->pHead;
	while(t2 && (t2 != t))
		t2 = t2->pNext;

	Assert(t2);
	*/

	if(t == tc->pHead)
	{
		cxxTokenChainTakeFirst(tc);
		return;
	}

	if(t == tc->pTail)
	{
		cxxTokenChainTakeLast(tc);
		return;
	}

	// in the middle

	CXXToken * n = t->pNext;
	CXXToken * p = t->pPrev;

	n->pPrev = p;
	p->pNext = n;

	tc->iCount--;

	Assert(tc->iCount > 1);
}

boolean cxxTokenChainTakeRecursive(CXXTokenChain * tc,CXXToken * t)
{
	if(!tc)
		return FALSE;

	CXXToken * aux = tc->pHead;
	while(aux)
	{
		if(t == aux)
		{
			cxxTokenChainTake(tc,aux);
			return TRUE;
		}

		if(cxxTokenTypeIsOneOf(
				aux,
				CXXTokenTypeParenthesisChain | CXXTokenTypeAngleBracketChain |
					CXXTokenTypeSquareParenthesisChain | CXXTokenTypeBracketChain
			))
		{
			if(cxxTokenChainTakeRecursive(aux->pChain,t))
				return TRUE;
		}

		aux = aux->pNext;
	}

	return FALSE;
}

#if 0
CXXToken * cxxTokenChainTakeAt(CXXTokenChain * tc,int index)
{
	if(!tc)
		return NULL;
	CXXToken * token = cxxTokenChainAt(tc,index);
	if(!token)
		return NULL;
	cxxTokenChainTake(tc,token);
	return token;
}
#endif

void cxxTokenChainClear(CXXTokenChain * tc)
{
	CXXToken * t;

	if(!tc)
		return;

	if(tc->iCount < 1)
		return;

	while((t = cxxTokenChainTakeFirst(tc)))
		cxxTokenDestroy(t);


	Assert(tc->iCount == 0);
	Assert(tc->pHead == NULL);
	Assert(tc->pTail == NULL);
}

void cxxTokenChainAppend(CXXTokenChain * tc,CXXToken * t)
{
	tc->iCount++;

	if(!tc->pTail)
	{
		tc->pHead = t;
		tc->pTail = t;
		t->pPrev = NULL;
		t->pNext = NULL;
		return;
	}

	t->pPrev = tc->pTail;
	t->pNext = NULL;

	tc->pTail->pNext = t;
	tc->pTail = t;
}

void cxxTokenChainPrepend(CXXTokenChain * tc,CXXToken * t)
{
	tc->iCount++;

	if(!tc->pHead)
	{
		tc->pHead = t;
		tc->pTail = t;
		t->pPrev = NULL;
		t->pNext = NULL;
		return;
	}

	t->pNext = tc->pHead;
	t->pPrev = NULL;

	tc->pHead->pPrev = t;
	tc->pHead = t;
}

void cxxTokenChainJoinRangeInString(
		CXXToken * from,
		CXXToken * to,
		vString * s,
		const char * szSeparator,
		unsigned int uFlags
	)
{
	if(!from)
		return;

	CXXToken * t = from;

	cxxTokenAppendToString(s,t);

	if((!(uFlags & CXXTokenChainJoinNoTrailingSpaces)) && t->bFollowedBySpace)
		vStringCatS(s," ");

	while(t && (t != to))
	{
		t = t->pNext;
		if(t)
			return;

		if(szSeparator)
			vStringCatS(s,szSeparator);

		cxxTokenAppendToString(s,t);

		if(
				(!(uFlags & CXXTokenChainJoinNoTrailingSpaces)) &&
				t->bFollowedBySpace
			)
			vStringCatS(s," ");
	}
}

vString * cxxTokenChainJoinRange(
		CXXToken * from,
		CXXToken * to,
		const char * szSeparator,
		unsigned int uFlags
	)
{
	if(!from)
		return NULL;

	vString * s = vStringNew();

	cxxTokenChainJoinRangeInString(from,to,s,szSeparator,uFlags);

	return s;
}

void cxxTokenChainJoinInString(
		CXXTokenChain * tc,
		vString * s,
		const char * szSeparator,
		unsigned int uFlags
	)
{
	if(!tc)
		return;

	if(tc->iCount == 0)
		return;

	CXXToken * t = tc->pHead;

	cxxTokenAppendToString(s,t);

	if(
			(!(uFlags & CXXTokenChainJoinNoTrailingSpaces)) &&
			t->bFollowedBySpace
		)
		vStringCatS(s," ");

	t = t->pNext;
	while(t)
	{
		if(szSeparator)
			vStringCatS(s,szSeparator);

		cxxTokenAppendToString(s,t);

		if(
				(!(uFlags & CXXTokenChainJoinNoTrailingSpaces)) &&
				t->bFollowedBySpace
			)
			vStringCatS(s," ");

		t = t->pNext;
	}
}


vString * cxxTokenChainJoin(
		CXXTokenChain * tc,
		const char * szSeparator,
		unsigned int uFlags
	)
{
	if(!tc)
		return NULL;

	if(tc->iCount == 0)
		return NULL;

	vString * s = vStringNew();

	cxxTokenChainJoinInString(tc,s,szSeparator,uFlags);

	return s;
}

void cxxTokenChainMoveEntries(CXXTokenChain * src,CXXTokenChain * dest)
{
	if(dest->iCount > 0)
		cxxTokenChainClear(dest);

	dest->iCount = src->iCount;
	dest->pHead = src->pHead;
	dest->pTail = src->pTail;

	src->iCount = 0;
	src->pHead = NULL;
	src->pTail = NULL;
}

void cxxTokenChainMoveEntryRange(
		CXXTokenChain * src,
		CXXToken * start,
		CXXToken * end,
		CXXTokenChain * dest
	)
{
	if(!src || !dest || !start || !end)
		return;

	CXX_DEBUG_ASSERT(
			cxxTokenChainFindToken(src,start) >= 0,
			"The start token must be in the source chain!"
		);
	CXX_DEBUG_ASSERT(
			cxxTokenChainFindToken(src,end) >= 0,
			"The end token must be in the source chain!"
		);
	CXX_DEBUG_ASSERT(
			cxxTokenChainFindToken(src,start) <= cxxTokenChainFindToken(src,end),
			"The start token must come before the end token"
		);

	CXXToken * t = start;
	for(;;)
	{
		CXXToken * next = t->pNext;

		cxxTokenChainTake(src,t);
		cxxTokenChainAppend(dest,t);

		if(t == end)
			break;

		t = next;
	}
}


void cxxTokenChainCondense(CXXTokenChain * tc,unsigned int uFlags)
{
	if(!tc)
		return;
	if(tc->iCount <= 1)
		return;

	CXXToken * pCondensed = cxxTokenCreate();

	pCondensed->eType = CXXTokenTypeUnknown;
	pCondensed->iLineNumber = tc->pHead->iLineNumber;
	pCondensed->oFilePosition = tc->pHead->oFilePosition;

	while(tc->iCount > 0)
	{
		CXXToken * t = cxxTokenChainTakeFirst(tc);

		cxxTokenAppendToString(pCondensed->pszWord,t);

		if(
				(!(uFlags & CXXTokenChainCondenseNoTrailingSpaces)) &&
				t->bFollowedBySpace
			)
			vStringCatS(pCondensed->pszWord," ");

		pCondensed->bFollowedBySpace = t->bFollowedBySpace;
		cxxTokenDestroy(t);
	}

	cxxTokenChainAppend(tc,pCondensed);
}

CXXToken * cxxTokenChainAt(CXXTokenChain * tc,int index)
{
	if(!tc)
		return NULL;
	if(index < 0)
		return NULL;
	if(index >= tc->iCount)
		return NULL;
	CXXToken * pToken = tc->pHead;
	while(pToken && index)
	{
		index--;
		pToken = pToken->pNext;
	}

	return pToken;
}

CXXToken * cxxTokenChainSkipToEndOfTemplateAngleBracket(CXXToken * t)
{
	if(!t)
		return NULL;
	CXX_DEBUG_ASSERT(
			t->eType == CXXTokenTypeSmallerThanSign,
			"This function must be called when pointing to a <"
		);
	int iLevel = 1;
	t = t->pNext;
	while(t)
	{
		if(cxxTokenTypeIs(t,CXXTokenTypeSmallerThanSign))
		{
			iLevel++;
		} else if(cxxTokenTypeIs(t,CXXTokenTypeGreaterThanSign))
		{
			if(iLevel == 1)
				return t;
			iLevel--;
		}
		t = t->pNext;
	}
	// invalid
	return NULL;
}

CXXToken * cxxTokenChainSkipBackToStartOfTemplateAngleBracket(CXXToken * t)
{
	if(!t)
		return NULL;
	CXX_DEBUG_ASSERT(
			t->eType == CXXTokenTypeGreaterThanSign,
			"This function must be called when pointing to a >"
		);
	int iLevel = 1;
	t = t->pPrev;
	while(t)
	{
		if(cxxTokenTypeIs(t,CXXTokenTypeGreaterThanSign))
		{
			iLevel++;
		} else if(cxxTokenTypeIs(t,CXXTokenTypeSmallerThanSign))
		{
			if(iLevel == 1)
				return t;
			iLevel--;
		}
		t = t->pPrev;
	}
	// invalid
	return NULL;
}

CXXToken * cxxTokenChainFirstTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	)
{
	if(!tc)
		return NULL;
	CXXToken * t = tc->pHead;
	while(t)
	{
		if(t->eType & uTokenTypes)
			return t;
		t = t->pNext;
	}
	return NULL;
}

CXXToken * cxxTokenChainNextTokenOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	)
{
	if(!t)
		return NULL;
	t = t->pNext;
	while(t)
	{
		if(t->eType & uTokenTypes)
			return t;
		t = t->pNext;
	}
	return NULL;
}

CXXToken * cxxTokenChainPreviousTokenOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	)
{
	if(!t)
		return NULL;
	t = t->pPrev;
	while(t)
	{
		if(t->eType & uTokenTypes)
			return t;
		t = t->pPrev;
	}
	return NULL;
}

CXXToken * cxxTokenChainPreviousTokenNotOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	)
{
	if(!t)
		return NULL;
	t = t->pPrev;
	while(t)
	{
		if(!(t->eType & uTokenTypes))
			return t;
		t = t->pPrev;
	}
	return NULL;
}

CXXToken * cxxTokenChainLastTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	)
{
	if(!tc)
		return NULL;
	CXXToken * t = tc->pTail;
	while(t)
	{
		if(t->eType & uTokenTypes)
			return t;
		t = t->pPrev;
	}
	return NULL;
}

CXXToken * cxxTokenChainLastPossiblyNestedTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	)
{
	if(!tc)
		return NULL;
	CXXToken * t = tc->pTail;
	while(t)
	{
		if(t->eType & uTokenTypes)
			return t;
		if(t->eType == CXXTokenTypeParenthesisChain)
		{
			CXXToken * tmp = cxxTokenChainLastPossiblyNestedTokenOfType(
					t->pChain,
					uTokenTypes
				);
			if(tmp)
				return tmp;
		}
		t = t->pPrev;
	}
	return NULL;

}

CXXToken * cxxTokenChainFirstPossiblyNestedTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	)
{
	if(!tc)
		return NULL;
	CXXToken * t = tc->pHead;
	while(t)
	{
		if(t->eType & uTokenTypes)
			return t;
		if(t->eType == CXXTokenTypeParenthesisChain)
		{
			CXXToken * tmp = cxxTokenChainFirstPossiblyNestedTokenOfType(
					t->pChain,
					uTokenTypes
				);
			if(tmp)
				return tmp;
		}
		t = t->pNext;
	}
	return NULL;

}


CXXToken * cxxTokenChainFirstTokenNotOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	)
{
	if(!tc)
		return NULL;
	CXXToken * t = tc->pHead;
	while(t)
	{
		if(!(t->eType & uTokenTypes))
			return t;
		t = t->pNext;
	}
	return NULL;
}

CXXToken * cxxTokenChainNextTokenNotOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	)
{
	if(!t)
		return NULL;
	t = t->pNext;
	while(t)
	{
		if(!(t->eType & uTokenTypes))
			return t;
		t = t->pNext;
	}
	return NULL;
}

CXXToken * cxxTokenChainLastTokenNotOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	)
{
	if(!tc)
		return NULL;
	CXXToken * t = tc->pTail;
	while(t)
	{
		if(!(t->eType & uTokenTypes))
			return t;
		t = t->pPrev;
	}
	return NULL;
}


int cxxTokenChainFindToken(
		CXXTokenChain * tc,
		CXXToken * t
	)
{
	if(!tc)
		return -1;
	if(tc->iCount < 1)
		return -1;

	CXXToken * pToken = tc->pHead;
	int idx = 0;
	while(pToken)
	{
		if(pToken == t)
			return idx;
		idx++;
		pToken = pToken->pNext;
	}

	return -1;
}

CXXToken * cxxTokenChainPreviousKeyword(
		CXXToken * from,
		enum CXXKeyword eKeyword
	)
{
	if(!from)
		return NULL;

	CXXToken * t = from->pPrev;
	while(t)
	{
		if(cxxTokenIsKeyword(t,eKeyword))
			return t;
		t = t->pPrev;
	}

	return NULL;
}

CXXToken * cxxTokenChainNextKeyword(
		CXXToken * from,
		enum CXXKeyword eKeyword
	)
{
	if(!from)
		return NULL;

	CXXToken * t = from->pNext;
	while(t)
	{
		if(cxxTokenIsKeyword(t,eKeyword))
			return t;
		t = t->pNext;
	}

	return NULL;
}

int cxxTokenChainFirstKeywordIndex(
		CXXTokenChain * tc,
		enum CXXKeyword eKeyword
	)
{
	if(!tc)
		return -1;
	if(tc->iCount < 1)
		return -1;

	CXXToken * pToken = tc->pHead;
	int idx = 0;
	while(pToken)
	{
		if(cxxTokenIsKeyword(pToken,eKeyword))
			return idx;
		idx++;
		pToken = pToken->pNext;
	}

	return -1;
}

#if 0
// This is working code but it's unused and coveralls complains.. sigh.
// Remove the #if above if needed.
CXXToken * cxxTokenChainFirstKeyword(
		CXXTokenChain * tc,
		enum CXXKeyword eKeyword
	)
{
	if(!tc)
		return NULL;
	if(tc->iCount < 1)
		return NULL;

	CXXToken * pToken = tc->pHead;
	while(pToken)
	{
		if(cxxTokenIsKeyword(pToken,eKeyword))
			return pToken;
		pToken = pToken->pNext;
	}

	return NULL;
}
#endif

CXXToken * cxxTokenChainNextIdentifier(
		CXXToken * from,
		const char * szIdentifier
	)
{
	if(!from)
		return NULL;

	CXXToken * t = from->pNext;
	while(t)
	{
		if(
			cxxTokenTypeIs(t,CXXTokenTypeIdentifier) &&
			(strcmp(vStringValue(t->pszWord),szIdentifier) == 0)
		)
			return t;
		t = t->pNext;
	}

	return NULL;
}

void cxxTokenChainDestroyRange(CXXTokenChain * pChain,CXXToken * from,CXXToken * to)
{
	if(!from || !to)
		return;
	CXX_DEBUG_ASSERT(from,"Bad from pointer passed to cxxTokenChainDestroyRange");
	CXX_DEBUG_ASSERT(to,"Bad to pointer passed to cxxTokenChainDestroyRange");

	for(;;)
	{
		CXXToken * next = from->pNext;
		cxxTokenChainTake(pChain,from);
		cxxTokenDestroy(from);
		if(from == to) // may be compared even if invalid
			return;
		from = next;
		CXX_DEBUG_ASSERT(from,"Should NOT have found chain termination here");
	}
}


CXXToken * cxxTokenChainExtractRange(
		CXXToken * from,
		CXXToken * to,
		unsigned int uFlags
	)
{
	if(!from)
		return NULL;

	CXXToken * pToken = from;

	CXXToken * pRet = cxxTokenCreate();
	pRet->iLineNumber = pToken->iLineNumber;
	pRet->oFilePosition = pToken->oFilePosition;
	pRet->eType = pToken->eType;

	cxxTokenAppendToString(pRet->pszWord,pToken);
	if(
			(!(uFlags & CXXTokenChainExtractRangeNoTrailingSpaces)) &&
			pToken->bFollowedBySpace
		)
		vStringCatS(pRet->pszWord," ");
	pRet->bFollowedBySpace = pToken->bFollowedBySpace;

	while(pToken != to)
	{
		pToken = pToken->pNext;
		if(!pToken)
			return pRet;
		cxxTokenAppendToString(pRet->pszWord,pToken);
		if(
				(!(uFlags & CXXTokenChainExtractRangeNoTrailingSpaces)) &&
				pToken->bFollowedBySpace
			)
			vStringCatS(pRet->pszWord," ");
		pRet->bFollowedBySpace = pToken->bFollowedBySpace;
	}

	return pRet;
}

CXXToken * cxxTokenChainExtractIndexRange(
		CXXTokenChain * tc,
		int iFirstIndex,
		int iLastIndex,
		unsigned int uFlags
	)
{
	if(!tc)
		return NULL;
	if(iFirstIndex < 0)
		return NULL;
	if(iFirstIndex >= tc->iCount)
		return NULL;

	CXXToken * pToken = tc->pHead;
	int idx = 0;
	while(pToken && (idx < iFirstIndex))
	{
		idx++;
		pToken = pToken->pNext;
	}

	if(!pToken)
		return NULL;

	CXXToken * pRet = cxxTokenCreate();
	pRet->iLineNumber = pToken->iLineNumber;
	pRet->oFilePosition = pToken->oFilePosition;
	pRet->eType = pToken->eType;

	cxxTokenAppendToString(pRet->pszWord,pToken);
	if(
			(!(uFlags & CXXTokenChainExtractRangeNoTrailingSpaces)) &&
			pToken->bFollowedBySpace
		)
		vStringCatS(pRet->pszWord," ");
	pRet->bFollowedBySpace = pToken->bFollowedBySpace;

	while(idx < iLastIndex)
	{
		pToken = pToken->pNext;
		if(!pToken)
			return pRet;
		cxxTokenAppendToString(pRet->pszWord,pToken);
		if(
				(!(uFlags & CXXTokenChainExtractRangeNoTrailingSpaces)) &&
				pToken->bFollowedBySpace
			)
			vStringCatS(pRet->pszWord," ");
		pRet->bFollowedBySpace = pToken->bFollowedBySpace;
		idx++;
	}

	return pRet;
}

void cxxTokenChainNormalizeTypeNameSpacing(CXXTokenChain * pChain)
{
	if(!pChain)
		return;

	if(pChain->iCount < 1)
		return;

	cxxTokenChainNormalizeTypeNameSpacingInRange(pChain->pHead,pChain->pTail);
}

void cxxTokenChainNormalizeTypeNameSpacingInRange(CXXToken * pFrom,CXXToken * pTo)
{
	if(!pFrom || !pTo)
		return;

	// Goals:

	// int
	// unsigned short int
	// int *
	// unsigned short int **
	// const Class &
	// Class &&
	// int (*)(type &,type *)
	// unsigned short int[3];
	// ClassA<ClassB<type *,type>> <-- fixme: not sure about the trailing >>
	// Class<Something> (*)(type[])

	CXXToken * t = pFrom;

	for(;;)
	{
		if(cxxTokenTypeIsOneOf(
				t,
				CXXTokenTypeParenthesisChain | CXXTokenTypeSquareParenthesisChain
			))
		{
			cxxTokenChainNormalizeTypeNameSpacing(t->pChain);
			t->bFollowedBySpace = FALSE;
		} else if(cxxTokenTypeIsOneOf(t,
					CXXTokenTypeIdentifier | CXXTokenTypeKeyword |
						CXXTokenTypeGreaterThanSign |
						CXXTokenTypeAnd | CXXTokenTypeMultipleAnds
				))
		{
			t->bFollowedBySpace = t->pNext &&
				cxxTokenTypeIsOneOf(
						t->pNext,
						CXXTokenTypeParenthesisChain | CXXTokenTypeIdentifier |
							CXXTokenTypeKeyword | CXXTokenTypeStar |
							CXXTokenTypeAnd | CXXTokenTypeMultipleAnds
					);
		} else if(cxxTokenTypeIs(t,CXXTokenTypeStar))
		{
			t->bFollowedBySpace = t->pNext &&
				(!cxxTokenTypeIsOneOf(
						t->pNext,
						CXXTokenTypeStar | CXXTokenTypeComma |
							CXXTokenTypeClosingParenthesis
					));
		} else {
			t->bFollowedBySpace = FALSE;
		}

		if(t == pTo)
			break;

		t = t->pNext;
	}

	// Finally the chain has no space at end
	pTo->bFollowedBySpace = FALSE;
}
