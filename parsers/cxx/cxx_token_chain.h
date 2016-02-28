#ifndef _cxx_token_chain_h_
#define _cxx_token_chain_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "cxx_token.h"
#include "cxx_keyword.h"

typedef struct _CXXTokenChain
{
	CXXToken * pHead;
	CXXToken * pTail;
	int iCount;
} CXXTokenChain;


CXXTokenChain * cxxTokenChainCreate();
void cxxTokenChainDestroy(CXXTokenChain * tc);

// Note: you don't need to call this after cxxTokenChainCreate().
void cxxTokenChainInit(CXXTokenChain * tc);

void cxxTokenChainClear(CXXTokenChain * tc);

// Find a specified token and return its index
int cxxTokenChainFindToken(CXXTokenChain * tc,CXXToken * t);

// Find the first token with one of the specified types
CXXToken * cxxTokenChainFirstTokenOfType(CXXTokenChain * tc,unsigned int uTokenTypes);

// Find the first token with one of the specified types that comes after the specified token
CXXToken * cxxTokenChainNextTokenOfType(CXXToken * t,unsigned int uTokenTypes);

// Find the first token with one of the specified types that comes before the specified token
CXXToken * cxxTokenChainPreviousTokenOfType(CXXToken * t,unsigned int uTokenTypes);

// Find the first token with that is not one of the specified types that comes before the specified token
CXXToken * cxxTokenChainPreviousTokenNotOfType(CXXToken * t,unsigned int uTokenTypes);

// Find the last token with one of the specified types
CXXToken * cxxTokenChainLastTokenOfType(CXXTokenChain * tc,unsigned int uTokenTypes);

// Find the last token with one of the specified types. Look also in nested () chains (only (), not [], {}...)
CXXToken * cxxTokenChainLastPossiblyNestedTokenOfType(CXXTokenChain * tc,unsigned int uTokenTypes);

// Find the first token with type that is not one of the specified types
CXXToken * cxxTokenChainFirstTokenNotOfType(CXXTokenChain * tc,unsigned int uTokenTypes);

// Find the first token with type that is not one of the specified types that comes after the specified token
CXXToken * cxxTokenChainNextTokenNotOfType(CXXToken * t,unsigned int uTokenTypes);

// Find the last token with type that is not one of the specified types
CXXToken * cxxTokenChainLastTokenNotOfType(CXXTokenChain * tc,unsigned int uTokenTypes);

// Specialized function to skip from a < to the matching > (used for templates). Nested <> pairs are skipped properly.
// Parenthesis chains are assumed to be condensed.
// Note that the function stops at the ending > and not past it.
CXXToken * cxxTokenChainSkipToEndOfAngleBracket(CXXToken * t);

inline CXXToken * cxxTokenChainFirst(CXXTokenChain * tc)
{
	return tc ? tc->pHead : NULL;
}

inline CXXToken * cxxTokenChainLast(CXXTokenChain * tc)
{
	return tc ? tc->pTail : NULL;
}

CXXToken * cxxTokenChainAt(CXXTokenChain * tc,int index);

CXXToken * cxxTokenChainTakeFirst(CXXTokenChain * tc);
CXXToken * cxxTokenChainTakeLast(CXXTokenChain * tc);
CXXToken * cxxTokenChainTakeAt(CXXTokenChain * tc,int index);
void cxxTokenChainTake(CXXTokenChain * tc,CXXToken * t);

// Destroy the last token
inline void cxxTokenChainDestroyLast(CXXTokenChain * tc)
{
	if(!tc)
		return;
	if(tc->iCount < 1)
		return;
	cxxTokenDestroy(cxxTokenChainTakeLast(tc));
}

// Destroy the first token
inline void cxxTokenChainDestroyFirst(CXXTokenChain * tc)
{
	if(!tc)
		return;
	if(tc->iCount < 1)
		return;
	cxxTokenDestroy(cxxTokenChainTakeFirst(tc));
}



void cxxTokenChainAppend(CXXTokenChain * tc,CXXToken * t);
void cxxTokenChainPrepend(CXXTokenChain * tc,CXXToken * t);

void cxxTokenChainMoveEntries(CXXTokenChain * src,CXXTokenChain * dest);

enum CXXTokenChainJoinFlags
{
	// Do not add trailing spaces for entries that are followed by space
	CXXTokenChainJoinNoTrailingSpaces = 1
};

void cxxTokenChainJoinInString(CXXTokenChain * tc,vString * s,const char * szSeparator,unsigned int uFlags);
vString * cxxTokenChainJoin(CXXTokenChain * tc,const char * szSeparator,unsigned int uFlags);

void cxxTokenChainJoinRangeInString(CXXToken * from,CXXToken * to,vString * s,const char * szSeparator,unsigned int uFlags);
vString * cxxTokenChainJoinRange(CXXToken * from,CXXToken * to,const char * szSeparator,unsigned int uFlags);


enum CXXTokenChainCondenseFlags
{
	// Do not add trailing spaces for entries that are followed by space
	CXXTokenChainCondenseNoTrailingSpaces = 1
};

void cxxTokenChainCondense(CXXTokenChain * tc,unsigned int uFlags);

enum CXXTokenChainExtractRangeFlags
{
	CXXTokenChainExtractRangeNoTrailingSpaces = 1
};

CXXToken * cxxTokenChainExtractRange(CXXToken * from,CXXToken * to,unsigned int uFlags);
CXXToken * cxxTokenChainExtractIndexRange(CXXTokenChain * tc,int iFirstIndex,int iLastIndex,unsigned int uFlags);

CXXToken * cxxTokenChainPreviousKeyword(CXXToken * from,enum CXXKeyword eKeyword);

int cxxTokenChainFirstKeywordIndex(CXXTokenChain * tc,enum CXXKeyword eKeyword);

#endif //!_cxx_token_chain_h_