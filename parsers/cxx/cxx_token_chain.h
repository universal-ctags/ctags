#ifndef ctags_cxx_token_chain_h_
#define ctags_cxx_token_chain_h_
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

struct _CXXTokenChain
{
	CXXToken * pHead;
	CXXToken * pTail;
	int iCount;
};

// The struct is typedef'd in cxx_token.h
// typedef struct _CXXTokenChain CXXTokenChain;

CXXTokenChain * cxxTokenChainCreate(void);
void cxxTokenChainDestroy(CXXTokenChain * tc);

// Note: you don't need to call this after cxxTokenChainCreate().
void cxxTokenChainInit(CXXTokenChain * tc);

void cxxTokenChainClear(CXXTokenChain * tc);

// Find a specified token and return its index
int cxxTokenChainFindToken(CXXTokenChain * tc,CXXToken * t);

// Find the first token with one of the specified types
CXXToken * cxxTokenChainFirstTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	);

// Find the first token with one of the specified types that comes
// after the specified token
CXXToken * cxxTokenChainNextTokenOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	);

// Find the first token with one of the specified types that comes
// before the specified token
CXXToken * cxxTokenChainPreviousTokenOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	);

// Find the first token with that is not one of the specified types
// that comes before the specified token
CXXToken * cxxTokenChainPreviousTokenNotOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	);

// Find the last token with one of the specified types
CXXToken * cxxTokenChainLastTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	);

// Find the last token with one of the specified types. Look also
// in nested () chains (only (), not [], {}...)
CXXToken * cxxTokenChainLastPossiblyNestedTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes,
		CXXTokenChain ** ppParentChain
	);

// Find the first token with one of the specified types. Look also
// in nested () chains (only (), not [], {}...)
CXXToken * cxxTokenChainFirstPossiblyNestedTokenOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes,
		CXXTokenChain ** ppParentChain
	);

// Find the first token with type that is not one of the specified types
CXXToken * cxxTokenChainFirstTokenNotOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	);


// Find the first token with type that is not accepted by PREDICATOR.
// PREDICATOR returns true when a token is acceptable.
CXXToken * cxxTokenChainNextTokenNotOfGeneric(
		CXXToken * t,
		bool (* predicator) (CXXToken *, void *),
		void *data
	);

// Find the first token with type that is not one of the specified types
// that comes after the specified token
CXXToken * cxxTokenChainNextTokenNotOfType(
		CXXToken * t,
		unsigned int uTokenTypes
	);

// Find the last token with type that is not one of the specified types
CXXToken * cxxTokenChainLastTokenNotOfType(
		CXXTokenChain * tc,
		unsigned int uTokenTypes
	);

// Specialized function to skip from a < to the matching > (used for
// templates). Nested <> pairs are skipped properly.
// Parenthesis chains are assumed to be condensed.
// Note that the function stops at the ending > and not past it.
CXXToken * cxxTokenChainSkipToEndOfTemplateAngleBracket(
		CXXToken * t
	);

// Specialized function to skip back from a > to the matching < (used for
// templates). Nested <> pairs are skipped properly.
// Parenthesis chains are assumed to be condensed.
// Note that the function stops at the initial < and not past it.
CXXToken * cxxTokenChainSkipBackToStartOfTemplateAngleBracket(
		CXXToken * t
	);

#define cxxTokenChainFirst(tc) (tc ? tc->pHead : NULL)
#define cxxTokenChainLast(tc) (tc ? tc->pTail : NULL)

CXXToken * cxxTokenChainAt(CXXTokenChain * tc,int index);

CXXToken * cxxTokenChainTakeFirst(CXXTokenChain * tc);
CXXToken * cxxTokenChainTakeLast(CXXTokenChain * tc);
#if 0
CXXToken * cxxTokenChainTakeAt(CXXTokenChain * tc,int index);
#endif
void cxxTokenChainTake(CXXTokenChain * tc,CXXToken * t);
bool cxxTokenChainTakeRecursive(CXXTokenChain * tc,CXXToken * t);

// Destroy the last token
#define cxxTokenChainDestroyLast(tc) \
		cxxTokenDestroy(cxxTokenChainTakeLast(tc))

// Destroy the first token
#define cxxTokenChainDestroyFirst(tc) \
		cxxTokenDestroy(cxxTokenChainTakeFirst(tc))

void cxxTokenChainDestroyRange(CXXTokenChain * pChain,CXXToken * from,CXXToken * to);

void cxxTokenChainAppend(CXXTokenChain * tc,CXXToken * t);
void cxxTokenChainPrepend(CXXTokenChain * tc,CXXToken * t);
void cxxTokenChainInsertAfter(CXXTokenChain * tc,CXXToken * before,CXXToken * t);

#if 0
// currently unused
void cxxTokenChainMoveEntries(
		CXXTokenChain * src,
		CXXTokenChain * dest
	);

void cxxTokenChainMoveEntryRange(
		CXXTokenChain * src,
		CXXToken * start,
		CXXToken * end,
		CXXTokenChain * dest
	);
#endif

enum CXXTokenChainJoinFlags
{
	// Do not add trailing spaces for entries that are followed by space
	CXXTokenChainJoinNoTrailingSpaces = 1
};

void cxxTokenChainJoinInString(
		CXXTokenChain * tc,
		vString * s,
		const char * szSeparator,
		unsigned int uFlags
	);
vString * cxxTokenChainJoin(
		CXXTokenChain * tc,
		const char * szSeparator,
		unsigned int uFlags
	);

void cxxTokenChainJoinRangeInString(
		CXXToken * from,
		CXXToken * to,
		vString * s,
		const char * szSeparator,
		unsigned int uFlags
	);
vString * cxxTokenChainJoinRange(
		CXXToken * from,
		CXXToken * to,
		const char * szSeparator,
		unsigned int uFlags
	);

// Treat the token chain tc as a comma separated sequence
// of items (something, blah foo, 1 2 3 4 5, ...)
// Create a token chain that contains tokens corresponding
// to each item (i.e, "something", "blah foo", "1 2 3 4 5").
// Please note that the returned chain may be empty!
CXXTokenChain * cxxTokenChainSplitOnComma(CXXTokenChain * tc);


enum CXXTokenChainCondenseFlags
{
	// Do not add trailing spaces for entries that are followed by space
	CXXTokenChainCondenseNoTrailingSpaces = 1
};

CXXToken * cxxTokenChainCondenseIntoToken(CXXTokenChain * tc,unsigned int uFlags);
void cxxTokenChainCondense(CXXTokenChain * tc,unsigned int uFlags);


enum CXXTokenChainExtractRangeFlags
{
	CXXTokenChainExtractRangeNoTrailingSpaces = 1
};

CXXToken * cxxTokenChainExtractRange(
		CXXToken * from,
		CXXToken * to,
		unsigned int uFlags
	);

CXXToken * cxxTokenChainExtractRangeFilterTypeName(
		CXXToken * from,
		CXXToken * to
	);

CXXToken * cxxTokenChainExtractIndexRange(
		CXXTokenChain * tc,
		int iFirstIndex,
		int iLastIndex,
		unsigned int uFlags
	);

CXXToken * cxxTokenChainPreviousKeyword(
		CXXToken * from,
		CXXKeyword eKeyword
	);

CXXToken * cxxTokenChainNextKeyword(
		CXXToken * from,
		CXXKeyword eKeyword
	);

CXXToken * cxxTokenChainNextIdentifier(
		CXXToken * from,
		const char * szIdentifier
	);

int cxxTokenChainFirstKeywordIndex(
		CXXTokenChain * tc,
		CXXKeyword eKeyword
	);

#if 0
// This is working code but it's unused and coveralls complains.. sigh.
// Remove the #if above if needed.
CXXToken * cxxTokenChainFirstKeyword(
		CXXTokenChain * tc,
		CXXKeyword eKeyword
	);
#endif

// Assuming that pChain contains a type name, attempt to normalize the
// spacing within the whole chain.
//
// Please note that this will work also for entire function signatures
// (since type names can contain function pointers which have signatures)
void cxxTokenChainNormalizeTypeNameSpacing(
		CXXTokenChain * pChain
	);
void cxxTokenChainNormalizeTypeNameSpacingInRange(
		CXXToken * pFrom,
		CXXToken * pTo
	);

#endif //!ctags_cxx_token_chain_h_
