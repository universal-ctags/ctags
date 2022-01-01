#ifndef ctags_cxx_token_h_
#define ctags_cxx_token_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"
#include "mio.h"
#include "vstring.h"

#include "cxx_keyword.h"

// We assume that the compiler is capable of generating 32 bit wide enums
// This is used as enumeration but also as mask in several functions.
//
// DON'T FORGET TO RUN misc/gencxxtypedumper.sh after updating the elements.
//
enum CXXTokenType
{
	CXXTokenTypeEOF = 1,
	CXXTokenTypeIdentifier = (1 << 1),
	CXXTokenTypeKeyword = (1 << 2),
	CXXTokenTypeNumber = (1 << 3),
	CXXTokenTypeSingleColon = (1 << 4),
	CXXTokenTypeMultipleColons = (1 << 5),
	CXXTokenTypeSemicolon = (1 << 6),
	CXXTokenTypeComma = (1 << 7), // ,
	CXXTokenTypeAssignment = (1 << 8), // =
	CXXTokenTypeOperator = (1 << 9), // != == += ++ -= -- / whatever
	CXXTokenTypeUnknown = (1 << 10),
	CXXTokenTypeDotOperator = (1 << 11), // .
	CXXTokenTypePointerOperator = (1 << 12), // ->
	CXXTokenTypeStringConstant = (1 << 13),
	CXXTokenTypeStar = (1 << 14), // *
	CXXTokenTypeAnd = (1 << 15), // &
	CXXTokenTypeMultipleAnds = (1 << 16), // &&
	CXXTokenTypeCharacterConstant = (1 << 17),
	CXXTokenTypeMultipleDots = (1 << 18), // ...

	// These must come in pairs. Note that the opening
	// tokens can be shifted by 4 to get the matching closing
	// tokens can be shifted by 8 to get the matching subchain marker below
	CXXTokenTypeOpeningBracket = (1 << 19), // {
	CXXTokenTypeOpeningParenthesis = (1 << 20), // (
	CXXTokenTypeOpeningSquareParenthesis = (1 << 21), // [
	CXXTokenTypeSmallerThanSign = (1 << 22), // <

	CXXTokenTypeClosingBracket = (1 << 23), // }
	CXXTokenTypeClosingParenthesis = (1 << 24), // )
	CXXTokenTypeClosingSquareParenthesis = (1 << 25), // ]
	CXXTokenTypeGreaterThanSign = (1 << 26), // >

	// Subchains (caution: read the comment above about CXXTokenTypeOpeningBracket)
	CXXTokenTypeBracketChain = (1 << 27), // {...}
	CXXTokenTypeParenthesisChain = (1 << 28), // (...)
	CXXTokenTypeSquareParenthesisChain = (1 << 29), // [...]
	CXXTokenTypeAngleBracketChain = (1 << 30), // <...>
};

// Forward decl
typedef struct _CXXTokenChain CXXTokenChain;



typedef struct _CXXToken
{
	enum CXXTokenType eType;
	vString * pszWord;
	CXXKeyword eKeyword;
	CXXTokenChain * pChain; // this is NOT the parent chain!
	bool bFollowedBySpace;

	int iLineNumber;
	MIOPos oFilePosition;

	struct _CXXToken * pNext;
	struct _CXXToken * pPrev;

	// These members are used by the scope management functions to store
	// scope information. Only cxxScope* functions can make sense of it.
	// In other contexts these are simply left
	// uninitialized and must be treated as undefined.
	unsigned char uInternalScopeType;
	unsigned char uInternalScopeAccess;
} CXXToken;

CXXToken * cxxTokenCreate(void);
void cxxTokenDestroy(CXXToken * t);

// A shortcut for quickly creating a fake token.
CXXToken * cxxTokenCopy(CXXToken *pToken);

// A shortcut for quickly creating keyword tokens.
CXXToken * cxxTokenCreateKeyword(int iLineNumber,MIOPos oFilePosition,CXXKeyword eKeyword);

CXXToken * cxxTokenCreateAnonymousIdentifier(unsigned int uTagKind);

#define cxxTokenTypeIsOneOf(_pToken,_uTypes) (_pToken->eType & (_uTypes))
#define cxxTokenTypeIs(_pToken,_eType) (_pToken->eType == _eType)
#define cxxTokenIsKeyword(_pToken,_eKeyword) \
		( \
			(_pToken->eType == CXXTokenTypeKeyword) && \
			(_pToken->eKeyword == _eKeyword) \
		)
#define cxxTokenIsNonConstantKeyword(_pToken) \
		( \
			cxxTokenTypeIs(_pToken,CXXTokenTypeKeyword) && \
			(!cxxKeywordIsConstant(_pToken->eKeyword)) \
		)

// FIXME: Bad argument order
void cxxTokenAppendToString(vString * s,CXXToken * t);

void cxxTokenAPIInit(void);
void cxxTokenAPINewFile(void);
void cxxTokenAPIDone(void);

void cxxTokenReduceBackward (CXXToken *pStart);

#endif //!ctags_cxx_token_h_
