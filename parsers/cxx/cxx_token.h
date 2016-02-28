#ifndef _cxx_token_h_
#define _cxx_token_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"
#include "vstring.h"

#include "cxx_keyword.h"

// We assume that the compiler is capable of generating 32 bit wide enums
// This is used as enumeration but also as mask in several functions.
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
	CXXTokenTypeCharacterConstant = (1 << 16),
	CXXTokenTypeMultipleDots = (1 << 17), // ...

	// These must come in pairs. Note that the opening
	// tokens can be shifted by 4 to get the matching closing
	// tokens and by 8 to get the matching subchain marker below
	CXXTokenTypeOpeningBracket = (1 << 18), // {
	CXXTokenTypeOpeningParenthesis = (1 << 19), // (
	CXXTokenTypeOpeningSquareParenthesis = (1 << 20), // [
	CXXTokenTypeSmallerThanSign = (1 << 21), // <
	
	CXXTokenTypeClosingBracket = (1 << 22), // }
	CXXTokenTypeClosingParenthesis = (1 << 23), // )
	CXXTokenTypeClosingSquareParenthesis = (1 << 24), // ]
	CXXTokenTypeGreaterThanSign = (1 << 25), // >
	
	// Subchains (caution: read the comment above about CXXTokenTypeOpeningBracket
	CXXTokenTypeBracketChain = (1 << 26), // {...}
	CXXTokenTypeParenthesisChain = (1 << 27), // (...)
	CXXTokenTypeSquareParenthesisChain = (1 << 28), // [...]
	CXXTokenTypeAngleBracketChain = (1 << 29), // <...>
};

// Forward decl
typedef struct _CXXTokenChain CXXTokenChain;



typedef struct _CXXToken
{
	enum CXXTokenType eType;
	vString * pszWord;
	enum CXXKeyword eKeyword;
	CXXTokenChain * pChain;
	boolean bFollowedBySpace;

	int iLineNumber;
	fpos_t oFilePosition;

	struct _CXXToken * pNext;
	struct _CXXToken * pPrev;
	
	// These members are used by the scope management functions to store the scope informations.
	// Only cxxScope* functions can make sense of it. In other contexts these are simply left
	// uninitialized and must be treated as undefined.
	unsigned char uInternalScopeKind;
	unsigned char uInternalScopeAccess;
} CXXToken;

CXXToken * cxxTokenCreate();
void cxxTokenDestroy(CXXToken * t);
CXXToken * cxxTokenCreateAnonymousIdentifier();

// FIXME: Bad argument order
void cxxTokenAppendToString(vString * s,CXXToken * t);

void cxxTokenAPIInit();
void cxxTokenAPINewFile();
void cxxTokenAPIDone();


#endif //!_cxx_token_h_