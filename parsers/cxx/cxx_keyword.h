#ifndef ctags_cxx_keyword_h_
#define ctags_cxx_keyword_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"
#include "parse.h"

// WARNING: There is a table in cxx_keyword.c that must match order in this enum
typedef enum _CXXKeyword
{
	CXXKeyword__ATTRIBUTE__, // GCC
	CXXKeyword__CONSTANT__, // CUDA
	CXXKeyword__DECLSPEC, // Microsoft C/C++
	CXXKeyword__DEVICE__, // CUDA
	CXXKeyword__FASTCALL, // Microsoft C/C++
	CXXKeyword__FORCEINLINE, // Microsoft C/C++
	CXXKeyword__FORCEINLINE__, // CUDA
	CXXKeyword__GLOBAL__, // CUDA
	CXXKeyword__HOST__, // CUDA
	CXXKeyword__INLINE, // Microsoft C/C++
	CXXKeyword__INLINE__, // GCC
	CXXKeyword__MANAGED__, // CUDA
	CXXKeyword__NOINLINE__, // CUDA
	CXXKeyword__RESTRICT, // Microsoft C/C++
	CXXKeyword__RESTRICT__, // CUDA
	CXXKeyword__SHARED__, // CUDA
	CXXKeyword__STDCALL, // Microsoft C/C++
	CXXKeyword__THISCALL, // Microsoft C/C++
	CXXKeywordALIGNAS, // (since C++11)
	CXXKeywordALIGNOF, // (since C++11)
	//CXXKeywordAND,
	//CXXKeywordAND_EQ,
	CXXKeywordASM,
	CXXKeywordAUTO,
	//CXXKeywordBITAND,
	//CXXKeywordBITOR,
	CXXKeywordBOOL,
	CXXKeywordBREAK,
	CXXKeywordCASE,
	CXXKeywordCATCH,
	CXXKeywordCHAR,
	CXXKeywordCHAR16_T, // (since C++11)
	CXXKeywordCHAR32_T, // (since C++11)
	CXXKeywordCLASS,
	//CXXKeywordCOMPL,
	CXXKeywordCONCEPT, // Concepts TS
	CXXKeywordCONST,
	CXXKeywordCONSTEXPR, // (since C++11)
	CXXKeywordCONST_CAST,
	CXXKeywordCONTINUE,
	CXXKeywordDECLTYPE, // (since C++11)
	CXXKeywordDEFAULT,
	CXXKeywordDELETE,
	CXXKeywordDO,
	CXXKeywordDOUBLE,
	CXXKeywordDYNAMIC_CAST,
	CXXKeywordELSE,
	CXXKeywordENUM,
	CXXKeywordEXPLICIT,
	CXXKeywordEXPORT,
	CXXKeywordEXTERN,
	CXXKeywordFALSE,
	CXXKeywordFINAL, // not really a keyword, has meanings in some specific contexts
	CXXKeywordFLOAT,
	CXXKeywordFOR,
	CXXKeywordFRIEND,
	CXXKeywordGOTO,
	CXXKeywordIF,
	CXXKeywordINLINE,
	CXXKeywordINT,
	CXXKeywordLONG,
	CXXKeywordMUTABLE,
	CXXKeywordNAMESPACE,
	CXXKeywordNEW,
	CXXKeywordNOEXCEPT, // (since C++11)
	//CXXKeywordNOT,
	//CXXKeywordNOT_EQ,
	CXXKeywordNULLPTR, // (since C++11)
	CXXKeywordOPERATOR,
	//CXXKeywordOR,
	//CXXKeywordOR_EQ,
	//CXXKeywordOVERRIDE, // not really a keyword, has meanings in some specific contexts
	CXXKeywordPRIVATE,
	CXXKeywordPROTECTED,
	CXXKeywordPUBLIC,
	CXXKeywordREGISTER,
	CXXKeywordREINTERPRET_CAST,
	CXXKeywordREQUIRES, // (Concepts TS)
	CXXKeywordRESTRICT, // C99 extension
	CXXKeywordRETURN,
	CXXKeywordSHORT,
	CXXKeywordSIGNED,
	CXXKeywordSIZEOF,
	CXXKeywordSTATIC,
	CXXKeywordSTATIC_ASSERT, // (since C++11)
	CXXKeywordSTATIC_CAST,
	CXXKeywordSTRUCT,
	CXXKeywordSWITCH,
	CXXKeywordTEMPLATE,
	CXXKeywordTHIS,
	CXXKeywordTHREAD_LOCAL, // (since C++11)
	CXXKeywordTHROW,
	CXXKeywordTRUE,
	CXXKeywordTRY,
	CXXKeywordTYPEDEF,
	CXXKeywordTYPEID,
	CXXKeywordTYPENAME,
	CXXKeywordUNION,
	CXXKeywordUNSIGNED,
	CXXKeywordUSING,
	CXXKeywordVIRTUAL,
	CXXKeywordVOID,
	CXXKeywordVOLATILE,
	CXXKeywordWCHAR_T,
	CXXKeywordWHILE,
	//CXXKeywordXOR,
	//CXXKeywordXOR_EQ,
	// WARNING: There is a table in cxx_keyword.c that must match order in this enumeration
} CXXKeyword;

bool cxxKeywordIsConstant(CXXKeyword eKeywordId);
bool cxxKeywordMayBePartOfTypeName(CXXKeyword eKeywordId);
bool cxxKeywordIsTypeRefMarker(CXXKeyword eKeywordId);
bool cxxKeywordExcludeFromTypeNames(CXXKeyword eKeywordId);
bool cxxKeywordMayAppearInVariableDeclaration(CXXKeyword eKeywordId);
bool cxxKeywordIsCPPSpecific(CXXKeyword eKeywordId);


const char * cxxKeywordName(CXXKeyword eKeywordId);

// uLanguage is really CXXLanguage, but we keep it as unsigned int to avoid
// problems with header inclusions. It works anyway.
void cxxBuildKeywordHash(const langType eLangType,unsigned int uLanguage);

// Keyword enabled/disabled state management.
//
// public, protected, private, class, namespace... keywords are C++ only.
// However when parsing .h files we don't know if they belong to a C program or C++
// one and thus for safety we parse them as C++. If our guess is wrong then the parser
// may become confused and in some cases even bail out.
//
// For this reason we enable/disable the processing of certain keyword sets
// in certain contexts.


//
// "public,protected,private" keywords
//
// In header files we disable processing of such keywords until we either figure
// out that the file really contains C++ or we start parsing a struct/union.
//
// This flag is meaningful only when parsing a .h file as C++ since in C
// public/protected/private are never keywords and we assume that .cpp files
// have C++ content (so public/protected/private are always keywords).
//
// This function returns the previous state of the public/protected/private keywords
// enabled flag so it can be easily restored.
bool cxxKeywordEnablePublicProtectedPrivate(bool bEnableIt);

//
// "final" keyword
//
// This is actually special at C++ level: it's a keyword only within a specific part
// of a class declaration. In other contexts it's not a keyword.
void cxxKeywordEnableFinal(bool bEnableIt);

// Is the specific keyword currently disabled?
bool cxxKeywordIsDisabled(CXXKeyword eKeywordId);


#endif //!ctags_cxx_keyword_h_