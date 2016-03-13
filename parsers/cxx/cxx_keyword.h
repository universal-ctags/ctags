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

// WARNING: There is a table in cxx_keyword.c that must match the order in this enumeration
enum CXXKeyword
{
	CXXKeyword__ATTRIBUTE__, // GCC
	CXXKeyword__DECLSPEC, // Microsoft C/C++
	CXXKeyword__FASTCALL, // Microsoft C/C++
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
	CXXKeywordFINAL, // not really a keyword, but has meanings in some specific contexts
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
	//CXXKeywordOVERRIDE, // not really a keyword, but has meanings in some specific contexts
	CXXKeywordPRIVATE,
	CXXKeywordPROTECTED,
	CXXKeywordPUBLIC,
	CXXKeywordREGISTER,
	CXXKeywordREINTERPRET_CAST,
	CXXKeywordREQUIRES, // (Concepts TS)
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
	// WARNING: There is a table in cxx_keyword.c that must match the order in this enumeration
};

boolean cxxKeywordMayBePartOfTypeName(enum CXXKeyword eKeywordId);

void cxxBuildKeywordHash(const langType language,boolean bCXX);


#endif //!ctags_cxx_keyword_h_