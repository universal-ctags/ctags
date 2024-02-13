/*
*   Copyright (c) 2024, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*
* Reference:
* - https://en.cppreference.com/w/cpp/language/modules
*/

#include "cxx_parser.h"
#include "cxx_parser_internal.h"

#include "cxx_debug.h"
#include "cxx_keyword.h"
#include "cxx_token.h"
#include "cxx_token_chain.h"
#include "cxx_scope.h"

#include "parse.h"
#include "vstring.h"
#include "read.h"

static CXXToken * cxxTokenModuleTokenCreate(CXXToken *pBegin, CXXToken *pEnd)
{
	CXXToken * pRet = cxxTokenCopy(pBegin);

	if (pBegin != pEnd)
	{
		vString * pszRest = cxxTokenChainJoinRange(pBegin->pNext, pEnd, "",
												   CXXTokenChainJoinNoTrailingSpaces);
		vStringCat(pRet->pszWord, pszRest);
		vStringDelete(pszRest);
	}

	return pRet;
}

bool cxxParserParseModule(void)
{
	CXX_DEBUG_ENTER();

	unsigned int uProperties = 0;

	if(cxxTagFieldEnabled(CXXTagFieldProperties))
	{
		if(g_cxx.uKeywordState & CXXParserKeywordStateSeenExport)
			uProperties |= CXXTagPropertyExport;
	}

	cxxParserNewStatement();

	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeSemicolon | CXXTokenTypeEOF,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to the next ;");
		return false;
	}

	if (cxxTokenTypeIs(cxxTokenChainFirst(g_cxx.pTokenChain), CXXTokenTypeIdentifier))
	{
		CXXToken * pModBegin = cxxTokenChainFirst(g_cxx.pTokenChain);
		CXXToken * pSep = cxxTokenChainNextTokenOfType(pModBegin,
													   CXXTokenTypeSingleColon
													   | CXXTokenTypeSemicolon
													   | CXXTokenTypeEOF);
		bool bHasPart = (cxxTokenTypeIs(pSep, CXXTokenTypeSingleColon)
						 && pSep->pNext && cxxTokenTypeIs(pSep->pNext, CXXTokenTypeIdentifier)
						 && g_cxx.pToken->pPrev && g_cxx.pToken->pPrev != pSep);

		int iMod = CORK_NIL;
		{
			CXXToken * pMod = cxxTokenModuleTokenCreate(pModBegin, pSep->pPrev);
			tagEntryInfo * tag = cxxRefTagBegin(CXXTagCPPKindMODULE,
												bHasPart? CXXTagMODULERolePartOwner: ROLE_DEFINITION_INDEX,
												pMod);
			if (tag)
			{
				vString * pszProperties = (!bHasPart && uProperties) ? cxxTagSetProperties(uProperties) : NULL;

				iMod = cxxTagCommit(NULL);
				vStringDelete(pszProperties); /* NULL is acceptable. */
			}
			cxxTokenDestroy(pMod);
		}

		if (iMod != CORK_NIL && bHasPart)
		{
			CXXToken * pPart = cxxTokenModuleTokenCreate(pSep->pNext, g_cxx.pToken->pPrev);

			tagEntryInfo * tag = cxxTagBegin(CXXTagCPPKindPARTITION,pPart);
			if (tag)
			{
				vString * pszProperties = uProperties ? cxxTagSetProperties(uProperties) : NULL;

				cxxTagCommit(NULL);
				vStringDelete(pszProperties); /* NULL is acceptable */
			}
			cxxTokenDestroy(pPart);
		}
	}

	cxxTokenChainClear(g_cxx.pTokenChain);
	CXX_DEBUG_LEAVE();
	return true;
}
