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

#include "../cpreprocessor.h"

#include "parse.h"
#include "vstring.h"
#include "read.h"
#include "trashbox.h"

static CXXToken * pCurrentModuleToken;

static void cxxParserSetCurrentModuleToken(CXXToken * pMod)
{
	if (pCurrentModuleToken)
		cxxTokenDestroy(pCurrentModuleToken);
	pCurrentModuleToken = pMod;
}

void cxxParserDestroyCurrentModuleToken(void)
{
	cxxParserSetCurrentModuleToken(NULL);
}


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

	CXXToken * pFirst = cxxTokenChainFirst(g_cxx.pTokenChain);
	if (cxxTokenTypeIsOneOf(pFirst, CXXTokenTypeSemicolon | CXXTokenTypeEOF))
	{
		CXXToken * pMod = cxxTokenCreateAnonymousIdentifier(CXXTagCPPKindMODULE, "__gmod_");
		tagEntryInfo * tag = cxxTagBegin(CXXTagCPPKindMODULE,pMod);
		if (tag)
			cxxTagCommit(NULL);
		cxxTokenDestroy(pMod);
	}
	else if (cxxTokenTypeIs(pFirst, CXXTokenTypeSingleColon)
		&& pFirst->pNext && cxxTokenIsKeyword(pFirst->pNext, CXXKeywordPRIVATE))
	{
		CXXToken * pPart = pFirst->pNext;
		if (pCurrentModuleToken)
		{
			CXXToken *pMod = cxxTokenCopy(pCurrentModuleToken);
			cxxScopePush(pMod, CXXScopeTypeModule, CXXScopeAccessUnknown);
		}

		tagEntryInfo * tag = cxxTagBegin(CXXTagCPPKindPARTITION,pPart);
		if (tag)
			cxxTagCommit(NULL);
		/* export is never set to the private partition. */

		if (pCurrentModuleToken)
			cxxScopePop();
	}
	else if (cxxTokenTypeIs(pFirst, CXXTokenTypeIdentifier))
	{
		CXXToken * pModBegin = pFirst;
		CXXToken * pSep = cxxTokenChainNextTokenOfType(pModBegin,
													   CXXTokenTypeSingleColon
													   | CXXTokenTypeSemicolon
													   | CXXTokenTypeEOF);
		bool bHasPart = (cxxTokenTypeIs(pSep, CXXTokenTypeSingleColon)
						 && pSep->pNext && cxxTokenTypeIs(pSep->pNext, CXXTokenTypeIdentifier)
						 && g_cxx.pToken->pPrev && g_cxx.pToken->pPrev != pSep);

		CXXToken * pMod = NULL;
		{
			pMod = cxxTokenModuleTokenCreate(pModBegin, pSep->pPrev);
			tagEntryInfo * tag = cxxRefTagBegin(CXXTagCPPKindMODULE,
												bHasPart? CXXTagMODULERolePartOwner: ROLE_DEFINITION_INDEX,
												pMod);
			if (tag)
			{
				vString * pszProperties = (!bHasPart && uProperties) ? cxxTagSetProperties(uProperties) : NULL;

				cxxTagCommit(NULL);
				cxxParserSetCurrentModuleToken(cxxTokenCopy(pMod));
				cxxScopePush(pMod, CXXScopeTypeModule, CXXScopeAccessUnknown);
				vStringDelete(pszProperties); /* NULL is acceptable. */
			}
			else
			{
				cxxParserSetCurrentModuleToken(pMod);
				pMod = NULL;
			}
		}

		if (pMod && bHasPart)
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

		if (pMod)
			cxxScopePop();
	}

	cxxTokenChainClear(g_cxx.pTokenChain);
	CXX_DEBUG_LEAVE();
	return true;
}

bool cxxParserParseImport(void)
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

	CXXToken *pFirst = cxxTokenChainFirst(g_cxx.pTokenChain);
	if (cxxTokenTypeIs(pFirst, CXXTokenTypeSmallerThanSign))
	{
		if (pFirst->pNext == NULL
			||  cxxTokenTypeIsOneOf(pFirst->pNext, CXXTokenTypeGreaterThanSign
									| CXXTokenTypeSemicolon
									| CXXTokenTypeEOF))
		{
			cxxTokenChainClear(g_cxx.pTokenChain);
			CXX_DEBUG_LEAVE_TEXT("Cannot find > on the chain");
			return true;
		}

		CXXToken * pHeaderBegin = pFirst->pNext;
		CXXToken * pSep = cxxTokenChainNextTokenOfType(pHeaderBegin,
													   CXXTokenTypeGreaterThanSign
													   | CXXTokenTypeSemicolon
													   | CXXTokenTypeEOF);
		CXX_DEBUG_ASSERT(pSep,"Cannot find the end of header file");
		CXX_DEBUG_ASSERT(pSep != pHeaderBegin, "Unexpected empty header file");

		CXXToken * pHeader = cxxTokenModuleTokenCreate(pHeaderBegin, pSep->pPrev);
		tagEntryInfo * tag = cxxRefTagBegin(CXXTagKindINCLUDE,CR_HEADER_SYSTEM,pHeader);
		if (tag)
		{
			assignRole(tag, CXXR_HEADER_IMPORTED);
			if (uProperties & CXXTagPropertyExport)
				assignRole(tag, CXXR_HEADER_EXPORTED);
			cxxTagCommit(NULL);
		}
		cxxTokenDestroy(pHeader);
	}
	else if (cxxTokenTypeIs(pFirst, CXXTokenTypeStringConstant))
	{
		const vString *pHName = cppGetLastCharOrStringContents();
		if (!pHName || vStringIsEmpty(pHName))
		{
			cxxTokenChainClear(g_cxx.pTokenChain);
			CXX_DEBUG_LEAVE_TEXT("Empty header name");
			return true;
		}

		vStringCopy(pFirst->pszWord, pHName);
		CXXToken *pHeader = pFirst;

		tagEntryInfo * tag = cxxRefTagBegin(CXXTagKindINCLUDE,CR_HEADER_LOCAL,pHeader);
		if (tag)
		{
			assignRole(tag, CXXR_HEADER_IMPORTED);
			if (uProperties & CXXTagPropertyExport)
				assignRole(tag, CXXR_HEADER_EXPORTED);
			cxxTagCommit(NULL);
		}
	}
	else if (cxxTokenTypeIs(pFirst, CXXTokenTypeSingleColon))
	{
		CXXToken * pPartBegin = pFirst->pNext;

		if (pPartBegin == NULL
			|| pPartBegin == g_cxx.pToken
			|| cxxTokenTypeIsOneOf(pPartBegin, CXXTokenTypeSemicolon
								   | CXXTokenTypeEOF))
		{
			cxxTokenChainClear(g_cxx.pTokenChain);
			CXX_DEBUG_LEAVE_TEXT("Empty partition name");
			return true;
		}

		CXXToken * pPart = cxxTokenModuleTokenCreate(pPartBegin, g_cxx.pToken->pPrev);
		if (pCurrentModuleToken)
		{
			CXXToken *pMod = cxxTokenCopy(pCurrentModuleToken);
			cxxScopePush(pMod, CXXScopeTypeModule, CXXScopeAccessUnknown);
		}

		tagEntryInfo * tag = cxxRefTagBegin(CXXTagCPPKindPARTITION,CXXTagPARTITIONRoleImported,pPart);
		if (tag)
		{
			vString * pszProperties = uProperties ? cxxTagSetProperties(uProperties) : NULL;
			cxxTagCommit(NULL);
			vStringDelete(pszProperties); /* NULL is acceptable. */
		}
		cxxTokenDestroy(pPart);

		if (pCurrentModuleToken)
			cxxScopePop();
	}
	else if (cxxTokenTypeIsOneOf(pFirst, CXXTokenTypeIdentifier))
	{
		CXXToken *pModBegin = pFirst;
		CXXToken * pSep = cxxTokenChainNextTokenOfType(pModBegin,
													   CXXTokenTypeSingleColon
													   | CXXTokenTypeSemicolon
													   | CXXTokenTypeEOF);
		bool bHasPart = (cxxTokenTypeIs(pSep, CXXTokenTypeSingleColon)
						 && pSep->pNext && cxxTokenTypeIs(pSep->pNext, CXXTokenTypeIdentifier)
						 && g_cxx.pToken->pPrev && g_cxx.pToken->pPrev != pSep);

		CXXToken * pMod = NULL;
		{
			pMod = cxxTokenModuleTokenCreate(pModBegin, pSep->pPrev);
			tagEntryInfo * tag = cxxRefTagBegin(CXXTagCPPKindMODULE,
												bHasPart? CXXTagMODULERolePartOwner: CXXTagMODULERoleImported,
												pMod);
			if (tag)
			{
				vString * pszProperties = (!bHasPart && uProperties) ? cxxTagSetProperties(uProperties) : NULL;

				cxxTagCommit(NULL);
				cxxScopePush(pMod, CXXScopeTypeModule, CXXScopeAccessUnknown);
				vStringDelete(pszProperties); /* NULL is acceptable. */
			}
			else
			{
				cxxTokenDestroy(pMod);
				pMod = NULL;
			}
		}

		if (pMod && bHasPart)
		{
			CXXToken * pPart = cxxTokenModuleTokenCreate(pSep->pNext, g_cxx.pToken->pPrev);

			tagEntryInfo * tag = cxxRefTagBegin(CXXTagCPPKindPARTITION,
												CXXTagPARTITIONRoleImported, pPart);
			if (tag)
			{
				vString * pszProperties = uProperties ? cxxTagSetProperties(uProperties) : NULL;

				cxxTagCommit(NULL);
				vStringDelete(pszProperties); /* NULL is acceptable */
			}
			cxxTokenDestroy(pPart);
		}

		if (pMod)
			cxxScopePop();
	}

	cxxTokenChainClear(g_cxx.pTokenChain);
	CXX_DEBUG_LEAVE();
	return true;
}
