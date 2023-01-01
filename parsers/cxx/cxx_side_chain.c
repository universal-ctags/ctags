/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for extracting information __attribute__
*   lists.
*/
#include "cxx_side_chain.h"

#include "cxx_debug.h"
#include "cxx_tag.h"
#include "cxx_token_chain.h"
#include "cxx_parser_internal.h"

#include "entry.h"

static const CXXToken *cxxExtractFirstArgumentInAttrs(const CXXToken * pToken)
{
	pToken = pToken->pNext;

	if(!pToken)
		return NULL;
	if(!pToken->pChain)
		return NULL;

	pToken = pToken->pChain->pHead;
	if(!pToken)
		return NULL;

	if(!cxxTokenTypeIs(pToken,CXXTokenTypeOpeningParenthesis))
		return NULL;

	pToken = pToken->pNext;
	if(!pToken)
		return NULL;

	if(!cxxTokenTypeIs(pToken,CXXTokenTypeStringConstant))
		return NULL;

	return pToken;
}

static void cxxScanAttrExtractSection(const CXXToken * pToken)
{
	const CXXToken *pArgToken = cxxExtractFirstArgumentInAttrs(pToken);

	if(pArgToken == NULL)
		return;

	Assert(vStringLength(pArgToken->pszWord));

	vStringChop(pArgToken->pszWord);
	cxxTagSetField(CXXTagFieldSection, vStringValue(pArgToken->pszWord)+1, true);

	tagEntryInfo e;
	static langType lang = LANG_AUTO;

	if(lang == LANG_AUTO)
		lang = getNamedLanguage("LdScript", 0);
	if(lang == LANG_IGNORE)
		goto out;

	static kindDefinition * kdef = NULL;
	if(kdef == NULL)
		kdef = getLanguageKindForName (lang, "inputSection");
	if(kdef == NULL)
		goto out;

	static roleDefinition *rdef = NULL;
	if(rdef == NULL)
		rdef = getLanguageRoleForName (lang, kdef->id, "destination");
	if(rdef == NULL)
		goto out;

	initForeignRefTagEntry(&e, vStringValue(pArgToken->pszWord)+1,
						   lang, kdef->id, rdef->id);
	makeTagEntry(&e);

 out:
	vStringPut(pArgToken->pszWord, '"');
}

static void cxxScanAttributes(const CXXTokenChain * pAttrChain)
{
	if(pAttrChain == NULL)
		return;

	CXXToken * t = pAttrChain->pHead;
	bool bSection = cxxTagFieldEnabled(CXXTagFieldSection)
		&& (cxxParserCurrentLanguageIsC() || cxxParserCurrentLanguageIsCPP());

	while(t)
	{
		if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain)
		   && t->pChain && t->pChain->pHead)
		{
			if(bSection)
			{
				CXXToken * s = cxxTokenChainNextIdentifier(t->pChain->pHead, "section");
				if(!s)
					s = cxxTokenChainNextIdentifier(t->pChain->pHead, "__section__");
				if(s)
					cxxScanAttrExtractSection(s);
			}
		}
		if(t == pAttrChain->pTail)
			break;
		t = t->pNext;
	}
}

void cxxSideChainScan(const CXXTokenChain * pSideChain)
{
	bool bAttr = false;
	if(pSideChain == NULL)
		return;

	CXXToken * t = pSideChain->pHead;
	while(t)
	{
		if(bAttr)
		{
			bAttr = false;
			if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
				cxxScanAttributes(t->pChain);
		}
		if(cxxTokenIsKeyword(t,CXXKeyword__ATTRIBUTE__))
			bAttr = true;

		if(t == pSideChain->pTail)
			break;
		t = t->pNext;
	}
}

void cxxSideChainCollectInRange(CXXToken *pStart, CXXToken *pEnd, CXXToken * pDest)
{
	do
	{
		if(pStart != pDest)
			cxxSideChainAppend(pStart, pDest);
		if(pStart == pEnd)
			break;
		pStart = pStart->pNext;
	}
	while(true);
}

void cxxSideChainAppendChain(CXXTokenChain * pSideChain, CXXToken * dest)
{
	if(!pSideChain)
		return;

	if(dest->pSideChain)
	{
		cxxTokenChainAppendEntries(pSideChain, dest->pSideChain);
		cxxTokenChainDestroy(pSideChain);
	} else
		dest->pSideChain = pSideChain;
}

void cxxSideChainAppend(CXXToken * src, CXXToken * dest)
{
	cxxSideChainAppendChain(src->pSideChain, dest);
	src->pSideChain = NULL;
}

CXXTokenChain * cxxSideChainEject(CXXToken * pToken)
{
	if(!pToken)
		return NULL;

	CXXTokenChain *pSideChain = pToken->pSideChain;
	pToken->pSideChain = NULL;
	return pSideChain;
}
