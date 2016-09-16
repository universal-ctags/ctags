/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/
#include "cxx_parser.h"
#include "cxx_parser_internal.h"

#include "cxx_debug.h"
#include "cxx_keyword.h"
#include "cxx_token.h"
#include "cxx_token_chain.h"
#include "cxx_scope.h"
#include "cxx_tag.h"

#include "parse.h"
#include "vstring.h"
#include "lcpp.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"

#include <string.h>

//
// This has to be called when pointing at an opening bracket in function scope.
// Returns NULL if it does not look to be a lambda invocation.
// Returns the lambda parameter parenthesis chain token if it DOES look like a
// lambda invocation.
//
CXXToken * cxxParserOpeningBracketIsLambda(void)
{
	// [ capture-list ] ( params ) mutable(opt) exception attr -> ret {} (1)
	// [ capture-list ] ( params ) -> ret { body }	(2)
	// [ capture-list ] ( params ) { body }	(3)
	// [ capture-list ] { body }	(4)

	CXX_DEBUG_ASSERT(cxxParserCurrentLanguageIsCPP(),"C++ only");

	CXXToken * t = g_cxx.pToken->pPrev;

	if(!t)
		return NULL; // not a lambda

	// Check simple cases first

	// case 4
	if(cxxTokenTypeIs(t,CXXTokenTypeSquareParenthesisChain))
	{
		// very likely parameterless lambda
		return t;
	}

	// case 3
	if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
	{
		t = t->pPrev;
		if(!t)
			return NULL; // can't be

		if(cxxTokenTypeIs(t,CXXTokenTypeSquareParenthesisChain))
			return t->pNext;

		return NULL;
	}

	// Stop also at commas, so in very large structures we will not be searching far
	t = cxxTokenChainPreviousTokenOfType(
			t,
			CXXTokenTypeSquareParenthesisChain |
			CXXTokenTypeComma
		);

	if(!t)
		return NULL;

	if(!cxxTokenTypeIs(t,CXXTokenTypeSquareParenthesisChain))
		return NULL;

	t = t->pNext;

	if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
		return t;

	return NULL;
}

// In case of a lambda without parentheses this is the capture list token.
boolean cxxParserHandleLambda(CXXToken * pParenthesis)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(cxxParserCurrentLanguageIsCPP(),"C++ only");

	CXXToken * pIdentifier = cxxTokenCreateAnonymousIdentifier(CXXTagKindFUNCTION);

	CXXTokenChain * pSave = g_cxx.pTokenChain;
	CXXTokenChain * pNew = cxxTokenChainCreate();
	g_cxx.pTokenChain = pNew;

	tagEntryInfo * tag = cxxTagBegin(CXXTagKindFUNCTION,pIdentifier);

	CXXToken * pAfterParenthesis = pParenthesis ? pParenthesis->pNext : NULL;

	CXXToken * pCaptureList = NULL;

	if(pParenthesis)
	{
		if(cxxTokenTypeIs(pParenthesis,CXXTokenTypeSquareParenthesisChain))
		{
			// form (4) of lambda (see cxxParserOpeningBracketIsLambda()).
			pCaptureList = pParenthesis;
		} else if(
			pParenthesis->pPrev &&
			cxxTokenTypeIs(pParenthesis->pPrev,CXXTokenTypeSquareParenthesisChain)
		)
		{
			// other forms of lambda (see cxxParserOpeningBracketIsLambda()).
			pCaptureList = pParenthesis->pPrev;
		}
	}

	if(
		pAfterParenthesis &&
		cxxTokenTypeIs(pAfterParenthesis,CXXTokenTypeKeyword) &&
		(pAfterParenthesis->eKeyword == CXXKeywordCONST)
	)
		pAfterParenthesis = pAfterParenthesis->pNext;

	CXXToken * pTypeStart = NULL;
	CXXToken * pTypeEnd;

	if(
			pAfterParenthesis &&
			cxxTokenTypeIs(pAfterParenthesis,CXXTokenTypePointerOperator) &&
			pAfterParenthesis->pNext &&
			!cxxTokenTypeIs(pAfterParenthesis->pNext,CXXTokenTypeOpeningBracket)
		)
	{
		pTypeStart = pAfterParenthesis->pNext;
		pTypeEnd = pTypeStart;
		while(
				pTypeEnd->pNext &&
				(!cxxTokenTypeIs(pTypeEnd->pNext,CXXTokenTypeOpeningBracket))
			)
			pTypeEnd = pTypeEnd->pNext;

		while(
				(pTypeStart != pTypeEnd) &&
				cxxTokenTypeIs(pTypeStart,CXXTokenTypeKeyword) &&
				cxxKeywordExcludeFromTypeNames(pTypeStart->eKeyword)
			)
			pTypeStart = pTypeStart->pNext;
	}

	int iCorkQueueIndex = CORK_NIL;

	if(tag)
	{
		tag->isFileScope = TRUE;

		CXXToken * pTypeName;

		if(pTypeStart)
			pTypeName = cxxTagCheckAndSetTypeField(pTypeStart,pTypeEnd);
		else
			pTypeName = NULL;

		if(pCaptureList && cxxTagFieldEnabled(CXXTagCPPFieldLambdaCaptureList))
		{
			CXX_DEBUG_ASSERT(pCaptureList->pChain,"The capture list must be a chain");
			cxxTokenChainCondense(pCaptureList->pChain,0);
			CXX_DEBUG_ASSERT(
					cxxTokenChainFirst(pCaptureList->pChain),
					"Condensation should have created a single token in the chain"
				);
			cxxTagSetField(
					CXXTagCPPFieldLambdaCaptureList,
					vStringValue(cxxTokenChainFirst(pCaptureList->pChain)->pszWord)
				);
		}

		// FIXME: Properties?

		vString * pszSignature = NULL;
		if(cxxTokenTypeIs(pParenthesis,CXXTokenTypeParenthesisChain))
			pszSignature = cxxTokenChainJoin(pParenthesis->pChain,NULL,0);

		if(pszSignature)
			tag->extensionFields.signature = vStringValue(pszSignature);

		iCorkQueueIndex = cxxTagCommit();

		if(pTypeName)
			cxxTokenDestroy(pTypeName);

		if(pszSignature)
			vStringDelete(pszSignature);
	}

	cxxScopePush(
			pIdentifier,
			CXXScopeTypeFunction,
			CXXScopeAccessUnknown
		);

	if(
		pParenthesis &&
		cxxTokenTypeIs(pParenthesis,CXXTokenTypeParenthesisChain) &&
		cxxTagKindEnabled(CXXTagKindPARAMETER)
	)
	{
		CXXFunctionParameterInfo oParamInfo;
		if(cxxParserTokenChainLooksLikeFunctionParameterList(
				pParenthesis->pChain,&oParamInfo
			))
			cxxParserEmitFunctionParameterTags(&oParamInfo);
	}

	boolean bRet = cxxParserParseBlock(TRUE);

	if(iCorkQueueIndex > CORK_NIL)
		cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndex);

	cxxScopePop();

	pNew = g_cxx.pTokenChain; // May have been destroyed and re-created

	g_cxx.pTokenChain = pSave;
	g_cxx.pToken = pSave->pTail;

	// change the type of token so following parsing code is not confused too much
	g_cxx.pToken->eType = CXXTokenTypeAngleBracketChain;
	g_cxx.pToken->pChain = pNew;

	cxxTokenChainClear(pNew);

	CXXToken * t = cxxTokenCreate();
	t->eType = CXXTokenTypeOpeningBracket;
	vStringCatS(t->pszWord,"{");
	cxxTokenChainAppend(pNew,t);

	t = cxxTokenCreate();
	t->eType = CXXTokenTypeClosingBracket;
	vStringCatS(t->pszWord,"}");
	cxxTokenChainAppend(pNew,t);

	CXX_DEBUG_LEAVE();
	return bRet;
}
