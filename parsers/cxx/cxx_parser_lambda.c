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
#include "../cpreprocessor.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"

#include <string.h>

//
// This has to be called when pointing at an opening bracket.
// Returns NULL if it does not look to be a lambda invocation.
// Otherwise it returns the parameter parenthesis token or the
// capture list square parenthesis token if the lambda is
// parameterless.
//
CXXToken * cxxParserOpeningBracketIsLambda(void)
{
	CXX_DEBUG_ENTER();

	// Lambda syntax variants:
	//
	// 1) [ capture-list ] ( params ) mutable(opt) exception attr -> ret { body }
	// 2) [ capture-list ] ( params ) -> ret { body }
	// 3) [ capture-list ] ( params ) { body }
	// 4) [ capture-list ] { body }

	// Similar, but not lambda:
	//
	// 5) type var[] { ... }
	// 6) operator [] ( params ) { ... }

	CXX_DEBUG_ASSERT(cxxParserCurrentLanguageIsCPP(),"C++ only");

	CXXToken * t = g_cxx.pToken->pPrev;

	if(!t)
	{
		CXX_DEBUG_LEAVE_TEXT("Not a lambda: no token before bracket");
		return NULL; // not a lambda
	}

	// Check simple cases first

	// case 4?
	if(cxxTokenTypeIs(t,CXXTokenTypeSquareParenthesisChain))
	{
		if(
			t->pPrev &&
			cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier)
		)
		{
			// case 5
			CXX_DEBUG_LEAVE_TEXT("Not a lambda: looks like type var[] { ... }");
			return NULL;
		}

		// very likely parameterless lambda
		CXX_DEBUG_LEAVE_TEXT("Likely a parameterless lambda");
		return t;
	}

	// case 3?
	if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
	{
		t = t->pPrev;
		if(!t)
		{
			CXX_DEBUG_LEAVE_TEXT("Not a lambda: nothing before ()");
			return NULL; // can't be
		}

		if(!cxxTokenTypeIs(t,CXXTokenTypeSquareParenthesisChain))
		{
			CXX_DEBUG_LEAVE_TEXT("Not a lambda: no [] before ()");
			return NULL; // can't be
		}

		if(
			t->pPrev &&
			// namely: operator [], operator new[], operator delete[]
			cxxTokenTypeIs(t->pPrev,CXXTokenTypeKeyword)
		)
		{
			// case 6
			CXX_DEBUG_LEAVE_TEXT("Not a lambda: keyword before []");
			return NULL;
		}

		CXX_DEBUG_LEAVE_TEXT("Looks like a lambda with parameters");
		return t->pNext;
	}

	// Handle the harder cases.
	// Look backwards for the square parenthesis chain, but stop at
	// tokens that shouldn't be present between the bracket and the
	// parenthesis.
	t = cxxTokenChainPreviousTokenOfType(
			t,
			CXXTokenTypeSquareParenthesisChain |
			CXXTokenTypeBracketChain |
			CXXTokenTypeAssignment |
			CXXTokenTypeOperator
		);

	if(!t)
	{
		CXX_DEBUG_LEAVE_TEXT("Not a lambda: no []");
		return NULL;
	}

	if(!cxxTokenTypeIs(t,CXXTokenTypeSquareParenthesisChain))
	{
		CXX_DEBUG_LEAVE_TEXT("Not a lambda: no [] before assignment or operator");
		return NULL;
	}

	if(
		t->pPrev &&
		// namely: operator [], operator new[], operator delete[]
		cxxTokenTypeIs(t->pPrev,CXXTokenTypeKeyword)
	)
	{
		// case 6
		CXX_DEBUG_LEAVE_TEXT("Not a lambda: keyword before []");
		return NULL;
	}

	t = t->pNext;

	if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
	{
		CXX_DEBUG_LEAVE_TEXT("Looks like a lambda (got () after [])");
		return t;
	}

	CXX_DEBUG_LEAVE_TEXT("Not a lambda: no () after []");
	return NULL;
}

// In case of a parameterless lambda (that has no parenthesis) the parameter
// is the capture list token.
bool cxxParserHandleLambda(CXXToken * pParenthesis)
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

#if 0
		while(
				(pTypeStart != pTypeEnd) &&
				cxxTokenTypeIs(pTypeStart,CXXTokenTypeKeyword) &&
				cxxKeywordExcludeFromTypeNames(pTypeStart->eKeyword)
			)
			pTypeStart = pTypeStart->pNext;
#endif
	}

	int iCorkQueueIndex = CORK_NIL;
	int iCorkQueueIndexFQ = CORK_NIL;

	if(tag)
	{
		tag->isFileScope = true;

		CXXToken * pTypeName;

		markTagExtraBit (tag, XTAG_ANONYMOUS);

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
					vStringValue(cxxTokenChainFirst(pCaptureList->pChain)->pszWord),
					false
				);
		}

		// FIXME: Properties?

		vString * pszSignature = NULL;
		if(cxxTokenTypeIs(pParenthesis,CXXTokenTypeParenthesisChain))
			pszSignature = cxxTokenChainJoin(pParenthesis->pChain,NULL,0);

		if(pszSignature)
			tag->extensionFields.signature = vStringValue(pszSignature);

		iCorkQueueIndex = cxxTagCommit(&iCorkQueueIndexFQ);

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
		CXXTypedVariableSet oParamInfo;
		if(cxxParserTokenChainLooksLikeFunctionParameterList(
				pParenthesis->pChain,&oParamInfo
			))
			cxxParserEmitFunctionParameterTags(&oParamInfo);
	}

	bool bRet = cxxParserParseBlock(true);

	if(iCorkQueueIndex > CORK_NIL)
	{
		cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndex);
		if(iCorkQueueIndexFQ > CORK_NIL)
			cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndexFQ);
	}

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
	vStringPut (t->pszWord, '{');
	cxxTokenChainAppend(pNew,t);

	t = cxxTokenCreate();
	t->eType = CXXTokenTypeClosingBracket;
	vStringPut (t->pszWord, '}');
	cxxTokenChainAppend(pNew,t);

	CXX_DEBUG_LEAVE();
	return bRet;
}
