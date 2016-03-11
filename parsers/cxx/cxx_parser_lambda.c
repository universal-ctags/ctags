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

#include "parse.h"
#include "vstring.h"
#include "get.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"

#include <string.h>

//
// This has to be called when pointing at an opening bracket in function scope.
// Returns NULL if it does not look to be a lambda invocation.
// Returns the lambda parameter parenthesis chain token if it DOES look like a lambda invocation.
//
CXXToken * cxxParserOpeningBracketIsLambda(void)
{
	// [ capture-list ] ( params ) mutable(optional) exception attribute -> ret { body }	(1)	
	// [ capture-list ] ( params ) -> ret { body }	(2)	
	// [ capture-list ] ( params ) { body }	(3)	
	// [ capture-list ] { body }	(4)	

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

	t = cxxTokenChainPreviousTokenOfType(t,CXXTokenTypeSquareParenthesisChain);
	if(!t)
		return NULL;

	t = t->pNext;

	if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
		return t;

	return NULL;
}

boolean cxxParserHandleLambda(CXXToken * pParenthesis)
{
	CXX_DEBUG_ENTER();

	CXXToken * pIdentifier = cxxTokenCreateAnonymousIdentifier(CXXTagKindFUNCTION);

	CXXTokenChain * pSave = g_cxx.pTokenChain;
	CXXTokenChain * pNew = cxxTokenChainCreate();
	g_cxx.pTokenChain = pNew;

	tagEntryInfo * tag = cxxTagBegin(
			vStringValue(pIdentifier->pszWord),
			CXXTagKindFUNCTION,
			pIdentifier
		);

	if(tag)
	{
		tag->isFileScope = TRUE;

		cxxTagCommit();
	}

	cxxScopePush(
			pIdentifier,
			CXXTagKindFUNCTION,
			CXXScopeAccessUnknown
		);

	if(pParenthesis && cxxTagKindEnabled(CXXTagKindPARAMETER))
	{
		CXX_DEBUG_ASSERT(pParenthesis->eType == CXXTokenTypeParenthesisChain,"The parameter must be really a parenthesis chain");
		CXXFunctionParameterInfo oParamInfo;
		if(cxxParserTokenChainLooksLikeFunctionParameterList(pParenthesis->pChain,&oParamInfo))
			cxxParserEmitFunctionParameterTags(&oParamInfo);
	}

	boolean bRet = cxxParserParseBlock(TRUE);

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
