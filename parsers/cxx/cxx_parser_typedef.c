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
#include "debug.h"
#include "read.h"

//
// This is used to pre-parse non struct/class/union/enum typedefs.
// Please note that struct/class/union/enum has its own pre-parsing routine.
//
boolean cxxParserParseGenericTypedef(void)
{
	CXX_DEBUG_ENTER();

	for(;;)
	{
		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeSemicolon | CXXTokenTypeEOF |
				CXXTokenTypeClosingBracket | CXXTokenTypeKeyword
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse fast statement");
			return FALSE;
		}

		// This fixes bug reported by Emil Rojas in 2002.
		// Though it's quite debatable if we really *should* do this.
		if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeKeyword))
		{
			// not a keyword
			if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
			{
				// not semicolon
				CXX_DEBUG_LEAVE_TEXT("Found EOF/closing bracket at typedef");
				return TRUE; // EOF
			}
			// semicolon: exit
			break;
		}

		// keyword
		if(
			(g_cxx.pToken->eKeyword == CXXKeywordEXTERN) ||
			(g_cxx.pToken->eKeyword == CXXKeywordTYPEDEF) ||
			(g_cxx.pToken->eKeyword == CXXKeywordSTATIC)
		)
		{
			CXX_DEBUG_LEAVE_TEXT("Found a terminating keyword inside typedef");
			return TRUE; // treat as semicolon but don't dare to emit a tag
		}
	}

	cxxParserExtractTypedef(g_cxx.pTokenChain,TRUE);
	CXX_DEBUG_LEAVE();
	return TRUE;
}

// This function attempts to extract a typedef from the
// specified chain.
// The typedef keyword should already have been removed (!)
// The function expects a terminator to be present at the end
// unless bExpectTerminatorAtEnd is set to FALSE
// The token chain may be condensed/destroyed upon exit.
void cxxParserExtractTypedef(
		CXXTokenChain * pChain,
		boolean bExpectTerminatorAtEnd
	)
{
	CXX_DEBUG_ENTER();

#ifdef CXX_DO_DEBUGGING
	vString * pX = cxxTokenChainJoin(pChain,NULL,0);
	CXX_DEBUG_PRINT("Extracting typedef from '%s'",vStringValue(pX));
	vStringDelete(pX);
#endif

	// Atleast something like
	//   a b;

	if(pChain->iCount < (bExpectTerminatorAtEnd ? 3 : 2))
	{
		CXX_DEBUG_LEAVE_TEXT("Not enough tokens for a type definition");
		return;
	}

	CXXToken * t;

	if(bExpectTerminatorAtEnd)
	{
		t = cxxTokenChainLast(pChain);
		CXX_DEBUG_ASSERT(
				cxxTokenTypeIs(t,CXXTokenTypeSemicolon),
				"The terminator should be present here"
			);
		cxxTokenChainDestroyLast(pChain);
	}

	t = cxxTokenChainLast(pChain);
	CXX_DEBUG_ASSERT(t,"We should have a last token here!");

	CXXTokenChain * pTParentChain;

	CXXToken * pAux;

	// check for special case of function pointer definition
	if(
		cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain) &&
		t->pPrev &&
		cxxTokenTypeIs(t->pPrev,CXXTokenTypeParenthesisChain) &&
		t->pPrev->pPrev &&
		(pAux = cxxTokenChainLastTokenOfType(
				t->pPrev->pChain,
				CXXTokenTypeIdentifier
			))
	)
	{
		CXX_DEBUG_PRINT("Found function pointer typedef");
		pTParentChain = t->pPrev->pChain;
		t = pAux;
	} else {
		t = cxxTokenChainLastTokenOfType(pChain,CXXTokenTypeIdentifier);
		pTParentChain = pChain;
	}

	if(!t)
	{
		CXX_DEBUG_LEAVE_TEXT("Didn't find an identifier");
		return; // EOF
	}

	if(!t->pPrev)
	{
		CXX_DEBUG_LEAVE_TEXT("No type before the typedef'd identifier");
		return; // EOF
	}

	tagEntryInfo * tag = cxxTagBegin(CXXTagKindTYPEDEF,t);

	if(tag)
	{
		pAux = t->pPrev;

		CXXToken * pTypeName = NULL;

		cxxTokenChainTake(pTParentChain,t);

		if(
				(pTParentChain == pChain) && // not function pointer (see above)
				cxxTokenChainFirstTokenOfType(pChain,CXXTokenTypeParenthesisChain)
			)
		{
			CXX_DEBUG_LEAVE_TEXT(
					"Wild parenthesis in type definition: not emitting typeref"
				);
		} else {
			// other kind of typeref, use typename here.
			CXX_DEBUG_ASSERT(pChain->iCount > 0,"There should be at least another token here!");

			pTypeName = cxxTagSetTypeField(
						tag,
						cxxTokenChainFirst(pChain),
						cxxTokenChainLast(pChain)
					);
		}

		tag->isFileScope = !isInputHeaderFile();

		cxxTagCommit();
		cxxTokenDestroy(t);
		if(pTypeName)
			cxxTokenDestroy(pTypeName);
	}

	CXX_DEBUG_LEAVE();
	return;
}
