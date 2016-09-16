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

	// There may be multiple typedefs inside a single declaration.

	// [typedef] x y, *z;
	//   -> y is x
	//   -> z is x *

	// The angle brackets are not necessairly condensed in chains here
	// since we were parsing a generic statement which expected less-than
	// and greater-than operators to be present. We need to take care of that.


	while(pChain->iCount >= 2)
	{
		// Skip either to a comma or to the end.

		t = cxxTokenChainFirst(pChain);

		CXX_DEBUG_ASSERT(t,"There should be a token here!");

		CXXToken * pComma;

skip_to_comma_or_end:

		pComma = cxxTokenChainNextTokenOfType(
				t,
				CXXTokenTypeComma | CXXTokenTypeSmallerThanSign
			);

		if(pComma)
		{
			// Either a comma or <
			if(cxxTokenTypeIs(pComma,CXXTokenTypeSmallerThanSign))
			{
				CXX_DEBUG_PRINT("Found angle bracket, trying to skip it");

				t = cxxTokenChainSkipToEndOfTemplateAngleBracket(pComma);
				if(!t)
				{
					CXX_DEBUG_LEAVE_TEXT("Mismatched < sign inside typedef: giving up on it");
					return;
				}
				goto skip_to_comma_or_end;
			}

			CXX_DEBUG_PRINT("Found comma");

			// Really a comma!
			if((!pComma->pPrev) || (!pComma->pPrev->pPrev))
			{
				CXX_DEBUG_LEAVE_TEXT("Found comma but not enough tokens before it");
				return;
			}

			t = pComma->pPrev;
		} else {
			CXX_DEBUG_PRINT("Found no comma, pointing at end of declaration");

			t = cxxTokenChainLast(pChain);
		}

		CXX_DEBUG_ASSERT(t,"We should have found an identifier token here!");

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
		} else if(cxxTokenTypeIs(t,CXXTokenTypeIdentifier))
		{
			CXX_DEBUG_PRINT("Found straight typedef");
			pTParentChain = pChain;
		} else if((pAux = cxxTokenChainPreviousTokenOfType(t,CXXTokenTypeIdentifier)))
		{
			CXX_DEBUG_PRINT("Found typedef of something that might look like an array");
			t = pAux;
			pTParentChain = pChain;
		} else {
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
			CXXToken * pTypeName = NULL;

			cxxTokenChainTake(pTParentChain,t);

			// Avoid emitting typerefs for strange things like
			//  typedef MACRO(stuff,stuff) X;
			// or parsing errors we might make in ugly cases like
			//  typedef WHATEVER struct x { ... } y;
			if(
					(pTParentChain == pChain) && // not function pointer (see above)
					(
						pComma ?
							cxxTokenChainPreviousTokenOfType(
									pComma,
									CXXTokenTypeParenthesisChain | CXXTokenTypeAngleBracketChain
								) :
							cxxTokenChainLastTokenOfType(
									pChain,
									CXXTokenTypeParenthesisChain | CXXTokenTypeAngleBracketChain
								)
					)
				)
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Wild parenthesis in type definition: not emitting typeref"
					);
			} else {
				// other kind of typeref, use typename here.
				CXX_DEBUG_ASSERT(
						pChain->iCount > 0,
						"There should be at least another token here!"
					);

				pTypeName = cxxTagCheckAndSetTypeField(
							cxxTokenChainFirst(pChain),
							pComma ? pComma->pPrev : cxxTokenChainLast(pChain)
						);
			}

			tag->isFileScope = !isInputHeaderFile();

			cxxTagCommit();
			cxxTokenDestroy(t);
			if(pTypeName)
				cxxTokenDestroy(pTypeName);
		}

		if(!pComma)
			break;

		// We must kill anything up to either an identifier, a keyword (type) or > which is
		// assumed to be part of a template.

		while(
				pComma->pPrev &&
				(
					!cxxTokenTypeIsOneOf(
							pComma->pPrev,
							CXXTokenTypeIdentifier | CXXTokenTypeKeyword |
							CXXTokenTypeGreaterThanSign | CXXTokenTypeAngleBracketChain
						)
				)
			)
		{
			pAux = pComma->pPrev;
			cxxTokenChainTake(pChain,pAux);
			cxxTokenDestroy(pAux);
		}

		// got a comma.
		cxxTokenChainTake(pChain,pComma);
		cxxTokenDestroy(pComma);
	}

	CXX_DEBUG_LEAVE();
	return;
}
