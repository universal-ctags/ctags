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
bool cxxParserParseGenericTypedef(void)
{
	CXX_DEBUG_ENTER();

	for(;;)
	{
		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeSemicolon | CXXTokenTypeEOF |
				CXXTokenTypeClosingBracket | CXXTokenTypeKeyword,
				false
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse fast statement");
			return false;
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
				return true; // EOF
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
			return true; // treat as semicolon but don't dare to emit a tag
		}
	}

	cxxParserExtractTypedef(g_cxx.pTokenChain,true,false);
	CXX_DEBUG_LEAVE();
	return true;
}

//
// This function attempts to extract a typedef from the
// specified chain.
// The typedef keyword should already have been removed (!)
// The function expects a terminator to be present at the end
// unless bExpectTerminatorAtEnd is set to false
// The token chain may be condensed/destroyed upon exit.
//
// Samples:
//    [typedef] int x;
//        x = int
//
//    [typedef] struct y x;
//        x = struct y
//
//    [typedef] some complex type x, *y, **z;
//        x = some complex type
//        y = some complex type *
//        z = some complex type **
//
//    [typedef] int x[2];
//        x = int[2]
//
//    [typedef] int (*x)[2];
//        x = int (*)[2] <-- pointer to an array of two integers
//
//    [typedef] int *x[2];
//        x = int * [2] <-- array of two pointers to integer
//
//    [typedef] int (*x)(void);
//        x = int (*)(void) <--- function pointer
//
//    [typedef] int (x)(void)
//        x = int ()(void) <--- function type (not a pointer!)
//
//    [typedef] int x(void)
//        x = int ()(void) <--- still function type
//
//    [typedef] int (MACRO *x)(void);
//        x = int (MACRO *)(void) <--- function pointer
//    (WINAPI is an example of MACRO.)
//
//    [typedef] int (MACRO x)(void)
//        x = int (MACRO)(void) <--- function type (not a pointer!)
//    (WINAPI is an example of MACRO.)
//
//    [typedef] int ((x))(void)
//        x = int ()(void) <--- same as above
//
//    [typedef] int (*(*x)(int))[2];
//        x = int (*(*)(int))[2] <-- which is a function pointer taking an int and
//                                   returning a pointer to an array of two integers...
//
//    [typedef] blah (*x(k (*)(y *)))(z *);
//        x = blah (*(k (*)(y *)))(z *) <-- which is a function (!not a function pointer!)
//                                   taking a function pointer A as argument and returning
//                                   a function pointer B. A = k (*)(y *)
//                                   and B = blah (*)(z *)
//
// Note that not all syntaxes involving parentheses are valid.
// Examples of what is NOT valid:
//
//    [typedef] unsigned (int)(*x)()
//    [typedef] int[] x;
//
// So:
//  - if there is an identifier at the end, we use that
//  - if there are round parentheses then the identifier seems to be the first
//    one found in the nested parentheses chain
//  - if there are no round parentheses then the identifier is the last one
//    found in the toplevel chain
//
// In case of multiple declarations it seems that only the first part with identifiers
// and keywords is kept across types.
// Ex:
//    [typedef] int (*int2ptr)[2], baz;
//        int2ptr = int (*)[2]
//        baz     = int
//
void cxxParserExtractTypedef(
		CXXTokenChain * pChain,
		bool bExpectTerminatorAtEnd,
		bool bGotTemplate
	)
{
	CXX_DEBUG_ENTER();

#ifdef CXX_DO_DEBUGGING
	vString * pX = cxxTokenChainJoin(pChain,NULL,0);
	CXX_DEBUG_PRINT("Extracting typedef from '%s'",vStringValue(pX));
	vStringDelete(pX);
#endif

	// At least something like
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

	// The angle brackets are not necessarily condensed in chains here
	// since we were parsing a generic statement which expected less-than
	// and greater-than operators to be present. We need to take care of that.

	while(pChain->iCount >= 2)
	{
		//
		// Skip either to a comma or to the end, but keep track of the first parenthesis.
		//

		t = cxxTokenChainFirst(pChain);

		CXX_DEBUG_ASSERT(t,"There should be a token here!");

		CXXToken * pFirstParenthesis = NULL;
		int iSearchTypes = CXXTokenTypeComma | CXXTokenTypeSmallerThanSign |
								CXXTokenTypeParenthesisChain;
		CXXToken * pComma;

skip_to_comma_or_end:

		pComma = cxxTokenChainNextTokenOfType(t,iSearchTypes);

		if(pComma)
		{
			// , < or (
			if(cxxTokenTypeIs(pComma,CXXTokenTypeSmallerThanSign))
			{
				CXX_DEBUG_PRINT("Found angle bracket, trying to skip it");

				t = cxxTokenChainSkipToEndOfTemplateAngleBracket(pComma);
				if(!t)
				{
					CXX_DEBUG_LEAVE_TEXT("Mismatched < sign inside typedef: giving up on it");
					return;
				}
				// and go ahead
				goto skip_to_comma_or_end;
			}

			if(cxxTokenTypeIs(pComma,CXXTokenTypeParenthesisChain))
			{
				// We keep track only of the first one
				CXX_DEBUG_ASSERT(
						!pFirstParenthesis,
						"We should have stopped only at the first parenthesis"
					);

				iSearchTypes &= ~CXXTokenTypeParenthesisChain;
				pFirstParenthesis = pComma;
				// and go ahead
				goto skip_to_comma_or_end;
			}

			CXX_DEBUG_ASSERT(cxxTokenTypeIs(pComma,CXXTokenTypeComma),"Oops, expected a comma!");

			// Really a comma!
			CXX_DEBUG_PRINT("Found comma");

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

		CXX_DEBUG_ASSERT(t,"We should have found a token here!");

		//
		// Now look for the identifier
		//
		CXXTokenChain * pTParentChain;
		CXXToken * pLookupStart = t;

		if(cxxTokenTypeIs(t,CXXTokenTypeIdentifier))
		{
			// Use the identifier at end, whatever comes before
			CXX_DEBUG_PRINT("Identifier seems to be at end: %s",vStringValue(t->pszWord));
			pTParentChain = pChain;
		} else if(pFirstParenthesis)
		{
			CXX_DEBUG_PRINT("Identifier not at end, but got parenthesis chain");
			//
			// Possibly function pointer or function type definition.
			//
			//    typedef blah (*(foo))(baz)
			//    typedef blah (*foo)(baz)
			//    typedef blah (*foo)[...]
			//    typedef blah (foo)(baz)
			//    typedef blah ((foo))(baz)
			//    typedef blah foo(baz)
			//
			// So we have two cases: either there are at least two parentheses at the
			// top level or there is only one. If there are at least two then
			// we expect foo we want to capture to be at the end of the first nested
			// parenthesis chain. If there is only one then we expect it to be the
			// last identifier before the parenthesis.
			//
			if(
					pFirstParenthesis->pNext &&
					cxxTokenTypeIsOneOf(
							pFirstParenthesis->pNext,
							CXXTokenTypeParenthesisChain |
								CXXTokenTypeSquareParenthesisChain
						)
				)
			{
				CXX_DEBUG_PRINT("There are two parenthesis chains. Looking in the first one");
				t = cxxTokenChainLastPossiblyNestedTokenOfType(
						pFirstParenthesis->pChain,
						CXXTokenTypeIdentifier,
						&pTParentChain
					);
			} else {
				CXX_DEBUG_PRINT("There is one parenthesis chain. Looking just before");

				if(
					pFirstParenthesis->pPrev &&
					cxxTokenTypeIs(pFirstParenthesis->pPrev,CXXTokenTypeIdentifier)
				)
				{
					// Nasty "typedef blah foo(baz)" case.
					t = pFirstParenthesis->pPrev;

					// Let's have a consistent typeref too. We correct user
					// input so it becomes "typedef blah (foo)(baz)".

					pTParentChain = cxxTokenChainCreate();

					CXXToken * par = cxxTokenCreate();
					par->eType = CXXTokenTypeOpeningParenthesis;
					par->iLineNumber = t->iLineNumber;
					par->oFilePosition = t->oFilePosition;
					vStringPut(par->pszWord,'(');
					par->pChain = NULL;
					cxxTokenChainAppend(pTParentChain,par);

					par = cxxTokenCreate();
					par->eType = CXXTokenTypeIdentifier;
					par->iLineNumber = t->iLineNumber;
					par->oFilePosition = t->oFilePosition;
					vStringCopy(par->pszWord,t->pszWord);
					par->pChain = NULL;
					cxxTokenChainAppend(pTParentChain,par);

					t->eType = CXXTokenTypeParenthesisChain;
					t->pChain = pTParentChain;
					vStringClear(t->pszWord);

					pFirstParenthesis = t;
					t = par;

					par = cxxTokenCreate();
					par->eType = CXXTokenTypeClosingParenthesis;
					par->iLineNumber = t->iLineNumber;
					par->oFilePosition = t->oFilePosition;
					vStringPut(par->pszWord,')');
					par->pChain = NULL;
					cxxTokenChainAppend(pTParentChain,par);

				} else {
					CXX_DEBUG_LEAVE_TEXT("Parenthesis but no identifier: no clue");
					return;
				}
			}
		} else {
			// just scan backwards to the last identifier
			CXX_DEBUG_PRINT("No identifier and no parenthesis chain, trying to scan backwards");
			pTParentChain = pChain;
			t = cxxTokenChainPreviousTokenOfType(t,CXXTokenTypeIdentifier);
		}

		if(!t)
		{
			// Not found yet.
			// If we're in C++ mode but we haven't confirmed that the language is really C++
			// then we might try to look for a C++ identifier here.
			if(
				cxxParserCurrentLanguageIsCPP() &&
				(!g_cxx.bConfirmedCPPLanguage) &&
				cxxTokenTypeIs(pLookupStart,CXXTokenTypeKeyword) &&
				cxxKeywordIsCPPSpecific(pLookupStart->eKeyword)
			)
			{
				// treat as identifier
				pLookupStart->eType = CXXTokenTypeIdentifier;
				t = pLookupStart;
			}

			if(!t)
			{
				CXX_DEBUG_LEAVE_TEXT("Didn't find an identifier: something nasty is going on");
				return;
			}
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

			if(bGotTemplate)
				cxxTagHandleTemplateFields();

			cxxTagCommit(NULL);

			if (
					bGotTemplate &&
					cxxTagKindEnabled(CXXTagCPPKindTEMPLATEPARAM)
				)
			{
				cxxScopePush(t,CXXScopeTypeTypedef,CXXScopeAccessUnknown);
				cxxParserEmitTemplateParameterTags();
				cxxScopePop();
			} else {
				cxxTokenDestroy(t);
			}
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
			CXXToken * pAux = pComma->pPrev;
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
