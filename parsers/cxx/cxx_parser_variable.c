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

#include "vstring.h"
#include "read.h"



//
// Assumptions:
//  - this function assumes that a function definition or prototype has been already excluded by other means.
//  - there is a terminator at the end: one of ; = {
//
// Returns true if at least one variable was extracted.
//
boolean cxxParserExtractVariableDeclarations(CXXTokenChain * pChain,unsigned int uFlags)
{
	CXX_DEBUG_ENTER();

	if(pChain->iCount < 1)
	{
		CXX_DEBUG_LEAVE_TEXT("Chain is empty");
		return FALSE;
	}

#ifdef CXX_DO_DEBUGGING
	vString * pJoinedChain = cxxTokenChainJoin(pChain,NULL,0);
	CXX_DEBUG_PRINT("Looking for variable declarations in '%s'",vStringValue(pJoinedChain));
	vStringDelete(pJoinedChain);
#endif

	//
	// Examples of possible matches:
	//   type var;
	//   type var1,var2;
	//   type var[];
	//   type var(constructor args);
	//   type var = ...;
	//   type (*ident)();
	//   type var:bits;
	//   type var: range declaration <-- (FIXME: this is only inside for!)
	//   very complex type with modifiers() namespace::namespace::var = ...;
	//   type<with template> namespace::var[] = {
	//   ...
	//
	// Strategy:
	//   - verify that the chain starts with an identifier or keyword (always present)
	//   - run to one of : ; [] () = ,
	//   - ensure that the previous token is an identifier (except for special cases)
	//   - go back to skip the eventual scope
	//   - ensure that there is a leading type
	//   - if we are at : [] or () then run to the next ; = or ,
	//   - emit variable tag
	//   - if we are at , then check if there are more declarations
	//

	CXXToken * t = cxxTokenChainFirst(pChain);

	enum CXXTagKind eScopeKind = cxxScopeGetKind();

	CXX_DEBUG_ASSERT(t,"There should be an initial token here");

	if(!cxxTokenTypeIsOneOf(t,CXXTokenTypeIdentifier | CXXTokenTypeKeyword))
	{
		CXX_DEBUG_LEAVE_TEXT("Statement does not start with identifier or keyword");
		return FALSE;
	}

	// Handle the special case of delete/new keywords at the beginning
	if(
			cxxTokenTypeIs(t,CXXTokenTypeKeyword) &&
			(
				(t->eKeyword == CXXKeywordDELETE) ||
				(t->eKeyword == CXXKeywordNEW)
			)
		)
	{
		CXX_DEBUG_LEAVE_TEXT("Statement looks like a delete or new call");
		return FALSE;
	}

	CXXToken * pTypeEnd = NULL;

	boolean bGotVariable = FALSE;

	while(t)
	{
		while(t)
		{
			if(
				cxxTokenTypeIsOneOf(
						t,
						CXXTokenTypeSingleColon | CXXTokenTypeParenthesisChain | CXXTokenTypeSquareParenthesisChain |
						CXXTokenTypeAssignment | CXXTokenTypeComma | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket
					)
				)
			{
				// possibly a variable ?
				break;
			}

			if(t->eType == CXXTokenTypeSmallerThanSign)
			{
				t = cxxTokenChainSkipToEndOfTemplateAngleBracket(t);
				if(!t)
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to skip past angle bracket chain");
					return bGotVariable;
				}
				goto next_token;
			}

			if(
				cxxTokenTypeIsOneOf(
						t,
						CXXTokenTypeOperator | CXXTokenTypeMultipleAnds | CXXTokenTypePointerOperator |
						CXXTokenTypeBracketChain | CXXTokenTypeStringConstant | CXXTokenTypeAngleBracketChain |
						CXXTokenTypeCharacterConstant | CXXTokenTypeMultipleDots |
						CXXTokenTypeClosingBracket | CXXTokenTypeClosingParenthesis |
						CXXTokenTypeClosingSquareParenthesis | CXXTokenTypeSmallerThanSign
					)
				)
			{
				// Nope.
				CXX_DEBUG_LEAVE_TEXT("Found token '%s' of type 0x%02x that should not appear in the initial part of a variable declaration",vStringValue(t->pszWord),t->eType);
				return bGotVariable;
			}
next_token:
			t = t->pNext;
		}

		if(!t)
		{
			CXX_DEBUG_LEAVE_TEXT("Nothing interesting here");
			return bGotVariable;
		}

		CXX_DEBUG_PRINT("Found notable token '%s' of type 0x%02x",vStringValue(t->pszWord),t->eType);

		// Now before the notable token there MUST be an identifier (eventually hidden in a parenthesis chain)
		// and also a typename.
		if(!t->pPrev)
		{
			CXX_DEBUG_LEAVE_TEXT("Nothing interesting before notable token");
			return bGotVariable;
		}

		CXXToken * pIdentifier = NULL;
		CXXToken * pTokenBefore = NULL;

		if(cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain))
		{
			// check for function pointer special case
			if(
					t->pNext &&
					cxxTokenTypeIs(t->pNext,CXXTokenTypeParenthesisChain) &&
					(cxxParserTokenChainLooksLikeFunctionParameterList(t->pNext->pChain,NULL)) &&
					(pIdentifier = cxxTokenChainLastPossiblyNestedTokenOfType(t->pChain,CXXTokenTypeIdentifier))
				)
			{
				// ok, function pointer
				pTokenBefore = t->pPrev;
				t = t->pNext;
			} else if(
					(t->pChain->iCount == 3) &&
					cxxTokenTypeIs(cxxTokenChainAt(t->pChain,1),CXXTokenTypeParenthesisChain) &&
					cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier) && // we're in a parenthesis chain so there is a previous token here (at least the open paren)
					cxxTokenTypeIs(t->pPrev->pPrev,CXXTokenTypeIdentifier) // if the one above succeeded there is yet another previous token
				)
			{
				CXX_DEBUG_LEAVE_TEXT("Parenthesis seems to define an __ARGS style prototype");
				return bGotVariable;
			} else if(
					cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier) &&
					(
						(eScopeKind == CXXTagKindNAMESPACE) ||
						(eScopeKind == CXXTagKindFUNCTION)
					) &&
					cxxParserCurrentLanguageIsCPP() &&
					cxxParserTokenChainLooksLikeConstructorParameterSet(t->pChain)
				)
			{
				// ok, *might* be variable instantiation
				// (note that this function assumes that a function declaration of prototype was already excluded by other means)
				pIdentifier = t->pPrev;
				pTokenBefore = pIdentifier->pPrev;
			} else {
				CXX_DEBUG_LEAVE_TEXT("No recognizable parenthesis form for a variable");
				return bGotVariable;
			}
		} else {
			if(t->pPrev->eType != CXXTokenTypeIdentifier)
			{
				CXX_DEBUG_LEAVE_TEXT("No identifier before the notable token");
				return bGotVariable;
			}

			pIdentifier = t->pPrev;
			pTokenBefore = pIdentifier->pPrev;
		}

		CXX_DEBUG_ASSERT(pIdentifier,"We should have found an identifier here");

		if(!pTokenBefore)
		{
			CXX_DEBUG_LEAVE_TEXT("Identifier not preceeded by a type");
			return bGotVariable;
		}

		CXXToken * pScopeEnd = pTokenBefore->pNext;
		CXXToken * pScopeStart = NULL;

		// Skip back to any namespace
		while(pTokenBefore->eType == CXXTokenTypeMultipleColons)
		{
			pTokenBefore = pTokenBefore->pPrev;
			if(!pTokenBefore)
			{
				CXX_DEBUG_LEAVE_TEXT("Identifier preceeded by multiple colons but not preceeded by a type");
				return bGotVariable;
			}

			if(!cxxTokenTypeIs(pTokenBefore,CXXTokenTypeIdentifier))
			{
				CXX_DEBUG_LEAVE_TEXT("Identifier preceeded by multiple colons with probable syntax error");
				return bGotVariable;
			}

			pScopeStart = pTokenBefore;

			pTokenBefore = pTokenBefore->pPrev;
			if(!pTokenBefore)
			{
				CXX_DEBUG_LEAVE_TEXT("Identifier preceeded by multiple colons but not preceeded by a type");
				return bGotVariable;
			}
		}

		if(!pTypeEnd)
		{
			// now pTokenBefore should be part of the type
			if(!cxxTokenTypeIsOneOf(pTokenBefore,CXXTokenTypeIdentifier | CXXTokenTypeKeyword | CXXTokenTypeStar | CXXTokenTypeAnd))
			{
				if(cxxTokenTypeIs(pTokenBefore,CXXTokenTypeGreaterThanSign))
				{
					// the < > must be balanced
					CXXToken * t = pTokenBefore->pPrev;
					int iLevel = 1;
					while(t)
					{
						if(cxxTokenTypeIs(t,CXXTokenTypeGreaterThanSign))
							iLevel++;
						else if(cxxTokenTypeIs(t,CXXTokenTypeSmallerThanSign))
							iLevel--;
						t = t->pPrev;
					}
					if(iLevel != 0)
					{
						CXX_DEBUG_LEAVE_TEXT("The > token is unbalanced and does not seem to be part of type name");
						return bGotVariable;
					}
				} else {
					CXX_DEBUG_LEAVE_TEXT("Token '%s' of type 0x%02x does not seem to be part of type name",vStringValue(pTokenBefore->pszWord),pTokenBefore->eType);
					return bGotVariable;
				}
			}

			CXX_DEBUG_PRINT("Type name seems to end at '%s' of type 0x%02x",vStringValue(pTokenBefore->pszWord),pTokenBefore->eType);
			pTypeEnd = pTokenBefore;
		}

		// Goodie. We have an identifier and almost certainly a type here.

		int iScopesPushed = 0;

		if(pScopeStart)
		{
			while(pScopeStart != pScopeEnd)
			{
				CXXToken * pScopeId = pScopeStart;
				pScopeStart = cxxTokenChainNextTokenOfType(pScopeStart,CXXTokenTypeMultipleColons);
				CXX_DEBUG_ASSERT(pScopeStart,"We should have found multiple colons here!");
				pScopeStart = pScopeStart->pNext;

				cxxTokenChainTake(pChain,pScopeId);

				cxxScopePush(
						pScopeId,
						CXXTagKindCLASS,
						CXXScopeAccessUnknown // WARNING: We don't know if it's really a class! (FIXME?)
					);
				iScopesPushed++;
			}
		}

		bGotVariable = TRUE;

		boolean bKnRStyleParameters = (uFlags & CXXExtractVariableDeclarationsKnRStyleParameters);

		// FIXME: Typeref?
		tagEntryInfo * tag = cxxTagBegin(
				bKnRStyleParameters ?
					CXXTagKindPARAMETER :
					((g_cxx.uKeywordState & CXXParserKeywordStateSeenExtern) ? CXXTagKindEXTERNVAR : cxxScopeGetVariableKind()),
				pIdentifier
			);

		if(tag)
		{
			// handle very simple typerefs
			//   struct X y;
			if(
				pTypeEnd &&
				(pChain->iCount == 4) &&
				cxxTokenTypeIs(pTypeEnd,CXXTokenTypeIdentifier) &&
				pTypeEnd->pPrev &&
				cxxTokenTypeIs(pTypeEnd->pPrev,CXXTokenTypeKeyword) &&
				(
					(pTypeEnd->pPrev->eKeyword == CXXKeywordSTRUCT) ||
					(pTypeEnd->pPrev->eKeyword == CXXKeywordUNION) ||
					(pTypeEnd->pPrev->eKeyword == CXXKeywordCLASS) ||
					(pTypeEnd->pPrev->eKeyword == CXXKeywordENUM)
				)
			)
			{
				tag->extensionFields.typeRef[0] = vStringValue(pTypeEnd->pPrev->pszWord);
				tag->extensionFields.typeRef[1] = vStringValue(pTypeEnd->pszWord);
				CXX_DEBUG_PRINT("Typeref is %s:%s",tag->extensionFields.typeRef[0],tag->extensionFields.typeRef[1]);
			} else {
				CXX_DEBUG_PRINT("No typeref found");
			}

			tag->isFileScope = bKnRStyleParameters ?
							TRUE :
							(
								((eScopeKind == CXXTagKindNAMESPACE) && (g_cxx.uKeywordState & CXXParserKeywordStateSeenStatic) && !isInputHeaderFile()) ||
								(eScopeKind == CXXTagKindFUNCTION) || // locals are always hidden
								((eScopeKind != CXXTagKindNAMESPACE) && (eScopeKind != CXXTagKindFUNCTION) && (!isInputHeaderFile()))
							);

			cxxTagCommit();
		}

		while(iScopesPushed > 0)
		{
			cxxScopePop();
			iScopesPushed--;
		}

		if(cxxTokenTypeIsOneOf(t,CXXTokenTypeParenthesisChain | CXXTokenTypeSquareParenthesisChain | CXXTokenTypeSingleColon | CXXTokenTypeAssignment))
		{
			t = cxxTokenChainNextTokenOfType(t,CXXTokenTypeComma | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket);
			if(!t)
			{
				CXX_DEBUG_LEAVE_TEXT("Didn't find an assignment, comma, semicolon or {");
				return bGotVariable;
			}
		}

		if(cxxTokenTypeIsOneOf(t,CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket))
		{
			CXX_DEBUG_LEAVE_TEXT("Noting else");
			return bGotVariable;
		}

		// Comma. Might have other declarations here.
		CXX_DEBUG_PRINT("At a comma, might have other declarations here");
		t = t->pNext;
	}

	CXX_DEBUG_LEAVE_TEXT("Reached end");
	return bGotVariable;
}
