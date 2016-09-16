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
// Attempt to extract variable declarations from the chain.
// Returns TRUE if at least one variable was extracted.
// Returns FALSE if a variable declaration could not be identified.
//
// Recognized variable declarations are of the following kinds:
//
//   type var;
//   type var1,var2;
//   type var[];
//   type var(constructor args);
//   type var{list initializer};
//   type var = ...;
//   type (*ident)();
//   type var:bits;
//   type var: range declaration <-- (FIXME: this is only inside for!)
//   very complex type with modifiers() namespace::namespace::var = ...;
//   type<with template> namespace::var[] = {
//   ...
//
// Assumptions:
//  - there is a terminator at the end: either ; or {
//
// Notes:
// - Be aware that if this function returns TRUE then the pChain very likely has been modified
//   (partially destroyed) as part of the type extraction algorithm.
//   If the function returns FALSE the chain has not been modified (and
//   to extract something else from it).
//
// - This function is quite tricky.
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
	CXX_DEBUG_PRINT(
			"Looking for variable declarations in '%s'",
			vStringValue(pJoinedChain)
		);
	vStringDelete(pJoinedChain);
#endif

	//
	// Strategy:
	//   - verify that the chain starts with an identifier or keyword (always present)
	//   - run to one of : ; [] () {} = ,
	//   - ensure that the previous token is an identifier (except for special cases)
	//   - go back to skip the eventual scope
	//   - ensure that there is a leading type
	//   - if we are at : [], () or {} then run to the next ; = or ,
	//   - once we have determined that a variable declaration is there
	//     modify the chain to contain only the type name
	//   - emit variable tag
	//   - if we are at , then check if there are more declarations
	//

	CXXToken * t = cxxTokenChainFirst(pChain);

	enum CXXScopeType eScopeType = cxxScopeGetType();

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

	boolean bGotVariable = FALSE;

	// Loop over the whole statement.

	while(t)
	{
		// Scan up to a notable token: ()[]{}=,;:{

		while(t)
		{
			if(cxxTokenTypeIsOneOf(
						t,
						CXXTokenTypeSingleColon | CXXTokenTypeParenthesisChain |
							CXXTokenTypeSquareParenthesisChain | CXXTokenTypeBracketChain |
							CXXTokenTypeAssignment | CXXTokenTypeComma |
							CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket
					))
			{
				// Notable token reached.
				break;
			}

			if(
				cxxTokenTypeIsOneOf(
						t,
						CXXTokenTypeOperator | CXXTokenTypeMultipleAnds |
							CXXTokenTypePointerOperator | CXXTokenTypeStringConstant |
							CXXTokenTypeAngleBracketChain | CXXTokenTypeCharacterConstant |
							CXXTokenTypeMultipleDots | CXXTokenTypeClosingBracket |
							CXXTokenTypeClosingParenthesis | CXXTokenTypeClosingSquareParenthesis |
							CXXTokenTypeGreaterThanSign
					)
				)
			{
				// Something that should not appear in a variable declaration
				CXX_DEBUG_LEAVE_TEXT(
						"Found token '%s' of type 0x%02x that should " \
							"not appear in the initial part of a variable declaration",
						vStringValue(t->pszWord),
						t->eType
					);
				return bGotVariable;
			}

			if(t->eType == CXXTokenTypeSmallerThanSign)
			{
				// Must be part of template type name (so properly balanced).
				t = cxxTokenChainSkipToEndOfTemplateAngleBracket(t);
				if(!t)
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to skip past angle bracket chain");
					return bGotVariable;
				}
			}

			t = t->pNext;
		}

		// Notable token reached?

		if(!t)
		{
			CXX_DEBUG_LEAVE_TEXT("Nothing interesting here");
			return bGotVariable;
		}

		CXX_DEBUG_PRINT(
				"Found notable token '%s' of type 0x%02x",
				vStringValue(t->pszWord),
				t->eType
			);

		// Now before the notable token there MUST be an identifier
		// (eventually hidden in a parenthesis chain) and also a typename.
		if(!t->pPrev)
		{
			CXX_DEBUG_LEAVE_TEXT("Nothing interesting before notable token");
			return bGotVariable;
		}

		CXXToken * pIdentifier = NULL;
		CXXToken * pTokenBefore = NULL;

		// If we have to continue scanning we'll remove the tokens from here
		// so they don't end up being part of the type name.
		// If this is set to NULL then it means that we cannot determine properly
		// what to remove and we should stop scanning after the current variable.
		CXXToken * pRemoveStart = t;

		switch(t->eType)
		{
			case CXXTokenTypeParenthesisChain:
			{
				// At a parenthesis chain we need some additional checks.
				if(
						// check for function pointers.
						// Possible cases:
						//    ret type (*variable)(params)
						//    ret type (* const (variable[4]))(params)
						t->pNext &&
						cxxTokenTypeIs(t->pNext,CXXTokenTypeParenthesisChain) &&
						cxxParserTokenChainLooksLikeFunctionParameterList(
								t->pNext->pChain,
								NULL
							) &&
						(pIdentifier = cxxTokenChainFirstPossiblyNestedTokenOfType(
								t->pChain,
								CXXTokenTypeIdentifier
							)) &&
						// Discard function declarations with function return types
						// like void (*A(B))(C);
						(
							(!pIdentifier->pNext) ||
							(!cxxTokenTypeIs(pIdentifier->pNext,CXXTokenTypeParenthesisChain))
						)
					)
				{
					// A function pointer.
					// There are two parentheses, skip the second too.
					pTokenBefore = t->pPrev;
					t = t->pNext->pNext;
					pRemoveStart = t;
				} else if(
						(t->pChain->iCount == 3) &&
						cxxTokenTypeIs(
								cxxTokenChainAt(t->pChain,1),
								CXXTokenTypeParenthesisChain
							) &&
						t->pPrev &&
						cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier) &&
						t->pPrev->pPrev &&
						cxxTokenTypeIs(t->pPrev->pPrev,CXXTokenTypeIdentifier)
					)
				{
					CXX_DEBUG_LEAVE_TEXT("Parenthesis seems to define an __ARGS style prototype");
					return bGotVariable;
				} else if(
						cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier) &&
						(
							(eScopeType == CXXScopeTypeNamespace) ||
							(eScopeType == CXXScopeTypeFunction)
						) &&
						cxxParserCurrentLanguageIsCPP() &&
						cxxParserTokenChainLooksLikeConstructorParameterSet(t->pChain)
					)
				{
					// ok, *might* be variable instantiation
					pIdentifier = t->pPrev;
					pTokenBefore = pIdentifier->pPrev;
				} else {
					CXX_DEBUG_LEAVE_TEXT("No recognizable parenthesis form for a variable");
					return bGotVariable;
				}
			}
			break;
			case CXXTokenTypeBracketChain:
				if(
						cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier) &&
						cxxParserCurrentLanguageIsCPP() &&
						cxxParserTokenChainLooksLikeConstructorParameterSet(t->pChain)
					)
				{
					// ok, *might* be new C++ style variable initialization
					pIdentifier = t->pPrev;
					pTokenBefore = pIdentifier->pPrev;
				} else {
					CXX_DEBUG_LEAVE_TEXT("Bracket chain that doesn't look like a C++ var init");
					return bGotVariable;
				}
			break;
			case CXXTokenTypeSingleColon:
				// check for bitfield
				if(
						t->pNext &&
						cxxTokenTypeIsOneOf(t->pNext,CXXTokenTypeNumber | CXXTokenTypeIdentifier)
					)
				{
					// ok, looks like a bit field
					if(
							cxxTokenTypeIs(t->pNext,CXXTokenTypeNumber) &&
							t->pNext->pNext &&
							cxxTokenTypeIsOneOf(
									t->pNext->pNext,
									CXXTokenTypeComma | CXXTokenTypeSemicolon |
									CXXTokenTypeAssignment
								)
						)
					{
						// keep bitfield width specification as part of type
						pIdentifier = t->pPrev;
						pTokenBefore = pIdentifier->pPrev;
						t = t->pNext->pNext;
					} else {
						// Too complex: strip width specification (the best we can do)
						pIdentifier = t->pPrev;
						pTokenBefore = pIdentifier->pPrev;
					}
				} else {
					CXX_DEBUG_LEAVE_TEXT("Single colon that doesn't look like a bit field");
					return bGotVariable;
				}
			break;
			case CXXTokenTypeSquareParenthesisChain:
				// check for array
				// Keep the array specifier as part of type

				pIdentifier = t->pPrev;
				pTokenBefore = pIdentifier->pPrev;

				while(t->pNext && cxxTokenTypeIs(t->pNext,CXXTokenTypeSquareParenthesisChain))
					t = t->pNext;

				if(
						(!t->pNext) ||
						(!cxxTokenTypeIsOneOf(
								t->pNext,
								CXXTokenTypeComma | CXXTokenTypeSemicolon |
								CXXTokenTypeAssignment
							))
					)
				{
					CXX_DEBUG_LEAVE_TEXT("No comma, semicolon or assignment after array specifier");
					return bGotVariable;
				}

				t = t->pNext;
				pRemoveStart = t;
			break;
			default:
				// Must be identifier
				if(t->pPrev->eType != CXXTokenTypeIdentifier)
				{
					CXX_DEBUG_LEAVE_TEXT("No identifier before the notable token");
					return bGotVariable;
				}

				pIdentifier = t->pPrev;
				pTokenBefore = pIdentifier->pPrev;
			break;
		}

		CXX_DEBUG_ASSERT(pIdentifier,"We should have found an identifier here");

		if(!pTokenBefore)
		{
			CXX_DEBUG_LEAVE_TEXT("Identifier not preceeded by a type");
			return bGotVariable;
		}

		CXXToken * pScopeEnd = pTokenBefore->pNext;
		CXXToken * pScopeStart = NULL;

		// Skip back to the beginning of the scope, if any
		while(pTokenBefore->eType == CXXTokenTypeMultipleColons)
		{
			if(!cxxParserCurrentLanguageIsCPP())
			{
				CXX_DEBUG_LEAVE_TEXT("Syntax error: found multiple colons in C language");
				return FALSE;
			}

			pTokenBefore = pTokenBefore->pPrev;
			if(!pTokenBefore)
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Identifier preceeded by multiple colons " \
							"but not preceeded by a type"
					);
				return bGotVariable;
			}

			if(cxxTokenTypeIs(pTokenBefore,CXXTokenTypeGreaterThanSign))
			{
				CXXToken * pAux = cxxTokenChainSkipBackToStartOfTemplateAngleBracket(pTokenBefore);
				if((!pAux) || (!pAux->pPrev))
				{
					CXX_DEBUG_LEAVE_TEXT(
							"Identifier preceeded by multiple colons " \
								"and by a >, but failed to skip back to starting <"
						);
					return bGotVariable;
				}

				pTokenBefore = pAux->pPrev;
			}

			if(!cxxTokenTypeIs(pTokenBefore,CXXTokenTypeIdentifier))
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Identifier preceeded by multiple colons " \
							"with probable syntax error"
					);
				return bGotVariable;
			}

			pScopeStart = pTokenBefore;

			pTokenBefore = pTokenBefore->pPrev;
			if(!pTokenBefore)
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Identifier preceeded by multiple colons " \
							"but not preceeded by a type"
					);
				return bGotVariable;
			}
		}

		if(!bGotVariable)
		{
			// now pTokenBefore should be part of a type (either the variable type or return
			// type of a function in case of a function pointer)
			if(!cxxTokenTypeIsOneOf(
					pTokenBefore,
					CXXTokenTypeIdentifier | CXXTokenTypeKeyword |
						CXXTokenTypeStar | CXXTokenTypeAnd
				))
			{
				if(cxxTokenTypeIs(pTokenBefore,CXXTokenTypeGreaterThanSign))
				{
					// the < > must be balanced
					CXXToken * t2 = pTokenBefore->pPrev;
					int iLevel = 1;
					while(t2)
					{
						if(cxxTokenTypeIs(t2,CXXTokenTypeGreaterThanSign))
							iLevel++;
						else if(cxxTokenTypeIs(t2,CXXTokenTypeSmallerThanSign))
							iLevel--;
						t2 = t2->pPrev;
					}
					if(iLevel != 0)
					{
						CXX_DEBUG_LEAVE_TEXT(
								"The > token is unbalanced and does not " \
									"seem to be part of type name"
							);
						return bGotVariable;
					}
				} else {
					CXX_DEBUG_LEAVE_TEXT(
							"Token '%s' of type 0x%02x does not seem " \
								"to be part of type name",
							vStringValue(pTokenBefore->pszWord),
							pTokenBefore->eType
						);
					return bGotVariable;
				}
			}

			bGotVariable = TRUE;
		}

		// Goodie. We have an identifier and almost certainly a type here.

		// From now on we start destroying the chain: mark the return value as true
		// so nobody else will try to extract stuff from it

		int iScopesPushed = 0;

		if(pScopeStart)
		{
			// Push the scopes and remove them from the chain so they are not in the way
			while(pScopeStart != pScopeEnd)
			{
				// This is the scope id START. It might contain
				// also other tokens like in ...::A<B>::...

				CXXToken * pPartEnd = cxxTokenChainNextTokenOfType(
						pScopeStart,
						CXXTokenTypeMultipleColons
					);
				CXX_DEBUG_ASSERT(
						pPartEnd,
						"We should have found multiple colons here!"
					);
				CXX_DEBUG_ASSERT(
						pPartEnd->pPrev,
						"And there should be a previous token too"
					);

				CXXToken * pScopeId = cxxTokenChainExtractRange(pScopeStart,pPartEnd->pPrev,0);
				cxxScopePush(
						pScopeId,
						CXXScopeTypeClass,
						// WARNING: We don't know if it's really a class! (FIXME?)
						CXXScopeAccessUnknown
					);

				CXXToken * pAux = pPartEnd->pNext;

				cxxTokenChainDestroyRange(pChain,pScopeStart,pPartEnd);

				pScopeStart = pAux;

				iScopesPushed++;
			}
		}

		boolean bKnRStyleParameters =
				(uFlags & CXXExtractVariableDeclarationsKnRStyleParameters);

		tagEntryInfo * tag = cxxTagBegin(
				bKnRStyleParameters ?
					CXXTagKindPARAMETER :
					((g_cxx.uKeywordState & CXXParserKeywordStateSeenExtern) ?
							CXXTagKindEXTERNVAR : cxxScopeGetVariableKind()),
				pIdentifier
			);

		if(tag)
		{
			CXX_DEBUG_ASSERT(t != pIdentifier,"This should not happen");
			// remove the identifier
			cxxTokenChainTakeRecursive(pChain,pIdentifier);

			// Fix square parentheses: if they contain something that is not a numeric
			// constant then empty them up
			CXXToken * pPartOfType = t->pPrev;
			CXX_DEBUG_ASSERT(pPartOfType,"There should be a part of type name here");

			while(pPartOfType && cxxTokenTypeIs(pPartOfType,CXXTokenTypeSquareParenthesisChain))
			{
				CXXTokenChain * pAuxChain = pPartOfType->pChain;

				if(pAuxChain->iCount > 2)
				{
					if(
						(pAuxChain->iCount > 3) ||
						(!cxxTokenTypeIs(cxxTokenChainAt(pAuxChain,1),CXXTokenTypeNumber))
					)
					{
						cxxTokenChainDestroyRange(
								pAuxChain,
								cxxTokenChainFirst(pAuxChain)->pNext,
								cxxTokenChainLast(pAuxChain)->pPrev
							);
					}
				}
				pPartOfType = pPartOfType->pPrev;
			}

			// anything that remains is part of type
			CXXToken * pTypeToken = cxxTagCheckAndSetTypeField(cxxTokenChainFirst(pChain),t->pPrev);

			tag->isFileScope = bKnRStyleParameters ?
					TRUE :
					(
						(
							(eScopeType == CXXScopeTypeNamespace) &&
							(g_cxx.uKeywordState & CXXParserKeywordStateSeenStatic) &&
							(!isInputHeaderFile())
						) ||
						// locals are always hidden
						(eScopeType == CXXScopeTypeFunction) ||
						(
							(eScopeType != CXXScopeTypeNamespace) &&
							(eScopeType != CXXScopeTypeFunction) &&
							(!isInputHeaderFile())
						)
					);

			vString * pszProperties = NULL;

			if(cxxTagFieldEnabled(CXXTagFieldProperties))
			{
				unsigned int uProperties = 0;

				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenStatic)
					uProperties |= CXXTagPropertyStatic;
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenExtern)
					uProperties |= CXXTagPropertyExtern;
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenMutable)
					uProperties |= CXXTagPropertyMutable;
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenAttributeDeprecated)
					uProperties |= CXXTagPropertyDeprecated;
				// Volatile is part of the type, so we don't mark it as a property
				//if(g_cxx.uKeywordState & CXXParserKeywordStateSeenVolatile)
				//	uProperties |= CXXTagPropertyVolatile;

				pszProperties = cxxTagSetProperties(uProperties);
			}

			cxxTagCommit();

			if(pTypeToken)
				cxxTokenDestroy(pTypeToken);
			if(pszProperties)
				vStringDelete(pszProperties);
			cxxTokenDestroy(pIdentifier);
		}

		while(iScopesPushed > 0)
		{
			cxxScopePop();
			iScopesPushed--;
		}

		if(!t)
		{
			CXX_DEBUG_LEAVE_TEXT("Nothing more");
			return bGotVariable;
		}

		if(!cxxTokenTypeIsOneOf(
				t,
				CXXTokenTypeComma | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket
			))
		{
			t = cxxTokenChainNextTokenOfType(
					t,
					CXXTokenTypeComma | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket
				);
			if(!t)
			{
				CXX_DEBUG_LEAVE_TEXT("Didn't find a comma, semicolon or {");
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

		CXX_DEBUG_ASSERT(t,"There should be something after the comma here!");

		if(!pRemoveStart)
		{
			CXX_DEBUG_LEAVE_TEXT("Could not properly fix type name for next token: stopping here");
			return bGotVariable;
		}

		cxxTokenChainDestroyRange(pChain,pRemoveStart,t->pPrev);
	}

	CXX_DEBUG_LEAVE_TEXT("Reached end");
	return bGotVariable;
}
