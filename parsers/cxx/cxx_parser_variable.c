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


static CXXToken * cxxParserVardefInParenthesis (CXXToken *pToken, int depth);

//
// This is used to find the first identifier in stuff like
//
//    ret type (*variable)(params)
//    ret type (* const (variable[4]))(params)
//    ret type (*baz)(params) <-- function pointer (variable)
//    ret type (*(baz))(params) <-- function pointer (variable)
//    ret type (* const (baz))(params) <-- function pointer (variable)
//    ret type (*baz())() <-- function returning function pointer
//    ret type (*baz(params))(params) <-- function returning function pointer
//    ret type (*baz(params)) <-- function returning a pointer
//    ret type (*baz(params))[2] <-- function returning a pointer to array
//
CXXToken * cxxParserFindFirstPossiblyNestedAndQualifiedIdentifier(
		CXXTokenChain * pChain,
		CXXTokenChain ** pParentChain
	)
{
	CXXToken * pId = cxxTokenChainFirstPossiblyNestedTokenOfType(
			pChain,
			CXXTokenTypeIdentifier,
			pParentChain
		);

	if(!pId)
		return NULL;

	if(!cxxParserCurrentLanguageIsCPP())
		return pId;

	// In the case of CPP we also handle qualifications.

	if(!pId->pNext)
		return pId;

	if(!cxxTokenTypeIs(pId->pNext,CXXTokenTypeMultipleColons))
		return pId;

	// identifier:: ...

	// Look for the LAST identifier in the same chain.
	// baz::foo::something <--

	return cxxTokenChainNextTokenOfType(pId,CXXTokenTypeIdentifier);
}

//
// Attempt to extract variable declarations from the chain.
// Returns true if at least one variable was extracted.
// Returns false if a variable declaration could not be identified.
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
//   type (var[]); <--   type (var); is also valid in C syntax.
//   type *(var);        However, it is handled as macro expansion.
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
// - Be aware that if this function returns true then the pChain very likely has been modified
//   (partially destroyed) as part of the type extraction algorithm.
//   If the function returns false the chain has not been modified (and
//   to extract something else from it).
//
// - This function is quite tricky.
//
bool cxxParserExtractVariableDeclarations(CXXTokenChain * pChain,unsigned int uFlags)
{
	int iCorkIndex = CORK_NIL;
	int iCorkIndexFQ = CORK_NIL;

	CXX_DEBUG_ENTER();

	if(pChain->iCount < 1)
	{
		CXX_DEBUG_LEAVE_TEXT("Chain is empty");
		return false;
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
		return false;
	}

	// Only keywords that can appear in a variable declaration.
	//
	// TODO?: We might add this check also on the other tokens here
	//        However it's unclear if it would provide some advantages
	//        It would certainly be a small overhead.
	if(
			cxxTokenTypeIs(t,CXXTokenTypeKeyword) &&
			(!cxxKeywordMayAppearInVariableDeclaration(t->eKeyword))
		)
	{
		CXX_DEBUG_LEAVE_TEXT("Initial keyword can't appear in a variable declaration");
		return false;
	}

	bool bGotVariable = false;

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
				"Found notable token '%s' of type 0x%02x(%s)",
				vStringValue(t->pszWord),
				t->eType,
				cxxDebugTypeDecode(t->eType)
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

				// At a parenthesis chain we need some additional checks.
				if(
						// check for function pointers or nasty arrays
						// Possible cases:
						//    ret type (*variable)(params)
						//    ret type (* const (variable[4]))(params)
						t->pNext &&
						(
							(
								cxxTokenTypeIs(t->pNext,CXXTokenTypeParenthesisChain) &&
								cxxParserTokenChainLooksLikeFunctionParameterList(
										t->pNext->pChain,
										NULL
									)
							) ||
							cxxTokenTypeIs(t->pNext,CXXTokenTypeSquareParenthesisChain)
						) &&
						(pIdentifier = cxxParserFindFirstPossiblyNestedAndQualifiedIdentifier(
								t->pChain,
								NULL
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
					goto got_identifier;
				}

				if(
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
				}

				if(
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
					goto got_identifier;
				}

				if (
						// Variable declaration in parenthesis.
						// We handle only cases that are clearly variable declarations.
						// We leave alone the stuff that may also be a function call.
						//   type *(var)
						//   type &(var)
						//   type const (var)
						//   type (var[])
						(
							cxxTokenTypeIsOneOf(t->pPrev,CXXTokenTypeStar | CXXTokenTypeAnd) ||
							(
								cxxTokenTypeIs(t->pPrev,CXXTokenTypeKeyword) &&
								cxxKeywordMayBePartOfTypeName(t->pPrev->eKeyword) &&
								// but not decltype(var)!
								(t->pPrev->eKeyword != CXXKeywordDECLTYPE)
							)
						) &&
						(
							// Inspect the inside parenthesis
							pIdentifier = cxxParserVardefInParenthesis(cxxTokenChainFirst(t->pChain), 0)
						)
					)
				{
					CXX_DEBUG_LEAVE_TEXT("Parenthesis seems to surround a variable definition");
					pTokenBefore = t->pPrev;
					t = t->pNext;
					goto got_identifier;
				}

				if(
					cxxTokenIsKeyword(t->pPrev,CXXKeywordDECLTYPE) &&
					t->pNext
				)
				{
					// part of typename -> skip ahead
					CXX_DEBUG_LEAVE_TEXT("Parenthesis follows decltype(), skipping");
					t = t->pNext;
					continue;
				}

				CXX_DEBUG_LEAVE_TEXT("No recognizable parenthesis form for a variable");
				return bGotVariable;
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
					goto got_identifier;
				}

				CXX_DEBUG_LEAVE_TEXT("Bracket chain that doesn't look like a C++ var init");
				return bGotVariable;
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

					goto got_identifier;

				}

				CXX_DEBUG_LEAVE_TEXT("Single colon that doesn't look like a bit field");
				return bGotVariable;
			break;
			case CXXTokenTypeSquareParenthesisChain:
				// check for array
				// Keep the array specifier as part of type

				pIdentifier = t->pPrev;
				pTokenBefore = pIdentifier->pPrev;

				while(t->pNext && cxxTokenTypeIs(t->pNext,CXXTokenTypeSquareParenthesisChain))
					t = t->pNext;

				if(!t->pNext)
				{
					CXX_DEBUG_LEAVE_TEXT("No token after []");
					return bGotVariable;
				}

				// skip identifies attached to the array as attributes
				while(t->pNext && cxxTokenTypeIs(t->pNext, CXXTokenTypeIdentifier))
				{
					t = t->pNext;
					// skip macro argument(s)
					if (t->pNext && cxxTokenTypeIs(t->pNext, CXXTokenTypeParenthesisChain))
						t = t->pNext;
				}

				if (!t->pNext)
				{
					CXX_DEBUG_LEAVE_TEXT("No token after attribute(s) attached to []");
					return bGotVariable;
				}

				if(!cxxTokenTypeIsOneOf(
							t->pNext,
							CXXTokenTypeComma | CXXTokenTypeSemicolon |
							CXXTokenTypeAssignment | CXXTokenTypeBracketChain
							))
				{
					CXX_DEBUG_LEAVE_TEXT("No comma, semicolon, = or {} after [] (\"%s\", %s)",
								vStringValue (t->pNext->pszWord),
								cxxDebugTypeDecode (t->pNext->eType));
					return bGotVariable;
				}

				t = t->pNext;
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

got_identifier:
		CXX_DEBUG_ASSERT(pIdentifier,"We should have found an identifier here");

		if(!pTokenBefore)
		{
			CXX_DEBUG_LEAVE_TEXT("Identifier not preceded by a type");
			// Here we can handle yet another one of the gazillion of special cases.
			//
			//    MACRO(whatever) variable;
			//
			if(
					cxxTokenTypeIs(t,CXXTokenTypeParenthesisChain) &&
					t->pNext &&
					cxxTokenTypeIs(t->pNext,CXXTokenTypeIdentifier) &&
					t->pNext->pNext &&
					cxxTokenTypeIs(t->pNext->pNext,CXXTokenTypeSemicolon)
				)
			{
				CXX_DEBUG_PRINT("Looks like the 'MACRO(whatever) variable;' special case");
				pIdentifier = t->pNext;
				pTokenBefore = t;
				t = t->pNext->pNext;
			} else {
				return bGotVariable;
			}
		}

		CXXToken * pScopeEnd = pTokenBefore->pNext;
		CXXToken * pScopeStart = NULL;

		// Skip back to the beginning of the scope, if any
		while(pTokenBefore->eType == CXXTokenTypeMultipleColons)
		{
			if(!cxxParserCurrentLanguageIsCPP())
			{
				CXX_DEBUG_LEAVE_TEXT("Syntax error: found multiple colons in C language");
				return false;
			}

			pTokenBefore = pTokenBefore->pPrev;
			if(!pTokenBefore)
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Identifier preceded by multiple colons " \
							"but not preceded by a type"
					);
				return bGotVariable;
			}

			if(cxxTokenTypeIs(pTokenBefore,CXXTokenTypeGreaterThanSign))
			{
				CXXToken * pAux = cxxTokenChainSkipBackToStartOfTemplateAngleBracket(pTokenBefore);
				if((!pAux) || (!pAux->pPrev))
				{
					CXX_DEBUG_LEAVE_TEXT(
							"Identifier preceded by multiple colons " \
								"and by a >, but failed to skip back to starting <"
						);
					return bGotVariable;
				}

				pTokenBefore = pAux->pPrev;
			}

			if(!cxxTokenTypeIs(pTokenBefore,CXXTokenTypeIdentifier))
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Identifier preceded by multiple colons " \
							"with probable syntax error"
					);
				return bGotVariable;
			}

			pScopeStart = pTokenBefore;

			pTokenBefore = pTokenBefore->pPrev;
			if(!pTokenBefore)
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Identifier preceded by multiple colons " \
							"but not preceded by a type"
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
				} else if(
						// Possibly one of:
						//   MACRO(whatever) variable;
						//   decltype(whatever) variable;
						cxxTokenTypeIs(pTokenBefore,CXXTokenTypeParenthesisChain) &&
						pTokenBefore->pPrev &&
						!pTokenBefore->pPrev->pPrev &&
						(
							// macro
							cxxTokenTypeIs(pTokenBefore->pPrev,CXXTokenTypeIdentifier) ||
							// decltype
							cxxTokenIsKeyword(pTokenBefore->pPrev,CXXKeywordDECLTYPE)
						)
					)
				{
					CXX_DEBUG_PRINT("Type seems to be hidden in a macro or defined by decltype");
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

			bGotVariable = true;
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

		CXX_DEBUG_ASSERT(t != pIdentifier,"This should not happen");

		// remove the identifier
		cxxTokenChainTakeRecursive(pChain,pIdentifier);

		bool bGotTemplate = g_cxx.pTemplateTokenChain &&
				(g_cxx.pTemplateTokenChain->iCount > 0) &&
				cxxParserCurrentLanguageIsCPP();

		bool bKnRStyleParameters =
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
			// Fix square parentheses: if they contain something that is not a numeric
			// constant then empty them up
			CXXToken * pPartOfType = t->pPrev; // identifier has been removed
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
					true :
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
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenInline)
					uProperties |= CXXTagPropertyInline;
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenAttributeDeprecated)
					uProperties |= CXXTagPropertyDeprecated;
				// Volatile is part of the type, so we don't mark it as a property
				//if(g_cxx.uKeywordState & CXXParserKeywordStateSeenVolatile)
				//	uProperties |= CXXTagPropertyVolatile;

				pszProperties = cxxTagSetProperties(uProperties);
			}

			if(bGotTemplate)
				cxxTagHandleTemplateFields();

			iCorkIndex = cxxTagCommit(&iCorkIndexFQ);

			if(pTypeToken)
				cxxTokenDestroy(pTypeToken);
			if(pszProperties)
				vStringDelete(pszProperties);
		}

		if(
				bGotTemplate &&
				cxxTagKindEnabled(CXXTagCPPKindTEMPLATEPARAM)
			)
		{
			cxxScopePush(pIdentifier,CXXScopeTypeVariable,CXXScopeAccessPublic);
			cxxParserEmitTemplateParameterTags();
			cxxScopePop();
		} else {
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
			// look for it, but also check for "<" signs: these usually indicate an uncondensed
			// template. We give up on them as they are too complicated in this context.
			// It's rather unlikely to have multiple declarations with templates after the first one
			t = cxxTokenChainNextTokenOfType(
					t,
					CXXTokenTypeComma | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket |
					CXXTokenTypeSmallerThanSign
				);
			if(!t)
			{
				CXX_DEBUG_LEAVE_TEXT("Didn't find a comma, semicolon or {");
				return bGotVariable;
			}
			if(cxxTokenTypeIs(t,CXXTokenTypeSmallerThanSign))
			{
				CXX_DEBUG_LEAVE_TEXT("Found '<': probably a template on the right side of declaration");
				return bGotVariable;
			}
		}

		if(cxxTokenTypeIsOneOf(t,CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket))
		{
			if (iCorkIndex != CORK_NIL)
			{
				cxxParserSetEndLineForTagInCorkQueue (iCorkIndex, t->iLineNumber);
				iCorkIndex = CORK_NIL;
				if(iCorkIndexFQ != CORK_NIL)
				{
					cxxParserSetEndLineForTagInCorkQueue (iCorkIndexFQ, t->iLineNumber);
					iCorkIndexFQ = CORK_NIL;
				}
			}
			CXX_DEBUG_LEAVE_TEXT("Noting else");
			return bGotVariable;
		}

		// Comma. Might have other declarations here.
		if (iCorkIndex != CORK_NIL)
		{
			cxxParserSetEndLineForTagInCorkQueue (iCorkIndex, t->iLineNumber);
			iCorkIndex = CORK_NIL;
			if(iCorkIndexFQ != CORK_NIL)
			{
				cxxParserSetEndLineForTagInCorkQueue (iCorkIndexFQ, t->iLineNumber);
				iCorkIndexFQ = CORK_NIL;
			}
		}

		CXX_DEBUG_PRINT("At a comma, might have other declarations here");

		t = t->pNext;

		CXX_DEBUG_ASSERT(t,"There should be something after the comma here!");

		if(!pRemoveStart)
		{
			CXX_DEBUG_LEAVE_TEXT("Could not properly fix type name for next token: stopping here");
			return bGotVariable;
		}

		// *, &, && and similar stuff do not "propagate" to the next type
		while(
				pRemoveStart->pPrev &&
				cxxTokenTypeIsOneOf(
						pRemoveStart->pPrev,
						CXXTokenTypeStar | CXXTokenTypeAnd |
						CXXTokenTypeMultipleAnds | CXXTokenTypeSquareParenthesisChain
					)
			)
			pRemoveStart = pRemoveStart->pPrev;

		cxxTokenChainDestroyRange(pChain,pRemoveStart,t->pPrev);
	}

	CXX_DEBUG_LEAVE_TEXT("Reached end");
	return bGotVariable;
}

static bool isConstVolatileOrStar (CXXToken *t, void *data)
{
	if (cxxTokenTypeIs (t, CXXTokenTypeStar))
		return true;

	if (cxxTokenTypeIs (t, CXXTokenTypeKeyword) &&
		((t->eKeyword == CXXKeywordCONST) ||
		 (t->eKeyword == CXXKeywordVOLATILE)))
		return true;

	return false;
}

static CXXToken * cxxParserVardefInParenthesis (CXXToken *pToken, int depth)
{
	// ( const volatile * foo [] ) <- ended with CXXTokenTypeSquareParenthesisChain
	// ( const volatile * foo ) <- ended with CXXTokenTypeClosingParenthesis
	// ((const volatile * foo)) <- depth > 0

	CXXToken *t;

	t = cxxTokenChainNextTokenNotOfGeneric (pToken, isConstVolatileOrStar, NULL);
	if (!t)
		return NULL;

	if (cxxTokenTypeIs(t, CXXTokenTypeParenthesisChain))
		return cxxParserVardefInParenthesis (cxxTokenChainFirst(t->pChain),
										   depth + 1);
	else if (cxxTokenTypeIs (t, CXXTokenTypeIdentifier) &&
			 ((t->pNext &&
			   (cxxTokenTypeIsOneOf (t->pNext,
									 CXXTokenTypeSquareParenthesisChain | CXXTokenTypeClosingParenthesis)))||
			  (t->pNext && depth > 0)))
		return t;
	return NULL;
}
