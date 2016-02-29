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
// This is called upon encountering a semicolon, when current language is C and we are in global scope.
//
// Try to handle the special case of C K&R style function declarations.
// Returns -1 in case of error, 1 if a K&R style function declaration has been
// found and parsed, 0 if no K&R style function declaration has been found.
// 
int cxxParserMaybeExtractKnRStyleFunctionDefinition()
{
#ifdef CXX_DO_DEBUGGING
	vString * pChain = cxxTokenChainJoin(g_cxx.pTokenChain,NULL,0);
	CXX_DEBUG_PRINT("Looking for K&R-style function in '%s'",vStringValue(pChain));
	vStringDelete(pChain);
#endif

	// Check if we are in the following situation:
	//
	//   type1 function(arg1,arg2,...) type2 arg1; type3 arg2; {
	//                                           ^
	//                                       we're here
	
	CXX_DEBUG_ASSERT(g_cxx.eLanguage == g_cxx.eCLanguage,"Should be called only when parsing C");
	CXX_DEBUG_ASSERT(cxxTokenChainLast(g_cxx.pTokenChain),"At least one token should be there");
	CXX_DEBUG_ASSERT(cxxTokenChainLast(g_cxx.pTokenChain)->eType == CXXTokenTypeSemicolon,"Only upon encountering a semicolon");

	// The minimum possible case is:
	//
	//   func(arg) type2 arg;
	//
	// where (arg) is a condensed parenthesis chain.
	// So the minimum number of tokens required is 5: func, (arg), type2, arg, ;
	if(g_cxx.pTokenChain->iCount < 5)
		return 0; // no way

	// There must be a parenthesis chain
	CXXToken * pParenthesis = cxxTokenChainFirstTokenOfType(g_cxx.pTokenChain,CXXTokenTypeParenthesisChain);
	if(!pParenthesis)
		return 0; // no parenthesis chain

	// The parenthesis chain must have an identifier before it
	CXXToken * pIdentifier = pParenthesis->pPrev;
	if(!pIdentifier)
		return 0;
	if(pIdentifier->eType != CXXTokenTypeIdentifier)
		return 0;

	// And least three tokens after it
	CXXToken * x = pParenthesis->pNext;
	if(!x)
		return 0;
	x = x->pNext;
	if(!x)
		return 0;
	x = x->pNext;
	if(!x)
		return 0;

	// The tokens following must be only things allowed in a variable declaration
	x = cxxTokenChainNextTokenNotOfType(
			pParenthesis,
			CXXTokenTypeIdentifier | CXXTokenTypeKeyword | CXXTokenTypeSquareParenthesisChain |
			CXXTokenTypeStar | CXXTokenTypeComma | CXXTokenTypeSingleColon | CXXTokenTypeNumber
		);

	CXX_DEBUG_ASSERT(x,"There should be at least the terminator here!");
	if(!x)
		return 0;
	
	if(x->eType != CXXTokenTypeSemicolon)
		return 0; // does not look like a variable declaration.

	x = cxxTokenChainPreviousTokenNotOfType(
			x,
			CXXTokenTypeSquareParenthesisChain | CXXTokenTypeSingleColon | CXXTokenTypeNumber
		);
	
	CXX_DEBUG_ASSERT(x,"We should have found an identifier here");
	if(!x)
		return 0;
	
	if(x->eType != CXXTokenTypeIdentifier)
		return 0; // does not look like a variable declaration.

	CXX_DEBUG_ASSERT(pParenthesis->pChain,"The parenthesis should be condensed here!");

	CXXTokenChain * pParenthesisTokenChain = g_cxx.pTokenChain;

	// Special case inside special case. Check if we're at something like func __ARGS(())
	if(
			(pParenthesis->pChain->iCount == 3) &&
			(cxxTokenChainAt(pParenthesis->pChain,1)->eType == CXXTokenTypeParenthesisChain) &&
			(pIdentifier->pPrev) &&
			(pIdentifier->pPrev->eType == CXXTokenTypeIdentifier)
		)
	{
		// Looks exactly like our special case.
		pIdentifier = pIdentifier->pPrev;
		pParenthesis = cxxTokenChainAt(pParenthesis->pChain,1);
		pParenthesisTokenChain = pParenthesis->pChain;
	}

	// Now check if the contents of the parenthesis chain look like a K&R signature
	
	// This is something like identifier,identifier,identifier,...
	if(pParenthesis->pChain->iCount < 3)
		return 0; // no way

	x = pParenthesis->pChain->pHead->pNext;
	CXX_DEBUG_ASSERT(x,"We should have found something inside the parenthesis chain");

	int iParameterCount = 0;
	boolean bGotMultipleDots = FALSE;

	for(;;)
	{
		if(x->eType == CXXTokenTypeIdentifier)
			iParameterCount++;
		else if(x->eType == CXXTokenTypeMultipleDots)
			bGotMultipleDots = TRUE;
		else
			return 0; // not valid (note that (void) is not allowed here since we wouln't have a following variable declaration)
			
		x = x->pNext;
		CXX_DEBUG_ASSERT(x,"We should have found at least the closing parenthesis here");
		if(x->eType == CXXTokenTypeClosingParenthesis)
			break;
		if(bGotMultipleDots)
			return 0; // not valid
		if(x->eType != CXXTokenTypeComma)
			return 0;
		x = x->pNext;
		CXX_DEBUG_ASSERT(x,"We should have found at least the closing parenthesis here");
	}

	if(iParameterCount < 1)
		return 0; // we should have found at least one paremeter (the one that we found before the ;)

	cxxTokenChainTake(g_cxx.pTokenChain,pIdentifier);
	cxxTokenChainTake(pParenthesisTokenChain,pParenthesis);

	// FIXME: Should keep the statement to extract parameters out of it!
	cxxParserNewStatement();

	// Now we should have no more than iParameterCount-1 parameters before an opening bracket.
	// There may be less declarations as each one may declare multiple variables and C89 supports
	// the implicit "int" type rule.
	// Note that we parse up to iParameterCount statements (which will be lost if we can't find an opening bracket).
	while(iParameterCount > 0)
	{
		if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket | CXXTokenTypeEOF))
		{
			cxxTokenDestroy(pIdentifier);
			cxxTokenDestroy(pParenthesis);
			return FALSE;
		}

		if(g_cxx.pToken->eType == CXXTokenTypeEOF)
			return TRUE; // tolerate syntax error
		if(g_cxx.pToken->eType == CXXTokenTypeOpeningBracket)
			break; // gotcha
		iParameterCount--;
		
		// FIXME: Should keep the statement to extract parameters out of it!
		cxxParserNewStatement();
	}
	
	if(g_cxx.pToken->eType != CXXTokenTypeOpeningBracket)
		return 0; // didn't find an opening bracket. This probably wasn't a K&R style function declaration after all.

	tagEntryInfo * tag = cxxTagBegin(
			vStringValue(pIdentifier->pszWord),
			CXXTagKindFUNCTION,
			pIdentifier
		);

	if(tag)
	{
	
		if(pParenthesis->pChain->pTail)
			pParenthesis->pChain->pTail->bFollowedBySpace = false; // make sure we don't emit the trailing space
	
		tag->isFileScope = (g_cxx.uKeywordState & CXXParserKeywordStateSeenStatic) && !isInputHeaderFile();
	
		vString * pszSignature = cxxTokenChainJoin(pParenthesis->pChain,NULL,0);
	
		if(pszSignature)
			tag->extensionFields.signature = vStringValue(pszSignature);
		cxxTagCommit();

		if(pszSignature)
			vStringDelete(pszSignature);
	}

	cxxTokenDestroy(pParenthesis);
	
	CXX_DEBUG_PRINT("Found K&R-style function '%s'",vStringValue(pIdentifier->pszWord));

	cxxScopePush(pIdentifier,CXXTagKindFUNCTION,CXXScopeAccessUnknown);

	return 1;
}

// FIXME: Extract return type?

//
// Look for a function signature in the specified chain.
//
// If a proper function signature is found then also standardize the spacing of the identifier
// so we always get it as "operator ()" and never as "operator() or operator ( ) ".
// 
// Note that this function does NOT handle the special case of K&R-style declarations.
//
// If pParamInfo is not null, it is passed to cxxParserTokenChainLooksLikeFunctionParameterList()
// which will eventually fill it up.
//
boolean cxxParserLookForFunctionSignature(CXXTokenChain * pChain,CXXFunctionSignatureInfo * pInfo,CXXFunctionParameterInfo * pParamInfo)
{
	CXX_DEBUG_ENTER();

	if(pChain->iCount < 1)
	{
		CXX_DEBUG_LEAVE_TEXT("Chain is empty");
		return FALSE;
	}

#ifdef CXX_DO_DEBUGGING
	vString * pJoinedChain = cxxTokenChainJoin(pChain,NULL,0);
	CXX_DEBUG_PRINT("Looking for function signature in '%s'",vStringValue(pJoinedChain));
	vStringDelete(pJoinedChain);
#endif

	if(pParamInfo)
		pParamInfo->uParameterCount = 0;

	CXX_DEBUG_ASSERT(pChain,"Null chain");

	CXXToken * pToken = cxxTokenChainFirst(pChain);
	
	pInfo->pParenthesis = NULL;

	CXXToken * pIdentifierStart = NULL;
	CXXToken * pIdentifierEnd = NULL;
	CXXToken * pTopLevelParenthesis = NULL;

	boolean bStopScanning = FALSE;

	while(pToken)
	{
		if((pToken->eType == CXXTokenTypeKeyword) && (pToken->eKeyword == CXXKeywordOPERATOR))
		{
			// Special case for operator <something> ()

			pIdentifierStart = pToken;
			pToken = pToken->pNext;

			while(pToken)
			{
				if(pToken->eType == CXXTokenTypeParenthesisChain)
				{
					// check for operator ()()
					if(pToken->pNext && (pToken->pNext->eType == CXXTokenTypeParenthesisChain))
						pToken = pToken->pNext;
					break;
				}
				
				if(!(pToken->eType &
						(CXXTokenTypeAnd | CXXTokenTypeAssignment | CXXTokenTypeComma |
						CXXTokenTypeDotOperator | CXXTokenTypeGreaterThanSign | CXXTokenTypeOperator |
						CXXTokenTypePointerOperator | CXXTokenTypeSingleColon | CXXTokenTypeSmallerThanSign |
						CXXTokenTypeSquareParenthesisChain | CXXTokenTypeStar)
					)
				)
				{
					CXX_DEBUG_LEAVE_TEXT("Unexpected token after the operator keyword");
					return FALSE;
				}
				
				pToken = pToken->pNext;
			}
			
			if(!pToken)
			{
				CXX_DEBUG_LEAVE_TEXT("Didn't find a parenthesis subchain after operator keyword");
				return FALSE;
			}
			
			CXX_DEBUG_ASSERT(pToken->eType == CXXTokenTypeParenthesisChain,"Must have found a parenthesis chain here");

			pIdentifierEnd = pToken->pPrev;
			
			bStopScanning = TRUE;

		} else if(pToken->eType == CXXTokenTypeSmallerThanSign)
		{
			pToken = cxxTokenChainSkipToEndOfAngleBracket(pToken);
			if(!pToken)
			{
				CXX_DEBUG_LEAVE_TEXT("Couln't skip past angle bracket chain");
				return FALSE;
			}
			goto next_token;

		} else if(pToken->eType & (CXXTokenTypeOpeningBracket | CXXTokenTypeSemicolon | CXXTokenTypeEOF))
		{
			// reached end
			bStopScanning = TRUE;
			break;
		} else if(
					cxxParserCurrentLanguageIsCPP() &&
					(pToken->eType & (CXXTokenTypeSingleColon | CXXTokenTypeAssignment | CXXTokenTypePointerOperator))
			)
		{
			// With a single colon it might be a constructor.
			// With assignment it might be virtual type func(..) = 0;
			// With a pointer operator it might be trailing return type
			bStopScanning = TRUE;
			break;
		} else if(pToken->eType & (
					CXXTokenTypeOperator | CXXTokenTypeSingleColon | CXXTokenTypeAssignment |
					CXXTokenTypePointerOperator | CXXTokenTypeBracketChain |
					CXXTokenTypeStringConstant | CXXTokenTypeAngleBracketChain |
					CXXTokenTypeAssignment | CXXTokenTypeCharacterConstant | CXXTokenTypeMultipleDots |
					CXXTokenTypeClosingBracket | CXXTokenTypeClosingParenthesis |
					CXXTokenTypeClosingSquareParenthesis
				))
		{
			// Nope.
			CXX_DEBUG_LEAVE_TEXT("Found tokens that should not appear in a function signature");
			return FALSE;
		} else {

			// must be parenthesis chain
			if(pToken->eType != CXXTokenTypeParenthesisChain)
				goto next_token; 
	
			// parentheses at position 0 are meaningless
			if(!pToken->pPrev)
				goto next_token;
	
			// parentheses at position 1 they are very likely to be macro invocations... but we still handle them in case we find nothing else
	
			// must have an identifier before (this excludes things like __attribute__ and declspec which are marked as keywords)
			if(pToken->pPrev->eType == CXXTokenTypeIdentifier)
			{
				// identifier before
				pIdentifierStart = pToken->pPrev;
				pIdentifierEnd = pToken->pPrev;
			} else {
				// no way
				goto next_token;
			}
		}

		// looks almost fine

		CXXToken * pInner = cxxTokenChainAt(pToken->pChain,1);

		// Look for the __ARGS(()) macro pattern.
		if(
				// nested parentheses
				(pToken->pChain->iCount == 3) &&
				(pInner->eType == CXXTokenTypeParenthesisChain) &&
				(pIdentifierEnd->eType == CXXTokenTypeIdentifier) && // FIXME: This actually excludes operator!
				// an identifier right before the identifier we found
				pIdentifierEnd->pPrev &&
				(pIdentifierEnd->pPrev->eType == CXXTokenTypeIdentifier) &&
				cxxParserTokenChainLooksLikeFunctionParameterList(pInner->pChain,pParamInfo)
			)
		{
			// __ARGS() case
			pTopLevelParenthesis = pToken;
			pInfo->pIdentifierEnd = pIdentifierEnd->pPrev;
			pInfo->pIdentifierStart = pInfo->pIdentifierEnd;
			pInfo->pParenthesis = pInner;
		} else if(cxxParserTokenChainLooksLikeFunctionParameterList(pToken->pChain,pParamInfo))
		{
			// non __ARGS()
			pTopLevelParenthesis = pToken;
			pInfo->pIdentifierStart = pIdentifierStart;
			pInfo->pIdentifierEnd = pIdentifierEnd;
			pInfo->pParenthesis = pToken;
		}

		if(bStopScanning)
			break; // no more possibilities

next_token:
		pToken = pToken->pNext;
	}

	if(!pInfo->pParenthesis)
	{
		CXX_DEBUG_LEAVE_TEXT("No suitable parenthesis chain found");
		return FALSE; // no function, no party
	}

	if(pInfo->pIdentifierStart != pInfo->pIdentifierEnd)
	{
		// operator case
		pInfo->pIdentifierStart->bFollowedBySpace = TRUE; // force proper spacing
		CXXToken * t = pInfo->pIdentifierStart->pNext;
		while(t != pInfo->pIdentifierEnd);
		{
			t->bFollowedBySpace = FALSE;
			t = t->pNext;
		} 
	} else {
		// non operator
		pInfo->pIdentifierStart->bFollowedBySpace = FALSE; // force proper spacing
	}

	pInfo->pIdentifierEnd->bFollowedBySpace = FALSE; // force proper spacing

	pInfo->pScopeStart = NULL;

	if(cxxParserCurrentLanguageIsCPP())
	{
		// Look for scope prefix
		pToken = pInfo->pIdentifierStart->pPrev;
	
		while(pToken)
		{
			if(pToken->eType != CXXTokenTypeMultipleColons)
				break;
			pToken = pToken->pPrev;
			if(!pToken)
				break;
			if(pToken->eType != CXXTokenTypeIdentifier)
			{
				// check for template specialization
				if(pToken->eType == CXXTokenTypeGreaterThanSign)
				{
					// might be something like type X<TemplateArg>::func() (explicit specialization of template<A> class X).
					CXXToken * pSmallerThan = cxxTokenChainPreviousTokenOfType(pToken,CXXTokenTypeSmallerThanSign);
					if(!pSmallerThan)
						break; // nope
					if(!pSmallerThan->pPrev)
						break; // nope
					if(pSmallerThan->pPrev->eType != CXXTokenTypeIdentifier)
						break; // nope
					// hmm.. probably a template specialisation
					pToken = pSmallerThan->pPrev;
				} else if(pToken->eType == CXXTokenTypeAngleBracketChain)
				{
					// same as above, but already condensed
					if(!pToken->pPrev)
						break; // nope
					if(pToken->pPrev->eType != CXXTokenTypeIdentifier)
						break; // nope
					// hmm.. probably a template specialisation
					pToken = pToken->pPrev;
				} else {
					// no more scope names
					break;
				}
			}
	
			pInfo->pScopeStart = pToken;
	
			pToken = pToken->pPrev;
		}
		
		// Look for trailing const.
		
		if(
				pTopLevelParenthesis->pNext &&
				(pTopLevelParenthesis->pNext->eType == CXXTokenTypeKeyword) &&
				(pTopLevelParenthesis->pNext->eKeyword == CXXKeywordCONST)
		)
			pInfo->pSignatureConst = pTopLevelParenthesis->pNext;
		else
			pInfo->pSignatureConst = NULL;
	} else {
		pInfo->pSignatureConst = NULL;
	}
	
	CXX_DEBUG_LEAVE_TEXT("Found function signature");
	return TRUE;
}


//
// Emit a function tag.
//
// Returns the number of scopes pushed if CXXEmitFunctionTagsPushScopes is present in uOptions and 0 otherwise.
//
int cxxParserEmitFunctionTags(
		CXXFunctionSignatureInfo * pInfo,
		enum CXXTagKind eTagKind,
		unsigned int uOptions
	)
{
	CXX_DEBUG_ENTER();

	int iScopesPushed = 0;

	enum CXXTagKind eOuterScopeKind = cxxScopeGetKind();

	boolean bPushScopes = uOptions & CXXEmitFunctionTagsPushScopes;

	if(bPushScopes && pInfo->pScopeStart)
	{
		// there is a scope
		while(pInfo->pScopeStart != pInfo->pIdentifierStart)
		{
			CXXToken * pScopeId = pInfo->pScopeStart;
			pInfo->pScopeStart = cxxTokenChainNextTokenOfType(pInfo->pScopeStart,CXXTokenTypeMultipleColons);
			CXX_DEBUG_ASSERT(pInfo->pScopeStart,"We should have found multiple colons here!");
			pInfo->pScopeStart = pInfo->pScopeStart->pNext;

			cxxTokenChainTake(g_cxx.pTokenChain,pScopeId);

			cxxScopePush(
					pScopeId,
					CXXTagKindCLASS,
					CXXScopeAccessUnknown // WARNING: We don't know if it's really a class! (FIXME?)
				);
			iScopesPushed++;
		}
	}

	CXXToken * pIdentifier = cxxTokenChainExtractRange(
			pInfo->pIdentifierStart,
			pInfo->pIdentifierEnd,
			0 // proper spacing has been already ensured by cxxParserLookForFunctionSignature()
		);

	CXX_DEBUG_ASSERT(pIdentifier,"The identifier should have been non null since the indices point inside this chain");
	pIdentifier->eType = CXXTokenTypeIdentifier; // force it
	
	CXX_DEBUG_PRINT("Identifier is '%s'",vStringValue(pIdentifier->pszWord));
	
	tagEntryInfo * tag = cxxTagBegin(
			vStringValue(pIdentifier->pszWord),
			eTagKind,
			pIdentifier
		);

	if(tag)
	{
		if(pInfo->pParenthesis->pChain->pTail)
			pInfo->pParenthesis->pChain->pTail->bFollowedBySpace = false; // make sure we don't emit the trailing space

		if(eOuterScopeKind == CXXTagKindNAMESPACE)
		{
			// in a namespace only static stuff declared in cpp files is file scoped
			tag->isFileScope = (g_cxx.uKeywordState & CXXParserKeywordStateSeenStatic) && !isInputHeaderFile();
		} else {
			// in a class/struct/union file scope stuff is only in cpp files
			tag->isFileScope = !isInputHeaderFile();
		}

		vString * pszSignature = cxxTokenChainJoin(pInfo->pParenthesis->pChain,NULL,0);
		if(pInfo->pSignatureConst)
		{
			vStringCatS(pszSignature," ");
			cxxTokenAppendToString(pszSignature,pInfo->pSignatureConst);
		}
		// FIXME: Here we also know the return type (or whatever it is coming before the function name/scope)
	
		if(pszSignature)
			tag->extensionFields.signature = vStringValue(pszSignature);

		cxxTagCommit();

		if(pszSignature)
			vStringDelete(pszSignature);
	}

#ifdef CXX_DO_DEBUGGING
	if(eTagKind == CXXTagKindFUNCTION)
		CXX_DEBUG_PRINT("Emitted function '%s'",vStringValue(pIdentifier->pszWord));
	else
		CXX_DEBUG_PRINT("Emitted prototype '%s'",vStringValue(pIdentifier->pszWord));
#endif

	if(bPushScopes)
	{
		cxxScopePush(pIdentifier,CXXTagKindFUNCTION,CXXScopeAccessUnknown);
		iScopesPushed++;
	} else {
		cxxTokenDestroy(pIdentifier);
	}
	
	CXX_DEBUG_LEAVE();
	return iScopesPushed;
}

//
// This is called at block level upon encountering an opening bracket, when we are not in a function.
// The current block chain almost certainly contains a function signature.
//
// This function attempts to extract the function name, emit it as a tag and push all the necessary
// scopes for the next block. It returns the number of scopes pushed.
//
int cxxParserExtractFunctionSignatureBeforeOpeningBracket()
{
	CXX_DEBUG_ENTER();
	
#ifdef CXX_DO_DEBUGGING
	vString * pChain = cxxTokenChainJoin(g_cxx.pTokenChain,NULL,0);
	CXX_DEBUG_PRINT("Looking for function in '%s'",vStringValue(pChain));
	vStringDelete(pChain);
#endif

	// Note that the token chain ALWAYS contains the final delimiter here.
	
	CXX_DEBUG_ASSERT(g_cxx.pTokenChain->iCount > 0,"There should be at least the terminator here!");
	CXX_DEBUG_ASSERT(cxxTokenChainLast(g_cxx.pTokenChain)->eType == CXXTokenTypeOpeningBracket,"We should have been called when pointing on an opening bracket!");

	cxxTokenChainDestroyLast(g_cxx.pTokenChain);

	CXXFunctionSignatureInfo oInfo;

	CXXFunctionParameterInfo oParamInfo;

	if(!cxxParserLookForFunctionSignature(g_cxx.pTokenChain,&oInfo,&oParamInfo))
	{
		CXX_DEBUG_LEAVE_TEXT("No parenthesis found: no function");
		return 0;
	}

	int iScopesPushed = cxxParserEmitFunctionTags(&oInfo,CXXTagKindFUNCTION,CXXEmitFunctionTagsPushScopes);

	if(cxxTagKindEnabled(CXXTagKindPARAMETER))
		cxxParserEmitFunctionParameterTags(&oParamInfo);

	CXX_DEBUG_LEAVE();
	return iScopesPushed;
}

void cxxParserEmitFunctionParameterTags(CXXFunctionParameterInfo * pInfo)
{
	// emit parameters
	unsigned int i = 0;
	while(i < pInfo->uParameterCount)
	{
		tagEntryInfo * tag = cxxTagBegin(
				vStringValue(pInfo->aIdentifiers[i]->pszWord),
				CXXTagKindPARAMETER,
				pInfo->aIdentifiers[i]
			);

		if(tag)
		{
			// FIXME: We have the type here!
			//cxxTokenChainExtractRangeOmitToken(oParamInfo.aDeclarationStarts[i],oParamInfo.aDeclarationEnds[i],oParamInfo.aIdentifiers[i]);
			
			tag->isFileScope = TRUE;
			cxxTagCommit();
		}
		i++;
	}
}



//
// This function checks if the specified token chain looks like a non K&R style function
// parameter list, eventually with default arguments and such.
//
// If pParamInfo is non NULL then the function will also gather informations about
// the parameters and store them.
//
boolean cxxParserTokenChainLooksLikeFunctionParameterList(CXXTokenChain * tc,CXXFunctionParameterInfo * pParamInfo)
{
	CXX_DEBUG_ENTER();
	CXX_DEBUG_ASSERT(tc->iCount >= 2,"At least linitial and final parenthesis should be there");

	if(pParamInfo)
		pParamInfo->uParameterCount = 0;

	if(tc->iCount == 2)
	{
		CXX_DEBUG_LEAVE_TEXT("Empty signature is valid for a function");
		return TRUE;
	}
	
	CXXToken * t = cxxTokenChainAt(tc,1);

	boolean bIsC = cxxParserCurrentLanguageIsC();

	for(;;)
	{
		// Check every parameter.
		//
		// Possibilities:
		//
		//    type variable
		//    type /* variable omitted */
		//    type variable[..]
		//    type variable:bits
		//    type (*variable)(args)
		//    <anything of the above> = default <-- C++ only   
		//    ... <-- vararg
		//
		
		CXXToken * pStart = t;
	
		// First token must be identifier/keyword or ...
		if(!(t->eType & (CXXTokenTypeIdentifier | CXXTokenTypeKeyword | CXXTokenTypeMultipleDots)))
		{
			CXX_DEBUG_LEAVE_TEXT("Token '%s' is something that is not a identifier, keyword or ...",vStringValue(t->pszWord));
			return FALSE;
		}

#define TOKENS_THAT_SHOULD_NOT_APPEAR_IN_SIGNATURE_BEFORE_ASSIGNMENT \
		( \
			CXXTokenTypePointerOperator | \
			CXXTokenTypeOperator | \
			CXXTokenTypeDotOperator | \
			CXXTokenTypeNumber | \
			CXXTokenTypeStringConstant | \
			CXXTokenTypeCharacterConstant | \
			CXXTokenTypeAngleBracketChain | \
			CXXTokenTypeSingleColon \
		)

		t = cxxTokenChainNextTokenOfType(
				t,
				CXXTokenTypeClosingParenthesis | CXXTokenTypeComma | CXXTokenTypeAssignment | CXXTokenTypeSmallerThanSign |
				TOKENS_THAT_SHOULD_NOT_APPEAR_IN_SIGNATURE_BEFORE_ASSIGNMENT
			);

		CXX_DEBUG_ASSERT(t,"We should have found the closing parenthesis here");

		if(t->eType == CXXTokenTypeSmallerThanSign)
		{
			CXX_DEBUG_PRINT("Maybe template?");

			t = cxxTokenChainSkipToEndOfAngleBracket(t);

			if(!t)
			{
				CXX_DEBUG_LEAVE_TEXT("Either not a function declaration or unbalanced template angle brackets");
				return FALSE;
			}

			t = cxxTokenChainNextTokenOfType(
					t,
					CXXTokenTypeClosingParenthesis | CXXTokenTypeComma | CXXTokenTypeAssignment |
					TOKENS_THAT_SHOULD_NOT_APPEAR_IN_SIGNATURE_BEFORE_ASSIGNMENT
				);
	
			CXX_DEBUG_ASSERT(t,"We should have found the closing parenthesis here");
		}

		if(t->eType & TOKENS_THAT_SHOULD_NOT_APPEAR_IN_SIGNATURE_BEFORE_ASSIGNMENT)
		{
			CXX_DEBUG_LEAVE_TEXT("Token '%s' is something that doesn't belong to a function parameter list",vStringValue(t->pszWord));
			return FALSE;
		}

		// closing parenthesis, assignment or comma

		if(pParamInfo && (t->pPrev != pStart))
		{
			// FIXME: This may break in some special macro cases?
			if(pParamInfo->uParameterCount < CXX_MAX_EXTRACTED_PARAMETERS)
			{
				// locate identifier

				CXXToken * pIdentifier = NULL;

				if(t->pPrev->eType == CXXTokenTypeIdentifier)
				{
					// type var
					pIdentifier = t->pPrev;
				} else if(t->pPrev->pPrev)
				{
					if(
						(t->pPrev->eType == CXXTokenTypeSquareParenthesisChain) &&
						(t->pPrev->pPrev->eType == CXXTokenTypeIdentifier)
					)
					{
						// type var[]
						pIdentifier = t->pPrev->pPrev;
					} else if(
						(t->pPrev->eType == CXXTokenTypeNumber) &&
						(t->pPrev->pPrev->eType == CXXTokenTypeIdentifier)
					)
					{
						// type var:bits
						pIdentifier = t->pPrev->pPrev;
					} else if(
						(t->pPrev->eType == CXXTokenTypeParenthesisChain) &&
						(t->pPrev->pPrev->eType == CXXTokenTypeParenthesisChain) &&
						(pIdentifier = cxxTokenChainLastPossiblyNestedTokenOfType(t->pPrev->pPrev->pChain,CXXTokenTypeIdentifier))
					)
					{
						// type (*ptr)(args)
						// pIdentifier already set above
						// FIXME: Check this better?
					}
				}
				
				if(pIdentifier)
				{
					pParamInfo->aDeclarationStarts[pParamInfo->uParameterCount] = pStart;
					pParamInfo->aDeclarationEnds[pParamInfo->uParameterCount] = t->pPrev;
					pParamInfo->aIdentifiers[pParamInfo->uParameterCount] = pIdentifier;
					pParamInfo->uParameterCount++;
					
#ifdef CXX_DO_DEBUGGING
					CXXToken * pDecl = cxxTokenChainExtractRange(pStart,t->pPrev,0);
					CXX_DEBUG_PRINT("Found parameter '%s' in '%s'",vStringValue(pIdentifier->pszWord),vStringValue(pDecl->pszWord));
					cxxTokenDestroy(pDecl);
#endif
				}
			} else {
				pParamInfo = NULL; // reset so condition will be faster to check
			}
		}

		if(t->eType == CXXTokenTypeClosingParenthesis)
		{
			CXX_DEBUG_LEAVE_TEXT("Found closing parenthesis, it's OK");
			return TRUE;
		}
		
		if(t->eType == CXXTokenTypeComma)
		{
			// ok, go ahead
			CXX_DEBUG_PRINT("Found comma");
			t = t->pNext;
			continue;
		}

		// assignment.
		if(bIsC)
		{
			CXX_DEBUG_LEAVE_TEXT("Found assignment, this doesn't look like valid C function parameter list");
			return FALSE;
		}

		CXX_DEBUG_PRINT("Found assignment");
		
		t = cxxTokenChainNextTokenOfType(t,CXXTokenTypeClosingParenthesis | CXXTokenTypeComma);
		
		CXX_DEBUG_ASSERT(t,"We should have found the closing parenthesis here");

		if(t->eType == CXXTokenTypeClosingParenthesis)
		{
			CXX_DEBUG_LEAVE_TEXT("Found closing parenthesis, it's OK");
			return TRUE;
		}

		// ok, comma
		t = t->pNext;
	}

	// not reached
	CXX_DEBUG_LEAVE();
	return TRUE;
}

