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
// The global parser state
//
CXXParserState g_cxx;

//
// This is set to false once the parser is run at least one time.
// Used by cleanup routines.
//
boolean g_bFirstRun = TRUE;

//
// Reset parser state:
// - Clear the token chain
// - Reset "seen" keywords
//
void cxxParserNewStatement(void)
{
	cxxTokenChainClear(g_cxx.pTokenChain);
	if(g_cxx.pTemplateTokenChain)
	{
		cxxTokenChainDestroy(g_cxx.pTemplateTokenChain);
		g_cxx.pTemplateTokenChain = NULL;
	}
	g_cxx.uKeywordState = 0;

	// FIXME: this cpp handling of end/statement is kind of broken:
	//        it works only because the moon is in the correct phase.
	cppEndStatement();
}

//
// Parse a subchain of input delimited by matching pairs: [],(),{},<>
// [WARNING: no other subchain types are recognized!]. This function expects
// g_cxx.pToken to point to the initial token of the pair ([{<.
// It will parse input until the matching terminator token is found.
// Inner parsing is done by cxxParserParseAndCondenseSubchainsUpToOneOf()
// so this is actually a recursive subchain nesting algorithm.
//
boolean cxxParserParseAndCondenseCurrentSubchain(
		unsigned int uInitialSubchainMarkerTypes,
		boolean bAcceptEOF
	)
{
	CXXTokenChain * pCurrentChain = g_cxx.pTokenChain;

	g_cxx.pTokenChain = cxxTokenChainCreate();

	CXXToken * pInitial = cxxTokenChainTakeLast(pCurrentChain);
	cxxTokenChainAppend(g_cxx.pTokenChain,pInitial);

	CXXToken * pChainToken = cxxTokenCreate();

	pChainToken->iLineNumber = pInitial->iLineNumber;
	pChainToken->oFilePosition = pInitial->oFilePosition;
	// see the declaration of CXXTokenType enum.
	// Shifting by 8 gives the corresponding chain marker
	pChainToken->eType = (enum CXXTokenType)(g_cxx.pToken->eType << 8);
	pChainToken->pChain = g_cxx.pTokenChain;
	cxxTokenChainAppend(pCurrentChain,pChainToken);

	// see the declaration of CXXTokenType enum.
	// Shifting by 4 gives the corresponding closing token type
	unsigned int uTokenTypes = g_cxx.pToken->eType << 4;
	if(bAcceptEOF)
		uTokenTypes |= CXXTokenTypeEOF;
	boolean bRet = cxxParserParseAndCondenseSubchainsUpToOneOf(
			uTokenTypes,
			uInitialSubchainMarkerTypes
		);
	g_cxx.pTokenChain = pCurrentChain;
	g_cxx.pToken = pCurrentChain->pTail;

	return bRet;
}

//
// This function parses input until one of the specified tokens appears.
// The current token is NOT checked agains the specified tokens.
//
// The algorithm will also build subchains of matching
// pairs ([...],(...),<...>,{...}): within the subchain analysis
// of uTokenTypes is completly disabled. Subchains do nest.
//
// Returns true if it stops before EOF or it stops at EOF and CXXTokenTypeEOF
// is present in uTokenTypes. Returns false in all the other stop conditions
// and when an unmatched subchain character pair is found (syntax error).
//
boolean cxxParserParseAndCondenseSubchainsUpToOneOf(
		unsigned int uTokenTypes,
		unsigned int uInitialSubchainMarkerTypes
	)
{
	CXX_DEBUG_ENTER_TEXT("Token types = 0x%x",uTokenTypes);
	if(!cxxParserParseNextToken())
	{
		CXX_DEBUG_LEAVE_TEXT("Found EOF");
		return (uTokenTypes & CXXTokenTypeEOF); // was already at EOF
	}

	// see the declaration of CXXTokenType enum.
	// Shifting by 4 gives the corresponding closing token type
	unsigned int uFinalSubchainMarkerTypes = uInitialSubchainMarkerTypes << 4;

	for(;;)
	{
		//CXX_DEBUG_PRINT(
		//		"Current token is '%s' 0x%x",
		//		vStringValue(g_cxx.pToken->pszWord),
		//		g_cxx.pToken->eType
		//);

		if(cxxTokenTypeIsOneOf(g_cxx.pToken,uTokenTypes))
		{
			CXX_DEBUG_LEAVE_TEXT(
					"Got terminator token '%s' 0x%x",
					vStringValue(g_cxx.pToken->pszWord),
					g_cxx.pToken->eType
				);
			return TRUE;
		}

		if(cxxTokenTypeIsOneOf(g_cxx.pToken,uInitialSubchainMarkerTypes))
		{
			// subchain
			CXX_DEBUG_PRINT(
					"Got subchain start token '%s' 0x%x",
					vStringValue(g_cxx.pToken->pszWord),
					g_cxx.pToken->eType
				);
			CXXToken * pParenthesis;

			if(
				cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeOpeningBracket) &&
				cxxParserCurrentLanguageIsCPP() &&
				(pParenthesis = cxxParserOpeningBracketIsLambda())
			)
			{
				if(!cxxParserHandleLambda(pParenthesis))
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to handle lambda");
					return FALSE;
				}
			} else {
				if(!cxxParserParseAndCondenseCurrentSubchain(
						uInitialSubchainMarkerTypes,
						(uTokenTypes & CXXTokenTypeEOF)
					)
				)
				{
					CXX_DEBUG_LEAVE_TEXT(
							"Failed to parse subchain of type 0x%x",
							g_cxx.pToken->eType
						);
					return FALSE;
				}
			}

			if(cxxTokenTypeIsOneOf(g_cxx.pToken,uTokenTypes))
			{
				// was looking for a subchain
				CXX_DEBUG_LEAVE_TEXT(
						"Got terminator subchain token 0x%x",
						g_cxx.pToken->eType
					);
				return TRUE;
			}

			if(!cxxParserParseNextToken())
			{
				CXX_DEBUG_LEAVE_TEXT("Found EOF(2)");
				return (uTokenTypes & CXXTokenTypeEOF); // was already at EOF
			}

			continue; // jump up to avoid checking for mismatched pairs below
		}

		// Check for mismatched brackets/parenthis
		// Note that if we were looking for one of [({ then we would have matched
		// it at the top of the for
		if(cxxTokenTypeIsOneOf(g_cxx.pToken,uFinalSubchainMarkerTypes))
		{
			CXX_DEBUG_LEAVE_TEXT(
					"Got mismatched subchain terminator 0x%x",
					g_cxx.pToken->eType
				);
			return FALSE; // unmatched: syntax error
		}

		if(!cxxParserParseNextToken())
		{
			CXX_DEBUG_LEAVE_TEXT("Found EOF(3)");
			return (uTokenTypes & CXXTokenTypeEOF); // was already at EOF
		}
	}

	// not reached
	CXX_DEBUG_LEAVE_TEXT("Internal error");
	return FALSE;
}

//
// This function parses input until one of the specified tokens appears.
// The current token is NOT checked agains the specified tokens.
//
// The algorithm will also build subchains of matching pairs ([...],(...),{...}).
// Within the subchain analysis of uTokenTypes is completly disabled.
// Subchains do nest.
//
// Please note that this function will skip entire scopes (matching {} pairs)
// unless you pass CXXTokenTypeOpeningBracket to stop at their beginning.
// This is usually what you want, unless you're really expecting a scope to begin
// in the current statement.
//
boolean cxxParserParseUpToOneOf(unsigned int uTokenTypes)
{
	return cxxParserParseAndCondenseSubchainsUpToOneOf(
			uTokenTypes,
			CXXTokenTypeOpeningBracket |
				CXXTokenTypeOpeningParenthesis |
				CXXTokenTypeOpeningSquareParenthesis
		);
}

//
// Attempts to skip to either a semicolon or an EOF, ignoring anything in between.
// May be also used to recovery from certain forms of syntax errors.
// This function works also if the current token is a semicolon or an EOF.
//
boolean cxxParserSkipToSemicolonOrEOF(void)
{
	if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeSemicolon | CXXTokenTypeEOF))
		return TRUE;

	return cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF);
}

// This has to be called when pointing to a double-colon token
// or an identifier.
//
// It tries to parse a qualified name in the form of ...::A::B::C::D ...
// and stops at the first token that is not part of such name.
//
// Returns false if it doesn't find an identifier after a double-colon
// or if it finds an EOF. Returns true otherwise.
//
// Upon exit the token preceding the current is the last identifier
// of the qualified name.
boolean cxxParserParseToEndOfQualifedName(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIsOneOf(
					g_cxx.pToken,
					CXXTokenTypeMultipleColons | CXXTokenTypeIdentifier
				),
			"This function should be called when pointing to a double-colon or an identifier"
		);

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier))
	{
		if(!cxxParserParseNextToken())
		{
			// syntax error, but we tolerate this
			CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
			return FALSE; // EOF
		}
	}

	while(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeMultipleColons))
	{
		if(!cxxParserParseNextToken())
		{
			// syntax error, but we tolerate this
			CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
			return FALSE; // EOF
		}

		if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier))
		{
			CXX_DEBUG_LEAVE_TEXT("Found no identifier after multiple colons");
			return FALSE;
		}

		if(!cxxParserParseNextToken())
		{
			// syntax error, but we tolerate this
			CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
			return FALSE; // EOF
		}
	}

	CXX_DEBUG_ASSERT(g_cxx.pToken->pPrev,"There should be a previous token here");
	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(g_cxx.pToken->pPrev,CXXTokenTypeIdentifier),
			"The qualified name should end with an identifier"
		);

	CXX_DEBUG_LEAVE();
	return TRUE;
}


//
// Attach the current position of input file as "end" field of
// the specified tag in the cork queue
//
void cxxParserMarkEndLineForTagInCorkQueue(int iCorkQueueIndex)
{
	CXX_DEBUG_ASSERT(iCorkQueueIndex > CORK_NIL,"The cork queue index is not valid");

	tagEntryInfo * tag = getEntryInCorkQueue (iCorkQueueIndex);

	CXX_DEBUG_ASSERT(tag,"No tag entry in the cork queue");

	tag->extensionFields.endLine = getInputLineNumber();
}

// Make sure that the token chain contains only the specified keyword and eventually
// the "const" or "volatile" type modifiers.
static void cxxParserCleanupEnumStructClassOrUnionPrefixChain(enum CXXKeyword eKeyword)
{
	CXXToken * pToken = cxxTokenChainFirst(g_cxx.pTokenChain);
	while(pToken)
	{
		if(
				cxxTokenTypeIs(pToken,CXXTokenTypeKeyword) &&
				(
					(pToken->eKeyword == eKeyword) ||
					(pToken->eKeyword == CXXKeywordCONST) ||
					(pToken->eKeyword == CXXKeywordVOLATILE)
				)
			)
		{
			// keep
			pToken = pToken->pNext;
		} else {
			CXXToken * pPrev = pToken->pPrev;
			if(pPrev)
			{
				cxxTokenChainTake(g_cxx.pTokenChain,pToken);
				cxxTokenDestroy(pToken);
				pToken = pPrev->pNext;
			} else {
				cxxTokenChainDestroyFirst(g_cxx.pTokenChain);
				pToken = cxxTokenChainFirst(g_cxx.pTokenChain);
			}
		}
	}
}

//
// This is called after a full enum/struct/class/union declaration
// that ends with a closing bracket.
//
static boolean cxxParserParseEnumStructClassOrUnionFullDeclarationTrailer(
		unsigned int uKeywordState,
		enum CXXKeyword eTagKeyword,
		const char * szTypeName
	)
{
	CXX_DEBUG_ENTER();

	cxxTokenChainClear(g_cxx.pTokenChain);

	CXX_DEBUG_PRINT(
			"Parse enum/struct/class/union trailer, typename is '%s'",
			szTypeName
		);

	MIOPos oFilePosition = getInputFilePosition();
	int iFileLine = getInputLineNumber();

	if(!cxxParserParseUpToOneOf(CXXTokenTypeEOF | CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to EOF/semicolon");
		return FALSE;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
	{
		// It's a syntax error, but we can be tolerant here.
		CXX_DEBUG_LEAVE_TEXT("Got EOF after enum/class/struct/union block");
		return TRUE;
	}

	if(g_cxx.pTokenChain->iCount < 2)
	{
		CXX_DEBUG_LEAVE_TEXT("Nothing interesting after enum/class/struct block");
		return TRUE;
	}

	// fake the initial two tokens
	CXXToken * pIdentifier = cxxTokenCreate();
	pIdentifier->oFilePosition = oFilePosition;
	pIdentifier->iLineNumber = iFileLine;
	pIdentifier->eType = CXXTokenTypeIdentifier;
	pIdentifier->bFollowedBySpace = TRUE;
	vStringCatS(pIdentifier->pszWord,szTypeName);
	cxxTokenChainPrepend(g_cxx.pTokenChain,pIdentifier);

	cxxTokenChainPrepend(
			g_cxx.pTokenChain,
			cxxTokenCreateKeyword(iFileLine,oFilePosition,eTagKeyword)
		);

	if(uKeywordState & CXXParserKeywordStateSeenConst)
	{
		cxxTokenChainPrepend(
				g_cxx.pTokenChain,
				cxxTokenCreateKeyword(iFileLine,oFilePosition,CXXKeywordCONST)
			);
	}

	if(uKeywordState & CXXParserKeywordStateSeenVolatile)
	{
		cxxTokenChainPrepend(
				g_cxx.pTokenChain,
				cxxTokenCreateKeyword(iFileLine,oFilePosition,CXXKeywordVOLATILE)
			);
	}

	if(uKeywordState & CXXParserKeywordStateSeenTypedef)
		cxxParserExtractTypedef(g_cxx.pTokenChain,TRUE);
	else
		cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);

	CXX_DEBUG_LEAVE();
	return TRUE;
}

boolean cxxParserParseEnum(void)
{
	CXX_DEBUG_ENTER();

	unsigned int uInitialKeywordState = g_cxx.uKeywordState;

	if(g_cxx.pTokenChain->iCount > 1)
		cxxParserCleanupEnumStructClassOrUnionPrefixChain(CXXKeywordENUM);

	/*
		Spec is:
			enum-key attr(optional) identifier(optional) enum-base(optional)
				{ enumerator-list(optional) }	(1)
			enum-key attr(optional) identifier enum-base(optional) ;
				(2)	(since C++11)
	*/

	// Skip attr and class-head-name
	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeEOF | CXXTokenTypeSemicolon |
				CXXTokenTypeParenthesisChain | CXXTokenTypeOpeningBracket
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Could not parse enum name");
		return FALSE;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain))
	{
		// probably a function declaration/prototype
		// something like enum x func()....
		// do not clear statement
		CXX_DEBUG_LEAVE_TEXT("Probably a function declaration!");
		return TRUE;
	}

	// FIXME: This block is duplicated in struct/union/class
	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
	{
		if(g_cxx.pTokenChain->iCount > 3)
		{
			 // [typedef] struct X Y; <-- typedef has been removed!
			if(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef)
				cxxParserExtractTypedef(g_cxx.pTokenChain,TRUE);
			else
				cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);
		}

		cxxParserNewStatement();
		CXX_DEBUG_LEAVE();
		return TRUE;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
	{
		// tolerate EOF, treat as forward declaration
		cxxParserNewStatement();
		CXX_DEBUG_LEAVE_TEXT("EOF before enum block: treating as forward declaration");
		return TRUE;
	}

	// semicolon or opening bracket

	// check if we can extract a class name identifier
	CXXToken * pEnumName = cxxTokenChainLastTokenOfType(
			g_cxx.pTokenChain,
			CXXTokenTypeIdentifier
		);

	int iPushedScopes = 0;

	if(pEnumName)
	{
		// good.
		// It may be qualified though.
		if(cxxParserCurrentLanguageIsCPP())
		{
			CXXToken * pNamespaceBegin = pEnumName;
			CXXToken * pPrev = pEnumName->pPrev;
			while(pPrev)
			{
				if(!cxxTokenTypeIs(pPrev,CXXTokenTypeMultipleColons))
					break;
				pPrev = pPrev->pPrev;
				if(!pPrev)
					break;
				if(!cxxTokenTypeIs(pPrev,CXXTokenTypeIdentifier))
					break;
				pNamespaceBegin = pPrev;
				pPrev = pPrev->pPrev;
			}

			while(pNamespaceBegin != pEnumName)
			{
				CXXToken * pNext = pNamespaceBegin->pNext;
				cxxTokenChainTake(g_cxx.pTokenChain,pNamespaceBegin);
				// FIXME: We don't really know if it's a class!
				cxxScopePush(pNamespaceBegin,CXXScopeTypeClass,CXXScopeAccessUnknown);
				iPushedScopes++;
				pNamespaceBegin = pNext->pNext;
			}
		}

		CXX_DEBUG_PRINT("Enum name is %s",vStringValue(pEnumName->pszWord));
		cxxTokenChainTake(g_cxx.pTokenChain,pEnumName);
	} else {
		pEnumName = cxxTokenCreateAnonymousIdentifier(CXXTagKindENUM);
		CXX_DEBUG_PRINT(
				"Enum name is %s (anonymous)",
				vStringValue(pEnumName->pszWord)
			);
	}

	tagEntryInfo * tag = cxxTagBegin(CXXTagKindENUM,pEnumName);

	int iCorkQueueIndex = CORK_NIL;

	if(tag)
	{
		// FIXME: this is debatable
		tag->isFileScope = !isInputHeaderFile();

		iCorkQueueIndex = cxxTagCommit();
	}

	cxxScopePush(pEnumName,CXXScopeTypeEnum,CXXScopeAccessPublic);
	iPushedScopes++;

	vString * pScopeName = cxxScopeGetFullNameAsString();

	// Special kind of block
	for(;;)
	{
		cxxTokenChainClear(g_cxx.pTokenChain);

		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeComma | CXXTokenTypeClosingBracket | CXXTokenTypeEOF
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse enum contents");
			if(pScopeName)
				vStringDelete(pScopeName);
			return FALSE;
		}

		CXXToken * pFirst = cxxTokenChainFirst(g_cxx.pTokenChain);

		// enumerator.
		if(
				(g_cxx.pTokenChain->iCount > 1) &&
				cxxTokenTypeIs(pFirst,CXXTokenTypeIdentifier)
			)
		{
			tag = cxxTagBegin(CXXTagKindENUMERATOR,pFirst);
			if(tag)
			{
				tag->isFileScope = !isInputHeaderFile();
				cxxTagCommit();
			}
		}

		if(cxxTokenTypeIsOneOf(
				g_cxx.pToken,
				CXXTokenTypeEOF | CXXTokenTypeClosingBracket
			))
			break;
	}

	if(iCorkQueueIndex > CORK_NIL)
		cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndex);

	while(iPushedScopes > 0)
	{
		cxxScopePop();
		iPushedScopes--;
	}

	boolean bRet = cxxParserParseEnumStructClassOrUnionFullDeclarationTrailer(
			uInitialKeywordState,
			CXXKeywordENUM,
			vStringValue(pScopeName)
		);

	if(pScopeName)
		vStringDelete(pScopeName);

	cxxParserNewStatement();
	CXX_DEBUG_LEAVE();
	return bRet;
};

static boolean cxxParserParseClassStructOrUnionInternal(
		enum CXXKeyword eKeyword,
		unsigned int uTagKind,
		unsigned int uScopeType
	)
{
	CXX_DEBUG_ENTER();

	unsigned int uInitialKeywordState = g_cxx.uKeywordState;

	if(g_cxx.pTokenChain->iCount > 1)
		cxxParserCleanupEnumStructClassOrUnionPrefixChain(eKeyword);

	/*
		Spec is:
			class-key attr class-head-name base-clause { member-specification }

			class-key	-	one of class or struct. The keywords are identical
				except for the default member access and the default base class access.
			attr(C++11)	-	optional sequence of any number of attributes,
				may include alignas specifier
			class-head-name	-	the name of the class that's being defined.
				Optionally qualified, optionally followed by keyword final.
				The name may be omitted, in which case the class is unnamed (note
				that unnamed class cannot be final)
			base-clause	-	optional list of one or more parent classes and the
				model of inheritance used for each (see derived class)
			member-specification	-	list of access specifiers, member object and
				member function declarations and definitions (see below)
	*/

	// Skip attr and class-head-name

	// enable "final" keyword handling
	g_cxx.bParsingClassStructOrUnionDeclaration = TRUE;

	unsigned int uTerminatorTypes = CXXTokenTypeEOF | CXXTokenTypeSingleColon |
			CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket |
			CXXTokenTypeSmallerThanSign;

	if(uTagKind != CXXTagCPPKindCLASS)
		uTerminatorTypes |= CXXTokenTypeParenthesisChain | CXXTokenTypeAssignment;

	boolean bRet;

	for(;;)
	{
		bRet = cxxParserParseUpToOneOf(uTerminatorTypes);

		if(!bRet)
		{
			g_cxx.bParsingClassStructOrUnionDeclaration = FALSE;
			CXX_DEBUG_LEAVE_TEXT("Could not parse class/struct/union name");
			return FALSE;
		}

		if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSmallerThanSign))
			break;

		// Probably a template specialisation

		// template<typename T> struct X<int>
		// {
		// }

		// FIXME: Should we add the specialisation arguments somewhere?
		//        Maye as a separate field?

		bRet = cxxParserParseAndCondenseCurrentSubchain(
					CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningBracket |
						CXXTokenTypeOpeningSquareParenthesis |
						CXXTokenTypeSmallerThanSign,
					FALSE
				);

		if(!bRet)
		{
			g_cxx.bParsingClassStructOrUnionDeclaration = FALSE;
			CXX_DEBUG_LEAVE_TEXT("Could not parse class/struct/union name");
			return FALSE;
		}
	}

	g_cxx.bParsingClassStructOrUnionDeclaration = FALSE;

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain))
	{
		// probably a function declaration/prototype
		// something like struct x * func()....
		// do not clear statement
		CXX_DEBUG_LEAVE_TEXT("Probably a function declaration!");
		return TRUE;
	}

	// FIXME: This block is duplicated in enum
	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
	{
		if(g_cxx.pTokenChain->iCount > 3)
		{
			// [typedef] struct X Y; <-- typedef has been removed!
			if(uInitialKeywordState & CXXParserKeywordStateSeenTypedef)
				cxxParserExtractTypedef(g_cxx.pTokenChain,TRUE);
			else
				cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);
		}

		cxxParserNewStatement();
		CXX_DEBUG_LEAVE();
		return TRUE;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeAssignment))
	{
		if(g_cxx.pTokenChain->iCount > 3)
		{
			// struct X Y = ...;
			cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);
		}

		// Skip the initialization (which almost certainly contains a block)
		if(!cxxParserParseUpToOneOf(CXXTokenTypeEOF | CXXTokenTypeSemicolon))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse up to EOF/semicolon");
			return FALSE;
		}

		cxxParserNewStatement();
		CXX_DEBUG_LEAVE();
		return TRUE;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
	{
		// tolerate EOF, just ignore this
		cxxParserNewStatement();
		CXX_DEBUG_LEAVE_TEXT("EOF: ignoring");
		return TRUE;
	}

	// semicolon or opening bracket

	// check if we can extract a class name identifier
	CXXToken * pClassName = cxxTokenChainLastTokenOfType(
			g_cxx.pTokenChain,
			CXXTokenTypeIdentifier
		);

	int iPushedScopes = 0;

	if(pClassName)
	{
		// good.
		// It may be qualified though.
		CXXToken * pNamespaceBegin = pClassName;
		CXXToken * pPrev = pClassName->pPrev;
		while(pPrev)
		{
			if(!cxxTokenTypeIs(pPrev,CXXTokenTypeMultipleColons))
				break;
			pPrev = pPrev->pPrev;
			if(!pPrev)
				break;
			if(!cxxTokenTypeIs(pPrev,CXXTokenTypeIdentifier))
				break;
			pNamespaceBegin = pPrev;
			pPrev = pPrev->pPrev;
		}

		while(pNamespaceBegin != pClassName)
		{
			CXXToken * pNext = pNamespaceBegin->pNext;
			cxxTokenChainTake(g_cxx.pTokenChain,pNamespaceBegin);
			// FIXME: We don't really know if it's a class!
			cxxScopePush(pNamespaceBegin,CXXScopeTypeClass,CXXScopeAccessUnknown);
			iPushedScopes++;
			pNamespaceBegin = pNext->pNext;
		}

		CXX_DEBUG_PRINT(
				"Class/struct/union name is %s",
				vStringValue(pClassName->pszWord)
			);
		cxxTokenChainTake(g_cxx.pTokenChain,pClassName);
	} else {
		pClassName = cxxTokenCreateAnonymousIdentifier(uTagKind);
		CXX_DEBUG_PRINT(
				"Class/struct/union name is %s (anonymous)",
				vStringValue(pClassName->pszWord)
			);
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSingleColon))
	{
		// check for base classes
		cxxTokenChainClear(g_cxx.pTokenChain);

		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeEOF | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket
			))
		{
			cxxTokenDestroy(pClassName);
			CXX_DEBUG_LEAVE_TEXT("Failed to parse base class part");
			return FALSE;
		}

		if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeSemicolon | CXXTokenTypeEOF))
		{
			cxxTokenDestroy(pClassName);
			cxxParserNewStatement();
			CXX_DEBUG_LEAVE_TEXT("Syntax error: ignoring");
			return TRUE;
		}

		cxxTokenChainDestroyLast(g_cxx.pTokenChain); // remove the {
	} else {
		cxxTokenChainClear(g_cxx.pTokenChain);
	}

	tagEntryInfo * tag = cxxTagBegin(uTagKind,pClassName);

	int iCorkQueueIndex = CORK_NIL;

	if(tag)
	{
		if(g_cxx.pTokenChain->iCount > 0)
		{
			// Strip inheritance type information
			// FIXME: This could be optional!

			CXXToken * t = cxxTokenChainFirst(g_cxx.pTokenChain);
			while(t)
			{
				if(
					cxxTokenTypeIs(t,CXXTokenTypeKeyword) &&
					(
						(t->eKeyword == CXXKeywordPUBLIC) ||
						(t->eKeyword == CXXKeywordPROTECTED) ||
						(t->eKeyword == CXXKeywordPRIVATE) ||
						(t->eKeyword == CXXKeywordVIRTUAL)
					)
				)
				{
					CXXToken * pNext = t->pNext;
					cxxTokenChainTake(g_cxx.pTokenChain,t);
					cxxTokenDestroy(t);
					t = pNext;
				} else {
					t = t->pNext;
				}
			}

			if(g_cxx.pTokenChain->iCount > 0)
			{
				cxxTokenChainCondense(
						g_cxx.pTokenChain,
						CXXTokenChainCondenseNoTrailingSpaces
					);
				tag->extensionFields.inheritance = vStringValue(
						g_cxx.pTokenChain->pHead->pszWord
					);
			}
		}

		if(
			g_cxx.pTemplateTokenChain && (g_cxx.pTemplateTokenChain->iCount > 0) &&
			cxxTagFieldEnabled(CXXTagCPPFieldTemplate)
		)
		{
			cxxTokenChainNormalizeTypeNameSpacing(g_cxx.pTemplateTokenChain);
			cxxTokenChainCondense(g_cxx.pTemplateTokenChain,0);
			cxxTagSetField(
					CXXTagCPPFieldTemplate,
					vStringValue(cxxTokenChainFirst(g_cxx.pTemplateTokenChain)->pszWord)
				);
		}

		tag->isFileScope = !isInputHeaderFile();

		iCorkQueueIndex = cxxTagCommit();
	}

	cxxScopePush(
			pClassName,
			uScopeType,
			(uTagKind == CXXTagCPPKindCLASS) ?
				CXXScopeAccessPrivate : CXXScopeAccessPublic
		);

	vString * pScopeName = cxxScopeGetFullNameAsString();

	if(!cxxParserParseBlock(TRUE))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse scope");
		if(pScopeName)
			vStringDelete(pScopeName);
		return FALSE;
	}

	if(iCorkQueueIndex > CORK_NIL)
		cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndex);

	iPushedScopes++;
	while(iPushedScopes > 0)
	{
		cxxScopePop();
		iPushedScopes--;
	}

	bRet = cxxParserParseEnumStructClassOrUnionFullDeclarationTrailer(
			uInitialKeywordState,
			eKeyword,
			vStringValue(pScopeName)
		);

	if(pScopeName)
		vStringDelete(pScopeName);

	cxxParserNewStatement();
	CXX_DEBUG_LEAVE();
	return bRet;
}

boolean cxxParserParseClassStructOrUnion(
		enum CXXKeyword eKeyword,
		unsigned int uTagKind,
		unsigned int uScopeType
	)
{
	// Trick for "smart" handling of public/protected/private keywords in .h files parsed as C++.
	// See the declaration of bEnablePublicProtectedPrivateKeywords for more info.

	// Save the state
	boolean bEnablePublicProtectedPrivateKeywords = g_cxx.bEnablePublicProtectedPrivateKeywords;

	// If parsing of the keywords was disabled, we're in C++ mode and the keyword is "class" then
	// we're fairly certain that the source code is *really* C++.
	if(
			(!bEnablePublicProtectedPrivateKeywords) &&
			(eKeyword == CXXKeywordCLASS) &&
			cxxParserCurrentLanguageIsCPP()
		)
		bEnablePublicProtectedPrivateKeywords = TRUE; // leave it on for good.

	// Enable public/protected/private keywords
	g_cxx.bEnablePublicProtectedPrivateKeywords = TRUE;

	boolean bRet = cxxParserParseClassStructOrUnionInternal(eKeyword,uTagKind,uScopeType);

	g_cxx.bEnablePublicProtectedPrivateKeywords = bEnablePublicProtectedPrivateKeywords;

	return bRet;
}


//
// This is called at block level, upon encountering a semicolon, an unbalanced
// closing bracket or EOF.The current token is something like:
//   static const char * variable;
//   int i = ....
//   const QString & function(whatever) const;
//   QString szText("ascii");
//   QString(...)
//
// Notable facts:
//   - several special statements never end up here: this includes class,
//     struct, union, enum, namespace, typedef, case, try, catch and other
//     similar stuff.
//   - the terminator is always at the end. It's either a semicolon, a closing
//     bracket or an EOF
//   - the parentheses and brackets are always condensed in subchains
//     (unless unbalanced).
//
//                int __attribute__() function();
//                                  |          |
//                             ("whatever")  (int var1,type var2)
//
//                const char * strings[] = {}
//                                    |     |
//                                   [10]  { "string","string",.... }
//
// This function tries to extract variable declarations and function prototypes.
//
// Yes, it's complex: it's because C/C++ is complex.
//
void cxxParserAnalyzeOtherStatement(void)
{
	CXX_DEBUG_ENTER();

#ifdef CXX_DO_DEBUGGING
	vString * pChain = cxxTokenChainJoin(g_cxx.pTokenChain,NULL,0);
	CXX_DEBUG_PRINT("Analyzing statement '%s'",vStringValue(pChain));
	vStringDelete(pChain);
#endif

	CXX_DEBUG_ASSERT(
			g_cxx.pTokenChain->iCount > 0,
			"There should be at least the terminator here!"
		);

	if(g_cxx.pTokenChain->iCount < 2)
	{
		CXX_DEBUG_LEAVE_TEXT("Empty statement");
		return;
	}

	if(g_cxx.uKeywordState & CXXParserKeywordStateSeenReturn)
	{
		CXX_DEBUG_LEAVE_TEXT("Statement after a return is not interesting");
		return;
	}

	// Everything we can make sense of starts with an identifier or keyword.
	// This is usually a type name (eventually decorated by some attributes
	// and modifiers) with the notable exception of constructor/destructor
	// declarations (which are still identifiers tho).

	CXXToken * t = cxxTokenChainFirst(g_cxx.pTokenChain);

	if(!cxxTokenTypeIsOneOf(t,CXXTokenTypeIdentifier | CXXTokenTypeKeyword))
	{
		CXX_DEBUG_LEAVE_TEXT("Statement does not start with an identifier or keyword");
		return;
	}

	enum CXXScopeType eScopeType = cxxScopeGetType();

	CXXFunctionSignatureInfo oInfo;

	// kinda looks like a function or variable instantiation... maybe
	if(eScopeType == CXXScopeTypeFunction)
	{
		// prefer variable declarations.
		// if none found then try function prototype
		if(cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0))
		{
			CXX_DEBUG_LEAVE_TEXT("Found variable declarations");
			return;
		}

		// FIXME: This *COULD* work but we should first rule out the possibility
		// of simple function calls like func(a). The function signature search
		// should be far stricter here.

		//if(cxxParserLookForFunctionSignature(g_cxx.pTokenChain,&oInfo,NULL))
		//	cxxParserEmitFunctionTags(&oInfo,CXXTagKindPROTOTYPE,0);

		CXX_DEBUG_LEAVE();
		return;
	}

	// prefer function.
check_function_signature:

	if(cxxParserLookForFunctionSignature(g_cxx.pTokenChain,&oInfo,NULL))
	{
		int iScopesPushed = cxxParserEmitFunctionTags(&oInfo,CXXTagKindPROTOTYPE,CXXEmitFunctionTagsPushScopes,NULL);
		while(iScopesPushed > 0)
		{
			cxxScopePop();
			iScopesPushed--;
		}
		CXX_DEBUG_LEAVE_TEXT("Found function prototype");

		if(oInfo.pTrailingComma)
		{
			// got a trailing comma after the function signature.
			// This might be a special case of multiple prototypes in a single declaration.
			//
			//   RetType functionA(...), functionB(...), functionC(...);
			//
			// Let's try to extract also the other declarations.
			//
			cxxTokenChainDestroyRange(g_cxx.pTokenChain,oInfo.pIdentifierStart,oInfo.pTrailingComma);
			goto check_function_signature;
		}

		return;
	}

	if(
		g_cxx.uKeywordState &
		(
			CXXParserKeywordStateSeenInline | CXXParserKeywordStateSeenExplicit |
			CXXParserKeywordStateSeenOperator | CXXParserKeywordStateSeenVirtual
		)
	)
	{
		// must be function!
		CXX_DEBUG_LEAVE_TEXT(
				"WARNING: Was expecting to find a function prototype " \
					"but did not find one"
			);
		return;
	}

	cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);
	CXX_DEBUG_LEAVE_TEXT("Nothing else");
}


// This is called when we encounter a "public", "protected" or "private" keyword
// that is NOT in the class declaration header line.
boolean cxxParserParseAccessSpecifier(void)
{
	CXX_DEBUG_ENTER();

	enum CXXScopeType eScopeType = cxxScopeGetType();

	if(
			(eScopeType != CXXScopeTypeClass) &&
			(eScopeType != CXXScopeTypeUnion) &&
			(eScopeType != CXXScopeTypeStruct)
		)
	{
		// this is a syntax error: we're in the wrong scope.
		CXX_DEBUG_LEAVE_TEXT(
				"Access specified in wrong context (%d): "
					"bailing out to avoid reporting broken structure",
				eScopeType
			);
		return FALSE;
	}

	switch(g_cxx.pToken->eKeyword)
	{
		case CXXKeywordPUBLIC:
			cxxScopeSetAccess(CXXScopeAccessPublic);
		break;
		case CXXKeywordPRIVATE:
			cxxScopeSetAccess(CXXScopeAccessPrivate);
		break;
		case CXXKeywordPROTECTED:
			cxxScopeSetAccess(CXXScopeAccessProtected);
		break;
		default:
			CXX_DEBUG_ASSERT(FALSE,"Bad keyword in cxxParserParseAccessSpecifier!");
		break;
	}

	// skip to the next :, without leaving scope.
	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeSingleColon | CXXTokenTypeSemicolon |
				CXXTokenTypeClosingBracket | CXXTokenTypeEOF
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to the next ;");
		return FALSE;
	}

	cxxTokenChainClear(g_cxx.pTokenChain);
	CXX_DEBUG_LEAVE();
	return TRUE;
}

boolean cxxParserParseIfForWhileSwitch(void)
{
	CXX_DEBUG_ENTER();

	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeParenthesisChain | CXXTokenTypeSemicolon |
				CXXTokenTypeOpeningBracket | CXXTokenTypeEOF
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse if/for/while/switch up to parenthesis");
		return FALSE;
	}

	if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeEOF | CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_LEAVE_TEXT("Found EOF/semicolon while parsing if/for/while/switch");
		return TRUE;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain))
	{
		// Extract variables from the parenthesis chain
		// We handle only simple cases.
		CXXTokenChain * pChain = g_cxx.pToken->pChain;

		CXX_DEBUG_ASSERT(
				pChain->iCount >= 2,
				"The parenthesis chain must have initial and final parenthesis"
			);

		// Simple check for cases like if(a & b), if(a * b).
		// If there is &, && or * then we expect there to be also a =.
		if(
				// & && * not present
				!cxxTokenChainFirstTokenOfType(
						pChain,
						CXXTokenTypeAnd | CXXTokenTypeMultipleAnds |
						CXXTokenTypeStar
					) ||
				// or = present
				cxxTokenChainFirstTokenOfType(
						pChain,
						CXXTokenTypeAssignment
					)
			)
		{
			// Kill the initial parenthesis
			cxxTokenChainDestroyFirst(pChain);
			// Fake the final semicolon
			CXXToken * t = cxxTokenChainLast(pChain);
			t->eType = CXXTokenTypeSemicolon;
			vStringClear(t->pszWord);
			vStringPut(t->pszWord,';');

			// and extract variable declarations if possible
			cxxParserExtractVariableDeclarations(pChain,0);
		}

		CXX_DEBUG_LEAVE_TEXT("Found if/for/while/switch parenthesis chain");
		return TRUE;
	}

	// must be opening bracket: parse it here.

	boolean bRet = cxxParserParseBlock(TRUE);

	CXX_DEBUG_LEAVE();

	return bRet;
}

static rescanReason cxxParserMain(const unsigned int passCount)
{
	cxxScopeClear();
	cxxTokenAPINewFile();
	cxxParserNewStatement();

	kindOption * kind_for_define = g_cxx.pKindOptions + CXXTagKindMACRO;
	kindOption * kind_for_header = g_cxx.pKindOptions + CXXTagKindINCLUDE;
	int role_for_macro_undef = CR_MACRO_UNDEF;
	int role_for_header_system = CR_HEADER_SYSTEM;
	int role_for_header_local = CR_HEADER_LOCAL;

	Assert(passCount < 3);

	cppInit(
			(boolean) (passCount > 1),
			FALSE,
			TRUE, // raw literals
			FALSE,
			kind_for_define,
			role_for_macro_undef,
			kind_for_header,
			role_for_header_system,
			role_for_header_local
		);

	g_cxx.iChar = ' ';

	boolean bRet = cxxParserParseBlock(FALSE);

	cppTerminate ();

	cxxTokenChainClear(g_cxx.pTokenChain);
	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainClear(g_cxx.pTemplateTokenChain);

	if(!bRet && (passCount == 1))
	{
		CXX_DEBUG_PRINT("Processing failed: trying to rescan");
		return RESCAN_FAILED;
	}

	return RESCAN_NONE;
}

rescanReason cxxCParserMain(const unsigned int passCount)
{
	CXX_DEBUG_ENTER();
	cxxTagInitForLanguage(g_cxx.eCLanguage);
	rescanReason r = cxxParserMain(passCount);
	CXX_DEBUG_LEAVE();
	return r;
}

rescanReason cxxCppParserMain(const unsigned int passCount)
{
	CXX_DEBUG_ENTER();
	cxxTagInitForLanguage(g_cxx.eCPPLanguage);

	// In header files we disable processing of public/protected/private keywords
	// until we either figure out that this is really C++ or we're start parsing
	// a struct/union.
	g_cxx.bEnablePublicProtectedPrivateKeywords = !isInputHeaderFile();

	CXX_DEBUG_PRINT("Parsing of public/protected/private is %d",g_cxx.bEnablePublicProtectedPrivateKeywords);

	rescanReason r = cxxParserMain(passCount);
	CXX_DEBUG_LEAVE();
	return r;
}

static void cxxParserFirstInit()
{
	memset(&g_cxx,0,sizeof(CXXParserState));

	g_cxx.eCLanguage = -1;
	g_cxx.eCPPLanguage = -1;

	cxxTokenAPIInit();

	g_cxx.pTokenChain = cxxTokenChainCreate();

	cxxScopeInit();

	g_bFirstRun = FALSE;
}

void cxxCppParserInitialize(const langType language)
{
	CXX_DEBUG_INIT();

	CXX_DEBUG_PRINT("Parser initialize for language C++");
	if(g_bFirstRun)
		cxxParserFirstInit();

	g_cxx.eCPPLanguage = language;

	cxxBuildKeywordHash(language,TRUE);
}

void cxxCParserInitialize(const langType language)
{
	CXX_DEBUG_INIT();

	CXX_DEBUG_PRINT("Parser initialize for language C");
	if(g_bFirstRun)
		cxxParserFirstInit();

	g_cxx.eCLanguage = language;

	cxxBuildKeywordHash(language,FALSE);
}

void cxxParserCleanup (langType language CTAGS_ATTR_UNUSED, boolean initialized CTAGS_ATTR_UNUSED)
{
	if(g_bFirstRun)
		return; // didn't run at all

	// This function is used as finalizer for both C and C++ parsers.
	// The next line forces this function to be called only once
	g_bFirstRun = TRUE;

	if(g_cxx.pTokenChain)
		cxxTokenChainDestroy(g_cxx.pTokenChain);
	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainDestroy(g_cxx.pTemplateTokenChain);

	cxxScopeDone();

	cxxTokenAPIDone();
}
