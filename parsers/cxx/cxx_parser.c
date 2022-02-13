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
#include "cxx_subparser_internal.h"

#include "parse.h"
#include "vstring.h"
#include "../cpreprocessor.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"
#include "ptrarray.h"
#include "trashbox.h"

#include <string.h>

//
// The global parser state
//
CXXParserState g_cxx;

//
// This is set to false once the parser is run at least one time.
// Used by cleanup routines.
//
bool g_bFirstRun = true;

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
		g_cxx.oTemplateParameters.uCount = 0;
	} else {
		// we don't care about stale specializations as they
		// are destroyed wen the base template prefix is extracted
	}
	g_cxx.uKeywordState = 0;

	// FIXME: this cpp handling of end/statement is kind of broken:
	//        it works only because the moon is in the correct phase.
	cppEndStatement();
}

//
// Parse a subchain of input delimited by matching pairs selected from
// [],(),{} and <>.
//
// On entry g_cxx.pToken is expected to point to the initial token of the pair,
// that is one of ([{<. The function will parse input until the matching
// terminator token is found. Inner parsing is done by
// cxxParserParseAndCondenseSubchainsUpToOneOf() so this is actually a recursive
// subchain nesting algorithm.
//
// Returns true if it has successfully extracted and "condensed" a subchain
// replacing the current token with a subchain subtree. Returns false if
// extraction fails for some reason.
//
// This function never leaves the token chain in an incoherent state.
// The current token is always replaced with a subchain tree. If the subchain
// is broken, its contents are discarded, regardless of the return value.
//
bool cxxParserParseAndCondenseCurrentSubchain(
		unsigned int uInitialSubchainMarkerTypes,
		bool bAcceptEOF,
		bool bCanReduceInnerElements
	)
{
	CXX_DEBUG_ENTER();

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
	enum CXXTokenType eTermType = (enum CXXTokenType)(g_cxx.pToken->eType << 4);

	unsigned int uTokenTypes = eTermType;
	if(bAcceptEOF)
		uTokenTypes |= CXXTokenTypeEOF;

	bool bRet = cxxParserParseAndCondenseSubchainsUpToOneOf(
			uTokenTypes,
			uInitialSubchainMarkerTypes,
			bCanReduceInnerElements
		);

	if(
		// Parsing the subchain failed: input is broken
		(!bRet) ||
		// Mismatched terminator (i.e EOF was accepted and encountered)
		(!cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),eTermType))
	)
	{
		// Input is probably broken: discard it in any case, so no one
		// is tempted to parse it later.

		CXX_DEBUG_PRINT(
				"Parsing the subchain failed or EOF found. Discarding broken subtree"
			);

		while(g_cxx.pTokenChain->iCount > 1)
			cxxTokenChainDestroyLast(g_cxx.pTokenChain);

		// Fake the terminator
		CXXToken * pFakeLast = cxxTokenCreate();
		pFakeLast->iLineNumber = pChainToken->iLineNumber;
		pFakeLast->oFilePosition = pChainToken->oFilePosition;
		switch(eTermType)
		{
			case CXXTokenTypeClosingBracket:
				vStringPut(pFakeLast->pszWord,'}');
			break;
			case CXXTokenTypeClosingParenthesis:
				vStringPut(pFakeLast->pszWord,')');
			break;
			case CXXTokenTypeClosingSquareParenthesis:
				vStringPut(pFakeLast->pszWord,']');
			break;
			case CXXTokenTypeGreaterThanSign:
				vStringPut(pFakeLast->pszWord,'>');
			break;
			default:
				CXX_DEBUG_ASSERT(false,"Unhandled terminator type");
			break;
		}
		pFakeLast->eType = eTermType;
		pFakeLast->pChain = NULL;

		cxxTokenChainAppend(g_cxx.pTokenChain,pFakeLast);
	}

	g_cxx.pTokenChain = pCurrentChain;
	g_cxx.pToken = pCurrentChain->pTail;

	CXX_DEBUG_LEAVE();
	return bRet;
}

//
// This function parses input until one of the specified tokens appears.
// The current token is NOT checked against the specified tokens.
//
// The algorithm will also build subchains of matching
// pairs ([...],(...),<...>,{...}): within the subchain analysis
// of uTokenTypes is completely disabled. Subchains do nest.
//
// Returns true if it stops before EOF or it stops at EOF and CXXTokenTypeEOF
// is present in uTokenTypes. Returns false in all the other stop conditions
// and when an unmatched subchain character pair is found (syntax error).
//
bool cxxParserParseAndCondenseSubchainsUpToOneOf(
		unsigned int uTokenTypes,
		unsigned int uInitialSubchainMarkerTypes,
		bool bCanReduceInnerElements
	)
{
	CXX_DEBUG_ENTER_TEXT("Token types = 0x%x(%s), reduce = %d", uTokenTypes, cxxDebugTypeDecode(uTokenTypes),
						 bCanReduceInnerElements);

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
			if (bCanReduceInnerElements)
				cxxTokenReduceBackward (g_cxx.pToken);
			CXX_DEBUG_LEAVE_TEXT(
					"Got terminator token '%s' 0x%x",
					vStringValue(g_cxx.pToken->pszWord),
					g_cxx.pToken->eType
				);
			return true;
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
					return false;
				}
			} else {
				g_cxx.iNestingLevels++;

				if(g_cxx.iNestingLevels > CXX_PARSER_MAXIMUM_NESTING_LEVELS)
				{
					CXX_DEBUG_LEAVE_TEXT("Nesting level grown too much: something nasty is going on");
					return false;
				}

				bool bRet = cxxParserParseAndCondenseCurrentSubchain(
						uInitialSubchainMarkerTypes,
						(uTokenTypes & CXXTokenTypeEOF),
						bCanReduceInnerElements
					);

				g_cxx.iNestingLevels--;

				if(!bRet)
				{
					CXX_DEBUG_LEAVE_TEXT(
							"Failed to parse subchain of type 0x%x",
							g_cxx.pToken->eType
						);
					return false;
				}
			}

			if(cxxTokenTypeIsOneOf(g_cxx.pToken,uTokenTypes))
			{
				// was looking for a subchain
				CXX_DEBUG_LEAVE_TEXT(
						"Got terminator subchain token 0x%x",
						g_cxx.pToken->eType
					);
				return true;
			}

			if(!cxxParserParseNextToken())
			{
				CXX_DEBUG_LEAVE_TEXT("Found EOF(2)");
				return (uTokenTypes & CXXTokenTypeEOF); // was already at EOF
			}

			continue; // jump up to avoid checking for mismatched pairs below
		}

		// Check for mismatched brackets/parentheses
		// Note that if we were looking for one of [({ then we would have matched
		// it at the top of the for
		if(cxxTokenTypeIsOneOf(g_cxx.pToken,uFinalSubchainMarkerTypes))
		{
			CXX_DEBUG_LEAVE_TEXT(
					"Got mismatched subchain terminator 0x%x",
					g_cxx.pToken->eType
				);
			return false; // unmatched: syntax error
		}

		if(!cxxParserParseNextToken())
		{
			CXX_DEBUG_LEAVE_TEXT("Found EOF(3)");
			return (uTokenTypes & CXXTokenTypeEOF); // was already at EOF
		}
	}

	// not reached
	CXX_DEBUG_LEAVE_TEXT("Internal error");
	return false;
}

//
// This function parses input until one of the specified tokens appears.
// The current token is NOT checked against the specified tokens.
//
// The algorithm will also build subchains of matching pairs ([...],(...),{...}).
// Within the subchain analysis of uTokenTypes is completely disabled.
// Subchains do nest.
//
// Please note that this function will skip entire scopes (matching {} pairs)
// unless you pass CXXTokenTypeOpeningBracket to stop at their beginning.
// This is usually what you want, unless you're really expecting a scope to begin
// in the current statement.
//
bool cxxParserParseUpToOneOf(unsigned int uTokenTypes,
							 bool bCanReduceInnerElements)
{
	return cxxParserParseAndCondenseSubchainsUpToOneOf(
			uTokenTypes,
			CXXTokenTypeOpeningBracket |
				CXXTokenTypeOpeningParenthesis |
				CXXTokenTypeOpeningSquareParenthesis,
			bCanReduceInnerElements
		);
}

//
// Attempts to skip to either a semicolon or an EOF, ignoring anything in between.
// May be also used to recovery from certain forms of syntax errors.
// This function works also if the current token is a semicolon or an EOF.
//
bool cxxParserSkipToSemicolonOrEOF(void)
{
	if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeSemicolon | CXXTokenTypeEOF))
		return true;

	return cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF,
								   false);
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
bool cxxParserParseToEndOfQualifedName(void)
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
			return false; // EOF
		}
	}

	while(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeMultipleColons))
	{
		if(!cxxParserParseNextToken())
		{
			// syntax error, but we tolerate this
			CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
			return false; // EOF
		}

		if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier))
		{
			CXX_DEBUG_LEAVE_TEXT("Found no identifier after multiple colons");
			return false;
		}

		if(!cxxParserParseNextToken())
		{
			// syntax error, but we tolerate this
			CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
			return false; // EOF
		}
	}

	CXX_DEBUG_ASSERT(g_cxx.pToken->pPrev,"There should be a previous token here");
	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(g_cxx.pToken->pPrev,CXXTokenTypeIdentifier),
			"The qualified name should end with an identifier"
		);

	CXX_DEBUG_LEAVE();
	return true;
}

void cxxParserSetEndLineForTagInCorkQueue(int iCorkQueueIndex,unsigned long lEndLine)
{
	CXX_DEBUG_ASSERT(iCorkQueueIndex > CORK_NIL,"The cork queue index is not valid");

	tagEntryInfo * tag = getEntryInCorkQueue (iCorkQueueIndex);

	CXX_DEBUG_ASSERT(tag,"No tag entry in the cork queue");

	tag->extensionFields.endLine = lEndLine;
}

//
// Attach the current position of input file as "end" field of
// the specified tag in the cork queue
//
void cxxParserMarkEndLineForTagInCorkQueue(int iCorkQueueIndex)
{
	cxxParserSetEndLineForTagInCorkQueue(iCorkQueueIndex,getInputLineNumber());
}


// Make sure that the token chain contains only the specified keyword and eventually
// the "const" or "volatile" type modifiers.
static void cxxParserCleanupEnumStructClassOrUnionPrefixChain(CXXKeyword eKeyword,CXXToken * pLastToken)
{
	CXXToken * pToken = cxxTokenChainFirst(g_cxx.pTokenChain);
	while(pToken && (pToken != pLastToken))
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
static bool cxxParserParseEnumStructClassOrUnionFullDeclarationTrailer(
		unsigned int uKeywordState,
		CXXKeyword eTagKeyword,
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

	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeEOF | CXXTokenTypeSemicolon |
				CXXTokenTypeOpeningBracket | CXXTokenTypeAssignment,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to EOF/semicolon");
		return false;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
	{
		// It's a syntax error, but we can be tolerant here.
		CXX_DEBUG_LEAVE_TEXT("Got EOF after enum/class/struct/union block");
		return true;
	}

	if(g_cxx.pTokenChain->iCount < 2)
	{
		CXX_DEBUG_LEAVE_TEXT("Nothing interesting after enum/class/struct block");
		return true;
	}

	// fake the initial two tokens
	CXXToken * pIdentifier = cxxTokenCreate();
	pIdentifier->oFilePosition = oFilePosition;
	pIdentifier->iLineNumber = iFileLine;
	pIdentifier->eType = CXXTokenTypeIdentifier;
	pIdentifier->bFollowedBySpace = true;
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

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeOpeningBracket))
	{
		CXX_DEBUG_PRINT("Found opening bracket: possibly a function declaration?");
		if(!cxxParserParseBlockHandleOpeningBracket())
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to handle the opening bracket");
			return false;
		}
		CXX_DEBUG_LEAVE_TEXT("Opening bracket handled");
		return true;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeAssignment))
	{
		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeEOF | CXXTokenTypeSemicolon,
				false
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse up to EOF/semicolon");
			return false;
		}
	}

	if(uKeywordState & CXXParserKeywordStateSeenTypedef)
		cxxParserExtractTypedef(g_cxx.pTokenChain,true,false);
	else
		cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);

	CXX_DEBUG_LEAVE();
	return true;
}

bool cxxParserParseEnum(void)
{
	CXX_DEBUG_ENTER();

	unsigned int uInitialKeywordState = g_cxx.uKeywordState;
	int iInitialTokenCount = g_cxx.pTokenChain->iCount;
	CXXToken * pLastToken = cxxTokenChainLast(g_cxx.pTokenChain);

	/*
		Spec is:
			enum-key attr(optional) identifier(optional) enum-base(optional)
				{ enumerator-list(optional) }	(1)
			enum-key attr(optional) identifier enum-base(optional) ;
				(2)	(since C++11)

			enum-key	-	one of enum, enum class(since C++11), or enum struct(since C++11)
			attr(C++11)	-	optional sequence of any number of attributes
			identifier	-	the name of the enumeration that's being declared.
				If present, and if this declaration is a re-declaration, it may be preceded by
				nested-name-specifier(since C++11): sequence of names and scope-resolution
				operators ::, ending with scope-resolution operator. The name can be omitted
				only in unscoped enumeration declarations

			enum-base(C++11)	-	colon (:), followed by a type-specifier-seq that names an
				integral type (if it is cv-qualified, qualifications are ignored)
			enumerator-list	-	comma-separated list of enumerator definitions, each of which is
				either simply an identifier, which becomes the name of the enumerator, or an
				identifier with an initializer: identifier = constexpr. In either case, the
				identifier can be directly followed by an optional attribute specifier
				sequence. (since C++17)
	*/

	// Skip attr and class-head-name
	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeEOF | CXXTokenTypeSemicolon | CXXTokenTypeKeyword |
				CXXTokenTypeSingleColon | CXXTokenTypeParenthesisChain |
				CXXTokenTypeOpeningBracket,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Could not parse enum name");
		return false;
	}

	bool bIsScopedEnum = false; // c++11 scoped enum (enum class | enum struct)

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeKeyword))
	{
		// enum class | enum struct ?
		if(
			(g_cxx.pToken->eKeyword == CXXKeywordSTRUCT) ||
			(g_cxx.pToken->eKeyword == CXXKeywordCLASS)
		)
		{
			bIsScopedEnum = true;
		}

		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeEOF | CXXTokenTypeSemicolon | CXXTokenTypeSingleColon |
				CXXTokenTypeParenthesisChain | CXXTokenTypeOpeningBracket,
				false
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Could not parse enum name");
			return false;
		}
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
	{
		// tolerate EOF, treat as forward declaration
		cxxParserNewStatement();
		CXX_DEBUG_LEAVE_TEXT("EOF before enum block: treating as forward declaration");
		return true;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain))
	{
		if(uInitialKeywordState & CXXParserKeywordStateSeenTypedef)
		{
			CXX_DEBUG_LEAVE_TEXT("Found parenthesis after typedef: parsing as generic typedef");
			return cxxParserParseGenericTypedef();
		}
		// probably a function declaration/prototype
		// something like enum x func()....
		// do not clear statement
		CXX_DEBUG_LEAVE_TEXT("Probably a function declaration!");
		return true;
	}

	// If we have found a semicolon then we might be in the special case of KnR function
	// declaration. This requires at least 5 tokens and has some additional constraints.
	// See cxxParserMaybeParseKnRStyleFunctionDefinition() for more informations.
	if(
			// found semicolon
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon) &&
			// many tokens before the enum keyword
			(iInitialTokenCount > 3) &&
			// C language
			cxxParserCurrentLanguageIsC() &&
			// global scope
			cxxScopeIsGlobal() &&
			// no typedef
			(!(uInitialKeywordState & CXXParserKeywordStateSeenTypedef))
		)
	{
		CXX_DEBUG_PRINT("Maybe KnR function definition");

		switch(cxxParserMaybeParseKnRStyleFunctionDefinition())
		{
			case 1:
				// parser moved forward and started a new statement
				CXX_DEBUG_LEAVE_TEXT("K&R parser did the job");
				return true;
			break;
			case 0:
				// something else, go ahead
			break;
			default:
				CXX_DEBUG_LEAVE_TEXT("Failed to check for K&R style function definition");
				return false;
			break;
		}
	}

	if(iInitialTokenCount > 1)
		cxxParserCleanupEnumStructClassOrUnionPrefixChain(CXXKeywordENUM,pLastToken);

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_PRINT("Found semicolon, maybe typedef or variable declaration");

		// scoped enums can't be used to declare variables.
		if((!bIsScopedEnum) && (g_cxx.pTokenChain->iCount > 3))
		{
			 // [typedef] enum X Y; <-- typedef has been removed!
			if(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef)
				cxxParserExtractTypedef(g_cxx.pTokenChain,true,false);
			else
				cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);
		}

		cxxParserNewStatement();
		CXX_DEBUG_LEAVE();
		return true;
	}

	// colon or opening bracket
	CXX_DEBUG_ASSERT(
			cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeSingleColon | CXXTokenTypeOpeningBracket),
			"We should be pointing to a : or a {"
		);

	// check if we can extract a class name identifier now
	CXXToken * pEnumName = cxxTokenChainLastTokenOfType(
			g_cxx.pTokenChain,
			CXXTokenTypeIdentifier
		);

	CXXToken * pTypeBegin; // no need to NULLify, only pTypeEnd matters.
	CXXToken * pTypeEnd = NULL;

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSingleColon))
	{
		// skip type
		CXX_DEBUG_PRINT("Single colon, trying to skip type");

		pTypeBegin = g_cxx.pToken;

		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeEOF | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket,
				false))
		{
			CXX_DEBUG_LEAVE_TEXT("Could not parse enum type");
			return false;
		}

		if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
		{
			// tolerate EOF, treat as forward declaration
			cxxParserNewStatement();
			CXX_DEBUG_LEAVE_TEXT("EOF before enum block: can't decode this");
			return true;
		}

		if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
		{
			bool bMember = (cxxScopeGetVariableKind() == CXXTagKindMEMBER);
			if (bMember)
			{
				// enum type structure member with bit-width:
				// e.g.
				//    sturct { enum E m: 2; } v;
				CXX_DEBUG_PRINT("Found semicolon, member definition with bit-width");
				cxxParserExtractVariableDeclarations(g_cxx.pTokenChain, 0);
			}
			cxxParserNewStatement();
			if (bMember)
				CXX_DEBUG_LEAVE_TEXT("Semicolon before enum block: can't decode this");
			else
				CXX_DEBUG_LEAVE();
			return true;
		}

		// certainly opening bracket now.
		if(g_cxx.pToken->pPrev != pTypeBegin)
		{
			// there were tokens between the semicolon and the type begin
			pTypeBegin = pTypeBegin->pNext;
			pTypeEnd = g_cxx.pToken->pPrev;
		}
	}


	int iPushedScopes = 0;
	bool bAnonymous = false;

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
				if(cxxParserCurrentLanguageIsCPP())
				{
					// FIXME: We don't really know if it's a class!
					cxxScopePush(pNamespaceBegin,CXXScopeTypeClass,CXXScopeAccessUnknown);
				} else {
					// it's a syntax error, but be tolerant
				}
				iPushedScopes++;
				pNamespaceBegin = pNext->pNext;
			}
		}

		CXX_DEBUG_PRINT("Enum name is %s",vStringValue(pEnumName->pszWord));
		cxxTokenChainTake(g_cxx.pTokenChain,pEnumName);
	} else {
		pEnumName = cxxTokenCreateAnonymousIdentifier(CXXTagKindENUM);
		bAnonymous = true;
		CXX_DEBUG_PRINT(
				"Enum name is %s (anonymous)",
				vStringValue(pEnumName->pszWord)
			);
	}


	tagEntryInfo * tag = cxxTagBegin(CXXTagKindENUM,pEnumName);

	int iCorkQueueIndex = CORK_NIL;
	int iCorkQueueIndexFQ = CORK_NIL;

	if(tag)
	{
		// FIXME: this is debatable
		tag->isFileScope = !isInputHeaderFile();

		if (bAnonymous)
			markTagExtraBit (tag, XTAG_ANONYMOUS);

		CXXToken * pTypeName = NULL;
		vString * pszProperties = NULL;

		if(pTypeEnd)
		{
			CXX_DEBUG_ASSERT(pTypeBegin,"Type begin should be also set here");
			pTypeName = cxxTagCheckAndSetTypeField(pTypeBegin,pTypeEnd);
		}

		if(bIsScopedEnum)
			pszProperties = cxxTagSetProperties(CXXTagPropertyScopedEnum);

		iCorkQueueIndex = cxxTagCommit(&iCorkQueueIndexFQ);

		if (pszProperties)
			vStringDelete (pszProperties);

		if(pTypeName)
			cxxTokenDestroy(pTypeName);
	}

	cxxScopePush(pEnumName,CXXScopeTypeEnum,CXXScopeAccessPublic);
	iPushedScopes++;

	vString * pScopeName = cxxScopeGetFullNameAsString();

	// Special kind of block
	for(;;)
	{
		cxxTokenChainClear(g_cxx.pTokenChain);

		if(!cxxParserParseUpToOneOf(
				CXXTokenTypeComma | CXXTokenTypeClosingBracket | CXXTokenTypeEOF,
				false
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse enum contents");
			if(pScopeName)
				vStringDelete(pScopeName);
			return false;
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
				cxxTagCommit(NULL);
			}
		}

		if(cxxTokenTypeIsOneOf(
				g_cxx.pToken,
				CXXTokenTypeEOF | CXXTokenTypeClosingBracket
			))
			break;
	}

	if(iCorkQueueIndex > CORK_NIL)
	{
		cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndex);
		if(iCorkQueueIndexFQ > CORK_NIL)
			cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndexFQ);
	}

	while(iPushedScopes > 0)
	{
		cxxScopePop();
		iPushedScopes--;
	}

	bool bRet = cxxParserParseEnumStructClassOrUnionFullDeclarationTrailer(
			uInitialKeywordState,
			CXXKeywordENUM,
			vStringValue(pScopeName)
		);

	if(pScopeName)
		vStringDelete(pScopeName);

	cxxParserNewStatement();
	CXX_DEBUG_LEAVE();
	return bRet;
}

static bool cxxParserParseClassStructOrUnionInternal(
		CXXKeyword eKeyword,
		unsigned int uTagKind,
		unsigned int uScopeType
	)
{
	CXX_DEBUG_ENTER();

	unsigned int uInitialKeywordState = g_cxx.uKeywordState;
	int iInitialTokenCount = g_cxx.pTokenChain->iCount;
	CXXToken * pLastToken = cxxTokenChainLast(g_cxx.pTokenChain);
	bool bAnonymous = false;

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
	cxxKeywordEnableFinal(true);

	unsigned int uTerminatorTypes = CXXTokenTypeEOF | CXXTokenTypeSingleColon |
			CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket |
			CXXTokenTypeSmallerThanSign | (cxxParserCurrentLanguageIsCPP()? CXXTokenTypeKeyword: 0) |
			CXXTokenTypeParenthesisChain;

	if(uTagKind != CXXTagCPPKindCLASS)
		uTerminatorTypes |= CXXTokenTypeAssignment;

	bool bRet;

	for(;;)
	{
		bRet = cxxParserParseUpToOneOf(uTerminatorTypes, false);

		if(!bRet)
		{
			cxxKeywordEnableFinal(false);
			CXX_DEBUG_LEAVE_TEXT("Could not parse class/struct/union name");
			return false;
		}

		if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeKeyword))
		{
			/* The statement declears or defines an operator,
			 * not a class, struct not union. */
			if(g_cxx.pToken->eKeyword == CXXKeywordOPERATOR)
				return true;
			continue;
		}

		if(
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain) &&
			(
				(
					// struct alignas(n) ...
					cxxTokenIsKeyword(g_cxx.pToken->pPrev,CXXKeywordALIGNAS)
				) || (
					// things like __builtin_align__(16)
					!cxxParserTokenChainLooksLikeFunctionParameterList(g_cxx.pToken->pChain,NULL)
				)
			)
		)
			continue;

		if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSmallerThanSign))
			break;

		// Probably a template specialisation
		if(!cxxParserCurrentLanguageIsCPP())
		{
			cxxKeywordEnableFinal(false);
			CXX_DEBUG_LEAVE_TEXT("Template specialization in C language?");
			return false;
		}

		// template<typename T> struct X<int>
		// {
		// }

		if(g_cxx.pTemplateSpecializationTokenChain)
			cxxTokenChainDestroy(g_cxx.pTemplateSpecializationTokenChain);

		g_cxx.pTemplateSpecializationTokenChain = cxxParserParseTemplateAngleBracketsToSeparateChain(false);
		if(!g_cxx.pTemplateSpecializationTokenChain)
		{
			cxxKeywordEnableFinal(false);
			CXX_DEBUG_LEAVE_TEXT("Could not parse class/struct/union name");
			return false;
		}
	}

	// Once we reached the terminator, "final" is not a keyword anymore.
	cxxKeywordEnableFinal(false);

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain))
	{
		if(uInitialKeywordState & CXXParserKeywordStateSeenTypedef)
		{
			CXX_DEBUG_LEAVE_TEXT("Found parenthesis after typedef: parsing as generic typedef");
			return cxxParserParseGenericTypedef();
		}

		// probably a function declaration/prototype
		// something like struct x * func()....
		// do not clear statement
		CXX_DEBUG_LEAVE_TEXT("Probably a function declaration!");
		return true;
	}

	// If we have found a semicolon then we might be in the special case of KnR function
	// declaration. This requires at least 5 tokens and has some additional constraints.
	// See cxxParserMaybeParseKnRStyleFunctionDefinition() for more informations.
	// FIXME: This block is duplicated in enum
	if(
			// found semicolon
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon) &&
			// many tokens before the enum keyword
			(iInitialTokenCount > 3) &&
			// C language
			cxxParserCurrentLanguageIsC() &&
			// global scope
			cxxScopeIsGlobal() &&
			// no typedef
			(!(uInitialKeywordState & CXXParserKeywordStateSeenTypedef))
		)
	{
		CXX_DEBUG_PRINT("Maybe KnR function definition?");

		switch(cxxParserMaybeParseKnRStyleFunctionDefinition())
		{
			case 1:
				// parser moved forward and started a new statement
				CXX_DEBUG_LEAVE_TEXT("K&R function definition parser did the job");
				return true;
			break;
			case 0:
				// something else, go ahead
			break;
			default:
				CXX_DEBUG_LEAVE_TEXT("Failed to check for K&R style function definition");
				return false;
			break;
		}
	}

	if(iInitialTokenCount > 1)
		cxxParserCleanupEnumStructClassOrUnionPrefixChain(eKeyword,pLastToken);

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
	{
		if(g_cxx.pTokenChain->iCount > 3)
		{
			// [typedef] struct X Y; <-- typedef has been removed!
			if(uInitialKeywordState & CXXParserKeywordStateSeenTypedef)
				cxxParserExtractTypedef(g_cxx.pTokenChain,true,false);
			else if(!(g_cxx.uKeywordState & CXXParserKeywordStateSeenFriend))
				cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);
		}

		cxxParserNewStatement();
		CXX_DEBUG_LEAVE();
		return true;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeAssignment))
	{
		// struct X Y = ...;
		bool bCanExtractVariables = g_cxx.pTokenChain->iCount > 3;

		// Skip the initialization (which almost certainly contains a block)
		if(!cxxParserParseUpToOneOf(CXXTokenTypeEOF | CXXTokenTypeSemicolon, false))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse up to EOF/semicolon");
			return false;
		}

		if(bCanExtractVariables)
			cxxParserExtractVariableDeclarations(g_cxx.pTokenChain,0);

		cxxParserNewStatement();
		CXX_DEBUG_LEAVE();
		return true;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
	{
		// tolerate EOF, just ignore this
		cxxParserNewStatement();
		CXX_DEBUG_LEAVE_TEXT("EOF: ignoring");
		return true;
	}

	// semicolon or opening bracket

	// check if we can extract a class name identifier
	CXXToken * pClassName = cxxTokenChainLastTokenOfType(
			g_cxx.pTokenChain,
			CXXTokenTypeIdentifier
		);

	// If no identifier has been found we can try some fallbacks.
	if(!pClassName)
	{
		// If we're in C++ mode, but C++ language hasn't been confirmed yet,
		// and there is a C++ specific keyword just before the terminator we found
		// then we'll try to use it as class/struct/union name.
		if(
			cxxParserCurrentLanguageIsCPP() &&
			(!g_cxx.bConfirmedCPPLanguage) &&
			(eKeyword != CXXKeywordCLASS) &&
			(g_cxx.pTokenChain->iCount >= 3) &&
			cxxTokenTypeIs(g_cxx.pToken->pPrev,CXXTokenTypeKeyword) &&
			cxxKeywordIsCPPSpecific(g_cxx.pToken->pPrev->eKeyword)
		)
		{
			pClassName = g_cxx.pToken->pPrev;
			pClassName->eType = CXXTokenTypeIdentifier;
			CXX_DEBUG_PRINT(
					"Found no class/struct/union name identifier but there is '%s' which might look good",
					vStringValue(pClassName->pszWord)
				);
		}
	}

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
			if(cxxParserCurrentLanguageIsCPP())
			{
				// FIXME: We don't really know if it's a class!
				cxxScopePush(pNamespaceBegin,CXXScopeTypeClass,CXXScopeAccessUnknown);
				iPushedScopes++;
			} else {
				// it's a syntax error, but be tolerant
				cxxTokenDestroy(pNamespaceBegin);
			}
			pNamespaceBegin = pNext->pNext;
		}

		CXX_DEBUG_PRINT(
				"Class/struct/union name is %s",
				vStringValue(pClassName->pszWord)
			);
		cxxTokenChainTake(g_cxx.pTokenChain,pClassName);
	} else {
		pClassName = cxxTokenCreateAnonymousIdentifier(uTagKind);
		bAnonymous = true;
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
				CXXTokenTypeEOF | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket,
				false
			))
		{
			cxxTokenDestroy(pClassName);
			CXX_DEBUG_LEAVE_TEXT("Failed to parse base class part");
			return false;
		}

		if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeSemicolon | CXXTokenTypeEOF))
		{
			cxxTokenDestroy(pClassName);
			cxxParserNewStatement();
			CXX_DEBUG_LEAVE_TEXT("Syntax error: ignoring");
			return true;
		}

		cxxTokenChainDestroyLast(g_cxx.pTokenChain); // remove the {
	} else {
		cxxTokenChainClear(g_cxx.pTokenChain);
	}

	// OK. This seems to be a valid class/struct/union declaration.

	if(
			(uTagKind == CXXTagCPPKindCLASS) &&
			(!g_cxx.bConfirmedCPPLanguage)
		)
	{
		CXX_DEBUG_PRINT("Succeeded in parsing a C++ class: this really seems to be C++");
		g_cxx.bConfirmedCPPLanguage = true;
	}

	tagEntryInfo * tag = cxxTagBegin(uTagKind,pClassName);

	int iCorkQueueIndex = CORK_NIL;
	int iCorkQueueIndexFQ = CORK_NIL;

	bool bGotTemplate = g_cxx.pTemplateTokenChain &&
			(g_cxx.pTemplateTokenChain->iCount > 0) &&
			cxxParserCurrentLanguageIsCPP();

	if(tag)
	{
		if (bAnonymous)
			markTagExtraBit (tag, XTAG_ANONYMOUS);

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

		if(bGotTemplate)
			cxxTagHandleTemplateFields();

		tag->isFileScope = !isInputHeaderFile();

		iCorkQueueIndex = cxxTagCommit(&iCorkQueueIndexFQ);

	}

	cxxScopePush(
			pClassName,
			uScopeType,
			(uTagKind == CXXTagCPPKindCLASS) ?
				CXXScopeAccessPrivate : CXXScopeAccessPublic
		);

	if(
			bGotTemplate &&
			cxxTagKindEnabled(CXXTagCPPKindTEMPLATEPARAM)
		)
		cxxParserEmitTemplateParameterTags();

	vString * pScopeName = cxxScopeGetFullNameAsString();

	if(!cxxParserParseBlock(true))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse scope");
		if(pScopeName)
			vStringDelete(pScopeName);
		return false;
	}

	if(iCorkQueueIndex > CORK_NIL)
	{
		cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndex);
		if(iCorkQueueIndexFQ > CORK_NIL)
			cxxParserMarkEndLineForTagInCorkQueue(iCorkQueueIndexFQ);
	}

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

bool cxxParserParseClassStructOrUnion(
		CXXKeyword eKeyword,
		unsigned int uTagKind,
		unsigned int uScopeType
	)
{
	// Trick for "smart" handling of public/protected/private keywords in .h files parsed as C++.
	// See the declaration of cxxKeywordEnablePublicProtectedPrivate for more info.

	// Enable public/protected/private keywords and save the previous state
	bool bEnablePublicProtectedPrivateKeywords = cxxKeywordEnablePublicProtectedPrivate(true);

	bool bRet = cxxParserParseClassStructOrUnionInternal(eKeyword,uTagKind,uScopeType);

	// If parsing succeeded, we're in C++ mode and the keyword is "class" then
	// we're fairly certain that the source code is *really* C++.
	if(g_cxx.bConfirmedCPPLanguage)
		bEnablePublicProtectedPrivateKeywords = true; // leave it on for good: we're (almost) sure it's C++

	cxxKeywordEnablePublicProtectedPrivate(bEnablePublicProtectedPrivateKeywords);

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
	CXXTypedVariableSet oParamInfo;
	const bool bPrototypeParams = cxxTagKindEnabled(CXXTagKindPROTOTYPE) && cxxTagKindEnabled(CXXTagKindPARAMETER);
check_function_signature:

	if(
		cxxParserLookForFunctionSignature(g_cxx.pTokenChain,&oInfo,bPrototypeParams?&oParamInfo:NULL)
		// Even if we saw "();", we cannot say it is a function prototype; a macro expansion can be used to
		// initialize a top-level variable like:
		//   #define INIT() 0
		//   int i = INIT();"
		&& ! (oInfo.pTypeEnd && cxxTokenTypeIs(oInfo.pTypeEnd,CXXTokenTypeAssignment))
		)
	{
		CXX_DEBUG_PRINT("Found function prototype");

		if(g_cxx.uKeywordState & CXXParserKeywordStateSeenFriend)
		{
			// class X {
			//   friend void aFunction();
			// };
			// 'aFunction' is NOT X::aFunction() and in complex cases we can't figure
			// out its proper scope. Better avoid emitting this one.
			CXX_DEBUG_PRINT("But it has been preceded by the 'friend' keyword: this is not a real prototype");
		} else {
			int iCorkQueueIndex, iCorkQueueIndexFQ;
			int iScopesPushed = cxxParserEmitFunctionTags(&oInfo,CXXTagKindPROTOTYPE,CXXEmitFunctionTagsPushScopes,&iCorkQueueIndex,&iCorkQueueIndexFQ);
			if (iCorkQueueIndex != CORK_NIL)
			{
				CXXToken * t = cxxTokenChainLast(g_cxx.pTokenChain);
				cxxParserSetEndLineForTagInCorkQueue (iCorkQueueIndex, t->iLineNumber);
				if (iCorkQueueIndexFQ != CORK_NIL)
					cxxParserSetEndLineForTagInCorkQueue (iCorkQueueIndexFQ, t->iLineNumber);
			}

			if(bPrototypeParams)
				cxxParserEmitFunctionParameterTags(&oParamInfo);

			while(iScopesPushed > 0)
			{
				cxxScopePop();
				iScopesPushed--;
			}
		}

		if(oInfo.pTrailingComma)
		{
			// got a trailing comma after the function signature.
			// This might be a special case of multiple prototypes in a single declaration.
			//
			//   RetType functionA(...), functionB(...), functionC(...);
			//
			// Let's try to extract also the other declarations.
			//
			// We cannot rely on oInfo.pIdentifierStart after cxxParserEmitFunctionTags()
			// since it has been removed. Manually skip the initial type name.

			CXXToken * pBegin = cxxTokenChainFirstTokenNotOfType(
					g_cxx.pTokenChain,
					CXXTokenTypeIdentifier | CXXTokenTypeKeyword
				);

			CXX_DEBUG_ASSERT(pBegin,"We should have found a begin token here!");
			cxxTokenChainDestroyRange(g_cxx.pTokenChain,pBegin,oInfo.pTrailingComma);
			goto check_function_signature;
		}

		CXX_DEBUG_LEAVE();
		return;
	}

	if(
		g_cxx.uKeywordState &
		(
			// Note that since C++-17 inline can be used as a modifier for variables
			// so don't be tempted to put it here.
			CXXParserKeywordStateSeenExplicit |
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
bool cxxParserParseAccessSpecifier(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeKeyword) &&
			(
				(g_cxx.pToken->eKeyword == CXXKeywordPUBLIC) ||
				(g_cxx.pToken->eKeyword == CXXKeywordPROTECTED) ||
				(g_cxx.pToken->eKeyword == CXXKeywordPRIVATE)
			),
			"This must be called just after parsing public/protected/private"
		);

	unsigned int uExtraType = 0;

	enum CXXScopeType eScopeType = cxxScopeGetType();

	static ptrArray *pSubparsers;
	if (!pSubparsers)
	{
		pSubparsers = ptrArrayNew(NULL);
		DEFAULT_TRASH_BOX (pSubparsers, ptrArrayDelete);
	}

	if(
			(eScopeType != CXXScopeTypeClass) &&
			(eScopeType != CXXScopeTypeUnion) &&
			(eScopeType != CXXScopeTypeStruct)
		)
	{
		CXX_DEBUG_LEAVE_TEXT(
				"Access specified in wrong context (%d)",
				eScopeType
			);

		if(!g_cxx.bConfirmedCPPLanguage)
		{
			CXX_DEBUG_LEAVE_TEXT("C++ is not confirmed and the scope is not right: likely not access specifier");
			g_cxx.pToken->eType = CXXTokenTypeIdentifier;
			return true;
		}

		// this is a syntax error: we're in the wrong scope.
		CXX_DEBUG_LEAVE_TEXT("C++ language is confirmed: bailing out to avoid reporting broken structure");
		return false;
	}

	if(!g_cxx.bConfirmedCPPLanguage)
	{
		if(g_cxx.pToken->pPrev)
		{
			// ugly, there is something before the public/private/protected keyword.
			// This is likely a type or something else.
			CXX_DEBUG_LEAVE_TEXT(
					"C++ is not confirmed and there is something before: likely not access specifier"
				);
			g_cxx.pToken->eType = CXXTokenTypeIdentifier;
			return true;
		}
	}

	if (cxxSubparserNotifyParseAccessSpecifier (pSubparsers))
		uExtraType = CXXTokenTypeIdentifier;

	CXXToken * pInitialToken = g_cxx.pToken;

	// skip to the next :, without leaving scope.
 findColon:
	if(!cxxParserParseUpToOneOf(
		    uExtraType |
			CXXTokenTypeSingleColon | CXXTokenTypeSemicolon |
				CXXTokenTypeClosingBracket | CXXTokenTypeEOF,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to the next ;");
		ptrArrayClear (pSubparsers);
		return false;
	}

	if (uExtraType && cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier))
	{
		cxxSubparserNotifyfoundExtraIdentifierAsAccessSpecifier (pSubparsers,
																 g_cxx.pToken);
		goto findColon;
	}

	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSingleColon))
	{
		if(!pInitialToken->pPrev)
		{
			CXX_DEBUG_PRINT("The access specifier was the first token and I have found a colon: this is C++");
			g_cxx.bConfirmedCPPLanguage = true;
		}
	}

	switch(pInitialToken->eKeyword)
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
			CXX_DEBUG_ASSERT(false,"Bad keyword in cxxParserParseAccessSpecifier!");
		break;
	}

	cxxTokenChainClear(g_cxx.pTokenChain);
	ptrArrayClear (pSubparsers);
	CXX_DEBUG_LEAVE();
	return true;
}

bool cxxParserParseIfForWhileSwitchCatchParenthesis(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeKeyword),
			"This function should be called only after encountering one of the keywords"
		);

	CXXKeyword eKeyword = g_cxx.pToken->eKeyword;

	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeParenthesisChain | CXXTokenTypeSemicolon |
				CXXTokenTypeOpeningBracket | CXXTokenTypeEOF,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse if/for/while/switch/catch up to parenthesis");
		return false;
	}

	if(cxxTokenTypeIsOneOf(
			g_cxx.pToken,
			CXXTokenTypeEOF | CXXTokenTypeSemicolon | CXXTokenTypeOpeningBracket
		))
	{
		CXX_DEBUG_LEAVE_TEXT(
				"Found EOF/semicolon/opening bracket while parsing if/for/while/switch/catch"
			);
		return true;
	}

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain),
			"Expected a parenthesis chain here"
		);

	CXX_DEBUG_PRINT("Found if/for/while/switch/catch parenthesis chain");

	// Extract variables from the parenthesis chain

	CXXTokenChain * pChain = g_cxx.pToken->pChain;

	CXX_DEBUG_ASSERT(
			pChain->iCount >= 2,
			"The parenthesis chain must have initial and final parenthesis"
		);

	// There are several constructs that can fool the parser here.
	//
	// The most frequent problems arise with
	//
	//     if(a & b ...)
	//     if(a * b ...)
	//     if(a && b ...)
	//
	// which may or may not be variable declarations, depending on the
	// meaning of identifier a.
	// Other problems involve balanced operator that resemble templates:
	//
	//     if(a < b || c > d ...)
	//
	// Here we attempt to rule out these special cases.

	// First try the easy "inclusive" cases.

	// catch() always contains variable declarations

	bool bOkToExtractVariables = eKeyword == CXXKeywordCATCH;

	if(!bOkToExtractVariables)
	{
		// Another easy one: try parenthesis contents that start with a keyword.
		//
		//   if(const std::exception & e)
		//   if(int i ...
		//
		bOkToExtractVariables = cxxTokenTypeIs(
				cxxTokenChainAt(pChain,1),
				CXXTokenTypeKeyword
			);

		if(!bOkToExtractVariables)
		{
			// If there is &, && or * then we expect there to be also a = or
			// a semicolon that comes after it.
			// This is not 100% foolproof but works most of the times.

			CXXToken * pToken = cxxTokenChainFirstTokenOfType(
					pChain,
					CXXTokenTypeAnd | CXXTokenTypeMultipleAnds | CXXTokenTypeStar |
					CXXTokenTypeSmallerThanSign |
					CXXTokenTypeAssignment | CXXTokenTypeSemicolon
				);

			if(pToken)
			{
				switch(pToken->eType)
				{
					case CXXTokenTypeAnd:
					case CXXTokenTypeMultipleAnds:
					case CXXTokenTypeStar:
					case CXXTokenTypeSmallerThanSign:
						// troublesome cases.
						// Require an assignment or a semicolon to follow
						bOkToExtractVariables = (cxxTokenChainFirstTokenOfType(
								pChain,
								CXXTokenTypeAssignment | CXXTokenTypeSemicolon
							) ? true : false); // ternary ?: needed because of MSVC
					break;
					case CXXTokenTypeAssignment:
					case CXXTokenTypeSemicolon:
						// looks ok
						bOkToExtractVariables = true;
					break;
					default:
						// should NOT happen!
						CXX_DEBUG_ASSERT(false,"Unexpected token type");
					break;
				}
			} else {
				// looks ok
				bOkToExtractVariables = true;
			}
		}
	}

	if(bOkToExtractVariables)
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

	CXX_DEBUG_LEAVE();
	return true;
}

static rescanReason cxxParserMain(const unsigned int passCount)
{
	cxxScopeClear();
	cxxTokenAPINewFile();
	cxxParserNewStatement();

	int kind_for_define = CXXTagKindMACRO;
	int kind_for_header = CXXTagKindINCLUDE;
	int kind_for_macro_param = CXXTagKindMACROPARAM;
	int role_for_macro_undef = CR_MACRO_UNDEF;
	int role_for_macro_condition = CR_MACRO_CONDITION;
	int role_for_header_system = CR_HEADER_SYSTEM;
	int role_for_header_local = CR_HEADER_LOCAL;

	Assert(passCount < 3);

	cppInit(
			(bool) (passCount > 1),
			false,
			true, // raw literals
			false,
			kind_for_define,
			role_for_macro_undef,
			role_for_macro_condition,
			kind_for_macro_param,
			kind_for_header,
			role_for_header_system,
			role_for_header_local,
			g_cxx.pFieldOptions[CXXTagFieldMacrodef].ftype
		);

	g_cxx.iChar = ' ';

	g_cxx.iNestingLevels = 0;

	bool bRet = cxxParserParseBlock(false);

	cppTerminate ();

	// Shut up coveralls: LCOV_EXCL_START
	cxxTokenChainClear(g_cxx.pTokenChain);
	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainClear(g_cxx.pTemplateTokenChain);
	if(g_cxx.pTemplateSpecializationTokenChain)
		cxxTokenChainClear(g_cxx.pTemplateSpecializationTokenChain);
	// Restart coveralls: LCOV_EXCL_END

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
	cxxTagInitForLanguage(g_cxx.eCLangType);

	g_cxx.bConfirmedCPPLanguage = false;
	cxxKeywordEnablePublicProtectedPrivate(false);

	rescanReason r = cxxParserMain(passCount);
	CXX_DEBUG_LEAVE();
	return r;
}

rescanReason cxxCUDAParserMain(const unsigned int passCount)
{
	CXX_DEBUG_ENTER();
	cxxTagInitForLanguage(g_cxx.eCUDALangType);

	// CUDA is C.
	g_cxx.bConfirmedCPPLanguage = false;
	cxxKeywordEnablePublicProtectedPrivate(false);

	rescanReason r = cxxParserMain(passCount);
	CXX_DEBUG_LEAVE();
	return r;
}

rescanReason cxxCppParserMain(const unsigned int passCount)
{
	CXX_DEBUG_ENTER();
	cxxTagInitForLanguage(g_cxx.eCPPLangType);

	// In header files we disable processing of public/protected/private keywords
	// until we either figure out that this is really C++ or we're start parsing
	// a struct/union.
	g_cxx.bConfirmedCPPLanguage = !isInputHeaderFile();
	cxxKeywordEnablePublicProtectedPrivate(g_cxx.bConfirmedCPPLanguage);

	rescanReason r = cxxParserMain(passCount);
	CXX_DEBUG_LEAVE();
	return r;
}

static void cxxParserFirstInit(void)
{
	memset(&g_cxx,0,sizeof(CXXParserState));

	g_cxx.eCLangType = -1;
	g_cxx.eCPPLangType = -1;
	g_cxx.eCUDALangType = -1;

	cxxTokenAPIInit();

	g_cxx.pTokenChain = cxxTokenChainCreate();

	cxxScopeInit();

	g_bFirstRun = false;
}

void cxxCUDAParserInitialize(const langType language)
{
	CXX_DEBUG_PRINT("Parser initialize for language CUDA");
	if(g_bFirstRun)
		cxxParserFirstInit();

	g_cxx.eCUDALangType = language;

	cxxBuildKeywordHash(language,CXXLanguageCUDA);
}

void cxxCppParserInitialize(const langType language)
{
	CXX_DEBUG_PRINT("Parser initialize for language C++");
	if(g_bFirstRun)
		cxxParserFirstInit();

	g_cxx.eCPPLangType = language;

	cxxBuildKeywordHash(language,CXXLanguageCPP);
}

void cxxCParserInitialize(const langType language)
{
	CXX_DEBUG_PRINT("Parser initialize for language C");
	if(g_bFirstRun)
		cxxParserFirstInit();

	g_cxx.eCLangType = language;

	cxxBuildKeywordHash(language,CXXLanguageC);
}

void cxxParserCleanup(langType language CTAGS_ATTR_UNUSED,bool initialized CTAGS_ATTR_UNUSED)
{
	if(g_bFirstRun)
		return; // didn't run at all

	// This function is used as finalizer for all the sub-language parsers.
	// The next line forces this function to be called only once
	g_bFirstRun = true;

	// Shut up coveralls: LCOV_EXCL_START
	if(g_cxx.pUngetToken)
		cxxTokenDestroy(g_cxx.pUngetToken);
	if(g_cxx.pTokenChain)
		cxxTokenChainDestroy(g_cxx.pTokenChain);
	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainDestroy(g_cxx.pTemplateTokenChain);
	if(g_cxx.pTemplateSpecializationTokenChain)
		cxxTokenChainDestroy(g_cxx.pTemplateSpecializationTokenChain);
	// Restart coveralls: LCOV_EXCL_END

	cxxScopeDone();

	cxxTokenAPIDone();
}
