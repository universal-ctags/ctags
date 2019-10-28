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
#include "../cpreprocessor.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"
#include "strlist.h"

#include <string.h>

typedef enum _CXXParserParseTemplateAngleBracketsResult
{
	// Succeeded parsing the template angle bracket, everything looks fine
	CXXParserParseTemplateAngleBracketsSucceeded,
	// Succeeded, but the parsing was unbalanced and was terminated by an 'unexpected' condition
	// that detected the end of the template. If this is function has been called
	// from an upper template parsing level, the caller should exit too.
	CXXParserParseTemplateAngleBracketsFinishedPrematurely,
	// Failed miserably, continuing parsing is not possible
	CXXParserParseTemplateAngleBracketsFailed,
	// Failed miserably, but it may be possible to continue parsing
	CXXParserParseTemplateAngleBracketsFailedRecoverable
} CXXParserParseTemplateAngleBracketsResult;


static bool cxxTokenCompareWord(const void *pToken,void * szWord)
{
	CXXToken * t = (CXXToken *)pToken;
	const char * w = (const char *)szWord;
	return strcmp(vStringValue(t->pszWord),w) == 0;
}

static bool cxxTokenIsPresentInTemplateParameters(CXXToken * t)
{
	CXX_DEBUG_ASSERT(
			cxxTokenTypeIsOneOf(t,CXXTokenTypeIdentifier | CXXTokenTypeKeyword),
			"Token must be identifier or keyword"
		);

	return ptrArrayHasTest(
			g_cxx.pTemplateParameters,
			cxxTokenCompareWord,
			vStringValue(t->pszWord)
		);
}

//
// Parses the <parameters> part of a template specification.
// Here we are pointing at the initial <.
//
static CXXParserParseTemplateAngleBracketsResult
cxxParserParseTemplateAngleBracketsInternal(bool bCaptureTypeParameters)
{
	CXX_DEBUG_ENTER();

	// Here we have the big problem of <> characters which may be
	// template argument delimiters, less than/greater than operators,
	// shift left/right operators.
	//
	// A well written code will have parentheses around all the ambiguous cases.
	// We handle that and we permit any kind of syntax inside a parenthesis.
	//
	// Without parentheses we still try to handle the << and >> shift operator cases:
	// - << is always recognized as shift operator
	// - >> is recognized as shift unless it's non-nested. This is what C++11
	//   spec says and theoretically it should be also pseudo-compatible with C++03
	//   which treats this case as a syntax error.
	//
	// The 'less-than' and 'greater-than' operators are hopeless in the general
	// case: gcc is smart enough to figure them out by looking at the identifiers
	// around but without proper state (include headers, macro expansion, full type
	// database etc) we simply can't do the same. However, we try to recover if we
	// figure out we (or the programmer?) screwed up.
	//
	//
	// Like gcc, if this function knows identifiers in a template prefix more,
	// the quality of parsing becomes better.
	// Introducing `pslTypeParams` is the first step to know identifiers.
	// pslTypeParams list keeps all type parameters introduced in templated prefixes.
	// When deciding whether ">>" is an end marker of template prefixes or a shift
	// operator, this function looks up pslTypeParams list.
	// If this function can find A of "A >>" in the list, we can say ">>" is not an
	// operator.

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),CXXTokenTypeSmallerThanSign),
			"We should be pointing at the opening angle bracket here"
		);

	int iNestedTemplateLevel = 0;

	for(;;)
	{
		// Within parentheses everything is permitted.
		if(!cxxParserParseAndCondenseSubchainsUpToOneOf(
				CXXTokenTypeGreaterThanSign | CXXTokenTypeSmallerThanSign |
					CXXTokenTypeOpeningBracket | CXXTokenTypeSemicolon |
					CXXTokenTypeEOF | CXXTokenTypeKeyword,
				CXXTokenTypeOpeningParenthesis |
					CXXTokenTypeOpeningSquareParenthesis,
				false
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse up to '<>{EOF'");
			return CXXParserParseTemplateAngleBracketsFailed;
		}

		// note that g_cxx.pToken->pPrev here is always non null.

		switch(g_cxx.pToken->eType)
		{
			case CXXTokenTypeSmallerThanSign:
			{
				// blah <

				CXXToken * pFirstSmallerThan = g_cxx.pToken;

				bool bFirstFollowedBySpace = g_cxx.pToken->bFollowedBySpace;

				int iSmallerThanCount = 1;

				// Here we skip ALL of the smaller-than signs we find.
				for(;;)
				{
					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
						return CXXParserParseTemplateAngleBracketsFailedRecoverable;
					}

					CXX_DEBUG_PRINT(
							"< followed by token '%s' of type 0x%02x (%s)",
							vStringValue(g_cxx.pToken->pszWord),
							g_cxx.pToken->eType,
							cxxDebugTypeDecode(g_cxx.pToken->eType)
						);

					if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSmallerThanSign))
						break;

					iSmallerThanCount++;
				}

				// check left shift operator: very narrow conditions
				if(
					(!bFirstFollowedBySpace) &&
					(iSmallerThanCount == 2) &&
					cxxTokenTypeIsOneOf(
							g_cxx.pToken,
							CXXTokenTypeIdentifier | CXXTokenTypeParenthesisChain |
							CXXTokenTypeNumber | CXXTokenTypeKeyword
						) &&
					// note that pFirstGreaterThan->pPrev is certainly valid here
					cxxTokenTypeIsOneOf(
							pFirstSmallerThan->pPrev,
							CXXTokenTypeIdentifier | CXXTokenTypeKeyword
						) &&
					// int << ...
					(!cxxTokenIsNonConstantKeyword(pFirstSmallerThan->pPrev)) &&
					// ... << class
					(!cxxTokenIsNonConstantKeyword(g_cxx.pToken)) &&
					// T <<
					(!cxxTokenIsPresentInTemplateParameters(pFirstSmallerThan->pPrev))
				)
				{
					// assume it's an operator
					CXX_DEBUG_PRINT("Treating << as shift-left operator");
					continue;
				}

				// Check less than operator for the special conditions
				// we can be sure of:
				//    ... 1 < whatever ...
				//    ... typeParam < 1 ... where ident is a type parameter
				// The other cases can't be handled safely. We expect the user
				// to use parentheses.
				if(
					(iSmallerThanCount == 1) &&
					(
						// ... 1 < whatever ...
						cxxTokenTypeIs(pFirstSmallerThan->pPrev,CXXTokenTypeNumber) ||
						// ... typeParam < 1 ...
						(
							cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeNumber) &&
							cxxTokenIsPresentInTemplateParameters(pFirstSmallerThan->pPrev)
						)
					)
				)
				{
					CXX_DEBUG_PRINT("Treating < as less-than operator");
					continue;
				}

				CXX_DEBUG_PRINT("Increasing template level by %d",iSmallerThanCount);
				iNestedTemplateLevel += iSmallerThanCount;

				if(cxxTokenTypeIsOneOf(
						g_cxx.pToken,
						CXXTokenTypeOpeningParenthesis |
							CXXTokenTypeOpeningSquareParenthesis
					))
				{
					// would need to be condensed: unget and try again above
					cxxParserUngetCurrentToken();
				}

				// anything else is OK
			}
			break;
			case CXXTokenTypeGreaterThanSign:
			{
				if(iNestedTemplateLevel == 0)
				{
					// Non-nested > : always a terminator
					CXX_DEBUG_LEAVE_TEXT("Found end of template");
					return CXXParserParseTemplateAngleBracketsSucceeded;
				}

				// Nested > : is it a shift operator?

				CXXToken * pFirstGreaterThan = g_cxx.pToken;

				bool bFirstFollowedBySpace = g_cxx.pToken->bFollowedBySpace;

				int iGreaterThanCount = 1;

				// Here we skip ALL of the greater than signs we find
				// unless they're more than we need to exit the template
				// parsing routine.
				//
				// Rationale:
				//
				// iNestedTemplateLevel is certainly > 0 here (see check above).
				// So it is always safe to parse N = (iNestedTemplateLevel + 1)
				// greater-than signs. And N >= 2, for sure.
				// If we parse exactly 2 greater-than signs (that is, a >>) then
				// it MAY be a shift operator, depending on what is found on the
				// left and right. To check what is found on right we need to
				// continue parsing even after then second greater-than.
				// So we really just continue parsing until either a
				// non-greater-than token is found OR we parse N+1 greater-than
				// tokens. In the latter case the shift operator is excluded
				// and we will have to unget one of the greater-thans.

				int iMaxGreaterThanCount = iNestedTemplateLevel + 1;

				CXX_DEBUG_ASSERT(iMaxGreaterThanCount >= 2,"Invariant violated");

				for(;;)
				{
					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
						return CXXParserParseTemplateAngleBracketsFailedRecoverable;
					}

					CXX_DEBUG_PRINT(
							"> followed by token '%s' of type 0x%02x (%s)",
							vStringValue(g_cxx.pToken->pszWord),
							g_cxx.pToken->eType,
							cxxDebugTypeDecode(g_cxx.pToken->eType)
						);

					if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeGreaterThanSign))
						break;

					iGreaterThanCount++;

					if(iGreaterThanCount > iMaxGreaterThanCount)
						break; // one greater than too much
				}

				// check right shift operator: very narrow conditions
				if(
					(!bFirstFollowedBySpace) &&
					(iGreaterThanCount == 2) &&
					cxxTokenTypeIsOneOf(
							g_cxx.pToken,
							CXXTokenTypeIdentifier | CXXTokenTypeParenthesisChain |
							CXXTokenTypeNumber | CXXTokenTypeKeyword
						) &&
					// note that pFirstGreaterThan->pPrev is certainly valid here
					cxxTokenTypeIsOneOf(
							pFirstGreaterThan->pPrev,
							CXXTokenTypeIdentifier | CXXTokenTypeKeyword
						) &&
					// int >> ...
					(!cxxTokenIsNonConstantKeyword(pFirstGreaterThan->pPrev)) &&
					// ... >> class
					(!cxxTokenIsNonConstantKeyword(g_cxx.pToken)) &&
					// T >>
					(!cxxTokenIsPresentInTemplateParameters(pFirstGreaterThan->pPrev))
				)
				{
					// assume it's an operator
					CXX_DEBUG_PRINT("Treating >> as shift-right operator");

					// invariant: iGreaterThanCount == 2 but iMaxGreaterTanCount >= 2
					CXX_DEBUG_ASSERT(iGreaterThanCount <= iMaxGreaterThanCount,"Bug");

					continue;
				}

				if(iGreaterThanCount > iMaxGreaterThanCount)
				{
					// ops! scanned one too much
					// Possibly something like template<template<...>>

					// invariant: at most one too much
					CXX_DEBUG_ASSERT(iGreaterThanCount == (iMaxGreaterThanCount+1),"Bug");
					CXX_DEBUG_ASSERT(iGreaterThanCount > 2,"Bug");

					cxxParserUngetCurrentToken();

					CXX_DEBUG_LEAVE_TEXT("Found end of template");
					return CXXParserParseTemplateAngleBracketsSucceeded;
				}

				// Check greater than operator for the special conditions
				// we can be sure of:
				//    ... blah > 1 ...
				// The other cases can't be handled safely. We expect the user
				// to use parentheses.
				if(
					(iGreaterThanCount == 1) &&
					cxxTokenTypeIsOneOf(
							g_cxx.pToken,
							CXXTokenTypeNumber
						)
				)
				{
					CXX_DEBUG_PRINT("Treating > as greater-than operator");
					continue;
				}

				CXX_DEBUG_PRINT("Decreasing template level by %d",iGreaterThanCount);

				iNestedTemplateLevel -= iGreaterThanCount;

				// Handle gracefully some special cases
				if(cxxTokenIsNonConstantKeyword(g_cxx.pToken))
				{
					// We found something like
					//   ... > void ...
					//   ... > static ...
					// The part on the right of > does not seem to be a constant
					// so this is not a comparison. Most likely explanation:
					// We screwed up the parsing of the template.
					// However we can still attempt to emit a symbol here.
					CXX_DEBUG_PRINT(
							"Found '> %s': assuming end of template",
							vStringValue(g_cxx.pToken->pszWord)
						);

					cxxParserUngetCurrentToken();
					CXX_DEBUG_LEAVE_TEXT("Found (broken) end of template");
					return CXXParserParseTemplateAngleBracketsFinishedPrematurely;
				}

				if(cxxTokenTypeIsOneOf(
						g_cxx.pToken,
						CXXTokenTypeOpeningParenthesis |
							CXXTokenTypeOpeningSquareParenthesis
					))
				{
					// would need to be condensed: unget and try again above
					cxxParserUngetCurrentToken();
				}

				// anything else is OK
			}
			break;
			case CXXTokenTypeKeyword:

				if(cxxTokenIsKeyword(g_cxx.pToken,CXXKeywordTEMPLATE))
				{
					CXX_DEBUG_PRINT("Found nested template keyword");

					// nested nastiness.
					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
						return CXXParserParseTemplateAngleBracketsFailedRecoverable;
					}

					if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSmallerThanSign))
					{
						// aaargh...
						CXX_DEBUG_PRINT(
								"Found unexpected token '%s' of type 0x%02x",
								vStringValue(g_cxx.pToken->pszWord),
								g_cxx.pToken->eType
							);

						CXX_DEBUG_LEAVE_TEXT("No smaller than sign after template keyword");
						return CXXParserParseTemplateAngleBracketsFailed;
					}

					switch(cxxParserParseTemplateAngleBracketsInternal(false))
					{
						case CXXParserParseTemplateAngleBracketsFailed:
							CXX_DEBUG_LEAVE_TEXT("Nested template parsing failed");
							return CXXParserParseTemplateAngleBracketsFailed;
						break;
						case CXXParserParseTemplateAngleBracketsFailedRecoverable:
							CXX_DEBUG_LEAVE_TEXT("Nested template parsing recovered");
							return CXXParserParseTemplateAngleBracketsFailedRecoverable;
						break;
						case CXXParserParseTemplateAngleBracketsFinishedPrematurely:
							CXX_DEBUG_LEAVE_TEXT("Nested template finished prematurely");
							return CXXParserParseTemplateAngleBracketsFinishedPrematurely;
						break;
						case CXXParserParseTemplateAngleBracketsSucceeded:
							// ok
							CXX_DEBUG_PRINT("Nested template parsing succeeded");
						break;
						default:
							CXX_DEBUG_ASSERT(false,"Should never end up here");
							return CXXParserParseTemplateAngleBracketsFailed;
						break;
					}

					continue;
				}

				if(
						bCaptureTypeParameters &&
						cxxKeywordMayBePartOfTypeName(g_cxx.pToken->eKeyword) &&
						g_cxx.pToken->pPrev &&
						cxxTokenTypeIsOneOf(
								g_cxx.pToken->pPrev,
								CXXTokenTypeSmallerThanSign | CXXTokenTypeComma
							)
					)
				{
					// < typename X
					// , class Y
					// , int z
					//
					// but not
					//
					// < typename boost::enable_if...
					//

					CXX_DEBUG_PRINT("Found part of typename: look for type param");

					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerable");
						return CXXParserParseTemplateAngleBracketsFailedRecoverable;
					}

					if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier))
					{
						// FIXME: this doesn't handle types with multiple keywords
						//        (unsigned int)

						CXX_DEBUG_PRINT("Found non-identifier: not a type param");
						cxxParserUngetCurrentToken();
						continue;
					}

					CXXToken * pParam = g_cxx.pToken;

					CXX_DEBUG_PRINT("Found identifier '%s'",vStringValue(pParam->pszWord));

					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerable");
						return CXXParserParseTemplateAngleBracketsFailedRecoverable;
					}

					if(!cxxTokenTypeIsOneOf(
							g_cxx.pToken,
							CXXTokenTypeGreaterThanSign |
							CXXTokenTypeComma |
							CXXTokenTypeAssignment
						))
					{
						// nope.
						CXX_DEBUG_PRINT("Followed by unexpected stuff: no type param");
						cxxParserUngetCurrentToken();
						continue;
					}

					CXX_DEBUG_PRINT(
							"Adding '%s' to template parameter list",
							vStringValue(pParam->pszWord)
						);

					ptrArrayAdd(g_cxx.pTemplateParameters,pParam);

					if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeGreaterThanSign))
						cxxParserUngetCurrentToken();

					continue;
				}

				// other keyword
			break;
			case CXXTokenTypeEOF:
				CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
				return CXXParserParseTemplateAngleBracketsFailedRecoverable;
			break;
			case CXXTokenTypeSemicolon:
				cxxParserNewStatement();
				CXX_DEBUG_LEAVE_TEXT("Broken template arguments, attempting to continue");
				return CXXParserParseTemplateAngleBracketsFailedRecoverable;
			break;
			case CXXTokenTypeOpeningBracket:
				CXX_DEBUG_PRINT(
						"Found opening bracket: either syntax error or we screwed up parsing " \
							"the template parameters (some kind of ugly C++11 syntax?), " \
							"but we try to recover..."
					);
				// skip the whole bracketed part.
				if(!cxxParserParseUpToOneOf(CXXTokenTypeClosingBracket | CXXTokenTypeEOF, false))
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to parse up to '}EOF'");
					return CXXParserParseTemplateAngleBracketsFailed;
				}
				cxxParserNewStatement();
				CXX_DEBUG_LEAVE_TEXT("Broken template arguments recovery complete");
				return CXXParserParseTemplateAngleBracketsFailedRecoverable;
			break;
			default:
				CXX_DEBUG_ASSERT(false,"Found unexpected token type 0x%02x",g_cxx.pToken->eType);
				CXX_DEBUG_LEAVE_TEXT("Found unexpected token type 0x%02x",g_cxx.pToken->eType);
				return CXXParserParseTemplateAngleBracketsFailed;
			break;
		}
	}

	// never reached
	CXX_DEBUG_LEAVE_TEXT("This should be never reached!");
	return CXXParserParseTemplateAngleBracketsFailed;
}

//
// Parses the <parameters> part of a template specification.
// Here we are pointing at the initial <.
//
static bool cxxParserParseTemplateAngleBrackets(void)
{
	CXX_DEBUG_ENTER();

	CXXParserParseTemplateAngleBracketsResult r;
	r = cxxParserParseTemplateAngleBracketsInternal(true);

	switch(r)
	{
		case CXXParserParseTemplateAngleBracketsFailed:
			CXX_DEBUG_LEAVE();
			return false;
		break;
		// TODO: We could signal failure+recovery to upper levels
		//       so the caller could take recovery actions too.
		//case CXXParserParseTemplateAngleBracketsFailedRecoverable:
		//case CXXParserParseTemplateAngleBracketsFinishedPrematurely:
		//case CXXParserParseTemplateAngleBracketsSucceeded:
		default:
			CXX_DEBUG_LEAVE();
			return true;
		break;
	}
	CXX_DEBUG_ASSERT(false,"Never here");
}

//
// Parses the template angle brackets and puts it in g_cxx.pTemplateTokenChain.
//
bool cxxParserParseTemplateAngleBracketsToSeparateChain(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(cxxParserCurrentLanguageIsCPP(),"This should be called only in C++");

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),CXXTokenTypeSmallerThanSign),
			"We should be pointing at the opening angle bracket here"
		);

	CXXTokenChain * pSave = g_cxx.pTokenChain;
	g_cxx.pTokenChain = cxxTokenChainCreate();
	cxxTokenChainAppend(g_cxx.pTokenChain,cxxTokenChainTakeLast(pSave));

	// TODO: Do we need to handle nesting?
	//       Not for template<template<>> as it is handled separately
	//       But maybe for nasty things like template<...(template<>)...>?

	if(g_cxx.pTemplateParameters)
		ptrArrayClear(g_cxx.pTemplateParameters);
	else
		g_cxx.pTemplateParameters = ptrArrayNew(NULL);

	if(!cxxParserParseTemplateAngleBrackets())
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse angle brackets");
		cxxTokenChainDestroy(pSave);
		return false;
	}

	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainDestroy(g_cxx.pTemplateTokenChain);

	g_cxx.pTemplateTokenChain = g_cxx.pTokenChain;
	g_cxx.pTokenChain = pSave;

	CXX_DEBUG_LEAVE();
	return true;
}


//
// Parses a template<anything> prefix.
// The parsed template parameter definition is stored in a separate token chain.
//
bool cxxParserParseTemplatePrefix(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(cxxParserCurrentLanguageIsCPP(),"This should be called only in C++");

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),CXXTokenTypeKeyword),
			"We should be pointing at the template keyword here"
		);

	cxxTokenChainDestroyLast(g_cxx.pTokenChain); // kill the template keyword

	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeSmallerThanSign | CXXTokenTypeEOF | CXXTokenTypeSemicolon,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to the < sign");
		return false;
	}

	if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeEOF | CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_LEAVE_TEXT("Found EOF or semicolon: assuming this is unparseable");
		cxxParserNewStatement();
		return true; // tolerate syntax error
	}

	bool bRet = cxxParserParseTemplateAngleBracketsToSeparateChain();

	CXX_DEBUG_LEAVE();
	return bRet;
}

void cxxParserEmitTemplateParameterTags(void)
{
	CXX_DEBUG_ASSERT(
			g_cxx.pTemplateTokenChain &&
			(g_cxx.pTemplateTokenChain->iCount > 0) &&
			cxxParserCurrentLanguageIsCPP() &&
			g_cxx.pTemplateParameters && // this should be ensured by chain existence
			cxxTagKindEnabled(CXXTagCPPKindTEMPLATEPARAM),
			"Template existence must be checked before calling this function"
		);

	int c = ptrArrayCount(g_cxx.pTemplateParameters);

	for(int i=0;i<c;i++)
	{
		CXXToken * t = (CXXToken *)ptrArrayItem(g_cxx.pTemplateParameters,i);

		CXX_DEBUG_PRINT("Emitting template param tag for '%s'",vStringValue(t->pszWord));

		tagEntryInfo * tag = cxxTagBegin(
				CXXTagCPPKindTEMPLATEPARAM,
				t
			);

		if(!tag)
			continue;

		cxxTagCommit();
	}
}
