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


static bool cxxTemplateTokenCheckIsNonTypeAndCompareWord(const CXXToken *t,void * szWord)
{
	// To be non type the token must NOT be preceded by class/struct/union
	if(!t->pPrev)
		return false;
	if(cxxTokenTypeIs(t->pPrev,CXXTokenTypeKeyword))
	{
		if(cxxKeywordIsTypeRefMarker(t->pPrev->eKeyword))
			return false; // preceded by a type ref marker

		// otherwise it's probably something like "int"
	}
	const char * w = (const char *)szWord;
	return strcmp(vStringValue(t->pszWord),w) == 0;
}

static bool cxxTokenIsPresentInTemplateParametersAsNonType(CXXToken * t)
{
	CXX_DEBUG_ASSERT(
			cxxTokenTypeIsOneOf(t,CXXTokenTypeIdentifier),
			"Token must be identifier"
		);

	for(unsigned int u=0;u<g_cxx.oTemplateParameters.uCount;u++)
	{
		if(
			cxxTemplateTokenCheckIsNonTypeAndCompareWord(
					t,
					vStringValue(g_cxx.oTemplateParameters.aIdentifiers[u]->pszWord)
				)
			)
			return true;
	}

	return false;
}

static bool cxxTemplateTokenCheckIsTypeAndCompareWord(const CXXToken * t,void * szWord)
{
	// To be non type the token must be preceded by class/struct/union
	if(!t->pPrev)
		return false;
	if(!cxxTokenTypeIs(t->pPrev,CXXTokenTypeKeyword))
		return false;
	if(!cxxKeywordIsTypeRefMarker(t->pPrev->eKeyword))
		return false;
	const char * w = (const char *)szWord;
	return strcmp(vStringValue(t->pszWord),w) == 0;
}

static bool cxxTokenIsPresentInTemplateParametersAsType(CXXToken * t)
{
	CXX_DEBUG_ASSERT(
			cxxTokenTypeIsOneOf(t,CXXTokenTypeIdentifier),
			"Token must be identifier"
		);

	for(unsigned int u=0;u<g_cxx.oTemplateParameters.uCount;u++)
	{
		if(
			cxxTemplateTokenCheckIsTypeAndCompareWord(
					t,
					vStringValue(g_cxx.oTemplateParameters.aIdentifiers[u]->pszWord)
				)
			)
			return true;
	}

	return false;
}

// Attempt to capture a template parameter that is between the
// specified tokens. pBeforeParameter points to the first token
// of the parameter (just after < or ,). pAfterParameter points
// somewhere after the end of the parameter, usually to the next
// , or > but it may happen that it points to an unbalanced >
// because of broken input or because we screwed up parsing a bit.
static void cxxParserParseTemplateAngleBracketsCaptureTypeParameter(
		CXXToken * pParameterStart,
		CXXToken * pAfterParameter
	)
{
	CXX_DEBUG_ENTER();

	if(g_cxx.oTemplateParameters.uCount >= CXX_TYPED_VARIABLE_SET_ITEM_COUNT)
	{
		CXX_DEBUG_LEAVE_TEXT("No space for more parameters");
		return;
	}

	CXX_DEBUG_ASSERT(
			pParameterStart &&
			pParameterStart->pPrev &&
			cxxTokenTypeIsOneOf(
					pParameterStart->pPrev,
					CXXTokenTypeSmallerThanSign | CXXTokenTypeComma
				),
			"pParameterStart should point to the parameter start"
		);

	CXX_DEBUG_ASSERT(
			pAfterParameter &&
			pAfterParameter->pPrev &&
			cxxTokenTypeIsOneOf(
					pAfterParameter,
					CXXTokenTypeGreaterThanSign | CXXTokenTypeComma
				),
			"pAfterParameter should point after the parameter"
		);

	CXX_DEBUG_ASSERT(
			pParameterStart != pAfterParameter,
			"The tokens should not be the same"
		);

	// We're OK with:
	//
	// typename X
	// class X
	// int X
	// unsigned int X
	// typeName * X
	// typeName X
	// typeName ...X
	//
	// but not
	//
	// typename boost::enable_if...
	//

	if(pParameterStart->pNext == g_cxx.pToken)
	{
		// Only one token in the parameter. Can't be.
		CXX_DEBUG_LEAVE_TEXT("Single parameter token");
		return;
	}

	// Straegy: run to the first , > or =.

	CXXToken * t = pParameterStart;

	for(;;)
	{
		CXX_DEBUG_PRINT(
				"Token '%s' [%s]",
				vStringValue(t->pszWord),
				cxxDebugTypeDecode(t->eType)
			);

		if(cxxTokenTypeIsOneOf(
				t,
				CXXTokenTypeComma | CXXTokenTypeGreaterThanSign |
				CXXTokenTypeAssignment
			))
		{
			CXX_DEBUG_PRINT("Found terminator, stopping");
			break;
		}

		if(!((
				cxxTokenTypeIs(t,CXXTokenTypeKeyword) &&
				cxxKeywordMayBePartOfTypeName(t->eKeyword)
			) || (
				cxxTokenTypeIsOneOf(
					t,
					CXXTokenTypeIdentifier | CXXTokenTypeStar |
					CXXTokenTypeAnd | CXXTokenTypeMultipleAnds |
					CXXTokenTypeMultipleDots
				)
			)))
		{
			// something we don't like
			CXX_DEBUG_LEAVE_TEXT("This is something we don't like");
			return;
		}

		t = t->pNext;
	}

	if(!cxxTokenTypeIs(t->pPrev,CXXTokenTypeIdentifier))
	{
		CXX_DEBUG_LEAVE_TEXT("The previous token is not an identifier");
		return; // bad
	}

	CXX_DEBUG_PRINT(
			"Adding %s to template parameters",
			vStringValue(t->pPrev->pszWord)
		);

	unsigned int c = g_cxx.oTemplateParameters.uCount;

	g_cxx.oTemplateParameters.aIdentifiers[c] = t->pPrev;
	g_cxx.oTemplateParameters.aTypeStarts[c] = pParameterStart;
	g_cxx.oTemplateParameters.aTypeEnds[c] = t->pPrev->pPrev;

	g_cxx.oTemplateParameters.uCount++;

	CXX_DEBUG_LEAVE();
}


//
// Parses the <parameters> part of a template specification.
// Here we are pointing at the initial <.
//
static CXXParserParseTemplateAngleBracketsResult
cxxParserParseTemplateAngleBracketsInternal(bool bCaptureTypeParameters,int iNestedTemplateLevel)
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

	int iNestedAngleBracketLevel = 1;

	// This points to the token before the current parameter start: < or a comma.
	CXXToken * pBeforeParameterStart = g_cxx.pToken;

	for(;;)
	{
		// Within parentheses everything is permitted.
		if(!cxxParserParseAndCondenseSubchainsUpToOneOf(
				CXXTokenTypeGreaterThanSign | CXXTokenTypeSmallerThanSign |
					CXXTokenTypeOpeningBracket | CXXTokenTypeSemicolon |
					CXXTokenTypeComma | CXXTokenTypeEOF | CXXTokenTypeKeyword,
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
			case CXXTokenTypeComma:
				if(
					bCaptureTypeParameters &&
					(iNestedTemplateLevel == 0) &&
					(iNestedAngleBracketLevel == 1) &&
					(pBeforeParameterStart->pNext != g_cxx.pToken)
				)
					cxxParserParseTemplateAngleBracketsCaptureTypeParameter(
							pBeforeParameterStart->pNext,
							g_cxx.pToken
						);

				if(iNestedAngleBracketLevel == 1)
					pBeforeParameterStart = g_cxx.pToken;
			break;
			case CXXTokenTypeSmallerThanSign:
			{
				// blah <

				CXXToken * pSmallerThan = g_cxx.pToken;

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

				// Check less than operator for the special conditions
				// we can be sure of:
				//    ... 1 < whatever ...
				//    ... typeParam < 1 ... where ident is a type parameter
				// The other cases can't be handled safely. We expect the user
				// to use parentheses.
				if(
					// ... 1 < whatever ...
					cxxTokenTypeIs(pSmallerThan->pPrev,CXXTokenTypeNumber) ||
					// ... nonTypeParam < whatever ...
					cxxTokenIsPresentInTemplateParametersAsNonType(pSmallerThan->pPrev)
				)
				{
					CXX_DEBUG_PRINT("Treating < as less-than operator");

				} else {

					CXX_DEBUG_PRINT("Increasing angle bracket level by one");
					iNestedAngleBracketLevel++;
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
			case CXXTokenTypeGreaterThanSign:
			{
				// > : is it a part of a shift operator?

				bool bFirstFollowedBySpace = g_cxx.pToken->bFollowedBySpace;

				int iGreaterThanCount = 1;

				// Here we skip all of the greater than signs we find
				// up to the number we need to exit the template, but at least three.
				// The minimum of three means that in the following loop we skip
				// at least two greater than signs AND at least another token.

				int iMaxGreaterThanCount = iNestedAngleBracketLevel;
				if(iMaxGreaterThanCount < 3)
					iMaxGreaterThanCount = 3;

				while(iGreaterThanCount < iMaxGreaterThanCount)
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
				}

				CXX_DEBUG_PRINT("Found %d greater-than signs",iGreaterThanCount);

				// check greater than operator: very narrow conditions
				if(
					(iGreaterThanCount == 1) &&
					(
						// whatever op 2 [C++03 allows this without parens]
						// whatever op (...) [C++03 allows this without parens]
						cxxTokenTypeIsOneOf(
								g_cxx.pToken,
								CXXTokenTypeNumber | CXXTokenTypeOpeningParenthesis
							) ||
						// whatever op nonTypeParameter [C++03 allows this without parens]
						(
							cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier) &&
							cxxTokenIsPresentInTemplateParametersAsNonType(g_cxx.pToken)
						)
						// WARNING: don't be tempted to add a loose condition that has
						// (!cxxTokenIsPresentInTemplateParametersAsType()) on the right.
						// It's unsafe.
					)
				)
				{
					CXX_DEBUG_PRINT("Treating as greater-than sign");

					if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeOpeningParenthesis))
						cxxParserUngetCurrentToken(); // needs to be condensed

					continue;
				}


				// check right shift operator: a bit broader conditions
				if(
					(
						(!bFirstFollowedBySpace) &&
						(iGreaterThanCount == 2)
					) && (
						// whatever op 2 [C++03 allows this without parens]
						// whatever op (...) [C++03 allows this without parens]
						cxxTokenTypeIsOneOf(
								g_cxx.pToken,
								CXXTokenTypeNumber | CXXTokenTypeOpeningParenthesis
							) ||
						// whatever op nonTypeParameter [C++03 allows this without parens]
						(
							cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier) &&
							cxxTokenIsPresentInTemplateParametersAsNonType(g_cxx.pToken)
						) ||
						// a broader condition that kind-of-works at top level
						(
							// topmost template nesting level
							(iNestedTemplateLevel == 0) &&
							// Only one level of angle brackets.
							// This means that:
							// - >> has one angle bracket too much to exit the template
							// - we have screwed up the parsing of an opening angle bracket
							// In the first case it's very likely that we're over a shift
							// operator. In the other case we're screwed anyway.
							(iNestedAngleBracketLevel == 1) &&
							// identifier on the right that is not clearly identified as type
							(
								cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier) &&
								(!cxxTokenIsPresentInTemplateParametersAsType(g_cxx.pToken))
							)
						)
					)
				)
				{
					CXX_DEBUG_PRINT("Treating as right shift operator");
					if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeOpeningParenthesis))
						cxxParserUngetCurrentToken(); // needs to be condensed

					continue;
				}

				if(!cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeGreaterThanSign))
				{
					// The loop above stopped because of a non > token.
					CXX_DEBUG_ASSERT(iGreaterThanCount < iMaxGreaterThanCount,"Bug");

					// Handle gracefully some special cases
					if(
							cxxTokenIsNonConstantKeyword(g_cxx.pToken) ||
							(
								cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier) &&
								cxxTokenIsPresentInTemplateParametersAsType(g_cxx.pToken)
							)
						)
					{
						// We found something like
						//   ... > void ...
						//   ... > static ...
						//   ... > typeParameter ...
						// The part on the right of > does not seem to be a constant
						// so this is not a comparison.
						CXX_DEBUG_PRINT(
								"Found '> %s': assuming end of template",
								vStringValue(g_cxx.pToken->pszWord)
							);

						cxxParserUngetCurrentToken();

						if(
							bCaptureTypeParameters &&
							(iNestedTemplateLevel == 0) &&
							(pBeforeParameterStart->pNext != g_cxx.pToken)
						)
							cxxParserParseTemplateAngleBracketsCaptureTypeParameter(
									pBeforeParameterStart->pNext,
									g_cxx.pToken
								);

						if(iGreaterThanCount > iNestedAngleBracketLevel)
						{
							// Most likely explanation:
							// We screwed up the parsing of the template.
							// However we can still attempt to emit a symbol here.
							CXX_DEBUG_LEAVE_TEXT("Found (broken) end of template");
							return CXXParserParseTemplateAngleBracketsFinishedPrematurely;
						}

						CXX_DEBUG_LEAVE_TEXT("Found end of template");
						return CXXParserParseTemplateAngleBracketsSucceeded;
					}

					cxxParserUngetCurrentToken();
				}

				while(iGreaterThanCount > iNestedAngleBracketLevel)
				{
					CXX_DEBUG_PRINT("Going back one >");
					iGreaterThanCount--;
					cxxParserUngetCurrentToken();
				}

				CXX_DEBUG_PRINT("Decreasing angle bracket level by %d",iGreaterThanCount);
				iNestedAngleBracketLevel -= iGreaterThanCount;

				if(iNestedAngleBracketLevel == 0)
				{
					if(
						bCaptureTypeParameters &&
						(iNestedTemplateLevel == 0) &&
						(pBeforeParameterStart->pNext != g_cxx.pToken)
					)
						cxxParserParseTemplateAngleBracketsCaptureTypeParameter(
								pBeforeParameterStart->pNext,
								g_cxx.pToken
							);

					CXX_DEBUG_LEAVE_TEXT("Found end of template");
					return CXXParserParseTemplateAngleBracketsSucceeded;
				}
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
						if(!cxxTokenTypeIs(g_cxx.pToken->pPrev->pPrev,CXXTokenTypeMultipleColons))
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

						//
						// Possibly X::template Something<Y,Z> disambiguation syntax.
						// See https://en.cppreference.com/w/cpp/language/dependent_name
						//
						CXX_DEBUG_PRINT("But it's not followed by a < and has leading ::");
						continue;
					}

					switch(
							cxxParserParseTemplateAngleBracketsInternal(
									false,
									iNestedTemplateLevel+1
								)
						)
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
static bool cxxParserParseTemplateAngleBrackets(bool bCaptureTypeParameters)
{
	CXX_DEBUG_ENTER();

	CXXParserParseTemplateAngleBracketsResult r;
	r = cxxParserParseTemplateAngleBracketsInternal(bCaptureTypeParameters,0);

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

CXXTokenChain * cxxParserParseTemplateAngleBracketsToSeparateChain(bool bCaptureTypeParameters)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(cxxParserCurrentLanguageIsCPP(),"This should be called only in C++");

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),CXXTokenTypeSmallerThanSign),
			"We should be pointing at the opening angle bracket here"
		);

	CXXTokenChain * pOut = cxxTokenChainCreate();
	cxxTokenChainAppend(pOut,cxxTokenChainTakeLast(g_cxx.pTokenChain));

	CXXTokenChain * pSave = g_cxx.pTokenChain;
	g_cxx.pTokenChain = pOut;

	bool bRet = cxxParserParseTemplateAngleBrackets(bCaptureTypeParameters);

	g_cxx.pTokenChain = pSave;

	if(!bRet)
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse angle brackets");
		cxxTokenChainDestroy(pOut);
		return NULL;
	}

	CXX_DEBUG_LEAVE();
	return pOut;
}

//
// Parses the template angle brackets and puts it in g_cxx.pTemplateTokenChain.
// Also captures he template type parameters in g_cxx.pTemplateParameters.
//
bool cxxParserParseTemplateAngleBracketsToTemplateChain(void)
{
	CXX_DEBUG_ENTER();

	g_cxx.oTemplateParameters.uCount = 0;

	CXXTokenChain * pOut = cxxParserParseTemplateAngleBracketsToSeparateChain(true);

	if(!pOut)
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse angle brackets");
		return false;
	}

	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainDestroy(g_cxx.pTemplateTokenChain);

	g_cxx.pTemplateTokenChain = pOut;

	g_cxx.oTemplateParameters.pChain = pOut;

	// make sure we have no stale specializations
	// (note that specializations always come AFTER the main template)
	if(g_cxx.pTemplateSpecializationTokenChain)
	{
		cxxTokenChainDestroy(g_cxx.pTemplateSpecializationTokenChain);
		g_cxx.pTemplateSpecializationTokenChain = NULL;
	}

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

	bool bRet = cxxParserParseTemplateAngleBracketsToTemplateChain();

	CXX_DEBUG_LEAVE();
	return bRet;
}

void cxxParserEmitTemplateParameterTags(void)
{
	CXX_DEBUG_ASSERT(
			g_cxx.pTemplateTokenChain &&
			(g_cxx.pTemplateTokenChain->iCount > 0) &&
			cxxParserCurrentLanguageIsCPP() &&
			cxxTagKindEnabled(CXXTagCPPKindTEMPLATEPARAM),
			"Template existence must be checked before calling this function"
		);

	unsigned int c = g_cxx.oTemplateParameters.uCount;

	for(unsigned int i=0;i<c;i++)
	{
		CXX_DEBUG_PRINT(
				"Emitting template param tag for '%s'",
				vStringValue(g_cxx.oTemplateParameters.aIdentifiers[i]->pszWord)
			);

		tagEntryInfo * tag = cxxTagBegin(
				CXXTagCPPKindTEMPLATEPARAM,
				g_cxx.oTemplateParameters.aIdentifiers[i]
			);

		if(!tag)
			continue;

		tag->extensionFields.nth = (short)i;

		CXXToken * pTypeToken = cxxTagCheckAndSetTypeField(
				g_cxx.oTemplateParameters.aTypeStarts[i],
				g_cxx.oTemplateParameters.aTypeEnds[i]
			);

		cxxTagCommit(NULL);
		if (pTypeToken)
			cxxTokenDestroy(pTypeToken);
	}
}
