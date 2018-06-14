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

//
// Parses the <parameters> part of a template specification.
// Here we are pointing at the initial <.
//
static CXXParserParseTemplateAngleBracketsResult cxxParserParseTemplateAngleBracketsInternal(void)
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

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),CXXTokenTypeSmallerThanSign),
			"We should be pointing at the opening angle bracket here"
		);

	int iTemplateLevel = 0;

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

evaluate_current_token:
		switch(g_cxx.pToken->eType)
		{
			case CXXTokenTypeSmallerThanSign:
				if(
						g_cxx.pToken->pPrev &&
						cxxTokenTypeIsOneOf(
								g_cxx.pToken->pPrev,
								CXXTokenTypeIdentifier | CXXTokenTypeKeyword
							)
					)
				{
					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
						return CXXParserParseTemplateAngleBracketsFailedRecoverable;
					}

					CXX_DEBUG_PRINT(
							"Got token '%s' of type 0x%02x",
							vStringValue(g_cxx.pToken->pszWord),
							g_cxx.pToken->eType
						);

					if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSmallerThanSign))
					{
						// assume it's an operator
						CXX_DEBUG_PRINT("Treating < as shift-left operator");
					} else {
						CXX_DEBUG_PRINT("Increasing template level");
						iTemplateLevel++;

						if(cxxTokenTypeIsOneOf(
								g_cxx.pToken,
								CXXTokenTypeOpeningParenthesis |
									CXXTokenTypeOpeningSquareParenthesis
							))
						{
							CXX_DEBUG_PRINT("Condensing subchain");
							// hmmm.. must condense as subchain
							if(!cxxParserParseAndCondenseCurrentSubchain(
									CXXTokenTypeOpeningParenthesis |
										CXXTokenTypeOpeningSquareParenthesis,
									true,
									false
								))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to condense current subchain");
								return CXXParserParseTemplateAngleBracketsFailed;
							}

							if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
							{
								CXX_DEBUG_LEAVE_TEXT(
										"Found EOF, syntax error but we tolerate it"
									);
								return CXXParserParseTemplateAngleBracketsFailedRecoverable;
							}
						} else {
							// it's ok
							CXX_DEBUG_PRINT("No need to condense subchain");
						}
					}
				} else {
					// assume it's an operator
					CXX_DEBUG_PRINT("Treating < as less-than operator");
				}
			break;
			case CXXTokenTypeGreaterThanSign:
			{
				if(iTemplateLevel == 0)
				{
					// Non-nested > : always a terminator
					CXX_DEBUG_LEAVE_TEXT("Found end of template");
					return CXXParserParseTemplateAngleBracketsSucceeded;
				}

				// Nested > : is is a shift operator?

				bool bFollowedBySpace = g_cxx.pToken->bFollowedBySpace;

				if(!cxxParserParseNextToken())
				{
					CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
					return CXXParserParseTemplateAngleBracketsFailedRecoverable;
				}

				CXX_DEBUG_PRINT(
						"Got token '%s' of type 0x%02x",
						vStringValue(g_cxx.pToken->pszWord),
						g_cxx.pToken->eType
					);

				bool bIsGreaterThan = cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeGreaterThanSign);

				if((!bFollowedBySpace) && bIsGreaterThan)
				{
					// assume it's an operator
					CXX_DEBUG_PRINT("Treating > as shift-right operator");
				} else {
					CXX_DEBUG_PRINT("Decreasing template level");
					iTemplateLevel--;

					if(bIsGreaterThan)
						goto evaluate_current_token;

					// Handle gracefully some cases
					if(
							cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeKeyword) &&
							(!cxxKeywordIsConstant(g_cxx.pToken->eKeyword))
						)
					{
						// We found something like
						//   ... > void ...
						//   ... > static ...
						// The part on the right of > does not seem to be a constant
						// so this is not a comparison. Most likely explaination:
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
						CXX_DEBUG_PRINT("Condensing subchain");
						// hmmm.. must condense as subchain
						if(!cxxParserParseAndCondenseCurrentSubchain(
								CXXTokenTypeOpeningParenthesis |
									CXXTokenTypeOpeningSquareParenthesis,
								true,
								false
							))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to condense current subchain");
							return CXXParserParseTemplateAngleBracketsFailed;
						}

						if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
						{
							CXX_DEBUG_LEAVE_TEXT("Found EOF, syntax error but we tolerate it");
							return CXXParserParseTemplateAngleBracketsFailedRecoverable;
						}
					} else {
						// it's ok
						CXX_DEBUG_PRINT("No need to condense subchain");
					}
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
						// aaargh...
						CXX_DEBUG_PRINT(
								"Found unexpected token '%s' of type 0x%02x",
								vStringValue(g_cxx.pToken->pszWord),
								g_cxx.pToken->eType
							);

						CXX_DEBUG_LEAVE_TEXT("No smaller than sign after template keyword");
						return CXXParserParseTemplateAngleBracketsFailed;
					}

					switch(cxxParserParseTemplateAngleBracketsInternal())
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
				}
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
	switch(cxxParserParseTemplateAngleBracketsInternal())
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
