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
// Parses the <parameters> part of the template prefix.
// Here we are pointing at the initial < (but the token chain has been emptied by cxxParserParseTemplatePrefix())
//
static boolean cxxParserParseTemplatePrefixAngleBrackets(void)
{
	CXX_DEBUG_ENTER();

	// Here we have the big prolem of <> characters which may be
	// template argument delimiters, less than/greater than operators, shift left/right operators.
	// 
	// A well written code will have parentheses around all the ambiguous cases. We handle that
	// and we permit any kind of syntax inside a parenthesis.
	// 
	// Without parentheses we still try to handle the << and >> shift operator cases:
	// - << is always recognized as shift operator
	// - >> is recognized as shift unless it's non-nested. This is what C++11 spec says
	//      and theoretically it should be also pseudo-compatible with C++03 which treats this
	//      case as a syntax error.
	//
	// The 'less-than' and 'greater-than' operators are hopeless in the general case: gcc
	// is smart enough to figure them out by looking at the identifiers around but without
	// proper state (include headers, macro expansion, full type database etc) we simply can't
	// do the same. However, we try to recover if we figure out we (or the programmer?) screwed up.

	int iTemplateLevel = 0;

	for(;;)
	{
		// Within parentheses everything is permitted.
		if(!cxxParserParseAndCondenseSubchainsUpToOneOf(
				CXXTokenTypeGreaterThanSign | CXXTokenTypeSmallerThanSign | CXXTokenTypeOpeningBracket | CXXTokenTypeSemicolon | CXXTokenTypeEOF | CXXTokenTypeAssignment,
				CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis
			))
		{
			CXX_DEBUG_LEAVE_TEXT("Failed to parse up to '<>{EOF'");
			return FALSE;
		}

evaluate_current_token:
		switch(g_cxx.pToken->eType)
		{
			case CXXTokenTypeSmallerThanSign:
				if(g_cxx.pToken->pPrev && cxxTokenTypeIsOneOf(g_cxx.pToken->pPrev,CXXTokenTypeIdentifier | CXXTokenTypeKeyword))
				{
					if(!cxxParserParseNextToken())
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
						return TRUE;
					}

					CXX_DEBUG_PRINT("Got token '%s' of type 0x%02x",vStringValue(g_cxx.pToken->pszWord),g_cxx.pToken->eType);

					if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSmallerThanSign))
					{
						// assume it's an operator
						CXX_DEBUG_PRINT("Treating < as shift-left operator");
					} else {
						CXX_DEBUG_PRINT("Increasing template level");
						iTemplateLevel++;

						if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis))
						{
							CXX_DEBUG_PRINT("Condensing subchain");
							// hmmm.. must condense as subchain
							if(!cxxParserParseAndCondenseCurrentSubchain(CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis,TRUE))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to condense current subchain");
								return TRUE;
							}
							if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
							{
								CXX_DEBUG_LEAVE_TEXT("Found EOF, syntax error but we tolerate it");
								return TRUE;
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
				if(iTemplateLevel == 0)
				{
					// Non-nested > : always a terminator
					cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					CXX_DEBUG_LEAVE_TEXT("Found end of template");
					return TRUE;
				}
				// Nested > : is is a shift operator?

				if(!cxxParserParseNextToken())
				{
					CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
					return TRUE;
				}

				CXX_DEBUG_PRINT("Got token '%s' of type 0x%02x",vStringValue(g_cxx.pToken->pszWord),g_cxx.pToken->eType);

				if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeGreaterThanSign))
				{
					// assume it's an operator
					CXX_DEBUG_PRINT("Treating > as shift-right operator");
				} else {
					CXX_DEBUG_PRINT("Decreasing template level");
					iTemplateLevel--;

					if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis))
					{
						CXX_DEBUG_PRINT("Condensing subchain");
						// hmmm.. must condense as subchain
						if(!cxxParserParseAndCondenseCurrentSubchain(CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis,TRUE))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to condense current subchain");
							return TRUE;
						}
						if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
						{
							CXX_DEBUG_LEAVE_TEXT("Found EOF, syntax error but we tolerate it");
							return TRUE;
						}
					} else {
						// it's ok
						CXX_DEBUG_PRINT("No need to condense subchain");
					}
				}
			break;
			case CXXTokenTypeAssignment:
				CXX_DEBUG_PRINT("Found assignment, trying to skip up to a 'notable' point");
				// try to skip to the next > or , without stopping at < characters.
				if(!cxxParserParseUpToOneOf(
						CXXTokenTypeGreaterThanSign | CXXTokenTypeComma | CXXTokenTypeOpeningBracket | CXXTokenTypeSemicolon | CXXTokenTypeEOF
					))
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to parse up to '}EOF'");
					return FALSE;
				}
				goto evaluate_current_token; // backward jump!
			break;
			case CXXTokenTypeEOF:
				CXX_DEBUG_LEAVE_TEXT("Syntax error, but tolerate it at this level");
				return TRUE;
			break;
			case CXXTokenTypeSemicolon:
				cxxParserNewStatement();
				CXX_DEBUG_LEAVE_TEXT("Broken template arguments, attempting to continue");
				return TRUE;
			break;
			case CXXTokenTypeOpeningBracket:
				CXX_DEBUG_PRINT("Found opening bracket: either syntax error or we screwed up parsing the template parameters (some kind of ugly C++11 syntax?), but we try to recover...");
				// skip the whole bracketed part.
				if(!cxxParserParseUpToOneOf(CXXTokenTypeClosingBracket | CXXTokenTypeEOF))
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to parse up to '}EOF'");
					return FALSE;
				}
				cxxParserNewStatement();
				CXX_DEBUG_LEAVE_TEXT("Broken template arguments recovery complete");
				return TRUE;
			break;
			default:
				CXX_DEBUG_ASSERT(FALSE,"Should not end up here");
				CXX_DEBUG_LEAVE_TEXT("Found unexpected token type 0x%02x",g_cxx.pToken->eType);
				return FALSE;
			break;
		}
	}

	// never reached
	CXX_DEBUG_LEAVE();
	return TRUE;
}

//
// Parses a template<anything> prefix.
// The parsed template parameter definition is stored in a separate token chain.
//
boolean cxxParserParseTemplatePrefix(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(cxxTokenTypeIs(cxxTokenChainLast(g_cxx.pTokenChain),CXXTokenTypeKeyword),"We should be pointing at the template keyword here");

	cxxTokenChainDestroyLast(g_cxx.pTokenChain); // kill the template keyword

	if(!cxxParserParseUpToOneOf(CXXTokenTypeSmallerThanSign | CXXTokenTypeEOF | CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to the < sign");
		return FALSE;
	}

	if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeEOF | CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_LEAVE_TEXT("Found EOF or semicolon: assuming this is unparseable");
		cxxParserNewStatement();
		return TRUE; // tolerate syntax error
	}
	
	CXXTokenChain * pSave = g_cxx.pTokenChain;
	g_cxx.pTokenChain = cxxTokenChainCreate();
	cxxTokenChainAppend(g_cxx.pTokenChain,cxxTokenChainTakeLast(pSave));

	if(!cxxParserParseTemplatePrefixAngleBrackets())
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse angle brackets");
		cxxTokenChainDestroy(pSave);
		return FALSE;
	}

	if(g_cxx.pTemplateTokenChain)
		cxxTokenChainDestroy(g_cxx.pTemplateTokenChain);

	g_cxx.pTemplateTokenChain = g_cxx.pTokenChain;
	g_cxx.pTokenChain = pSave;

	CXX_DEBUG_LEAVE();
	return TRUE;
}

