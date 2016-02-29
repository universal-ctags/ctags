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

boolean cxxParserParseBlockHandleOpeningBracket()
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(g_cxx.pToken->eType == CXXTokenTypeOpeningBracket,"This must be called when pointing at an opening bracket!");

	enum CXXTagKind eScopeKind = cxxScopeGetKind();

	if(
			(g_cxx.pToken->pPrev) &&
			(g_cxx.pToken->pPrev->eType == CXXTokenTypeAssignment) &&
			(
				(eScopeKind == CXXTagKindFUNCTION) ||
				(eScopeKind == CXXTagKindNAMESPACE)
			)
		)
	{
		// array or C++11-list-like initialisation
		boolean bRet = cxxParserParseAndCondenseCurrentSubchain(
				CXXTokenTypeOpeningBracket | CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis,
				FALSE
			);

		CXX_DEBUG_LEAVE_TEXT("Handled array or C++11-list-like initialisation");
		return bRet;
	}

	int iScopes;
	if(eScopeKind != CXXTagKindFUNCTION)
	{
		// very likely a function definition
		iScopes = cxxParserExtractFunctionSignatureBeforeOpeningBracket();
	} else {
		// some kind of other block:
		// - anonymous block
		// - block after for(),while(),foreach(),if() and other similar stuff
		// - lambda

		// check for lambdas
		CXXToken * pParenthesis;
		
		if(
			cxxParserCurrentLanguageIsCPP() &&
			(pParenthesis = cxxParserOpeningBracketIsLambda())
		)
		{
			CXX_DEBUG_LEAVE_TEXT("Will handle lambda");
			return cxxParserHandleLambda(pParenthesis);
		}

		iScopes = 0;
	}

	cxxParserNewStatement();

	if(!cxxParserParseBlock(TRUE))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse nested block");
		return FALSE;
	}

	while(iScopes > 0)
	{
		cxxScopePop();
		iScopes--;
	}

	CXX_DEBUG_LEAVE();
	return TRUE;
}

//
// This is the toplevel scanning function. It's a forward-only scanner that keeps
// accumulating tokens in the chain until either a characteristic token is found
// or the statement ends. When a characteristic token is found it usually enters
// a specialized scanning routine (e.g for classes, namespaces, structs...).
// When the statement ends without finding any characteristic token the chain
// is passed to an analysis routine which does a second scan pass.
//
boolean cxxParserParseBlock(boolean bExpectClosingBracket)
{
	CXX_DEBUG_ENTER();

	//char * szScopeName = cxxScopeGetFullName();
	//CXX_DEBUG_PRINT("Scope name is '%s'",szScopeName ? szScopeName : "");

	cxxParserNewStatement();
	
	if(bExpectClosingBracket)
		cppBeginStatement(); // FIXME: this cpp handling is broken: it works only because the moon is in the correct phase.

	for(;;)
	{
		if(!cxxParserParseNextToken())
		{
			if(bExpectClosingBracket)
			{
				CXX_DEBUG_LEAVE_TEXT("Syntax error: found EOF in block but a closing bracket was expected!");
				return FALSE;
			}
			CXX_DEBUG_LEAVE_TEXT("EOF in main block");
			return TRUE; // EOF
		}

		CXX_DEBUG_PRINT("Token '%s' of type 0x%02x",vStringValue(g_cxx.pToken->pszWord),g_cxx.pToken->eType);

		switch(g_cxx.pToken->eType)
		{
			case CXXTokenTypeKeyword:
			{
				switch(g_cxx.pToken->eKeyword)
				{
					case CXXKeywordNAMESPACE:
					{
						int iCurrentScopeKind = cxxScopeGetKind();
						if(iCurrentScopeKind == CXXTagKindNAMESPACE)
						{
							// namespaces can be nested only within themselves
							if(!cxxParserParseNamespace())
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse namespace");
								return FALSE;
							}
						} else {
							// hm... syntax error?
							CXX_DEBUG_LEAVE_TEXT("Found namespace in a wrong place: we're probably out of sync");
							return FALSE;
						}

						//cxxParserNewStatement(); <-- already called by cxxParserParseNamespace()
					}
					break;
					case CXXKeywordTEMPLATE:
						if(!cxxParserParseTemplatePrefix())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse template");
							return FALSE;
						}
						// Here we are just after the "template<parameters>" prefix.
					break;
					case CXXKeywordTYPEDEF:
						// Mark the next declaration as a typedef
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenTypedef;
						cxxTokenChainClear(g_cxx.pTokenChain);
					break;
					case CXXKeywordENUM:
						if(!cxxParserParseEnum())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse enum");
							return FALSE;
						}
					break;
					case CXXKeywordCLASS:
						if(!cxxParserParseClassStructOrUnion(CXXKeywordCLASS,CXXTagKindCLASS))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse class/struct/union");
							return FALSE;
						}
					break;
					case CXXKeywordSTRUCT:
						if(!cxxParserParseClassStructOrUnion(CXXKeywordSTRUCT,CXXTagKindSTRUCT))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse class/struct/union");
							return FALSE;
						}
					break;
					case CXXKeywordUNION:
						if(!cxxParserParseClassStructOrUnion(CXXKeywordUNION,CXXTagKindUNION))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse class/struct/union");
							return FALSE;
						}
					break;
					case CXXKeywordPUBLIC:
					case CXXKeywordPROTECTED:
					case CXXKeywordPRIVATE:
						// Note that the class keyword has its own handler so the only possibility here is an access specifier
						if(!cxxParserParseAccessSpecifier())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse access specifier");
							return FALSE;
						}
					break;
					case CXXKeywordUSING:
						if(!cxxParserParseUsingClause())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse using clause");
							return FALSE;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordIF:
					case CXXKeywordFOR:
					case CXXKeywordWHILE:
					case CXXKeywordSWITCH:
						if(!cxxParserParseIfForWhileSwitch())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse if/for/while/switch");
							return FALSE;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordTRY:
					case CXXKeywordELSE:
					case CXXKeywordDO:
						// parse as normal statement/block
						cxxParserNewStatement();
					break;
					case CXXKeywordRETURN:
						if(cxxParserCurrentLanguageIsCPP())
						{
							// may be followed by a lambda, otherwise it's not interesting.
							g_cxx.uKeywordState |= CXXParserKeywordStateSeenReturn;
							cxxParserNewStatement();
						} else {
							// ignore
							if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse return");
								return FALSE;
							}
							cxxParserNewStatement();
						}
					break;
					case CXXKeywordCONTINUE:
					case CXXKeywordBREAK:
					case CXXKeywordGOTO:
						// ignore
						if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse continue/break/goto");
							return FALSE;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordTHROW:
						// ignore when inside a function
						if(cxxScopeGetKind() == CXXTagKindFUNCTION)
						{
							if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse return/continue/break");
								return FALSE;
							}
							cxxParserNewStatement();
						}
					break;
					break;
					case CXXKeywordCASE:
						// ignore
						if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF | CXXTokenTypeSingleColon))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse case keyword");
							return FALSE;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordEXTERN:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenExtern;
						cxxTokenChainClear(g_cxx.pTokenChain);
					break;
					case CXXKeywordSTATIC:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenStatic;
						cxxTokenChainClear(g_cxx.pTokenChain);
					break;
					case CXXKeywordINLINE:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenInline;
						cxxTokenChainClear(g_cxx.pTokenChain);
					break;
					case CXXKeywordEXPLICIT:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenExplicit;
						cxxTokenChainClear(g_cxx.pTokenChain);
					break;
					case CXXKeywordOPERATOR:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenOperator;
					break;
					case CXXKeywordVIRTUAL:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenVirtual;
					break;
					default:
						if(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef)
						{
							g_cxx.uKeywordState &= ~CXXParserKeywordStateSeenTypedef;
							if(!cxxParserParseGenericTypedef())
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse generic typedef");
								return FALSE;
							}
							cxxParserNewStatement();
						}
					break;
				}
			}
			break;
			case CXXTokenTypeSemicolon:
			{
				if(
						(g_cxx.eLanguage == g_cxx.eCLanguage) &&
						cxxScopeIsGlobal() &&
						(!(g_cxx.uKeywordState & CXXParserKeywordStateSeenExtern)) &&
						(!(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef))
					)
				{
					// Special handling of K&R style function declarations.
					// We might be in the following situation:
					//  
					//  type whatever fname(par1,par2) int par1; int par2; {
					//                                        ^
					//
					switch(cxxParserMaybeExtractKnRStyleFunctionDefinition())
					{
						case 1:
							// got K&R style function definition, one scope was pushed.
							cxxParserNewStatement();
							if(!cxxParserParseBlock(TRUE))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse nested block");
								return FALSE;
							}
							cxxScopePop();
						break;
						case 0:
							// something else
							cxxParserAnalyzeOtherStatement();
						break;
						default:
							CXX_DEBUG_LEAVE_TEXT("Failed to check for K&R style function definition");
							return FALSE;
						break;
					}
				} else {
					// K&R style function declarations not allowed here.
					cxxParserAnalyzeOtherStatement();
				}
				cxxParserNewStatement();
			}
			break;
			case CXXTokenTypeSingleColon:
			{
				// label ?
				if((g_cxx.pTokenChain->iCount == 2) && (cxxTokenChainFirst(g_cxx.pTokenChain)->eType == CXXTokenTypeIdentifier))
				{
					CXXToken * pFirst = cxxTokenChainFirst(g_cxx.pTokenChain);
					// assume it's label
					tagEntryInfo * tag = cxxTagBegin(
							vStringValue(pFirst->pszWord),
							CXXTagKindLABEL,
							pFirst
						);
					
					if(tag)
					{
						tag->isFileScope = TRUE;
	
						cxxTagCommit();
					}
				} else {
					// what is this? (default: and similar things have been handled at keyword level)
				}
			}
			break;
			case CXXTokenTypeOpeningBracket:
				if(!cxxParserParseBlockHandleOpeningBracket())
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to handle opening bracket");
					return FALSE;
				}
			break;
			case CXXTokenTypeClosingBracket:
				// scope finished
				CXX_DEBUG_LEAVE_TEXT("Closing bracket!");
				cxxParserNewStatement();
				return TRUE;
			break;
			case CXXTokenTypeOpeningParenthesis:
			case CXXTokenTypeOpeningSquareParenthesis:
				if(!cxxParserParseAndCondenseCurrentSubchain(
						CXXTokenTypeOpeningBracket | CXXTokenTypeOpeningParenthesis | CXXTokenTypeOpeningSquareParenthesis,
						TRUE
					))
				{
					CXX_DEBUG_LEAVE_TEXT("Parsing the parenthesis failed");
					return FALSE;
				}

				if(g_cxx.pToken->eType == CXXTokenTypeEOF)
				{
					if(bExpectClosingBracket)
					{
						CXX_DEBUG_LEAVE_TEXT("Syntax error: found EOF in block but a closing bracket was expected!");
						return FALSE;
					}
					return TRUE; // EOF
				}
			break;
			case CXXTokenTypeIdentifier:
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef)
				{
					g_cxx.uKeywordState &= ~CXXParserKeywordStateSeenTypedef;
					if(!cxxParserParseGenericTypedef())
					{
						CXX_DEBUG_LEAVE_TEXT("Failed to parse generic typedef");
						return FALSE;
					}
					cxxParserNewStatement();
				}
			break;
			default:
				// something else we didn't handle
			break;
		}
	}
	
	CXX_DEBUG_LEAVE_TEXT("WARNING: Not reached");
	return TRUE;
}
