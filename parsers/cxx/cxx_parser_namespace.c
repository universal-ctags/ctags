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



boolean cxxParserParseNamespace(void)
{
	CXX_DEBUG_ENTER();

	// FIXME: Do it better...

	cxxParserNewStatement(); // always a new statement

	/*
		Spec is:
	
			namespace ns_name { declarations }	(1)	
			inline namespace ns_name { declarations }	(2)	(since C++11)
			namespace { declarations }	(3)	
			ns_name::name	(4)	
			using namespace ns_name;	(5)	
			using ns_name::name;	(6)	
			namespace name = qualified-namespace ;	(7)	
			namespace ns_name::name	(8)	(since C++17)
	*/

	// namespace <X> {
	// namespace <X>::<Y>::<Z> {
	// namespace <X>::<Y>::<Z>;
	// namespace <X>;
	// namespace;

	int iScopeCount = 0;

	for(;;)
	{
		if(!cxxParserParseNextToken())
		{
			// syntax error, but we tolerate this
			CXX_DEBUG_LEAVE_TEXT("Implicit EOF in cxxParserParseNextToken");
			return TRUE; // EOF
		}

		switch(g_cxx.pToken->eType)
		{
			case CXXTokenTypeIdentifier:
				CXX_DEBUG_PRINT("Got identifier %s",g_cxx.pToken->pszWord->buffer);
				tagEntryInfo * tag = cxxTagBegin(g_cxx.pToken->pszWord->buffer,CXXTagKindNAMESPACE,g_cxx.pToken);
				if(tag)
				{
					// This is highly questionable but well.. it's how old ctags did, so we do.
					tag->isFileScope = !isInputHeaderFile();
					cxxTagCommit();
				}
				cxxScopePush(cxxTokenChainTakeLast(g_cxx.pTokenChain),CXXTagKindNAMESPACE,CXXScopeAccessUnknown);
				iScopeCount++;
			break;
			case CXXTokenTypeMultipleColons:
				CXX_DEBUG_PRINT("Got multiple colons");
				// ok, skip
			break;
			case CXXTokenTypeSemicolon:
				// doh.. but tolerate this
				CXX_DEBUG_LEAVE_TEXT("got semicolon!");
				while(iScopeCount > 0)
				{
					cxxScopePop();
					iScopeCount--;
				}
				cxxTokenChainClear(g_cxx.pTokenChain);
				return TRUE;
			break;
			case CXXTokenTypeOpeningBracket:
				CXX_DEBUG_PRINT("Got opening bracket!");
				
				if(iScopeCount == 0)
				{
					// anonymous namespace!
					CXXToken * t = cxxTokenCreateAnonymousIdentifier(CXXTagKindNAMESPACE);
					tagEntryInfo * tag = cxxTagBegin(t->pszWord->buffer,CXXTagKindNAMESPACE,t);
					if(tag)
					{
						tag->isFileScope = !isInputHeaderFile();
						cxxTagCommit();
					}
					cxxScopePush(t,CXXTagKindNAMESPACE,CXXScopeAccessUnknown);
					iScopeCount++;
				}
				
				if(!cxxParserParseBlock(TRUE))
				{
					CXX_DEBUG_LEAVE_TEXT("Failed to parse scope");
					return FALSE;
				}
				
				while(iScopeCount > 0)
				{
					cxxScopePop();
					iScopeCount--;
				}

				if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeClosingBracket))
				{
					CXX_DEBUG_LEAVE_TEXT("Got closing bracket after scope");
					return TRUE;
				}
				if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
				{
					CXX_DEBUG_LEAVE_TEXT("Got EOF after scope");
					return TRUE;
				}
			break;
			default:
				// syntax error
				CXX_DEBUG_LEAVE_TEXT("Some kind of syntax error");
				return FALSE;
			break;
		}
	}
	
	CXX_DEBUG_LEAVE_TEXT("WARNING: Not reached");
	return TRUE;
}

