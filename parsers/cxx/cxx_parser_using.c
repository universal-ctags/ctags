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
#include "read.h"

bool cxxParserParseUsingClause(void)
{
	CXX_DEBUG_ENTER();

	// using-directives for namespaces and using-declarations
	// for namespace members
	// using-declarations for class members
	// type alias and alias template declaration (since C++11)

	// using namespace ns_name;	(5)	// whole namespace
	// using ns_name::name;	(6)	 // only symbol name
	// using B::g; // inside class, using method g from base class B
	// using identifier attr(optional) = type-id ; <-- equivalent to a typedef!

	cxxTokenChainClear(g_cxx.pTokenChain);

	// skip to the next ; without leaving scope.
	if(!cxxParserParseUpToOneOf(
			CXXTokenTypeSemicolon | CXXTokenTypeClosingBracket | CXXTokenTypeEOF,
			false
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse up to the next ;");
		return false;
	}

	if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeSemicolon))
	{
		CXX_DEBUG_LEAVE_TEXT("This is a syntax error but we tolerate it");
		return true;
	}

	cxxTokenChainDestroyLast(g_cxx.pTokenChain);

	if(g_cxx.pTokenChain->iCount < 1)
	{
		CXX_DEBUG_LEAVE_TEXT("This is a syntax error but we tolerate it");
		return true;
	}

	CXXToken * pAssignment = cxxTokenChainFirstTokenOfType(
			g_cxx.pTokenChain,
			CXXTokenTypeAssignment
		);

	if(pAssignment)
	{
		CXXToken * pFirst = cxxTokenChainFirst(g_cxx.pTokenChain);
		bool bGotTemplate = g_cxx.pTemplateTokenChain &&
			(g_cxx.pTemplateTokenChain->iCount > 0) &&
			cxxParserCurrentLanguageIsCPP();

		if(cxxTokenTypeIs(pFirst,CXXTokenTypeIdentifier))
		{
			CXX_DEBUG_PRINT(
					"Found using clause '%s' which defines a type",
					vStringValue(pFirst->pszWord)
				);

			// It's a typedef. Reorder the tokens in the chain
			// so it really looks like a typedef
			// and pass it to the specialized extraction routine
			cxxTokenChainTake(g_cxx.pTokenChain,pFirst);

			while(cxxTokenChainFirst(g_cxx.pTokenChain) != pAssignment)
				cxxTokenChainDestroyFirst(g_cxx.pTokenChain);
			// kill assignment itself
			cxxTokenChainDestroyFirst(g_cxx.pTokenChain);

			// in typedefs it's at the end
			cxxTokenChainAppend(g_cxx.pTokenChain,pFirst);

			cxxParserExtractTypedef(g_cxx.pTokenChain,false,bGotTemplate);
		}
	} else {
		CXX_DEBUG_ASSERT(
				g_cxx.pTokenChain->iCount > 0,
				"The token chain should be non empty at this point"
			);

		CXXToken * t = cxxTokenChainFirst(g_cxx.pTokenChain);

		bool bUsingNamespace = false;

		if(cxxTokenTypeIs(t,CXXTokenTypeKeyword))
		{
			if(t->eKeyword == CXXKeywordNAMESPACE)
			{
				bUsingNamespace = true;
				cxxTokenChainDestroyFirst(g_cxx.pTokenChain);
			} else if(t->eKeyword == CXXKeywordTYPENAME)
			{
				cxxTokenChainDestroyFirst(g_cxx.pTokenChain);
			}
		}

		if(g_cxx.pTokenChain->iCount > 0)
		{
			tagEntryInfo * tag;

			if(bUsingNamespace)
			{
				cxxTokenChainCondense(g_cxx.pTokenChain,0);

				t = cxxTokenChainFirst(g_cxx.pTokenChain);
				CXX_DEBUG_ASSERT(
						t,
						"Condensation of a non empty chain should produce a token!"
					);


				CXX_DEBUG_PRINT(
						"Found using clause '%s' which extends scope",
						vStringValue(t->pszWord)
					);
				tag = cxxTagBegin(CXXTagCPPKindUSING,t);
			} else {

				t = cxxTokenChainLast(g_cxx.pTokenChain);

				CXX_DEBUG_PRINT(
						"Found using clause '%s' which imports a name",
						vStringValue(t->pszWord)
					);
				tag = cxxTagBegin(CXXTagCPPKindNAME,t);

				// FIXME: We need something like "nameref:<condensed>" here!
			}

			if(tag)
			{
				tag->isFileScope = (cxxScopeGetType() == CXXScopeTypeNamespace) &&
							(!isInputHeaderFile());
				cxxTagCommit(NULL);
			}
		}
	}

	if(!g_cxx.bConfirmedCPPLanguage)
	{
		CXX_DEBUG_PRINT(
				"Succeeded in parsing C++ using: this really seems to be C++"
			);
		g_cxx.bConfirmedCPPLanguage = true;
	}

	CXX_DEBUG_LEAVE();
	return true;
}
