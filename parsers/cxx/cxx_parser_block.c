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
#include "../cpreprocessor.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"

#include "cxx_subparser_internal.h"

#include <string.h>

bool cxxParserParseBlockHandleOpeningBracket(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(
			g_cxx.pToken->eType == CXXTokenTypeOpeningBracket,
			"This must be called when pointing at an opening bracket!"
		);

	enum CXXScopeType eScopeType = cxxScopeGetType();
	bool bIsCPP = cxxParserCurrentLanguageIsCPP();
	CXXToken * pAux;

	if(
			(
				// something = {...}
				(g_cxx.pToken->pPrev) &&
				cxxTokenTypeIs(g_cxx.pToken->pPrev,CXXTokenTypeAssignment) &&
				(
					(eScopeType == CXXScopeTypeFunction) ||
					(eScopeType == CXXScopeTypeClass)    ||
					(eScopeType == CXXScopeTypeStruct)   ||
					(eScopeType == CXXScopeTypeUnion)    ||
					(eScopeType == CXXScopeTypeNamespace)
				)
			) || (
				bIsCPP &&
				(g_cxx.pToken->pPrev) &&
				(
					(
						// T { arg1, arg2, ... } (1)
						// T object { arg1, arg2, ... } (2)
						// new T { arg1, arg2, ... } (3)
						// Class::Class() : member { arg1, arg2, ... } { (4)
						cxxTokenTypeIs(g_cxx.pToken->pPrev,CXXTokenTypeIdentifier) &&
						(
							// case 1
							(!g_cxx.pToken->pPrev->pPrev) ||
							// case 4
							cxxTokenTypeIsOneOf(
									g_cxx.pToken->pPrev->pPrev,
									CXXTokenTypeSingleColon | CXXTokenTypeComma
							) ||
							// cases 1,2,3 but not 4
							(
								// more parts of typename or maybe the "new" keyword before the identifier
								cxxTokenTypeIsOneOf(
										g_cxx.pToken->pPrev->pPrev,
										CXXTokenTypeIdentifier | CXXTokenTypeStar | CXXTokenTypeAnd |
										CXXTokenTypeGreaterThanSign | CXXTokenTypeKeyword
								) &&
								// but no parenthesis (discard things like bool test() Q_DECL_NO_THROW { ... })
								(!(pAux = cxxTokenChainPreviousTokenOfType(
										g_cxx.pToken->pPrev->pPrev,
										CXXTokenTypeParenthesisChain
									))
								)
							)
						) &&
						// "override" is handled as identifier since it's a keyword only after function signatures
						(strcmp(vStringValue(g_cxx.pToken->pPrev->pszWord),"override") != 0)
					) || (
						// type var[][][]..[] { ... }
						// (but not '[] { ... }' which is a parameterless lambda)
						cxxTokenTypeIs(g_cxx.pToken->pPrev,CXXTokenTypeSquareParenthesisChain) &&
						(
							pAux = cxxTokenChainPreviousTokenNotOfType(
									g_cxx.pToken->pPrev,
									CXXTokenTypeSquareParenthesisChain
								)
						) &&
						cxxTokenTypeIs(pAux,CXXTokenTypeIdentifier)
					)
				)
			) || (
				// return { }
				(!g_cxx.pToken->pPrev) &&
				(g_cxx.uKeywordState & CXXParserKeywordStateSeenReturn)
			)
		)
	{
		// array or list-like initialisation
		bool bRet = cxxParserParseAndCondenseCurrentSubchain(
				CXXTokenTypeOpeningBracket | CXXTokenTypeOpeningParenthesis |
					CXXTokenTypeOpeningSquareParenthesis,
				false,
				true
			);

		CXX_DEBUG_LEAVE_TEXT("Handled array or list-like initialisation or return");
		return bRet;
	}

	// In C++ mode check for lambdas
	CXXToken * pParenthesis;

	if(
		bIsCPP &&
		(pParenthesis = cxxParserOpeningBracketIsLambda())
	)
	{
		if(!cxxParserHandleLambda(pParenthesis))
		{
			CXX_DEBUG_LEAVE_TEXT("Lambda handling failed");
			return false;
		}

		// Note that here we're leaving the token chain "alive" so further parsing can be performed.
		CXX_DEBUG_LEAVE_TEXT("Lambda handling succeeded");
		return true;
	}

	int iScopes;
	int iCorkQueueIndex = CORK_NIL;
	int iCorkQueueIndexFQ = CORK_NIL;

	CXXFunctionSignatureInfo oInfo;

	if(eScopeType != CXXScopeTypeFunction)
	{
		// very likely a function definition
		// (but may be also a toplevel block, like "extern "C" { ... }")
		iScopes = cxxParserExtractFunctionSignatureBeforeOpeningBracket(&oInfo,&iCorkQueueIndex,&iCorkQueueIndexFQ);

		// FIXME: Handle syntax (5) of list initialization:
		//        Class::Class() : member { arg1, arg2, ... } {...
	} else {
		// some kind of other block:
		// - anonymous block
		// - block after for(),while(),foreach(),if() and other similar stuff
		// (note that {}-style initializers have been handled above and thus are excluded)

		iScopes = 0;
	}

	cxxParserNewStatement();

	if(!cxxParserParseBlock(true))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse nested block");
		return false;
	}

	if(iScopes < 1)
	{
		CXX_DEBUG_LEAVE_TEXT("The block was not a function");
		return true;
	}

	unsigned long uEndPosition = getInputLineNumber();

	// If the function contained a "try" keyword before the opening bracket
	// then it's likely to be a function-try-block and should be followed by a catch
	// block that is in the same scope.

	if(oInfo.uFlags & CXXFunctionSignatureInfoFunctionTryBlock)
	{
		// look for the catch blocks.
		CXX_DEBUG_PRINT("The function is a function-try-block: looking for catch blocks");

		for(;;)
		{
			CXX_DEBUG_PRINT("Looking ahead for a catch block...");

			if(!cxxParserParseNextToken())
				break; // EOF

			if(!cxxTokenIsKeyword(g_cxx.pToken,CXXKeywordCATCH))
			{
				// No more catches. Unget and exit.
				CXX_DEBUG_PRINT("No more catch blocks");
				cxxParserUngetCurrentToken();
				break;
			}

			// assume it's a catch block.

			CXX_DEBUG_PRINT("Found catch block");

			if(!cxxParserParseIfForWhileSwitchCatchParenthesis())
			{
				CXX_DEBUG_LEAVE_TEXT("Failed to parse the catch parenthesis");
				return false;
			}

			// the standard requires a bracket here (catch block is always a compound statement).

			cxxParserNewStatement();

			if(!cxxParserParseNextToken())
			{
				CXX_DEBUG_LEAVE_TEXT("Found EOF while looking for catch() block: playing nice");
				break; // EOF (would be a syntax error!)
			}

			if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeOpeningBracket))
			{
				// Aargh...
				CXX_DEBUG_LEAVE_TEXT("Found something unexpected while looking for catch() block: playing nice");
				cxxParserUngetCurrentToken();
				break; // (would be a syntax error!)
			}

			if(!cxxParserParseBlock(true))
				return false;

 			uEndPosition = getInputLineNumber();
 		}
 	}

	if(iCorkQueueIndex > CORK_NIL)
	{
		cxxParserSetEndLineForTagInCorkQueue(iCorkQueueIndex,uEndPosition);
		if(iCorkQueueIndexFQ > CORK_NIL)
			cxxParserSetEndLineForTagInCorkQueue(iCorkQueueIndexFQ,uEndPosition);
	}
	while(iScopes > 0)
	{
		cxxScopePop();
		iScopes--;
	}

	CXX_DEBUG_LEAVE();
	return true;
}

static bool cxxParserParseBlockInternal(bool bExpectClosingBracket)
{
	CXX_DEBUG_ENTER();

	//char * szScopeName = cxxScopeGetFullName();
	//CXX_DEBUG_PRINT("Scope name is '%s'",szScopeName ? szScopeName : "");

	cxxParserNewStatement();

	if(bExpectClosingBracket)
	{
		// FIXME: this cpp handling is kind of broken:
		//        it works only because the moon is in the correct phase.
		cppBeginStatement();
	}

	for(;;)
	{
		if(!cxxParserParseNextToken())
		{
found_eof:

			if(bExpectClosingBracket)
			{
				CXX_DEBUG_LEAVE_TEXT(
						"Syntax error: found EOF in block but a closing " \
							"bracket was expected!"
					);
				return false;
			}

			CXX_DEBUG_LEAVE_TEXT("EOF in main block");
			return true; // EOF
		}

process_token:

		CXX_DEBUG_PRINT(
				"Token '%s' of type 0x%02x",
				vStringValue(g_cxx.pToken->pszWord),
				g_cxx.pToken->eType
			);

		if (cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier)
			&& cxxScopeGetType() == CXXScopeTypeClass
			&& cxxSubparserNewIdentifierAsHeadOfMemberNotify(g_cxx.pToken))
		{
			cxxTokenChainDestroyLast(g_cxx.pTokenChain);
			continue;
		}

		switch(g_cxx.pToken->eType)
		{
			case CXXTokenTypeKeyword:
			{
				switch(g_cxx.pToken->eKeyword)
				{
					case CXXKeywordNAMESPACE:
					{
						enum CXXScopeType eScopeType = cxxScopeGetType();

						if(
								(
									// toplevel or nested within a namespace
									(eScopeType == CXXScopeTypeNamespace) ||
									// namespace X = Y inside a function
									(eScopeType == CXXScopeTypeFunction)
								) && (
									// either certainly C++
									g_cxx.bConfirmedCPPLanguage ||
									// or a "sane" namespace syntax
									(
										!cxxTokenChainPreviousTokenOfType(
												g_cxx.pToken,
												CXXTokenTypeStar |
												CXXTokenTypeAnd |
												CXXTokenTypeKeyword
											)
									)
								)
							)
						{
							if(!cxxParserParseNamespace())
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse namespace");
								return false;
							}
						} else {
							// If we're pretty sure this is C++ then this is a syntax error.
							// If we're not sure (namely when we're in a *.h file) then
							// let's try to be flexible: treat the namespace keyword as an identifier.
							if(!g_cxx.bConfirmedCPPLanguage)
							{
								CXX_DEBUG_LEAVE_TEXT(
									"Found namespace in unexpected place, but we're not sure it's really C++ "
									"so we'll treat it as an identifier instead"
								);
								g_cxx.pToken->eType = CXXTokenTypeIdentifier;
								continue;
							}

							CXX_DEBUG_LEAVE_TEXT(
								"Found namespace in a wrong place: we're probably out of sync"
							);
							return false;
						}

						cxxParserNewStatement();
					}
					break;
					case CXXKeywordTEMPLATE:
						if(
							// beginning of the statement
							(!g_cxx.pToken->pPrev) ||
							// previous token is not "." or "->", syntax that is found in
							//     p.template func<int>();
							(!cxxTokenTypeIsOneOf(
									g_cxx.pToken->pPrev,
									CXXTokenTypeDotOperator | CXXTokenTypePointerOperator
								)
							)
						)
						{
							if(!cxxParserParseTemplatePrefix())
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse template");
								return false;
							}
							// Here we are just after the "template<parameters>" prefix.
						} else {
							CXX_DEBUG_LEAVE_TEXT("Template keyword that is not a prefix");
						}
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
							return false;
						}
					break;
					case CXXKeywordCLASS:
						if(
							// do not trigger on X<class Y>
							(!g_cxx.pToken->pPrev) ||
							(!cxxTokenTypeIsOneOf(g_cxx.pToken->pPrev,CXXTokenTypeSmallerThanSign | CXXTokenTypeComma))
						)
						{
							if(!cxxParserParseClassStructOrUnion(CXXKeywordCLASS,CXXTagCPPKindCLASS,CXXScopeTypeClass))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse class/struct/union");
								return false;
							}
						}
					break;
					case CXXKeywordSTRUCT:
						if(
							// do not trigger on X<struct Y>
							(!g_cxx.pToken->pPrev) ||
							(!cxxTokenTypeIsOneOf(g_cxx.pToken->pPrev,CXXTokenTypeSmallerThanSign | CXXTokenTypeComma))
						)
						{
							if(!cxxParserParseClassStructOrUnion(CXXKeywordSTRUCT,CXXTagKindSTRUCT,CXXScopeTypeStruct))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse class/struct/union");
								return false;
							}
						}
					break;
					case CXXKeywordUNION:
						if(
							// do not trigger on X<union Y>
							(!g_cxx.pToken->pPrev) ||
							(!cxxTokenTypeIsOneOf(g_cxx.pToken->pPrev,CXXTokenTypeSmallerThanSign | CXXTokenTypeComma))
						)
						{
							if(!cxxParserParseClassStructOrUnion(CXXKeywordUNION,CXXTagKindUNION,CXXScopeTypeUnion))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse class/struct/union");
								return false;
							}
						}
					break;
					case CXXKeywordPUBLIC:
					case CXXKeywordPROTECTED:
					case CXXKeywordPRIVATE:
						// Note that the class keyword has its own handler
						// so the only possibility here is an access specifier
						if(!cxxParserParseAccessSpecifier())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse access specifier");
							return false;
						}
					break;
					case CXXKeywordUSING:
						if(!cxxParserParseUsingClause())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse using clause");
							return false;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordIF:
					case CXXKeywordFOR:
					case CXXKeywordWHILE:
					case CXXKeywordSWITCH:
					case CXXKeywordCATCH:
						if(!cxxParserParseIfForWhileSwitchCatchParenthesis())
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse if/for/while/switch/catch parenthesis");
							return false;
						}
						// Now we're just before the block that follows the parenthesis.
						cxxParserNewStatement();
						// Force the cpp preprocessor to think that we're in the middle of a statement.
						cppBeginStatement();
					break;
					case CXXKeywordTRY:
						// We parse try in different ways depending on the context.
						// Inside a function, and without preceding tokens it's assumed to be
						// a plain try {} catch {} block. This is easy.
						// Out of a function it's likely to be a function try block:
						//    int f(int n = 2) try { ... } catch { ... }
						// Inside a function but with some preceding tokens it's likely to be a
						// lambda expressed as function-try-block.
						//    auto f() -> void try { ... } catch { ... }
						if((cxxScopeGetType() != CXXScopeTypeFunction) || g_cxx.pToken->pPrev)
						{
							CXX_DEBUG_PRINT("Found try that looks like a function-try-block");
							// Maybe function-try-block.
							// Keep in the chain and continue parsing.
							continue;
						}
						// Fall through.
					case CXXKeywordELSE:
					case CXXKeywordDO:
						// parse as normal statement/block
						cxxParserNewStatement();
						// Force the cpp preprocessor to think that we're in the middle of a statement.
						cppBeginStatement();
					break;
					case CXXKeywordRETURN:
						if(cxxParserCurrentLanguageIsCPP())
						{
							// may be followed by a lambda, otherwise it's not interesting.
							cxxParserNewStatement();
							g_cxx.uKeywordState |= CXXParserKeywordStateSeenReturn;
						} else {
							// ignore
							if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF,
								   false))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse return");
								return false;
							}
							cxxParserNewStatement();
						}
					break;
					case CXXKeywordCONTINUE:
					case CXXKeywordBREAK:
					case CXXKeywordGOTO:
						// ignore
						if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF,
							   false))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse continue/break/goto");
							return false;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordTHROW:
						// We ignore whole "throw expressions" as they contain nothing useful
						// and may confuse us. We keep "throw" when used as exception specification,
						// and this is certainly outside of a function and when the token chain
						// already contains at least a type, an identifier and a parenthesis.
						// This check seems excessive but keep in mind that we deal with
						// broken input and we might also be wrong about the current scope.
						if((cxxScopeGetType() == CXXScopeTypeFunction) || (g_cxx.pTokenChain->iCount < 3))
						{
							CXX_DEBUG_PRINT("Skipping throw statement");
							if(!cxxParserParseUpToOneOf(CXXTokenTypeSemicolon | CXXTokenTypeEOF,
								   false))
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to skip throw statement");
								return false;
							}
							cxxParserNewStatement();
						}
					break;
					case CXXKeywordCASE:
						// ignore
						if(!cxxParserParseUpToOneOf(
								CXXTokenTypeSemicolon | CXXTokenTypeEOF | CXXTokenTypeSingleColon,
								false
							))
						{
							CXX_DEBUG_LEAVE_TEXT("Failed to parse case keyword");
							return false;
						}
						cxxParserNewStatement();
					break;
					case CXXKeywordEXTERN:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenExtern;

						cxxTokenChainDestroyLast(g_cxx.pTokenChain);

						if(!cxxParserParseNextToken())
							goto found_eof;

						if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeStringConstant))
						{
							// assume extern "language"

							// Strictly speaking this is a C++ only syntax.
							// However we allow it also in C as it doesn't really hurt.

							cxxTokenChainDestroyLast(g_cxx.pTokenChain);

							// Note that extern "C" may be followed by a block with declarations
							//
							//   extern "C" { ... }
							//
							// However in this case the declarations are ALSO definitions
							// and extern "C" is used only to specify the name mangling mode.
							//
							//   extern "C" int x; <-- a declaration and not a definition
							//   extern "C" { int x; } <-- a declaration and definition: x IS defined
							//                             here and is NOT extern.
							//
							// A variable in an extern "C" block has to be re-declared extern again
							// to be really treated as declaration only.
							//
							//   extern "C" { extern int x; }
							//
							// So in this case we do NOT treat the inner declarations as extern
							// and we don't need specific handling code for this case.
						} else {
							// something else: handle it the normal way
							goto process_token;
						}
					break;
					case CXXKeywordSTATIC:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenStatic;
						cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					break;
					case CXXKeywordINLINE:
					case CXXKeyword__INLINE:
					case CXXKeyword__INLINE__:
					case CXXKeyword__FORCEINLINE:
					case CXXKeyword__FORCEINLINE__:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenInline;
						cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					break;
					case CXXKeywordEXPLICIT:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenExplicit;
						cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					break;
					case CXXKeywordOPERATOR:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenOperator;
					break;
					case CXXKeywordVIRTUAL:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenVirtual;
						cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					break;
					case CXXKeywordMUTABLE:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenMutable;
						cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					break;
					case CXXKeywordFRIEND:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenFriend;
					break;
					// "const" and "volatile" are part of the type. Don't treat them specially
					// and don't attempt to extract an eventual typedef yet,
					// as there might be a struct/class/union keyword following.
					case CXXKeywordVOLATILE:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenVolatile;
					break;
					case CXXKeywordCONST:
						g_cxx.uKeywordState |= CXXParserKeywordStateSeenConst;
					break;
					default:
						if(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef)
						{
							g_cxx.uKeywordState &= ~CXXParserKeywordStateSeenTypedef;
							if(!cxxParserParseGenericTypedef())
							{
								CXX_DEBUG_LEAVE_TEXT("Failed to parse generic typedef");
								return false;
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
						(cxxParserCurrentLanguageIsC()) &&
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
					switch(cxxParserMaybeParseKnRStyleFunctionDefinition())
					{
						case 1:
							// K&R parser did the job and started a new statement
						break;
						case 0:
							// something else
							cxxParserAnalyzeOtherStatement();
						break;
						default:
							CXX_DEBUG_LEAVE_TEXT("Failed to check for K&R style function definition");
							return false;
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
				if(
						(g_cxx.pTokenChain->iCount == 2) &&
						cxxTokenTypeIs(
								cxxTokenChainFirst(g_cxx.pTokenChain),
								CXXTokenTypeIdentifier
							)
					)
				{
					CXXToken * pFirst = cxxTokenChainFirst(g_cxx.pTokenChain);
					// assume it's label
					tagEntryInfo * tag = cxxTagBegin(CXXTagKindLABEL,pFirst);

					if(tag)
					{
						tag->isFileScope = true;
						cxxTagCommit(NULL);
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
					return false;
				}
			break;
			case CXXTokenTypeClosingBracket:
				// scope finished
				if(!bExpectClosingBracket)
				{
					CXX_DEBUG_LEAVE_TEXT(
						"Found unexpected closing bracket: probably preprocessing problem"
					);
					return false;
				}
				CXX_DEBUG_LEAVE_TEXT("Closing bracket!");
				cxxParserNewStatement();
				return true;
			break;
			case CXXTokenTypeOpeningParenthesis:
			case CXXTokenTypeOpeningSquareParenthesis:
				if(!cxxParserParseAndCondenseCurrentSubchain(
						CXXTokenTypeOpeningBracket | CXXTokenTypeOpeningParenthesis |
							CXXTokenTypeOpeningSquareParenthesis,
						true,
						false
					))
				{
					CXX_DEBUG_LEAVE_TEXT("Parsing the parenthesis failed");
					return false;
				}

				if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeEOF))
				{
					if(bExpectClosingBracket)
					{
						CXX_DEBUG_LEAVE_TEXT(
								"Syntax error: found EOF in block but a closing bracket was expected!"
							);
						return false;
					}
					return true; // EOF
				}
			break;
			case CXXTokenTypeIdentifier:
				if(g_cxx.uKeywordState & CXXParserKeywordStateSeenTypedef)
				{
					g_cxx.uKeywordState &= ~CXXParserKeywordStateSeenTypedef;
					if(!cxxParserParseGenericTypedef())
					{
						CXX_DEBUG_LEAVE_TEXT("Failed to parse generic typedef");
						return false;
					}
					cxxParserNewStatement();
				}
				else if (cxxScopeGetType() == CXXScopeTypeClass)
					cxxSubparserUnknownIdentifierInClassNotify(g_cxx.pToken);
			break;
			default:
				// something else we didn't handle
			break;
		}
	}

	CXX_DEBUG_LEAVE_TEXT("WARNING: Not reached");
	return true;
}

//
// This is the toplevel scanning function. It's a forward-only scanner that keeps
// accumulating tokens in the chain until either a characteristic token is found
// or the statement ends. When a characteristic token is found it usually enters
// a specialized scanning routine (e.g for classes, namespaces, structs...).
// When the statement ends without finding any characteristic token the chain
// is passed to an analysis routine which does a second scan pass.
//
bool cxxParserParseBlock(bool bExpectClosingBracket)
{
	cxxSubparserNotifyEnterBlock ();

	cppPushExternalParserBlock();
	bool bRet = cxxParserParseBlockInternal(bExpectClosingBracket);
	cppPopExternalParserBlock();

	cxxSubparserNotifyLeaveBlock ();

	return bRet;
}
