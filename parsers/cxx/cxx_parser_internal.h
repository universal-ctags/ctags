#ifndef ctags_cxx_parser_internal_h_
#define ctags_cxx_parser_internal_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "parse.h"

#include "cxx_tag.h"
#include "cxx_keyword.h"
#include "cxx_token.h"
#include "cxx_token_chain.h"

//
// CXX parser internal declarations.
// This file is included only by cxx_parser_*.c
//

// cxx_parser_tokenizer.c
boolean cxxParserParseNextToken(void);

// cxx_parser_lambda.c
CXXToken * cxxParserOpeningBracketIsLambda(void);
boolean cxxParserHandleLambda(CXXToken * pParenthesis);

// cxx_parser_block.c
boolean cxxParserParseBlock(boolean bExpectClosingBracket);

enum CXXExtractVariableDeclarationsFlags
{
	// We are parsing K&R style parameter declarations.
	CXXExtractVariableDeclarationsKnRStyleParameters = 1
};

// cxx_parser_variable.c
boolean cxxParserExtractVariableDeclarations(CXXTokenChain * pChain,unsigned int uFlags);

// cxx_parser_function.c

//
// Description of a function signature.
//
typedef struct _CXXFunctionSignatureInfo
{
	// The parenthesis token. Note that in some special cases it may belong to a subchain.
	CXXToken * pParenthesis;

	// The identifier. It's either a single token (so both pIdentifierStart and pIdentifierEnd point to the same token)
	// or multiple tokens starting with the "operator" keyword. Spacing of the tokens is adjusted.
	CXXToken * pIdentifierStart;
	CXXToken * pIdentifierEnd;
	
	// Non-NULL if the signature is followed by the "const" keyword
	CXXToken * pSignatureConst;

	// Non-NULL if there is a scope before the identifier.
	// The scope ends at pIdentifierStart.
	CXXToken * pScopeStart;

} CXXFunctionSignatureInfo;

int cxxParserMaybeExtractKnRStyleFunctionDefinition(void);
int cxxParserExtractFunctionSignatureBeforeOpeningBracket(void);

#define CXX_MAX_EXTRACTED_PARAMETERS 24

typedef struct _CXXFunctionParameterInfo
{
	// The number of parameters found
	unsigned int uParameterCount;
	
	// All the tokens are references to the source chain (do not delete)
	CXXTokenChain * pChain;
	// The initial tokens of the declaration
	CXXToken * aDeclarationStarts[CXX_MAX_EXTRACTED_PARAMETERS];
	// The final tokens of the declaration
	CXXToken * aDeclarationEnds[CXX_MAX_EXTRACTED_PARAMETERS];
	// The identifier tokens (betweeh initial and final)
	CXXToken * aIdentifiers[CXX_MAX_EXTRACTED_PARAMETERS];
} CXXFunctionParameterInfo;

boolean cxxParserTokenChainLooksLikeFunctionParameterList(CXXTokenChain * tc,CXXFunctionParameterInfo * pParamInfo);
boolean cxxParserLookForFunctionSignature(CXXTokenChain * pChain,CXXFunctionSignatureInfo * pInfo,CXXFunctionParameterInfo * pParamInfo);

enum CXXEmitFunctionTagsOptions
{
	// Push the scopes defined by the function
	CXXEmitFunctionTagsPushScopes = 1
};

int cxxParserEmitFunctionTags(
		CXXFunctionSignatureInfo * pInfo,
		enum CXXTagKind eTagKind,
		unsigned int uOptions
	);

void cxxParserEmitFunctionParameterTags(CXXFunctionParameterInfo * pInfo);

// cxx_parser_typedef.c
boolean cxxParserParseGenericTypedef(void);
void cxxParserExtractTypedef(CXXTokenChain * pChain,boolean bExpectTerminatorAtEnd);


// cxx_parser.c
void cxxParserNewStatement(void);
boolean cxxParserParseNamespace(void);
boolean cxxParserParseEnum(void);
boolean cxxParserParseClassStructOrUnion(enum CXXKeyword eKeyword,enum CXXTagKind eTagKind);
boolean cxxParserParseAndCondenseCurrentSubchain(unsigned int uInitialSubchainMarkerTypes,boolean bAcceptEOF);
boolean cxxParserParseUpToOneOf(unsigned int uTokenTypes);
boolean cxxParserParseIfForWhileSwitch(void);
boolean cxxParserParseTemplatePrefix(void);
boolean cxxParserParseUsingClause(void);
boolean cxxParserParseAccessSpecifier(void);
void cxxParserAnalyzeOtherStatement(void);
boolean cxxParserParseAndCondenseSubchainsUpToOneOf(unsigned int uTokenTypes,unsigned int uInitialSubchainMarkerTypes);

typedef enum _CXXParserKeywordState
{
	// We are parsing a statement that comes right after a typedef keyword (so we're parsing the type being typedef'd).
	CXXParserKeywordStateSeenTypedef = 1,
	// We are parsing a statement that comes right after an inline keyword
	CXXParserKeywordStateSeenInline = 2,
	// We are parsing a statement that comes right after a extern keyword
	CXXParserKeywordStateSeenExtern = 4,
	// We are parsing a statement that comes right after a static keyword
	CXXParserKeywordStateSeenStatic = 8,
	// an "explicit" keyword has been seen
	CXXParserKeywordStateSeenExplicit = 16,
	// an "operator" keyword has been seen
	CXXParserKeywordStateSeenOperator = 32,
	// "virtual" has been seen
	CXXParserKeywordStateSeenVirtual = 64,
	// "return" has been seen
	CXXParserKeywordStateSeenReturn = 128
} CXXParserKeywordState;


typedef struct _CXXParserState
{
	// The current language
	langType eLanguage;
	
	// The identifier of the CPP language, as indicated by ctags core
	langType eCPPLanguage;
	// The identifier of the C language, as indicated by ctags core
	langType eCLanguage;

	// The current token chain
	CXXTokenChain * pTokenChain;

	// The last template we found
	CXXTokenChain * pTemplateTokenChain;
	
	// The last token we have extracted. This is always pushed to the token chain tail (which will take care of deletion)
	CXXToken * pToken; // the token chain tail

	// The last char we have extracted from input
	int iChar;
	
	// Toplevel keyword state. A combination of CXXParserKeywordState flags.
	// Please note that the keywords appearing inside a () subchain are NOT marked.
	unsigned int uKeywordState;
	
	// This is used to handle the special case of template angle brackets.
	// The less than and greater than signs must be always treated as single characters
	// and not joined in multiple groups.
	boolean bParsingTemplateAngleBrackets;
	
	// This is used to handle the special case of "final" which is a keyword
	// in class/struct/union declarations but not anywhere else
	boolean bParsingClassStructOrUnionDeclaration;
	
} CXXParserState;



// defined in cxx_parser.c
extern CXXParserState g_cxx;

#define cxxParserCurrentLanguageIsCPP() \
	(g_cxx.eLanguage == g_cxx.eCPPLanguage)

#define cxxParserCurrentLanguageIsC() \
	(g_cxx.eLanguage == g_cxx.eCLanguage)

#endif //!ctags_cxx_parser_internal_h_
