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
#include "ptrarray.h"

#include "cxx_tag.h"
#include "cxx_keyword.h"
#include "cxx_token.h"
#include "cxx_token_chain.h"

//
// CXX parser internal declarations.
// This file is included only by cxx_parser_*.c
//

// CXX parser language. We use a specific enum and not langType
// since we want to be able to use it in bit fields.
typedef enum _CXXLanguage
{
	CXXLanguageC = 1,
	CXXLanguageCPP = (1 << 1),
	CXXLanguageCUDA = (1 << 2)
} CXXLanguage;

// cxx_parser_tokenizer.c
bool cxxParserParseNextToken(void);
void cxxParserUngetCurrentToken(void);

// cxx_parser_lambda.c
CXXToken * cxxParserOpeningBracketIsLambda(void);
bool cxxParserHandleLambda(CXXToken * pParenthesis);

// cxx_parser_block.c
bool cxxParserParseBlock(bool bExpectClosingBracket);
bool cxxParserParseBlockHandleOpeningBracket(void);

enum CXXExtractVariableDeclarationsFlags
{
	// We are parsing K&R style parameter declarations.
	CXXExtractVariableDeclarationsKnRStyleParameters = 1
};

// cxx_parser_variable.c
bool cxxParserExtractVariableDeclarations(
		CXXTokenChain * pChain,
		unsigned int uFlags
	);

CXXToken * cxxParserFindFirstPossiblyNestedAndQualifiedIdentifier(
		CXXTokenChain * pChain,
		CXXTokenChain ** pParentChain
	);

// cxx_parser_function.c

bool cxxParserTokenChainLooksLikeFunctionCallParameterSet(
		CXXTokenChain * pChain
	);
bool cxxParserTokenChainLooksLikeConstructorParameterSet(
		CXXTokenChain * pChain
	);

typedef enum _CXXFunctionSignatureInfoFlag
{
	// Followed by = 0
	CXXFunctionSignatureInfoPure = 1,
	// Followed by = default
	CXXFunctionSignatureInfoDefault = (1 << 1),
	// Followed by "override"
	CXXFunctionSignatureInfoOverride = (1 << 2),
	// Followed by "final"
	CXXFunctionSignatureInfoFinal = (1 << 3),
	// Followed by = delete
	CXXFunctionSignatureInfoDelete = (1 << 4),
	// Followed by volatile
	CXXFunctionSignatureInfoVolatile = (1 << 5),
	// Is template specialization  a<x>()
	CXXFunctionSignatureInfoTemplateSpecialization = (1 << 6),
	// Is scope template specialization a<x>::b()
	// (implies that this is a template specialization too)
	CXXFunctionSignatureInfoScopeTemplateSpecialization = (1 << 7),
	// function-try-block: int f() try { ... } catch { ... }
	CXXFunctionSignatureInfoFunctionTryBlock = (1 << 8),
} CXXFunctionSignatureInfoFlag;

//
// Description of a function signature.
//
typedef struct _CXXFunctionSignatureInfo
{
	// The parenthesis token.
	// It is always contained in the chain pointed by pParenthesisContainerChain
	CXXToken * pParenthesis;

	// The token chain that contains the parenthesis above. May or may not
	// be the toplevel chain.
	CXXTokenChain * pParenthesisContainerChain;

	// The identifier. It's either a single token (so both pIdentifierStart
	// and pIdentifierEnd point to the same token) or multiple tokens starting
	// with the "operator" keyword. Spacing of the tokens is adjusted.
	// The identifier is always contained in the chain pointed by pIdentifierChain.
	CXXToken * pIdentifierStart;
	CXXToken * pIdentifierEnd;

	// The chain that pIdentifierStart, pIdentifierEnd and pScopeStart
	// belong to. It MAY be a nested chain and it may even be included in the
	// range specified by pTypeStart / pTypeEnd below!
	CXXTokenChain * pIdentifierChain;

	// Non-NULL if the signature is followed by the "const" keyword
	CXXToken * pSignatureConst;

	// Non-NULL if there is a scope before the identifier.
	// The scope ends at pIdentifierStart.
	// The scope start is always in the chain pointed by pIdentifierChain.
	CXXToken * pScopeStart;

	// Non-NULL if a return type has been identified.
	CXXToken * pTypeStart;
	CXXToken * pTypeEnd;
	// There are cases in that the type range defined above may
	// contain the identifier, scope and signature ranges.
	// This happens, for example, with functions returning
	// nasty things, like:
	//     int (*foo(void))[2]
	// It is granted that the scope and identifier are either
	// completely included or completely excluded from the type range.
	bool bTypeContainsIdentifierScopeAndSignature;

	// Non-NULL if there is a trailing comma after the function.
	// This is used for the special case of multiple prototypes in a single
	// declaration:
	//     RetType functionA(...), functionB(...);
	CXXToken * pTrailingComma;

	// Template specialization token range, if any.
	CXXToken * pTemplateSpecializationStart;
	CXXToken * pTemplateSpecializationEnd;

	// Additional informations
	unsigned int uFlags;

} CXXFunctionSignatureInfo;

int cxxParserMaybeParseKnRStyleFunctionDefinition(void);
int cxxParserExtractFunctionSignatureBeforeOpeningBracket(
		CXXFunctionSignatureInfo * pInfo,
		int * piCorkQueueIndex,
		int * piCorkQueueIndexFQ
	);

/* This must be smaller than (sizeof(unsigned int) * 8).
 * See CXXTypedVariableSet::uAnonymous. */
#define CXX_TYPED_VARIABLE_SET_ITEM_COUNT 24

typedef struct _CXXTypedVariableSet
{
	// The number of parameters found
	unsigned int uCount;

	// All the tokens are references to the source chain (do not delete)
	CXXTokenChain * pChain;
	// The initial tokens of the type
	CXXToken * aTypeStarts[CXX_TYPED_VARIABLE_SET_ITEM_COUNT];
	// The final tokens of the type
	CXXToken * aTypeEnds[CXX_TYPED_VARIABLE_SET_ITEM_COUNT];
	// The identifier tokens
	CXXToken * aIdentifiers[CXX_TYPED_VARIABLE_SET_ITEM_COUNT];

	unsigned int uAnonymous;
} CXXTypedVariableSet;

bool cxxParserTokenChainLooksLikeFunctionParameterList(
		CXXTokenChain * tc,
		CXXTypedVariableSet * pParamInfo
	);
bool cxxParserLookForFunctionSignature(
		CXXTokenChain * pChain,
		CXXFunctionSignatureInfo * pInfo,
		CXXTypedVariableSet * pParamInfo
	);

enum CXXEmitFunctionTagsOptions
{
	// Push the scopes defined by the function
	CXXEmitFunctionTagsPushScopes = 1
};

int cxxParserEmitFunctionTags(
		CXXFunctionSignatureInfo * pInfo,
		unsigned int uTagKind,
		unsigned int uOptions,
		int * piCorkQueueIndex,
		int * piCorkQueueIndexFQ
	);

void cxxParserEmitFunctionParameterTags(CXXTypedVariableSet * pInfo);

// cxx_parser_typedef.c
bool cxxParserParseGenericTypedef(void);
void cxxParserExtractTypedef(
		CXXTokenChain * pChain,
		bool bExpectTerminatorAtEnd,
		bool bGotTemplate
	);

// cxx_parser_namespace.c
bool cxxParserParseNamespace(void);

// cxx_parser.c
void cxxParserNewStatement(void);
bool cxxParserSkipToSemicolonOrEOF(void);
bool cxxParserParseToEndOfQualifedName(void);
bool cxxParserParseEnum(void);
bool cxxParserParseClassStructOrUnion(
		CXXKeyword eKeyword,
		unsigned int uTagKind,
		unsigned int uScopeType
	);
bool cxxParserParseAndCondenseCurrentSubchain(
		unsigned int uInitialSubchainMarkerTypes,
		bool bAcceptEOF,
		bool bCanReduceInnerElements
	);
bool cxxParserParseUpToOneOf(unsigned int uTokenTypes,
							 bool bCanReduceInnerElements);
bool cxxParserParseIfForWhileSwitchCatchParenthesis(void);
bool cxxParserParseTemplatePrefix(void);
CXXTokenChain * cxxParserParseTemplateAngleBracketsToSeparateChain(bool bCaptureTypeParameters);
bool cxxParserParseTemplateAngleBracketsToTemplateChain(void);
void cxxParserEmitTemplateParameterTags(void);
bool cxxParserParseUsingClause(void);
bool cxxParserParseAccessSpecifier(void);
void cxxParserAnalyzeOtherStatement(void);
bool cxxParserParseAndCondenseSubchainsUpToOneOf(
		unsigned int uTokenTypes,
		unsigned int uInitialSubchainMarkerTypes,
		bool bCanReduceInnerElements
	);
void cxxParserMarkEndLineForTagInCorkQueue(int iCorkQueueIndex);
void cxxParserSetEndLineForTagInCorkQueue(int iCorkQueueIndex,unsigned long lEndLine);

typedef enum _CXXParserKeywordState
{
	// We are parsing a statement that comes right after
	// a typedef keyword (so we're parsing the type being typedef'd).
	CXXParserKeywordStateSeenTypedef = 1,
	// We are parsing a statement that comes right after
	// an inline keyword
	CXXParserKeywordStateSeenInline = (1 << 1),
	// We are parsing a statement that comes right after
	// an extern keyword
	CXXParserKeywordStateSeenExtern = (1 << 2),
	// We are parsing a statement that comes right after
	// a static keyword
	CXXParserKeywordStateSeenStatic = (1 << 3),
	// an "explicit" keyword has been seen
	CXXParserKeywordStateSeenExplicit = (1 << 4),
	// an "operator" keyword has been seen
	CXXParserKeywordStateSeenOperator = (1 << 5),
	// "virtual" has been seen
	CXXParserKeywordStateSeenVirtual = (1 << 6),
	// "return" has been seen
	CXXParserKeywordStateSeenReturn = (1 << 7),
	// "mutable" has been seen
	CXXParserKeywordStateSeenMutable = (1 << 8),
	// "const" has been seen at block level
	CXXParserKeywordStateSeenConst = (1 << 9),
	// "volatile" has been seen at block level
	CXXParserKeywordStateSeenVolatile = (1 << 10),
	// __attribute__((deprecated)) has been seen
	CXXParserKeywordStateSeenAttributeDeprecated = (1 << 11),
	// "friend" has been seen at block level
	CXXParserKeywordStateSeenFriend = (1 << 12),
} CXXParserKeywordState;

#define CXX_PARSER_MAXIMUM_NESTING_LEVELS 1024

typedef struct _CXXParserState
{
	// The current language
	CXXLanguage eLanguage;

	// The current language as langType
	langType eLangType;

	// The identifier of the CPP language, as indicated by ctags core
	langType eCPPLangType;
	// The identifier of the C language, as indicated by ctags core
	langType eCLangType;
	// The identifier of the CUDA language, as indicated by ctags core
	langType eCUDALangType;

	// The kind options associated to the current language
	kindDefinition * pKindDefinitions;
	// The number of kind options, used mainly for checking/debug purposes
	unsigned int uKindDefinitionCount;

	// The fields associated to the current language
	fieldDefinition * pFieldOptions;
	// The number of field options, used mainly for checking/debug purposes
	unsigned int uFieldOptionCount;

	// The current token chain
	CXXTokenChain * pTokenChain;

	// The last template token chain we found
	// This remains valid within the statement, so it can be used slightly
	// after the template has been parsed (i.e. in the class coming after)
	CXXTokenChain * pTemplateTokenChain;

	// The last template specialization token chain we found. May be null.
	// This pointer, if non null, is valid only if pTemplateTokenChain is non null.
	CXXTokenChain * pTemplateSpecializationTokenChain;

	// The array of CXXToken objects that are found to be template
	// type parameters and belong to the pTemplateTokenChain above.
	// The validity of this array is tied to the validity of
	// pTemplateTokenChain above. If there is no pTemplateTokenChain
	// then this array is simply invalid (even if not empty)
	CXXTypedVariableSet oTemplateParameters;

	// The last token we have extracted. This is always pushed to
	// the token chain tail (which will take care of deletion)
	CXXToken * pToken; // the token chain tail

	// The parser internally supports a look-ahead of one token.
	// It is rarely needed though.
	// This is the token that has been "unget" from the token chain tail.
	CXXToken * pUngetToken;

	// The last char we have extracted from input
	int iChar;

	// Toplevel keyword state. A combination of CXXParserKeywordState flags.
	// Please note that the keywords appearing inside a () subchain are NOT marked.
	unsigned int uKeywordState;

	// This is set to true when we're parsing a *.cpp file (cpp extension!)
	// or we're parsing a header but we have encountered valid C++ constructs that
	// definitely confirm we're parsing C++.
	bool bConfirmedCPPLanguage;

	// The nesting levels our parser is in.
	//
	// Note that this is really a kind-of arbitrary measure as the counter
	// is increased in certain parser code paths that often lead to recursion.
	// It does not necessairly match the real number of stack frames or nested
	// brackets/parentheses in the input.
	//
	// The counter is used to avoid stack overflow when nesting grows too much.
	// This usually happens only with erroneous macro usage or broken input.
	int iNestingLevels;

} CXXParserState;


// defined in cxx_parser.c
extern CXXParserState g_cxx;

#define cxxParserCurrentLanguageIs(_eLanguage) \
	(g_cxx.eLanguage == _eLanguage)

#define cxxParserCurrentLanguageIsOneOf(_eLanguageMask) \
	(((int)(g_cxx.eLanguage)) & (_eLanguageMask))

#define cxxParserCurrentLanguageIsCPP() \
	cxxParserCurrentLanguageIs(CXXLanguageCPP)

#define cxxParserCurrentLanguageIsC() \
	cxxParserCurrentLanguageIs(CXXLanguageC)

#define cxxParserCurrentLanguageIsCUDA() \
	cxxParserCurrentLanguageIs(CXXLanguageCUDA)

#endif //!ctags_cxx_parser_internal_h_
