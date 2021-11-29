/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/
#include "cxx_tag.h"

#include "cxx_scope.h"
#include "cxx_debug.h"
#include "cxx_token_chain.h"
#include "cxx_parser_internal.h"

#include "entry.h"
#include "../cpreprocessor.h"
#include "routines.h"
#include "trashbox.h"
#include "xtag.h"

#define CXX_COMMON_MACRO_ROLES(__langPrefix) \
	static roleDefinition __langPrefix##MacroRoles [] = { \
		RoleTemplateUndef, \
		RoleTemplateCondition, \
	}

CXX_COMMON_MACRO_ROLES(C);
CXX_COMMON_MACRO_ROLES(CXX);
CXX_COMMON_MACRO_ROLES(CUDA);

#define CXX_COMMON_HEADER_ROLES(__langPrefix) \
	static roleDefinition __langPrefix##HeaderRoles [] = { \
		RoleTemplateSystem, \
		RoleTemplateLocal, \
	}

CXX_COMMON_HEADER_ROLES(C);
CXX_COMMON_HEADER_ROLES(CXX);
CXX_COMMON_HEADER_ROLES(CUDA);


#define CXX_COMMON_KINDS(_langPrefix, _szMemberDescription, _syncWith)	\
	{ true,  'd', "macro",      "macro definitions", \
			.referenceOnly = false, ATTACH_ROLES(_langPrefix##MacroRoles), .syncWith = _syncWith \
	}, \
	{ true,  'e', "enumerator", "enumerators (values inside an enumeration)", .syncWith = _syncWith }, \
	{ true,  'f', "function",   "function definitions", .syncWith = _syncWith },		\
	{ true,  'g', "enum",       "enumeration names", .syncWith = _syncWith },		\
	{ true, 'h', "header",     "included header files", \
			.referenceOnly = true,  ATTACH_ROLES(_langPrefix##HeaderRoles), .syncWith = _syncWith \
	}, \
	{ false, 'l', "local",      "local variables", .syncWith = _syncWith },   \
	{ true,  'm', "member",     _szMemberDescription, .syncWith = _syncWith },	\
	{ false, 'p', "prototype",  "function prototypes", .syncWith = _syncWith },		\
	{ true,  's', "struct",     "structure names", .syncWith = _syncWith },		\
	{ true,  't', "typedef",    "typedefs", .syncWith = _syncWith },			\
	{ true,  'u', "union",      "union names", .syncWith = _syncWith },			\
	{ true,  'v', "variable",   "variable definitions", .syncWith = _syncWith },		\
	{ false, 'x', "externvar",  "external and forward variable declarations", .syncWith = _syncWith }, \
	{ false, 'z', "parameter",  "function parameters inside function or prototype definitions", .syncWith = _syncWith }, \
	{ false, 'L', "label",      "goto labels", .syncWith = _syncWith }, \
	{ false, 'D', "macroparam", "parameters inside macro definitions", .syncWith = _syncWith }

static kindDefinition g_aCXXCKinds [] = {
	/* All other than LANG_AUTO are ignored.
	   LANG_IGNORE is specified as a just placeholder for the macro,
	   and is not needed. */
	CXX_COMMON_KINDS(C,"struct, and union members", LANG_IGNORE)
};

static kindDefinition g_aCXXCPPKinds [] = {
	CXX_COMMON_KINDS(CXX,"class, struct, and union members", LANG_AUTO),
	{ true,  'c', "class",      "classes" },
	{ true,  'n', "namespace",  "namespaces" },
	{ false, 'A', "alias",      "namespace aliases" },
	{ false, 'N', "name",       "names imported via using scope::symbol" },
	{ false, 'U', "using",      "using namespace statements" },
	{ false, 'Z', "tparam",     "template parameters" },
};

static kindDefinition g_aCXXCUDAKinds [] = {
	CXX_COMMON_KINDS(CUDA,"struct, and union members", LANG_IGNORE)
};

static const char * g_aCXXAccessStrings [] = {
	NULL,
	"public",
	"private",
	"protected",
};

#define CXX_COMMON_FIELDS \
	{ \
		.name = "properties", \
		.description = "properties (static, inline, mutable,...)", \
		.enabled = false \
	}, { \
		.name = "macrodef", \
		.description = "macro definition", \
			.enabled = false \
	}

static fieldDefinition g_aCXXCFields [] = {
	CXX_COMMON_FIELDS
};

static fieldDefinition g_aCXXCPPFields [] = {
	CXX_COMMON_FIELDS,
	{
		.name = "template",
		.description = "template parameters",
		.enabled = false,
	},
	{
		.name = "captures",
		.description = "lambda capture list",
		.enabled = false
	},
	{
		.name = "name",
		.description = "aliased names",
		.enabled = true
	},
	{
		.name = "specialization",
		.description = "template specialization parameters",
		.enabled = false,
	},
};

static fieldDefinition g_aCXXCUDAFields [] = {
	CXX_COMMON_FIELDS
};

void cxxTagInitForLanguage(langType eLangType)
{
	g_cxx.eLangType = eLangType;

	if(g_cxx.eLangType == g_cxx.eCLangType)
	{
		g_cxx.eLanguage = CXXLanguageC;
		g_cxx.pKindDefinitions = g_aCXXCKinds;
		g_cxx.uKindDefinitionCount = sizeof(g_aCXXCKinds) / sizeof(kindDefinition);
		g_cxx.pFieldOptions = g_aCXXCFields;
		g_cxx.uFieldOptionCount = sizeof(g_aCXXCFields) / sizeof(fieldDefinition);
	} else if(g_cxx.eLangType == g_cxx.eCPPLangType)
	{
		g_cxx.eLanguage = CXXLanguageCPP;
		g_cxx.pKindDefinitions = g_aCXXCPPKinds;
		g_cxx.uKindDefinitionCount = sizeof(g_aCXXCPPKinds) / sizeof(kindDefinition);
		g_cxx.pFieldOptions = g_aCXXCPPFields;
		g_cxx.uFieldOptionCount = sizeof(g_aCXXCPPFields) / sizeof(fieldDefinition);
	} else if(g_cxx.eLangType == g_cxx.eCUDALangType)
	{
		g_cxx.eLanguage = CXXLanguageCUDA;
		g_cxx.pKindDefinitions = g_aCXXCUDAKinds;
		g_cxx.uKindDefinitionCount = sizeof(g_aCXXCUDAKinds) / sizeof(kindDefinition);
		g_cxx.pFieldOptions = g_aCXXCUDAFields;
		g_cxx.uFieldOptionCount = sizeof(g_aCXXCUDAFields) / sizeof(fieldDefinition);
	} else {
		CXX_DEBUG_ASSERT(false,"Invalid language passed to cxxTagInitForLanguage()");
	}
}

kindDefinition * cxxTagGetCKindDefinitions(void)
{
	return g_aCXXCKinds;
}

int cxxTagGetCKindDefinitionCount(void)
{
	return sizeof(g_aCXXCKinds) / sizeof(kindDefinition);
}

kindDefinition * cxxTagGetCUDAKindDefinitions(void)
{
	return g_aCXXCUDAKinds;
}

int cxxTagGetCUDAKindDefinitionCount(void)
{
	return sizeof(g_aCXXCUDAKinds) / sizeof(kindDefinition);
}

kindDefinition * cxxTagGetCPPKindDefinitions(void)
{
	return g_aCXXCPPKinds;
}

int cxxTagGetCPPKindDefinitionCount(void)
{
	return sizeof(g_aCXXCPPKinds) / sizeof(kindDefinition);
}

bool cxxTagKindEnabled(unsigned int uKind)
{
	CXX_DEBUG_ASSERT(
			uKind < g_cxx.uKindDefinitionCount,
			"The kind must be associated to the current language!"
		);
	return g_cxx.pKindDefinitions[uKind].enabled;
}

fieldDefinition * cxxTagGetCPPFieldDefinitionifiers(void)
{
	return g_aCXXCPPFields;
}

int cxxTagGetCPPFieldDefinitionifierCount(void)
{
	return sizeof(g_aCXXCPPFields) / sizeof(fieldDefinition);
}

fieldDefinition * cxxTagGetCUDAFieldDefinitionifiers(void)
{
	return g_aCXXCUDAFields;
}

int cxxTagGetCUDAFieldDefinitionifierCount(void)
{
	return sizeof(g_aCXXCUDAFields) / sizeof(fieldDefinition);
}

fieldDefinition * cxxTagGetCFieldDefinitionifiers(void)
{
	return g_aCXXCFields;
}

int cxxTagGetCFieldDefinitionifierCount(void)
{
	return sizeof(g_aCXXCFields) / sizeof(fieldDefinition);
}

bool cxxTagFieldEnabled(unsigned int uField)
{
	CXX_DEBUG_ASSERT(
			uField < g_cxx.uFieldOptionCount,
			"The field must be associated to the current language!"
		);
	return g_cxx.pFieldOptions[uField].enabled;
}


static tagEntryInfo g_oCXXTag;


tagEntryInfo * cxxTagBegin(unsigned int uKind,CXXToken * pToken)
{
	kindDefinition * pKindDefinitions = g_cxx.pKindDefinitions;

	if(!pKindDefinitions[uKind].enabled)
	{
		//CXX_DEBUG_PRINT("Tag kind %s is not enabled",g_aCXXKinds[eKind].name);
		return NULL;
	}

	initTagEntry(
			&g_oCXXTag,
			vStringValue(pToken->pszWord),
			uKind
		);

	g_oCXXTag.lineNumber = pToken->iLineNumber;
	g_oCXXTag.filePosition = pToken->oFilePosition;
	g_oCXXTag.isFileScope = false;

	if(!cxxScopeIsGlobal())
	{
		g_oCXXTag.extensionFields.scopeKindIndex = cxxScopeGetKind();
		g_oCXXTag.extensionFields.scopeName = cxxScopeGetFullName();
	}

	// FIXME: meaning of "is file scope" is quite debatable...
	g_oCXXTag.extensionFields.access = g_aCXXAccessStrings[cxxScopeGetAccess()];

	return &g_oCXXTag;
}

vString * cxxTagSetProperties(unsigned int uProperties)
{
	if(uProperties == 0)
		return NULL;

	if(!cxxTagFieldEnabled(CXXTagFieldProperties))
		return NULL;

	vString * pszProperties = vStringNew();

	bool bFirst = true;

#define ADD_PROPERTY(_szProperty) \
	do { \
		if(bFirst) \
			bFirst = false; \
		else \
			vStringPut(pszProperties,','); \
		vStringCatS(pszProperties,_szProperty); \
	} while(0)

	if(uProperties & CXXTagPropertyConst)
		ADD_PROPERTY("const");
	if(uProperties & CXXTagPropertyDefault)
		ADD_PROPERTY("default");
	if(uProperties & CXXTagPropertyDelete)
		ADD_PROPERTY("delete");
	if(uProperties & CXXTagPropertyExplicit)
		ADD_PROPERTY("explicit");
	if(uProperties & CXXTagPropertyExtern)
		ADD_PROPERTY("extern");
	if(uProperties & CXXTagPropertyFinal)
		ADD_PROPERTY("final");
	if(uProperties & CXXTagPropertyInline)
		ADD_PROPERTY("inline");
	if(uProperties & CXXTagPropertyMutable)
		ADD_PROPERTY("mutable");
	if(uProperties & CXXTagPropertyOverride)
		ADD_PROPERTY("override");
	if(uProperties & CXXTagPropertyPure)
		ADD_PROPERTY("pure");
	if(uProperties & CXXTagPropertyScopeTemplateSpecialization)
		ADD_PROPERTY("scopespecialization");
	if(uProperties & CXXTagPropertyStatic)
		ADD_PROPERTY("static");
	if(uProperties & CXXTagPropertyTemplateSpecialization)
		ADD_PROPERTY("specialization");
	if(uProperties & CXXTagPropertyVirtual)
		ADD_PROPERTY("virtual");
	if(uProperties & CXXTagPropertyVolatile)
		ADD_PROPERTY("volatile");
	if(uProperties & CXXTagPropertyDeprecated)
		ADD_PROPERTY("deprecated");
	if(uProperties & CXXTagPropertyScopedEnum)
		ADD_PROPERTY("scopedenum");
	if(uProperties & CXXTagPropertyFunctionTryBlock)
		ADD_PROPERTY("fntryblock");

	cxxTagSetField(CXXTagFieldProperties,vStringValue(pszProperties),false);

	return pszProperties;
}

static bool cxxTagCheckTypeField(
		CXXToken * pTypeStart,
		CXXToken * pTypeEnd
	)
{
	CXX_DEBUG_ENTER();
	if(!pTypeStart || !pTypeEnd)
	{
		CXX_DEBUG_LEAVE_TEXT("One of the pointers is NULL");
		return false;
	}

	int iTotalCount = 0;
	int iParenthesisCount = 0;
	int iIdentifierOrKeywordCount = 0;
	int iConsecutiveIdentifiers = 0;

	while(pTypeStart)
	{
		iTotalCount++;
		if(iTotalCount > 30)
		{
			CXX_DEBUG_LEAVE_TEXT("The chain is really too long to be a type name");
			return false;
		}

		if(cxxTokenTypeIs(pTypeStart,CXXTokenTypeIdentifier))
		{
			iConsecutiveIdentifiers++;
			iIdentifierOrKeywordCount++;
			if(iConsecutiveIdentifiers > 4)
			{
				// Probably many macros inside. Too many.
				CXX_DEBUG_LEAVE_TEXT("Too many consecutive identifiers for a type name");
				return false;
			}
		} else {
			iConsecutiveIdentifiers = 0;

			if(cxxTokenTypeIs(pTypeStart,CXXTokenTypeParenthesisChain))
			{
				iParenthesisCount++;
				if(iParenthesisCount > 3)
				{
					CXX_DEBUG_LEAVE_TEXT("Too many non-nested parentheses for a type name");
					return false;
				}

				if(
					(iTotalCount > 1) &&
					cxxTokenTypeIs(pTypeStart->pPrev,CXXTokenTypeIdentifier) &&
					pTypeStart != pTypeEnd &&
					pTypeStart->pNext &&
					cxxTokenTypeIs(pTypeStart->pNext,CXXTokenTypeIdentifier)
				)
				{
					// identifier () identifier
					// Looks suspicious, might be macros gathered by mistake
					CXX_DEBUG_LEAVE_TEXT("Identifier-parenthesis-identifier pattern: looks suspicious");
					return false;
				}
			} else if(cxxTokenTypeIs(pTypeStart,CXXTokenTypeKeyword))
			{
				iIdentifierOrKeywordCount++;
			}
		}

		if(pTypeStart == pTypeEnd)
			break;

		pTypeStart = pTypeStart->pNext;
	}

	if(iIdentifierOrKeywordCount < 1)
	{
		CXX_DEBUG_LEAVE_TEXT("Type does not seem to contains identifiers or keywords, can't be a type name");
		return false;
	}

	if(!pTypeStart)
	{
		CXX_DEBUG_LEAVE_TEXT("Type tokens do not belong to the same chain!");
		return false;
	}

	CXX_DEBUG_LEAVE();
	return true;
}

CXXToken * cxxTagCheckAndSetTypeField(
		CXXToken * pTypeStart,
		CXXToken * pTypeEnd
	)
{
	CXX_DEBUG_ASSERT(pTypeStart,"Non null type start is expected here");
	CXX_DEBUG_ASSERT(pTypeEnd,"Non null type end is expected here");

	const char * szTypeRef0;

	// "typename" is debatable since it's not really
	// allowed by C++ for unqualified types. However I haven't been able
	// to come up with something better... so "typename" it is for now.

	// FIXME: The typeRef forma with two fields should be dropped.
	//        It has been created with specific use cases in mind
	//        and we are pushing it way beyond them.
	//        We should have a plain "type" field instead.

	static const char * szTypename = "typename";
	static const char * szMeta = "meta"; // for type template arguments

	// Filter out initial keywords that need to be excluded from typenames
	for(;;)
	{
		if(!cxxTokenTypeIs(pTypeStart,CXXTokenTypeKeyword))
			break;
		if(!cxxKeywordExcludeFromTypeNames(pTypeStart->eKeyword))
			break;
		// must be excluded
		if(pTypeStart == pTypeEnd)
		{
			CXX_DEBUG_PRINT("Type name composed only of ignored keywords");
			return NULL; // only excluded keywords
		}
		pTypeStart = pTypeStart->pNext;
	}

	if(pTypeStart != pTypeEnd)
	{
		// Note that this does not work for types like "const enum X"
		// But that's not backward compatible anyway, so we live with it.
		if(
				cxxTokenTypeIs(pTypeStart,CXXTokenTypeKeyword) &&
				cxxKeywordIsTypeRefMarker(pTypeStart->eKeyword)
			)
		{
			szTypeRef0 = cxxKeywordName(pTypeStart->eKeyword);
			pTypeStart = pTypeStart->pNext;
		} else {
			szTypeRef0 = szTypename;
		}
	} else {
		if(
				cxxTokenTypeIs(pTypeStart,CXXTokenTypeKeyword) &&
				cxxKeywordIsTypeRefMarker(pTypeStart->eKeyword)
			)
		{
			// A lone "typename", "class", "struct" or similar.
			// This almost certainly comes from a template.
			szTypeRef0 = szMeta;
		} else {
			szTypeRef0 = szTypename;
		}
	}

	if(!cxxTagCheckTypeField(pTypeStart,pTypeEnd))
	{
		CXX_DEBUG_PRINT("Type name looks suspicious: refusing to emit it");
		return NULL;
	}

	cxxTokenChainNormalizeTypeNameSpacingInRange(pTypeStart,pTypeEnd);
	CXXToken * pTypeName = cxxTokenChainExtractRangeFilterTypeName(pTypeStart,pTypeEnd);

	if(!pTypeName)
	{
		CXX_DEBUG_PRINT("Can't extract type name");
		return NULL;
	}

	CXX_DEBUG_PRINT("Type name is '%s'",vStringValue(pTypeName->pszWord));

	g_oCXXTag.extensionFields.typeRef[0] = szTypeRef0;
	g_oCXXTag.extensionFields.typeRef[1] = vStringValue(pTypeName->pszWord);

	return pTypeName;
}

void cxxTagSetField(unsigned int uField,const char * szValue,bool bCopyValue)
{
	CXX_DEBUG_ASSERT(
			uField < g_cxx.uFieldOptionCount,
			"The field must be associated to the current language!"
		);

	if(!g_cxx.pFieldOptions[uField].enabled)
		return;

	/* If we make a copy for the value, the copy must be freed after
	 * calling cxxTagCommit() for g_oCXXTag. The parser trash box
	 * allows us to delay freeing the copy. */
	attachParserField(&g_oCXXTag,false,g_cxx.pFieldOptions[uField].ftype,
					  bCopyValue?parserTrashBoxPut(eStrdup(szValue),eFree):szValue);
}

void cxxTagSetCorkQueueField(
		int iIndex,
		unsigned int uField,
		const char * szValue
	)
{
	CXX_DEBUG_ASSERT(
			uField < g_cxx.uFieldOptionCount,
			"The field must be associated to the current language!"
		);

	CXX_DEBUG_ASSERT(g_cxx.pFieldOptions[uField].enabled,"The field must be enabled!");

	attachParserFieldToCorkEntry(iIndex,g_cxx.pFieldOptions[uField].ftype,szValue);
}

void cxxTagHandleTemplateFields()
{
	CXX_DEBUG_ASSERT(
		g_cxx.pTemplateTokenChain &&
		(g_cxx.pTemplateTokenChain->iCount > 0) &&
		cxxParserCurrentLanguageIsCPP(),
		"Template existence must be checked before calling this function"
	);

	if(cxxTagFieldEnabled(CXXTagCPPFieldTemplate))
	{
		cxxTokenChainNormalizeTypeNameSpacing(g_cxx.pTemplateTokenChain);

		CXXToken * t = cxxTokenChainCondenseIntoToken(g_cxx.pTemplateTokenChain,0);

		if(t)
		{
			cxxTagSetField(
					CXXTagCPPFieldTemplate,
					vStringValue(t->pszWord),
					true
				);

			cxxTokenDestroy(t);
		}
	}

	if(
			g_cxx.pTemplateSpecializationTokenChain &&
			cxxTagFieldEnabled(CXXTagCPPFieldTemplateSpecialization)
		)
	{
		cxxTokenChainNormalizeTypeNameSpacing(g_cxx.pTemplateSpecializationTokenChain);

		CXXToken * tx = cxxTokenChainCondenseIntoToken(g_cxx.pTemplateSpecializationTokenChain,0);

		if(tx)
		{
			cxxTagSetField(
					CXXTagCPPFieldTemplateSpecialization,
					vStringValue(tx->pszWord),
					true
				);

			cxxTokenDestroy(tx);
		}
	}

}

int cxxTagCommit(int *piCorkQueueIndexFQ)
{
	if(piCorkQueueIndexFQ)
		*piCorkQueueIndexFQ = CORK_NIL;

	if(g_oCXXTag.isFileScope)
	{
		if(!isXtagEnabled(XTAG_FILE_SCOPE))
			return CORK_NIL;

		markTagExtraBit(&g_oCXXTag,XTAG_FILE_SCOPE);
	}

#ifdef CXX_DO_DEBUGGING
	CXX_DEBUG_PRINT(
			"Emitting tag for symbol '%s', kind '%s', line %d",
			g_oCXXTag.name,
			getLanguageKindName(g_oCXXTag.langType, g_oCXXTag.kindIndex),
			g_oCXXTag.lineNumber
		);
	if(
			g_oCXXTag.extensionFields.typeRef[0] &&
			g_oCXXTag.extensionFields.typeRef[1]
		)
		CXX_DEBUG_PRINT(
				"Tag has typeref %s %s",
				g_oCXXTag.extensionFields.typeRef[0],
				g_oCXXTag.extensionFields.typeRef[1]
			);
#endif

	int iCorkQueueIndex = makeTagEntry(&g_oCXXTag);

	// Handle --extra=+q
	if(!isXtagEnabled(XTAG_QUALIFIED_TAGS))
		return iCorkQueueIndex;

	markTagExtraBit(&g_oCXXTag,XTAG_QUALIFIED_TAGS);

	if(!g_oCXXTag.extensionFields.scopeName)
		return iCorkQueueIndex;

	// WARNING: The following code assumes that the scope
	// didn't change between cxxTagBegin() and cxxTagCommit().

	enum CXXScopeType eScopeType = cxxScopeGetType();

	if(eScopeType == CXXScopeTypeFunction || eScopeType == CXXScopeTypePrototype)
	{
		// old ctags didn't do this, and --extra=+q is mainly
		// for backward compatibility so...
		return iCorkQueueIndex;
	}

	// Same tag. Only the name changes.

	vString * x;

	if(eScopeType == CXXScopeTypeEnum)
	{
		// If the scope kind is enumeration then we need to remove the
		// last scope part. This is what old ctags did.
		if(cxxScopeGetSize() < 2)
			return -1; // toplevel enum

		x = cxxScopeGetFullNameExceptLastComponentAsString();
		CXX_DEBUG_ASSERT(x,"Scope with size >= 2 should have returned a value here");
	} else {
		x = vStringNewInit(g_oCXXTag.extensionFields.scopeName);
	}

	vStringCatS(x,"::");
	vStringCatS(x,g_oCXXTag.name);

	g_oCXXTag.name = vStringValue(x);

	CXX_DEBUG_PRINT(
			"Emitting extra tag for symbol '%s', kind '%s', line %d",
			g_oCXXTag.name,
			getLanguageKindName(g_oCXXTag.langType, g_oCXXTag.kindIndex),
			g_oCXXTag.lineNumber
		);

	int iCorkQueueIndexFQ = makeTagEntry(&g_oCXXTag);
	if(piCorkQueueIndexFQ)
		*piCorkQueueIndexFQ = iCorkQueueIndexFQ;

	vStringDelete(x);

	return iCorkQueueIndex;
}

void cxxTag(unsigned int uKind,CXXToken * pToken)
{
	if(cxxTagBegin(uKind,pToken) != NULL)
		cxxTagCommit(NULL);
}
