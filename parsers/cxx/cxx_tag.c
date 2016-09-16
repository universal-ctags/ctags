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
#include "lcpp.h"
#include "routines.h"
#include "xtag.h"

#define CXX_COMMON_MACRO_ROLES(__langPrefix)		\
    static roleDesc __langPrefix##MacroRoles [] = {	\
	    RoleTemplateUndef,				\
    }

CXX_COMMON_MACRO_ROLES(C);
CXX_COMMON_MACRO_ROLES(CXX);


#define CXX_COMMON_HEADER_ROLES(__langPrefix)		\
    static roleDesc __langPrefix##HeaderRoles [] = {	\
	    RoleTemplateSystem,				\
	    RoleTemplateLocal,				\
    }

CXX_COMMON_HEADER_ROLES(C);
CXX_COMMON_HEADER_ROLES(CXX);


#define CXX_COMMON_KINDS(_langPrefix, _szMemberDescription, _syncWith)	\
	{ TRUE,  'd', "macro",      "macro definitions", \
			.referenceOnly = FALSE, ATTACH_ROLES(_langPrefix##MacroRoles), .syncWith = _syncWith \
	}, \
	{ TRUE,  'e', "enumerator", "enumerators (values inside an enumeration)", .syncWith = _syncWith }, \
	{ TRUE,  'f', "function",   "function definitions", .syncWith = _syncWith },		\
	{ TRUE,  'g', "enum",       "enumeration names", .syncWith = _syncWith },		\
	{ TRUE, 'h', "header",     "included header files", \
			.referenceOnly = TRUE,  ATTACH_ROLES(_langPrefix##HeaderRoles), .syncWith = _syncWith \
	}, \
	{ FALSE, 'l', "local",      "local variables", .syncWith = _syncWith },   \
	{ TRUE,  'm', "member",     _szMemberDescription, .syncWith = _syncWith },	\
	{ FALSE, 'p', "prototype",  "function prototypes", .syncWith = _syncWith },		\
	{ TRUE,  's', "struct",     "structure names", .syncWith = _syncWith },		\
	{ TRUE,  't', "typedef",    "typedefs", .syncWith = _syncWith },			\
	{ TRUE,  'u', "union",      "union names", .syncWith = _syncWith },			\
	{ TRUE,  'v', "variable",   "variable definitions", .syncWith = _syncWith },		\
	{ FALSE, 'x', "externvar",  "external and forward variable declarations", .syncWith = _syncWith }, \
	{ FALSE, 'z', "parameter",  "function parameters inside function definitions", .syncWith = _syncWith }, \
	{ FALSE, 'L', "label",      "goto labels", .syncWith = _syncWith }

static kindOption g_aCXXCKinds [] = {
	/* All other than LANG_AUTO are ignored.
	   LANG_IGNORE is specified as a just placeholder for the macro,
	   and is not needed. */
	CXX_COMMON_KINDS(C,"struct, and union members", LANG_IGNORE)
};

static kindOption g_aCXXCPPKinds [] = {
	CXX_COMMON_KINDS(CXX,"class, struct, and union members", LANG_AUTO),
	{ TRUE,  'c', "class",      "classes" },
	{ TRUE,  'n', "namespace",  "namespaces" },
	{ FALSE, 'A', "alias",      "namespace aliases" },
	{ FALSE, 'N', "name",       "names imported via using scope::symbol" },
	{ FALSE, 'U', "using",      "using namespace statements",
			.referenceOnly = TRUE },
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
		.enabled = FALSE \
	}

static fieldSpec g_aCXXCFields [] = {
	CXX_COMMON_FIELDS
};

static fieldSpec g_aCXXCPPFields [] = {
	CXX_COMMON_FIELDS,
	{
		.name = "template",
		.description = "template parameters",
		.enabled = FALSE,
	},
	{
		.name = "captures",
		.description = "lambda capture list",
		.enabled = FALSE
	},
	{
		.name = "name",
		.description = "aliased names",
		.enabled = TRUE
	}
};

void cxxTagInitForLanguage(langType eLanguage)
{
	g_cxx.eLanguage = eLanguage;

	if(g_cxx.eLanguage == g_cxx.eCLanguage)
	{
		g_cxx.pKindOptions = g_aCXXCKinds;
		g_cxx.uKindOptionCount = sizeof(g_aCXXCKinds) / sizeof(kindOption);
		g_cxx.pFieldOptions = g_aCXXCFields;
		g_cxx.uFieldOptionCount = sizeof(g_aCXXCFields) / sizeof(fieldSpec);
	} else if(g_cxx.eLanguage == g_cxx.eCPPLanguage)
	{
		g_cxx.pKindOptions = g_aCXXCPPKinds;
		g_cxx.uKindOptionCount = sizeof(g_aCXXCPPKinds) / sizeof(kindOption);
		g_cxx.pFieldOptions = g_aCXXCPPFields;
		g_cxx.uFieldOptionCount = sizeof(g_aCXXCPPFields) / sizeof(fieldSpec);
	} else {
		CXX_DEBUG_ASSERT(FALSE,"Invalid language passed to cxxTagInitForLanguage()");
	}
}

kindOption * cxxTagGetCKindOptions(void)
{
	return g_aCXXCKinds;
}

int cxxTagGetCKindOptionCount(void)
{
	return sizeof(g_aCXXCKinds) / sizeof(kindOption);
}

kindOption * cxxTagGetCPPKindOptions(void)
{
	return g_aCXXCPPKinds;
}

int cxxTagGetCPPKindOptionCount(void)
{
	return sizeof(g_aCXXCPPKinds) / sizeof(kindOption);
}

boolean cxxTagKindEnabled(unsigned int uKind)
{
	CXX_DEBUG_ASSERT(
			uKind < g_cxx.uKindOptionCount,
			"The kind must be associated to the current language!"
		);
	return g_cxx.pKindOptions[uKind].enabled;
}

fieldSpec * cxxTagGetCPPFieldSpecifiers(void)
{
	return g_aCXXCPPFields;
}

int cxxTagGetCPPFieldSpecifierCount(void)
{
	return sizeof(g_aCXXCPPFields) / sizeof(fieldSpec);
}

fieldSpec * cxxTagGetCFieldSpecifiers(void)
{
	return g_aCXXCFields;
}

int cxxTagGetCFieldSpecifierCount(void)
{
	return sizeof(g_aCXXCFields) / sizeof(fieldSpec);
}

boolean cxxTagFieldEnabled(unsigned int uField)
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
	kindOption * pKindOptions = g_cxx.pKindOptions;

	if(!pKindOptions[uKind].enabled)
	{
		//CXX_DEBUG_PRINT("Tag kind %s is not enabled",g_aCXXKinds[eKind].name);
		return NULL;
	}

	initTagEntry(
			&g_oCXXTag,
			vStringValue(pToken->pszWord),
			pKindOptions + uKind
		);

	g_oCXXTag.lineNumber = pToken->iLineNumber;
	g_oCXXTag.filePosition = pToken->oFilePosition;
	g_oCXXTag.isFileScope = FALSE;

	if(!cxxScopeIsGlobal())
	{
		g_oCXXTag.extensionFields.scopeKind = &(g_cxx.pKindOptions[cxxScopeGetKind()]);
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

	boolean bFirst = TRUE;

#define ADD_PROPERTY(_szProperty) \
	do { \
		if(bFirst) \
			bFirst = FALSE; \
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

	cxxTagSetField(CXXTagFieldProperties,vStringValue(pszProperties));

	return pszProperties;
}

static boolean cxxTagCheckTypeField(
		CXXToken * pTypeStart,
		CXXToken * pTypeEnd
	)
{
	CXX_DEBUG_ENTER();
	if(!pTypeStart || !pTypeEnd)
	{
		CXX_DEBUG_LEAVE_TEXT("One of the pointers is NULL");
		return FALSE;
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
			return FALSE;
		}

		if(cxxTokenTypeIs(pTypeStart,CXXTokenTypeIdentifier))
		{
			iConsecutiveIdentifiers++;
			iIdentifierOrKeywordCount++;
			if(iConsecutiveIdentifiers > 4)
			{
				// Probably many macros inside. Too many.
				CXX_DEBUG_LEAVE_TEXT("Too many consecutive identifiers for a type name");
				return FALSE;
			}
		} else {
			iConsecutiveIdentifiers = 0;

			if(cxxTokenTypeIs(pTypeStart,CXXTokenTypeParenthesisChain))
			{
				iParenthesisCount++;
				if(iParenthesisCount > 3)
				{
					CXX_DEBUG_LEAVE_TEXT("Too many non-nested parentheses for a type name");
					return FALSE;
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
					return FALSE;
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
		return FALSE;
	}
	
	if(!pTypeStart)
	{
		CXX_DEBUG_LEAVE_TEXT("Type tokens do not belong to the same chain!");
		return FALSE;
	}
	
	CXX_DEBUG_LEAVE();
	return TRUE;
}

CXXToken * cxxTagCheckAndSetTypeField(
		CXXToken * pTypeStart,
		CXXToken * pTypeEnd
	)
{
	CXX_DEBUG_ASSERT(pTypeStart && pTypeEnd,"Non null parameters are expected");

	const char * szTypeRef0;

	// "typename" is debatable since it's not really
	// allowed by C++ for unqualified types. However I haven't been able
	// to come up with something better... so "typename" it is for now.
	static const char * szTypename = "typename";

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
		szTypeRef0 = szTypename;
	}

	if(!cxxTagCheckTypeField(pTypeStart,pTypeEnd))
	{
		CXX_DEBUG_PRINT("Type name looks suspicious: refusing to emit it");
		return NULL;
	}

	cxxTokenChainNormalizeTypeNameSpacingInRange(pTypeStart,pTypeEnd);
	CXXToken * pTypeName = cxxTokenChainExtractRange(pTypeStart,pTypeEnd,0);

	CXX_DEBUG_PRINT("Type name is '%s'",vStringValue(pTypeName->pszWord));

	g_oCXXTag.extensionFields.typeRef[0] = szTypeRef0;
	g_oCXXTag.extensionFields.typeRef[1] = vStringValue(pTypeName->pszWord);

	return pTypeName;
}

void cxxTagSetField(unsigned int uField,const char * szValue)
{
	CXX_DEBUG_ASSERT(
			uField < g_cxx.uFieldOptionCount,
			"The field must be associated to the current language!"
		);

	if(!g_cxx.pFieldOptions[uField].enabled)
		return;

	attachParserField(&g_oCXXTag,g_cxx.pFieldOptions[uField].ftype,szValue);
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

int cxxTagCommit(void)
{
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
			g_oCXXTag.kind->name,
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

	if(eScopeType == CXXScopeTypeFunction)
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
			g_oCXXTag.kind->name,
			g_oCXXTag.lineNumber
		);

	makeTagEntry(&g_oCXXTag);

	vStringDelete(x);

	return iCorkQueueIndex;
}

void cxxTag(unsigned int uKind,CXXToken * pToken)
{
	if(cxxTagBegin(uKind,pToken) != NULL)
		cxxTagCommit();
}
