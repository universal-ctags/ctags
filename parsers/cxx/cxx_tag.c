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
#include "get.h"
#include "routines.h"
#include "xtag.h"


static roleDesc CMacroRoles [] = {
	RoleTemplateUndef,
};

static roleDesc CHeaderRoles [] = {
	RoleTemplateSystem,
	RoleTemplateLocal,
};

static kindOption g_aCXXKinds [] = {
	{ TRUE,  'c', "class",      "classes" },
	{ TRUE,  'd', "macro",      "macro definitions",
			.referenceOnly = FALSE, ATTACH_ROLES(CMacroRoles)
	},
	{ TRUE,  'e', "enumerator", "enumerators (values inside an enumeration)" },
	{ TRUE,  'f', "function",   "function definitions" },
	{ TRUE,  'g', "enum",       "enumeration names" },
	{ FALSE, 'h', "header",     "included header files",
			.referenceOnly = TRUE,  ATTACH_ROLES(CHeaderRoles)
	},
	{ FALSE, 'l', "local",      "local variables" },
	{ TRUE,  'm', "member",     "class, struct, and union members" },
	{ TRUE,  'n', "namespace",  "namespaces" },
	{ FALSE, 'p', "prototype",  "function prototypes" },
	{ TRUE,  's', "struct",     "structure names" },
	{ TRUE,  't', "typedef",    "typedefs" },
	{ TRUE,  'u', "union",      "union names" },
	{ TRUE,  'v', "variable",   "variable definitions" },
	{ FALSE, 'x', "externvar",  "external and forward variable declarations" },
	{ FALSE, 'z', "parameter",  "function parameters inside function definitions" },
	{ FALSE, 'L', "label",      "goto labels" },
	// FIXME: not sure about referenceOnly: if this is referenceOnly then
	// so is externvar and probably also prototype.
	{ FALSE, 'N', "name",       "names imported via using scope::symbol"
			/*, .referenceOnly = TRUE*/ },
	{ FALSE, 'U', "using",      "using namespace statements",
			.referenceOnly = TRUE },
};

static const char * g_aCXXAccessStrings [] = {
	NULL,
	"public",
	"private",
	"protected",
};

static fieldSpec g_aCXXCPPFields [] = {
	{
		//.letter = 'e'
		.name = "end",
		.description = "end lines of various constructs",
		.enabled = FALSE
	},
	{
		//.letter = 'T', ?
		.name = "template",
		.description = "template parameters",
		.enabled = FALSE,
	},
	{
		//.letter = 'L', ?
		.name = "captures",
		.description = "lambda capture list",
		.enabled = FALSE
	},
	{
		//.letter = 'P', ?
		.name = "properties",
		.description = "properties (static, virtual, inline, mutable,...)",
		.enabled = FALSE
	}
};

static fieldSpec g_aCXXCFields [] = {
	{
		//.letter = 'e'
		.name = "end",
		.description = "end lines of various constructs",
		.enabled = FALSE
	},
	{
		//.letter = 'P', ?
		.name = "properties",
		.description = "properties (static, inline, mutable,...)",
		.enabled = FALSE
	}
};

kindOption * cxxTagGetKindOptions(void)
{
	return g_aCXXKinds;
}

int cxxTagGetKindOptionCount(void)
{
	return sizeof(g_aCXXKinds) / sizeof(kindOption);
}

boolean cxxTagKindEnabled(enum CXXTagKind eKindId)
{
	return g_aCXXKinds[eKindId].enabled;
}

fieldSpec * cxxTagGetCPPFieldSpecifiers(void)
{
	return g_aCXXCPPFields;
}

int cxxTagGetCPPFieldSpecifierCount(void)
{
	return sizeof(g_aCXXCPPFields) / sizeof(fieldSpec);
}

int cxxTagCPPFieldEnabled(CXXTagCPPField eField)
{
	return g_aCXXCPPFields[eField].enabled;
}


fieldSpec * cxxTagGetCFieldSpecifiers(void)
{
	return g_aCXXCFields;
}

int cxxTagGetCFieldSpecifierCount(void)
{
	return sizeof(g_aCXXCFields) / sizeof(fieldSpec);
}

int cxxTagCFieldEnabled(CXXTagCField eField)
{
	return g_aCXXCFields[eField].enabled;
}


static tagEntryInfo g_oCXXTag;


tagEntryInfo * cxxTagBegin(enum CXXTagKind eKindId,CXXToken * pToken)
{
	if(!g_aCXXKinds[eKindId].enabled)
	{
		//CXX_DEBUG_PRINT("Tag kind %s is not enabled",g_aCXXKinds[eKindId].name);
		return NULL;
	}

	initTagEntry(
			&g_oCXXTag,
			vStringValue(pToken->pszWord),
			&(g_aCXXKinds[eKindId])
		);

	g_oCXXTag.lineNumber = pToken->iLineNumber;
	g_oCXXTag.filePosition = pToken->oFilePosition;
	g_oCXXTag.isFileScope = FALSE;

	if(!cxxScopeIsGlobal())
	{
		g_oCXXTag.extensionFields.scopeKind = &g_aCXXKinds[cxxScopeGetKind()];
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
	
	if(cxxParserCurrentLanguageIsCPP())
	{
		if(!cxxTagCPPFieldEnabled(CXXTagCPPFieldProperties))
			return NULL;
	} else {
		if(!cxxTagCFieldEnabled(CXXTagCFieldProperties))
			return NULL;
	}

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

	if(cxxParserCurrentLanguageIsCPP())
		cxxTagSetCPPField(CXXTagCPPFieldProperties,vStringValue(pszProperties));
	else
		cxxTagSetCField(CXXTagCFieldProperties,vStringValue(pszProperties));

	return pszProperties;
}


CXXToken * cxxTagSetTypeField(
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

	cxxTokenChainNormalizeTypeNameSpacingInRange(pTypeStart,pTypeEnd);
	CXXToken * pTypeName = cxxTokenChainExtractRange(pTypeStart,pTypeEnd,0);

	CXX_DEBUG_PRINT("Type name is '%s'",vStringValue(pTypeName->pszWord));

	g_oCXXTag.extensionFields.typeRef[0] = szTypeRef0;
	g_oCXXTag.extensionFields.typeRef[1] = vStringValue(pTypeName->pszWord);

	return pTypeName;
}

void cxxTagSetCPPField(CXXTagCPPField eField,const char * szValue)
{
	if(!g_aCXXCPPFields[eField].enabled)
		return;
	
	attachParserField(&g_oCXXTag,g_aCXXCPPFields[eField].ftype,szValue);
}

void cxxTagSetCorkQueueCPPField(
		int iIndex,
		CXXTagCPPField eField,
		const char * szValue
	)
{
	CXX_DEBUG_ASSERT(g_aCXXCPPFields[eField].enabled,"The field must be enabled!");

	attachParserFieldToCorkEntry(iIndex,g_aCXXCPPFields[eField].ftype,szValue);
}

void cxxTagSetCField(CXXTagCField eField,const char * szValue)
{
	if(!g_aCXXCFields[eField].enabled)
		return;
	
	attachParserField(&g_oCXXTag,g_aCXXCFields[eField].ftype,szValue);
}

void cxxTagSetCorkQueueCField(
		int iIndex,
		CXXTagCField eField,
		const char * szValue
	)
{
	CXX_DEBUG_ASSERT(g_aCXXCFields[eField].enabled,"The field must be enabled!");

	attachParserFieldToCorkEntry(iIndex,g_aCXXCFields[eField].ftype,szValue);
}

int cxxTagCommit(void)
{
	if(g_oCXXTag.isFileScope)
	{
		if (isXtagEnabled(XTAG_FILE_SCOPE))
			markTagExtraBit (&g_oCXXTag, XTAG_FILE_SCOPE);
		else
			return CORK_NIL; // FIXME: why the "invalid" cork queue index is CORK_NIL?
	}

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

	int iCorkQueueIndex = makeTagEntry(&g_oCXXTag);

	// Handle --extra=+q
	if(!isXtagEnabled(XTAG_QUALIFIED_TAGS))
		return iCorkQueueIndex;
	else
		markTagExtraBit (&g_oCXXTag, XTAG_QUALIFIED_TAGS);

	if(!g_oCXXTag.extensionFields.scopeName)
		return iCorkQueueIndex;

	// WARNING: The following code assumes that the scope
	// didn't change between cxxTagBegin() and cxxTagCommit().

	enum CXXTagKind eScopeKind = cxxScopeGetKind();

	if(eScopeKind == CXXTagKindFUNCTION)
	{
		// old ctags didn't do this, and --extra=+q is mainly
		// for backward compatibility so...
		return iCorkQueueIndex;
	}

	// Same tag. Only the name changes.

	vString * x;

	if(eScopeKind == CXXTagKindENUM)
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

void cxxTag(enum CXXTagKind eKindId,CXXToken * pToken)
{
	if(cxxTagBegin(eKindId,pToken) != NULL)
		cxxTagCommit();
}
