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

static roleDesc CXXUsingRoles [] = {
	{ TRUE, "extending-scope", "... (for using namespace a::b)" },
    { TRUE, "importing-name", "... (for using a::b)" }
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
	{ FALSE, 'U', "using",      "using namespace statements",
			.referenceOnly = TRUE, ATTACH_ROLES(CXXUsingRoles)
	}
};

static const char * g_aCXXAccessStrings [] = {
	"public",
	"protected",
	"private"
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

static tagEntryInfo g_oCXXTag;


// FIXME: szName is always vStringValue(pRefCXXToken->pszWord) (?)
tagEntryInfo * cxxTagBegin(
		const char * szName,
		enum CXXTagKind eKindId,
		CXXToken * pRefCXXToken
	)
{
	if(!g_aCXXKinds[eKindId].enabled)
	{
		//CXX_DEBUG_PRINT("Tag kind %s is not enabled",g_aCXXKinds[eKindId].name);
		return NULL;
	}

	initTagEntry(&g_oCXXTag,szName,&(g_aCXXKinds[eKindId]));
	
	g_oCXXTag.lineNumber = pRefCXXToken->iLineNumber;
	g_oCXXTag.filePosition = pRefCXXToken->oFilePosition;
	g_oCXXTag.isFileScope = FALSE;

	if(!cxxScopeIsGlobal())
	{
		g_oCXXTag.extensionFields.scopeKind = &g_aCXXKinds[cxxScopeGetKind()];
		g_oCXXTag.extensionFields.scopeName = cxxScopeGetFullName();
	}
	
	// FIXME: meaning of "is file scope" is quite debatable...

	switch(cxxScopeGetAccess())
	{
		case CXXScopeAccessPrivate:
			g_oCXXTag.extensionFields.access = g_aCXXAccessStrings[2];
		break;
		case CXXScopeAccessProtected:
			g_oCXXTag.extensionFields.access = g_aCXXAccessStrings[1];
		break;
		case CXXScopeAccessPublic:
			g_oCXXTag.extensionFields.access = g_aCXXAccessStrings[0];
		break;
		default:
			// ignore
		break;
	}

	return &g_oCXXTag;
}

void cxxTagCommit(void)
{
	if(g_oCXXTag.isFileScope && !isXtagEnabled(XTAG_FILE_SCOPE))
		return;

	CXX_DEBUG_PRINT("Emitting tag for symbol '%s', kind '%s', line %d",g_oCXXTag.name,g_oCXXTag.kind->name,g_oCXXTag.lineNumber);
	if(g_oCXXTag.extensionFields.typeRef[0] && g_oCXXTag.extensionFields.typeRef[1])
		CXX_DEBUG_PRINT("Tag has typeref %s %s",g_oCXXTag.extensionFields.typeRef[0],g_oCXXTag.extensionFields.typeRef[1]);

	makeTagEntry(&g_oCXXTag);
	
	// Handle --extra=+q
	if(!isXtagEnabled(XTAG_QUALIFIED_TAGS))
		return;
	
	if(!g_oCXXTag.extensionFields.scopeName)
		return;
	
	// WARNING: The following code assumes that the scope didn't change between cxxTagBegin() and cxxTagCommit().

	enum CXXTagKind eScopeKind = cxxScopeGetKind();

	if(eScopeKind == CXXTagKindFUNCTION)
		return; // old ctags didn't do this, and --extra=+q is mainly for backward compatibility so...
	
	// Same tag. Only the name changes.

	vString * x;

	if(eScopeKind == CXXTagKindENUM)
	{
		// If the scope kind is enumeration then we need to remove the last scope part. This is what old ctags did.
		if(cxxScopeGetSize() < 2)
			return; // toplevel enum
		
		x = cxxScopeGetFullNameExceptLastComponentAsString();
		CXX_DEBUG_ASSERT(x,"Scope with size >= 2 should have returned a value here");
	} else {
		x = vStringNewInit(g_oCXXTag.extensionFields.scopeName);
	}

	vStringCatS(x,"::");
	vStringCatS(x,g_oCXXTag.name);

	g_oCXXTag.name = vStringValue(x);

	CXX_DEBUG_PRINT("Emitting extra tag for symbol '%s', kind '%s', line %d",g_oCXXTag.name,g_oCXXTag.kind->name,g_oCXXTag.lineNumber);

	makeTagEntry(&g_oCXXTag);
	
	vStringDelete(x);
}

void cxxTag(
		const char * szName,
		enum CXXTagKind eKindId,
		CXXToken * pRefCXXToken
	)
{
	if(cxxTagBegin(szName,eKindId,pRefCXXToken) != NULL)
		cxxTagCommit();
}

