/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_scope.h"

#include "vstring.h"
#include "debug.h"

#include "cxx_debug.h"
#include "cxx_token_chain.h"

// The tokens defining current scope
static CXXTokenChain * g_pScope = NULL;
static vString * g_szScopeName = NULL;
static boolean g_bScopeNameDirty = TRUE;

void cxxScopeInit()
{
	g_pScope = cxxTokenChainCreate();
}

void cxxScopeDone()
{
	cxxTokenChainDestroy(g_pScope);
	if(g_szScopeName)
	{
		vStringDelete(g_szScopeName);
		g_szScopeName = NULL;
	}
}

void cxxScopeClear()
{
	if(g_pScope)
		cxxTokenChainClear(g_pScope);
	if(g_szScopeName)
	{
		vStringDelete(g_szScopeName);
		g_szScopeName = NULL;
	}
}

boolean cxxScopeIsGlobal()
{
	return (g_pScope->iCount < 1);
}

enum CXXTagKind cxxScopeGetVariableKind()
{
	switch(cxxScopeGetKind())
	{
		case CXXTagKindCLASS:
		case CXXTagKindSTRUCT:
		case CXXTagKindUNION:
			return CXXTagKindMEMBER;
		break;
		case CXXTagKindFUNCTION:
			return CXXTagKindLOCAL;
		break;
		//case CXXTagKindNAMESPACE:
		default:
			// fall down
		break;
	}
	return CXXTagKindVARIABLE;
}

enum CXXTagKind cxxScopeGetKind()
{
	if(g_pScope->iCount < 1)
		return CXXTagKindNAMESPACE;
	return (enum CXXTagKind)g_pScope->pTail->uInternalScopeKind;
}

enum CXXScopeAccess cxxScopeGetAccess()
{
	if(g_pScope->iCount < 1)
		return CXXScopeAccessUnknown;
	return (enum CXXScopeAccess)g_pScope->pTail->uInternalScopeAccess;
}

const char * cxxScopeGetName()
{
	if(g_pScope->iCount < 1)
		return NULL;
	return vStringValue(g_pScope->pTail->pszWord);
}

int cxxScopeGetSize()
{
	return g_pScope->iCount;
}

const char * cxxScopeGetFullName()
{
	if(!g_bScopeNameDirty)
		return g_szScopeName ? g_szScopeName->buffer : NULL;

	if(g_pScope->iCount < 1)
	{
		g_bScopeNameDirty = FALSE;
		return NULL;
	}

	if(g_szScopeName)
		vStringClear(g_szScopeName);
	else
		g_szScopeName = vStringNew();

	cxxTokenChainJoinInString(g_pScope,g_szScopeName,"::",CXXTokenChainJoinNoTrailingSpaces);

	g_bScopeNameDirty = FALSE;
	return g_szScopeName->buffer;
}

vString * cxxScopeGetFullNameAsString()
{
	vString * ret;

	if(!g_bScopeNameDirty)
	{
		ret = g_szScopeName;
		g_szScopeName = NULL;
		g_bScopeNameDirty = TRUE;
		return ret;
	}

	if(g_pScope->iCount < 1)
		return NULL;

	if(g_szScopeName)
		vStringClear(g_szScopeName);
	else
		g_szScopeName = vStringNew();

	cxxTokenChainJoinInString(g_pScope,g_szScopeName,"::",CXXTokenChainJoinNoTrailingSpaces);

	ret = g_szScopeName;
	g_szScopeName = NULL;
	return ret;
}

vString * cxxScopeGetFullNameExceptLastComponentAsString()
{
	if(g_pScope->iCount < 2)
		return NULL;

	return cxxTokenChainJoinRange(g_pScope->pHead,g_pScope->pTail->pPrev,"::",CXXTokenChainJoinNoTrailingSpaces);
}


void cxxScopeSetAccess(enum CXXScopeAccess eAccess)
{
	if(g_pScope->iCount < 1)
		return; // warning?
	g_pScope->pTail->uInternalScopeAccess = (unsigned char)eAccess;
}

void cxxScopePush(CXXToken * t,enum CXXTagKind eScopeKind,enum CXXScopeAccess eInitialAccess)
{
	CXX_DEBUG_ASSERT(t->eType == CXXTokenTypeIdentifier,"The scope name must be an identifer");
	CXX_DEBUG_ASSERT(t->pszWord,"The scope name should have a text");
	cxxTokenChainAppend(g_pScope,t);
	t->uInternalScopeKind = (unsigned char)eScopeKind;
	t->uInternalScopeAccess = (unsigned char)eInitialAccess;
	g_bScopeNameDirty = TRUE;

#ifdef CXX_DO_DEBUGGING
	const char * szScopeName = cxxScopeGetFullName();

	CXX_DEBUG_PRINT("Pushed scope: '%s'",szScopeName ? szScopeName : "");
#endif
}

void cxxScopePop()
{
	CXX_DEBUG_ASSERT(g_pScope->iCount > 0,"When popping as scope there must be a scope to pop");

	cxxTokenDestroy(cxxTokenChainTakeLast(g_pScope));
	g_bScopeNameDirty = TRUE;

#ifdef CXX_DO_DEBUGGING
	const char * szScopeName = cxxScopeGetFullName();

	CXX_DEBUG_PRINT("Popped scope: '%s'",szScopeName ? szScopeName : "");
#endif
}
