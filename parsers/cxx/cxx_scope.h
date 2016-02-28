#ifndef _cxx_scope_h_
#define _cxx_scope_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "cxx_token.h"
#include "cxx_tag.h"

enum CXXScopeAccess
{
	CXXScopeAccessUnknown,
	CXXScopeAccessPublic,
	CXXScopeAccessPrivate,
	CXXScopeAccessProtected
};

void cxxScopeInit();
void cxxScopeDone();
void cxxScopeClear();

// Returns the full current scope name or NULL if there is no scope currently.
const char * cxxScopeGetFullName();

// Returns the current scope name of NULL if there is no scope currnetly.
// This name does not include namespaces so it is always a single identifier.
const char * cxxScopeGetName();

// Return the number of components of the scope name.
int cxxScopeGetSize();

// Returns the current scope name or NULL if there is no scope currently.
// Ownership of the string is transferred.
vString * cxxScopeGetFullNameAsString();

// Returns the current scope name with the exception of the last component
// or NULL if there is either no scope or there are less than two components.
// Ownership of the string is transferred.
vString * cxxScopeGetFullNameExceptLastComponentAsString();

// Returns the current scope kind
enum CXXTagKind cxxScopeGetKind();
enum CXXTagKind cxxScopeGetVariableKind();
enum CXXScopeAccess cxxScopeGetAccess();
// Are we in global scope?
boolean cxxScopeIsGlobal();

// Add a token to the scope chain. The token ownership is transferred.
void cxxScopePush(CXXToken * t,enum CXXTagKind eScopeKind,enum CXXScopeAccess eInitialAccess);
void cxxScopeSetAccess(enum CXXScopeAccess eAccess);
void cxxScopePop();



#endif //!_cxx_scope_h_