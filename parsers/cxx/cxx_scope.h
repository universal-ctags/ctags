#ifndef ctags_cxx_scope_h_
#define ctags_cxx_scope_h_
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

enum CXXScopeAccess
{
	CXXScopeAccessUnknown,
	CXXScopeAccessPublic,
	CXXScopeAccessPrivate,
	CXXScopeAccessProtected
};

enum CXXScopeType
{
	CXXScopeTypeFunction,
	CXXScopeTypeNamespace,
	CXXScopeTypeClass,
	CXXScopeTypeEnum,
	CXXScopeTypeUnion,
	CXXScopeTypeStruct,
	CXXScopeTypeVariable, // template variables, mainly
	CXXScopeTypePrototype,
	CXXScopeTypeTypedef,  // template variables used in "using A = B<T>"
	CXXScopeTypeLAST
};

void cxxScopeInit(void);
void cxxScopeDone(void);
void cxxScopeClear(void);

// Returns the full current scope name or NULL if there
// is no scope currently.
const char * cxxScopeGetFullName(void);

// Returns the current scope name of NULL if there is no
// scope currently. This name does not include namespaces so
// it is always a single identifier.
const char * cxxScopeGetName(void);

// Return the number of components of the scope name.
int cxxScopeGetSize(void);

// Returns the current scope name or NULL if there is no scope
// currently. Ownership of the string is transferred.
vString * cxxScopeGetFullNameAsString(void);

// Returns the current scope name with the exception of the
// last component or NULL if there is either no scope or there
// are less than two components. Ownership of the string is transferred.
vString * cxxScopeGetFullNameExceptLastComponentAsString(void);

enum CXXScopeType cxxScopeGetType(void);
// Returns the current scope kind
unsigned int cxxScopeGetKind(void);
unsigned int cxxScopeGetVariableKind(void);
enum CXXScopeAccess cxxScopeGetAccess(void);
// Are we in global scope?
bool cxxScopeIsGlobal(void);

// Add a token to the scope chain. The token ownership is transferred.
void cxxScopePush(
		CXXToken * t,
		enum CXXScopeType eScopeType,
		enum CXXScopeAccess eInitialAccess
	);
void cxxScopeSetAccess(enum CXXScopeAccess eAccess);
// Remove the last token from the scope chain
void cxxScopePop(void);

// Special management: pop one scope level but keep it so it can be pushed back
CXXToken * cxxScopeTakeTop(void);
// Special management: push back a scope taken earlier via cxxScopeTakeTop()
void cxxScopePushTop(CXXToken * t);

#endif //!ctags_cxx_scope_h_
