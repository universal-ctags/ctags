#ifndef ctags_cxx_subparser_interanl_h_
#define ctags_cxx_subparser_interanl_h_
/*
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "cxx_subparser.h"
#include "ptrarray.h"

bool cxxSubparserNotifyParseAccessSpecifier (ptrArray *pSubparsers);
void cxxSubparserNotifyfoundExtraIdentifierAsAccessSpecifier(ptrArray *pSubparsers,
															 CXXToken *pToken);

bool cxxSubparserNewIdentifierAsHeadOfMemberNotify(CXXToken *pToken);
void cxxSubparserUnknownIdentifierInClassNotify(CXXToken *pToken);
void cxxSubparserNotifyEnterBlock (void);
void cxxSubparserNotifyLeaveBlock (void);

#endif //!ctags_cxx_subparser_interanl_h_
