#ifndef ctags_cxx_subparser_h_
#define ctags_cxx_subparser_h_
/*
 *  Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "subparser.h"

#include "cxx_token.h"


typedef struct sCxxSubparser cxxSubparser;
struct sCxxSubparser {
	subparser subparser;

	void (* enterBlockNotify) (struct sCxxSubparser *pSubparser);
	void (* leaveBlockNotify) (struct sCxxSubparser *pSubparser);

	/* Return true if the base parser should delete the token. */
	bool (* newIdentifierAsHeadOfMemberNotify) (struct sCxxSubparser *pSubparser,
											  CXXToken * pToken);

	/* Return true if the subparser consumes the token and the base
	   parser should not call the other subparsers. */
	bool (* unknownIdentifierInClassNotify) (struct sCxxSubparser *pSubparser,
											 CXXToken * pToken);

	/* Return true from parseAccessSpecifierNotify () if a subparser
	   has an interest in extra identifier in place where an access
	   specifier is written. The token holding the extra identifier
	   has passed via foundExtraIdentifierAsAccessSpecifier method. */
	bool (* parseAccessSpecifierNotify) (struct sCxxSubparser *pSubparser);
	void (* foundExtraIdentifierAsAccessSpecifier) (struct sCxxSubparser *pSubparser,
													CXXToken * pToken);
};

#endif //!ctags_cxx_subparser_h_
