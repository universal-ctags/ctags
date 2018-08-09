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

#include "cxx_subparser_internal.h"
#include "cxx_token_chain.h"


bool cxxSubparserNotifyParseAccessSpecifier (ptrArray *pSubparsers)
{
	bool bR = false;
	subparser *pSubparser;

	foreachSubparser (pSubparser, false)
	{
		cxxSubparser *pS = (cxxSubparser *)pSubparser;
		if (pS->parseAccessSpecifierNotify)
		{
			enterSubparser(pSubparser);
			if (pS->parseAccessSpecifierNotify (pS))
			{
				ptrArrayAdd(pSubparsers, pS);
				bR = true;
			}
			leaveSubparser();
		}
	}
	return bR;
}

void cxxSubparserNotifyfoundExtraIdentifierAsAccessSpecifier(ptrArray *pSubparsers,
															 CXXToken *pToken)
{
	unsigned int c = ptrArrayCount(pSubparsers);
	for (unsigned int i = 0; i < c; i++)
	{
		cxxSubparser *pS = ptrArrayItem (pSubparsers, i);
		if (pS->foundExtraIdentifierAsAccessSpecifier)
		{
			enterSubparser((subparser*)pS);
			pS->foundExtraIdentifierAsAccessSpecifier(pS, pToken);
			leaveSubparser();
		}
	}
}

bool cxxSubparserNewIdentifierAsHeadOfMemberNotify(CXXToken *pToken)
{
	subparser *pSubparser;
	bool handled = false;

	foreachSubparser (pSubparser, false)
	{
		cxxSubparser *pS = (cxxSubparser *)pSubparser;
		if (pS->newIdentifierAsHeadOfMemberNotify)
		{
			enterSubparser(pSubparser);
			if (pS->newIdentifierAsHeadOfMemberNotify (pS, pToken))
				handled = true;
			leaveSubparser();
			if (handled)
				break;
		}
	}
	return handled;
}

void cxxSubparserUnknownIdentifierInClassNotify(CXXToken *pToken)
{
	subparser *pSubparser;
	bool handled = false;

	foreachSubparser (pSubparser, false)
	{
		cxxSubparser *pS = (cxxSubparser *)pSubparser;
		if (pS->unknownIdentifierInClassNotify)
		{
			enterSubparser(pSubparser);
			if (pS->unknownIdentifierInClassNotify (pS, pToken))
				handled = true;
			leaveSubparser();
			if (handled)
				break;
		}

	}
}

void cxxSubparserNotifyEnterBlock (void)
{
	subparser *pSubparser;
	foreachSubparser (pSubparser, false)
	{
		cxxSubparser *pS = (cxxSubparser *)pSubparser;
		if (pS->enterBlockNotify)
		{
			enterSubparser(pSubparser);
			pS->enterBlockNotify (pS);
			leaveSubparser();
		}
	}
}

void cxxSubparserNotifyLeaveBlock (void)
{
	subparser *pSubparser;
	foreachSubparser (pSubparser, false)
	{
		cxxSubparser *pS = (cxxSubparser *)pSubparser;
		if (pS->leaveBlockNotify)
		{
			enterSubparser(pSubparser);
			pS->leaveBlockNotify (pS);
			leaveSubparser();
		}
	}
}
