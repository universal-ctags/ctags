/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for extracting reference tags
*/

#include "cxx_reftag.h"

#include "cxx_token.h"
#include "cxx_parser_internal.h"
#include "cxx_scope.h"
#include "read.h"


void cxxReftagEvalNewToken(void)
{
	if(cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeIdentifier) &&
	   cxxTagKindEnabled(CXXTagKindUNKNOWN) &&
	   cxxTagRoleEnabled(CXXTagKindUNKNOWN, CXXTagUnknownRoleREFERENCED))
	{
		// QtMoc subparser calls this function indirectly.
		// However, "unknown" kind + "ref" role is part of the
		// base parser like C++. They are not part of the subparser.
		// So we have to swtich the context for making the reference tag
		// temporarily here.
		const bool in_subparser = (g_cxx.eLangType != getInputLanguage());
		if (in_subparser)
			pushLanguage(g_cxx.eLangType);

		tagEntryInfo oEntry;
		initRefTagEntry(&oEntry, vStringValue(g_cxx.pToken->pszWord),
						CXXTagKindUNKNOWN, CXXTagUnknownRoleREFERENCED);
		oEntry.extensionFields.scopeIndex = cxxScopeGetDefTag();
		g_cxx.pToken->iCorkIndex = makeTagEntry(&oEntry);
		g_cxx.pToken->bCorkIndexForReftag = true;

		if (in_subparser)
			popLanguage();
	}
}

void cxxReftagReset(int iCorkIndex, int iScopeCorkIndex,
					int iKindIndex, int iRoleIndex, bool bUpdateScope)
{
	tagEntryInfo *pEntry = getEntryInCorkQueue(iCorkIndex);
	if(pEntry == NULL)
		return;
	if (pEntry->placeholder)
		return;

	clearRoles(pEntry);
	if(bUpdateScope && pEntry->extensionFields.scopeIndex != CORK_NIL)
		unregisterEntry(iCorkIndex);
	if (bUpdateScope)
		pEntry->extensionFields.scopeIndex = iScopeCorkIndex;
	pEntry->kindIndex = iKindIndex;
	assignRole(pEntry, iRoleIndex);
	if (bUpdateScope)
		registerEntry(iCorkIndex);
}
