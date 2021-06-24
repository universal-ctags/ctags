/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_debug.h"

#ifdef CXX_DO_DEBUGGING

#include "trashbox.h"
#include "cxx_parser_internal.h"
#include "cxx_scope.h"

static void cxxDebugDumpToken0 (CXXToken *pToken,
								struct circularRefChecker *pTokenChecker,
								struct circularRefChecker *pChainChecker,
								bool top_level);

static void cxxDebugDumpChain0 (CXXTokenChain *pChain,
								struct circularRefChecker *pTokenChecker,
								struct circularRefChecker *pChainChecker,
								bool top_level)
{
	int backref;

	if (top_level)
	{
		debugIndent ();
		fprintf (stderr, "<chain ");
	}
	else if (pChain == NULL)
	{
		fprintf (stderr, "NULL\n");
		return;
	}
	else
	{
		fprintf (stderr, "<");
	}

	backref = circularRefCheckerCheck (pChainChecker, pChain);
	if (backref)
	{
		fprintf (stderr, "*C#%d>\n", backref);
		return;
	}

	backref = circularRefCheckerGetCurrent (pChainChecker);

	fprintf (stderr, "[%d %p&C#%d]\n", pChain->iCount, pChain, backref);

	debugInc();
	debugIndent ();
	cxxDebugDumpToken0 (pChain->pHead, pTokenChecker, pChainChecker, false);
	debugDec();

	debugIndent ();
	fprintf (stderr, ">\n");
}

static void cxxDebugDumpToken0 (CXXToken *pToken,
								struct circularRefChecker *pTokenChecker,
								struct circularRefChecker *pChainChecker,
								bool top_level)
{
	int backref;

	if (top_level)
	{
		debugIndent ();
		fprintf (stderr, "<token ");
	}
	else if (pToken == NULL)
	{
		fprintf (stderr, "NULL\n");
		return;
	}
	else
	{
		fprintf (stderr, "<");
	}

	backref = circularRefCheckerCheck (pTokenChecker, pToken);
	if (backref)
	{
		fprintf (stderr, "*T#%d>\n", backref);
		return;
	}

	backref = circularRefCheckerGetCurrent (pTokenChecker);

	fprintf (stderr, "\"%s\": [%s %p &T#%d]\n",
			 vStringValue (pToken->pszWord),
			 cxxDebugTypeDecode (pToken->eType), pToken, backref);

	debugIndent ();
	fprintf (stderr, "  chain: ");
	debugInc();
	cxxDebugDumpChain0 (pToken->pChain, pTokenChecker, pTokenChecker, false);
	debugDec();

	debugIndent ();
	fprintf (stderr, "  next: ");
	debugInc();
	cxxDebugDumpToken0 (pToken->pNext, pTokenChecker, pTokenChecker, false);
	debugDec();

	debugIndent ();
	fprintf (stderr, "  prev: ");
	debugInc();
	cxxDebugDumpToken0 (pToken->pPrev, pTokenChecker, pTokenChecker, false);
	debugDec();

	debugIndent ();
	fprintf (stderr, ">\n");
}

typedef void (* cxxDebugDumpCommonFunc)(void *,
										struct circularRefChecker *,
										struct circularRefChecker *,
										bool);
void cxxDebugDumpCommon (void *data,
						 void (* func)(void *,
									   struct circularRefChecker *,
									   struct circularRefChecker *,
									   bool))
{
	static struct circularRefChecker *pTokenChecker;
	static struct circularRefChecker *pChainChecker;

	if (!pTokenChecker)
	{
		pTokenChecker = circularRefCheckerNew();
		DEFAULT_TRASH_BOX(pTokenChecker, (TrashBoxDestroyItemProc)circularRefCheckerDestroy);
	}

	if (!pChainChecker)
	{
		pChainChecker = circularRefCheckerNew();
		DEFAULT_TRASH_BOX(pChainChecker, (TrashBoxDestroyItemProc)circularRefCheckerDestroy);
	}

	func(data, pTokenChecker, pChainChecker, true);

	circularRefCheckClear (pTokenChecker);
	circularRefCheckClear (pChainChecker);
}

void cxxDebugDumpToken (CXXToken *pToken)
{
	cxxDebugDumpCommon (pToken, (cxxDebugDumpCommonFunc)cxxDebugDumpToken0);
}

void cxxDebugDumpChain (CXXTokenChain *pChain)
{
	cxxDebugDumpCommon (pChain, (cxxDebugDumpCommonFunc)cxxDebugDumpChain0);
}

const char* cxxDebugScopeDecode(enum CXXScopeType scope)
{
	const char * table[] = {
		[CXXScopeTypeFunction]  = "function",
		[CXXScopeTypeNamespace] = "namespace",
		[CXXScopeTypeClass] = "class",
		[CXXScopeTypeEnum] = "enum",
		[CXXScopeTypeUnion] = "union",
		[CXXScopeTypeStruct] = "struct",
		[CXXScopeTypeVariable] = "variable",
		[CXXScopeTypePrototype]  = "prototype",
		[CXXScopeTypeTypedef] = "typedef",
	};
	if (CXXScopeTypeLAST > scope)
		return table[scope];
	else
		return NULL;
}

#endif
