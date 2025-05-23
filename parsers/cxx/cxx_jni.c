/*
*  Copyright (c) 2025, Red Hat, Inc.
*   Copyright (c) 2025, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for handling JNI names.
*/

#include "general.h"

#include "entry.h"

#include "cxx_tag.h"
#include "cxx_subparser.h"
#include "cxx_token_chain.h"

#include <string.h>

struct sJNISubparser {
	struct sCxxSubparser cxx;
	int JNINativeMethodVarIndex;
};

typedef enum {
	K_METHOD,
} jniKind;


static kindDefinition JNIKinds [] = {
	{ true, 'm', "method", "methods" },
};

static langType Lang_C;
static langType Lang_Cxx;

static void inputStart(subparser *s)
{
	struct sJNISubparser *pJNI = (struct sJNISubparser*)s;

	pJNI->JNINativeMethodVarIndex = CORK_NIL;
}

static void makeTagEntryNotify (subparser *s, const tagEntryInfo *entry, int corkIndex)
{
	if ((entry->langType == Lang_C || entry->langType == Lang_Cxx)
		&& (entry->kindIndex == CXXTagKindVARIABLE || entry->kindIndex == CXXTagKindLOCAL)
		&& entry->extensionFields.typeRef[1]
		&& strstr (entry->extensionFields.typeRef[1],
				   "JNINativeMethod"))
	{
		struct sJNISubparser *pJNI = (struct sJNISubparser*)s;
		pJNI->JNINativeMethodVarIndex = corkIndex;
	}
}

static bool wantsVariableBody (struct sCxxSubparser *pSubparser, CXXToken * pEndOfRightSide)
{
	// JNINativeMethod varname [] = { ....
	//                         ^
	// pEndOfRightSide --------|

	return ( cxxTokenTypeIs(pEndOfRightSide,CXXTokenTypeSquareParenthesisChain)
			 && pEndOfRightSide->pPrev
			 && cxxTokenTypeIs(pEndOfRightSide->pPrev,CXXTokenTypeIdentifier)
			 && pEndOfRightSide->pPrev->pPrev
			 && cxxTokenTypeIs(pEndOfRightSide->pPrev->pPrev,CXXTokenTypeIdentifier)
			 && strcmp(vStringValue(pEndOfRightSide->pPrev->pPrev->pszWord),"JNINativeMethod") == 0);
}

static void jniMakeTagEntryForMethod (CXXToken * pToken, int scopeIndex,
									  vString *signature, vString *typeref)
{
	tagEntryInfo tag;

	const char *name = vStringValue (pToken->pszWord);

	if (vStringIsEmpty (pToken->pszWord))
		return;


	if (vStringLastSafe (pToken->pszWord) == '"'
		&& vStringValue (pToken->pszWord)[0] == '"')
	{
		if (vStringLength (pToken->pszWord) == 2)
			return;

		// surgery extracting name from "name".
		name = vStringValue (pToken->pszWord) + 1;
		vStringTruncate (pToken->pszWord,
						 vStringLength (pToken->pszWord) - 1);
	}

	initTagEntry(&tag, name, K_METHOD);
	updateTagLine (&tag, pToken->iLineNumber, pToken->oFilePosition);
	tag.isFileScope = false;

	tag.extensionFields.scopeIndex = scopeIndex;

	if (signature)
		tag.extensionFields.signature = vStringValue (signature);
	if (typeref)
	{
		tag.extensionFields.typeRef [0] = "typename";
		tag.extensionFields.typeRef [1] = vStringValue (typeref);
	}

	makeTagEntry(&tag);

	if (name != vStringValue (pToken->pszWord))
		vStringPut (pToken->pszWord, '"');
}

static vString *extractSignature0 (CXXToken *pSignature, vString *pszSignature)
{
	if (!pSignature)
		return pszSignature;

	if (!cxxTokenTypeIs (pSignature, CXXTokenTypeStringConstant))
		return pszSignature;

	if (!pszSignature)
		pszSignature = vStringNew ();

	for (size_t i = 0; i < vStringLength (pSignature->pszWord); i++)
	{
		char c = vStringChar (pSignature->pszWord, i);
		if (c == '"')
			continue;

		vStringPut (pszSignature, c);
	}

	return extractSignature0 (pSignature->pNext, pszSignature);
}

static vString *extractSignature (CXXToken *pSignature)
{
	return extractSignature0 (pSignature, NULL);
}

static vString *extractTyperef (vString *pszSignature)
{
	vString *pszTyperef = NULL;
	const char *str = vStringValue (pszSignature);
	const char *t = strrchr (str, ')');

	if (t && t[1])
	{
		pszTyperef = vStringNewInit (t + 1);
		size_t  newlen = vStringLength (pszSignature) - vStringLength (pszTyperef);
		vStringTruncate (pszSignature, newlen);
	}

	return pszTyperef;
}

static void parseMethodEntry (CXXToken *pMethod, int iVarCork)
{
	while (pMethod && !cxxTokenTypeIs (pMethod, CXXTokenTypeStringConstant))
	{
		if (cxxTokenTypeIs (pMethod, CXXTokenTypeComma)
			|| cxxTokenTypeIs (pMethod, CXXTokenTypeClosingBracket))
			return;

		pMethod = pMethod->pNext;
	}
	if (!pMethod || !cxxTokenTypeIs (pMethod, CXXTokenTypeStringConstant))
		return;

	CXXToken *pComma = pMethod->pNext;
	while (pComma && !cxxTokenTypeIs (pComma, CXXTokenTypeComma))
		pComma = pComma->pNext;
	if (!pComma || !cxxTokenTypeIs (pComma, CXXTokenTypeComma))
		return;

	CXXToken *pSignature = pComma->pNext;
	while (pSignature && !cxxTokenTypeIs (pSignature, CXXTokenTypeStringConstant))
		pSignature = pSignature->pNext;
	if (!pSignature || !cxxTokenTypeIs (pSignature, CXXTokenTypeStringConstant))
		return;

	vString *pszSignature =extractSignature (pSignature);
	if (!pszSignature)
		return;

	if (vStringValue (pszSignature) [0] != '(')
	{
		vStringDelete (pszSignature);
		return;
	}

	vString *pszTyperef = extractTyperef (pszSignature);
	if (!pszTyperef)
	{
		vStringDelete (pszSignature);
		return;
	}

	if (pszSignature && pszTyperef)
		jniMakeTagEntryForMethod (pMethod, iVarCork,
								  pszSignature, pszTyperef);

	/* NULLs are acceptable. */
	vStringDelete (pszTyperef);
	vStringDelete (pszSignature);
}

static void variableBodyNotify (struct sCxxSubparser *pSubparser, int iVarCork,
								CXXToken * pStart, CXXToken * pEnd)
{
	struct sJNISubparser *pJNI = (struct sJNISubparser*)pSubparser;

	if (iVarCork != pJNI->JNINativeMethodVarIndex)
		return;

	if (!cxxTokenTypeIs (pStart, CXXTokenTypeBracketChain))
		return;

	CXXTokenChain *pTopChain = pStart->pChain;
	for (CXXToken *pT = pTopChain->pHead; pT; pT = pT->pNext)
	{
		if (!cxxTokenTypeIs (pT, CXXTokenTypeBracketChain))
			continue;

		CXXTokenChain *pEntryChain = pT->pChain;
		if (!pEntryChain)
			continue;

		CXXToken *pEntryHead = pEntryChain->pHead;
		if (!pEntryHead)
			continue;

		if (pEntryHead->pNext)
			parseMethodEntry (pEntryHead->pNext, iVarCork);
	}
}

static void initialize (langType lang)
{
	Lang_C = getNamedLanguage ("C", 0);
	Lang_Cxx = getNamedLanguage ("C++", 0);
}

static void findJNITags(void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* JNIParser (void)
{
	parserDefinition* const def = parserNew("JNI");

	static struct sJNISubparser jniSubparser = {
		.cxx = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
				.inputStart = inputStart,
				.makeTagEntryNotify = makeTagEntryNotify,
			},
			.wantsVariableBody = wantsVariableBody,
			.variableBodyNotify = variableBodyNotify,
		},
		. JNINativeMethodVarIndex = CORK_NIL,
	};

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "C++", &jniSubparser },
		[1] = { DEPTYPE_SUBPARSER, "C", &jniSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = JNIKinds;
	def->kindCount = ARRAY_SIZE(JNIKinds);

	def->parser = findJNITags;
	def->initialize = initialize;
	def->useCork = CORK_QUEUE;

	return def;
}
