/*
 *  Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for handling Qt Moc tokens
*/

#include "general.h"

#include "types.h"

#include "debug.h"
#include "cxx_debug.h"

#include "cxx_scope.h"
#include "cxx_parser_internal.h"

#include "cxx_subparser.h"

#include "keyword.h"
#include "read.h"

#include <string.h>


typedef enum {
	K_SLOT,
	K_SIGNAL,
	K_PROPERTY,
} qtMocKind;

static kindDefinition QtMocKinds [] = {
	{ true, 's', "slot", "slots" },
	{ true, 'S', "signal", "signals" },
	{ true, 'p', "property", "properties" },
};

enum {
	KEYWORD_QOBJECT,
	KEYWORD_SIGNALS,
	KEYWORD_SLOTS,
	KEYWORD_PROPERTY,
};

typedef int keywordId; /* to allow KEYWORD_NONE */

static const keywordTable QtMocKeywordTable[] = {
	/* keyword			keyword ID */
	{ "Q_OBJECT",	KEYWORD_QOBJECT  },
	{ "Q_SIGNALS",	KEYWORD_SIGNALS	 },
	{ "signals",	KEYWORD_SIGNALS	 },
	{ "Q_SLOTS",	KEYWORD_SLOTS	 },
	{ "slots",		KEYWORD_SLOTS	 },
	{ "Q_PROPERTY", KEYWORD_PROPERTY },
};

enum QtMocMemberMarker
{
	QtMocMemberMarkerNone = 0,
	QtMocMemberMarkerSlot,
	QtMocMemberMarkerSignal,
};

struct sQtMocSubparser {
	struct sCxxSubparser cxx;
	int iBlockDepth;
	int iDepthOfQtClass;
	enum QtMocMemberMarker eMemberMarker;
};

static langType Lang_QtMoc;

static bool cxxParserSkipToClosingParenthesisOrEOF(void)
{
	if(cxxTokenTypeIsOneOf(g_cxx.pToken,CXXTokenTypeClosingParenthesis | CXXTokenTypeEOF))
		return true;

	return cxxParserParseUpToOneOf(CXXTokenTypeClosingParenthesis | CXXTokenTypeEOF,
								   false);
}

static void qtMocMakeTagForProperty (CXXToken * pToken, const char *pszType)
{
	tagEntryInfo tag;

	initTagEntry(&tag,
				 vStringValue(pToken->pszWord),
				 K_PROPERTY);
	tag.lineNumber = pToken->iLineNumber;
	tag.filePosition = pToken->oFilePosition;
	tag.isFileScope = false;

	if(!cxxScopeIsGlobal())
	{
		tag.extensionFields.scopeLangType = getNamedLanguage ("C++", 0); /* ??? */
		tag.extensionFields.scopeKindIndex = cxxScopeGetKind();
		tag.extensionFields.scopeName = cxxScopeGetFullName();
	}

	tag.extensionFields.typeRef[0] = "typename";
	tag.extensionFields.typeRef[1] = pszType;

	makeTagEntry(&tag);
}

static bool qtMocParseProperty(void)
{
	char *pszPropType;

	CXX_DEBUG_ENTER();

	if(!cxxParserParseNextToken())
	{
		CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
		return false;
	}
	if (!cxxTokenTypeIs(g_cxx.pToken, CXXTokenTypeOpeningParenthesis))
	{
		CXX_DEBUG_LEAVE_TEXT("Found no Opening Parenthesis after Q_PROPERTY");
		return false;
	}

	if (!cxxParserParseNextToken())
	{
		CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
		return false;
	}
	if (!(cxxTokenTypeIs(g_cxx.pToken, CXXTokenTypeIdentifier)
		  || (cxxTokenTypeIs(g_cxx.pToken, CXXTokenTypeKeyword)
			  && cxxKeywordMayBePartOfTypeName (g_cxx.pToken->eKeyword))))

	{
		CXX_DEBUG_LEAVE_TEXT("Found no identifier after Q_PROPERTY(");

		cxxParserSkipToClosingParenthesisOrEOF ();
		return false;
	}

	pszPropType = vStringStrdup (g_cxx.pToken->pszWord);
	if(!cxxParserParseNextToken())
	{
		CXX_DEBUG_LEAVE_TEXT("EOF in cxxParserParseNextToken");
		eFree (pszPropType);
		return false;
	}

	if (!cxxTokenTypeIs(g_cxx.pToken, CXXTokenTypeIdentifier))
	{
		CXX_DEBUG_LEAVE_TEXT("Found no identifier after Q_PROPERTY(%s", pszPropType);
		cxxParserSkipToClosingParenthesisOrEOF ();
		eFree (pszPropType);
		return false;
	}

	qtMocMakeTagForProperty (g_cxx.pToken, pszPropType);

	eFree (pszPropType);
	cxxParserSkipToClosingParenthesisOrEOF ();

	CXX_DEBUG_LEAVE();
	return true;
}

static void inputStart(subparser *s)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser*)s;

	pQtMoc->iBlockDepth = 0;
	pQtMoc->iDepthOfQtClass = 0;
	pQtMoc->eMemberMarker = QtMocMemberMarkerNone;
}

static void makeTagEntryNotify (subparser *s, const tagEntryInfo *entry, int corkIndex)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser*)s;

	if (pQtMoc->iDepthOfQtClass == 0)
		return;

	if ((pQtMoc->eMemberMarker != QtMocMemberMarkerNone) &&
		entry->kindIndex == CXXTagKindPROTOTYPE)
	{
		tagEntryInfo parasiteTag = *entry;
		parasiteTag.langType = getInputLanguage ();
		parasiteTag.kindIndex = (pQtMoc->eMemberMarker == QtMocMemberMarkerSlot)
			? K_SLOT
			: K_SIGNAL;

		parasiteTag.extensionFields.scopeLangType = entry->langType;
		makeTagEntry (&parasiteTag);
	}
}

static void enterBlockNotify (struct sCxxSubparser *pSubparser)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser *)pSubparser;

	pQtMoc->iBlockDepth++;
}

static void leaveBlockNotify (struct sCxxSubparser *pSubparser)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser *)pSubparser;

	if (pQtMoc->iDepthOfQtClass == pQtMoc->iBlockDepth)
		pQtMoc->iDepthOfQtClass = 0;

	pQtMoc->iBlockDepth--;
}

static bool newIdentifierAsHeadOfMemberNotify (struct sCxxSubparser *pSubparser,
											   CXXToken *pToken)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser *)pSubparser;
	keywordId keyword = lookupKeyword (vStringValue (pToken->pszWord), Lang_QtMoc);

	if (keyword == KEYWORD_QOBJECT)
	{
		if (pQtMoc->iDepthOfQtClass == 0)
			pQtMoc->iDepthOfQtClass = pQtMoc->iBlockDepth;
		CXX_DEBUG_PRINT("Found \"Q_OBJECT\" Qt Object Marker in depth: %d",
						pQtMoc->iDepthOfQtClass);
		return true;
	}
	return false;
}

static bool unknownIdentifierInClassNotify (struct sCxxSubparser *pSubparser,
											CXXToken *pToken)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser *)pSubparser;

	if (pQtMoc->iDepthOfQtClass == 0)
		return false;

	keywordId keyword = lookupKeyword (vStringValue (pToken->pszWord), Lang_QtMoc);

	switch (keyword)
	{
	case KEYWORD_SIGNALS:
		CXX_DEBUG_PRINT("Found \"signals\" QtMoc Keyword");
		pToken->eType = CXXTokenTypeKeyword;
		pToken->eKeyword = CXXKeywordPUBLIC;
		cxxParserParseAccessSpecifier();
		pQtMoc->eMemberMarker = QtMocMemberMarkerSignal;
		return true;
	case KEYWORD_SLOTS:
		CXX_DEBUG_PRINT("Found \"slots\" QtMoc Keyword");
		pToken->eType = CXXTokenTypeKeyword;
		g_cxx.pToken->eKeyword = CXXKeywordPUBLIC; /* ??? */
		cxxParserParseAccessSpecifier();
		pQtMoc->eMemberMarker = QtMocMemberMarkerSlot;
		return true;
	case KEYWORD_PROPERTY:
		CXX_DEBUG_PRINT("Found \"Q_PROPERTY\" QtMoc Keyword");
		qtMocParseProperty ();
		return true;
	default:
		break;
	}

	return false;
}

static bool parseAccessSpecifierNotify(struct sCxxSubparser *pSubparser)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser *)pSubparser;

	if (pQtMoc->iBlockDepth > 0)
	{
		CXX_DEBUG_PRINT("Reset QtMoc member marker state");
		pQtMoc->eMemberMarker = QtMocMemberMarkerNone;
		return true;
	}
	return false;
}

static void foundExtraIdentifierAsAccessSpecifier(struct sCxxSubparser *pSubparser,
												  CXXToken *pToken)
{
	struct sQtMocSubparser *pQtMoc = (struct sQtMocSubparser *)pSubparser;
	keywordId keyword = lookupKeyword (vStringValue (pToken->pszWord), Lang_QtMoc);

	if (keyword == KEYWORD_SLOTS)
	{
		CXX_DEBUG_PRINT("Found \"slots\" QtMoc Keyword");
		pQtMoc->eMemberMarker = QtMocMemberMarkerSlot;
	}
}

static void findQtMocTags(void)
{
	scheduleRunningBaseparser (0);
}

static void initialize (langType lang)
{
	Lang_QtMoc = lang;
}

extern parserDefinition* QtMocParser (void)
{
	parserDefinition* const def = parserNew("QtMoc");

	static struct sQtMocSubparser qtMocSubparser = {
		.cxx = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
				.inputStart = inputStart,
				.makeTagEntryNotify = makeTagEntryNotify,
			},
			.enterBlockNotify = enterBlockNotify,
			.leaveBlockNotify = leaveBlockNotify,
			.newIdentifierAsHeadOfMemberNotify = newIdentifierAsHeadOfMemberNotify,
			.unknownIdentifierInClassNotify = unknownIdentifierInClassNotify,
			.parseAccessSpecifierNotify = parseAccessSpecifierNotify,
			.foundExtraIdentifierAsAccessSpecifier = foundExtraIdentifierAsAccessSpecifier,
		}
		/* The rest fields are initialized in inputStart(). */
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "C++", &qtMocSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = QtMocKinds;
	def->kindCount = ARRAY_SIZE(QtMocKinds);

	def->keywordTable = QtMocKeywordTable;
	def->keywordCount = ARRAY_SIZE (QtMocKeywordTable);

	def->parser = findQtMocTags;
	def->initialize = initialize;
	def->useCork = CORK_QUEUE;

	return def;
}
