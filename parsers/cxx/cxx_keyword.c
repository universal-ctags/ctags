/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_keyword.h"
#include "cxx_parser_internal.h"
#include "cxx_debug.h"

#include "keyword.h"

enum CXXKeywordFlag
{
	// Keywords that in most cases are parts of the name of a type.
	// Examples: int, void, const, float, stuff like that
	CXXKeywordFlagMayBePartOfTypeName = 1,
	// struct, class, union, enum, typename
	CXXKeywordIsTypeRefMarker = (1 << 1),
	// Stuff that often appears together with a type name
	// (for example a function return type or a variable type)
	// but is not part of the type itself.
	// Examples: virtual, inline, friend, static
	CXXKeywordExcludeFromTypeNames = (1 << 2),
	// true, false, nullptr
	CXXKeywordIsConstant = (1 << 3),
	// certain keywords are disabled "on-the-fly" to better
	// handle C / C++ guessing errors (public, protected, private, namespace etc..)
	CXXKeywordIsDisabled = (1 << 4),
	// Similar to MayBePartOfTypeName but includes more keywords that are NOT part
	// of the type itself. Keywords that do NOT have this flag simply cannot appear
	// in a variable declaration.
	// Examples: __global__, __host__, restrict, register...
	CXXKeywordMayAppearInVariableDeclaration = (1 << 5)
};

typedef struct _CXXKeywordDescriptor
{
	const char * szName;
	unsigned int uLanguages;
	unsigned int uFlags;
} CXXKeywordDescriptor;


// This array is indexed by the CXXKeywordType enum
static CXXKeywordDescriptor g_aCXXKeywordTable[] = {
	{
		"__attribute__",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration
	},
	{
		"__constant__",
		CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__declspec",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__device__",
		CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__fastcall",
		CXXLanguageCPP
	},
	{
		"__forceinline",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordExcludeFromTypeNames
	},
	{
		"__forceinline__",
		CXXLanguageCUDA,
		CXXKeywordExcludeFromTypeNames
	},
	{
		"__global__",
		CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__host__",
		CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__inline",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__inline__",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__managed__",
		CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__noinline__",
		CXXLanguageCUDA,
		CXXKeywordExcludeFromTypeNames
	},
	{
		"__restrict",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__restrict__",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__shared__",
		CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"__stdcall",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"__thiscall",
		CXXLanguageCPP,
		0
	},
	{
		"alignas",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration
	},
	{
		"alignof",
		CXXLanguageCPP,
		0
	},
	//{ 1, "and", 0 },
	//{ 1, "and_eq", 0 },
	{
		"asm",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"auto",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	//{ 1, "bitand", 0 },
	//{ 1, "bitor", 0 },
	{
		"bool",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"break",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"case",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"catch",
		CXXLanguageCPP,
		0
	},
	{
		"char",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"char16_t",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"char32_t",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"class",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName |
			CXXKeywordIsTypeRefMarker
	},
	//{ 0, "compl", 0 },
	{
		"concept",
		CXXLanguageCPP,
		0
	},
	{
		"const",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"constexpr",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"const_cast",
		CXXLanguageCPP,
		0
	},
	{
		"continue",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"decltype",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"default",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"delete",
		CXXLanguageCPP,
		0
	},
	{
		"do",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"double",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"dynamic_cast",
		CXXLanguageCPP,
		0
	},
	{
		"else",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"enum",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName |
			CXXKeywordIsTypeRefMarker
	},
	{
		"explicit",
		CXXLanguageCPP,
		0
	},
	{
		"export",
		CXXLanguageCPP,
		0
	},
	{
		"extern",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"false",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordIsConstant
	},
	// this is a keyword only in special contexts (we have a switch to enable/disable it)
	{
		"final",
		CXXLanguageCPP,
		0
	},
	{
		"float",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"for",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"friend",
		CXXLanguageCPP,
		CXXKeywordExcludeFromTypeNames
	},
	{
		"goto",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"if",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"inline",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"int",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"long",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"mutable",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration
	},
	{
		"namespace",
		CXXLanguageCPP,
		0
	},
	{
		"new",
		CXXLanguageCPP,
		0
	},
	{
		"noexcept",
		CXXLanguageCPP,
		0
	},
	//{ 0, "not", 0 },
	//{ 0, "not_eq", 0 },
	{
		"nullptr",
		CXXLanguageCPP,
		CXXKeywordIsConstant
	},
	{
		"operator",
		CXXLanguageCPP,
		0
	},
	//{ 0, "or", 0 },
	//{ 0, "or_eq", 0 },
	// override is a keyword only after function declarators,
	// it's easier handling it as identifier
	//{ 0, "override", 0 },
	{
		"private",
		CXXLanguageCPP,
		0
	},
	{
		"protected",
		CXXLanguageCPP,
		0
	},
	{
		"public",
		CXXLanguageCPP,
		0
	},
	{
		"register",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration
	},
	{
		"reinterpret_cast",
		CXXLanguageCPP,
		0
	},
	{
		"requires",
		CXXLanguageCPP,
		0
	},
	{
		"restrict",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"return",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"short",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"signed",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"sizeof",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"static",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordExcludeFromTypeNames
	},
	{
		"static_assert",
		CXXLanguageCPP,
		0
	},
	{
		"static_cast",
		CXXLanguageCPP,
		0
	},
	{
		"struct",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName |
			CXXKeywordIsTypeRefMarker
	},
	{
		"switch",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"template",
		CXXLanguageCPP,
		0
	},
	{
		"this",
		CXXLanguageCPP,
		0
	},
	{
		"thread_local",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration
	},
	{
		"throw",
		CXXLanguageCPP,
		0
	},
	{
		"true",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordIsConstant
	},
	{
		"try",
		CXXLanguageCPP,
		0
	},
	{
		"typedef",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	{
		"typeid",
		CXXLanguageCPP,
		0
	},
	{
		"typename",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName |
			CXXKeywordIsTypeRefMarker
	},
	{
		"union",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName |
			CXXKeywordIsTypeRefMarker
	},
	{
		"unsigned",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"using",
		CXXLanguageCPP,
		0
	},
	{
		"virtual",
		CXXLanguageCPP,
		CXXKeywordExcludeFromTypeNames
	},
	{
		"void",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"volatile",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		CXXKeywordMayAppearInVariableDeclaration
	},
	{
		"wchar_t",
		CXXLanguageCPP,
		CXXKeywordMayAppearInVariableDeclaration | CXXKeywordFlagMayBePartOfTypeName
	},
	{
		"while",
		CXXLanguageC | CXXLanguageCPP | CXXLanguageCUDA,
		0
	},
	//{ 0, "xor", 0 },
	//{ 0, 1, "xor_eq", 0 }
};

const char * cxxKeywordName(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].szName;
}

bool cxxKeywordMayBePartOfTypeName(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uFlags &
			CXXKeywordFlagMayBePartOfTypeName;
}

bool cxxKeywordMayAppearInVariableDeclaration(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uFlags &
			CXXKeywordMayAppearInVariableDeclaration;
}

bool cxxKeywordIsTypeRefMarker(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uFlags &
			CXXKeywordIsTypeRefMarker;
}

bool cxxKeywordIsConstant(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uFlags &
			CXXKeywordIsConstant;
}

bool cxxKeywordIsCPPSpecific(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uLanguages == CXXLanguageCPP;
}

bool cxxKeywordExcludeFromTypeNames(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uFlags &
			CXXKeywordExcludeFromTypeNames;
}

bool cxxKeywordIsDisabled(CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].uFlags &
			CXXKeywordIsDisabled;
}

bool cxxKeywordEnablePublicProtectedPrivate(bool bEnableIt)
{
	bool bEnabledNow =
			!(g_aCXXKeywordTable[CXXKeywordPUBLIC].uFlags & CXXKeywordIsDisabled);

	if(bEnabledNow == bEnableIt)
		return bEnabledNow;

	if(bEnableIt)
	{
		CXX_DEBUG_PRINT("Enabling public/protected/private keywords");

		g_aCXXKeywordTable[CXXKeywordPUBLIC].uFlags &= ~CXXKeywordIsDisabled;
		g_aCXXKeywordTable[CXXKeywordPROTECTED].uFlags &= ~CXXKeywordIsDisabled;
		g_aCXXKeywordTable[CXXKeywordPRIVATE].uFlags &= ~CXXKeywordIsDisabled;
	} else {
		CXX_DEBUG_PRINT("Disabling public/protected/private keywords");

		g_aCXXKeywordTable[CXXKeywordPUBLIC].uFlags |= CXXKeywordIsDisabled;
		g_aCXXKeywordTable[CXXKeywordPROTECTED].uFlags |= CXXKeywordIsDisabled;
		g_aCXXKeywordTable[CXXKeywordPRIVATE].uFlags |= CXXKeywordIsDisabled;
	}

	return bEnabledNow;
}

void cxxKeywordEnableFinal(bool bEnableIt)
{
	if(bEnableIt)
		g_aCXXKeywordTable[CXXKeywordFINAL].uFlags &= ~CXXKeywordIsDisabled;
	else
		g_aCXXKeywordTable[CXXKeywordFINAL].uFlags |= CXXKeywordIsDisabled;
}


void cxxBuildKeywordHash(const langType eLangType,unsigned int uLanguage)
{
	const size_t count = sizeof(g_aCXXKeywordTable) / sizeof(CXXKeywordDescriptor);

	size_t i;

	for(i = 0;i < count;i++)
	{
		const CXXKeywordDescriptor * p = g_aCXXKeywordTable + i;
		if(p->uLanguages & uLanguage)
			addKeyword(p->szName,eLangType,i);
	}
}
