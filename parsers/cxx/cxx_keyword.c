/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_keyword.h"

#include "keyword.h"

typedef struct _CXXKeywordDescriptor
{
	unsigned char bValidC;
	const char * szName;
	char bMayBePartOfTypeName;
} CXXKeywordDescriptor;

// This array is indexed by the CXXKeywordType enum
static const CXXKeywordDescriptor g_aCXXKeywordTable[] = {
	{ 1, "__attribute__", 0 },
	{ 1, "__declspec", 0 },
	{ 0, "__fastcall", 0 },
	{ 1, "__stdcall", 0 },
	{ 0, "__thiscall", 0 },
	{ 0, "alignas", 0 },
	{ 0, "alignof", 0 },
	//{ 1, "and", 0 },
	//{ 1, "and_eq", 0 },
	{ 1, "asm", 0 },
	{ 0, "auto", 1 },
	//{ 1, "bitand", 0 },
	//{ 1, "bitor", 0 },
	{ 1, "bool", 1 },
	{ 1, "break", 0 },
	{ 1, "case", 0 },
	{ 0, "catch", 0 },
	{ 1, "char", 1 },
	{ 0, "char16_t", 1 },
	{ 0, "char32_t", 1 },
	{ 0, "class", 1 },
	//{ 0, "compl", 0 },
	{ 0, "concept", 0 },
	{ 1, "const", 1 },
	{ 0, "constexpr", 0 },
	{ 0, "const_cast", 0 },
	{ 1, "continue", 0 },
	{ 0, "decltype", 0 },
	{ 1, "default", 0 },
	{ 0, "delete", 0 },
	{ 1, "do", 0 },
	{ 1, "double", 0 },
	{ 0, "dynamic_cast", 0 },
	{ 1, "else", 0 },
	{ 1, "enum", 1 },
	{ 0, "explicit", 0 },
	{ 0, "export", 0 },
	{ 1, "extern", 0 },
	{ 1, "false", 0 },
	{ 0, "final", 0 }, // this is a keyword only in special contexts (we have a switch to enable/disable it)
	{ 1, "float", 1 },
	{ 1, "for", 0 },
	{ 0, "friend", 0 },
	{ 1, "goto", 0 },
	{ 1, "if", 0 },
	{ 1, "inline", 0 },
	{ 1, "int", 1 },
	{ 1, "long", 1 },
	{ 1, "mutable", 0 },
	{ 0, "namespace", 0 },
	{ 0, "new", 0 },
	{ 0, "noexcept", 0 },
	//{ 0, "not", 0 },
	//{ 0, "not_eq", 0 },
	{ 0, "nullptr", 0 },
	{ 0, "operator", 0 },
	//{ 0, "or", 0 },
	//{ 0, "or_eq", 0 },
	//{ 0, "override", 0 }, <-- override is a keyword only after function declarators, it's easier handling it as identifier
	{ 0, "private", 0 },
	{ 0, "protected", 0 },
	{ 0, "public", 0 },
	{ 1, "register", 0 },
	{ 0, "reinterpret_cast", 0 },
	{ 0, "requires", 0 },
	{ 1, "return", 0 },
	{ 1, "short", 1 },
	{ 1, "signed", 1 },
	{ 1, "sizeof", 0 },
	{ 1, "static", 0 },
	{ 0, "static_assert", 0 },
	{ 0, "static_cast", 0 },
	{ 1, "struct", 1 },
	{ 1, "switch", 0 },
	{ 0, "template", 0 },
	{ 0, "this", 0 },
	{ 0, "thread_local", 0 },
	{ 0, "throw", 0 },
	{ 1, "true", 0 },
	{ 0, "try", 0 },
	{ 1, "typedef", 0 },
	{ 0, "typeid", 0 },
	{ 0, "typename", 1 },
	{ 1, "union", 1 },
	{ 1, "unsigned", 1 },
	{ 0, "using", 0 },
	{ 0, "virtual", 0 },
	{ 1, "void", 1 },
	{ 1, "volatile", 0 },
	{ 0, "wchar_t", 1 },
	{ 1, "while", 0 },
	//{ 0, "xor", 0 },
	//{ 0, 1, "xor_eq", 0 }
};

boolean cxxKeywordMayBePartOfTypeName(enum CXXKeyword eKeywordId)
{
	return g_aCXXKeywordTable[eKeywordId].bMayBePartOfTypeName != 0;
}

void cxxBuildKeywordHash(const langType language,boolean bCXX)
{
	const size_t count = sizeof(g_aCXXKeywordTable) / sizeof(CXXKeywordDescriptor);
	size_t i;

	if(bCXX)
	{
		for(i = 0;i < count;i++)
		{
			const CXXKeywordDescriptor * p = g_aCXXKeywordTable + i;
			addKeyword(p->szName,language,i);
		}
	} else {
		for(i = 0;i < count;i++)
		{
			const CXXKeywordDescriptor * p = g_aCXXKeywordTable + i;
			if(p->bValidC)
				addKeyword(p->szName,language,i);
		}
	}
}
