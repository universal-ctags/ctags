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
} CXXKeywordDescriptor;

// This array is indexed by the CXXKeywordType enum
static const CXXKeywordDescriptor g_aCXXKeywordTable[] = {
	{ 1, "__attribute__" },
	{ 1, "__declspec" },
	{ 0, "__fastcall" },
	{ 1, "__stdcall" },
	{ 0, "__thiscall" },
	{ 0, "alignas" },
	{ 0, "alignof" },
	//{ 1, "and" },
	//{ 1, "and_eq" },
	{ 1, "asm" },
	{ 0, "auto" },
	//{ 1, "bitand" },
	//{ 1, "bitor" },
	{ 1, "bool" },
	{ 1, "break" },
	{ 1, "case" },
	{ 0, "catch" },
	{ 1, "char" },
	{ 0, "char16_t" },
	{ 0, "char32_t" },
	{ 0, "class" },
	//{ 0, "compl" },
	{ 0, "concept" },
	{ 1, "const" },
	{ 0, "constexpr" },
	{ 0, "const_cast" },
	{ 1, "continue" },
	{ 0, "decltype" },
	{ 1, "default" },
	{ 0, "delete" },
	{ 1, "do" },
	{ 1, "double" },
	{ 0, "dynamic_cast" },
	{ 1, "else" },
	{ 1, "enum" },
	{ 0, "explicit" },
	{ 0, "export" },
	{ 1, "extern" },
	{ 1, "false" },
	{ 0, "final" }, // this is a keyword only in special contexts (we have a switch to enable/disable it)
	{ 1, "float" },
	{ 1, "for" },
	{ 0, "friend" },
	{ 1, "goto" },
	{ 1, "if" },
	{ 1, "inline" },
	{ 1, "int" },
	{ 1, "long" },
	{ 1, "mutable" },
	{ 0, "namespace" },
	{ 0, "new" },
	{ 0, "noexcept" },
	//{ 0, "not" },
	//{ 0, "not_eq" },
	{ 0, "nullptr" },
	{ 0, "operator" },
	//{ 0, "or" },
	//{ 0, "or_eq" },
	//{ 0, "override" }, <-- override is a keyword only after function declarators, it's easier handling it as identifier
	{ 0, "private" },
	{ 0, "protected" },
	{ 0, "public" },
	{ 1, "register" },
	{ 0, "reinterpret_cast" },
	{ 0, "requires" },
	{ 1, "return" },
	{ 1, "short" },
	{ 1, "signed" },
	{ 1, "sizeof" },
	{ 1, "static" },
	{ 0, "static_assert" },
	{ 0, "static_cast" },
	{ 1, "struct" },
	{ 1, "switch" },
	{ 0, "template" },
	{ 0, "this" },
	{ 0, "thread_local" },
	{ 0, "throw" },
	{ 1, "true" },
	{ 0, "try" },
	{ 1, "typedef" },
	{ 0, "typeid" },
	{ 0, "typename" },
	{ 1, "union" },
	{ 1, "unsigned" },
	{ 0, "using" },
	{ 0, "virtual" },
	{ 1, "void" },
	{ 1, "volatile" },
	{ 0, "wchar_t" },
	{ 1, "while" },
	//{ 0, "xor" },
	//{ 0, 1, "xor_eq" }
};

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
