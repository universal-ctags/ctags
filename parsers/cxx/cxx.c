/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "cxx_debug.h"
#include "cxx_keyword.h"
#include "cxx_token.h"
#include "cxx_token_chain.h"
#include "cxx_parser.h"
#include "cxx_scope.h"
#include "cxx_tag.h"

#include "dependency.h"
#include "selectors.h"

//
// ----------------------------------------------------------------------------
// Assumptions.
// ----------------------------------------------------------------------------
//
// - Parsing C/C++ is hard. Parsing C/C++ correctly without includes and
//   without a complete preprocessor is close to impossible in the general
//   case. Also ctags is not a compiler. This means that our parser must be
//   a "guessing" parser. It's hopeless to try to decode the syntax of the
//   language down to the last bit.
//
// - The input may contain syntax errors. This is because we don't have a full
//   preprocessor and also because ctags is often used "online" in editors,
//   while the user is typing. ctags should be tolerant and try to do its best
//   even with syntax errors but:
//   - Syntax errors that break the scope hierarchy should be detected and tag
//     emission should probably be stopped. Correct tags in a broken hierarchy
//     are useless (well, unless the hierarchy itself is ignored by the ctags
//     user).
//   - CTags should try to avoid emitting tags which involve syntax errors
//
// - There will always be pathologic cases. Don't cry, live with it.
//
// ----------------------------------------------------------------------------
// TODO LIST
// ----------------------------------------------------------------------------
//
// - In case of simple syntax error try to recover:
//       Skip to the next ; without entering or exiting scopes.
//       If this can be done then recovery is feasible.
// - Extension of each block/scope.
// - Unnamed blocks/scopes?
// - Handle syntax errors:
//   - If a special switch is used then stop on detecting a syntax error
//     (this is useful for code editors that frequently update tags for
//     single files)
//   - If the switch is not used then do NOT emit tags for a file on a syntax
//     error [but do not stop execution of the whole program and continue on
//     other files]
//   For this purpose:
//   - Do not emit tags until the end of the file, if scopes do not match we
//     either screwed up something or the programmer did
//     Maybe the cork api can be used for this?
//
// Handle variable declarations inside things like while() foreach() FOR() etc..
//
// - Friend classes.
// - Template parameters as field
// - Template specialisations (another field?)
// - Forward declarations might become tags


parserDefinition * CParser (void)
{
	static const char * const extensions [] =
	{
		"c",
		NULL
	};

	static selectLanguage selectors[] = { selectByObjectiveCKeywords, NULL };

	parserDefinition* def = parserNew("C");

	def->kindTable = cxxTagGetCKindDefinitions();
	def->kindCount = cxxTagGetCKindDefinitionCount();
	def->fieldTable = cxxTagGetCFieldDefinitionifiers();
	def->fieldCount = cxxTagGetCFieldDefinitionifierCount();
	def->extensions = extensions;
	def->parser2 = cxxCParserMain;
	def->initialize = cxxCParserInitialize;
	def->finalize = cxxParserCleanup;
	def->selectLanguage = selectors;
	def->useCork = CORK_QUEUE|CORK_SYMTAB; // We use corking to block output until the end of file

	return def;
}

parserDefinition * CppParser (void)
{
	static const char * const extensions [] =
	{
		"c++", "cc", "cp", "cpp", "cxx",
		"h", "h++", "hh", "hp", "hpp", "hxx", "inl",
#ifndef CASE_INSENSITIVE_FILENAMES
		"C", "H", "CPP", "CXX",
#endif
		NULL
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_KIND_OWNER, "C" },
	};

	static selectLanguage selectors[] = { selectByObjectiveCKeywords, NULL };

	parserDefinition* def = parserNew("C++");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	def->kindTable = cxxTagGetCPPKindDefinitions();
	def->kindCount = cxxTagGetCPPKindDefinitionCount();
	def->fieldTable = cxxTagGetCPPFieldDefinitionifiers();
	def->fieldCount = cxxTagGetCPPFieldDefinitionifierCount();
	def->extensions = extensions;
	def->parser2 = cxxCppParserMain;
	def->initialize = cxxCppParserInitialize;
	def->finalize = cxxParserCleanup;
	def->selectLanguage = selectors;
	def->useCork = CORK_QUEUE|CORK_SYMTAB; // We use corking to block output until the end of file

	return def;
}

parserDefinition * CUDAParser (void)
{
	static const char * const extensions [] =
	{
		"cu", "cuh",
		NULL
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_KIND_OWNER, "C" },
	};

	parserDefinition* def = parserNew("CUDA");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	def->kindTable = cxxTagGetCUDAKindDefinitions();
	def->kindCount = cxxTagGetCUDAKindDefinitionCount();
	def->fieldTable = cxxTagGetCUDAFieldDefinitionifiers();
	def->fieldCount = cxxTagGetCUDAFieldDefinitionifierCount();
	def->extensions = extensions;
	def->parser2 = cxxCUDAParserMain;
	def->initialize = cxxCUDAParserInitialize;
	def->finalize = cxxParserCleanup;
	def->selectLanguage = NULL;
	def->useCork = CORK_QUEUE|CORK_SYMTAB; // We use corking to block output until the end of file

	return def;
}
