#ifndef ctags_cxx_parser_h_
#define ctags_cxx_parser_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "parse.h"

// public parser api
rescanReason cxxCParserMain(const unsigned int passCount);
rescanReason cxxCppParserMain(const unsigned int passCount);
rescanReason cxxCUDAParserMain(const unsigned int passCount);

void cxxCParserInitialize(const langType language);
void cxxCppParserInitialize(const langType language);
void cxxCUDAParserInitialize(const langType language);

void cxxParserCleanup(langType language, bool initialized);

#endif //!ctags_cxx_parser_h_