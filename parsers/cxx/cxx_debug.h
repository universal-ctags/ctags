#ifndef ctags_cxx_debug_h_
#define ctags_cxx_debug_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"
#include "debug.h"
#include "trace.h"
#include "cxx_token.h"
#include "cxx_scope.h"

#if defined(DO_TRACING)
	#define CXX_DO_DEBUGGING
#endif

#ifdef CXX_DO_DEBUGGING

const char* cxxDebugTypeDecode(enum CXXTokenType);
void cxxDebugDumpToken (CXXToken *pToken);
void cxxDebugDumpChain (CXXTokenChain *pChain);
const char* cxxDebugScopeDecode(enum CXXScopeType);

#define CXX_DEBUG_ENTER() TRACE_ENTER()
#define CXX_DEBUG_LEAVE() TRACE_LEAVE()

#define CXX_DEBUG_ENTER_TEXT(_szFormat,...) \
	TRACE_ENTER_TEXT(_szFormat,## __VA_ARGS__)

#define CXX_DEBUG_LEAVE_TEXT(_szFormat,...) \
	TRACE_LEAVE_TEXT(_szFormat,## __VA_ARGS__)

#define CXX_DEBUG_PRINT(_szFormat,...) \
	TRACE_PRINT(_szFormat,## __VA_ARGS__)

#define CXX_DEBUG_ASSERT(_condition,_szFormat,...) \
	TRACE_ASSERT(_condition,_szFormat,## __VA_ARGS__)

#define CXX_DEBUG_TOKEN(T)  cxxDebugDumpToken(T)
#define CXX_DEBUG_CHAIN(C)  cxxDebugDumpChain(C)
#else //!CXX_DO_DEBUGGING

#define CXX_DEBUG_ENTER() do { } while(0)
#define CXX_DEBUG_LEAVE() do { } while(0)

#define CXX_DEBUG_ENTER_TEXT(_szFormat,...) do { } while(0)
#define CXX_DEBUG_LEAVE_TEXT(_szFormat,...) do { } while(0)

#define CXX_DEBUG_PRINT(_szFormat,...) do { } while(0)

#define CXX_DEBUG_ASSERT(_condition,_szFormat,...) do { } while(0)

#define CXX_DEBUG_TOKEN(T) do { } while(0)
#define CXX_DEBUG_CHAIN(T) do { } while(0)
#endif //!CXX_DO_DEBUGGING


#endif //!ctags_cxx_debug_h_
