#ifndef _cxx_debug_h_
#define _cxx_debug_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"


//
// Uncomment this to enable extensive debugging to stderr in cxx code.
// Currently cxx-specific debugging is supported only on gcc (because of variadic macros and __PRETTY_FUNC__)
//
//#define CXX_DEBUGGING_ENABLED 1

#if defined (__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 4))
	#define CXX_DEBUGGING_SUPPORTED
#endif

#if defined(CXX_DEBUGGING_SUPPORTED) && defined(CXX_DEBUGGING_ENABLED)
	#define CXX_DO_DEBUGGING
#endif

#ifdef CXX_DO_DEBUGGING

void cxxDebugEnter(const char * szFunction,const char * szFormat,...);
void cxxDebugLeave(const char * szFunction,const char * szFormat,...);
void cxxDebugPrint(const char * szFunction,const char * szFormat,...);
void cxxDebugInit();

#define CXX_DEBUG_ENTER() cxxDebugEnter(__PRETTY_FUNCTION__,"")
#define CXX_DEBUG_LEAVE() cxxDebugLeave(__PRETTY_FUNCTION__,"")

#define CXX_DEBUG_ENTER_TEXT(_szFormat,...) cxxDebugEnter(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)
#define CXX_DEBUG_LEAVE_TEXT(_szFormat,...) cxxDebugLeave(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)

#define CXX_DEBUG_PRINT(_szFormat,...) cxxDebugPrint(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)

#define CXX_DEBUG_INIT() cxxDebugInit()

#define CXX_DEBUG_ASSERT(_condition,_szFormat,...) if(!(_condition))cxxDebugPrint(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)

#else //!CXX_DO_DEBUGGING

#define CXX_DEBUG_ENTER() do { } while(0)
#define CXX_DEBUG_LEAVE() do { } while(0)

#define CXX_DEBUG_ENTER_TEXT(_szFormat,...) do { } while(0)
#define CXX_DEBUG_LEAVE_TEXT(_szFormat,...) do { } while(0)

#define CXX_DEBUG_PRINT(_szFormat,...) do { } while(0)

#define CXX_DEBUG_INIT() do { } while(0)

#define CXX_DEBUG_ASSERT(_condition,_szFormat,...) do { } while(0)


#endif //!CXX_DO_DEBUGGING


#endif //!_cxx_debug_h_