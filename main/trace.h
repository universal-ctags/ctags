#ifndef ctags_trace_h_
#define ctags_trace_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Tracing facility.
*/

#include "general.h"
#include "debug.h"

//
// Master tracing switch.
//
// Uncomment this to enable extensive debugging to stderr in code.
// Use only for development as tracing reduces performance.
//
// "./configure --enable-debugging" defines DEBUG.
//
#ifdef DEBUG
//#define TRACING_ENABLED 1
#endif

//
// Currently this kind of debugging is supported only on gcc (because of
// variadic macros and __PRETTY_FUNC__).
//
#if defined (__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 4))
	#ifdef TRACING_ENABLED
		#define DO_TRACING
	#endif
#else
	#ifdef TRACING_ENABLED
		#error "Tracing is not supported on this compiler (yet)"
	#endif
#endif


#ifdef DO_TRACING

	void traceEnter(const char * szFunction,const char * szFormat,...);
	void traceLeave(const char * szFunction,const char * szFormat,...);
	void tracePrint(const char * szFunction,const char * szFormat,...);

	#define TRACE_ENTER() traceEnter(__PRETTY_FUNCTION__,"")
	#define TRACE_LEAVE() traceLeave(__PRETTY_FUNCTION__,"")

	#define TRACE_ENTER_TEXT(_szFormat,...) \
		traceEnter(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)

	#define TRACE_LEAVE_TEXT(_szFormat,...) \
		traceLeave(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)

	#define TRACE_PRINT(_szFormat,...) \
		tracePrint(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__)

	#define TRACE_ASSERT(_condition,_szFormat,...) \
		do { \
			if(!(_condition)) \
			{ \
				tracePrint(__PRETTY_FUNCTION__,_szFormat,## __VA_ARGS__); \
				Assert(false); \
			} \
		} while(0)

#else //!DO_TRACING

	#define TRACE_ENTER() do { } while(0)
	#define TRACE_LEAVE() do { } while(0)

	#define TRACE_ENTER_TEXT(_szFormat,...) do { } while(0)
	#define TRACE_LEAVE_TEXT(_szFormat,...) do { } while(0)

	#define TRACE_PRINT(_szFormat,...) do { } while(0)

	#define TRACE_ASSERT(_condition,_szFormat,...) do { } while(0)

#endif //!DO_TRACING


#endif //!ctags_trace_h_
