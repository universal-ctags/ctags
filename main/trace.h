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
// When running ctags, pass --_trace=<LANG>[,<LANG>]* option.
//
#ifdef DEBUG
#define DO_TRACING
#endif

bool isTraced (void);
void traceLanguage (langType language);
bool isLanguageTraced (langType language);

void traceEnter(const char * szFunction,const char * szFormat,...);
void traceLeave(const char * szFunction,const char * szFormat,...);
void tracePrint(const char * szFunction,const char * szFormat,...);

void tracePrintPrefix(const char * szFunction);
void tracePrintFmt(const char * szFormat,...);
void tracePrintNewline(void);

void traceMain(void);
bool isMainTraced(void);

#ifdef DO_TRACING

	#define TRACE_ENTER() traceEnter(__func__,"")
	#define TRACE_LEAVE() traceLeave(__func__,"")

	#define TRACE_ENTER_TEXT(_szFormat,...) \
		traceEnter(__func__,_szFormat,## __VA_ARGS__)

	#define TRACE_LEAVE_TEXT(_szFormat,...) \
		traceLeave(__func__,_szFormat,## __VA_ARGS__)

	#define TRACE_PRINT(_szFormat,...) \
		tracePrint(__func__,_szFormat,## __VA_ARGS__)

	/* TRACE_PRINT prints line at once.
	 * If you want to print a trace line incrementally,
	 * use following macros.
	 *
	 * TRACE_PRINT_PREFIX: just print a trace prefix. No newline.
	 * TRACE_PRINT_FMT: print as a format. No prefix, no newline.
	 * TRACE_PRINT_NEWLINE: just print a newline.
	 */
	#define TRACE_PRINT_PREFIX() \
		tracePrintPrefix(__func__)
	#define TRACE_PRINT_FMT(_szFormat,...) \
		tracePrintFmt(_szFormat,## __VA_ARGS__)
	#define TRACE_PRINT_NEWLINE() \
		tracePrintNewline()

	#define TRACE_ASSERT(_condition,_szFormat,...) \
		do { \
			if(!(_condition)) \
			{ \
				tracePrint(__func__,_szFormat,## __VA_ARGS__); \
				Assert(false); \
			} \
		} while(0)

#else //!DO_TRACING

	#define TRACE_ENTER() do { } while(0)
	#define TRACE_LEAVE() do { } while(0)

	#define TRACE_ENTER_TEXT(_szFormat,...) do { } while(0)
	#define TRACE_LEAVE_TEXT(_szFormat,...) do { } while(0)

	#define TRACE_PRINT(_szFormat,...) do { } while(0)

	#define TRACE_PRINT_PREFIX() do { } while(0)
	#define TRACE_PRINT_FMT(_szFormat,...) do { } while(0)
	#define TRACE_PRINT_NEWLINE() do { } while(0)

	#define TRACE_ASSERT(_condition,_szFormat,...) do { } while(0)

#endif //!DO_TRACING


#endif //!ctags_trace_h_
