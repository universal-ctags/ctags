/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Tracing facility.
*/

#include "general.h"
#include "trace.h"

#ifdef DO_TRACING

#include "options.h"
#include "read.h"

#include <stdio.h>
#include <stdarg.h>

void traceEnter(const char * szFunction,const char * szFormat,...)
{
	if (!isTraced())
		return;

	debugIndent ();

	fprintf(stderr,"[>> %s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");

	debugInc();
}

void traceLeave(const char * szFunction,const char * szFormat,...)
{
	if (!isTraced())
		return;

	debugDec();
	debugIndent ();

	fprintf(stderr,"[<< %s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");
}

static void tracePrintFmtVa(const char * szFormat, va_list va)
{
	if (!isTraced())
		return;

	vfprintf(stderr,szFormat,va);
}

void tracePrint(const char * szFunction, const char * szFormat,...)
{
	if (!isTraced())
		return;

	tracePrintPrefix(szFunction);

	va_list va;
	va_start(va,szFormat);
	tracePrintFmtVa (szFormat,va);
	va_end(va);

	tracePrintNewline();
}

void tracePrintFmt(const char * szFormat,...)
{
	va_list va;
	va_start(va,szFormat);
	tracePrintFmtVa (szFormat,va);
	va_end(va);
}

void tracePrintPrefix(const char * szFunction)
{
	if (!isTraced())
		return;

	debugIndent();

	fprintf(stderr,"[%s][at %lu] ",szFunction,getInputLineNumber());
}

void tracePrintNewline(void)
{
	if (!isTraced())
		return;

	fprintf(stderr,"\n");
}

static bool tracingMain;

void traceMain(void)
{
	verbose("Tracing main part\n");
	tracingMain = true;
}

bool isMainTraced(void)
{
	return tracingMain;
}

#else
bool isTraced (void) { return false; }
void traceLanguage (langType language) {}
bool isLanguageTraced (langType language) { return false; }

void traceEnter(const char * szFunction,const char * szFormat,...) {}
void traceLeave(const char * szFunction,const char * szFormat,...) {}
void tracePrint(const char * szFunction,const char * szFormat,...) {}

void tracePrintPrefix(const char * szFunction) {}
void tracePrintFmt(const char * szFormat,...) {}
void tracePrintNewline(void) {}

void traceMain(void);
bool isMainTraced(void) { return false; }

#endif // DO_TRACING
