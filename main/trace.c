/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Tracing facility.
*/

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

void tracePrint(const char * szFunction,const char * szFormat,...)
{
	if (!isTraced())
		return;

	debugIndent();

	fprintf(stderr,"[%s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

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

#endif // DO_TRACING
