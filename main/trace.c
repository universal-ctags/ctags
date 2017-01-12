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

#include "read.h"

#include <stdio.h>
#include <stdarg.h>

static int g_iTraceScopeLevel = 0;

void traceInit(void)
{
	g_iTraceScopeLevel = 0;
}

void traceEnter(const char * szFunction,const char * szFormat,...)
{
	for(int i=0;i<g_iTraceScopeLevel;i++)
		fprintf(stderr,"    ");

	fprintf(stderr,"[>> %s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");

	g_iTraceScopeLevel++;
}

void traceLeave(const char * szFunction,const char * szFormat,...)
{
	g_iTraceScopeLevel--;
	if(g_iTraceScopeLevel < 0)
		g_iTraceScopeLevel = 0;

	for(int i=0;i<g_iTraceScopeLevel;i++)
		fprintf(stderr,"    ");
	fprintf(stderr,"[<< %s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");
}

void tracePrint(const char * szFunction,const char * szFormat,...)
{
	for(int i=0;i<g_iTraceScopeLevel;i++)
		fprintf(stderr,"    ");

	fprintf(stderr,"[%s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");
}

#endif // DO_TRACING


