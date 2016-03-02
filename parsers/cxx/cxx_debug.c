/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "cxx_debug.h"

#ifdef CXX_DO_DEBUGGING

#include "read.h"

#include <stdio.h>
#include <stdarg.h>

static int g_iCXXDebugScopeLevel = 0;

void cxxDebugInit(void)
{
	g_iCXXDebugScopeLevel = 0;
}

void cxxDebugEnter(const char * szFunction,const char * szFormat,...)
{
	for(int i=0;i<g_iCXXDebugScopeLevel;i++)
		fprintf(stderr,"    ");

	fprintf(stderr,"[>> %s][at %lu] ",szFunction,getInputLineNumber());
	
	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");

	g_iCXXDebugScopeLevel++;
}

void cxxDebugLeave(const char * szFunction,const char * szFormat,...)
{
	g_iCXXDebugScopeLevel--;
	if(g_iCXXDebugScopeLevel < 0)
		g_iCXXDebugScopeLevel = 0;

	for(int i=0;i<g_iCXXDebugScopeLevel;i++)
		fprintf(stderr,"    ");
	fprintf(stderr,"[<< %s][at %lu] ",szFunction,getInputLineNumber());

	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");
}

void cxxDebugPrint(const char * szFunction,const char * szFormat,...)
{
	for(int i=0;i<g_iCXXDebugScopeLevel;i++)
		fprintf(stderr,"    ");

	fprintf(stderr,"[%s][at %lu] ",szFunction,getInputLineNumber());
	
	va_list va;
	va_start(va,szFormat);
	vfprintf(stderr,szFormat,va);
	va_end(va);

	fprintf(stderr,"\n");
}

#endif // CXX_DO_DEBUGGING