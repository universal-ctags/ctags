/*
*   Copyright (c) 2025, Red Hat, Inc.
*   Copyright (c) 2025, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"

#include "collector.h"
#include "routines.h"

/*
*   FUNCTION DEFINITIONS
*/

void collectorInit (collector *collector, int xdata)
{
	collector->repr = vStringNew ();
	collector->lastLen = 0;
	collector->xdata = xdata;
}

void collectorFini (collector *collector)
{
	vStringDelete (collector->repr);
}

void collectorStamp (collector *collector, int xdata)
{
	collector->lastLen = vStringLength (collector->repr);
	collector->xdata = xdata;
}

char *collectorStrdup (collector *collector)
{

	return eStrdup (vStringValue (collector->repr));
}

void collectorPut (collector *collector, char c)
{
	vStringPut (collector->repr, c);
}

void collectorCat (collector *collector, vString *vstr)
{
	vStringCat (collector->repr, vstr);
}

void collectorCatS (collector *collector, const char *str)
{
	vStringCatS (collector->repr, str);
}

void collectorJoin (collector *collector, char c, vString *vstr)
{
	vStringJoin (collector->repr, c, vstr);
}
