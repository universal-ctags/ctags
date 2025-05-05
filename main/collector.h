/*
*   Copyright (c) 2025, Red Hat, Inc.
*   Copyright (c) 2025, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/
#ifndef CTAGS_MAIN_COLLECTOR_H
#define CTAGS_MAIN_COLLECTOR_H

/*
*   INCLUDE FILES
*/
#include "general.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/

/* vString based string collector.
 *
 * This data type is for building values for signature and typeref fields
 * from tokens.
 */
struct sCollector {
	vString *repr;
	size_t lastLen;
	int xdata;
};
typedef struct sCollector collector;

/*
*   FUNCTION PROTOTYPES
*/
void collectorInit (collector *collector, int xdata);
void collectorFini (collector *collector);
void collectorStamp (collector *collector, int xdata);
char *collectorStrdup (collector *collector);
void collectorPut (collector *collector, char c);
void collectorCat (collector *collector, vString *vstr);
void collectorCatS (collector *collector, const char *str);
void collectorJoin (collector *collector, char c, vString *vstr);
#endif	/* CTAGS_MAIN_COLLECTOR_H */
