/*
 *
 *  Copyright (c) 2019, Red Hat, Inc.
 *  Copyright (c) 2019, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   Unwindable input stream
 *
 */
#ifndef CTAGS_MAIN_UNWINDI_H
#define CTAGS_MAIN_UNWINDI_H

/* This header file unwindable input stream.
 * You cannot use this API combining with functions defined
 * in read.h:
 *
 * - getcFromInputFile
 * - ungetcToInputFile
 * - readLineFromInputFile
 * - getInputLineNumber
 * - getInputFilePosition
 * - getInputLineOffset
 *
 * Instead, you can use
 * - uwiGetC
 * - uwiUngetC
 * - uwiGetLineNumber
 * - uwiGetFilePosition
 *
 * You can mark a position in the current input stream with
 * uwiPushMarker().
 * Later, call uwiPopMarker() to unwind the input stream to the
 * position where you marked.
 *
 * uwiPopMarker() takes COUNT as a parameter. It controls unwinding
 * how many bytes. If -1 is passed as COUNT, unwinding to the marked
 * position.
 *
 * If you find that you don't have to unwind though you called
 * uwiPushMarker(), call uwiDropMaker().
 *
 * uwiPopMarker() and uwiDropMaker() release internally allocated resources and
 * clear the marker.
 *
 * If no marker is set, you can use uwiUngetC().
 */

/*
*   INCLUDE FILES
*/
#include "general.h"

/*
*   DATA DECLARATIONS
*/
struct sUwiStats {
	int maxLength;
	bool overflow;
	bool underflow;
};

/*
*   FUNCTION PROTOTYPES
*/
extern void uwiActivate   (unsigned int);
extern void uwiDeactivate (struct sUwiStats *statsToBeUpdated);

extern void uwiStatsInit  (struct sUwiStats *stats);
extern void uwiStatsPrint (struct sUwiStats *stats);

extern int uwiGetC (void);
extern void uwiUngetC (int c);
extern unsigned long uwiGetLineNumber (void);
extern MIOPos uwiGetFilePosition (void);

extern void uwiPushMarker (void);
extern void uwiClearMarker (const int count, const bool revertChars);
extern void uwiPopMarker (const int count, const bool revertChars);
extern void	uwiDropMaker (void);
#endif	/* CTAGS_MAIN_UNWINDI_H */
