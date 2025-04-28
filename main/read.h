/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to read.c
*/
#ifndef CTAGS_MAIN_READ_H
#define CTAGS_MAIN_READ_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <ctype.h>

#include "types.h"
#include "vstring.h"
#include "mio.h"

/*
*   MACROS
*/

/*
*   DATA DECLARATIONS
*/

/*
*   FUNCTION PROTOTYPES
*/

/*
 * We provide parsers some functions to get *locations*.
 *
 * We have three types of locations: line number, position, and
 * offset.
 *
 * Area
 * ----
 * An *area* is a contentious part of an input file.
 * If a parser runs as a *host parser*, the area covers whole the
 * input file.
 *
 * input file ---> +-----------------+
 *                 |<                |
 *                 |                 |
 *                 |      area       |
 *                 |                 |
 *                 |                >|
 *                 +-----------------+
 *
 * When a *host parser* finds a sub area where another programming
 * language is used, the parser can let another parser (*guest
 * parser*) suitable for the language process the sub area.
 *
 * input file ---> +-----------------+
 *                 |<      area      |
 *                 |   [.............|
 *                 |....area (sub)...|
 *                 |....]            |
 *                 |                >|
 *                 +-----------------+
 *
 * Regardless of whether a parser runs as a host or guest, we
 * call the area that the parser processes "the current area".
 * We call the area that the host parser processes "the base area".
 *
 * From the view of a host parser, its current area and its
 * base area are the same.
 * From the vie of a guest parser, its current area and its
 * base area are different.
 *
 * Coordinate system
 * -----------------
 * When dealing with the functions related to locations, we must
 * consider the *coordinate system* of locations.
 *
 * *Absolute coordinate system* is a coordinate system that origin
 * is at the start of the base area.
 *
 * *Current coordinate system* is a coordinate system that origin
 * is at the start of the current area.
 *
 * Let's see an example.
 *
 * Consider calling a function getting a location from a parser
 * running as a guest.
 *
 * input file ---> +-----------------+
 *                 |<   base area    |
 *                 |   [.........X...|
 *                 |..current area...|
 *                 |....]            |
 *                 |                >|
 *                 +-----------------+
 *
 * The line number for X is 2 in the absolute coordinate system.
 * The line number for X is 1 in the current coordinate system.
 */

/* We have had many bugs caused by misunderstanding coordinate systems.
 * So we use markers [absolute], [current] or [buggy] and put them
 * on the declarations of functions getting locations.
 *
 * A function marked [buggy] may return [absolute] or [current]
 * locations. We should have no [buggy] marker in the future.
 *
 * [rename] is a marker for functions that should be renamed.
 */

/* return: [absolute] */
extern unsigned long getInputLineNumber (void);

/* return: [buggy],
 * args (offset): [buggy] */
extern unsigned long getInputLineNumberForFileOffset(long offset);

/* Get the column number, the byte offset of the input from the
 * begging of the line.
 *
 * This function works only if the parser uses getcFromInputFile().
 * This function doesn't work if the parser uses readLineFromInputFile().
 *
 * return: [absolute]
 */
extern int getInputColumnNumber (void);

/* return: [buggy] */
extern MIOPos getInputFilePosition (void);

/* return: [buggy],
 * args (line): [buggy] */
extern MIOPos getInputFilePositionForLine (unsigned int line);

/* return: [absolute]
 * args (line): [current]
 *
 * LINE argument is assumed 1 based.
 */
extern unsigned long translateLineNumber (unsigned long line);

extern const char *getInputFileName (void);
extern langType getInputLanguage (void);
extern bool isInputLanguage (langType lang);
extern bool isInputHeaderFile (void);
extern bool isInputLanguageKindEnabled (int kindIndex);
extern bool isInputLanguageRoleEnabled (int kindIndex, int roleIndex);

extern const unsigned char *getInputFileData (size_t *size);

extern int getcFromInputFile (void);
extern int getNthPrevCFromInputFile (unsigned int nth, int def);
extern int skipToCharacterInInputFile (int c);
extern int skipToCharacterInInputFile2 (int c0, int c1);
extern void ungetcToInputFile (int c);
extern const unsigned char *readLineFromInputFile (void);
extern const unsigned char *readLineFromInputFileWithLength (size_t *length);

extern unsigned long getSourceLineNumber (void);

/* Raw: reading from given a parameter, mio */
extern char *readLineRaw (vString *const vLine, MIO *const mio);

extern void     pushLanguage(const langType language);
extern langType popLanguage (void);

#endif  /* CTAGS_MAIN_READ_H */
