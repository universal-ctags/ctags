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

/* Usually, a parser runs on an input file.
 * However, when running a guest, the parser runs on an area.
 *
 * input file ---> +----------------------+
 *                 |                      |
 *                 |  [...................|
 *                 |....... area .........|
 *                 |......................|
 *                 |......]               |
 *                 |                      |
 *                 |                      |
 *                 |                      |
 *                 |                      |
 *                 +----------------------+
 *
 * We have many functions for getting locaitons.
 *
 * We have three data types repesenting a locaiton: line number,
 * position, and offset.
 *
 * When dealing with a location, the coordination becomes an issue: a
 * location in an input file or a location in an area.
 *
 * To make the following explanation simple, we generalize the term
 * "area". For a non-guest (host) parser, the area is the same as the input
 * file. So both host and guest parsers can call functions getting a location
 * in a area.
 *
 * input file ---> +----------------------+
 *                 |......................|
 *                 |......................|
 *                 |......................|
 *                 |......................|
 *                 |........ area ........|
 *                 |......................|
 *                 |......................|
 *                 |......................|
 *                 |......................|
 *                 +----------------------+
 *
 * We have had many bugs caused by misunderstanding coordination.
 * So we put marks [input file], [area] or [buggy] for the declarations
 * of functions getting a location.
 *
 * A function marked [buggy] may return [input file] or [area].
 * We should remove [buggy] by improving the code.
 *
 * Just marking the functions is not enough. The mark is for the
 * value returned from the functions. Some functions take location
 * values as arguments. So we have to put marks on such arguments.
 */

/* InputFile: reading from fp in inputFile with updating fields in input fields */

/* [input file] */
extern unsigned long getInputLineNumber (void);

/* [input file]
 * offset: [input file]
 */
extern unsigned long getInputLineNumberForFileOffset(long offset);
extern int getInputLineOffset (void);
extern const char *getInputFileName (void);
extern MIOPos getInputFilePosition (void); /* TODO */
extern MIOPos getInputFilePositionForLine (unsigned int line); /* TODO */
extern MIOPos getInputFilePositionForOffset (long offset);	   /* TODO */
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

extern unsigned long getCurrentAreaStartLine (void);

#endif  /* CTAGS_MAIN_READ_H */
