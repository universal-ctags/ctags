/*
*   $Id$
*
*   Copyright (c) 1998-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to main.c
*/
#ifndef _MAIN_H
#define _MAIN_H

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <stdio.h>

#include "vstring.h"

/*
*   MACROS
*/
#define xMalloc(n,Type)    (Type *)eMalloc((size_t)(n) * sizeof (Type))
#define xCalloc(n,Type)    (Type *)eCalloc((size_t)(n), sizeof (Type))
#define xRealloc(p,n,Type) (Type *)eRealloc((p), (n) * sizeof (Type))

/*
*   DATA DECLARATIONS
*/
typedef int errorSelection;
enum eErrorTypes { FATAL = 1, WARNING = 2, PERROR = 4 };
extern char *CurrentDirectory;

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_MALLOC
extern void *malloc (size_t);
extern void *realloc (void *ptr, size_t);
#endif

extern void error (const errorSelection selection, const char *const format, ...) __printf__ (2, 3);
extern FILE *tempFile (const char *const mode, char **const pName);
extern char* eStrdup (const char* str);
extern void *eMalloc (const size_t size);
extern void *eCalloc (const size_t count, const size_t size);
extern void *eRealloc (void *const ptr, const size_t size);
extern void eFree (void *const ptr);
extern void toLowerString (char* str);
extern void toUpperString (char* str);
extern char* newLowerString (const char* str);
extern char* newUpperString (const char* str);
extern long unsigned int getFileSize (const char *const name);
extern boolean isExecutable (const char *const name);
extern boolean isSameFile (const char *const name1, const char *const name2);
extern boolean doesFileExist (const char *const fileName);
extern char* absoluteDirname (char *file);
extern char* relativeFilename (const char *file, const char *dir);
extern void addTotals (const unsigned int files, const long unsigned int lines, const long unsigned int bytes);
extern const char *baseFilename (const char *const filePath);
extern boolean isAbsolutePath (const char *const path);
extern vString *combinePathAndFile (const char *const path, const char *const file);
extern boolean isDestinationStdout (void);
extern const char *getExecutableName (void);
extern void processExcludeOption (const char *const option, const char *const parameter);
extern int main (int argc, char **argv);

#ifndef HAVE_STRICMP
extern int stricmp (const char *s1, const char *s2);
#endif
#ifndef HAVE_STRNICMP
extern int strnicmp (const char *s1, const char *s2, size_t n);
#endif
#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr);
#endif

#endif	/* _MAIN_H */

/* vi:set tabstop=8 shiftwidth=4: */
