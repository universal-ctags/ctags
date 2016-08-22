/*
*   Copyright (c) 2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to routines.c
*/
#ifndef CTAGS_MAIN_ROUTINES_H
#define CTAGS_MAIN_ROUTINES_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>

#include "mio.h"

/*
*   MACROS
*/
#define xMalloc(n,Type)    (Type *)eMalloc((size_t)(n) * sizeof (Type))
#define xCalloc(n,Type)    (Type *)eCalloc((size_t)(n), sizeof (Type))
#define xRealloc(p,n,Type) (Type *)eRealloc((p), (n) * sizeof (Type))

#define ARRAY_SIZE(X)      (sizeof (X) / sizeof (X[0]))
#define ARRAY_AND_SIZE(X)  (X), ARRAY_SIZE(X)

#define STRINGIFY(X) STRINGIFY_(X)
#define STRINGIFY_(X) #X

/*
 *  Portability macros
 */
#ifndef PATH_SEPARATOR
# if defined (MSDOS_STYLE_PATH)
#  define PATH_SEPARATOR '\\'
# else
#  define PATH_SEPARATOR '/'
# endif
#endif

#if defined (MSDOS_STYLE_PATH) && defined (UNIX_PATH_SEPARATOR)
# define OUTPUT_PATH_SEPARATOR	'/'
#else
# define OUTPUT_PATH_SEPARATOR	PATH_SEPARATOR
#endif

/*
*   DATA DECLARATIONS
*/
#if defined (MSDOS_STYLE_PATH)
extern const char *const PathDelimiters;
#endif
extern char *CurrentDirectory;
typedef int errorSelection;
enum eErrorTypes { FATAL = 1, WARNING = 2, PERROR = 4 };

typedef struct {
		/* Name of file for which status is valid */
	char* name;

		/* Does file exist? If not, members below do not contain valid data. */
	boolean exists;

		/* is file path a symbolic link to another file? */
	boolean isSymbolicLink;

		/* Is file (pointed to) a directory? */
	boolean isDirectory;

		/* Is file (pointed to) a normal file? */
	boolean isNormalFile;

		/* Is file (pointed to) executable? */
	boolean isExecutable;

		/* Is file (pointed to) setuid? */
	boolean isSetuid;

		/* Is file (pointed to) setgid? */
	boolean isSetgid;

		/* Size of file (pointed to) */
	unsigned long size;
} fileStatus; 

/*
*   FUNCTION PROTOTYPES
*/
extern void freeRoutineResources (void);
extern void setExecutableName (const char *const path);
extern const char *getExecutableName (void);
extern const char *getExecutablePath (void);
extern void error (const errorSelection selection, const char *const format, ...) CTAGS_ATTR_PRINTF (2, 3);

/* Memory allocation functions */
#ifdef NEED_PROTO_MALLOC
extern void *malloc (size_t);
extern void *realloc (void *ptr, size_t);
#endif
extern void *eMalloc (const size_t size);
extern void *eCalloc (const size_t count, const size_t size);
extern void *eRealloc (void *const ptr, const size_t size);
extern void eFree (void *const ptr);

/* String manipulation functions */
extern int struppercmp (const char *s1, const char *s2);
extern int strnuppercmp (const char *s1, const char *s2, size_t n);
#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr);
#endif
extern char* strrstr (const char *str, const char *substr);
extern char* eStrdup (const char* str);
extern char* eStrndup (const char* str, size_t len);
extern void toLowerString (char* str);
extern void toUpperString (char* str);
extern char* newLowerString (const char* str);
extern char* newUpperString (const char* str);
extern boolean strToUInt(const char *const str, int base, unsigned int *value);
extern boolean strToULong(const char *string, int base, unsigned long *value);
extern boolean strToInt(const char *const str, int base, int *value);
extern boolean strToLong(const char *string, int base, long *value);

/* File system functions */
extern void setCurrentDirectory (void);
extern fileStatus *eStat (const char *const fileName);
extern void eStatFree (fileStatus *status);
extern boolean doesFileExist (const char *const fileName);
extern boolean doesExecutableExist (const char *const fileName);
extern boolean isRecursiveLink (const char* const dirName);
extern boolean isSameFile (const char *const name1, const char *const name2);
extern const char *baseFilename (const char *const filePath);
extern const char *fileExtension (const char *const fileName);
extern boolean isAbsolutePath (const char *const path);
extern char *combinePathAndFile (const char *const path, const char *const file);
extern char* absoluteFilename (const char *file);
extern char* absoluteDirname (char *file);
extern char* relativeFilename (const char *file, const char *dir);
extern MIO *tempFile (const char *const mode, char **const pName);

extern char* baseFilenameSansExtensionNew (const char *const fileName, const char *const templateExt);

#endif  /* CTAGS_MAIN_ROUTINES_H */
