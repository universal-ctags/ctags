/*
*   Copyright (c) 2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Main part private interface to routines.c
*/
#ifndef CTAGS_MAIN_ROUTINES_PRIVATE_H
#define CTAGS_MAIN_ROUTINES_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "mio.h"
#include "portable-dirent_p.h"

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

#if defined (MSDOS_STYLE_PATH)
# define OUTPUT_PATH_SEPARATOR	'/'
#else
# define OUTPUT_PATH_SEPARATOR	PATH_SEPARATOR
#endif

/*
*   DATA DECLARATIONS
*/
extern char *CurrentDirectory;
#if defined (MSDOS_STYLE_PATH)
extern const char *const PathDelimiters;
#endif

typedef struct {
		/* Name of file for which status is valid */
	char* name;

		/* Does file exist? If not, members below do not contain valid data. */
	bool exists;

		/* is file path a symbolic link to another file? */
	bool isSymbolicLink;

		/* Is file (pointed to) a directory? */
	bool isDirectory;

		/* Is file (pointed to) a normal file? */
	bool isNormalFile;

		/* Is file (pointed to) executable? */
	bool isExecutable;

		/* Is file (pointed to) setuid? */
	bool isSetuid;

		/* Is file (pointed to) setgid? */
	bool isSetgid;

		/* Size of file (pointed to) */
	unsigned long size;

		/* The last modified time */
	time_t mtime;
} fileStatus;

/*
*   FUNCTION PROTOTYPES
*/
extern void freeRoutineResources (void);
extern void setExecutableName (const char *const path);

/* File system functions */
extern const char *getExecutableName (void);
extern const char *getExecutablePath (void);
extern void setCurrentDirectory (void);
extern fileStatus *eStat (const char *const fileName);
extern void eStatFree (fileStatus *status);
extern bool doesFileExist (const char *const fileName);
extern bool doesExecutableExist (const char *const fileName);
extern bool isRecursiveLink (const char* const dirName);
extern bool isSameFile (const char *const name1, const char *const name2);
extern bool isAbsolutePath (const char *const path);
extern char *combinePathAndFile (const char *const path, const char *const file);
extern char* absoluteFilename (const char *file);
extern char* absoluteDirname (char *file);
extern char* relativeFilename (const char *file, const char *dir);
extern MIO *tempFile (const char *const mode, char **const pName);

extern char* baseFilenameSansExtensionNew (const char *const fileName, const char *const templateExt);

#endif  /* CTAGS_MAIN_ROUTINES_PRIVATE_H */
