/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2.
*
*/
#ifndef _KIND_H
#define _KIND_H

/*
 * Predefined kinds
 */
#define KIND_REGEX_DEFAULT 'r'
#define KIND_REGEX_DEFAULT_LONG "regex"
/* We treat ' ' as a ghost kind.
   It will never be listed up in --list-kinds. */

#define KIND_NULL    '\0'

#define KIND_GHOST   ' '
#define KIND_GHOST_LONG "ghost"

#define KIND_FILE_DEFAULT 'F'
#define KIND_FILE_DEFAULT_LONG "file"

#define KIND_FILE_ALT '!'

typedef struct sKindOption {
	boolean enabled;          /* are tags for kind enabled? */
	char  letter;               /* kind letter */
	char* name;		  /* kind name */
	char* description;	  /* displayed in --help output */
} kindOption;

extern void printKind (const kindOption* const kind, boolean allKindFields, boolean indent);
#endif	/* _KIND_H */
