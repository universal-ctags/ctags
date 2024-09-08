/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for the public domain.
*
*   This file defines the public interface for looking up tag entries in tag
*   files.
*/
#ifndef PRINTTAGS_H
#define PRINTTAGS_H

#ifdef __cplusplus
extern "C" {
#endif

/*
*   INCLUDE FILES
*/
#include "readtags.h"
#include <stdbool.h>

/*
*  DATA DECLARATIONS
*/

typedef struct {
	int  (* printStr) (const char *, void *);
	int  (* printChar) (int, void *);
} tagPrintProcs;

typedef struct {
	bool extensionFields;
	bool lineNumber;
	bool escaping;
	bool escapingInputField;
} tagPrintOptions;

/*
*  FUNCTION PROTOTYPES
*/

/*
* Print a tag to the file stream.
*/
extern int tagsPrint (const tagEntry *entry,
					  tagPrintOptions *opts, tagPrintProcs *procs, void *outfp);
extern int tagsPrintPseudoTag (const tagEntry *entry,
							   tagPrintOptions *opts, tagPrintProcs *procs, void *outfp);

extern int tagsPrintValue (const char *val, bool escaping, tagPrintProcs *procs, void *outfp);

#ifdef __cplusplus
};
#endif

#endif	/* PRINTTAGS_H */
