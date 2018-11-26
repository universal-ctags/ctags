/*
*   Copyright (c) 2017 Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_COLPRINT_PRIVATE_H
#define CTAGS_MAIN_COLPRINT_PRIVATE_H

#include "general.h"

#include "vstring.h"
#include <stdio.h>

struct colprintTable;
struct colprintLine;

/* Each column must have a prefix for specifying justification: "L:" or "R:". */
struct colprintTable *colprintTableNew (const char* columnHeader, ... /* NULL TERMINATED */);
void colprintTableDelete (struct colprintTable *table);
void colprintTablePrint (struct colprintTable *table, unsigned int startFrom, bool withHeader, bool machinable, FILE *fp);
void colprintTableSort  (struct colprintTable *table, int (* compareFn) (struct colprintLine *, struct colprintLine *));

struct colprintLine *colprintTableGetNewLine (struct colprintTable *table);

void colprintLineAppendColumnCString (struct colprintLine *line, const char* column);
void colprintLineAppendColumnVString (struct colprintLine *line, vString* column);
void colprintLineAppendColumnChar (struct colprintLine *line, char column);
void colprintLineAppendColumnInt  (struct colprintLine *line, unsigned int column);

/* Appends "yes" or "no". */
void colprintLineAppendColumnBool (struct colprintLine *line, bool column);

const char *colprintLineGetColumn (struct colprintLine *line, unsigned int column);

#endif /* CTAGS_MAIN_COLPRINT_PRIVATE_H */
