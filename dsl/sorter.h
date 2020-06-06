/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef SORTER_H
#define SORTER_H

/*
 * Includes
 */

#include "es.h"
#include "readtags.h"

#include <stdio.h>


/*
 * Type declarations
 */

typedef struct sSCode SCode;


/*
 * function declarations
 */

SCode       *s_compile        (EsObject *exp);
int          s_compare        (const tagEntry * a, const tagEntry * b, SCode *code);
void         s_destroy        (SCode *code);
void         s_help           (FILE *fp);

#endif
