/*
*   Copyright (c) 2021, Masatake YAMATO
*   Copyright (c) 2021, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef FORMATTER_H
#define FORMATTER_H

/*
 * Includes
 */

#include "es.h"
#include "readtags.h"

#include <stdio.h>

/*
 * Type declarations
 */

typedef struct sFCode FCode;


/*
 * function declarations
 */

FCode       *f_compile        (EsObject *exp);
int          f_print          (const tagEntry * entry, FCode *code, FILE *out);
void         f_destroy        (FCode *code);
void         f_help           (FILE *fp);

#endif
