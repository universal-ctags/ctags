/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

#ifndef QUALIFIER_H
#define QUALIFIER_H

#include "es-lang-c-stdc99.h"
#include "readtags.h"

#include <stdio.h>

typedef struct sQCode QCode;

enum QRESULT {
	Q_REJECT,
	Q_ACCEPT,
	Q_ERROR,
};

QCode       *q_compile        (EsObject *exp);
enum QRESULT q_is_acceptable  (QCode *code, tagEntry *entry);
void         q_destroy        (QCode *code);
void         q_help           (FILE *fp);

#endif
