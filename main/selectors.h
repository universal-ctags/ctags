/*
 * Copyright (c) 2015, Dmitri Tikhonov
 *
 * This source code is released for free distribution under the terms of the
 * GNU General Public License version 2 or (at your option) any later version.
 *
 * selectors.h
 */

#ifndef CTAGS_MAIN_SELECTORS_H
#define CTAGS_MAIN_SELECTORS_H

#include <stdio.h>

const char *
selectByPickingPerlVersion (FILE *);

const char *
selectByObjectiveCAndMatLabKeywords (FILE *);

const char *
selectByObjectiveCKeywords(FILE *);

#endif
