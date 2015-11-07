/*
 *
 *  Copyright (c) 2015, Red Hat, Inc.
 *  Copyright (c) 2015, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#ifndef FMT_H
#define FMT_H

#include "general.h"
#include "entry.h"
#include <stdio.h>

typedef struct sFmtElement fmtElement;
extern fmtElement *fmtNew     (const char* fmtString);
extern int         fmtPrint   (fmtElement * fmtelts, FILE* fp, const tagEntryInfo *tag);
extern void        fmtDelete  (fmtElement * fmtelts);

#endif	/* FMT_H */
