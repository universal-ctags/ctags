/*
 *
 *  Copyright (c) 2014, Red Hat, Inc.
 *  Copyright (c) 2014, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *  This source code is released for free distribution under the terms of the
 *  GNU General Public License version 2 or (at your option) any later version.
 *  It is provided on an as-is basis and no responsibility is accepted for its
 *  failure to perform as expected.
 *
 */
#ifndef _TG_H
#define _TG_H

#include <stdio.h>


unsigned char*  tgCreate (void);
void            tgDestroy(unsigned char *mini_table);
void            tgLoad   (unsigned char *mini_table, FILE *fp);

/* if t is similar than a, return negative value.
   if t is similar than b, return positive value. */
int             tgCompare(const unsigned char *a, const unsigned char *b, const unsigned char *t);

#endif	/* _TG_H */

/* vi:set tabstop=4 shiftwidth=4: */
