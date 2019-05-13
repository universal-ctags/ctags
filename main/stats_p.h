/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   Author: Darren Hiebert <dhiebert@users.sourceforge.net>
*           http://ctags.sourceforge.net
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*   It is provided on an as-is basis and no responsibility is accepted for its
*   failure to perform as expected.
*/

#ifndef CTAGS_MAIN_STATS_PRIVATE_H
#define CTAGS_MAIN_STATS_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "options_p.h"

/*
*   FUNCTION PROTOTYPES
*/
extern void addTotals (const unsigned int files, const long unsigned int lines, const long unsigned int bytes);
extern void printTotals (const clock_t *const timeStamps, bool append, sortType sorted);

#endif  /* CTAGS_MAIN_STATS_PRIVATE_H */
