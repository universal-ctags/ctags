/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef CTAGS_PARSER_SH_H
#define CTAGS_PARSER_SH_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"
#include "vstring.h"

typedef struct sShSubparser shSubparser;

struct sShSubparser {
	subparser subparser;

	/* Scan the line pointed by CP and return the number of
	 * consumed bytes if something interesting is found.
	 * Return 0 if no interest.
	 */
	int (* lineNotify) (shSubparser *s, const unsigned char *cp);

	/* Extract the interesting item from CP and store it to NAME.
	 * Return the number of consumed bytes during extracting.
	 */
	int (* extractName) (shSubparser *s, const unsigned char *cp,
						 vString *name);

	/* Make a tag for NAME.
	 * Return the cork index for the tag.
	 */
	int (* makeTag) (shSubparser *s, vString *name);
};

#endif	/* CTAGS_PARSER_SH_H */
