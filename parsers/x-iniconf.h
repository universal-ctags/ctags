/*
*
*   Copyright (c) 2000-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for ini/config files.
*/

/*
 *  This is based on geany's conf.c:
 * --------------------------------
 * commit 3af538fa65f8b17897259080db8144b1edc43470
 * Author: Enrico Tröger <enrico.troeger@uvena.de>
 * Date:   Sun Nov 27 20:39:57 2005 +0000
 *
 * added tag support for filetype Conf
lang *
 *
 * git-svn-id: https://geany.svn.sourceforge.net/svnroot/geany/trunk@15 ea778897-0a13-0410-b9d1-a72fbfd435f5
 *
 */

#ifndef CTAGS_INITCONF_H
#define CTAGS_INITCONF_H

#include "general.h"

#include "subparser.h"

typedef struct sIniconfSubparser iniconfSubparser;
struct sIniconfSubparser {
	subparser subparser;

	bool  (* probeLanguage)  (const char *section, const char *key, const char *value);
	void  (* newDataNotify)  (iniconfSubparser *s,
							  const char *section, const char *key, const char *value);
};

#endif
