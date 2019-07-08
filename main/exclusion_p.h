/*
*   Copyright (c) 1996-2003, Darren Hiebert
*   Copyright (c) 2019, Red Hat, Inc.
*   Copyright (c) 2019, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#include "general.h"  /* must always come first */

typedef enum eExclusionType {
	EXCLUSION_GLOB_STRING,
	EXCLUSION_GLOB_PATH,
} exclusionType;

struct sExclusionSpec {
	exclusionType type;

}
