/*
*   Copyright (c) 2024, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for TOML files.
*/
#ifndef CTAGS_PARSER_TOML_H
#define CTAGS_PARSER_TOML_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"
#include "numarray.h"

typedef enum {
	TOML_K_KEY,
	TOML_K_TABLE,
	TOML_K_ARRAYTABLE,
	TOML_K_QKEY,
} tomlKind;

typedef enum {
	TOML_R_KEY_CHAINELT,
} tomlKeyRole;

typedef struct sTomlSubparser tomlSubparser;

struct sTomlSubparser {
	subparser subparser;

	void (* valueNotify) (tomlSubparser *sub, const char *value, long offset, int corkIndex);
};

#endif	/* CTAGS_PARSER_TOML_H */
