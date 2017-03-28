/*
 *
 *   Copyright (c) 2016, Masatake YAMATO
 *   Copyright (c) 2016, Red Hat, K.K.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#ifndef CTAGS_YAML__H
#define CTAGS_YAML__H

#include "general.h"
#include "subparser.h"
#include "types.h"

#ifdef HAVE_LIBYAML
#include <yaml.h>
#else
#define yaml_token_t void
#endif

typedef struct sYamlSubparser yamlSubparser;
struct sYamlSubparser {
	subparser subparser;
	void (* newTokenNotfify) (yamlSubparser *s, yaml_token_t *token);
};

extern void attachYamlPosition (tagEntryInfo *tag, yaml_token_t *token, bool asEndPosition);

#endif
