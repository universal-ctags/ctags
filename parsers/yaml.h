/*
 *
 *   Copyright (c) 2016, Masatake YAMATO
 *   Copyright (c) 2016, Red Hat, K.K.
 *   Copyright (c) 2022, Vasily Kulikov
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

struct ypathTypeStack;

typedef struct sYamlSubparser yamlSubparser;
struct sYamlSubparser {
	subparser subparser;
	void (* newTokenNotfify) (yamlSubparser *s, yaml_token_t *token);
	struct sTagYpathTable *ypathTables;
	size_t ypathTableCount;

	bool compiled;
	struct ypathTypeStack *ypathTypeStack;
	enum ypathDetectingState {
		YPATH_DSTAT_LAST_KEY,
		YPATH_DSTAT_LAST_VALUE,
		YPATH_DSTAT_INITIAL,
	} detectionState;
};

#define YAML(S) ((yamlSubparser *)S)

extern void attachYamlPosition (tagEntryInfo *tag, yaml_token_t *token, bool asEndPosition);

/*
 * Experimental Ypath code
 */
typedef struct sTagYpathTable {
	const char * ypath;
	int expected_state;
	/* If INITTAGENTRY filed is non-NULL, call it for initializing
	 * a tagEntry. If it is NULL, initializing the tagEntry in usual way:
	 * call initTagEntry() defined in the main part with KIND. */
	int kind;
	bool (* initTagEntry) (tagEntryInfo *, char *, void *);
	void *data;
	void *code;					/* YAML base parser private */
} tagYpathTable;

extern void ypathPrintTypeStack(yamlSubparser *yaml);
#endif
