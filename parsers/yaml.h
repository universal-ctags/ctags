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
	struct ypathTypeStack *ypathTypeStack;
};
#define YAML(S) ((yamlSubparser *)S)

extern void attachYamlPosition (tagEntryInfo *tag, yaml_token_t *token, bool asEndPosition);

/*
 * Experimental Ypath code
 */
typedef struct sTagYpathTable {
	const char * ypath;
	int expected_state;
	int kind;
	void *code;
} tagYpathTable;

extern int ypathCompileTable (langType language, tagYpathTable *table, int keywordId);
extern void ypathCompileTables (langType language, tagYpathTable tables[], size_t count, int keywordId);
extern void ypathCompiledCodeDelete (tagYpathTable tables[], size_t count);

extern void ypathHandleToken (yamlSubparser *yaml, yaml_token_t *token, int state, tagYpathTable tables[], size_t count);

extern void ypathPushType (yamlSubparser *yaml, yaml_token_t *token);
extern void ypathPopType (yamlSubparser *yaml);
extern void ypathPopAllTypes (yamlSubparser *yaml);
extern void ypathFillKeywordOfTokenMaybe (yamlSubparser *yaml, yaml_token_t *token, langType lang);

extern void ypathPrintTypeStack(yamlSubparser *yaml);
#endif
