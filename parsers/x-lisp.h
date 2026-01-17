/*
 *   Copyright (c) 2011, Colomban Wendling <colomban@geany.org>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   List meata parser interface exported to the other lisp families
 */

#ifndef CTAGS_LISP_H
#define CTAGS_LISP_H

#include "general.h"

#include "field.h"

struct lispIsDefResult {
	bool is_def;
	int kind;
};

struct lispKindHint {
	vString * str;
	struct lispIsDefResult isDefResult;
};

struct lispDialect {
	int (* definer2kind) (struct lispKindHint *hint, const char *namespace);
	bool case_insensitive;
	unsigned char namespace_sep;
	int unknown_kind;
	fieldDefinition *definer_field;
	bool skip_initial_spaces;
	bool lambda_syntax_sugar;
	struct lispIsDefResult (* is_def) (struct lispDialect *, const unsigned char *);
	int (* get_it) (struct lispDialect *,
					vString *const, const unsigned char *, struct lispKindHint *,
					const char *);
	int scope;
};

void findLispTagsCommon (struct lispDialect *dialect);

int lispGetIt (struct lispDialect *dialect,
			   vString *const name, const unsigned char *dbp, struct lispKindHint *kind_hint,
			   const char *namespace);
struct lispIsDefResult lispIsDef (struct lispDialect *dialect, const unsigned char *strp);

#endif	/* CTAGS_LISP_H */
