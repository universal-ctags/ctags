/**
 *   Copyright (c) 2015, Miloslav Nenadál <nenadalm@gmail.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains code for generating tags for the Clojure language.
 */


/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "parse.h"

#include "x-lisp.h"

#include <ctype.h>
#include <string.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_UNKNOWN,
	K_FUNCTION,
	K_NAMESPACE
} clojureKind;

typedef enum {
	F_DEFINER,
} clojureField;

static fieldDefinition ClojureFields[] = {
	{ .name = "definer",
	  .description = "the name of the function or macro that defines the unknown/Y-kind object",
	  .enabled = true,
	  .version = 1 },
};

static kindDefinition ClojureKinds[] = {
	{ true, 'Y', "unknown", "unknown type of definitions",
	  .version = 1 },
	{ true, 'f', "function", "functions" },
	{ true, 'n', "namespace", "namespaces" },
};

/*
*   FUNCTION DEFINITIONS
*/
static bool clojure_is_def (struct lispDialect *dialect CTAGS_ATTR_UNUSED, const unsigned char *strp)
{
	if (strp [1] == 'n' && strp [2] == 's' && isspace (strp [3]))
		return true;

	if (strp [1] == 'd' && strp [2] == 'e' && strp [3] == 'f' && strp [4] == 'n'
		&& isspace (strp [5]))
		return true;

	return false;
}

static int  clojure_hint2kind (const vString *const hint, const char *namespace)
{
	int k = K_UNKNOWN;
	int n = vStringLength (hint) - 4;
	unsigned int offset = 1;

	if (strcmp (namespace, "clojure.core/") == 0)
	{
		offset = 0;
		n++;
	}

	if (strncmp (vStringValue (hint) + offset, "ns", 2) == 0)
		return K_NAMESPACE;

#define EQN(X) strncmp(vStringValue (hint) + offset + 3, &X[3], n) == 0
	switch (n)
	{
	case 1:
		if (EQN("defn"))
			k = K_FUNCTION;
		break;
	}
#undef EQN
	return k;
}

const unsigned char* clojure_skip_metadata (const unsigned char *dbp)
{
	while (1)
	{
		if (*dbp == '^')
		{
			dbp++;
			if (*dbp == '{')
			{
				/* skipping an arraymap */
				for (; *dbp != '\0' && *dbp != '}'; dbp++)
					;
			}
			else
			{
				/* skip a keyword or a symbol */
				for (; *dbp != '\0' && !isspace((unsigned char)*dbp); dbp++)
					;
			}

			if (*dbp == '\0')
				break;

			dbp++;
			while (isspace ((unsigned char)*dbp))
				dbp++;
		}
		else
			break;
	}

	return dbp;
}

static int clojure_get_it (struct lispDialect *dialect,
						   vString *const name, const unsigned char *dbp, vString *kind_hint,
						   const char *namespace)
{
	dbp = clojure_skip_metadata (dbp);
	int index = lispGetIt (dialect, name, dbp, kind_hint, namespace);
	tagEntryInfo *e = getEntryInCorkQueue (index);

	if (!e)
		return CORK_NIL;

	if (e->kindIndex == K_NAMESPACE)
		dialect->scope = index;
	else
		e->extensionFields.scopeIndex = dialect->scope;

	return index;
}

static void findClojureTags (void)
{
	struct lispDialect clojure_dialect = {
		.definer2kind = clojure_hint2kind,
		.case_insensitive = false,
		.namespace_sep = '/',
		.unknown_kind = K_UNKNOWN,
		.definer_field = ClojureFields + F_DEFINER,
		.skip_initial_spaces = true,
		.is_def = clojure_is_def,
		.get_it = clojure_get_it,
		.scope = CORK_NIL,
	};

	findLispTagsCommon (&clojure_dialect);
}

extern parserDefinition *ClojureParser (void)
{
	static const char *const extensions[] = {
		"clj", "cljs", "cljc", NULL
	};
	static const char *const aliases[] = {
		NULL
	};

	parserDefinition *def = parserNew ("Clojure");
	def->kindTable = ClojureKinds;
	def->kindCount = ARRAY_SIZE (ClojureKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findClojureTags;
	def->useCork = CORK_QUEUE;
	def->versionCurrent = 1;
	def->versionAge = 1;
	return def;
}
