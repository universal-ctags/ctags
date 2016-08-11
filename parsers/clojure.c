/**
 *   Copyright (c) 2015, Miloslav Nenadál <nenadalm@gmail.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains code for generating tags for the Clojure language.
 */

#include "general.h"

#include <string.h>

#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "entry.h"

typedef enum {
	K_FUNCTION,
	K_NAMESPACE
} clojureKind;

static kindOption ClojureKinds[] = {
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'n', "namespace", "namespaces"}
};

static int isNamespace (const char *strp)
{
	return strncmp (++strp, "ns", 2) == 0 && isspace (strp[2]);
}

static int isFunction (const char *strp)
{
	return strncmp (++strp, "defn", 4) == 0 && isspace (strp[4]);
}

static int isQuote (const char *strp)
{
	return strncmp (++strp, "quote", 5) == 0 && isspace (strp[5]);
}

static void functionName (vString * const name, const char *dbp)
{
	const char *p;

	if (*dbp == '\'')
		dbp++;
	else if (*dbp == '(' && isQuote (dbp))
	{
		dbp += 7;
		while (isspace (*dbp))
			dbp++;
	}

	for (p = dbp; *p != '\0' && *p != '(' && !isspace ((int) *p) && *p != ')';
		p++)
		vStringPut (name, *p);
	vStringTerminate (name);
}

static int makeNamespaceTag (vString * const name, const char *dbp)
{
	functionName (name, dbp);
	if (vStringLength (name) > 0 && ClojureKinds[K_NAMESPACE].enabled)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (name), &(ClojureKinds[K_NAMESPACE]));
		e.lineNumber = getInputLineNumber ();
		e.filePosition = getInputFilePosition ();

		return makeTagEntry (&e);
	}
	else
		return CORK_NIL;
}

static void makeFunctionTag (vString * const name, const char *dbp, int scope_index)
{
	functionName (name, dbp);
	if (vStringLength (name) > 0 && ClojureKinds[K_FUNCTION].enabled)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (name), &(ClojureKinds[K_FUNCTION]));
		e.lineNumber = getInputLineNumber ();
		e.filePosition = getInputFilePosition ();

		e.extensionFields.scopeIndex =  scope_index;
		makeTagEntry (&e);
	}
}

static void skipToSymbol (const char **p)
{
	while (**p != '\0' && !isspace ((int) **p))
		*p = *p + 1;
	while (isspace ((int) **p))
		*p = *p + 1;
}

static void findClojureTags (void)
{
	vString *name = vStringNew ();
	const char *p;
	int scope_index = CORK_NIL;

	while ((p = (char *)readLineFromInputFile ()) != NULL)
	{
		vStringClear (name);

		while (isspace (*p))
			p++;

		if (*p == '(')
		{
			if (isNamespace (p))
			{
				skipToSymbol (&p);
				scope_index = makeNamespaceTag (name, p);
			}
			else if (isFunction (p))
			{
				skipToSymbol (&p);
				makeFunctionTag (name, p, scope_index);
			}
		}
	}
	vStringDelete (name);
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
	def->kinds = ClojureKinds;
	def->kindCount = ARRAY_SIZE (ClojureKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findClojureTags;
	def->useCork = TRUE;
	return def;
}
