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

static kindDefinition ClojureKinds[] = {
	{true, 'f', "function", "functions"},
	{true, 'n', "namespace", "namespaces"}
};

static int isNamespace (const char *strp)
{
	return strncmp (++strp, "ns", 2) == 0 && isspace (strp[2]);
}

static int isCoreNamespace (const char *strp)
{
	return strncmp (++strp, "clojure.core/ns", 15) == 0 && isspace (strp[15]);
}

static int isFunction (const char *strp)
{
	return (strncmp (++strp, "defn", 4) == 0 && isspace (strp[4]));
}

static int isCoreFunction (const char *strp)
{
	return (strncmp (++strp, "clojure.core/defn", 17) == 0 && isspace (strp[17]));
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
}

const char* skipMetadata (const char *dbp)
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

static int makeNamespaceTag (vString * const name, const char *dbp)
{
	dbp = skipMetadata (dbp);
	functionName (name, dbp);
	if (vStringLength (name) > 0 && ClojureKinds[K_NAMESPACE].enabled)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (name), K_NAMESPACE);
		e.lineNumber = getInputLineNumber ();
		e.filePosition = getInputFilePosition ();

		return makeTagEntry (&e);
	}
	else
		return CORK_NIL;
}

static void makeFunctionTag (vString * const name, const char *dbp, int scope_index)
{
	dbp = skipMetadata (dbp);
	functionName (name, dbp);
	if (vStringLength (name) > 0 && ClojureKinds[K_FUNCTION].enabled)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (name), K_FUNCTION);
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
			if (isNamespace (p) || isCoreNamespace (p))
			{
				skipToSymbol (&p);
				scope_index = makeNamespaceTag (name, p);
			}
			else if (isFunction (p) || isCoreFunction (p))
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
	def->kindTable = ClojureKinds;
	def->kindCount = ARRAY_SIZE (ClojureKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findClojureTags;
	def->useCork = CORK_QUEUE;
	return def;
}
