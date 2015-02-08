/**
 *   Copyright (c) 2015, Miloslav Nenadál <nenadalm@gmail.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains code for generating tags for the Clojure language.
 */

#include <string.h>

#include "general.h"

#include "parse.h"
#include "read.h"
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

static vString *CurrentNamespace;

static int isNamespace (const unsigned char *strp)
{
	return strncmp (++strp, "ns", 2) == 0 && isspace (strp[2]);
}

static int isFunction (const unsigned char *strp)
{
	return strncmp (++strp, "defn", 4) == 0 && isspace (strp[4]);
}

static int isQuote (const unsigned char *strp)
{
	return strncmp (++strp, "quote", 5) && isspace (strp[5]);
}

static void functionName (vString * const name, const unsigned char *dbp)
{
	const unsigned char *p;

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

static void makeNamespaceTag (vString * const name, const unsigned char *dbp)
{
	vStringClear (CurrentNamespace);

	functionName (name, dbp);
	vStringCopy (CurrentNamespace, name);
	if (vStringLength (CurrentNamespace) > 0)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (CurrentNamespace));
		e.lineNumber = getSourceLineNumber ();
		e.filePosition = getInputFilePosition ();
		e.kindName = ClojureKinds[K_NAMESPACE].name;
		e.kind = (char) ClojureKinds[K_NAMESPACE].letter;

		makeTagEntry (&e);
	}
}

static void makeFunctionTag (vString * const name, const unsigned char *dbp)
{
	functionName (name, dbp);
	if (vStringLength (name) > 0)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (name));
		e.lineNumber = getSourceLineNumber ();
		e.filePosition = getInputFilePosition ();
		e.kindName = ClojureKinds[K_FUNCTION].name;
		e.kind = (char) ClojureKinds[K_FUNCTION].letter;

		if (vStringLength (CurrentNamespace) > 0)
		{
			e.extensionFields.scope[0] = ClojureKinds[K_NAMESPACE].name;
			e.extensionFields.scope[1] = vStringValue (CurrentNamespace);
		}

		makeTagEntry (&e);
	}
}

static void skipToSymbol (const unsigned char **p)
{
	while (**p != '\0' && !isspace ((int) **p))
		*p = *p + 1;
	while (isspace ((int) **p))
		*p = *p + 1;
}

static void findClojureTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *p;
	CurrentNamespace = vStringNew ();

	while ((p = fileReadLine ()) != NULL)
	{
		vStringClear (name);

		while (isspace (*p))
			p++;

		if (*p == '(')
		{
			if (isNamespace (p))
			{
				skipToSymbol (&p);
				makeNamespaceTag (name, p);
			}
			else if (isFunction (p))
			{
				skipToSymbol (&p);
				makeFunctionTag (name, p);
			}
		}
	}
	vStringDelete (name);
	vStringDelete (CurrentNamespace);
}

extern parserDefinition *ClojureParser (void)
{
	static const char *const extensions[] = {
		"clj", NULL
	};
	static const char *const aliases[] = {
		NULL
	};

	parserDefinition *def = parserNew ("Clojure");
	def->kinds = ClojureKinds;
	def->kindCount = KIND_COUNT (ClojureKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findClojureTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
