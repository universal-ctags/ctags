/*
 *   Copyright (c) 2022 Nik Silver
 *   based on the Kotlin code by Jan Dolinár
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Elm.
 *   See https://elm-lang.org/docs/syntax for language reference.
 */

/*
 *   INCLUDE FILES
 */
#include "parse.h"
#include "trace.h"

/*
 * FUNCTION DEFINITIONS
 */

/* kind - The kind of this tag.
 * role - The role this tag plays. But def(ined) is expressed by setting
 *     this to ROLE_DEFINITION_INDEX.
 * pushScope - If true, also update the scope to be this tag.
 */
static int makeElmTag (struct parserCtx *auxil, const char *name, long offset, int kind, int role)
{
	tagEntryInfo e;

	if (role == ROLE_DEFINITION_INDEX)
	{
		initTagEntry (&e, name, kind);
	}
	else
	{
		initRefTagEntry (&e, name, kind, role);
	}

	e.lineNumber = getInputLineNumberForFileOffset (offset);
	e.filePosition = getInputFilePositionForLine (e.lineNumber);
	e.extensionFields.scopeIndex = BASE_SCOPE (auxil);
	int scope_index = makeTagEntry (&e);
	return scope_index;
}

/* Like makeElmTag, but have this tag be the latest scope.
 */
static int makeElmTagSettingScope (struct parserCtx *auxil, const char *name, long offset, int kind, int role)
{
	// Here is a useless line just to use a function and so
	// get rid of a compiler warning.
	if (1 < 0)
	{
		PEEK_KIND (auxil);
	}
	// The real function starts here...

	int scope_index = makeElmTag (auxil, name, offset, kind, role);
	SET_SCOPE (auxil, scope_index);
	return scope_index;
}

static void addElmTypeRef(int scope_index, const char *sig)
{
	tagEntryInfo *e = getEntryInCorkQueue (scope_index);

	if (e)
	{
		vString *vsig = collapseWhitespace (sig);

		e->extensionFields.typeRef [0] = eStrdup ("description");
		e->extensionFields.typeRef [1] = vStringDeleteUnwrap (vsig);
	}
}

/* For a signature such as "a1   b2  c3" we want to transform it
 * to "a1 b2 c3" for the signature field.
 */
static vString *collapseWhitespace (const char *sig)
{
	vString *vstr = vStringNew ();

	const char *c = sig;
	char c2;
	int found_ws = 0;

	for ( ; *c != '\0'; c++)
	{
		// The character, in case we need to change it
		c2 = *c;

		if (c2 == ' ' || c2 == '\t' || c2 == '\r' || c2 == '\n' || c2 == '\f')
		{
			// It's whitespace. Make it plain space
			c2 = ' ';
			if (found_ws)
			{
				// We found whitespace just before, so ignore this
				continue;
			}

			found_ws = 1;
		}
		else
		{
			found_ws = 0;
		}
		vStringPut (vstr, c2);
	}

	return vstr;
}

static void ctxInit (struct parserCtx *auxil)
{
	BASE_INIT (auxil, K_MODULE);
}

static void ctxFini (struct parserCtx *auxil)
{
	BASE_FINI (auxil);
}

static void findElmTags (void)
{
	struct parserCtx auxil;

	ctxInit (&auxil);
	pelm_context_t *pctx = pelm_create (&auxil);

	while (pelm_parse (pctx, NULL) && (! BASE_ERROR (&auxil)))
		;

	pelm_destroy (pctx);
	ctxFini (&auxil);
}

extern parserDefinition *ElmParser (void)
{
	static const char *const extensions [] = { "elm", NULL };
	parserDefinition *def = parserNew ("Elm");
	def->kindTable = ElmKinds;
	def->kindCount = ARRAY_SIZE (ElmKinds);
	def->extensions = extensions;
	def->parser = findElmTags;
	def->fieldTable = ElmFields;
	def->fieldCount = ARRAY_SIZE (ElmFields);
	def->useCork = true;
	def->enabled    = true;
	//def->requestAutomaticFQTag = true;
	def->defaultScopeSeparator = ".";
	return def;
}
