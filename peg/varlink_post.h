/*
 *   Copyright (c) 2019 Masatake YAMATO
 *   Copyright (c) 2019 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Varlink.
 */

/*
*   INCLUDE FILES
*/
#include "debug.h"
#include "options.h"
#include "parse.h"
#include "routines.h"

/*
* FUNCTION DEFINITIONS
*/
static void pushKind (struct parserCtx *auxil, int kind)
{
	intArrayAdd (auxil->kind_stack, kind);
}

static void popKind (struct parserCtx *auxil, bool popScopeToo)
{
	intArrayRemoveLast (auxil->kind_stack);

	if (popScopeToo)
	{
		tagEntryInfo *e = getEntryInCorkQueue (auxil->scope_cork_index);
		if (e)
			auxil->scope_cork_index = e->extensionFields.scopeIndex;
	}
}

static void pushKindContextual (struct parserCtx *auxil)
{
	int k0 = intArrayLast (auxil->kind_stack);
	int k = KIND_GHOST_INDEX;

	switch (k0)
	{
	case K_METHOD:
		if (auxil->mparam_state == METHOD_PARAM_INPUT)
			k = K_IPARAM;
		else
			k = K_OPARAM;
		break;
	case K_STRUCT:
		k = K_FIELD;
		break;
	case K_ENUM:
		k = K_ENUMERATION;
		break;
	case K_ERROR:
		k = K_EDESC;
		break;
	default:
		k = k0;
		break;
	}

	pushKind (auxil, k);
}

static int peekKind (struct parserCtx *auxil)
{
	return intArrayLast (auxil->kind_stack);
}

static void setMethodParamState (struct parserCtx *auxil, methodParamState s)
{
	auxil->mparam_state = s;
}

static void reportError (struct parserCtx *auxil)
{
	auxil->found_syntax_error = true;
	verbose ("%s: syntax error in \"%s\"\n",
			 getLanguageName (getInputLanguage ()),
			 getInputFileName());
}

static int makeVarlinkTag (struct parserCtx *auxil, const char *name, long offset)
{
	tagEntryInfo e;
	int k = peekKind (auxil);
	initTagEntry(&e, name, k);
	e.lineNumber = getInputLineNumberForFileOffset (offset);
	e.filePosition = getInputFilePositionForLine (e.lineNumber);
	e.extensionFields.scopeIndex = auxil->scope_cork_index;
	return makeTagEntry (&e);
}

static void ctxInit (struct parserCtx *auxil)
{
	auxil->kind_stack = intArrayNew ();
	pushKind (auxil, K_INTERFACE);
	auxil->scope_cork_index = CORK_NIL;
	auxil->found_syntax_error = false;
}

static void ctxFini (struct parserCtx *auxil)
{
	popKind (auxil, false);
	intArrayDelete (auxil->kind_stack);
}

static void findVarlinkTags (void)
{
	struct parserCtx auxil;

	ctxInit (&auxil);
	pvarlink_context_t *pctx = pvarlink_create(&auxil);

	while (pvarlink_parse(pctx, NULL) && (!auxil.found_syntax_error) );

	pvarlink_destroy(pctx);
	ctxFini (&auxil);
}

extern parserDefinition* VarlinkParser (void)
{
	static const char *const extensions [] = { "varlink", NULL };
	parserDefinition* def = parserNew ("Varlink");
	def->kindTable  = VarlinkKinds;
	def->kindCount  = ARRAY_SIZE (VarlinkKinds);
	def->extensions = extensions;
	def->parser     = findVarlinkTags;
	def->useCork    = true;
	def->enabled    = true;
	def->defaultScopeSeparator = ".";
	return def;
}
