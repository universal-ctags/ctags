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
#include "options.h"
#include "parse.h"
#include "routines.h"

/*
* FUNCTION DEFINITIONS
*/
static void pushKindContextual (struct parserCtx *auxil)
{
	int k0 = basePeekKind(BASE(auxil));
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

	basePushKind (BASE(auxil), k);
}

static void setMethodParamState (struct parserCtx *auxil, methodParamState s)
{
	auxil->mparam_state = s;
}

static int makeVarlinkTag (struct parserCtx *auxil, const char *name, long offset)
{
	tagEntryInfo e;
	int k = PEEK_KIND (auxil);
	initTagEntry(&e, name, k);
	e.lineNumber = getInputLineNumberForFileOffset (offset);
	e.filePosition = getInputFilePositionForLine (e.lineNumber);
	e.extensionFields.scopeIndex = BASE_SCOPE (auxil);
	return makeTagEntry (&e);
}

static void ctxInit (struct parserCtx *auxil)
{
	BASE_INIT(auxil, K_INTERFACE);
}

static void ctxFini (struct parserCtx *auxil)
{
	BASE_FINI(auxil);
}

static void findVarlinkTags (void)
{
	struct parserCtx auxil;

	ctxInit (&auxil);
	pvarlink_context_t *pctx = pvarlink_create(&auxil);

	while (pvarlink_parse(pctx, NULL) && (!BASE_ERROR(&auxil)) );

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
