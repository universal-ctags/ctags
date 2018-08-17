/*
 *   Copyright (c) 2018 Masatake YAMATO
 *   Copyright (c) 2018 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Java 1.7.
 */

#include "trace.h"

static bool includeTypeArgument;

static void leaveBlock (struct parserCtx *auxil);

static void ctxInit (struct parserCtx *auxil)
{
	auxil->currentSink = NULL;
	auxil->offset = -1;

	for (int i = 0; i < MAX_SINK_TYPE; i++)
		auxil->sinks[i] = vStringNew();
	auxil->sinkBlockDepth = 0;

	auxil->nlevels = nestingLevelsNew (0);
	auxil->inPackage = false;
	auxil->modifiersTracker = ulongArrayNew ();
	auxil->varKindsTracker = intArrayNew ();

	auxil->modifier = 0UL;
	auxil->found_syntax_error = false;
	auxil->lastPoppedCorkIndex = CORK_NIL;
	auxil->looking_for_anon_class = false;
}

static void ctxFini (struct parserCtx *auxil)
{
	auxil->currentSink = NULL;
	auxil->offset = -1;

	for (int i = 0; i < MAX_SINK_TYPE; i++)
	{
		vStringDelete (auxil->sinks[i]);
		auxil->sinks[i] = NULL;
	}
	auxil->currentSink = NULL;

	nestingLevelsFree(auxil->nlevels);
	auxil->nlevels = NULL;
	auxil->inPackage = false;
	ulongArrayDelete (auxil->modifiersTracker);
	auxil->modifiersTracker = NULL;
	intArrayDelete (auxil->varKindsTracker);
	auxil->varKindsTracker = NULL;
}

static void enterTypeArguments(struct parserCtx *auxil)
{
	if (includeTypeArgument == 0)
		auxil->sinkBlockDepth++;
}

static void leaveTypeArguments(struct parserCtx *auxil)
{
	if (includeTypeArgument == 0)
		auxil->sinkBlockDepth--;
}

static void activateSink (struct parserCtx *auxil, sinkType stype)
{
	auxil->currentSink = auxil->sinks [stype];
	auxil->offset = -1;
}

static void deactivateSink (struct parserCtx *auxil)
{
	if (auxil->currentSink)
		vStringClear (auxil->currentSink);
	auxil->currentSink = NULL;
	auxil->offset = -1;
}

static const vString* fossick (struct parserCtx *auxil)
{
	return auxil->currentSink;
}

static void eatIdentifier (struct parserCtx *auxil, const char *id,
						   int idOffset, int endCandidateOffset)
{
	if (auxil->currentSink)
	{
		if (auxil->offset == -1)
			auxil->offset = idOffset;
		else
		{
			if (auxil->currentSink == auxil->sinks[S_NAME])
				eatChar (auxil, '.');
		}
		eatString(auxil, id, endCandidateOffset - idOffset);
		auxil->endCandidateOffset = endCandidateOffset;
	}
}

static void eatRightwing (struct parserCtx *auxil, int wingOffset)
{
	auxil->endCandidateOffset = wingOffset;
}

static void eatComma (struct parserCtx *auxil, int commaOffset)
{
	auxil->endCandidateOffset = commaOffset;
}

static void eatChar (struct parserCtx *auxil, char c)
{
	if (auxil->currentSink && auxil->sinkBlockDepth == 0)
		vStringPut (auxil->currentSink, c);
}

static void eatString (struct parserCtx *auxil, const char *s, size_t len)
{
	if (auxil->currentSink && auxil->sinkBlockDepth == 0)
		vStringNCatSUnsafe (auxil->currentSink, s, len);
}

static void patchEndline (struct parserCtx *auxil)
{
	if (auxil->lastPoppedCorkIndex != CORK_NIL)
	{
		tagEntryInfo *e = getEntryInCorkQueue (auxil->lastPoppedCorkIndex);
		e->extensionFields.endLine = getInputLineNumberForFileOffset (auxil->endCandidateOffset);
	}
}

static const char* chooseAccessString (unsigned long modifier, int kind, int kind_parent)
{
	if (modifier & (1UL << JMOD_PUBLIC))
		return "public";
	else if (modifier & (1UL << JMOD_PROTECTED))
		return "protected";
	else if (modifier & (1UL << JMOD_PRIVATE))
		return "private";
	else
	{
		switch (kind_parent)
		{
		case K_CLASS:
			return "default";
		case K_INTERFACE:
		case K_ENUM:
		case K_ANNOTATION:
			return "public";
		default:
			return "default";
			return NULL;
		}
	}
}

static int makeJavaRefTag (struct parserCtx *auxil, const char* name, int kind, int role)
{
	tagEntryInfo e, *e_parent = NULL;
	NestingLevel *n;
	vString *anon = NULL;

	if (name)
		initRefTagEntry(&e, name, kind, role);
	else
	{
		anon = vStringNew ();

		if (kind == K_CLASS)
			anonGenerate (anon, "__anon", K_CLASS);
		else
			anonGenerate (anon, "__anon???", kind);

		initTagEntry(&e, vStringValue(anon), kind);
		markTagExtraBit (&e, XTAG_ANONYMOUS);
	}

	if (kind != K_PACKAGE
		&& (n = nestingLevelsGetCurrent (auxil->nlevels)))
	{
		e.extensionFields.scopeIndex = n->corkIndex;
		e_parent = getEntryInCorkQueue (n->corkIndex);
	}

	if (auxil->offset != -1)
	{
		e.lineNumber = getInputLineNumberForFileOffset (auxil->offset);
		e.filePosition = getInputFilePositionForLine (e.lineNumber);
	}

	e.isFileScope = (auxil->modifier & (1UL << JMOD_PRIVATE))? 1: 0;

	{
		int kind_parent = e_parent? e_parent->kindIndex: K_UNDEFINED;
		e.extensionFields.access = chooseAccessString(auxil->modifier, kind,
													  kind_parent);
	}
	e.extensionFields.implementation = (auxil->modifier & (1UL << JMOD_ABSTRACT))
		? "abstract": NULL;

	/* TODO: signature, typeRef */

	unsigned int cork_Id = makeTagEntry(&e);

	switch (kind)
	{
	case K_ANNOTATION:
	case K_CLASS:
	case K_INTERFACE:
	case K_METHOD:
	case K_ENUM:
	case K_ENUM_CONSTANT:
		nestingLevelsPush(auxil->nlevels, cork_Id);
		break;
	case K_PACKAGE:
		if (role == ROLE_INDEX_DEFINITION)
			nestingLevelsPush(auxil->nlevels, cork_Id);
		break;
	}

	if (anon)
		vStringDelete (anon);

	return cork_Id;
}

static int makeJavaTag (struct parserCtx *auxil, const char* name, int kind)
{
	return makeJavaRefTag (auxil, name, kind, ROLE_INDEX_DEFINITION);
}

static void enterPackage (struct parserCtx *auxil)
{
	const vString *package = fossick (auxil);

	makeJavaTag (auxil, vStringValue(package), K_PACKAGE);
	auxil->inPackage = true;

	deactivateSink (auxil);
}

static void leavePackageMaybe (struct parserCtx *auxil, int endOffset)
{
	if (auxil->inPackage)
	{
		auxil->endCandidateOffset = endOffset;
		leaveBlock(auxil);
		auxil->inPackage = false;
	}
}

static int enterBlock (struct parserCtx *auxil, const vString *const name, int kind)
{
	int corkIndex;

	auxil->lastPoppedCorkIndex = CORK_NIL;
	corkIndex = makeJavaTag (auxil, name? vStringValue(name): NULL, kind);
	resetModifier (auxil);
	return corkIndex;
}

static void leaveBlock (struct parserCtx *auxil)
{
	NestingLevel *n = nestingLevelsGetCurrent (auxil->nlevels);

	if (n)
	{
		int corkIndex = n->corkIndex;

		tagEntryInfo *e;
		e = getEntryInCorkQueue (corkIndex);
		if (e)
		{
			switch (e->kindIndex)
			{
			case K_ANNOTATION:
			case K_CLASS:
			case K_INTERFACE:
			case K_METHOD:
			case K_ENUM:
			case K_ENUM_CONSTANT:
			case K_PACKAGE:
				e->extensionFields.endLine = getInputLineNumberForFileOffset (auxil->endCandidateOffset);
				nestingLevelsPop(auxil->nlevels);
				auxil->lastPoppedCorkIndex = corkIndex;
			}
		}
	}
}

static void enterClass (struct parserCtx *auxil)
{
	const vString *klass = fossick (auxil);
	enterBlock (auxil, klass, K_CLASS);
	deactivateSink (auxil);
}

static void leaveClass (struct parserCtx *auxil)
{
	leaveBlock (auxil);
}

static void enterAnonClassMaybe (struct parserCtx *auxil)
{
	int corkIndex = enterBlock (auxil, NULL, K_CLASS);
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	const vString *inheritance = fossick (auxil);
	e->extensionFields.inheritance = vStringStrdup (inheritance);
	deactivateSink (auxil);

	e->placeholder = true;
	auxil->looking_for_anon_class = true;
}

static void markAnonClassMaybe (struct parserCtx *auxil)
{
	if (auxil->looking_for_anon_class)
	{
		NestingLevel *n = nestingLevelsGetCurrent (auxil->nlevels);

		if (n && (n->corkIndex != CORK_NIL))
		{
			tagEntryInfo *e = getEntryInCorkQueue (n->corkIndex);
			e->placeholder = false;
		}
		auxil->looking_for_anon_class = false;
	}
}

static void leaveAnonClass (struct parserCtx *auxil)
{
	auxil->looking_for_anon_class = false;
	leaveBlock(auxil);
}

static void enterInterface (struct parserCtx *auxil)
{
	const vString *iface = fossick (auxil);
	enterBlock (auxil, iface, K_INTERFACE);
	deactivateSink (auxil);
}

static void leaveInterface (struct parserCtx *auxil)
{
	leaveBlock (auxil);
}

static void enterMethod (struct parserCtx *auxil)
{
	const vString *method = fossick (auxil);
	enterBlock (auxil, method, K_METHOD);
	deactivateSink (auxil);
}

static void leaveMethod (struct parserCtx *auxil)
{
	leaveBlock (auxil);
}

static void enterAnnotationDecl (struct parserCtx *auxil)
{
	const vString *anot = fossick (auxil);
	enterBlock (auxil, anot, K_ANNOTATION);
	deactivateSink (auxil);
}

static void leaveAnnotationDecl (struct parserCtx *auxil)
{
	leaveBlock (auxil);
}

static void resetModifier(struct parserCtx *auxil)
{
	auxil->modifier = 0UL;
}

static void markModifier(struct parserCtx *auxil, javaModifier jmod)
{
	auxil->modifier |= (1UL << jmod);
}

static void pushModifier(struct parserCtx *auxil)
{
	TRACE_PRINT("modifier: %x", auxil->modifier);
	ulongArrayAdd (auxil->modifiersTracker, auxil->modifier);
	resetModifier (auxil);
}

static void popModifier(struct parserCtx *auxil)
{
	ulongArrayRemoveLast (auxil->modifiersTracker);
	resetModifier (auxil);
}

static unsigned long peekModifier(struct parserCtx *auxil)
{
	return ulongArrayLast (auxil->modifiersTracker);
}

static void pushVarKind(struct parserCtx *auxil, javaKind kind)
{
	intArrayAdd(auxil->varKindsTracker, kind);
}

static void popVarKind(struct parserCtx *auxil)
{
	intArrayRemoveLast(auxil->varKindsTracker);
}

static void captureField(struct parserCtx *auxil)
{
	const vString *field = fossick (auxil);
	auxil->modifier = peekModifier (auxil);
	makeJavaTag (auxil, vStringValue (field), K_FIELD);
	resetModifier (auxil);
	deactivateSink (auxil);
}

static void captureVariable(struct parserCtx *auxil)
{
	if (intArrayCount (auxil->varKindsTracker) == 0)
		return;

	const vString *var = fossick(auxil);
	javaKind var_kind = intArrayLast (auxil->varKindsTracker);
	if (var_kind == K_FIELD)
		auxil->modifier = peekModifier (auxil);
	makeJavaTag (auxil, vStringValue (var), var_kind);
	resetModifier (auxil);
	deactivateSink (auxil);
}

static void enterEnum (struct parserCtx *auxil)
{
	const vString *enumClass = fossick (auxil);
	enterBlock (auxil, enumClass, K_ENUM);
	deactivateSink (auxil);
}

static void leaveEnum (struct parserCtx *auxil)
{
	leaveBlock (auxil);
}

static void enterEnumConstant (struct parserCtx *auxil)
{
	const vString *enumConstant = fossick (auxil);
	enterBlock (auxil, enumConstant, K_ENUM_CONSTANT);
	deactivateSink (auxil);
}

static void leaveEnumConstant (struct parserCtx *auxil)
{
	leaveBlock (auxil);
}

static void attachInheritsMaybe (struct parserCtx *auxil)
{
	const vString *superList = fossick (auxil);
	if (superList && vStringLength (superList) > 0)
	{
		NestingLevel *n = nestingLevelsGetCurrent (auxil->nlevels);
		if (n)
		{
			int corkIndex = n->corkIndex;

			tagEntryInfo *e;
			e = getEntryInCorkQueue (corkIndex);
			if (e)
			{
				Assert(!e->extensionFields.inheritance);
				e->extensionFields.inheritance = vStringStrdup (superList);
			}
		}
	}
	deactivateSink (auxil);
}

static void captureImportedPackage(struct parserCtx *auxil)
{
	const vString *imported = fossick (auxil);
	makeJavaRefTag (auxil, vStringValue (imported),
					K_PACKAGE, JAVAR_PACKAGE_IMPORTED);
	resetModifier (auxil);
	deactivateSink (auxil);
}

static void findJavaTags (void)
{
	struct parserCtx auxil;

	ctxInit (&auxil);
	pjava_context_t *pctx = pjava_create(&auxil);

	while (pjava_parse(pctx, NULL) && (!auxil.found_syntax_error) );

	pjava_destroy(pctx);
	ctxFini (&auxil);
}

static void java_include_type_argument_handler (const langType language CTAGS_ATTR_UNUSED,
												const char *optname,
												const char *arg)
{
	includeTypeArgument = paramParserBool (arg, includeTypeArgument,
										   optname, "parameter");
}

static parameterHandlerTable JavaParameterHandlerTable [] = {
	{ .name = "includeTypeArgument",
	  .desc = "include type arguments to inheritance field: true or [false]",
	  .handleParameter = java_include_type_argument_handler,
	},
};

extern parserDefinition* NewJavaParser (void)
{
	static const char *const extensions [] = { "java", NULL };
	parserDefinition* def = parserNew ("NewJava");
	def->kindTable  = JavaKinds;
	def->kindCount  = ARRAY_SIZE (JavaKinds);
	def->extensions = extensions;
	def->parser     = findJavaTags;
	def->useCork    = true;
	def->enabled    = 0;
	def->parameterHandlerTable = JavaParameterHandlerTable;
	def->parameterHandlerCount = ARRAY_SIZE(JavaParameterHandlerTable);

	return def;
}
