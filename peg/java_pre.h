/*
 *   Copyright (c) 2018 Masatake YAMATO
 *   Copyright (c) 2018 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Java 1.7.
 */

#include "general.h"  /* must always come first */

#include "options.h"
#include "entry.h"
#include "kind.h"
#include "nestlevel.h"
#include "numarray.h"
#include "parse.h"
#include "param.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"
#include "vstring.h"

#define PCC_GETCHAR(auxil) getcFromInputFile()
#define PCC_MALLCO(auxil,size) eMalloc(size)
#define PCC_REALLOC(auxil,ptr,size) eRealloc(ptr,size)
#define PCC_FREE(auxil,ptr) eFreeNoNullCheck(ptr)
#define PCC_ERROR(auxil) reportError(auxil)

/* Used to index into the JavaKinds table. */
typedef enum {
	JAVAR_PACKAGE_IMPORTED,
} javaPackageRole;

static roleDefinition JavaPackageRoles [] = {
	{ true, "imported", "imported package"},
};

typedef enum {
	K_UNDEFINED = -1,
	K_ANNOTATION, K_CLASS, K_ENUM_CONSTANT, K_FIELD, K_ENUM, K_INTERFACE,
	K_LOCAL, K_METHOD, K_PACKAGE,
} javaKind;

static kindDefinition JavaKinds [] = {
	{ true,  'a', "annotation",    "annotation declarations" },
	{ true,  'c', "class",         "classes"},
	{ true,  'e', "enumConstant",  "enum constants"},
	{ true,  'f', "field",         "fields"},
	{ true,  'g', "enum",          "enum types"},
	{ true,  'i', "interface",     "interfaces"},
	{ false, 'l', "local",         "local variables"},
	{ true,  'm', "method",        "methods"},
	{ true,  'p', "package",       "packages",
	  .referenceOnly = false, ATTACH_ROLES(JavaPackageRoles)},
};

typedef enum {
	JMOD_PUBLIC,
	JMOD_PROTECTED,
	JMOD_PRIVATE,
	JMOD_STATIC,
	JMOD_ABSTRACT,
	JMOD_FINAL,
	JMOD_NATIVE,
	JMOD_SYNCHRONIZED,
	JMOD_TRANSIENT,
	JMOD_VOLATILE,
	JMOD_STRICTFP,
} javaModifier;

typedef enum  {
	S_NAME,
	S_SUPER,
	MAX_SINK_TYPE
} sinkType;

struct parserCtx {
	vString *currentSink;
	vString *sinks[MAX_SINK_TYPE];
	int sinkBlockDepth;
	int offset;
	int endCandidateOffset;
	NestingLevels *nlevels;
	bool inPackage;
	ulongArray *modifiersTracker;
	intArray *varKindsTracker;

	unsigned long modifier;
	bool found_syntax_error;
	int lastPoppedCorkIndex;
	bool looking_for_anon_class;
};

static void enterTypeArguments(struct parserCtx *auxil);
static void leaveTypeArguments(struct parserCtx *auxil);

static void activateSink (struct parserCtx *auxil, sinkType stype);
static void eatIdentifier(struct parserCtx *auxil, const char *const id, int idOffset, int endCandidateOffset);
static void eatRightwing (struct parserCtx *auxil, int wingOffset);
static void eatComma (struct parserCtx *auxil, int commaOffset);
static void eatChar (struct parserCtx *auxil, char c);
static void eatString (struct parserCtx *auxil, const char *const s, size_t len);

static void patchEndline (struct parserCtx *auxil);

static void enterPackage (struct parserCtx *auxil);
static void leavePackageMaybe (struct parserCtx *auxil, int endOffset);

static void enterClass (struct parserCtx *auxil);
static void leaveClass (struct parserCtx *auxil);

static void enterAnonClassMaybe (struct parserCtx *auxil);
static void markAnonClassMaybe (struct parserCtx *auxil);
static void leaveAnonClass (struct parserCtx *auxil);

static void enterInterface (struct parserCtx *auxil);
static void leaveInterface (struct parserCtx *auxil);

static void enterAnnotationDecl (struct parserCtx *auxil);
static void leaveAnnotationDecl (struct parserCtx *auxil);

static void enterMethod (struct parserCtx *auxil);
static void leaveMethod (struct parserCtx *auxil);

static void resetModifier(struct parserCtx *auxil);
static void markModifier(struct parserCtx *auxil, javaModifier jmod);
static void pushModifier(struct parserCtx *auxil);
static void popModifier(struct parserCtx *auxil);

static void captureField(struct parserCtx *auxil);

static void pushVarKind(struct parserCtx *auxil, javaKind kind);
static void popVarKind(struct parserCtx *auxil);
static void captureVariable(struct parserCtx *auxil);

static void enterEnum (struct parserCtx *auxil);
static void leaveEnum (struct parserCtx *auxil);

static void enterEnumConstant (struct parserCtx *auxil);
static void leaveEnumConstant (struct parserCtx *auxil);

static void attachInheritsMaybe (struct parserCtx *auxil);

static void captureImportedPackage (struct parserCtx *auxil);

static void reportError (struct parserCtx *auxil)
{
	auxil->found_syntax_error = true;
	verbose ("NewJava: syntax error\n");
}
