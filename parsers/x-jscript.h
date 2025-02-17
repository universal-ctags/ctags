/*
 * Copyright (c) 2003, Darren Hiebert
 *
 * This source code is released for free distribution under the terms of the
 * GNU General Public License version 2 or (at your option) any later version.
 *
 * This module contains functions for generating tags for JavaScript language
 * files.
 */

#ifndef CTAGS_JSCRIPT_H
#define CTAGS_JSCRIPT_H

typedef enum {
	JSTAG_FUNCTION,
	JSTAG_CLASS,
	JSTAG_METHOD,
	JSTAG_PROPERTY,
	JSTAG_CONSTANT,
	JSTAG_VARIABLE,
	JSTAG_GENERATOR,
	JSTAG_GETTER,
	JSTAG_SETTER,
	JSTAG_FIELD,
	JSTAG_COUNT
} jsKind;

typedef enum {
	JSTAG_FUNCTIONRoleFOREIGNDECL
} JSTAGFunctionRole;

/* Skip over { ... } */
void javaScriptSkipObjectExpression (void);

#endif
