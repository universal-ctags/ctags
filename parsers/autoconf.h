/*
 *   Copyright (c) 2011, Colomban Wendling <colomban@geany.org>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   Autoconf parser interface exported to the other parsers
 */

#ifndef CTAGS_AUTOCONF_H
#define CTAGS_AUTOCONF_H

#include "general.h"

typedef enum {
	AUTOCONF_PACKAGE_KIND,
	AUTOCONF_TEMPLATE_KIND,
	AUTOCONF_MACRO_KIND,
	AUTOCONF_OPTWITH_KIND,
	AUTOCONF_OPTENABLE_KIND,
	AUTOCONF_SUBST_KIND,
	AUTOCONF_CONDITION_KIND,
	AUTOCONF_DEFINITION_KIND,
} autoconfKind;

typedef enum {
	AUTOCONF_OPTWITH_CMDLINE_ROLE,
} autoconfOptwithRole;

typedef enum {
	AUTOCONF_OPTENABLE_CMDLINE_ROLE,
} autoconfOptenableRole;

#endif	/* CTAGS_AUTOCONF_H */
