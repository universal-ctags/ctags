/*
*
*   Copyright (c) 2015, Red Hat, Inc.
*   Copyright (c) 2015, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#ifndef CTAGS_SYSTEMDUNIT_H
#define CTAGS_SYSTEMDUNIT_H

#include "general.h"

typedef enum {
	SYSTEMD_UNIT_KIND,
} systemdUnitKind;

typedef enum {
	SYSTEMD_UNIT_Requires_ROLE,
	SYSTEMD_UNIT_Wants_ROLE,
	SYSTEMD_UNIT_After_ROLE,
	SYSTEMD_UNIT_Before_ROLE,
	SYSTEMD_UNIT_RequiredBy_ROLE,
	SYSTEMD_UNIT_WantedBy_ROLE,
	SYSTEMD_UNIT_FOREIGNLANG_ROLE,
} systemdUnitRole;

#endif	/* CTAGS_SYSTEMDUNIT_H */
