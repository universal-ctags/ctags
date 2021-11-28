/*
*   Copyright (c) 2019, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/
#ifndef CTAGS_PARSER_PERL_H
#define CTAGS_PARSER_PERL_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"

typedef struct sPerlSubparser perlSubparser;

enum PerlModuleRoleType {
	ROLE_PERL_MODULE_USED,
	ROLE_PERL_MODULE_UNUSED,
};

enum PerlKindType {
	KIND_PERL_NONE = -1,
	KIND_PERL_CONSTANT,
	KIND_PERL_FORMAT,
	KIND_PERL_LABEL,
	KIND_PERL_PACKAGE,
	KIND_PERL_SUBROUTINE,
	KIND_PERL_SUBROUTINE_DECLARATION,
	KIND_PERL_MODULE,
	KIND_PERL_HEREDOCMARKER,
};

struct sPerlSubparser {
	subparser subparser;
	void (* findingQuotedWordNotify) (perlSubparser *,
									  int moduleIndex,
									  const char *qwd);
	void (* enteringPodNotify) (perlSubparser *);
	void (* leavingPodNotify)    (perlSubparser *);
};

#endif	/* CTAGS_PARSER_PERL_H */
