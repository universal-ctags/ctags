/*
 *
 *  Copyright (c) 2016, Red Hat, Inc.
 *  Copyright (c) 2016, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"
#include "param.h"
#include "options.h"

#define PR_PARAM_WIDTH_NAME         8
#define PR_PARAM_WIDTH_DESCRIPTION 30

#define MAKE_PARAM_FMT(PREFIX)					\
	PREFIX										\
	PR_PARAM_FMT (NAME,s)						\
	" "										    \
	PR_PARAM_FMT (DESCRIPTION,s)				\
	"\n"

extern void printParameterListHeader (bool indent, bool tabSeparated)
{
#define PARAM_HEADER_COMMON_FMT MAKE_PARAM_FMT("%s")

	const char *fmt = tabSeparated
		? "%s%s%s\t%s\n"
		: (indent
		   ? PR_PARAM_FMT (LANG,s) PARAM_HEADER_COMMON_FMT
		   : "%s" PARAM_HEADER_COMMON_FMT)
		;
	printf (fmt,
			(indent? "#PARSER": ""),
			(indent? (tabSeparated? "\t": " "): ""),
			"NAME",
			"DESCRIPTION");
}

extern void printParameter (const parameterHandlerTable *const paramHandler, bool indent, bool tabSeparated)
{
#define PARAM_FMT MAKE_PARAM_FMT("")

	printf ((tabSeparated
			 ? "%s%s\t%s\n"
			 : "%s" PARAM_FMT),
			(indent? (tabSeparated? "\t": " "): ""),
			paramHandler->name,
			paramHandler->desc);

}
