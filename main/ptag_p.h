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
#ifndef CTAGS_MAIN_PTAG_PRIVATE_H
#define CTAGS_MAIN_PTAG_PRIVATE_H

#include "general.h"

#include "ptag.h"
#include "types.h"
#include "hint.h"

typedef enum ePtagFlag {
	/* use isPtagCommonInParsers() for testing. */
	PTAGF_COMMON = 1 << 0,
	/* use isPtagParserSpecific for testing.
	 * PSEUDO_TAG_SEPARATOR is used for printing. */
	PTAGF_PARSER = 1 << 1,
} ptagFlag;

struct sPtagDesc {
	bool enabled;
	const char* name;
	const char* description;  /* displayed in --list-pseudo-tags output */

	/* For making ptags for common in parsers, LANG_IGNOR is for the second
	 * argument and a pointer for optionValues type value for the third argument
	 * are passed.
	 *
	 * For parser specific ptags, the pointer for parserObject
	 * of the parser is passed as the thrid argument.
	 */
	bool (* makeTag) (ptagDesc *, langType, const void *);

	/* The hint loader calls this method when the loader reads a
	 * pseudo tag from a hint file.
	 *
	 * pseudo tag associated with a ptag desc having PTAGF_PARSER
	 * flag have following form:
	 *
	 *     !_TAG_KIND_DESCRIPTION!C	g,enum	/enumeration names/
	 *
	 * The language name, "C" in the above example, in the pseudo tag
	 * is extracted and converted to a langType value. The value is
	 * passed to the method as the second argument. LANG_AUTO is
	 * passed instead for ptag descs not having PTAGF_PARSER flag.
	 *
	 * Some pseudo tags have more complicated form like:
	 *
	 *     !_TAG_ROLE_DESCRIPTION!C!header	local	/local header/
	 *
	 * The part after the language name, "header" in the above example,
	 * is passed to the method as the third argument. NULL is passed
	 * instead if no such part is.
	 */
	void (* preloadMetaHint) (ptagDesc *, langType, const char *, hintEntry *);

	ptagFlag flags;
};

extern bool makePtagIfEnabled (ptagType type, langType language, const void *data);
extern ptagDesc* getPtagDesc (ptagType type);
extern ptagType  getPtagTypeForName (const char *name);
extern void printPtags (bool withListHeader, bool machinable, FILE *fp);
extern bool isPtagEnabled (ptagType type);
extern bool isPtagCommonInParsers (ptagType type);
extern bool isPtagParserSpecific (ptagType type);
extern bool enablePtag (ptagType type, bool state);

extern void preloadMetaHint (hintEntry *metaHint);

#endif	/* CTAGS_MAIN_PTAG_PRIVATE_H */
