/*
 *
 *  Copyright (c) 2015, Red Hat, Inc.
 *  Copyright (c) 2015, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"  /* must always come first */
#include "debug.h"
#include "main.h"
#include "options.h"
#include "routines.h"
#include "xtag.h"

static boolean isPseudoTagsEnabled (xtagDesc *pdesc __unused__)
{
	return ! isDestinationStdout ();
}

static xtagDesc xtagDescs [] = {
	{ TRUE, 'F',  "fileScope",
	  "Include tags of file scope" },
	{ FALSE, 'f', "inputFile",
	  "Include an entry for the base file name of every input file"},
	{ FALSE, 'p', "pseudo",
	  "Include pseudo tags",
	  isPseudoTagsEnabled},
	{ FALSE, 'q', "qualified",
	  "Include an extra class-qualified tag entry for each tag"},
	{ FALSE, 'r', "reference",
	  "Include reference tags"},
	{ FALSE, '.', "inputFileWithEndline",
	  "Similar to the f extra flag but the entry addresses the end line"},
};

extern xtagDesc* getXtagDesc (xtagType type)
{
	Assert ((0 <= type) && (type < XTAG_COUNT));
	return xtagDescs + type;
}

extern xtagType  getXtagTypeForOption (char letter)
{
	int i;

	for (i = 0; i < XTAG_COUNT; i++)
	{
		if (xtagDescs [i].letter == letter)
			return i;
	}
	return XTAG_UNKNOWN;
}


#define PR_XTAG_WIDTH_LETTER     7
#define PR_XTAG_WIDTH_NAME      22
#define PR_XTAG_WIDTH_ENABLED   7
#define PR_XTAG_WIDTH_DESC      30

#define PR_XTAG_STR(X) PR_XTAG_WIDTH_##X
#define PR_XTAG_FMT(X,T) "%-" STRINGIFY(PR_XTAG_STR(X)) STRINGIFY(T)
#define MAKE_XTAG_FMT(LETTER_SPEC)		\
	PR_XTAG_FMT (LETTER,LETTER_SPEC)	\
	" "					\
	PR_XTAG_FMT (NAME,s)			\
	" "					\
	PR_XTAG_FMT (ENABLED,s)			\
	" "					\
	PR_XTAG_FMT (DESC,s)			\
	"\n"

static void printXtag (xtagType i)
{
	printf((Option.machinable? "%c\t%s\t%s\t%s\n": MAKE_XTAG_FMT(c)),
	       xtagDescs[i].letter,
	       xtagDescs[i].name,
	       getXtagDesc (i)->enabled? "TRUE": "FALSE",
	       xtagDescs[i].description? xtagDescs[i].description: "NONE");
}

extern void printXtags (void)
{
	unsigned int i;

	if (Option.withListHeader)
		printf ((Option.machinable? "%s\t%s\t%s\t%s\n": MAKE_XTAG_FMT(s)),
			"#LETTER", "NAME", "ENABLED", "DESCRIPTION");

	for (i = 0; i < XTAG_COUNT; i++)
		printXtag (i);
}

extern boolean isXtagEnabled (xtagType type)
{
	xtagDesc* desc = getXtagDesc (type);

	Assert (desc);

	if (desc->isEnabled)
		return desc->isEnabled (desc);
	else
		return desc->enabled;
}

extern boolean enableXtag (xtagType type, boolean state)
{
	boolean old;
	xtagDesc* desc = getXtagDesc (type);

	Assert (desc);

	old = isXtagEnabled (type);
	desc->enabled = state;
	desc->isEnabled = NULL;;

	return old;
}

const char* getXtagName (xtagType type)
{
	xtagDesc* desc = getXtagDesc (type);
	if (desc)
		return desc->name;
	else
		return NULL;
}
