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
#include "routines.h"
#include "xtag.h"

static boolean isPseudoTagsEnabled (xtagDesc *pdesc)
{
	return ! isDestinationStdout ();
}

static xtagDesc xtagDescs [] = {
	{ TRUE, 'F',
	  "Include tags of file scope" },
	{ FALSE, 'f',
	  "Include an entry for the base file name of every input file"},
	{ FALSE, 'p',
	  "Include pseudo tags",
	  isPseudoTagsEnabled},
	{ FALSE, 'q',
	  "Include an extra class-qualified tag entry for each tag"},
	{ FALSE, 'r',
	  "Include reference tags"},
	{ FALSE, '.',
	  "Do the similar to the f extra flag but the entry addresses the end line"},
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

static void printXtag (xtagType i)
{
	printf("%c\t%s\t%s\n",
	       xtagDescs[i].letter,
	       xtagDescs[i].description? xtagDescs[i].description: "NONE",
	       getXtagDesc (i)->enabled? "on": "off");
}

extern void printXtags (void)
{
	int i;

	for (i = 0; i < ARRAY_SIZE (xtagDescs); i++)
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
