/*
 *
 *  Copyright (c) 1996-2002, Darren Hiebert
 *  Copyright (c) 2016, Red Hat, Inc.
 *  Copyright (c) 2016, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"  /* must always come first */

#include "colprint.h"
#include "ctags.h"
#include "debug.h"
#include "options.h"
#include "parse.h"
#include "ptag.h"
#include "writer.h"
#include <string.h>


static bool ptagMakeFormat (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	char format [11];
	const char *formatComment = "unknown format";

	sprintf (format, "%u", Option.tagFileFormat);
	if (Option.tagFileFormat == 1)
		formatComment = "original ctags format";
	else if (Option.tagFileFormat == 2)
		formatComment =
			"extended format; --format=1 will not append ;\" to lines";
	return writePseudoTag (desc, format, formatComment, NULL);
}

static bool ptagMakeHowSorted (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
			       Option.sorted == SO_FOLDSORTED ? "2" :
			       (Option.sorted == SO_SORTED ? "1" : "0"),
			       "0=unsorted, 1=sorted, 2=foldcase",
			       NULL);
}

static bool ptagMakeAuthor (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
						   AUTHOR_NAME,  "", NULL);
}

static bool ptagMakeProgName (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
						   PROGRAM_NAME,  "Derived from Exuberant Ctags", NULL);
}

static bool ptagMakeProgURL (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
						   PROGRAM_URL, "official site", NULL);
}

static bool ptagMakeProgVersion (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	const char* repoinfo = ctags_repoinfo? ctags_repoinfo: "";
	return writePseudoTag (desc, PROGRAM_VERSION, repoinfo, NULL);
}

#ifdef HAVE_ICONV
static bool ptagMakeFileEncoding (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	if (! Option.outputEncoding)
		return false;

	return writePseudoTag (desc, Option.outputEncoding, "", NULL);
}
#endif

static bool ptagMakeKindSeparators (ptagDesc *desc, void *data)
{
	langType *language = data;

	return makeKindSeparatorsPseudoTags (*language, desc);
}

static bool ptagMakeKindDescriptions (ptagDesc *desc, void *data)
{
	langType *language = data;
	return makeKindDescriptionsPseudoTags (*language, desc);
}

static ptagDesc ptagDescs [] = {
	{
	  /* The prefix is not "TAG_".
	     Only --output-format=json use this ptag. */
	  false, "JSON_OUTPUT_VERSION",
	  "the version of json output stream format",
	  ptagMakeJsonOutputVersion,
	  true },
	{ true, "TAG_FILE_FORMAT",
	  "the version of tags file format",
	  ptagMakeFormat,
	  true },
	{ true, "TAG_FILE_SORTED",
	  "how tags are sorted",
	  ptagMakeHowSorted,
	  true },
	{ true, "TAG_PROGRAM_AUTHOR",
	  "the author of this ctags implementation",
	  ptagMakeAuthor,
	  true },
	{ true, "TAG_PROGRAM_NAME",
	  "the name of this ctags implementation",
	  ptagMakeProgName,
	  true },
	{ true, "TAG_PROGRAM_URL",
	  "the official site URL of this ctags implementation",
	  ptagMakeProgURL,
	  true },
	{ true, "TAG_PROGRAM_VERSION",
	  "the version of this ctags implementation",
	  ptagMakeProgVersion,
	  true },
#ifdef HAVE_ICONV
	{ true, "TAG_FILE_ENCODING",
	  "the encoding used in output tags file",
	  ptagMakeFileEncoding,
	  true },
#endif
	{ false, "TAG_KIND_SEPARATOR",
	  "the separators used in kinds",
	  ptagMakeKindSeparators,
	  false },
	{ false, "TAG_KIND_DESCRIPTION",
	  "the letters, names and descriptions of kinds in a parser",
	  ptagMakeKindDescriptions,
	  false },
	{ true, "TAG_OUTPUT_MODE",
	  "the output mode: u-ctags or e-ctags",
	  ptagMakeCtagsOutputMode,
	  true },
};

extern bool makePtagIfEnabled (ptagType type, void *data)
{
	ptagDesc *desc;

	Assert (0 <= type && type < PTAG_COUNT);

	desc = ptagDescs + type;
	if (desc->enabled)
		return desc->makeTag (desc, data);
	else
		return false;
}

extern bool isPtagEnabled (ptagType type)
{
	ptagDesc *desc;

	Assert (0 <= type && type < PTAG_COUNT);

	desc = ptagDescs + type;
	return desc->enabled;

}

extern bool enablePtag (ptagType type, bool state)
{
	bool oldstate;
	ptagDesc *desc;

	Assert (0 <= type && type < PTAG_COUNT);

	desc = ptagDescs + type;
	oldstate = desc->enabled;
	desc->enabled = state;
	return oldstate;
}

extern ptagDesc* getPtagDesc (ptagType type)
{
	if (type == PTAG_UNKNOWN
	    || type >= PTAG_COUNT)
		return NULL;

	return ptagDescs + type;
}

extern ptagType getPtagTypeForName (const char *name)
{
	int i;

	Assert (name);
	for (i = 0; i < PTAG_COUNT; i++)
		if (strcmp (ptagDescs [i].name, name) == 0)
			return i;
	return PTAG_UNKNOWN;
}

extern bool isPtagCommonInParsers  (ptagType type)
{
	ptagDesc* pdesc = getPtagDesc (type);
	return pdesc->commonInParsers;
}

static int ptagCompare (struct colprintLine *a, struct colprintLine *b)
{
	const char *a_name = colprintLineGetColumn (a, 0);
	const char *b_name = colprintLineGetColumn (b, 0);
	return strcmp(a_name, b_name);
}

extern void printPtags (bool withListHeader, bool machinable, FILE *fp)
{
	struct colprintTable *table = colprintTableNew ("L:NAME",
													"L:ENABLED",
													"L:DESCRIPTION",
													NULL);
	for (unsigned int i = 0; i < PTAG_COUNT; i++)
	{
		struct colprintLine *line = colprintTableGetNewLine (table);
		colprintLineAppendColumnCString (line, ptagDescs[i].name);
		colprintLineAppendColumnCString (line, ptagDescs[i].enabled
										 ? "on"
										 : "off");
		colprintLineAppendColumnCString (line, ptagDescs[i].description);
	}

	colprintTableSort (table, ptagCompare);
	colprintTablePrint (table, 0, withListHeader, machinable, fp);
	colprintTableDelete (table);
}
