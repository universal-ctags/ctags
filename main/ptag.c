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

#include "ctags.h"
#include "debug.h"
#include "options.h"
#include "parse.h"
#include "ptag.h"
#include "output.h"
#include <string.h>


static boolean writePseudoTagForXcmdData (ptagDesc *desc,
					  struct ptagXcmdData *pdata)
{
	return writePseudoTag (desc,
			       pdata->fileName,  pdata->pattern, pdata->language);
}

static boolean ptagMakeFormat (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
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

static boolean ptagMakeHowSorted (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
			       Option.sorted == SO_FOLDSORTED ? "2" :
			       (Option.sorted == SO_SORTED ? "1" : "0"),
			       "0=unsorted, 1=sorted, 2=foldcase",
			       NULL);
}

static boolean ptagMakeAuthor (ptagDesc *desc, void *data)
{
	struct ptagXcmdData *pdata = data;

	if (pdata)
		return writePseudoTagForXcmdData (desc, data);
	else
		return writePseudoTag (desc,
				       AUTHOR_NAME,  "", NULL);
}

static boolean ptagMakeProgName (ptagDesc *desc, void *data)
{
	struct ptagXcmdData *pdata = data;

	if (pdata)
		return writePseudoTagForXcmdData (desc, data);
	else
		return writePseudoTag (desc,
				PROGRAM_NAME,  "Derived from Exuberant Ctags", NULL);
}

static boolean ptagMakeProgURL (ptagDesc *desc, void *data)
{
	struct ptagXcmdData *pdata = data;

	if (pdata)
		return writePseudoTagForXcmdData (desc, data);
	else
		return writePseudoTag (desc,
				       PROGRAM_URL, "official site", NULL);
}

static boolean ptagMakeProgVersion (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	const char* repoinfo = ctags_repoinfo? ctags_repoinfo: "";
	return writePseudoTag (desc, PROGRAM_VERSION, repoinfo, NULL);
}

#ifdef HAVE_ICONV
static boolean ptagMakeFileEncoding (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED)
{
	if (! Option.outputEncoding)
		return FALSE;

	return writePseudoTag (desc, Option.outputEncoding, "", NULL);
}
#endif

static boolean ptagMakeKindSeparators (ptagDesc *desc, void *data)
{
	langType *language = data;

	return makeKindSeparatorsPseudoTags (*language, desc);
}

static boolean ptagMakeKindDescriptions (ptagDesc *desc, void *data)
{
	langType *language = data;
	return makeKindDescriptionsPseudoTags (*language, desc);
}

static ptagDesc ptagDescs [] = {
	{
	  /* The prefix is not "TAG_".
	     Only --output-format=json use this ptag. */
	  FALSE, "JSON_OUTPUT_VERSION",
	  "the version of json output stream format",
	  ptagMakeJsonOutputVersion,
	  TRUE },
	{ TRUE, "TAG_FILE_FORMAT",
	  "the version of tags file format",
	  ptagMakeFormat,
	  TRUE },
	{ TRUE, "TAG_FILE_SORTED",
	  "how tags are sorted",
	  ptagMakeHowSorted,
	  TRUE },
	{ TRUE, "TAG_PROGRAM_AUTHOR",
	  "the author of this ctags implementation",
	  ptagMakeAuthor,
	  TRUE },
	{ TRUE, "TAG_PROGRAM_NAME",
	  "the name of this ctags implementation",
	  ptagMakeProgName,
	  TRUE },
	{ TRUE, "TAG_PROGRAM_URL",
	  "the official site URL of this ctags implementation",
	  ptagMakeProgURL,
	  TRUE },
	{ TRUE, "TAG_PROGRAM_VERSION",
	  "the version of this ctags implementation",
	  ptagMakeProgVersion,
	  TRUE },
#ifdef HAVE_ICONV
	{ TRUE, "TAG_FILE_ENCODING",
	  "the encoding used in output tags file",
	  ptagMakeFileEncoding,
	  TRUE },
#endif
	{ FALSE, "TAG_KIND_SEPARATOR",
	  "the separators used in kinds",
	  ptagMakeKindSeparators,
	  FALSE },
	{ FALSE, "TAG_KIND_DESCRIPTION",
	  "the letters, names and descriptions of kinds in a parser",
	  ptagMakeKindDescriptions,
	  FALSE },
};

extern boolean makePtagIfEnabled (ptagType type, void *data)
{
	ptagDesc *desc;

	Assert (0 <= type && type < PTAG_COUNT);

	desc = ptagDescs + type;
	if (desc->enabled)
		return desc->makeTag (desc, data);
	else
		return FALSE;
}

extern boolean isPtagEnabled (ptagType type)
{
	ptagDesc *desc;

	Assert (0 <= type && type < PTAG_COUNT);

	desc = ptagDescs + type;
	return desc->enabled;

}

extern boolean enablePtag (ptagType type, boolean state)
{
	boolean oldstate;
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

extern boolean isPtagCommonInParsers  (ptagType type)
{
	ptagDesc* pdesc = getPtagDesc (type);
	return pdesc->commonInParsers;
}

extern void printPtag (ptagType type)
{
	printf("%s\t%s\t%s\n",
	       ptagDescs[type].name,
	       ptagDescs[type].description? ptagDescs[type].description: "NONE",
	       ptagDescs[type].enabled? "on": "off");
}
