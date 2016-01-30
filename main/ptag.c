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
#include "ptag.h"

static void writePseudoTagForXcmdData (ptagDesc *desc,
				       struct ptagXcmdData *pdata)
{
	writePseudoTag (desc,
			pdata->fileName,  pdata->pattern, pdata->language);
}

static boolean ptagMakeFormat (ptagDesc *desc, void *data __unused__)
{
	char format [11];
	const char *formatComment = "unknown format";

	sprintf (format, "%u", Option.tagFileFormat);
	if (Option.tagFileFormat == 1)
		formatComment = "original ctags format";
	else if (Option.tagFileFormat == 2)
		formatComment =
			"extended format; --format=1 will not append ;\" to lines";
	writePseudoTag (desc, format, formatComment, NULL);

	return TRUE;
}

static boolean ptagMakeHowSorted (ptagDesc *desc, void *data __unused__)
{
	writePseudoTag (desc,
			Option.sorted == SO_FOLDSORTED ? "2" :
			(Option.sorted == SO_SORTED ? "1" : "0"),
			"0=unsorted, 1=sorted, 2=foldcase",
			NULL);
	return TRUE;
}

static boolean ptagMakeAuthor (ptagDesc *desc, void *data)
{
	struct ptagXcmdData *pdata = data;

	if (pdata)
		writePseudoTagForXcmdData (desc, data);
	else
		writePseudoTag (desc,
				AUTHOR_NAME,  "", NULL);
	return TRUE;
}

static boolean ptagMakeProgName (ptagDesc *desc, void *data)
{
	struct ptagXcmdData *pdata = data;

	if (pdata)
		writePseudoTagForXcmdData (desc, data);
	else
		writePseudoTag (desc,
				PROGRAM_NAME,  "Derived from Exuberant Ctags", NULL);
	return TRUE;
}

static boolean ptagMakeProgURL (ptagDesc *desc, void *data)
{
	struct ptagXcmdData *pdata = data;

	if (pdata)
		writePseudoTagForXcmdData (desc, data);
	else
		writePseudoTag (desc,
				PROGRAM_URL, "official site", NULL);
	return TRUE;
}

static boolean ptagMakeProgVersion (ptagDesc *desc, void *data __unused__)
{
	const char* repoinfo = ctags_repoinfo? ctags_repoinfo: "";
	writePseudoTag (desc, PROGRAM_VERSION, repoinfo, NULL);
	return TRUE;
}

#ifdef HAVE_ICONV
static boolean ptagMakeFileEncoding (ptagDesc *desc, void *data __unused__)
{
	if (! Option.outputEncoding)
		return FALSE;

	writePseudoTag (desc, Option.outputEncoding, "", NULL);
	return TRUE;
}
#endif

static ptagDesc ptagDescs [] = {
	{ TRUE, "TAG_FILE_FORMAT",
	  "the version of tags file format",
	  ptagMakeFormat },
	{ TRUE, "TAG_FILE_SORTED",
	  "how tags are sorted",
	  ptagMakeHowSorted },
	{ TRUE, "TAG_PROGRAM_AUTHOR",
	  "the author of this ctags implementation",
	  ptagMakeAuthor },
	{ TRUE, "TAG_PROGRAM_NAME",
	  "the name of this ctags implementation",
	  ptagMakeProgName },
	{ TRUE, "TAG_PROGRAM_URL",
	  "the official site URL of this ctags implementation",
	  ptagMakeProgURL },
	{ TRUE, "TAG_PROGRAM_VERSION",
	  "the version of this ctags implementation",
	  ptagMakeProgVersion },
#ifdef HAVE_ICONV
	{ TRUE, "TAG_FILE_ENCODING",
	  "the encoding used in output tags file",
	  ptagMakeFileEncoding },
#endif
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
