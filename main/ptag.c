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

#include "colprint_p.h"
#include "ctags.h"
#include "debug.h"
#include "entry_p.h"
#include "field.h"
#include "field_p.h"
#include "options_p.h"
#include "parse.h"
#include "parse_p.h"
#include "ptag_p.h"
#include "routines_p.h"
#include "writer_p.h"
#include <string.h>


static bool ptagMakeFormat (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
							const void *data CTAGS_ATTR_UNUSED)
{
	char format [11];
	const char *formatComment = "unknown format";
	const optionValues *opt = data;

	sprintf (format, "%u", opt->tagFileFormat);
	if (opt->tagFileFormat == 1)
		formatComment = "original ctags format";
	else if (opt->tagFileFormat == 2)
		formatComment =
			"extended format; --format=1 will not append ;\" to lines";
	return writePseudoTag (desc, format, formatComment, NULL);
}

static bool ptagMakeHowSorted (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
							   const void *data CTAGS_ATTR_UNUSED)
{
	const optionValues *opt = data;
	return writePseudoTag (desc,
			       opt->sorted == SO_FOLDSORTED ? "2" :
			       (opt->sorted == SO_SORTED ? "1" : "0"),
			       "0=unsorted, 1=sorted, 2=foldcase",
			       NULL);
}

static bool ptagMakeAuthor (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
							const void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
						   AUTHOR_NAME,  "", NULL);
}

static bool ptagMakeProgName (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
							  const void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
						   PROGRAM_NAME,  "Derived from Exuberant Ctags", NULL);
}

static bool ptagMakeProgURL (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
							 const void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc,
						   PROGRAM_URL, "official site", NULL);
}

static bool ptagMakeProgVersion (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
								 const void *data CTAGS_ATTR_UNUSED)
{
	const char* repoinfo = ctags_repoinfo? ctags_repoinfo: "";
	return writePseudoTag (desc, PROGRAM_VERSION, repoinfo, NULL);
}

#ifdef HAVE_ICONV
static bool ptagMakeFileEncoding (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED,
								  const void *data)
{
	const optionValues *opt = data;
	if (! opt->outputEncoding)
		return false;

	return writePseudoTag (desc, opt->outputEncoding, "", NULL);
}
#endif

static bool ptagMakeKindSeparators (ptagDesc *desc, langType language,
									const void *data CTAGS_ATTR_UNUSED)
{
	return makeKindSeparatorsPseudoTags (language, desc);
}

static bool ptagMakeKindDescriptions (ptagDesc *desc, langType language,
									  const void *data CTAGS_ATTR_UNUSED)
{
	return makeKindDescriptionsPseudoTags (language, desc);
}

static void preloadMetaHintForKindDescriptions (ptagDesc *ptag, langType lang,
												const char *rest_part, hintEntry * metaHint)
{
	if (metaHint->file == NULL)
	{
		error (WARNING, "kind name is NULL: %s", metaHint->name);
		return;
	}

	if (lang == LANG_IGNORE)
	{
		error (WARNING, "no lang part: %s", metaHint->name);
		return;
	}

	char kind_letter = metaHint->file [0];
	if (kind_letter == '\0')
	{
		error (WARNING, "kind part is empty");
		return;
	}

	kindDefinition* kdef = getLanguageKindForLetter (lang, kind_letter);
	if (kdef == NULL)
	{
		error (WARNING, "no such kind for letter: %c in %s",
			   kind_letter, getLanguageName (lang));
		return;
	}

	makeLanguageKindAvailableInHint (lang, kdef->id);
}

static bool ptagMakeFieldDescriptions (ptagDesc *desc, langType language,
									   const void *data CTAGS_ATTR_UNUSED)
{
	return makeFieldDescriptionsPseudoTags (language, desc);
}

static void preloadMetaHintForFieldDescriptions (ptagDesc *ptag, langType lang,
												 const char *rest_part, hintEntry * metaHint)
{
	fieldType f;

	if (metaHint->file == NULL)
	{
		error (WARNING, "field name is NULL: %s", metaHint->name);
		return;
	}

	if (lang == LANG_IGNORE)
	{
		f = getFieldTypeForName (metaHint->file);
		if (f == FIELD_UNKNOWN)
		{
			error (WARNING, "unknown field in hint file: %s", metaHint->file);
			return;
		}
	}
	else
	{
		f = getFieldTypeForNameAndLanguage (metaHint->file, lang);
		if (f == FIELD_UNKNOWN)
		{
			error (WARNING, "unknown field in hint file: %s (lang: %s)",
				   metaHint->file, getLanguageName (lang));
			return;
		}
	}
	makeFieldAvailableInHint (f);
}

static bool ptagMakeExtraDescriptions (ptagDesc *desc, langType language,
									   const void *data CTAGS_ATTR_UNUSED)
{
	return makeExtraDescriptionsPseudoTags (language, desc);
}

static bool ptagMakeRoleDescriptions (ptagDesc *desc, langType language,
									  const void *data CTAGS_ATTR_UNUSED)
{
	return makeRoleDescriptionsPseudoTags (language, desc);
}

static bool ptagMakeProcCwd (ptagDesc *desc, langType language,
									   const void *data CTAGS_ATTR_UNUSED)
{
	return writePseudoTag (desc, CurrentDirectory, "", NULL);
}

static ptagDesc ptagDescs [] = {
	{
	  /* The prefix is not "TAG_".
	     Only --output-format=json use this ptag. */
	  false, "JSON_OUTPUT_VERSION",
	  "the version of json output stream format",
	  ptagMakeJsonOutputVersion,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_FILE_FORMAT",
	  "the version of tags file format",
	  ptagMakeFormat,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_FILE_SORTED",
	  "how tags are sorted",
	  ptagMakeHowSorted,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_PROGRAM_AUTHOR",
	  "the author of this ctags implementation",
	  ptagMakeAuthor,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_PROGRAM_NAME",
	  "the name of this ctags implementation",
	  ptagMakeProgName,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_PROGRAM_URL",
	  "the official site URL of this ctags implementation",
	  ptagMakeProgURL,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_PROGRAM_VERSION",
	  "the version of this ctags implementation",
	  ptagMakeProgVersion,
	  NULL,
	  PTAGF_COMMON },
#ifdef HAVE_ICONV
	{ true, "TAG_FILE_ENCODING",
	  "the encoding used in output tags file",
	  ptagMakeFileEncoding,
	  NULL,
	  PTAGF_COMMON },
#endif
	{ false, "TAG_KIND_SEPARATOR",
	  "the separators used in kinds",
	  ptagMakeKindSeparators,
	  NULL,
	  PTAGF_PARSER },
	{ true, "TAG_KIND_DESCRIPTION",
	  "the letters, names and descriptions of enabled kinds in the language",
	  ptagMakeKindDescriptions,
	  preloadMetaHintForKindDescriptions,
	  PTAGF_PARSER },
	{ true, "TAG_FIELD_DESCRIPTION",
	  "the names and descriptions of enabled fields",
	  ptagMakeFieldDescriptions,
	  preloadMetaHintForFieldDescriptions,
	  PTAGF_COMMON|PTAGF_PARSER },
	{ true, "TAG_EXTRA_DESCRIPTION",
	  "the names and descriptions of enabled extras",
	  ptagMakeExtraDescriptions,
	  NULL,
	  PTAGF_COMMON|PTAGF_PARSER },
	{ true, "TAG_ROLE_DESCRIPTION",
	  "the names and descriptions of enabled roles",
	  ptagMakeRoleDescriptions,
	  NULL,
	  PTAGF_PARSER },
	{ true, "TAG_OUTPUT_MODE",
	  "the output mode: u-ctags or e-ctags",
	  ptagMakeCtagsOutputMode,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_OUTPUT_FILESEP",
	  "the separator used in file name (slash or backslash)",
	  ptagMakeCtagsOutputFilesep,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_PATTERN_LENGTH_LIMIT",
	  "the limit of pattern length",
	  ptagMakePatternLengthLimit,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_PROC_CWD",
	  "the current working directory of the tags generator",
	  ptagMakeProcCwd,
	  NULL,
	  PTAGF_COMMON },
	{ true, "TAG_OUTPUT_EXCMD",
	  "the excmd: number, pattern, mixed, or combine",
	  ptagMakeCtagsOutputExcmd,
	  NULL,
	  PTAGF_COMMON },
};

extern bool makePtagIfEnabled (ptagType type, langType language, const void *data)
{
	ptagDesc *desc;

	Assert (0 <= type && type < PTAG_COUNT);

	desc = ptagDescs + type;
	if (desc->enabled)
		return desc->makeTag (desc, language, data);
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
	return pdesc->flags & PTAGF_COMMON;
}

extern bool isPtagParserSpecific (ptagType type)
{
	ptagDesc* pdesc = getPtagDesc (type);
	return pdesc->flags & PTAGF_PARSER;
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

static void preloadMetaHintOnePerParser (hintEntry *metaHint, ptagType type,
										 const char *lang_name)
{
	size_t sep_len = strlen (PSEUDO_TAG_SEPARATOR);
	const char *next_sep = strstr (lang_name, PSEUDO_TAG_SEPARATOR);
	langType lang = getNamedLanguage (lang_name, next_sep? next_sep - lang_name: 0);

	if (lang == LANG_IGNORE)
	{
		vString *s = (next_sep)
			? vStringNewNInit (lang_name, next_sep - lang_name)
			: vStringNewInit (lang_name);
		error (WARNING, "unknown language in hint file: %s", vStringValue (s));
		vStringDelete (s);
	}

	initializeParser (lang);
	const char *rest_part = next_sep? next_sep + sep_len: NULL;
	if (ptagDescs [type].preloadMetaHint)
		ptagDescs [type].preloadMetaHint (ptagDescs + type, lang, rest_part, metaHint);
	parserPreloadMetaHint (type, lang, rest_part, metaHint);
}

static void preloadMetaHintOne (hintEntry *metaHint, ptagType type,
								const char *hint_rest_name)
{
	ptagFlag flags = ptagDescs [type].flags;
	size_t sep_len = strlen (PSEUDO_TAG_SEPARATOR);
	bool sep = (strncmp (hint_rest_name, PSEUDO_TAG_SEPARATOR, sep_len) == 0);

	Assert (flags & (PTAGF_PARSER | PTAGF_COMMON));

	if (sep && (flags & PTAGF_PARSER))
	{
		preloadMetaHintOnePerParser (metaHint, type, hint_rest_name + sep_len);
		return;
	}

	if (flags & PTAGF_COMMON)
	{
		if (ptagDescs [type].preloadMetaHint)
			ptagDescs [type].preloadMetaHint (ptagDescs + type, LANG_IGNORE, NULL, metaHint);
		parserPreloadMetaHint (type, LANG_IGNORE, NULL, metaHint);
		return;
	}

	error (WARNING, "no language part found: %s", metaHint->name);
}

extern void preloadMetaHint (hintEntry *metaHint)
{
	size_t prefix_len = strlen (PSEUDO_TAG_PREFIX);

	if (strncmp (metaHint->name, PSEUDO_TAG_PREFIX, prefix_len) != 0)
	{
		error (WARNING, "pseudo tag doesn't start with %s: %s",
			   PSEUDO_TAG_PREFIX, metaHint->name);
		return;
	}

	bool handled = false;
	for (int i = 0; i < PTAG_COUNT; i++)
	{
		size_t name_len = strlen (ptagDescs [i].name);
		if (strncmp (ptagDescs [i].name, metaHint->name + prefix_len, name_len) == 0)
		{
			const char *hint_rest_name = metaHint->name + prefix_len + name_len;
			preloadMetaHintOne (metaHint, i, hint_rest_name);
			handled = true;
			break;
		}
	}
	if (!handled)
		verbose ("handled pseudo tag in hint file: %s\n", metaHint->name);
}
