/*
*   Copyright (c) 2019, Jiri Techet
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Provides a simple application using ctags as a library and using the same
*   set of ctags functions as the Geany editor
*/

#include "general.h"  /* must always come first */

#include "types.h"
#include "routines.h"
#include "mio.h"
#include "error_p.h"
#include "writer_p.h"
#include "parse_p.h"
#include "options_p.h"
#include "trashbox_p.h"
#include "field_p.h"
#include "xtag_p.h"
#include "entry_p.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>

static int writeEntry (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag, void *clientData);
static void rescanFailed (tagWriter *writer, unsigned long validTagNum, void *clientData);

/* we need to be able to provide a custom writer using which we collect the tags */
tagWriter customWriter = {
	.writeEntry = writeEntry,
	.writePtagEntry = NULL,
	.preWriteEntry = NULL,
	.postWriteEntry = NULL,
	.rescanFailedEntry = rescanFailed,
	.treatFieldAsFixed = NULL,
	.defaultFileName = "tags_file_which_should_never_appear_anywhere",
	.private = NULL,
};


/* we need to be able to provide an error printer which doesn't crash Geany on error */
static bool nofatalErrorPrinter (const errorSelection selection,
					  const char *const format,
					  va_list ap, void *data CTAGS_ATTR_UNUSED)
{
	fprintf (stderr, "%s: ", (selection & WARNING) ? "Warning: " : "Error");
	vfprintf (stderr, format, ap);
	if (selection & PERROR)
#ifdef HAVE_STRERROR
		fprintf (stderr, " : %s", strerror (errno));
#else
		perror (" ");
#endif
	fputs ("\n", stderr);

	return false;
}


/* we need to be able to enable all kinds for all languages (some are disabled by default) */
static void enableAllLangKinds()
{
	unsigned int lang;

	for (lang = 0; lang < countParsers(); lang++)
	{
		unsigned int kindNum = countLanguageKinds(lang);
		unsigned int kind;

		for (kind = 0; kind < kindNum; kind++)
		{
			kindDefinition *def = getLanguageKind(lang, kind);
			enableKind(def, true);
		}
	}
}


/* we need to be able to initialize ctags like in main() but skipping some things */
static void ctagsInit(void)
{
	initDefaultTrashBox ();

	setErrorPrinter (nofatalErrorPrinter, NULL);
	setTagWriter (WRITER_CUSTOM, &customWriter);

	checkRegex ();
	initFieldObjects ();
	initXtagObjects ();

	initializeParsing ();
	initOptions ();
	initRegexOptscript ();

	/* make sure all parsers are initialized */
	initializeParser (LANG_AUTO);

	/* change default value which is false */
	enableXtag(XTAG_TAGS_GENERATED_BY_GUEST_PARSERS, true);
	enableXtag(XTAG_REFERENCE_TAGS, true);

	/* some kinds we are interested in are disabled by default */
	enableAllLangKinds();
}


/* we need to be able to get a name of a given language */
static const char *ctagsGetLangName(int lang)
{
	return getLanguageName(lang);
}


/* we need to be able to get an int representing a given language */
static int ctagsGetNamedLang(const char *name)
{
	return getNamedLanguage(name, 0);
}


/* we need to be able to get kind letters provided by a given language */
static const char *ctagsGetLangKinds(int lang)
{
	unsigned int kindNum = countLanguageKinds(lang);
	static char kinds[257];
	unsigned int i;

	for (i = 0; i < kindNum; i++)
		kinds[i] = getLanguageKind(lang, i)->letter;
	kinds[i] = '\0';

	return kinds;
}


/* we need to be able to get kind name from a kind letter for a given language */
static const char *ctagsGetKindName(char kind, int lang)
{
	kindDefinition *def = getLanguageKindForLetter (lang, kind);
	return def ? def->name : "unknown";
}


/* we need to be able to get kind letter from a kind name for a given language */
static char ctagsGetKindFromName(const char *name, int lang)
{
	kindDefinition *def = getLanguageKindForName (lang, name);
	return def ? def->letter : '-';
}


/* we need to be able to get kind letter from a kind index */
static char ctagsGetKindFromIndex(int index, int lang)
{
	return getLanguageKind(lang, index)->letter;
}


/* we need to be able to get the number of parsers */
static unsigned int ctagsGetLangCount(void)
{
	return countParsers();
}

/*******************************************************************************
 * So let's just use what we have for our great client...
 ******************************************************************************/

/* our internal tag representation - this is all the tag information we use in Geany */
typedef struct {
	char *name;
	char *signature;
	char *scopeName;
	char *inheritance;
	char *varType;
	char *access;
	char *implementation;
	char kindLetter;
	bool isFileScope;
	unsigned long lineNumber;
	int lang;
} Tag;


static Tag *createTag(const tagEntryInfo *info)
{
	Tag *tag = xCalloc(1, Tag);
	if (info->name)
		tag->name = eStrdup(info->name);
	if (info->extensionFields.signature)
		tag->signature = eStrdup(info->extensionFields.signature);
	if (info->extensionFields.scopeName)
		tag->scopeName = eStrdup(info->extensionFields.scopeName);
	if (info->extensionFields.inheritance)
		tag->inheritance = eStrdup(info->extensionFields.inheritance);
	if (info->extensionFields.typeRef[1])
		tag->varType = eStrdup(info->extensionFields.typeRef[1]);
	if (info->extensionFields.access)
		tag->access = eStrdup(info->extensionFields.access);
	if (info->extensionFields.implementation)
		tag->implementation = eStrdup(info->extensionFields.implementation);
	tag->kindLetter = ctagsGetKindFromIndex(info->kindIndex, info->langType);
	tag->isFileScope = info->isFileScope;
	tag->lineNumber = info->lineNumber;
	tag->lang = info->langType;
	return tag;
}


static void destroyTag(Tag *tag)
{
	if (tag->name)
		eFree(tag->name);
	if (tag->signature)
		eFree(tag->signature);
	if (tag->scopeName)
		eFree(tag->scopeName);
	if (tag->inheritance)
		eFree(tag->inheritance);
	if (tag->varType)
		eFree(tag->varType);
	if (tag->access)
		eFree(tag->access);
	if (tag->implementation)
		eFree(tag->implementation);
	eFree(tag);
}


/* callback from ctags informing us about a new tag */
static int writeEntry (tagWriter *writer, MIO *mio, const tagEntryInfo *const tag, void *clientData)
{
	Tag *t;

	/* apparently we have to call this to get the scope info - maybe we can
	 * specify some option during initialization so we don't have to cal this
	 * ?????? */
	getTagScopeInformation((tagEntryInfo *)tag, NULL, NULL);

	/* convert tags into our internal representation and store them into an array */
	t = createTag(tag);
	ptrArrayAdd((ptrArray *)clientData, t);

	/* output length - we don't write anything to the MIO */
	return 0;
}


/* scan has failed so we have invalid tags in our array - validTagNum should
 * tell us the number of valid tags so remove all the extra tags and shrink the array */
static void rescanFailed (tagWriter *writer, unsigned long validTagNum, void *clientData)
{
	ptrArray *tagArray = clientData;
	int num = ptrArrayCount(tagArray);

	if (num > validTagNum)
	{
		int i;
		for (i = validTagNum; i < num; i++)
		{
			Tag *tag = ptrArrayLast(tagArray);
			destroyTag(tag);
			ptrArrayRemoveLast(tagArray);
		}
	}
}


/* do whatever we want to do with the tags */
static void processCollectedTags(ptrArray *tagArray)
{
	int i;
	int num = ptrArrayCount(tagArray);

	for (i = 0; i < num; i++)
	{
		Tag *tag = ptrArrayItem(tagArray, i);
		printf("%s\tline: %lu\tkind: %s\t lang: %s\n",
			tag->name,
			tag->lineNumber,
			ctagsGetKindName(tag->kindLetter, tag->lang),
			ctagsGetLangName(tag->lang));
	}

	/* prepare for the next parsing by clearing the tag array */
	ptrArrayClear(tagArray);
}


extern int main (int argc, char **argv)
{
	/* called once when Geany starts */
	ctagsInit();

	/* create empty tag array *
	 * this is where we store the collected tags
	 * NOTE: Geany doesn't use the ptrArray type - it is used just for the purpose of this demo */
	ptrArray *tagArray = ptrArrayNew((ptrArrayDeleteFunc)destroyTag);

	printf("This parser only parses C files - provide them as arguments on the "
			"command line or get a hard-coded buffer parsed when no arguments "
			"are provided\n\n");
	if (argc == 1)  /* parsing contents of a buffer */
	{
		char *program = "int foo() {}\n\n int bar() {}\n\n int main() {}\n";
		int lang = ctagsGetNamedLang("C");
		const char *kinds = ctagsGetLangKinds(lang);
		int i;

		printf("Number of all parsers is: %d\n", ctagsGetLangCount());
		printf("We are parsing %s which provides the following kinds:\n",
			ctagsGetLangName(lang));
		for (i = 0; kinds[i] != '\0'; i++)
		{
			printf("%c: %s\n",
				/* back and forth conversion - the same like just kinds[i] */
				ctagsGetKindFromName(ctagsGetKindName(kinds[i], lang), lang),
				ctagsGetKindName(kinds[i], lang));
		}

		printf("\nParsing buffer:\n");
		/* we always specify the language by ourselves and don't use ctags detection */
		parseRawBuffer("whatever", (unsigned char *)program, strlen(program), lang, tagArray);

		processCollectedTags(tagArray);
	}
	else  /* parsing contents of a file */
	{
		int i;
		for (i = 1; i < argc; i++)
		{
			printf("\nParsing %s:\n", argv[i]);
			/* parseRawBuffer() is called repeatadly during Geany execution */
			parseRawBuffer(argv[i], NULL, 0, getNamedLanguage("C", 0), tagArray);

			processCollectedTags(tagArray);
		}
	}

	ptrArrayDelete(tagArray);

	return 0;
}
