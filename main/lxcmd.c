/*
*
*   Copyright (c) 1996-2003, Darren Hiebert
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for invoking external comand.
*   Half of codes are derived from lregex.c.
*   Core data structure is taken from readtags.h.
*
*/

#define XCMD_NOT_AVAILABLE_STATUS 77

/*
  XCMD PROTOCOL
  =============
  When invoking xcmd just only with --lint-kinds=LANG option,
  xcmd must write lines matching one of following patterns
  to stdout.

  patterns
  --------
  ([^ \t])
  ([^ \t])[ \t]+
  ([^ \t])[ \t]+([^ \t]+)
  ([^ \t])[ \t]+([^ \t]+)[ \t]+
  ([^ \t])[ \t]+([^ \t]+)[ \t]+(.+)

  patterns are dealt as follows
  \1 => letter
  \2 => name (default "xcmd")
  \3 => description (default: "xcmd" or name if it is available) */


/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#define _GNU_SOURCE   /* for WIFEXITED and WEXITSTATUS */
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>   /* for WIFEXITED and WEXITSTATUS */
#include <string.h>
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h> /* for WIFEXITED and WEXITSTATUS */
#endif

#include "debug.h"
#include "main.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#include "pcoproc.h"

#ifdef HAVE_COPROC

/*
*   MACROS
*/

#define XCMD_LIST_KIND_OPTION "--list-kinds"

/*
*   DATA DECLARATIONS
*/

typedef struct {

		/* the key of the extension field */
	const char *key;

		/* the value of the extension field (may be an empty string) */
	const char *value;
	int   pull_count;

} tagExtensionField;

/* This structure contains information about a specific tag. */
typedef struct {

		/* name of tag */
	const char *name;

		/* path of source file containing definition of tag */
	const char *file;

		/* address for locating tag in source file */
	struct {
			/* pattern for locating source line
			 * (may be NULL if not present) */
		const char *pattern;

			/* line number in source file of tag definition
			 * (may be zero if not known) */
		unsigned long lineNumber;
	} address;

		/* kind of tag (may by name, character, or NULL if not known) */
	const char *kind;

		/* is tag of file-limited scope? */
	short fileScope;

		/* miscellaneous extension fields */
	struct {
			/* number of entries in `list' */
		unsigned short count;

			/* list of key value pairs */
		tagExtensionField *list;
	} fields;

} tagEntry;

struct sKind {
	boolean enabled;
	char letter;
	char* name;
	char* description;
};

typedef struct {
	vString *path;
	struct sKind *kinds;
	unsigned int kount;
	boolean available;
	unsigned int id;	/* not used yet */
} xcmdPath;

typedef struct {
	xcmdPath *paths;
	unsigned int count;
} pathSet;

static pathSet* Sets = NULL;
static int SetUpper = -1;  /* upper language index in list */

static void clearPathSet (const langType language)
{
	if (language <= SetUpper)
	{
		pathSet* const set = Sets + language;
		unsigned int i, k;
		for (i = 0  ;  i < set->count  ;  ++i)
		{
			xcmdPath *p = &set->paths [i];

			vStringDelete (p->path);
			p->path = NULL;
			p->available = FALSE;
			for (k = 0; k < p->kount; k++)
			{
				struct sKind* kind = &(p->kinds[k]);

				eFree (kind->name);
				kind->name = NULL;
				eFree (kind->description);
				kind->description = NULL;
			}
			if (p->kinds)
			{
				eFree (p->kinds);
				p->kinds = NULL;
			}
		}
		if (set->paths != NULL)
			eFree (set->paths);
		set->paths = NULL;
		set->count = 0;
	}
}

static boolean loadPathKind (xcmdPath *const path, char* line, char *args[])
{
	struct sKind *kind;

	if (line[0] == '\0')
		return FALSE;
	else if (line[1] != ' ')
	{
		error (WARNING, "unexpected line(%s) from pipe connected to \"%s\"",
		       line, args[0]);
		return FALSE;
	}

	path->kinds = xRealloc (path->kinds, path->kount + 1, struct sKind);
	kind = &path->kinds [path->kount];
	kind->enabled = TRUE;
	kind->letter = line[0];

	verbose ("	kind letter: <%c>\n", kind->letter);

	for (line++; isblank(*line); line++)
		; /* do nothing */

	if (*line == '\0')
	{
		kind->name = eStrdup ("xcmd");
		kind->description = eStrdup ("xcmd");
	}
	else
	{
		char* name;

		name = line;
		for (line++; (*line != '\0') && (!isblank(*line)); line++)
			; /* do nothing */
		if (*line == '\0')
		{
			kind->name = eStrdup (name);
			kind->description = eStrdup (name);
		}
		else
		{
			Assert (isblank (*line));
			*line = '\0';
			kind->name = eStrdup (name);
			*line = ' ';
			for (line++; isblank(*line); line++)
				; /* do nothing */
			Assert (!isblank (*line));
			kind->description = eStrdup (*line == '\0'? kind->name: line);
		}
	}
	path->kount += 1;
	return TRUE;
}

static boolean loadPathKinds  (xcmdPath *const path, const langType language)
{
	enum pcoprocError r;
	FILE* pp = NULL;
	char * argv[3];
	int status;
	vString * opt;
	char file_kind = getLanguageFileKind (language);

	opt = vStringNewInit(XCMD_LIST_KIND_OPTION);
	vStringCatS (opt, "=");
	vStringCatS (opt, getLanguageName(language));

	argv[2] = NULL;
	argv[1] = vStringValue (opt);
	argv[0] = vStringValue (path->path);

	errno = 0;

	verbose ("loading path kinds of %s from [%s %s]\n", getLanguageName(language), argv[0], argv[1]);
	r = pcoprocOpen (vStringValue (path->path), argv, &pp, NULL);
	switch (r) {
	case PCOPROC_ERROR_WPIPE:
		error (WARNING | PERROR, "failed to make pipe to write to command: [%s %s]",
		       argv[0], argv[1]);
		break;
	case PCOPROC_ERROR_RPIPE:
		error (WARNING | PERROR, "failed to make pipe to read from command: [%s %s]",
		       argv[0], argv[1]);
		break;
	case PCOPROC_ERROR_FORK:
		error (WARNING | PERROR, "failed to do fork: [%s %s]",
		       argv[0], argv[1]);
		break;
	case PCOPROC_SUCCESSFUL:
		break;
	}

	if (pp)
	{
		vString* vline = vStringNew();

		while (readLineWithNoSeek (vline, pp))
		{
			char* line;
			char  kind_letter;

			vStringStripNewline (vline);
			line = vStringValue (vline);
			if (!loadPathKind (path, line, argv))
				break;

			kind_letter = path->kinds [path->kount - 1].letter;
			if (kind_letter == file_kind)
				error (FATAL,
				       "Kind letter \'%c\' returned from xcmd %s of %s language is reserved in ctags main",
				       kind_letter,
				       vStringValue (path->path),
				       getLanguageName (language));
		}

		vStringDelete (vline);


		status = pcoprocClose (pp);

		/* TODO: Decode status */
		verbose("	status: %d\n", status);
		if (status != 0)
		{
			if (status > 0 
			    && WIFEXITED (status) 
			    && (WEXITSTATUS (status) == XCMD_NOT_AVAILABLE_STATUS))
				verbose ("xcmd: the %s backend is not available\n", argv[0]);
			else
				error (WARNING, "xcmd exits abnormally status(%d): [%s %s]",
				       status, argv[0], argv[1]);
			vStringDelete (opt);
			return FALSE;
		}
	}
	else
	{
		error (WARNING | PERROR, "cannot make pipe to xcmd: [%s %s]",
		       argv[0], argv[1]);
	}

	vStringDelete (opt);
	return path->kinds == NULL? FALSE: TRUE;
}
#endif	/* HAVE_COPROC */

extern void resetXcmdKinds (const langType language, boolean mode)
{
#ifdef HAVE_COPROC
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		pathSet* const set = Sets + language;
		xcmdPath * path = set->paths;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
		{
			unsigned int k;
			if (!path[i].available)
				continue;

			for (k = 0; k < path[i].kount; k++)
			{
				struct sKind *const kind = path[i].kinds + k;
				kind->enabled = mode;
			}
		}
	}
#endif
}

extern boolean enableXcmdKind (const langType language, const int kind,
			       const boolean mode)
{
	boolean result = FALSE;
#ifdef HAVE_COPROC
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		pathSet* const set = Sets + language;
		xcmdPath * path = set->paths;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
		{
			unsigned int k;
			if (!path[i].available)
				continue;

			for (k = 0; k < path[i].kount; k++)
			{
				if (path[i].kinds[k].letter == kind)
				{
					path[i].kinds[k].enabled = mode;
					result = TRUE;
				}
			}
		}
	}
#endif
	return result;
}

#ifdef HAVE_COPROC
static void printXcmdKind (xcmdPath *path, unsigned int i, boolean indent)
{
	unsigned int k;
	const char *const indentation = indent ? "    " : "";

	if (!path[i].available)
		return;

	for (k = 0; k < path[i].kount; k++)
	{
		const struct sKind *const kind = path[i].kinds + k;
		printf ("%s%c  %s %s\n", indentation,
			kind->letter != '\0' ? kind->letter : '?',
			kind->description != NULL ? kind->description : kind->name,
			kind->enabled ? "" : " [off]");
	}
}
#endif

extern void printXcmdKinds (const langType language __unused__, boolean indent __unused__)
{
#ifdef HAVE_COPROC
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		pathSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
			printXcmdKind (set->paths, i, indent);
	}
#endif
}

extern void freeXcmdResources (void)
{
#ifdef HAVE_COPROC
	int i;
	for (i = 0  ;  i <= SetUpper  ;  ++i)
		clearPathSet (i);
	if (Sets != NULL)
		eFree (Sets);
	Sets = NULL;
	SetUpper = -1;
#endif
}
extern void addTagXcmd (const langType language, vString* pathvstr)
{
#ifdef HAVE_COPROC
	pathSet* set;
	xcmdPath *path;
	Assert (pathvstr != NULL);

	if (language > SetUpper)
	{
		int i;
		Sets = xRealloc (Sets, (language + 1), pathSet);
		for (i = SetUpper + 1  ;  i <= language  ;  ++i)
		{
			Sets [i].paths = NULL;
			Sets [i].count = 0;
		}
		SetUpper = language;
	}
	set = Sets + language;
	set->paths = xRealloc (set->paths, (set->count + 1), xcmdPath);

	path = &set->paths [set->count];
	path->path = pathvstr;
	path->kinds = NULL;
	path->kount = 0;
	path->id = set->count;

	set->count += 1;

	path->available = (loadPathKinds (path, language));
	useXcmdMethod (language);
	if (path->available)
		notifyAvailabilityXcmdMethod (language);
#endif
}
extern void addLanguageXcmd (
	const langType language __unused__, const char* const parameter __unused__)
{
#ifdef HAVE_COPROC
	vString* vpath;

	if (parameter [0] != '/' && parameter [0] != '.')
	{
		vpath = expandOnDriversPathList (parameter);
		vpath = vpath? vpath: vStringNewInit(parameter);
	}
	else
		vpath = vStringNewInit(parameter);
	addTagXcmd (language, vpath);
#endif
}

#ifdef HAVE_COPROC
static void processLanguageXcmd (const langType language,
				 const char* const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		clearPathSet (language);
	else
		addLanguageXcmd (language, parameter);
}
#endif

extern boolean processXcmdOption (const char *const option, const char *const parameter)
{
	const char* const dash = strchr (option, '-');
	char* lang;

#ifdef HAVE_COPROC
	langType language;
#endif

	if (dash == NULL)
		return FALSE;

	if (strncmp (option, "xcmd", dash - option) == 0)
		lang = eStrdup (dash + 1);
	else if (strcmp (dash + 1, "xcmd") == 0)
		lang = eStrndup (option, dash - option);
	else
		return FALSE;

#ifdef HAVE_COPROC
	language = getNamedLanguage (lang);
	if (language == LANG_IGNORE)
		error (WARNING, "unknown language \"%s\" in --%s option", lang, option);
	else
		processLanguageXcmd (language, parameter);
#else
	error (WARNING, "coproc feature is not available; required for --%s option",
	       option);
#endif

	eFree (lang);

	return TRUE;
}

#ifdef HAVE_COPROC
static const char* lookupKindName  (char kind_letter, const xcmdPath* const path)
{
	unsigned int k;
	struct sKind *kind;

	for (k = 0; k < path->kount; k++)
	{
		kind = path->kinds + k;
		if (kind->letter == kind_letter)
			if (kind->name)
				return kind->name;
	}
	return NULL;

}

static const char* lookupKindLetter (const char* const kind_name, const xcmdPath *const path)
{
	unsigned int k;
	struct sKind *kind;

	for (k = 0; k < path->kount; k++)
	{
		kind = path->kinds + k;
		if (kind->name && (!strcmp(kind->name, kind_name)))
			return &kind->letter;

	}
	return NULL;

}

static const char* entryLookupField (tagEntry *const entry, const char *const kind, boolean pulling)
{
	int i;

	for (i = 0; i < entry->fields.count; i++)
	{
		if (!strcmp (entry->fields.list [i].key, kind))
		{
			if (pulling)
				entry->fields.list [i].pull_count++;
			return entry->fields.list [i].value;
		}
	}
	return NULL;
}

static const char* entryGetAnyUnpulledField (tagEntry *const entry, const char **const kind, boolean pulling)
{
	int i;

	for (i = 0; i < entry->fields.count; i++)
	{
		if (entry->fields.list [i].pull_count == 0)
		{
			*kind = entry->fields.list [i].key;
			if (pulling)
				entry->fields.list [i].pull_count++;
			return entry->fields.list [i].value;
		}
	}
	return NULL;
}

static boolean isKindEnabled (xcmdPath* path, const char* value)
{
	unsigned int k;
	struct sKind *kind;

	Assert (path->kinds);
	Assert (value);
	Assert (*value);

	for (k = 0; k < path->kount; k++)
	{
		kind = path->kinds + k;
		if (!kind->enabled)
		{
			if (value[1] == '\0' && value[0] == kind->letter)
				return FALSE;
			if (!strcmp(value, kind->name))
				return FALSE;
			if (!strcmp(value, kind->description))
				return FALSE;
		}
	}
	return TRUE;
}

static void entryAddField (tagEntry *const entry, const char *const key, const char *const value)
{
	entry->fields.list = xRealloc (entry->fields.list,
				       entry->fields.count + 1,
				       tagExtensionField);
	entry->fields.list [entry->fields.count].key = key;
	entry->fields.list [entry->fields.count].value = value;
	entry->fields.list [entry->fields.count].pull_count = 0;
	++entry->fields.count;
}

static boolean parseExtensionFields (tagEntry *const entry, char *const string, xcmdPath* path)
{
	char *p = string;

	while (p != NULL  &&  *p != '\0')
	{
		while (*p == TAB)
			*p++ = '\0';
		if (*p != '\0')
		{
			char *colon;
			char *field = p;
			p = strchr (p, TAB);
			if (p != NULL)
				*p++ = '\0';
			colon = strchr (field, ':');
			if (colon == NULL)
			{
				if (isKindEnabled (path, field))
					entry->kind = field;
				else
					goto reject;
			}
			else
			{
				const char *key = field;
				const char *value = colon + 1;
				*colon = '\0';
				if (strcmp (key, "kind") == 0)
				{
					if (*value == '\0')
						goto reject;
					else if (isKindEnabled (path, value))
						entryAddField(entry, key, value);
					else
						goto reject;
				}
				else if (strcmp (key, "file") == 0)
					entry->fileScope = 1;
				else if (strcmp (key, "line") == 0)
					entry->address.lineNumber = atol (value);
				else if (strcmp (key, "language") == 0)
					continue;
				else
					entryAddField (entry, key, value);
			}
		}
	}
	return TRUE;
  reject:
	eFree (entry->fields.list);
	entry->fields.list = NULL;
	entry->fields.count = 0;
	return FALSE;
}

static boolean fillEntry (const xcmdPath* path, tagEntry* entry)
{
	if (entry->kind)
	{
		const char* name = lookupKindName (entry->kind[0], path);
		if (name)
		{
			entryAddField(entry, "kind", name);
			return TRUE;
		}
	}
	else
	{
		const char* kind = entryLookupField (entry, "kind", FALSE);
		if (kind)
		{
			entry->kind = lookupKindLetter (kind, path);
			if (entry->kind)
				return TRUE;
		}
	}
	return FALSE;
}

static const char *const PseudoTagPrefix = "!_";
static boolean hasPseudoTagPrefix (const char* const name)
{
	const size_t prefixLength = strlen (PseudoTagPrefix);
	return !strncmp (name, PseudoTagPrefix, prefixLength);
}

static boolean parseXcmdPath (char* line, xcmdPath* path, tagEntry* entry)
{
	char *p = line;
	char *tab = strchr (p, TAB);
	boolean pseudoTag = FALSE;

	// verbose("<%s>line: %s\n", vStringValue (path->path), line);

	entry->name = p;
	if (tab != NULL)
	{
		*tab = '\0';
		pseudoTag = hasPseudoTagPrefix (entry->name);

		p = tab + 1;
		entry->file = p;
		tab = strchr (p, TAB);
		if (tab != NULL)
		{
			 int fieldsPresent;
			 *tab = '\0';
			 p = tab + 1;
			 if (*p == '/'  ||  *p == '?')
			 {
				 /* parse pattern */
				 int delimiter = *(unsigned char*) p;
				 entry->address.lineNumber = 0;
				 entry->address.pattern = p;
				 do
				 {
					 p = strchr (p + 1, delimiter);
				 } while (p != NULL  &&  *(p - 1) == '\\');
				 if (p == NULL)
				 {
					 *tab = '\t';
					 error (WARNING, "pattern from %s is not ended with `%c': %s",
						vStringValue (path->path),
						(char)delimiter,
					       line);
					 return FALSE;
				 }
				else
					++p;
			 }
			 else if (isdigit ((int) *(unsigned char*) p))
			 {
				 /* parse line number */
				 entry->address.pattern = p;
				 entry->address.lineNumber = atol (p);
				 while (isdigit ((int) *(unsigned char*) p))
					 ++p;
			 }
			 else
			 {
				 *tab = '\t';
				 error (WARNING, "cannot parse as ctags output from %s: %s",
					vStringValue (path->path), line);
				 return FALSE;
			 }
			 fieldsPresent = (strncmp (p, ";\"", 2) == 0);
			 *p = '\0';

			 if (pseudoTag)
				 return TRUE;

			 if (fieldsPresent)
			 {
				 if (!parseExtensionFields (entry, p + 2, path))
					 return FALSE;
				 if (fillEntry (path, entry))
					 return TRUE;
			 }
		}
	}
	return FALSE;
}

static void freeTagEntry (tagEntry* entry)
{
	eFree (entry->fields.list);
	entry->fields.list = NULL;
	entry->fields.count = 0;
}

static boolean makePseudoTagEntryFromTagEntry (tagEntry* entry)
{
	const char *tagName, *fileName, *pattern;
	const size_t prefixLength = strlen (PseudoTagPrefix);

	tagName = entry->name + prefixLength;
	fileName = entry->file;
	pattern = entry->address.pattern;

	if (strcmp (tagName, "TAG_FILE_SORTED") == 0)
		return FALSE;
	else if (strcmp (tagName, "TAG_FILE_FORMAT") == 0)
		return FALSE;	/* ??? */
	else if ((strcmp (tagName, "TAG_PROGRAM_AUTHOR") == 0)
		 || (strcmp (tagName, "TAG_PROGRAM_NAME") == 0)
		 || (strcmp (tagName, "TAG_PROGRAM_URL") == 0)
		 || (strcmp (tagName, "TAG_PROGRAM_VERSION") == 0))
	{
		writePseudoTag (tagName, fileName, pattern,
				entryLookupField(entry, "language", FALSE));
		return TRUE;
	}

	return FALSE;
}

static boolean makeTagEntryFromTagEntry (tagEntry* entry)
{
	tagEntryInfo tag;
	fpos_t      filePosition;

	if (hasPseudoTagPrefix (entry->name))
	{
		if  (isDestinationStdout())
			return FALSE;
		else if (Option.xref)
			return FALSE;
		else if (Option.etags)
			return FALSE;
		else
			return makePseudoTagEntryFromTagEntry (entry);
	}

	memset(&filePosition, 0, sizeof(filePosition));

	// pseudo if (entry->name...);
	initTagEntryFull (&tag, entry->name,
			  entry->address.lineNumber,
			  entryLookupField(entry, "language", TRUE),
			  filePosition,
			  entry->file);

	tag.kind = entry->kind[0];
	tag.kindName = entryLookupField(entry, "kind", TRUE);
	tag.pattern = entry->address.pattern;

	tag.isFileScope = (boolean)entry->fileScope;
	tag.extensionFields.access = entryLookupField(entry, "access", TRUE);
	tag.extensionFields.implementation = entryLookupField(entry, "implementation", TRUE);
	tag.extensionFields.inheritance = entryLookupField(entry, "inherits", TRUE);
	tag.extensionFields.signature = entryLookupField(entry, "signature", TRUE);
	tag.extensionFields.typeRef[0] = entryLookupField(entry, "typeref", TRUE);
	if (tag.extensionFields.typeRef[0])
	{
		char *tmp;
		tmp = strchr (tag.extensionFields.typeRef[0], ':');
		if (tmp)
		{
			*tmp = '\0';
			tag.extensionFields.typeRef[1] = tmp + 1;
		}
	}
	tag.extensionFields.scope[1] = entryGetAnyUnpulledField (entry,
								 &tag.extensionFields.scope[0],
								 TRUE);

	makeTagEntry (&tag);
	return TRUE;
}

static boolean invokeXcmdPath (const char* const fileName, xcmdPath* path, const langType language)
{
	enum pcoprocError r;
	boolean result = FALSE;
	char* argv[4];
	FILE* pp = NULL;

	if (!path->available)
		return FALSE;

	argv[2] = NULL;
	argv[1] = (char * const)fileName;
	argv[0] = vStringValue (path->path);

	verbose ("getting tags of %s language from [%s %s]\n", getLanguageName(language), argv[0], argv[1]);
	r = pcoprocOpen (vStringValue (path->path), argv, &pp, NULL);
	switch (r) {
	case PCOPROC_ERROR_WPIPE:
		error (WARNING | PERROR, "failed to make pipe to write to command: [%s %s]",
		       argv[0], argv[1]);
		break;
	case PCOPROC_ERROR_RPIPE:
		error (WARNING | PERROR, "failed to make pipe to read from command: [%s %s]",
		       argv[0], argv[1]);
		break;
	case PCOPROC_ERROR_FORK:
		error (WARNING | PERROR, "failed to do fork: [%s %s]",
		       argv[0], argv[1]);
		break;
	case PCOPROC_SUCCESSFUL:
		break;
	}

	if (pp)
	{
		vString* vline = vStringNew();
		int status;

		while (readLineWithNoSeek (vline, pp))
		{
			char* line;
			tagEntry entry;

			memset(&entry, 0, sizeof(entry));
			vStringStripNewline (vline);
			line = vStringValue (vline);


			if (parseXcmdPath (line, path, &entry) )
			{
				entryAddField (&entry, "language", getLanguageName (language));
				if (makeTagEntryFromTagEntry (&entry))
					result = TRUE;
				freeTagEntry (&entry);
			}
		}

		vStringDelete (vline);

		status = pcoprocClose (pp);
		verbose("	status: %d\n", status);
		if (status)
		{
			error (WARNING | PERROR, "xcmd exits abnormally status(%d): [%s %s]",
			       status, argv[0], argv[1]);
			return FALSE;
		}
	}
	else
	{
		error (WARNING | PERROR, "cannot make pipe to xcmd: [%s %s]",
		       argv[0], argv[1]);
	}

	return result;
}

#endif

extern boolean invokeXcmd (const char* const fileName, const langType language)
{
	boolean result = FALSE;

#ifdef HAVE_COPROC
	if (language != LANG_IGNORE  &&  language <= SetUpper  &&
		Sets [language].count > 0)
	{
		const pathSet* const set = Sets + language;
		unsigned int i;

		for (i = 0; i < set->count ; ++i)
		{
			xcmdPath* path = set->paths + i;
			if (invokeXcmdPath (fileName, path, language))
				result = TRUE;
		}

	}
#endif
	return result;
}
