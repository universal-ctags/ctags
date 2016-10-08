/*
*
*   Copyright (c) 1996-2003, Darren Hiebert
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for invoking external command.
*   Half of codes are derived from lregex.c.
*   Core data structure is taken from readtags.h.
*
*/

/*
  XCMD PROTOCOL (version 2.1)
  ==================================================================
  When invoking xcmd just only with --lint-kinds=LANG option,
  xcmd must write lines matching one of following patterns
  to stdout.

  patterns
  --------

  ^([^ \t])[ \t]+([^\t]+)([ \t]+(\[off\]))?$
  \1 => letter
  \2 => name
  \4 => \[off\] is optional.


  exit code
  ---------

  If xcmd itself recognizes it cannot run, it should exit with
  XCMD_NOT_AVAILABLE_STATUS exit code. ctags may ignore the xcmd.
*/

#define XCMD_NOT_AVAILABLE_STATUS 127

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#ifndef _GNU_SOURCE
# define _GNU_SOURCE   /* for WIFEXITED and WEXITSTATUS */
#endif
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>   /* for WIFEXITED and WEXITSTATUS */
#include <string.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h> /* for WIFEXITED and WEXITSTATUS */
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif


#include "debug.h"
#include "main.h"
#include "options.h"
#include "parse.h"
#include "ptag.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#include "pcoproc.h"

#include "flags.h"
#include "xtag.h"
#include "ptag.h"

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

		/* path of input file containing definition of tag */
	const char *file;

		/* address for locating tag in input file */
	struct {
			/* pattern for locating input line
			 * (may be NULL if not present) */
		const char *pattern;

			/* line number in input file of tag definition
			 * (may be zero if not known) */
		unsigned long lineNumber;
	} address;


	const kindOption* kind;

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

typedef struct {
	vString *path;
	kindOption *kinds;
	unsigned int n_kinds;
	bool available;
	unsigned int id;	/* not used yet */
	int not_available_status;
} xcmdPath;

typedef struct {
	xcmdPath *paths;
	unsigned int count;
} pathSet;

#ifdef HAVE_COPROC

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
			p->available = false;
			for (k = 0; k < p->n_kinds; k++)
			{
				kindOption* kind = &(p->kinds[k]);

				eFree ((void *)kind->name);
				kind->name = NULL;
				eFree ((void *)kind->description);
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

static bool loadPathKind (xcmdPath *const path, char* line, char *args[])
{
	const char* backup = line;
	char* off;
	vString *desc;
	kindOption *kind;

	if (line[0] == '\0')
		return false;
	else if (!isblank(line[1]))
	{
		error (WARNING, "[%s] a space after letter is not found in kind description line: %s", args[0], backup);
		return false;
	}

	path->kinds = xRealloc (path->kinds, path->n_kinds + 1, kindOption);
	kind = &path->kinds [path->n_kinds];
	memset (kind, 0, sizeof (*kind));
	kind->enabled = true;
	kind->letter = line[0];
	kind->name = NULL;
	kind->description = NULL;
	kind->referenceOnly = false;
	kind->nRoles = 0;
	kind->roles = NULL;

	verbose ("	kind letter: <%c>\n", kind->letter);

	for (line++; isblank(*line); line++)
		;		/* do nothing */

	if (*line == '\0')
	{
		error (WARNING, "[%s] unexpectedly a kind description line is terminated: %s",
		       args[0], backup);
		return false;
	}

	Assert (!isblank (*line));

	off = strrstr(line, "[off]");
	if (off == line)
	{
		error (WARNING, "[%s] [off] is given but no kind description is found: %s",
		       args[0], backup);
		return false;
	}
	else if (off)
	{
		if (!isblank (*(off - 1)))
		{
			error (WARNING, "[%s] a whitespace must precede [off] flag: %s",
			       args[0], backup);
			return false;
		}
		kind->enabled = false;
		*off = '\0';
	}
	desc = vStringNewInit (line);
	vStringStripTrailing (desc);

	Assert (vStringLength (desc) > 0);

	kind->description = vStringDeleteUnwrap (desc);

	/* TODO: This conversion should be part of protocol. */
	{
	  char *tmp = eStrdup (kind->description);
	  char *c;
	  for (c = tmp; *c != '\0'; c++)
	    {
	      if (*c == ' ' || *c == '\t')
		*c = '_';
	    }
	  kind->name = tmp;
	}

	path->n_kinds += 1;
	return true;
}

static bool isSafeExecutable (const char* path)
{
	fileStatus *file;
	bool r;

	Assert (path);
	file =  eStat (path);

	if (!file->exists)
	{
		/* The file doesn't exist. So I cannot say
		   it is unsafe. The caller should
		   handle this case. */
		r = true;
	}
	else if (file->isSetuid)
	{
		error (WARNING, "xcmd doesn't run a setuid executable: %s", path);
		r = false;
	}
	else if (file->isSetgid)
	{
		error (WARNING, "xcmd doesn't run a setgid executable: %s", path);
		r = false;
	}
	else
		r = true;

	eStatFree (file);
	return r;
}

static bool loadPathKinds  (xcmdPath *const path, const langType language)
{
	enum pcoprocError r;
	FILE* pp = NULL;
	char * argv[3];
	int status;
	vString * opt;
	char file_kind = getLanguageFileKind (language)->letter;

	opt = vStringNewInit(XCMD_LIST_KIND_OPTION);
	vStringCatS (opt, "=");
	vStringCatS (opt, getLanguageName(language));

	argv[2] = NULL;
	argv[1] = vStringValue (opt);
	argv[0] = vStringValue (path->path);

	errno = 0;

	if (getuid() == 0 || geteuid() == 0)
	{
		verbose ("all xcmd feature is disabled when running ctags in root privilege\n");
		vStringDelete (opt);
		return false;
	}

	if (! isSafeExecutable (argv [0]))
	{
		vStringDelete (opt);
		return false;
	}
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

		while (readLineRawWithNoSeek (vline, pp))
		{
			char* line;
			char  kind_letter;

			vStringStripNewline (vline);
			line = vStringValue (vline);
			if (!loadPathKind (path, line, argv))
				break;

			kind_letter = path->kinds [path->n_kinds - 1].letter;
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
			    && (WEXITSTATUS (status) == path->not_available_status))
				verbose ("xcmd: the %s backend is not available\n", argv[0]);
			else
				error (WARNING, "xcmd exits abnormally status(%d): [%s %s]",
				       status, argv[0], argv[1]);
			vStringDelete (opt);
			return false;
		}
	}
	else
	{
		error (WARNING | PERROR, "cannot make pipe to xcmd: [%s %s]",
		       argv[0], argv[1]);
	}

	vStringDelete (opt);
	return path->kinds == NULL? false: true;
}
#endif	/* HAVE_COPROC */


extern void foreachXcmdKinds (const langType language,
			      bool (*func) (kindOption *, void *),
			      void *data)
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

			for (k = 0; k < path[i].n_kinds; k++)
				if (func (& (path[i].kinds[k]), data))
					break;
		}
	}
#endif
}

static bool kind_reset_cb (kindOption *kind, void *data)
{
	kind->enabled = *(bool *)data;
	return false;		/* continue */
}

extern void resetXcmdKinds (const langType language, bool mode)
{
	foreachXcmdKinds (language, kind_reset_cb, &mode);
}

struct kind_and_mode_and_result
{
	int kind;
	const char *kindLong;
	bool mode;
	bool result;
};

static bool enable_kind_cb (kindOption *kind, void *data)
{
	struct kind_and_mode_and_result *kmr = data;
	if ((kmr->kind != KIND_NULL
	     && kind->letter == kmr->kind)
	    || (kmr->kindLong && kind->name
		&& (strcmp (kmr->kindLong, kind->name) == 0)))
	{
		kind->enabled = kmr->mode;
		kmr->result = true;
	}
	/* conitnue:
	   There can be more than one paths which deals this kind.
	   Consider /bin/X and /bin/Y are both parser for a language L.
	   ctags --langdef=L --xcmd-L=/bin/X --xcmd-L=/bin/Y ... */
	return false;

}

extern bool enableXcmdKind (const langType language, const int kind,
			       const bool mode)
{
	struct kind_and_mode_and_result kmr;

	kmr.kind = kind;
	kmr.kindLong = NULL;
	kmr.mode = mode;
	kmr.result = false;

	foreachXcmdKinds (language, enable_kind_cb, &kmr);
	return kmr.result;
}

extern bool enableXcmdKindLong (const langType language, const char *kindLong,
				   const bool mode)
{
	struct kind_and_mode_and_result kmr;

	kmr.kind = KIND_NULL;
	kmr.kindLong = kindLong;
	kmr.mode = mode;
	kmr.result = false;

	foreachXcmdKinds (language, enable_kind_cb, &kmr);
	return kmr.result;
}

struct kind_and_result
{
	int kind;
	bool result;
};

static bool is_kind_enabled_cb (kindOption *kind, void *data)
{
	bool r = false;
	struct kind_and_result *kr = data;

	if (kind->letter == kr->kind)
	{
		kr->result = kind->enabled;
		r = true;
	}

	return r;
}

static bool does_kind_exist_cb (kindOption *kind, void *data)
{
	bool r = false;
	struct kind_and_result *kr = data;

	if (kind->letter == kr->kind)
	{
		kr->result = true;
		r = true;
	}

	return r;
}

extern bool isXcmdKindEnabled (const langType language, const int kind)
{
	struct kind_and_result d;

	d.kind = kind;
	d.result = false;

	foreachXcmdKinds (language, is_kind_enabled_cb, &d);

	return d.result;
}

extern bool hasXcmdKind (const langType language, const int kind)
{
	struct kind_and_result d;

	d.kind = kind;
	d.result = false;

	foreachXcmdKinds (language, does_kind_exist_cb, &d);

	return d.result;
}

struct printXcmdKindCBData {
	const char *langName;
	bool allKindFields;
	bool indent;
	bool tabSeparated;
};

#ifdef HAVE_COPROC
static bool printXcmdKind (kindOption *kind, void *user_data)
{
	struct printXcmdKindCBData *data = user_data;

	if (data->allKindFields && data->indent)
		printf (Option.machinable? "%s": PR_KIND_FMT (LANG,s), data->langName);
	
	printKind (kind, data->allKindFields, data->indent, data->tabSeparated);
	return false;
}
#endif

extern void printXcmdKinds (const langType language CTAGS_ATTR_UNUSED,
			    bool allKindFields CTAGS_ATTR_UNUSED,
			    bool indent CTAGS_ATTR_UNUSED,
			    bool tabSeparated CTAGS_ATTR_UNUSED)
{
#ifdef HAVE_COPROC
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
                const char* const langName = getLanguageName(language);
		struct printXcmdKindCBData data = {
			.langName      = langName,
			.allKindFields = allKindFields,
			.indent        = indent,
			.tabSeparated  = tabSeparated,
		};
		foreachXcmdKinds (language, printXcmdKind, &data);
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

#ifdef HAVE_COPROC
static void xcmd_flag_not_avaible_status_long (const char* const s, const char* const v, void* data)
{
	xcmdPath *path = data;

	if(!strToInt(v, 0, &path->not_available_status))
		error (FATAL, "Could not parse the value for %s flag: %s", s, v);
}
#endif

extern void addTagXcmd (const langType language, vString* pathvstr, const char* flags)
{
#ifdef HAVE_COPROC
	pathSet* set;
	xcmdPath *path;

	flagDefinition xcmdFlagDefs[] = {
		{ '\0', "notAvailableStatus", NULL, xcmd_flag_not_avaible_status_long },
	};

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
	path->n_kinds = 0;
	path->id = set->count;
	path->not_available_status = XCMD_NOT_AVAILABLE_STATUS;

	set->count += 1;

	flagsEval (flags, xcmdFlagDefs, ARRAY_SIZE(xcmdFlagDefs), path);

	path->available = (loadPathKinds (path, language));
	useXcmdMethod (language);
	if (path->available)
		notifyAvailabilityXcmdMethod (language);
#endif
}
extern void addLanguageXcmd (
	const langType language CTAGS_ATTR_UNUSED, const char* const parameter CTAGS_ATTR_UNUSED)
{
#ifdef HAVE_COPROC
	char *path;
	vString* vpath;
	const char* flags;

	flags = strchr (parameter, LONG_FLAGS_OPEN);
	if (flags)
		path = eStrndup (parameter, flags - parameter);
	else
		path = eStrdup (parameter);

	if (parameter [0] != '/' && parameter [0] != '.')
	{
		vpath = expandOnDriversPathList (path);
		vpath = vpath? vpath: vStringNewInit(path);
	}
	else
		vpath = vStringNewInit(path);

	eFree (path);

	addTagXcmd (language, vpath, flags);
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

extern bool processXcmdOption (const char *const option, const char *const parameter,
				  OptionLoadingStage stage)
{
	langType language;

	language = getLanguageComponentInOption (option, "xcmd-");
	if (language == LANG_IGNORE)
		return false;

	if (stage == OptionLoadingStageCurrentRecursive)
	{
		error (WARNING, "Don't use --xcmd-<LANG> option in ./.ctags nor ./.ctags/*: %s",
		       option);
		/* Consume it here. */
		return true;
	}
	else if (stage == OptionLoadingStageHomeRecursive && (!Option.allowXcmdInHomeDir))
	{
		error (WARNING, "Don't use --xcmd-<LANG> option in ~/.ctags and/or ~/.ctags/*: %s",
		       option);
		/* Consume it here. */
		return true;
	}

#ifdef HAVE_COPROC
	processLanguageXcmd (language, parameter);
#else
	error (WARNING, "coproc feature is not available; required for --%s option",
	       option);
#endif

	return true;
}

#ifdef HAVE_COPROC
static const kindOption* lookupKindFromLetter (const xcmdPath* const path, char kind_letter)
{
	unsigned int k;
	kindOption *kind;

	for (k = 0; k < path->n_kinds; k++)
	{
		kind = path->kinds + k;
		if (kind->letter == kind_letter)
			return kind;
	}
	return NULL;

}

static const kindOption* lookupKindFromName (const xcmdPath* const path, const char* const kind_name)
{
	unsigned int k;
	kindOption *kind;

	for (k = 0; k < path->n_kinds; k++)
	{
		kind = path->kinds + k;
		if (kind->name && (!strcmp(kind->name, kind_name)))
			return kind;

	}
	return NULL;

}

static const char* entryLookupField (tagEntry *const entry, const char *const kind, bool pulling)
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

static const char* entryGetAnyUnpulledField (tagEntry *const entry, const char **const kind, bool pulling)
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

static bool isKindEnabled (xcmdPath* path, const char* value)
{
	unsigned int k;
	kindOption *kind;

	Assert (path->kinds);
	Assert (value);
	Assert (*value);

	for (k = 0; k < path->n_kinds; k++)
	{
		kind = path->kinds + k;
		if (!kind->enabled)
		{
			if (value[1] == '\0' && value[0] == kind->letter)
				return false;
			if (!strcmp(value, kind->name))
				return false;
			if (!strcmp(value, kind->description))
				return false;
		}
	}
	return true;
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

static bool parseExtensionFields (tagEntry *const entry, char *const string, xcmdPath* path)
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
				{
					if (entry->kind == NULL)
					{
						entry->kind = lookupKindFromLetter (path, field[0]);
						if (entry->kind == NULL)
						{
							kindOption *fileKind = getInputLanguageFileKind ();
							if (fileKind && fileKind->letter == field[0])
								/* ctags will make a tag for file. */
								goto reject;

						}
					}
					else {
                                            ; /* TODO Handle warning */
                                        }
					Assert (entry->kind);

				}
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
					{
						if (entry->kind == NULL)
						{
							entry->kind = lookupKindFromName (path, value);
							if (entry->kind == NULL)
							{
								kindOption *fileKind = getInputLanguageFileKind ();
								if (fileKind && (strcmp(fileKind->name, value) == 0))
									/* ctags will make a tag for file. */
									goto reject;

							}
						}

						else {
                                                    ; /*TODO Handle warning*/
                                                }
						Assert (entry->kind);
					}
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
	return true;
  reject:
	if (entry->fields.list)
	{
		eFree (entry->fields.list);
		entry->fields.list = NULL;
	}
	entry->fields.count = 0;
	return false;
}

static bool hasPseudoTagPrefix (const char* const name)
{
	const size_t prefixLength = strlen (PSEUDO_TAG_PREFIX);
	return !strncmp (name, PSEUDO_TAG_PREFIX, prefixLength);
}

static bool parseXcmdPath (char* line, xcmdPath* path, tagEntry* entry)
{
	char *p = line;
	char *tab = strchr (p, TAB);
	bool pseudoTag = false;

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
					 return false;
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
				 return false;
			 }
			 fieldsPresent = (strncmp (p, ";\"", 2) == 0);
			 *p = '\0';

			 if (pseudoTag)
				 return true;

			 if (fieldsPresent)
			 {
				 if (!parseExtensionFields (entry, p + 2, path))
					 return false;
				 return true;
			 }
		}
	}
	return false;
}

static void freeTagEntry (tagEntry* entry)
{
	if (entry->fields.list)
	{
		eFree (entry->fields.list);
		entry->fields.list = NULL;
	}
	entry->fields.count = 0;
}

static bool makePseudoTagEntryFromTagEntry (tagEntry* entry)
{
	const char *tagName, *fileName, *pattern;
	const size_t prefixLength = strlen (PSEUDO_TAG_PREFIX);
	ptagType t = PTAG_UNKNOWN;

	tagName = entry->name + prefixLength;
	fileName = entry->file;
	pattern = entry->address.pattern;

	if (strcmp (tagName, "TAG_FILE_SORTED") == 0)
		return false;
	else if (strcmp (tagName, "TAG_FILE_FORMAT") == 0)
		return false;	/* ??? */
	else if (strcmp (tagName, "TAG_PROGRAM_AUTHOR") == 0)
		t = PTAG_PROGRAM_AUTHOR;
	else if (strcmp (tagName, "TAG_PROGRAM_NAME") == 0)
		t = PTAG_PROGRAM_NAME;
	else if (strcmp (tagName, "TAG_PROGRAM_URL") == 0)
		t = PTAG_PROGRAM_URL;
	else if (strcmp (tagName, "TAG_PROGRAM_VERSION") == 0)
		t = PTAG_PROGRAM_VERSION;

	if (t == PTAG_UNKNOWN)
		return false;
	else
	{
		struct ptagXcmdData data = {
			.fileName = fileName,
			.pattern  = pattern,
			.language = entryLookupField(entry,
						     "language",
						     false),
		};
		return makePtagIfEnabled (t, &data);
	}
}

static bool makeTagEntryFromTagEntry (xcmdPath* path, tagEntry* entry)
{
	tagEntryInfo tag;
	MIOPos      filePosition;

	if (hasPseudoTagPrefix (entry->name))
	{
		if  (isXtagEnabled (XTAG_PSEUDO_TAGS))
			return makePseudoTagEntryFromTagEntry (entry);
		else
			return false;
	}

	memset(&filePosition, 0, sizeof(filePosition));

	// pseudo if (entry->name...);
	initTagEntryFull (&tag, entry->name,
			  entry->address.lineNumber,
			  entryLookupField(entry, "language", true),
			  filePosition,
			  entry->file,
			  entry->kind,
			  ROLE_INDEX_DEFINITION,
			  NULL,
			  NULL,
			  0);

	tag.pattern = entry->address.pattern;

	tag.isFileScope = (bool)entry->fileScope;
	tag.extensionFields.access = entryLookupField(entry, "access", true);
	tag.extensionFields.implementation = entryLookupField(entry, "implementation", true);
	tag.extensionFields.inheritance = entryLookupField(entry, "inherits", true);
	tag.extensionFields.signature = entryLookupField(entry, "signature", true);
	tag.extensionFields.typeRef[0] = entryLookupField(entry, "typeref", true);
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

	const char *kindName = NULL;
	tag.extensionFields.scopeName = entryGetAnyUnpulledField (entry, &kindName, true);
	if (tag.extensionFields.scopeName && kindName)
		tag.extensionFields.scopeKind = lookupKindFromName (path, kindName);

	/* TODO: role */

	makeTagEntry (&tag);
	return true;
}

static bool invokeXcmdPath (const char* const fileName, xcmdPath* path, const langType language)
{
	enum pcoprocError r;
	bool result = false;
	char* argv[4];
	FILE* pp = NULL;

	if (!path->available)
		return false;

	argv[2] = NULL;
	argv[1] = (char * const)fileName;
	argv[0] = vStringValue (path->path);

	Assert (!(getuid() == 0 || geteuid() == 0));
	if (! isSafeExecutable (argv [0]))
		return false;

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

		while (readLineRawWithNoSeek (vline, pp))
		{
			char* line;
			tagEntry entry;

			memset(&entry, 0, sizeof(entry));
			vStringStripNewline (vline);
			line = vStringValue (vline);


			if (parseXcmdPath (line, path, &entry) )
			{
				entryAddField (&entry, "language", getLanguageName (language));

				/* Throw away the input file name returned from the xcmd.
				   Instead we use the input file name arranged
				   (relative or absolute) by ctags main side. */
				entry.file = getInputFileTagPath ();

				if (makeTagEntryFromTagEntry (path, &entry))
					result = true;
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
			return false;
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

#ifdef HAVE_COPROC
extern bool invokeXcmd (const char* const fileName, const langType language)
{
	bool result = false;

	if (language != LANG_IGNORE  &&  language <= SetUpper  &&
		Sets [language].count > 0)
	{
		const pathSet* const set = Sets + language;
		unsigned int i;

		for (i = 0; i < set->count ; ++i)
		{
			xcmdPath* path = set->paths + i;
			if (invokeXcmdPath (fileName, path, language))
				result = true;
		}

	}
	return result;
}
#endif
