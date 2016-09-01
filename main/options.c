/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions to process command line options.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#define _GNU_SOURCE   /* for asprintf */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>  /* to declare isspace () */

#if defined(HAVE_SCANDIR)
#include <dirent.h>
#endif

#include "ctags.h"
#include "debug.h"
#include "field.h"
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "output.h"
#include "parse.h"
#include "ptag.h"
#include "routines.h"
#include "xtag.h"
#include "routines.h"

/*
*   MACROS
*/
#define INVOCATION  "Usage: %s [options] [file(s)]\n"

#define CTAGS_DATA_PATH_ENVIRONMENT "CTAGS_DATA_PATH"
#define CTAGS_LIBEXEC_PATH_ENVIRONMENT "CTAGS_LIBEXEC_PATH"
#define CTAGS_ENVIRONMENT  "CTAGS"
#define ETAGS_ENVIRONMENT  "ETAGS"

#define CTAGS_FILE  "tags"
#define ETAGS_FILE  "TAGS"

#ifndef ETAGS
# define ETAGS	"etags"  /* name which causes default use of to -e */
#endif

/*  The following separators are permitted for list options.
 */
#define EXTENSION_SEPARATOR '.'
#define PATTERN_START '('
#define PATTERN_STOP  ')'
#define IGNORE_SEPARATORS   ", \t\n"

#ifndef DEFAULT_FILE_FORMAT
# define DEFAULT_FILE_FORMAT  2
#endif

#if defined (HAVE_OPENDIR) || defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)
# define RECURSE_SUPPORTED
#endif

#define isCompoundOption(c)  (boolean) (strchr ("fohiILpDb", (c)) != NULL)

#define SUBDIR_OPTLIB "optlib"
#define SUBDIR_PRELOAD "preload"
#define SUBDIR_DRIVERS "drivers"

#define ENTER(STAGE) do {							\
		Assert (Stage <= OptionLoadingStage##STAGE); \
		Stage = OptionLoadingStage##STAGE;	\
	} while (0)
#define ACCEPT(STAGE) (1UL << OptionLoadingStage##STAGE)

/*
*   Data declarations
*/

enum eOptionLimits {
	MaxHeaderExtensions	= 100,  /* maximum number of extensions in -h option */
	MaxSupportedTagFormat = 2
};

typedef struct sOptionDescription {
	int usedByEtags;
	const char *description;
} optionDescription;

typedef void (*parametricOptionHandler) (const char *const option, const char *const parameter);

typedef const struct {
	const char* name;   /* name of option as specified by user */
	parametricOptionHandler handler;  /* routine to handle option */
	boolean initOnly;   /* option must be specified before any files */
	unsigned long acceptableStages;
} parametricOption;

typedef const struct sBooleanOption {
	const char* name;   /* name of option as specified by user */
	boolean* pValue;    /* pointer to option value */
	boolean initOnly;   /* option must be specified before any files */
	unsigned long acceptableStages;
	boolean* (* redirect) (const struct sBooleanOption *const option);
} booleanOption;

/*
*   DATA DEFINITIONS
*/

static boolean NonOptionEncountered = FALSE;
static stringList *OptionFiles;

typedef stringList searchPathList;
static searchPathList *OptlibPathList;
static searchPathList *PreloadPathList;
static searchPathList *DriversPathList;

static stringList* Excluded;
static boolean FilesRequired = TRUE;
static boolean SkipConfiguration;

static const char *const HeaderExtensions [] = {
	"h", "H", "hh", "hpp", "hxx", "h++", "inc", "def", NULL
};

optionValues Option = {
	NULL,       /* -I */
	FALSE,      /* -a */
	FALSE,      /* -B */
	FALSE,      /* -e */
#ifdef MACROS_USE_PATTERNS
	EX_PATTERN, /* -n, --excmd */
#else
	EX_MIX,     /* -n, --excmd */
#endif
	FALSE,      /* -R */
	SO_SORTED,  /* -u, --sort */
	FALSE,      /* -V */
	FALSE,      /* -x */
	.customXfmt = NULL,
	NULL,       /* -L */
	NULL,       /* -o */
	NULL,       /* -h */
	NULL,		/* --config-filename */
	NULL,       /* --etags-include */
	DEFAULT_FILE_FORMAT,/* --format */
#ifdef HAVE_ICONV
	NULL,		/* --input-encoding */
	NULL,		/* --output-encoding */
#endif
	FALSE,      /* --if0 */
	LANG_AUTO,  /* --lang */
	TRUE,       /* --links */
	FALSE,      /* --filter */
	NULL,       /* --filter-terminator */
	FALSE,      /* --tag-relative */
	FALSE,      /* --totals */
	FALSE,      /* --line-directives */
	FALSE,	    /* --print-language */
	FALSE,	    /* --guess-language-eagerly(-G) */
	FALSE,	    /* --quiet */
	FALSE,	    /* --_allow-xcmd-in-homedir */
	FALSE,	    /* --_fatal-warnings */
	.patternLengthLimit = 96,
	.putFieldPrefix = FALSE,
	.maxRecursionDepth = 0xffffffff,
	.machinable = FALSE,
	.withListHeader = TRUE,
#ifdef DEBUG
	0, 0        /* -D, -b */
#endif
};

static OptionLoadingStage Stage = OptionLoadingStageNone;
#define STAGE_ANY ~0UL

/*
-   Locally used only
*/

static optionDescription LongOptionDescription [] = {
 {1,"  -a   Append the tags to an existing tag file."},
#ifdef DEBUG
 {1,"  -b <line>"},
 {1,"       Set break line."},
#endif
 {0,"  -B   Use backward searching patterns (?...?)."},
#ifdef DEBUG
 {1,"  -D <level>"},
 {1,"       Set debug level."},
#endif
 {0,"  -e   Output tag file for use with Emacs."},
 {1,"  -f <name>"},
 {1,"       Write tags to specified file. Value of \"-\" writes tags to stdout"},
 {1,"       [\"tags\"; or \"TAGS\" when -e supplied]."},
 {0,"  -F   Use forward searching patterns (/.../; default)."},
 {1,"  -G   Equivalent to --guess-language-eagerly."},
 {1,"  -h <list>"},
 {1,"       Specify list of file extensions to be treated as include files"},
 {1,"       [\".h.H.hh.hpp.hxx.h++.inc.def\"]."},
 {1,"  -I <list|@file>"},
 {1,"       A list of tokens to be specially handled is read from either the"},
 {1,"       command line or the specified file."},
 {1,"  -L <file>"},
 {1,"       A list of input file names is read from the specified file."},
 {1,"       If specified as \"-\", then standard input is read."},
 {0,"  -n   Equivalent to --excmd=number."},
 {0,"  -N   Equivalent to --excmd=pattern."},
 {1,"  -o   Alternative for -f."},
#ifdef RECURSE_SUPPORTED
 {1,"  -R   Equivalent to --recurse."},
#else
 {1,"  -R   Not supported on this platform."},
#endif
 {0,"  -u   Equivalent to --sort=no."},
 {1,"  -V   Equivalent to --verbose."},
 {1,"  -x   Print a tabular cross reference file to standard output."},
 {1,"  --alias-<LANG>=[+|-]aliasPattern"},
 {1,"      Add a pattern detecting a name, can be used as an alternative name"},
 {1,"      for LANG."},
 {1,"  --append=[yes|no]"},
 {1,"       Should tags should be appended to existing tag file [no]?"},
 {1,"  --config-filename=fileName"},
 {1,"      Use 'fileName' instead of 'ctags' in option file names."},
 {1,"  --data-dir=[+]DIR"},
 {1,"      Add or set DIR to data directory search path."},
 {1,"  --etags-include=file"},
 {1,"      Include reference to 'file' in Emacs-style tag file (requires -e)."},
 {1,"  --exclude=pattern"},
 {1,"      Exclude files and directories matching 'pattern'."},
 {0,"  --excmd=number|pattern|mix"},
#ifdef MACROS_USE_PATTERNS
 {0,"       Uses the specified type of EX command to locate tags [pattern]."},
#else
 {0,"       Uses the specified type of EX command to locate tags [mix]."},
#endif
 {1,"  --extra=[+|-]flags"},
 {1,"      Include extra tag entries for selected information (flags: \"Ffq.\") [F]."},
 {1,"  --fields=[+|-]flags"},
 {1,"      Include selected extension fields (flags: \"afmikKlnsStzZ\") [fks]."},
 {1,"  --fields-<LANG|*>=[+|-]flags"},
 {1,"      Include selected <LANG> own extension fields"},
 {1,"      (flags: --list-fields=<LANG>)."},
 {1,"  --file-scope=[yes|no]"},
 {1,"       Should tags scoped only for a single file (e.g. \"static\" tags)"},
 {1,"       be included in the output [yes]?"},
 {1,"  --filter=[yes|no]"},
 {1,"       Behave as a filter, reading file names from standard input and"},
 {1,"       writing tags to standard output [no]."},
 {1,"  --filter-terminator=string"},
 {1,"       Specify string to print to stdout following the tags for each file"},
 {1,"       parsed when --filter is enabled."},
 {0,"  --format=level"},
#if DEFAULT_FILE_FORMAT == 1
 {0,"       Force output of specified tag file format [1]."},
#else
 {0,"       Force output of specified tag file format [2]."},
#endif
 {1,"  --guess-language-eagerly"},
 {1,"       Guess the language of input file more eagerly"},
 {1,"       (but taking longer time for guessing):"},
 {1,"       o shebang, even if the input file is not executable,"},
 {1,"       o emacs mode specification at the beginning and end of input file, and"},
 {1,"       o vim syntax specification at the end of input file."},
 {1,"  --help"},
 {1,"       Print this option summary."},
 {1,"  --if0=[yes|no]"},
 {1,"       Should code within #if 0 conditional branches be parsed [no]?"},
#ifdef HAVE_ICONV
 {1,"  --input-encoding=encoding"},
 {1,"      Specify encoding of all input files."},
 {1,"  --input-encoding-<LANG>=encoding"},
 {1,"      Specify encoding of the LANG input files."},
#endif
 {1,"  --kinds-<LANG>=[+|-]kinds, or"},
 {1,"  --<LANG>-kinds=[+|-]kinds"},
 {1,"       Enable/disable tag kinds for language <LANG>."},
 {1,"  --langdef=name"},
 {1,"       Define a new language to be parsed with regular expressions."},
 {1,"  --langmap=map(s)"},
 {1,"       Override default mapping of language to input file extension."},
 {1,"  --language-force=language"},
 {1,"       Force all files to be interpreted using specified language."},
 {1,"  --languages=[+|-]list"},
 {1,"       Restrict files scanned for tags to those mapped to languages"},
 {1,"       specified in the comma-separated 'list'. The list can contain any"},
 {1,"       built-in or user-defined language [all]."},
 {1,"  --libexec-dir=[+]DIR"},
 {1,"      Add or set DIR to libexec directory search path."},
 {1,"  --license"},
 {1,"       Print details of software license."},
 {0,"  --line-directives=[yes|no]"},
 {0,"       Should #line directives be processed [no]?"},
 {1,"  --links=[yes|no]"},
 {1,"       Indicate whether symbolic links should be followed [yes]."},
 {1,"  --list-aliases=[language|all]"},
 {1,"       Output list of alias patterns."},
 {1,"  --list-extensions=[language|all]"},
 {1,"       Output list of language extensions in mapping."},
 {1,"  --list-extra"},
 {1,"       Output list of extra tag flags."},
 {1,"  --list-features"},
 {1,"       Output list of features."},
 {1,"  --list-fields=[language|all]"},
 {1,"       Output list of fields. This works with --machinable."},
 {1,"  --list-file-kind"},
 {1,"       List kind letter for file."},
 {1,"  --list-kinds=[language|all]"},
 {1,"       Output a list of all tag kinds for specified language or all."},
 {1,"  --list-kinds-full=[language|all]"},
 {1,"       List the details of all tag kinds for specified language or all"},
 {1,"       For each line, associated language name is printed when \"all\" is"},
 {1,"       specified as language. This works with --machinable."},
 {1,"  --list-languages"},
 {1,"       Output list of supported languages."},
 {1,"  --list-maps=[language|all]"},
 {1,"       Output list of language mappings(both extensions and patterns)."},
 {1,"  --list-patterns=[language|all]"},
 {1,"       Output list of language patterns in mapping."},
 {0,"  --list-pseudo-tags"},
 {0,"       Output list of pseudo tags."},
 {1,"  --list-regex-flags"},
 {1,"       Output list of flags which can be used in a regex parser definition."},
 {1,"  --machinable=[yes|no]"},
 {1,"       Use tab separated representation in --list- option output. [no]"},
 {1,"       --list-extra, --list-fields, and --list-kinds-full support this option."},
 {1,"       Suitable for scripting. Specify before --list-* option."},
 {1,"  --map-<LANG>=[+|-]pattern|extension"},
 {1,"       Set or add(+) a map for <LANG>."},
 {1,"       Unlike --langmap, only one pattern or one extension can be specified"},
 {1,"       at once. Unlike, --langmap adding one affects the map of LANG; it does"},
 {1,"       not affect the maps of the other languages."},
 {1,"  --maxdepth=N"},
#ifdef RECURSE_SUPPORTED
 {1,"       Specify maximum recursion depth."},
#else
 {1,"       Not supported on this platform."},
#endif
 {1,"  --options=file"},
 {1,"       Specify file from which command line options should be read."},
#ifdef HAVE_ICONV
 {1,"  --output-encoding=encoding"},
 {1,"      The encoding to write the tag file in. Defaults to UTF-8 if --input-encoding"},
 {1,"      is specified, otherwise no conversion is performed."},
#endif
 {0,"  --output-format=ctags|etags|xref"
#ifdef HAVE_JANSSON
  "|json"
#endif
 },
 {0,"      Specify the output format. [ctags]"},
 {0,"  --pattern-length-limit=N"},
 {0,"      Cutoff patterns of tag entries after N characters. Disable by setting to 0. [96]"},
 {0,"  --print-language"},
 {0,"       Don't make tags file but just print the guessed language name for"},
 {0,"       input file."},
 {0,"  --pseudo-tags=[+|-]ptag"},
 {0,"  --pseudo-tags=*"},
 {0,"       Enable/disable emitting pseudo tag named ptag."},
 {0,"       if * is given, enable emitting all pseudo tags."},
 {0,"  --put-field-prefix"},
 {0,"       Put \"" CTAGS_FIELD_PREFIX "\" as prefix for the name of fields newly introduced in"},
 {0,"       universal-ctags."},
 {1,"  --quiet=[yes|no]"},
 {0,"       Don't print NOTICE class messages [no]."},
 {1,"  --recurse=[yes|no]"},
#ifdef RECURSE_SUPPORTED
 {1,"       Recurse into directories supplied on command line [no]."},
#else
 {1,"       Not supported on this platform."},
#endif
 {1,"  --regex-<LANG>=/line_pattern/name_pattern/[flags]"},
 {1,"       Define regular expression for locating tags in specific language."},
 {0,"  --sort=[yes|no|foldcase]"},
 {0,"       Should tags be sorted (optionally ignoring case) [yes]?"},
 {0,"  --tag-relative=[yes|no]"},
 {0,"       Should paths be relative to location of tag file [no; yes when -e]?"},
 {1,"  --totals=[yes|no]"},
 {1,"       Print statistics about input and tag files [no]."},
 {1,"  --verbose=[yes|no]"},
 {1,"       Enable verbose messages describing actions on each input file."},
 {1,"  --version"},
 {1,"       Print version identifier to standard output."},
 {1,"  --with-list-header=[yes|no]"},
 {1,"       Preprend the column descriptions in --list- output. [yes]"},
 {1,"       --list-extra, --list-fields, and --list-kinds-full support this option."},
 {1,"       Specify before --list-* option."},
 {1,"       --list-fields, and --list-kinds-full support this option."},
#ifdef HAVE_COPROC
 {1,"  --xcmd-<LANG>=parser_command_path|parser_command_name"},
 {1,"       Define external parser command path or name for specific language."},
#endif
 {1,"  --_allow-xcmd-in-homedir"},
 {1,"       Allow specifying --xcmd-<LANG> option in ~/.ctags and/or ~/.ctags/*."},
 {1,"       By default it is not allowed. This option itself can be specified only"},
 {1,"       in /etc or /usr/local/etc."},
 {1,"  --_echo=msg"},
 {1,"       Echo MSG to standard error. Useful to debug the chain"},
 {1,"       of loading option files."},
 {1,"  --_fatal-warnings"},
 {1,"       Make all warnings fatal."},
 {1,"  --_force-quit=[num]"},
 {1,"       Quit when the option is processed. Useful to debug the chain"},
 {1,"       of loading option files."},
 {1,"  --_list-roles=[[language|all]:[kindletters|*]]"},
 {1,"       Output list of all roles of tag kind(s) specified for language(s)."},
 {1,"       e.g. --_list-roles=Make:I"},
 {1,"  --_xformat=field_format"},
 {1,"       Specify custom format for tabular cross reference (-x)."},
 {1,"       Fields can be specified with letter listed in --list-fields."},
 {1,"       e.g. --_xformat=%10N %10l:%K @ %-20F:%-20n"},
 {1, NULL}
};

static const char* const License1 =
"This program is free software; you can redistribute it and/or\n"
"modify it under the terms of the GNU General Public License\n"
"as published by the Free Software Foundation; either version 2"
"of the License, or (at your option) any later version.\n"
"\n";
static const char* const License2 =
"This program is distributed in the hope that it will be useful,\n"
"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"GNU General Public License for more details.\n"
"\n"
"You should have received a copy of the GNU General Public License\n"
"along with this program; if not, write to the Free Software\n"
"Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.\n";

/*  Contains a set of strings describing the set of "features" compiled into
 *  the code.
 */
static const char *const Features [] = {
#ifdef WIN32
	"win32",
#endif
	"wildcards",		/* Always available on universal ctags */
	"regex",		/* Always available on universal ctags */
#ifndef EXTERNAL_SORT
	"internal-sort",
#endif
#ifdef CUSTOM_CONFIGURATION_FILE
	"custom-conf",
#endif
#if defined (WIN32) && defined (UNIX_PATH_SEPARATOR)
	"unix-path-separator",
#endif
#ifdef HAVE_ICONV
	"multibyte",
#endif
#ifdef DEBUG
	"debug",
#endif
#ifdef HAVE_SCANDIR
	"option-directory",
#endif
#ifdef HAVE_COPROC
	"coproc",
#endif
#ifdef HAVE_LIBXML
	"xpath",
#endif
#ifdef HAVE_JANSSON
	"json",
#endif
	NULL
};

static const char *const StageDescription [] = {
	[OptionLoadingStageNone]   = "not initialized",
	[OptionLoadingStageCustom] = "custom file",
	[OptionLoadingStageDosCnf] = "DOS .cnf file",
	[OptionLoadingStageEtc] = "file under /etc (e.g. ctags.conf)",
	[OptionLoadingStageLocalEtc] = "file under /usr/local/etc (e.g. ctags.conf)",
	[OptionLoadingStageHomeRecursive] = "file(s) under HOME",
	[OptionLoadingStageCurrentRecursive] = "file(s) under the current directory",
	[OptionLoadingStagePreload] = "optlib preload files",
	[OptionLoadingStageEnvVar] = "environment variable",
	[OptionLoadingStageCmdline] = "command line",
};

/*
*   FUNCTION PROTOTYPES
*/
static boolean parseFileOptions (const char *const fileName);
static boolean parseAllConfigurationFilesOptionsInDirectory (const char *const fileName,
							     stringList* const already_loaded_files);

/*
*   FUNCTION DEFINITIONS
*/


static vString* getHome (void)
{
	const char* const home = getenv ("HOME");

	if (home)
		return vStringNewInit (home);
	else
	{
#ifdef MSDOS_STYLE_PATH
		/*
		 * Windows users don't usually set HOME.
		 * The OS sets HOMEDRIVE and HOMEPATH for them.
		 */
		const char* homeDrive = getenv ("HOMEDRIVE");
		const char* homePath = getenv ("HOMEPATH");
		if (homeDrive != NULL && homePath != NULL)
		{
			vString* const windowsHome = vStringNew ();
			vStringCatS (windowsHome, homeDrive);
			vStringCatS (windowsHome, homePath);
			return windowsHome;
		}
#endif
		return NULL;
	}
}

#if defined(_WIN32) && !(defined(__USE_MINGW_ANSI_STDIO) && defined(__MINGW64_VERSION_MAJOR))

/* Some versions of MinGW are missing _vscprintf's declaration, although they
 * still provide the symbol in the import library.
 */
#ifdef __MINGW32__
_CRTIMP int _vscprintf(const char *format, va_list argptr);
#endif

#ifndef va_copy
#define va_copy(dest, src) (dest = src)
#endif

int asprintf(char **strp, const char *fmt, ...)
{
	va_list args;
	va_list args_copy;
	int length;
	size_t size;

	va_start(args, fmt);

	va_copy(args_copy, args);

#ifdef _WIN32
	/* We need to use _vscprintf to calculate the length as vsnprintf returns -1
	 * if the number of characters to write is greater than count.
	 */
	length = _vscprintf(fmt, args_copy);
#else
	char dummy;
	length = vsnprintf(&dummy, sizeof dummy, fmt, args_copy);
#endif

	va_end(args_copy);

	Assert(length >= 0);
	size = length + 1;

	*strp = malloc(size);
	if (!*strp) {
		return -1;
	}

	va_start(args, fmt);
	vsnprintf(*strp, size, fmt, args);
	va_end(args);

	return length;
}
#endif

extern void verbose (const char *const format, ...)
{
	if (Option.verbose)
	{
		va_list ap;
		va_start (ap, format);
		vfprintf (stderr, format, ap);
		va_end (ap);
	}
}

extern void notice (const char *const format, ...)
{
	if (!Option.quiet)
	{
		va_list ap;
		fprintf (stderr, "%s: Notice: ", getExecutableName ());
		va_start (ap, format);
		vfprintf (stderr, format, ap);
		va_end (ap);
		fputs ("\n", stderr);
	}
}


static char *stringCopy (const char *const string)
{
	char* result = NULL;
	if (string != NULL)
		result = eStrdup (string);
	return result;
}

static void freeString (char **const pString)
{
	if (*pString != NULL)
	{
		eFree (*pString);
		*pString = NULL;
	}
}

extern void freeList (stringList** const pList)
{
	if (*pList != NULL)
	{
		stringListDelete (*pList);
		*pList = NULL;
	}
}

extern void setDefaultTagFileName (void)
{
	if (Option.tagFileName != NULL)
		;  /* accept given name */
	else if (Option.etags)
		Option.tagFileName = stringCopy (ETAGS_FILE);
	else
		Option.tagFileName = stringCopy (CTAGS_FILE);
}

extern boolean filesRequired (void)
{
	boolean result = FilesRequired;
	if (Option.recurse)
		result = FALSE;
	return result;
}

extern void checkOptions (void)
{
	const char* notice;
	if (Option.xref && (Option.customXfmt == NULL))
	{
		notice = "xref output";
		if (isXtagEnabled(XTAG_FILE_NAMES))
		{
			error (WARNING, "%s disables file name tags", notice);
			enableXtag (XTAG_FILE_NAMES, FALSE);
		}
	}
	if (Option.append)
	{
		notice = "append mode is not compatible with";
		if (isDestinationStdout ())
			error (FATAL, "%s tags to stdout", notice);
	}
	if (Option.filter)
	{
		notice = "filter mode";
		if (Option.printTotals)
		{
			error (WARNING, "%s disables totals", notice);
			Option.printTotals = FALSE;
		}
		if (Option.tagFileName != NULL)
			error (WARNING, "%s ignores output tag file name", notice);
	}
}

extern langType getLanguageComponentInOption (const char *const option,
					      const char *const prefix)
{
	size_t len;
	langType language;
	const char *lang;

	Assert (prefix && prefix[0]);
	Assert (option);

	len = strlen (prefix);
	if (strncmp (option, prefix, len) != 0)
		return LANG_IGNORE;
	else
	{
		lang = option + len;
		if (lang [0] == '\0')
			return LANG_IGNORE;
	}

	language = getNamedLanguage (lang, 0);
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", lang, option);

	return language;
}

static void setEtagsMode (void)
{
	Option.etags = TRUE;
	Option.sorted = SO_UNSORTED;
	Option.lineDirectives = FALSE;
	Option.tagRelative = TRUE;
	setTagWriter (&etagsWriter);
}

extern void testEtagsInvocation (void)
{
	char* const execName = eStrdup (getExecutableName ());
	char* const etags = eStrdup (ETAGS);
#ifdef CASE_INSENSITIVE_FILENAMES
	toLowerString (execName);
	toLowerString (etags);
#endif
	if (strstr (execName, etags) != NULL)
	{
		verbose ("Running in etags mode\n");
		setEtagsMode ();
	}
	eFree (execName);
	eFree (etags);
}

static void setXrefMode (void)
{
	Option.xref = TRUE;
	setTagWriter (&xrefWriter);
}

#ifdef HAVE_JANSSON
static void setJsonMode (void)
{
	enablePtag (PTAG_JSON_OUTPUT_VERSION, TRUE);
	setTagWriter (&jsonWriter);
}
#endif

/*
 *  Cooked argument parsing
 */

static void parseShortOption (cookedArgs *const args)
{
	args->simple [0] = *args->shortOptions++;
	args->simple [1] = '\0';
	args->item = eStrdup (args->simple);
	if (! isCompoundOption (*args->simple))
		args->parameter = "";
	else if (*args->shortOptions == '\0')
	{
		argForth (args->args);
		if (argOff (args->args))
			args->parameter = NULL;
		else
			args->parameter = argItem (args->args);
		args->shortOptions = NULL;
	}
	else
	{
		args->parameter = args->shortOptions;
		args->shortOptions = NULL;
	}
}

static void parseLongOption (cookedArgs *const args, const char *item)
{
	const char* const equal = strchr (item, '=');
	if (equal == NULL)
	{
		args->item = eStrdup (item);
		args->parameter = "";
	}
	else
	{
		args->item = eStrndup (item, equal - item);
		args->parameter = equal + 1;
	}
	Assert (args->item != NULL);
	Assert (args->parameter != NULL);
}

static void cArgRead (cookedArgs *const current)
{
	char* item;

	Assert (current != NULL);
	if (! argOff (current->args))
	{
		item = argItem (current->args);
		current->shortOptions = NULL;
		Assert (item != NULL);
		if (strncmp (item, "--", (size_t) 2) == 0)
		{
			current->isOption = TRUE;
			current->longOption = TRUE;
			parseLongOption (current, item + 2);
			Assert (current->item != NULL);
			Assert (current->parameter != NULL);
		}
		else if (*item == '-')
		{
			current->isOption = TRUE;
			current->longOption = FALSE;
			current->shortOptions = item + 1;
			parseShortOption (current);
		}
		else
		{
			current->isOption = FALSE;
			current->longOption = FALSE;
			current->item = eStrdup (item);
			current->parameter = NULL;
		}
	}
}

extern cookedArgs* cArgNewFromString (const char* string)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromString (string);
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromArgv (char* const* const argv)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromArgv (argv);
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromFile (FILE* const fp)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromFile (fp);
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromLineFile (FILE* const fp)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromLineFile (fp);
	cArgRead (result);
	return result;
}

extern void cArgDelete (cookedArgs* const current)
{
	Assert (current != NULL);
	argDelete (current->args);
	if (current->item != NULL)
		eFree (current->item);
	memset (current, 0, sizeof (cookedArgs));
	eFree (current);
}

static boolean cArgOptionPending (cookedArgs* const current)
{
	boolean result = FALSE;
	if (current->shortOptions != NULL)
		if (*current->shortOptions != '\0')
			result = TRUE;
	return result;
}

extern boolean cArgOff (cookedArgs* const current)
{
	Assert (current != NULL);
	return (boolean) (argOff (current->args) && ! cArgOptionPending (current));
}

extern boolean cArgIsOption (cookedArgs* const current)
{
	Assert (current != NULL);
	return current->isOption;
}

extern const char* cArgItem (cookedArgs* const current)
{
	Assert (current != NULL);
	return current->item;
}

extern void cArgForth (cookedArgs* const current)
{
	Assert (current != NULL);
	Assert (! cArgOff (current));
	if (current->item != NULL)
		eFree (current->item);
	if (cArgOptionPending (current))
		parseShortOption (current);
	else
	{
		Assert (! argOff (current->args));
		argForth (current->args);
		if (! argOff (current->args))
			cArgRead (current);
		else
		{
			current->isOption = FALSE;
			current->longOption = FALSE;
			current->shortOptions = NULL;
			current->item = NULL;
			current->parameter = NULL;
		}
	}
}

/*
 *  File extension and language mapping
 */

static void addExtensionList (
		stringList *const slist, const char *const elist, const boolean clear)
{
	char *const extensionList = eStrdup (elist);
	const char *extension = NULL;
	boolean first = TRUE;

	if (clear)
	{
		verbose ("      clearing\n");
		stringListClear (slist);
	}
	verbose ("      adding: ");
	if (elist != NULL  &&  *elist != '\0')
	{
		extension = extensionList;
		if (elist [0] == EXTENSION_SEPARATOR)
			++extension;
	}
	while (extension != NULL)
	{
		char *separator = strchr (extension, EXTENSION_SEPARATOR);
		if (separator != NULL)
			*separator = '\0';
		verbose ("%s%s", first ? "" : ", ",
				*extension == '\0' ? "(NONE)" : extension);
		stringListAdd (slist, vStringNewInit (extension));
		first = FALSE;
		if (separator == NULL)
			extension = NULL;
		else
			extension = separator + 1;
	}
	BEGIN_VERBOSE(vfp);
	{
		fprintf (vfp, "\n      now: ");
		stringListPrint (slist, vfp);
		putc ('\n', vfp);
	}
	END_VERBOSE();
}

static boolean isFalse (const char *parameter)
{
	return (boolean) (
		strcasecmp (parameter, "0"  ) == 0  ||
		strcasecmp (parameter, "n"  ) == 0  ||
		strcasecmp (parameter, "no" ) == 0  ||
		strcasecmp (parameter, "off") == 0);
}

static boolean isTrue (const char *parameter)
{
	return (boolean) (
		strcasecmp (parameter, "1"  ) == 0  ||
		strcasecmp (parameter, "y"  ) == 0  ||
		strcasecmp (parameter, "yes") == 0  ||
		strcasecmp (parameter, "on" ) == 0);
}

/*  Determines whether the specified file name is considered to be a header
 *  file for the purposes of determining whether enclosed tags are global or
 *  static.
 */
extern boolean isIncludeFile (const char *const fileName)
{
	boolean result = FALSE;
	const char *const extension = fileExtension (fileName);
	if (Option.headerExt != NULL)
		result = stringListExtensionMatched (Option.headerExt, extension);
	return result;
}

/*
 *  Specific option processing
 */

 static void processConfigFilenameOption (
		const char *const option CTAGS_ATTR_UNUSED, const char *const parameter)
 {
	freeString (&Option.configFilename);
	Option.configFilename = stringCopy (parameter);
 }

static void processEtagsInclude (
		const char *const option, const char *const parameter)
{
	if (! Option.etags)
		error (FATAL, "Etags must be enabled to use \"%s\" option", option);
	else
	{
		vString *const file = vStringNewInit (parameter);
		if (Option.etagsInclude == NULL)
			Option.etagsInclude = stringListNew ();
		stringListAdd (Option.etagsInclude, file);
		FilesRequired = FALSE;
	}
}

static void processExcludeOption (
		const char *const option CTAGS_ATTR_UNUSED, const char *const parameter)
{
	const char *const fileName = parameter + 1;
	if (parameter [0] == '\0')
		freeList (&Excluded);
	else if (parameter [0] == '@')
	{
		stringList* const sl = stringListNewFromFile (fileName);
		if (sl == NULL)
			error (FATAL | PERROR, "cannot open \"%s\"", fileName);
		if (Excluded == NULL)
			Excluded = sl;
		else
			stringListCombine (Excluded, sl);
		verbose ("    adding exclude patterns from %s\n", fileName);
	}
	else
	{
		vString *const item = vStringNewInit (parameter);
		if (Excluded == NULL)
			Excluded = stringListNew ();
		stringListAdd (Excluded, item);
		verbose ("    adding exclude pattern: %s\n", parameter);
	}
}

extern boolean isExcludedFile (const char* const name)
{
	const char* base = baseFilename (name);
	boolean result = FALSE;
	if (Excluded != NULL)
	{
		result = stringListFileMatched (Excluded, base);
		if (! result  &&  name != base)
			result = stringListFileMatched (Excluded, name);
	}
	return result;
}

static void processExcmdOption (
		const char *const option, const char *const parameter)
{
	switch (*parameter)
	{
		case 'm': Option.locate = EX_MIX;     break;
		case 'n': Option.locate = EX_LINENUM; break;
		case 'p': Option.locate = EX_PATTERN; break;
		default:
			error (FATAL, "Invalid value for \"%s\" option", option);
			break;
	}
}

static void resetXtags (boolean mode)
{
	int i;
	for (i = 0; i < XTAG_COUNT; i++)
		enableXtag (i, mode);
}

static void processExtraTagsOption (
		const char *const option, const char *const parameter)
{
	xtagType t;
	const char *p = parameter;
	boolean mode = TRUE;
	int c;
	static vString *longName;
	boolean inLongName = FALSE;
	const char *x;

	if (*p == '*')
	{
		resetXtags (TRUE);
		p++;
	}
	else if (*p != '+'  &&  *p != '-')
		resetXtags (FALSE);

	longName = vStringNewOrClear (longName);

	while ((c = *p++) != '\0')
	{
		switch (c)
		{
		case '+':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = TRUE;
			break;
		case '-':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = FALSE;
			break;
		case '{':
			if (inLongName)
				error(FATAL,
				      "unexpected character in extra specification: \'%c\'",
				      c);
			inLongName = TRUE;
			break;
		case '}':
			if (!inLongName)
				error(FATAL,
				      "unexpected character in extra specification: \'%c\'",
				      c);
			x = vStringValue (longName);
			t = getXtagTypeForName (x);

			if (t == XTAG_UNKNOWN)
				error(WARNING, "Unsupported parameter '{%s}' for \"%s\" option",
				      x, option);
			else
				enableXtag (t, mode);

			inLongName = FALSE;
			vStringClear (longName);
			break;
		default:
			if (inLongName)
				vStringPut (longName, c);
			else
			{
				t = getXtagTypeForLetter (c);
				if (t == XTAG_UNKNOWN)
					error(WARNING, "Unsupported parameter '%c' for \"%s\" option",
					      c, option);
				else
					enableXtag (t, mode);
			}
			break;
		}
	}
}

static void resetFieldsOption (langType lang, boolean mode)
{
	int i;

	for (i = 0; i < countFields (); ++i)
		if ((lang == LANG_AUTO) || (lang == getFieldOwner (i)))
			enableField (i, mode, FALSE);
}

static void processFieldsOption (
		const char *const option, const char *const parameter)
{
	const char *p = parameter;
	boolean mode = TRUE;
	int c;
	fieldType t;

	static vString * longName;
	boolean inLongName = FALSE;

	longName = vStringNewOrClear (longName);

	if (*p == '*')
	{
		resetFieldsOption (LANG_IGNORE, TRUE);
		p++;
	}
	else if (*p != '+'  &&  *p != '-')
		resetFieldsOption (LANG_IGNORE, FALSE);

	while ((c = *p++) != '\0') switch (c)
	{
		case '+':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = TRUE;
			break;
		case '-':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = FALSE;
			break;
		case '{':
			if (inLongName)
				error(FATAL,
				      "unexpected character in field specification: \'%c\'",
				      c);
			inLongName = TRUE;
			break;
		case '}':
			if (!inLongName)
				error(FATAL,
				      "unexpected character in field specification: \'%c\'",
				      c);

			{
				const char *f = vStringValue (longName);
				t = getFieldTypeForNameAndLanguage (f, LANG_IGNORE);
			}

			if (t == FIELD_UNKNOWN)
				error(FATAL, "nosuch field: \'%s\'", vStringValue (longName));

			enableField (t, mode, TRUE);

			inLongName = FALSE;
			vStringClear (longName);
			break;
		default :
			if (inLongName)
				vStringPut (longName, c);
			else
			{
				t = getFieldTypeForOption (c);
				if (t == FIELD_UNKNOWN)
					error(WARNING, "Unsupported parameter '%c' for \"%s\" option",
					      c, option);
				else
					enableField (t, mode, TRUE);
			}
			break;
	}
}

static void processFilterTerminatorOption (
		const char *const option CTAGS_ATTR_UNUSED, const char *const parameter)
{
	freeString (&Option.filterTerminator);
	Option.filterTerminator = stringCopy (parameter);
}

static void processFormatOption (
		const char *const option, const char *const parameter)
{
	unsigned int format;

	if (sscanf (parameter, "%u", &format) < 1)
		error (FATAL, "Invalid value for \"%s\" option",option);
	else if (format <= (unsigned int) MaxSupportedTagFormat)
		Option.tagFileFormat = format;
	else
		error (FATAL, "Unsupported value for \"%s\" option", option);
}

#ifdef HAVE_ICONV
static void processInputEncodingOption(const char *const option,
				const char *const parameter)
{
	if (Option.inputEncoding)
		eFree (Option.inputEncoding);
	else
	{
		if (!Option.outputEncoding)
			Option.outputEncoding = eStrdup("UTF-8");
	}
	Option.inputEncoding = eStrdup(parameter);
}

static void processOutputEncodingOption(const char *const option,
				const char *const parameter)
{
	if (Option.outputEncoding)
		eFree (Option.outputEncoding);
	Option.outputEncoding = eStrdup(parameter);
}
#endif

static void printInvocationDescription (void)
{
	printf (INVOCATION, getExecutableName ());
}

static void printOptionDescriptions (const optionDescription *const optDesc)
{
	int i;
	for (i = 0 ; optDesc [i].description != NULL ; ++i)
	{
		if (! Option.etags || optDesc [i].usedByEtags)
			puts (optDesc [i].description);
	}
}

static void printFeatureList (void)
{
	int i;

	for (i = 0 ; Features [i] != NULL ; ++i)
	{
		if (i == 0)
			printf ("  Optional compiled features: ");
		if (strcmp (Features [i], "regex") != 0 || checkRegex ())
			printf ("%s+%s", (i>0 ? ", " : ""), Features [i]);
#ifdef CUSTOM_CONFIGURATION_FILE
		if (strcmp (Features [i], "custom-conf") == 0)
			printf ("=%s", CUSTOM_CONFIGURATION_FILE);
#endif
	}
	if (i > 0)
		putchar ('\n');
}


static void processListFeaturesOption(const char *const option CTAGS_ATTR_UNUSED,
				      const char *const parameter CTAGS_ATTR_UNUSED)
{
	int i;

	for (i = 0 ; Features [i] != NULL ; ++i)
		if (strcmp (Features [i], "regex") != 0 || checkRegex ())
			printf ("%s\n", Features [i]);
	if (i == 0)
		putchar ('\n');
	exit (0);
}

static void processListFieldsOption(const char *const option CTAGS_ATTR_UNUSED,
				    const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
	{
		initializeParser (LANG_AUTO);
		printFields (LANG_AUTO);
	}
	else
	{
		langType language = getNamedLanguage (parameter, 0);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
		{
			initializeParser (language);
			printFields (language);
		}

	}
	exit (0);
}

static void printProgramIdentification (void)
{
	if ((ctags_repoinfo == NULL)
	    || (strcmp (ctags_repoinfo, PROGRAM_VERSION) == 0))
		printf ("%s %s, %s %s\n",
			PROGRAM_NAME, PROGRAM_VERSION,
			PROGRAM_COPYRIGHT, AUTHOR_NAME);
	else
		printf ("%s %s(%s), %s %s\n",
			PROGRAM_NAME, PROGRAM_VERSION, ctags_repoinfo,
			PROGRAM_COPYRIGHT, AUTHOR_NAME);
	printf ("Universal Ctags is derived from Exuberant Ctags.\n");
	printf ("Exuberant Ctags 5.8, Copyright (C) 1996-2009 Darren Hiebert\n");

	printf ("  Compiled: %s, %s\n", __DATE__, __TIME__);
	printf ("  URL: %s\n", PROGRAM_URL);

	printFeatureList ();
}

static void processHelpOption (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	printProgramIdentification ();
	putchar ('\n');
	printInvocationDescription ();
	putchar ('\n');
	printOptionDescriptions (LongOptionDescription);
	exit (0);
}

static void processLanguageForceOption (
		const char *const option, const char *const parameter)
{
	langType language;
	if (strcasecmp (parameter, "auto") == 0)
		language = LANG_AUTO;
	else
		language = getNamedLanguage (parameter, 0);

	if (strcmp (option, "lang") == 0  ||  strcmp (option, "language") == 0)
		error (WARNING,
			   "\"--%s\" option is obsolete; use \"--language-force\" instead",
			   option);
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
	else
		Option.language = language;
}
static char* skipPastMap (char* p)
{
	while (*p != EXTENSION_SEPARATOR  &&
			*p != PATTERN_START  &&  *p != ','  &&  *p != '\0')
		++p;
	return p;
}

/* Parses the mapping beginning at `map', adds it to the language map, and
 * returns first character past the map.
 */
static char* extractMapFromParameter (const langType language,
				      char* parameter,
				      char** tail,
				      boolean* pattern_p,
				      char* (* skip) (char *))
{
	char* p = NULL;
	const char first = *parameter;
	char  tmp;
	char* result;

	if (first == EXTENSION_SEPARATOR)  /* extension map */
	{
		*pattern_p = FALSE;

		++parameter;
		p = (* skip) (parameter);
		if (*p == '\0')
		{
			result = eStrdup (parameter);
			*tail = parameter + strlen (parameter);
			return result;
		}
		else
		{
			tmp = *p;
			*p = '\0';
			result = eStrdup (parameter);
			*p = tmp;
			*tail = p;
			return result;
		}
	}
	else if (first == PATTERN_START)  /* pattern map */
	{
		*pattern_p = TRUE;

		++parameter;
		for (p = parameter  ;  *p != PATTERN_STOP  &&  *p != '\0'  ;  ++p)
		{
			if (*p == '\\'  &&  *(p + 1) == PATTERN_STOP)
				++p;
		}
		if (*p == '\0')
			error (FATAL, "Unterminated file name pattern for %s language",
			   getLanguageName (language));
		else
		{
			tmp = *p;
			*p = '\0';
			result = eStrdup (parameter);
			*p = tmp;
			*tail = p + 1;
			return result;
		}
	}

	return NULL;
}

static char* addLanguageMap (const langType language, char* map_parameter,
			     boolean exclusiveInAllLanguages)
{
	char* p = NULL;
	boolean pattern_p;
	char* map;

	map = extractMapFromParameter (language, map_parameter, &p, &pattern_p, skipPastMap);
	if (map && pattern_p == FALSE)
		addLanguageExtensionMap (language, map, exclusiveInAllLanguages);
	else if (map && pattern_p == TRUE)
		addLanguagePatternMap (language, map, exclusiveInAllLanguages);
	else
		error (FATAL, "Badly formed language map for %s language",
				getLanguageName (language));

	if (map)
		eFree (map);
	return p;
}

static char* removeLanguageMap (const langType language, char* map_parameter)
{
	char* p = NULL;
	boolean pattern_p;
	char* map;

	map = extractMapFromParameter (language, map_parameter, &p, &pattern_p, skipPastMap);
	if (map && pattern_p == FALSE)
		removeLanguageExtensionMap (language, map);
	else if (map && pattern_p == TRUE)
		removeLanguagePatternMap (language, map);
	else
		error (FATAL, "Badly formed language map for %s language",
		       getLanguageName (language));

	if (map)
		eFree (map);
	return p;
}

static char* processLanguageMap (char* map)
{
	char* const separator = strchr (map, ':');
	char* result = NULL;
	if (separator != NULL)
	{
		langType language;
		char *list = separator + 1;
		boolean clear = FALSE;
		*separator = '\0';
		language = getNamedLanguage (map, 0);
		if (language != LANG_IGNORE)
		{
			const char *const deflt = "default";
			char* p;
			if (*list == '+')
				++list;
			else
				clear = TRUE;
			for (p = list  ;  *p != ','  &&  *p != '\0'  ;  ++p)  /*no-op*/ ;
			if ((size_t) (p - list) == strlen (deflt) &&
				strncasecmp (list, deflt, p - list) == 0)
			{
				verbose ("    Restoring default %s language map: ", getLanguageName (language));
				installLanguageMapDefault (language);
				list = p;
			}
			else
			{
				if (clear)
				{
					verbose ("    Setting %s language map:", getLanguageName (language));
					clearLanguageMap (language);
				}
				else
					verbose ("    Adding to %s language map:", getLanguageName (language));
				while (list != NULL  &&  *list != '\0'  &&  *list != ',')
					list = addLanguageMap (language, list, TRUE);
				verbose ("\n");
			}
			if (list != NULL  &&  *list == ',')
				++list;
			result = list;
		}
	}
	return result;
}

static void processLanguageMapOption (
		const char *const option, const char *const parameter)
{
	char *const maps = eStrdup (parameter);
	char *map = maps;

	if (strcmp (parameter, "default") == 0)
	{
		verbose ("    Restoring default language maps:\n");
		installLanguageMapDefaults ();
	}
	else while (map != NULL  &&  *map != '\0')
	{
		char* const next = processLanguageMap (map);
		if (next == NULL)
			error (WARNING, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		map = next;
	}
	eFree (maps);
}

static void processLanguagesOption (
		const char *const option, const char *const parameter)
{
	char *const langs = eStrdup (parameter);
	enum { Add, Remove, Replace } mode = Replace;
	boolean first = TRUE;
	char *lang = langs;
	const char* prefix = "";
	verbose ("    Enabled languages: ");
	while (lang != NULL)
	{
		char *const end = strchr (lang, ',');
		if (lang [0] == '+')
		{
			++lang;
			mode = Add;
			prefix = "+ ";
		}
		else if (lang [0] == '-')
		{
			++lang;
			mode = Remove;
			prefix = "- ";
		}
		if (mode == Replace)
			enableLanguages (FALSE);
		if (end != NULL)
			*end = '\0';
		if (lang [0] != '\0')
		{
			if (strcmp (lang, "all") == 0)
				enableLanguages ((boolean) (mode != Remove));
			else
			{
				const langType language = getNamedLanguage (lang, 0);
				if (language == LANG_IGNORE)
					error (WARNING, "Unknown language \"%s\" in \"%s\" option", lang, option);
				else
					enableLanguage (language, (boolean) (mode != Remove));
			}
			verbose ("%s%s%s", (first ? "" : ", "), prefix, lang);
			prefix = "";
			first = FALSE;
			if (mode == Replace)
				mode = Add;
		}
		lang = (end != NULL ? end + 1 : NULL);
	}
	verbose ("\n");
	eFree (langs);
}

extern boolean processMapOption (
			const char *const option, const char *const parameter)
{
	langType language;
	const char* spec;
	char* map_parameter;
	boolean clear = FALSE;
	char op;

	language = getLanguageComponentInOption (option, "map-");
	if (language == LANG_IGNORE)
		return FALSE;

	if (parameter == NULL || parameter [0] == '\0')
		error (FATAL, "no parameter is given for %s", option);

	spec = parameter;
	if (*spec == '+' || *spec == '-')
	{
		op = *spec;
		spec++;
	}
	else
	{
		op = '\0';
		clear = TRUE;
	}

	if (clear)
	{
		verbose ("    Setting %s language map:", getLanguageName (language));
		clearLanguageMap (language);
		op = '+';
	}
	else
		verbose ("    %s %s %s %s language map:",
			 op == '+'? "Adding": "Removing",
			 spec,
			 op == '+'? "to": "from",
			 getLanguageName (language));
	map_parameter = eStrdup (spec);

	if (op == '+')
		addLanguageMap (language, map_parameter, FALSE);
	else if (op == '-')
		removeLanguageMap (language, map_parameter);
	else
		Assert ("Should not reach here" == NULL);

	eFree (map_parameter);
	verbose ("\n");

	return TRUE;
}

static void processLicenseOption (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	printProgramIdentification ();
	puts ("");
	puts (License1);
	puts (License2);
	exit (0);
}

static void processListAliasesOption (
		const char *const option, const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
		printLanguageAliases (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter, 0);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageAliases (language);
	}
	exit (0);
}

static void processListExtraOption (
		const char *const option CTAGS_ATTR_UNUSED, const char *const parameter CTAGS_ATTR_UNUSED)
{
	printXtags ();
	exit (0);
}

static void processListFileKindOption (
		const char *const option, const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
		printLanguageFileKind (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter, 0);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageFileKind (language);
	}
	exit (0);
}

static void processListKindsOption (
		const char *const option, const char *const parameter)
{
	boolean print_all = (strcmp (option, "list-kinds-full") == 0)? TRUE: FALSE;

	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
		printLanguageKinds (LANG_AUTO, print_all);
	else
	{
		langType language = getNamedLanguage (parameter, 0);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageKinds (language, print_all);
	}
	exit (0);
}

static void processListMapsOptionForType (const char *const option CTAGS_ATTR_UNUSED,
					  const char *const  parameter,
					  langmapType type)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
		printLanguageMaps (LANG_AUTO, type);
	else
	{
		langType language = getNamedLanguage (parameter, 0);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageMaps (language, type);
	}
	exit (0);
}

static void processListExtensionsOption (const char *const option,
					 const char *const parameter)
{
	processListMapsOptionForType (option, parameter, LMAP_EXTENSION);
}

static void processListPatternsOption (const char *const option,
				       const char *const parameter)
{
	processListMapsOptionForType (option, parameter, LMAP_PATTERN);
}

static void processListMapsOption (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	processListMapsOptionForType (option, parameter, LMAP_ALL);
}

static void processListLanguagesOption (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	printLanguageList ();
	exit (0);
}

static void processListPseudoTagsOptions (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	int i;
	for (i = 0; i < PTAG_COUNT; i++)
		printPtag (i);
	exit (0);
}

static void processListRegexFlagsOptions (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	printRegexFlags ();
	exit (0);
}

static void processListRolesOptions (const char *const option CTAGS_ATTR_UNUSED,
				     const char *const parameter)
{
	const char* sep;
	const char *kindletters;
	langType lang;


	if (parameter == NULL || parameter[0] == '\0')
	{
		printLanguageRoles (LANG_AUTO, "*");
		exit (0);
	}

	sep = strchr (parameter, '.');

	if (sep == NULL || sep [1] == '\0')
	{
		vString* vstr = vStringNewInit (parameter);
		vStringCatS (vstr, (sep? "*": ".*"));
		processListRolesOptions (option, vStringValue (vstr));
		/* The control should never reache here. */
	}

	kindletters = sep + 1;
	if (strncmp (parameter, "all.", 4) == 0
	    || strncmp (parameter, "*.", 1) == 0
	    || strncmp (parameter, ".", 1) == 0)
		lang = LANG_AUTO;
	else
	{
		lang = getNamedLanguage (parameter, sep - parameter);
		if (lang == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\"", parameter, option);
	}
	printLanguageRoles (lang, kindletters);
	exit (0);
}
static void freeSearchPathList (searchPathList** pathList)
{
	stringListClear (*pathList);
	stringListDelete (*pathList);
	*pathList = NULL;
}

static void verboseSearchPathList (const searchPathList* pathList, const char *const varname)
{
	unsigned int i;

	verbose ("Install %s:\n", varname);
	for (i = 0; i < stringListCount (pathList); ++i)
		verbose ("  %s\n", vStringValue (stringListItem (pathList, i)));
}

static vString* expandOnSearchPathList (searchPathList *pathList, const char* leaf,
					boolean (* check) (const char *const))
{
	unsigned int i;

	for (i = 0; i < stringListCount (pathList); ++i)
	{
		const char* const body = vStringValue (stringListItem (pathList, i));
		char* tmp = combinePathAndFile (body, leaf);

		if ((* check) (tmp))
		{
			vString *r = vStringNewOwn (tmp);
			return r;
		}
		else
			eFree (tmp);
	}
	return NULL;
}

static boolean isDirectory (const char *const dirName)
{
	fileStatus *status = eStat (dirName);
	return status->exists && status->isDirectory;
}

static vString* expandOnOptlibPathList (const char* leaf)
{
	vString* r;
	vString* leaf_with_suffix;

	leaf_with_suffix = vStringNewInit (leaf);
	vStringCatS (leaf_with_suffix, ".d");

	r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
				    isDirectory);

	if (!r)
	{
		vStringCopyS (leaf_with_suffix, leaf);
		vStringCatS (leaf_with_suffix, ".conf");
		r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
					    doesFileExist);
	}

	if (!r)
	{
		vStringCopyS (leaf_with_suffix, leaf);
		vStringCatS (leaf_with_suffix, ".ctags");
		r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
					    doesFileExist);
	}

#ifdef MSDOS_STYLE_PATH
	if (!r)
	{
		vStringCopyS (leaf_with_suffix, leaf);
		vStringCatS (leaf_with_suffix, ".cnf");
		r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
					    doesFileExist);
	}
#endif

	vStringDelete (leaf_with_suffix);

	return r;
}

extern vString* expandOnDriversPathList (const char* leaf)
{
	return expandOnSearchPathList (DriversPathList, leaf, doesExecutableExist);
}

static void processOptionFile (
		const char *const option, const char *const parameter)
{
	const char* path;
	vString* vpath = NULL;
	fileStatus *status;

	if (parameter [0] == '\0')
		error (FATAL, "no option file supplied for \"%s\"", option);

	if (parameter [0] != '/' && parameter [0] != '.')
	{
		vpath = expandOnOptlibPathList (parameter);
		path = vpath? vStringValue (vpath): parameter;
	}
	else
		path = parameter;

	status = eStat (path);
	if (!status->exists)
	{
		error (FATAL | PERROR, "cannot stat \"%s\"", path);
	}
	else if (status->isDirectory)
	{
		if (!parseAllConfigurationFilesOptionsInDirectory (path, NULL))
			error (FATAL | PERROR, "cannot open option directory \"%s\"", path);
	}
	else
	{
		if (!parseFileOptions (path))
			error (FATAL | PERROR, "cannot open option file \"%s\"", path);
	}

	eStatFree (status);
	if (vpath)
		vStringDelete (vpath);
}

static void processOutputFormat (const char *const option CTAGS_ATTR_UNUSED,
				 const char *const parameter)
{
	if (parameter [0] == '\0')
		error (FATAL, "no output format name supplied for \"%s\"", option);

	if (strcmp (parameter, "ctags") == 0)
		;
	else if (strcmp (parameter, "etags") == 0)
		setEtagsMode ();
	else if (strcmp (parameter, "xref") == 0)
		setXrefMode ();
#ifdef HAVE_JANSSON
	else if (strcmp (parameter, "json") == 0)
		setJsonMode ();
#endif
	else
		error (FATAL, "unknown output format name supplied for \"%s=%s\"", option, parameter);
}

static void processPseudoTags (const char *const option CTAGS_ATTR_UNUSED,
			       const char *const parameter)
{
	const char *p = parameter;
	boolean s;
	ptagType t;

	if (*p == '*')
	{
		int i;
		for (i = 0; i < PTAG_COUNT; i++)
			enablePtag (i, TRUE);
		return;
	}

	if (*p == '-')
	{
		s= FALSE;
		p++;
	}
	else if (*p == '+')
	{
		s = TRUE;
		p++;
	}
	else
	{
		unsigned int i;

		s = TRUE;
		for (i = 0; i < PTAG_COUNT; i++)
			enablePtag (i, FALSE);
		if (*p == '\0')
			return;
	}

	t = getPtagTypeForName (p);
	if (t == PTAG_UNKNOWN)
		error (FATAL, "Unknown pseudo tag name: %s", p);

	enablePtag (t, s);
}

static void processSortOption (
		const char *const option, const char *const parameter)
{
	if (isFalse (parameter))
		Option.sorted = SO_UNSORTED;
	else if (isTrue (parameter))
		Option.sorted = SO_SORTED;
	else if (strcasecmp (parameter, "f") == 0 ||
			strcasecmp (parameter, "fold") == 0 ||
			strcasecmp (parameter, "foldcase") == 0)
		Option.sorted = SO_FOLDSORTED;
	else
		error (FATAL, "Invalid value for \"%s\" option", option);
}

static void installHeaderListDefaults (void)
{
	Option.headerExt = stringListNewFromArgv (HeaderExtensions);

	BEGIN_VERBOSE(vfp);
	{
		fprintf (vfp, "    Setting default header extensions: ");
		stringListPrint (Option.headerExt, vfp);
		putc ('\n', vfp);
	}
	END_VERBOSE();
}

static void processHeaderListOption (const int option, const char *parameter)
{
	/*  Check to make sure that the user did not enter "ctags -h *.c"
	 *  by testing to see if the list is a filename that exists.
	 */
	if (doesFileExist (parameter))
		error (FATAL, "-%c: Invalid list", option);
	if (strcmp (parameter, "default") == 0)
		installHeaderListDefaults ();
	else
	{
		boolean clear = TRUE;

		if (parameter [0] == '+')
		{
			++parameter;
			clear = FALSE;
		}
		if (Option.headerExt == NULL)
			Option.headerExt = stringListNew ();
		verbose ("    Header Extensions:\n");
		addExtensionList (Option.headerExt, parameter, clear);
	}
}

/*
 *  Token ignore processing
 */

/*  Determines whether or not "name" should be ignored, per the ignore list.
 */
extern boolean isIgnoreToken (
		const char *const name, boolean *const pIgnoreParens,
		const char **const replacement)
{
	boolean result = FALSE;

	if (Option.ignore != NULL)
	{
		const size_t nameLen = strlen (name);
		unsigned int i;

		if (pIgnoreParens != NULL)
			*pIgnoreParens = FALSE;

		for (i = 0  ;  i < stringListCount (Option.ignore)  ;  ++i)
		{
			vString *token = stringListItem (Option.ignore, i);

			if (strncmp (vStringValue (token), name, nameLen) == 0)
			{
				const size_t tokenLen = vStringLength (token);

				if (nameLen == tokenLen)
				{
					result = TRUE;
					break;
				}
				else if (tokenLen == nameLen + 1  &&
						vStringChar (token, tokenLen - 1) == '+')
				{
					result = TRUE;
					if (pIgnoreParens != NULL)
						*pIgnoreParens = TRUE;
					break;
				}
				else if (vStringChar (token, nameLen) == '=')
				{
					if (replacement != NULL)
						*replacement = vStringValue (token) + nameLen + 1;
					break;
				}
			}
		}
	}
	return result;
}

static void saveIgnoreToken (vString *const ignoreToken)
{
	if (Option.ignore == NULL)
		Option.ignore = stringListNew ();
	stringListAdd (Option.ignore, ignoreToken);
	verbose ("    ignore token: %s\n", vStringValue (ignoreToken));
}

static void readIgnoreList (const char *const list)
{
	char* newList = stringCopy (list);
	const char *token = strtok (newList, IGNORE_SEPARATORS);

	while (token != NULL)
	{
		vString *const entry = vStringNewInit (token);

		saveIgnoreToken (entry);
		token = strtok (NULL, IGNORE_SEPARATORS);
	}
	eFree (newList);
}

static void addIgnoreListFromFile (const char *const fileName)
{
	stringList* tokens = stringListNewFromFile (fileName);
	if (tokens == NULL)
		error (FATAL | PERROR, "cannot open \"%s\"", fileName);
	if (Option.ignore == NULL)
		Option.ignore = tokens;
	else
		stringListCombine (Option.ignore, tokens);
}

static void processIgnoreOption (const char *const list)
{
	if (strchr ("@./\\", list [0]) != NULL)
	{
		const char* fileName = (*list == '@') ? list + 1 : list;
		addIgnoreListFromFile (fileName);
	}
#if defined (WIN32)
	else if (isalpha (list [0])  &&  list [1] == ':')
		addIgnoreListFromFile (list);
#endif
	else if (strcmp (list, "-") == 0)
	{
		freeList (&Option.ignore);
		verbose ("    clearing list\n");
	}
	else
		readIgnoreList (list);
}

static void processEchoOption (const char *const option, const char *const parameter)
{
	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "Something message is needed for \"%s\" option", option);
	notice ("%s", parameter);
}

static void processForceQuitOption (const char *const option CTAGS_ATTR_UNUSED,
				    const char *const parameter)
{
	int s;
	if (parameter == NULL || parameter[0] == '\0' || !strToInt(parameter, 0, &s))
		s = 0;
	exit (s);
}

static void processVersionOption (
		const char *const option CTAGS_ATTR_UNUSED,
		const char *const parameter CTAGS_ATTR_UNUSED)
{
	printProgramIdentification ();
	exit (0);
}

static void processXformatOption (const char *const option CTAGS_ATTR_UNUSED,
				  const char *const parameter)
{
	if (Option.customXfmt)
		fmtDelete (Option.customXfmt);

	Option.customXfmt = fmtNew (parameter);
}

static void resetPathList (searchPathList** pathList, const char *const varname)
{
	freeSearchPathList (pathList);
	verbose ("Reset %s\n", varname);
	*pathList = stringListNew ();
}

static void resetDataPathList (void)
{
	resetPathList (&OptlibPathList, "OptlibPathList");
}

static void resetLibexecPathList (void)
{
	resetPathList (&DriversPathList, "DriversPathList");
}

static void appendToPathList (const char *const dir, const char *const subdir, searchPathList* const pathList, const char *const varname,
				   boolean report_in_verboe, const char* const action)
{
	char* path;

	path = combinePathAndFile (dir, subdir);
	if (report_in_verboe)
		verbose ("%s %s to %s\n", action, path, varname);
	stringListAdd (pathList, vStringNewOwn (path));
}

static void prependToPathList (const char *const dir, const char *const subdir, searchPathList* const pathList, const char *const varname,
				    boolean report_in_verboe, const char* const action)
{
	stringListReverse (pathList);
	appendToPathList(dir, subdir, pathList, varname, report_in_verboe, action);
	stringListReverse (pathList);

}

static void appendToDataPathList (const char *const dir, boolean from_cmdline)
{
	appendToPathList (dir, SUBDIR_OPTLIB, OptlibPathList, "OptlibPathList",
			       from_cmdline, from_cmdline? "Append": NULL);

	if (!from_cmdline)
		appendToPathList (dir, SUBDIR_PRELOAD, PreloadPathList, "PreloadPathList",
				       FALSE, NULL);
}

static void appendToLibexecPathList (const char *const dir, boolean from_cmdline)
{
	appendToPathList (dir, SUBDIR_DRIVERS, DriversPathList, "DriversPathList",
			       from_cmdline, from_cmdline? "Append": NULL);
}

static void prependToDataPathList (const char *const dir, boolean from_cmdline)
{
	prependToPathList (dir, SUBDIR_OPTLIB, OptlibPathList, "OptlibPathList",
				from_cmdline, from_cmdline? "Prepend": NULL);
	if (!from_cmdline)
		prependToPathList (dir, SUBDIR_PRELOAD, PreloadPathList, "PreloadPathList",
					FALSE, NULL);
}

static void prependToLibexecPathList (const char *const dir, boolean from_cmdline)
{
	prependToPathList (dir, SUBDIR_DRIVERS, DriversPathList, "DriversPathList",
			   from_cmdline, from_cmdline? "Prepend": NULL);
}

static void processDataDir (
		const char *const option, const char *const parameter)
{
	const char* path;

	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "Path for a directory is needed for \"%s\" option", option);

	if (parameter[0] == '+')
	{
		path = parameter + 1;
		prependToDataPathList (path, TRUE);
	}
	else if (!strcmp (parameter, "NONE"))
		resetDataPathList ();
	else
	{
		resetDataPathList ();
		path = parameter;
		appendToDataPathList (path, TRUE);
	}
}

static void processLibexecDir (const char *const option,
			       const char *const parameter)
{
	const char* path;

	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "Path for a directory is needed for \"%s\" option", option);

	if (parameter[0] == '+')
	{
		path = parameter + 1;
		prependToLibexecPathList (path, TRUE);
	}
	else if (!strcmp (parameter, "NONE"))
		resetLibexecPathList ();
	else
	{
		resetLibexecPathList ();
		path = parameter;
		appendToLibexecPathList (path, TRUE);
	}
}

static void processMaxRecursionDepthOption (const char *const option, const char *const parameter)
{
	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "A parameter is needed after \"%s\" option", option);

	if (atol (parameter) < 1)
		error (FATAL, "-%s: Invalid maximum recursion depth", option);

	Option.maxRecursionDepth = atol(parameter);
}

static void processPatternLengthLimit(const char *const option, const char *const parameter)
{
	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "A parameter is needed after \"%s\" option", option);

	if (!strToUInt(parameter, 0, &Option.patternLengthLimit))
		error (FATAL, "-%s: Invalid pattern length limit", option);
}

static boolean* redirectToXtag(const booleanOption *const option)
{
	/* WARNING/TODO: This function breaks capsulization. */
	xtagType t = (xtagType)option->pValue;
	boolean default_value = isXtagEnabled (t);

	enableXtag (t, default_value);

	return &(getXtagDesc (t)->enabled);
}

/*
 *  Option tables
 */

static parametricOption ParametricOptions [] = {
	{ "config-filename",		processConfigFilenameOption,	TRUE,   STAGE_ANY },
	{ "data-dir",               processDataDir,                 FALSE,  STAGE_ANY },
	{ "etags-include",          processEtagsInclude,            FALSE,  STAGE_ANY },
	{ "exclude",                processExcludeOption,           FALSE,  STAGE_ANY },
	{ "excmd",                  processExcmdOption,             FALSE,  STAGE_ANY },
	{ "extra",                  processExtraTagsOption,         FALSE,  STAGE_ANY },
	{ "fields",                 processFieldsOption,            FALSE,  STAGE_ANY },
	{ "filter-terminator",      processFilterTerminatorOption,  TRUE,   STAGE_ANY },
	{ "format",                 processFormatOption,            TRUE,   STAGE_ANY },
	{ "help",                   processHelpOption,              TRUE,   STAGE_ANY },
#ifdef HAVE_ICONV
	{ "input-encoding",         processInputEncodingOption,     FALSE,  STAGE_ANY },
	{ "output-encoding",        processOutputEncodingOption,    FALSE,  STAGE_ANY },
#endif
	{ "lang",                   processLanguageForceOption,     FALSE,  STAGE_ANY },
	{ "language",               processLanguageForceOption,     FALSE,  STAGE_ANY },
	{ "language-force",         processLanguageForceOption,     FALSE,  STAGE_ANY },
	{ "languages",              processLanguagesOption,         FALSE,  STAGE_ANY },
	{ "langdef",                processLanguageDefineOption,    FALSE,  STAGE_ANY },
	{ "langmap",                processLanguageMapOption,       FALSE,  STAGE_ANY },
	{ "libexec-dir",            processLibexecDir,              FALSE,  STAGE_ANY },
	{ "license",                processLicenseOption,           TRUE,   STAGE_ANY },
	{ "list-aliases",           processListAliasesOption,       TRUE,   STAGE_ANY },
	{ "list-extensions",        processListExtensionsOption,    TRUE,   STAGE_ANY },
	{ "list-extra",             processListExtraOption,        TRUE,   STAGE_ANY },
	{ "list-features",          processListFeaturesOption,      TRUE,   STAGE_ANY },
	{ "list-fields",            processListFieldsOption,        TRUE,   STAGE_ANY },
	{ "list-file-kind",         processListFileKindOption,      TRUE,   STAGE_ANY },
	{ "list-kinds",             processListKindsOption,         TRUE,   STAGE_ANY },
	{ "list-kinds-full",        processListKindsOption,         TRUE,   STAGE_ANY },
	{ "list-languages",         processListLanguagesOption,     TRUE,   STAGE_ANY },
	{ "list-maps",              processListMapsOption,          TRUE,   STAGE_ANY },
	{ "list-patterns",          processListPatternsOption,      TRUE,   STAGE_ANY },
	{ "list-pseudo-tags",       processListPseudoTagsOptions,   TRUE,   STAGE_ANY },
	{ "list-regex-flags",       processListRegexFlagsOptions,   TRUE,   STAGE_ANY },
	{ "_list-roles",            processListRolesOptions,        TRUE,   STAGE_ANY },
	{ "maxdepth",               processMaxRecursionDepthOption, TRUE,   STAGE_ANY },
	{ "options",                processOptionFile,              FALSE,  STAGE_ANY },
	{ "output-format",          processOutputFormat,            TRUE,   STAGE_ANY },
	{ "pattern-length-limit",   processPatternLengthLimit,      TRUE,   STAGE_ANY },
	{ "pseudo-tags",            processPseudoTags,              FALSE,  STAGE_ANY },
	{ "sort",                   processSortOption,              TRUE,   STAGE_ANY },
	{ "version",                processVersionOption,           TRUE,   STAGE_ANY },
	{ "_echo",                  processEchoOption,              FALSE,  STAGE_ANY },
	{ "_force-quit",            processForceQuitOption,         FALSE,  STAGE_ANY },
	{ "_xformat",               processXformatOption,           FALSE,  STAGE_ANY },
};

static booleanOption BooleanOptions [] = {
	{ "append",         &Option.append,                 TRUE,  STAGE_ANY },
	{ "file-scope",     ((boolean *)XTAG_FILE_SCOPE),   FALSE, STAGE_ANY, redirectToXtag },
	{ "file-tags",      ((boolean *)XTAG_FILE_NAMES),   FALSE, STAGE_ANY, redirectToXtag },
	{ "filter",         &Option.filter,                 TRUE,  STAGE_ANY },
	{ "guess-language-eagerly", &Option.guessLanguageEagerly, FALSE, STAGE_ANY },
	{ "if0",            &Option.if0,                    FALSE, STAGE_ANY },
	{ "line-directives",&Option.lineDirectives,         FALSE, STAGE_ANY },
	{ "links",          &Option.followLinks,            FALSE, STAGE_ANY },
	{ "machinable",     &Option.machinable,             TRUE,  STAGE_ANY },
	{ "put-field-prefix", &Option.putFieldPrefix,       FALSE, STAGE_ANY },
	{ "print-language", &Option.printLanguage,          TRUE,  STAGE_ANY },
	{ "quiet",          &Option.quiet,                  FALSE, STAGE_ANY },
#ifdef RECURSE_SUPPORTED
	{ "recurse",        &Option.recurse,                FALSE, STAGE_ANY },
#endif
	{ "tag-relative",   &Option.tagRelative,            TRUE,  STAGE_ANY },
	{ "totals",         &Option.printTotals,            TRUE,  STAGE_ANY },
	{ "verbose",        &Option.verbose,                FALSE, STAGE_ANY },
	{ "with-list-header", &Option.withListHeader,       TRUE,  STAGE_ANY },
	{ "_allow-xcmd-in-homedir", &Option.allowXcmdInHomeDir, TRUE, ACCEPT(Etc)|ACCEPT(LocalEtc) },
	{ "_fatal-warnings",&Option.fatalWarnings,          FALSE, STAGE_ANY },
};

/*
 *  Generic option parsing
 */

static void checkOptionOrder (const char* const option, boolean longOption)
{
	if (NonOptionEncountered)
		error (FATAL, "-%s%s option may not follow a file name", longOption? "-": "", option);
}

static boolean processParametricOption (
		const char *const option, const char *const parameter)
{
	const int count = sizeof (ParametricOptions) / sizeof (parametricOption);
	boolean found = FALSE;
	int i;

	for (i = 0  ;  i < count  &&  ! found  ;  ++i)
	{
		parametricOption* const entry = &ParametricOptions [i];
		if (strcmp (option, entry->name) == 0)
		{
			found = TRUE;
			if (!(entry->acceptableStages & (1UL << Stage)))
			{
				error (WARNING, "Cannot use --%s option in %s",
				       option, StageDescription[Stage]);
				break;
			}
			if (entry->initOnly)
				checkOptionOrder (option, TRUE);
			(entry->handler) (option, parameter);
		}
	}
	return found;
}

static boolean getBooleanOption (
		const char *const option, const char *const parameter)
{
	boolean selection = TRUE;

	if (parameter [0] == '\0')
		selection = TRUE;
	else if (isFalse (parameter))
		selection = FALSE;
	else if (isTrue (parameter))
		selection = TRUE;
	else
		error (FATAL, "Invalid value for \"%s\" option", option);

	return selection;
}

static boolean processBooleanOption (
		const char *const option, const char *const parameter)
{
	const int count = sizeof (BooleanOptions) / sizeof (booleanOption);
	boolean found = FALSE;
	int i;

	for (i = 0  ;  i < count  &&  ! found  ;  ++i)
	{
		booleanOption* const entry = &BooleanOptions [i];
		boolean *slot;
		if (strcmp (option, entry->name) == 0)
		{
			found = TRUE;
			if (!(entry->acceptableStages & (1UL << Stage)))
			{
				error (WARNING, "Cannot use --%s option in %s",
				       option, StageDescription[Stage]);
				break;
			}
			if (entry->initOnly)
				checkOptionOrder (option, TRUE);
			if (entry->redirect)
				slot = entry->redirect (entry);
			else
				slot = entry->pValue;
			*slot = getBooleanOption (option, parameter);
		}
	}
	return found;
}

static void enableLanguageField (langType language, const char *field, boolean mode)
{

	fieldType t;

	t = getFieldTypeForNameAndLanguage (field, language);
	if (t == FIELD_UNKNOWN)
		error(FATAL, "no such field: \'%s\'", field);
	enableField (t, mode, (language != LANG_AUTO));
	if (language == LANG_AUTO)
	{
		fieldType ftype_next = t;

		while ((ftype_next = nextSiblingField (ftype_next)) != FIELD_UNKNOWN)
			enableField (ftype_next, mode, (language != LANG_AUTO));
	}
}

static boolean processLangSpecificFieldsOption (const char *const option,
						const char *const parameter)
{
#define PREFIX "fields-"
#define PREFIX_LEN strlen(PREFIX)
	const char* lang;
	size_t len;
	langType language = LANG_IGNORE;
	const char *p = parameter;
	int c;
	static vString * longName;
	boolean mode = TRUE;
	const char *f;
	boolean inLongName = FALSE;

	if ( strncmp (option, PREFIX, PREFIX_LEN) != 0 )
		return FALSE;

	lang = option + PREFIX_LEN;
	len = strlen (lang);
	if (len == 0)
		error (FATAL, "No language given in \"%s\" option", option);
	else if (len == 1 && lang[0] == '*')
		language = LANG_AUTO;
	else
		language = getNamedLanguage (lang, len);

	if (language == LANG_IGNORE)
	{
		error (WARNING, "Unknown language: %s (ignoring \"--%s\")", lang, option);
		/* The option is consumed in tihs function. */
		return TRUE;
	}

	initializeParser (language);

	if (*p == '*')
	{
		resetFieldsOption (language, TRUE);
		p++;
	}
	else if (*p == '{')
		resetFieldsOption (language, FALSE);
	else if (*p != '+' && *p != '-')
		error (WARNING, "Wrong per language field specification: %s", p);

	longName = vStringNewOrClear (longName);
	while ((c = *p++) != '\0')
	{
		switch (c)
		{
		case '+':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = TRUE;
			break;
		case '-':
			if (inLongName)
				vStringPut (longName, c);
			else
				mode = FALSE;
			break;
		case '{':
			if (inLongName)
				error (FATAL,
				       "unexpected character in field specification: \'%c\'",
				       c);
			inLongName = TRUE;
			break;
		case '}':
			if (!inLongName)
				error (FATAL,
				       "unexpected character in field specification: \'%c\'",
				       c);

			f = vStringValue (longName);
			enableLanguageField (language, f, mode);
			inLongName = FALSE;
			vStringClear (longName);
			break;
		default:
			if (inLongName)
				vStringPut (longName, c);
			else
				error (FATAL,
				       "only long name can be used in per language field spec: \'%c\'",
				       c);
			break;
		}
	}
	return TRUE;
}

static void processLongOption (
		const char *const option, const char *const parameter)
{
	Assert (parameter != NULL);
	Assert (option != NULL);

	if (parameter [0] == '\0')
		verbose ("  Option: --%s\n", option);
	else
		verbose ("  Option: --%s=%s\n", option, parameter);

	if (processBooleanOption (option, parameter))
		;
	else if (processLangSpecificFieldsOption(option, parameter))
		 ;
	else if (processParametricOption (option, parameter))
		;
	else if (processKindOption (option, parameter))
		;
	else if (processAliasOption (option, parameter))
		;
	else if (processRegexOption (option, parameter))
		;
	else if (processXcmdOption (option, parameter, Stage))
		;
	else if (processMapOption (option, parameter))
		;
#ifdef HAVE_ICONV
	else if (processLanguageEncodingOption (option, parameter))
		;
#endif
#ifndef RECURSE_SUPPORTED
	else if (strcmp (option, "recurse") == 0)
		error (WARNING, "%s option not supported on this host", option);
#endif
	else
		error (FATAL, "Unknown option: --%s", option);
}

static void processShortOption (
		const char *const option, const char *const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		verbose ("  Option: -%s\n", option);
	else
		verbose ("  Option: -%s %s\n", option, parameter);

	if (isCompoundOption (*option) && (parameter == NULL  ||  parameter [0] == '\0'))
		error (FATAL, "Missing parameter for \"%s\" option", option);
	else switch (*option)
	{
		case '?':
			processHelpOption ("?", NULL);
			exit (0);
			break;
		case 'a':
			checkOptionOrder (option, FALSE);
			Option.append = TRUE;
			break;
#ifdef DEBUG
		case 'b':
			if (atol (parameter) < 0)
				error (FATAL, "-%s: Invalid line number", option);
			Option.breakLine = atol (parameter);
			break;
		case 'D':
			if (!strToLong(parameter, 0, &Option.debugLevel))
				error (FATAL, "-%s: Invalid debug level", option);

			if (debug (DEBUG_STATUS))
				Option.verbose = TRUE;
			break;
#endif
		case 'B':
			Option.backward = TRUE;
			break;
		case 'e':
			checkOptionOrder (option, FALSE);
			setEtagsMode ();
			break;
		case 'f':
		case 'o':
			checkOptionOrder (option, FALSE);
			if (Option.tagFileName != NULL)
			{
				error (WARNING,
					"-%s option specified more than once, last value used",
					option);
				freeString (&Option.tagFileName);
			}
			else if (parameter [0] == '-'  &&  parameter [1] != '\0')
				error (FATAL, "output file name may not begin with a '-'");
			Option.tagFileName = stringCopy (parameter);
			break;
		case 'F':
			Option.backward = FALSE;
			break;
		case 'G':
			Option.guessLanguageEagerly = TRUE;
			break;
		case 'h':
			processHeaderListOption (*option, parameter);
			break;
		case 'I':
			processIgnoreOption (parameter);
			break;
		case 'L':
			if (Option.fileList != NULL)
			{
				error (WARNING,
					"-%s option specified more than once, last value used",
					option);
				freeString (&Option.fileList);
			}
			Option.fileList = stringCopy (parameter);
			break;
		case 'n':
			Option.locate = EX_LINENUM;
			break;
		case 'N':
			Option.locate = EX_PATTERN;
			break;
		case 'R':
#ifdef RECURSE_SUPPORTED
			Option.recurse = TRUE;
#else
			error (WARNING, "-%s option not supported on this host", option);
#endif
			break;
		case 'u':
			checkOptionOrder (option, FALSE);
			Option.sorted = SO_UNSORTED;
			break;
		case 'V':
			Option.verbose = TRUE;
			break;
		case 'w':
			/* silently ignored */
			break;
		case 'x':
			checkOptionOrder (option, FALSE);
			setXrefMode ();
			break;
		default:
			error (FATAL, "Unknown option: -%s", option);
			break;
	}
}

static void parseOption (cookedArgs* const args)
{
	Assert (! cArgOff (args));
	if (args->isOption)
	{
		if (args->longOption)
			processLongOption (args->item, args->parameter);
		else
		{
			const char *parameter = args->parameter;
			while (*parameter == ' ')
				++parameter;
			processShortOption (args->item, parameter);
		}
		cArgForth (args);
	}
}

static void parseOptions (cookedArgs* const args)
{
	while (! cArgOff (args)  &&  cArgIsOption (args))
		parseOption (args);
	if (! cArgOff (args)  &&  ! cArgIsOption (args))
		NonOptionEncountered = TRUE;
}

extern void parseCmdlineOptions (cookedArgs* const args)
{
	ENTER (Cmdline);
	parseOptions (args);
}

static boolean checkSameFile (const char *const fileName, void * userData)
{
	return isSameFile ((const char* const) userData, fileName);
}

static boolean parseFileOptions (const char* const fileName)
{
	boolean fileFound = FALSE;
	const char* const format = "Considering option file %s: %s\n";
	if (stringListHasTest (OptionFiles, checkSameFile, (void *) fileName))
	{
		verbose (format, fileName, "already considered");
		fileFound = TRUE;
	}
	else
	{
		FILE* const fp = fopen (fileName, "r");
		if (fp == NULL)
			verbose (format, fileName, "not found");
		else
		{
			cookedArgs* const args = cArgNewFromLineFile (fp);
			vString* file = vStringNewInit (fileName);
			stringListAdd (OptionFiles, file);
			verbose (format, fileName, "reading...");
			parseOptions (args);
			if (NonOptionEncountered)
				error (WARNING, "Ignoring non-option in %s\n", fileName);
			cArgDelete (args);
			fclose (fp);
			fileFound = TRUE;
		}
	}
	return fileFound;
}

/* Actions to be taken before reading any other options */
extern void previewFirstOption (cookedArgs* const args)
{
	while (cArgIsOption (args))
	{
		if (strcmp (args->item, "V") == 0
		    || strcmp (args->item, "verbose") == 0
		    || strcmp (args->item, "config-filename") == 0
		    || strcmp (args->item, "quiet") == 0)
			parseOption (args);
		else if (strcmp (args->item, "options") == 0  &&
				strcmp (args->parameter, "NONE") == 0)
		{
			notice ("No options will be read from files or environment");
			SkipConfiguration = TRUE;
			cArgForth (args);
		}
		else
			break;
	}
}

static void parseConfigurationFileOptionsInDirectoryWithLeafname (const char* directory, const char* leafname)
{
	char* pathname = combinePathAndFile (directory, leafname);
	parseFileOptions (pathname);
	eFree (pathname);
}

static void parseConfigurationFileOptionsInDirectory (const char* directory)
{
	char	*leafname = NULL;

	if (asprintf (&leafname,".%s",(Option.configFilename)?Option.configFilename:"ctags") == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseConfigurationFileOptionsInDirectoryWithLeafname (directory, leafname);
	free (leafname);
#ifdef MSDOS_STYLE_PATH
	if (asprintf (&leafname,"%s.cnf",(Option.configFilename)?Option.configFilename:"ctags") == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseConfigurationFileOptionsInDirectoryWithLeafname (directory, leafname);
	free (leafname);
#endif
}

#if defined(HAVE_SCANDIR)
static int ignore_dot_file(const struct dirent* dent)
{
	/* Ignore a file which name is started from dot. */
	if (*dent->d_name == '.')
		return 0;
	else
		return 1;
}

static int accept_only_dot_d(const struct dirent* dent)
{
	size_t len;

	/* accept only a directory ended with ".d" */
	len = strlen(dent->d_name);

	if (len < 3)
		return 0;
	return !strcmp(dent->d_name + (len - 2), ".d");
}

static int accept_only_dot_ctags(const struct dirent* dent)
{
	size_t len;

	/* accept only a file ended with ".conf" or ".ctags" */
	len = strlen(dent->d_name);

	if (len < 6)
		return 0;
	if (strcmp(dent->d_name + (len - 5), ".conf") == 0)
		return 1;

	if (len < 7)
		return 0;
	if (strcmp(dent->d_name + (len - 6), ".ctags") == 0)
		return 1;

	return 0;
}

static boolean parseAllConfigurationFilesOptionsInDirectory (const char* const dirName,
							     stringList* const already_loaded_files)
{
	struct dirent **dents;
	int i, n;

	n = scandir (dirName, &dents, ignore_dot_file, alphasort);
	if (n < 0)
		return FALSE;

	for (i = 0; i < n; i++)
	{
		char* path;
		fileStatus *s;

		if (already_loaded_files && stringListHas (already_loaded_files, dents[i]->d_name))
			continue;
		else if (already_loaded_files)
			stringListAdd (already_loaded_files, vStringNewInit (dents[i]->d_name));

		path = combinePathAndFile (dirName, dents[i]->d_name);
		s = eStat (path);

		if (s->exists && s->isDirectory && accept_only_dot_d(dents[i]))
			parseAllConfigurationFilesOptionsInDirectory (path,
								      already_loaded_files);
		else if (s->exists && accept_only_dot_ctags(dents[i]))
			parseConfigurationFileOptionsInDirectoryWithLeafname (dirName,
									      dents[i]->d_name);
		eStatFree (s);
		free (dents[i]);
		eFree (path);
	}
	free (dents);
	return TRUE;
}
#else
static boolean parseAllConfigurationFilesOptionsInDirectory (const char* const dirName,
							     stringList* const already_loaded_files)
{
	return FALSE;
}
#endif

static void preload (const searchPathList *const pathList)
{
	unsigned int i;
	stringList* loaded;

	loaded = stringListNew ();
	for (i = 0; i < stringListCount (pathList); ++i)
	{
		const char* const dir = vStringValue (stringListItem (pathList, i));
		parseAllConfigurationFilesOptionsInDirectory (dir, loaded);
	}
	stringListClear (loaded);
	stringListDelete (loaded);
}

static void parseConfigurationFileOptions (void)
{
	vString *home;
	/* We parse .ctags on all systems, and additionally ctags.cnf on DOS. */
	char *filename = NULL;
	const char *filename_body;

#ifdef CUSTOM_CONFIGURATION_FILE
	ENTER(Custom);
	parseFileOptions (CUSTOM_CONFIGURATION_FILE);
#endif
	filename_body = (Option.configFilename)?Option.configFilename:"ctags";
#ifdef MSDOS_STYLE_PATH

	if (asprintf (&filename,"/%s.cnf", filename_body) == -1)
	{
		error (FATAL, "error in asprintf");
	}
	ENTER(DosCnf);
	parseFileOptions (filename);
	free (filename);
#endif
	if (asprintf (&filename,"/etc/%s.conf", filename_body) == -1)
	{
		error (FATAL, "error in asprintf");
	}
	ENTER(Etc);
	parseFileOptions (filename);
	free (filename);

	if (asprintf (&filename,"/usr/local/etc/%s.conf", filename_body) == -1)
	{
		error (FATAL, "error in asprintf");
	}
	ENTER(LocalEtc);
	parseFileOptions (filename);
	free (filename);

	home = getHome ();
	if (home != NULL)
	{
		ENTER(HomeRecursive);
		parseConfigurationFileOptionsInDirectory (vStringValue (home));
		vStringDelete (home);
	}

	ENTER(CurrentRecursive);
	parseConfigurationFileOptionsInDirectory (".");

	ENTER(Preload);
	preload (PreloadPathList);
}

static void parseEnvironmentOptions (void)
{
	const char *envOptions = NULL;
	const char* var = NULL;

	ENTER(EnvVar);
	if (Option.etags)
	{
		var = ETAGS_ENVIRONMENT;
		envOptions = getenv (var);
	}
	if (envOptions == NULL)
	{
		var = CTAGS_ENVIRONMENT;
		envOptions = getenv (var);
	}
	if (envOptions != NULL  &&  envOptions [0] != '\0')
	{
		cookedArgs* const args = cArgNewFromString (envOptions);
		verbose ("Reading options from $CTAGS\n");
		parseOptions (args);
		cArgDelete (args);
		if (NonOptionEncountered)
			error (WARNING, "Ignoring non-option in %s variable", var);
	}
}

extern void readOptionConfiguration (void)
{
	if (! SkipConfiguration)
	{
		parseConfigurationFileOptions ();
		parseEnvironmentOptions ();
	}
}

static void installDataPathList (void)
{
	char* dataPath = getenv (CTAGS_DATA_PATH_ENVIRONMENT);

	OptlibPathList = stringListNew ();
	PreloadPathList = stringListNew ();

	if (dataPath)
	{
		char* needle;

		while (dataPath[0])
		{
			needle = strchr (dataPath, ':');
			if (needle)
				*needle = '\0';

			appendToDataPathList (dataPath, FALSE);

			if (needle)
			{
				*needle = ':';
				dataPath = needle + 1;
			}
			else
				break;
		}
	}

	{
		vString *home = getHome ();
		if (home != NULL)
		{
			char *ctags_d;

			ctags_d = combinePathAndFile (vStringValue (home), ".ctags.d");

			appendToDataPathList (ctags_d, FALSE);
			eFree (ctags_d);
			vStringDelete (home);
		}
	}
#ifdef PKGSYSCONFDIR
	appendToDataPathList (PKGSYSCONFDIR, FALSE);
#endif
}

static void installLibexecPathList (void)
{
	char* libexecPath = getenv (CTAGS_LIBEXEC_PATH_ENVIRONMENT);

	DriversPathList = stringListNew ();

	if (libexecPath)
	{
		char* needle;

		while (libexecPath[0])
		{
			needle = strchr (libexecPath, ':');
			if (needle)
				*needle = '\0';

			appendToDataPathList (libexecPath, FALSE);

			if (needle)
			{
				*needle = ':';
				libexecPath = needle + 1;
			}
			else
				break;
		}
	}

	{
		vString *home = getHome ();
		if (home != NULL)
		{
			char *ctags_d;

			ctags_d = combinePathAndFile (vStringValue (home), ".ctags.d");

			appendToLibexecPathList (ctags_d, FALSE);
			eFree (ctags_d);
			vStringDelete (home);
		}
	}

#ifdef PKGLIBEXECDIR
	appendToLibexecPathList (PKGLIBEXECDIR, FALSE);
#endif
}

/*
*   Option initialization
*/

extern void initOptions (void)
{
	OptionFiles = stringListNew ();
	installDataPathList ();
	installLibexecPathList ();
	verboseSearchPathList (OptlibPathList,  "OptlibPathList");
	verboseSearchPathList (PreloadPathList, "PreloadPathList");
	verboseSearchPathList (DriversPathList, "DriversPathList");

	verbose ("Setting option defaults\n");
	installHeaderListDefaults ();
	verbose ("  Installing default language mappings:\n");
	installLanguageMapDefaults ();
	verbose ("  Installing default language aliases:\n");
	installLanguageAliasesDefaults ();

	/* always excluded by default */
	verbose ("  Installing default exclude patterns:\n");
	processExcludeOption (NULL, "{arch}");
	processExcludeOption (NULL, ".arch-ids");
	processExcludeOption (NULL, ".arch-inventory");
	processExcludeOption (NULL, "autom4te.cache");
	processExcludeOption (NULL, "BitKeeper");
	processExcludeOption (NULL, ".bzr");
	processExcludeOption (NULL, ".bzrignore");
	processExcludeOption (NULL, "CVS");
	processExcludeOption (NULL, ".cvsignore");
	processExcludeOption (NULL, "_darcs");
	processExcludeOption (NULL, ".deps");
	processExcludeOption (NULL, "EIFGEN");
	processExcludeOption (NULL, ".git");
	processExcludeOption (NULL, ".gitignore");
	processExcludeOption (NULL, ".gitattributes");
	processExcludeOption (NULL, ".hg");
	processExcludeOption (NULL, ".hgignore");
	processExcludeOption (NULL, "PENDING");
	processExcludeOption (NULL, "RCS");
	processExcludeOption (NULL, "RESYNC");
	processExcludeOption (NULL, "SCCS");
	processExcludeOption (NULL, ".svn");
	processExcludeOption (NULL, "*~");
	processExcludeOption (NULL, ".*.swp");

	/* Exclude binary files
	 * -----------------------------------------------
	 *
	 * TODO
	 *
	 * It will be interesting if ctags can extract
	 * symbols from these binaries.
	 *
	 * --langdef=nm --regex-nm=...
	 * --langdef=elf --pre-processor-elf=/bin/nm ...
	 *
	 * vim/emacs users never wants the cursor to jump to
	 * a binary file but may wants to utilize the symbol
	 * information for completion.
	 *
	 * https://bitbucket.org/haypo/hachoir3 can be
	 * used the alternative for /bin/nm
	 */
	processExcludeOption (NULL, "*.o");
	processExcludeOption (NULL, "*.a");
	processExcludeOption (NULL, "*.so");

	processExcludeOption (NULL, "*.obj");
	processExcludeOption (NULL, "*.lib");
	processExcludeOption (NULL, "*.dll");
	processExcludeOption (NULL, "*.exe");

	processExcludeOption (NULL, "*.gcno");
	processExcludeOption (NULL, "*.gcda");

	processExcludeOption (NULL, "*.class");

	processExcludeOption (NULL, "*.pyc");
	processExcludeOption (NULL, "*.pyo");
}

extern void freeOptionResources (void)
{
	freeString (&Option.tagFileName);
	freeString (&Option.fileList);
	freeString (&Option.filterTerminator);

	freeList (&Excluded);
	freeList (&Option.ignore);
	freeList (&Option.headerExt);
	freeList (&Option.etagsInclude);

	freeSearchPathList (&OptlibPathList);
	freeSearchPathList (&PreloadPathList);
	freeSearchPathList (&DriversPathList);

	freeList (&OptionFiles);
}
