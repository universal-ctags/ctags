.. _ctags(1):

==============================================================
@CTAGS_NAME_EXECUTABLE@
==============================================================
--------------------------------------------------------------
Generate tag files for source code
--------------------------------------------------------------
:Version: @VERSION@
:Manual group: Universal-ctags
:Manual section: 1

SYNOPSIS
--------
|	**@CTAGS_NAME_EXECUTABLE@** [options] [file(s)]
|	**@ETAGS_NAME_EXECUTABLE@** [options] [file(s)]


DESCRIPTION
-----------

The **@CTAGS_NAME_EXECUTABLE@** and **@ETAGS_NAME_EXECUTABLE@** programs
(hereinafter collectively referred to as **@CTAGS_NAME_EXECUTABLE@**,
except where distinguished) generate an index (or "tag") file for a
variety of language objects found in file(s). This tag file allows
these items to be quickly and easily located by a text editor or other
utility. A "tag" signifies a language object for which an index entry is
available (or, alternatively, the index entry created for that object).

Alternatively, **@CTAGS_NAME_EXECUTABLE@** can generate a cross reference
file which lists, in human readable form, information about the various
source objects found in a set of language files.

Tag index files are supported by numerous editors, which allow the user to
locate the object associated with a name appearing in a source file and
jump to the file and line which defines the name. Those known about at
the time of this release are:

	**Vi** (1) and its derivatives (e.g. Elvis, Vim, Vile, Lemmy), **CRiSP**,
	**Emacs**, **FTE** (Folding Text Editor), **JED**, **jEdit**, **Mined**,
	**NEdit** (Nirvana Edit), **TSE** (The SemWare Editor), **UltraEdit**,
	**WorkSpace**, **X2**, **Zeus**

**@CTAGS_NAME_EXECUTABLE@** is capable of generating different kinds of tags
for each of many different languages. For a complete list of supported
languages, the names by which they are recognized, and the kinds of tags
which are generated for each, see the ``--list-languages`` and ``--list-kinds``
options.


SOURCE FILES
------------

Unless the ``--language-force`` option is specified, the language of each source
file is automatically selected based upon a mapping of file names to
languages. The mappings in effect for each language may be display using
the ``--list-maps`` option and may be changed using the ``--langmap`` option. On
platforms which support it, if the name of a file is not mapped to a
language and the file is executable, the first line of the file is checked
to see if the file is a "#!" script for a recognized language.

By default, all other files names are ignored. This permits running
@CTAGS_NAME_EXECUTABLE@ on all files in either a single directory (e.g.
"@CTAGS_NAME_EXECUTABLE@ \*"), or on all files in an entire source directory
tree (e.g. "@CTAGS_NAME_EXECUTABLE@ -R"), since only those files whose
names are mapped to languages will be scanned.

[The reason that .h extensions are mapped to C++ files rather than C files
is because it is common to use .h extensions in C++, and no harm
results in treating them as C++ files.]

OPTIONS
-------

Despite the wealth of available options, defaults are set so that
@CTAGS_NAME_EXECUTABLE@ is most commonly executed without any options (e.g.
"@CTAGS_NAME_EXECUTABLE@ \*", or "@CTAGS_NAME_EXECUTABLE@ -R"), which will
create a tag file in the current directory for all recognized source
files. The options described below are provided merely to allow custom
tailoring to meet special needs.

Note that spaces separating the single-letter options from their parameters
are optional.

Note also that the boolean parameters to the long form options (those
beginning with "--" and that take a "[=yes|no]" parameter) may be omitted,
in which case "=yes" is implied. (e.g. --sort is equivalent to --sort=yes).
Note further that "=1" and "=on" are considered synonyms for "=yes",
and that "=0" and "=off" are considered synonyms for "=no".

Some options are either ignored or useful only when used while running in
etags mode (see -e option). Such options will be noted.

Most options may appear anywhere on the command line, affecting only those
files which follow the option. A few options, however, must appear
before the first file name and will be noted as such.

Options taking language names will accept those names in either upper or
lower case. See the --list-languages option for a complete list of the
built-in language names.

-a
	Equivalent to ``--append``.

-B
	Use backward searching patterns (e.g. ?pattern?). [Ignored in etags mode]

-e
	Enable etags mode, which will create a tag file for use with the Emacs
	editor. Alternatively, if @CTAGS_NAME_EXECUTABLE@ is invoked by a
	name containing the string "@ETAGS_NAME_EXECUTABLE@" (either by renaming,
	or creating a link to, the executable), etags mode will be enabled.
	This option must appear before the first file name.

-f tagfile
	Use the name specified by tagfile for the tag file (default is "tags",
	or "TAGS" when running in etags mode). If tagfile is specified as "-",
	then the tag file is written to standard output instead. @CTAGS_NAME_EXECUTABLE@
	will stubbornly refuse to take orders if tagfile exists and
	its first line contains something other than a valid tags line. This
	will save your neck if you mistakenly type "@CTAGS_NAME_EXECUTABLE@ -f
	\*.c", which would otherwise overwrite your first C file with the tags
	generated by the rest! It will also refuse to accept a multi-character
	file name which begins with a '-' (dash) character, since this most
	likely means that you left out the tag file name and this option tried to
	grab the next option as the file name. If you really want to name your
	output tag file "-ugly", specify it as "./-ugly". This option must
	appear before the first file name. If this option is specified more
	than once, only the last will apply.

-F
	Use forward searching patterns (e.g. /pattern/) (default). [Ignored
	in etags mode]

-h list
	Specifies a list of file extensions, separated by periods, which are
	to be interpreted as include (or header) files. To indicate files having
	no extension, use a period not followed by a non-period character
	(e.g. ".", "..x", ".x."). This option only affects how the scoping of a
	particular kinds of tags is interpreted (i.e. whether or not they are
	considered as globally visible or visible only within the file in which
	they are defined); it does not map the extension to any particular
	language. Any tag which is located in a non-include file and cannot be
	seen (e.g. linked to) from another file is considered to have file-limited
	(e.g. static) scope. No kind of tag appearing in an include file
	will be considered to have file-limited scope. If the first character
	in the list is a plus sign, then the extensions in the list will be
	appended to the current list; otherwise, the list will replace the
	current list. See, also, the --file-scope option. The default list is
	".h.H.hh.hpp.hxx.h++.inc.def". To restore the default list, specify -h
	default. Note that if an extension supplied to this option is not
	already mapped to a particular language (see SOURCE FILES, above),
	you will also need to use either the --langmap or --language-force option.

-I identifier-list
	Specifies a list of identifiers which are to be specially handled while
	parsing C and C++ source files. This option is specifically provided
	to handle special cases arising through the use of preprocessor macros.
	When the identifiers listed are simple identifiers, these identifiers
	will be ignored during parsing of the source files. If an identifier is
	suffixed with a '+' character, @CTAGS_NAME_EXECUTABLE@ will also
	ignore any parenthesis-enclosed argument list which may immediately
	follow the identifier in the source files. If two identifiers are
	separated with the '=' character, the first identifiers is replaced by
	the second identifiers for parsing purposes. The list of identifiers may
	be supplied directly on the command line or read in from a separate file.
	If the first character of identifier-list is '@', '.' or a pathname
	separator ('/' or '\'), or the first two characters specify a drive
	letter (e.g. "C:"), the parameter identifier-list will be interpreted as
	a filename from which to read a list of identifiers, one per input line.
	Otherwise, identifier-list is a list of identifiers (or identifier
	pairs) to be specially handled, each delimited by a either a comma or
	by white space (in which case the list should be quoted to keep the
	entire list as one command line argument). Multiple -I options may be
	supplied. To clear the list of ignore identifiers, supply a single
	dash ("-") for identifier-list.

	This feature is useful when preprocessor macros are used in such a way
	that they cause syntactic confusion due to their presence. Indeed,
	this is the best way of working around a number of problems caused by
	the presence of syntax-busting macros in source files (see CAVEATS).
	Some examples will illustrate this point.

	.. code-block::

		int foo ARGDECL4(void *, ptr, long int, nbytes)

	In the above example, the macro "ARGDECL4" would be mistakenly
	interpreted to be the name of the function instead of the correct name
	of "foo". Specifying -I ARGDECL4 results in the correct behavior.

	.. code-block::

		/* creates an RCS version string in module */
		MODULE_VERSION("$Revision$")

	In the above example the macro invocation looks too much like a function
	definition because it is not followed by a semicolon (indeed, it
	could even be followed by a global variable definition that would look
	much like a K&R style function parameter declaration). In fact, this
	seeming function definition could possibly even cause the rest of the
	file to be skipped over while trying to complete the definition.
	Specifying -I MODULE_VERSION+ would avoid such a problem.

	.. code-block::

		CLASS Example {
			// your content here
		};

	The example above uses "CLASS" as a preprocessor macro which expands to
	something different for each platform. For instance CLASS may be
	defined as "class __declspec(dllexport)" on Win32 platforms and simply
	"class" on UNIX. Normally, the absence of the C++ keyword "class"
	would cause the source file to be incorrectly parsed. Correct behavior
	can be restored by specifying -I CLASS=class.

-L file
	Read from file a list of file names for which tags should be generated.
	If file is specified as "-", then file names are read from standard
	input. File names read using this option are processed following file
	names appearing on the command line. Options are also accepted in this
	input. If this option is specified more than once, only the last will
	apply. Note: file is read in line-oriented mode, where a new line is
	the only delimiter and non-trailing white space is considered significant,
	in order that file names containing spaces may be supplied
	(however, trailing white space is stripped from lines); this can affect
	how options are parsed if included in the input.

-n
	Equivalent to --excmd=number.

-N
	Equivalent to --excmd=pattern.

-o tagfile
	Equivalent to -f tagfile.

-R
	Equivalent to --recurse.

-u
	Equivalent to --sort=no (i.e. "unsorted").

-V
	Equivalent to --verbose.

-w
	This option is silently ignored for backward-compatibility with the
	ctags of SVR4 Unix.

-x
	Print a tabular, human-readable cross reference (xref) file to standard
	output instead of generating a tag file. The information contained in
	the output includes: the tag name; the kind of tag; the line number,
	file name, and source line (with extra white space condensed) of the
	file which defines the tag. No tag file is written and all options
	affecting tag file output will be ignored. Example applications for this
	feature are generating a listing of all functions located in a source
	file (e.g. @CTAGS_NAME_EXECUTABLE@ -x --c-kinds=f file), or generating
	a list of all externally visible global variables located in a source
	file (e.g. @CTAGS_NAME_EXECUTABLE@ -x --c-kinds=v --file-scope=no file).
	This option must appear before the first file name.

--append[=yes|no]
	Indicates whether tags generated from the specified files should be
	appended to those already present in the tag file or should replace them.
	This option is off by default. This option must appear before the
	first file name.

--etags-include=file
	Include a reference to file in the tag file. This option may be specified
	as many times as desired. This supports Emacs' capability to use a
	tag file which "includes" other tag files. [Available only in etags mode]

--exclude=[pattern]
	Add pattern to a list of excluded files and directories. This option may
	be specified as many times as desired. For each file name considered
	by @CTAGS_NAME_EXECUTABLE@, each pattern specified using this option
	will be compared against both the complete path (e.g.
	some/path/base.ext) and the base name (e.g. base.ext) of the file, thus
	allowing patterns which match a given file name irrespective of its
	path, or match only a specific path. If appropriate support is available
	from the runtime library of your C compiler, then pattern may
	contain the usual shell wildcards (not regular expressions) common on
	Unix (be sure to quote the option parameter to protect the wildcards from
	being expanded by the shell before being passed to @CTAGS_NAME_EXECUTABLE@;
	also be aware that wildcards can match the slash character, '/').
	You can determine if shell wildcards are available on your platform by
	examining the output of the --version option, which will include
	"+wildcards" in the compiled feature list; otherwise, pattern is matched
	against file names using a simple textual comparison.

	If pattern begins with the character '@', then the rest of the string
	is interpreted as a file name from which to read exclusion patterns,
	one per line. If pattern is empty, the list of excluded patterns is
	cleared. Note that at program startup, the default exclude list contains
	"EIFGEN", "SCCS", "RCS", and "CVS", which are names of directories for
	which it is generally not desirable to descend while processing the
	--recurse option.

--excmd=type
	Determines the type of EX command used to locate tags in the source
	file. [Ignored in etags mode]

	The valid values for type (either the entire word or the first letter
	is accepted) are:

	number
		Use only line numbers in the tag file for locating tags. This has
		four advantages:

		1.	Significantly reduces the size of the resulting tag file.
		2.	Eliminates failures to find tags because the line defining the
			tag has changed, causing the pattern match to fail (note that
			some editors, such as vim, are able to recover in many such
			instances).
		3.	Eliminates finding identical matching, but incorrect, source
			lines (see BUGS).
		4.	Retains separate entries in the tag file for lines which are
			identical in content. In pattern mode, duplicate entries are
			dropped because the search patterns they generate are identical,
			making the duplicate entries useless.

		However, this option has one significant drawback: changes to the
		source files can cause the line numbers recorded in the tag file
		to no longer correspond to the lines in the source file, causing
		jumps to some tags to miss the target definition by one or more
		lines. Basically, this option is best used when the source code
		to which it is applied is not subject to change. Selecting this
		option type causes the following options to be ignored: ``-BF``.

	pattern
		Use only search patterns for all tags, rather than the line numbers
		usually used for macro definitions. This has the advantage of
		not referencing obsolete line numbers when lines have been added or
		removed since the tag file was generated.

	mixed
		In this mode, patterns are generally used with a few exceptions.
		For C, line numbers are used for macro definition tags. This was
		the default format generated by the original ctags and is, therefore,
		retained as the default for this option. For Fortran, line numbers
		are used for common blocks because their corresponding source lines
		are generally identical, making pattern searches useless
		for finding all matches.

--extra=[+|-]flags|\*
	Specifies whether to include extra tag entries for certain kinds of
	information. The parameter flags is a set of one-letter flags, each
	representing one kind of extra tag entry to include in the tag file.
	If flags is preceded by either the '+' or '-' character, the effect of
	each flag is added to, or removed from, those currently enabled;
	otherwise the flags replace any current settings. All entries are
	included  if '*' is given. The meaning of each flag is as follows:

	F
		Equivalent to --file-scope.
		This option is on by default.

	f
		Include an entry for the base file name of every source file
		(e.g. "example.c"), which addresses the first line of the file.

	p
		Include pseudo tags. Enabled by default unless the tag file is
		written to standard output.

	q
		Include an extra class-qualified tag entry for each tag which is a
		member of a class (for languages for which this information is
		extracted; currently C++, Eiffel, Java, and Perl). The actual form
		of the qualified tag depends upon the language from which the tag
		was derived (using a form that is most natural for how qualified
		calls are specified in the language). For C++ and Perl, it is in the
		form "class::member"; for Eiffel and Java, it is in the form
		"class.member". This may allow easier location of a specific tags
		when multiple occurrences of a tag name occur in the tag file.
		Note, however, that this could potentially more than double the
		size of the tag file.

--fields=[+|-]flags|*
	Specifies the available extension fields which are to be included in
	the entries of the tag file (see TAG FILE FORMAT, below, for more
	information). The parameter flags is a set of one-letter flags,
	each representing one type of extension field to include, with the
	following meanings (disabled by default unless indicated):

	a	Access (or export) of class members
	f	File-restricted scoping [enabled]
	i	Inheritance information
	k	Kind of tag as a single letter [enabled]
	K	Kind of tag as full name
	l	Language of source file containing tag
	m	Implementation information
	n	Line number of tag definition
	s	Scope of tag definition [enabled]
	S	Signature of routine (e.g. prototype or parameter list)
	t	Type and name of a variable or typedef as "typeref:" field [enabled]
	z	Include the "kind:" key in kind field
	Z	Include the "scope:" key in scope field

	Each letter or group of letters may be preceded by either '+' to add it
	to the default set, or '-' to exclude it. In the absence of any
	preceding '+' or '-' sign, only those kinds explicitly listed in flags
	will be included in the output (i.e. overriding the default set). All
	fields are included if '*' is given. This option is ignored if the
	option --format=1 has been specified. The default value of this option
	is fkst.

--file-scope[=yes|no]
	Indicates whether tags scoped only for a single file (i.e. tags which
	cannot be seen outside of the file in which they are defined, such as
	"static" tags) should be included in the output. See, also, the -h
	option. This option is enabled by default.

--filter[=yes|no]
	Causes @CTAGS_NAME_EXECUTABLE@ to behave as a filter, reading source
	file names from standard input and printing their tags to standard
	output on a file-by-file basis. If --sorted is enabled, tags are sorted
	only within the source file in which they are defined. File names are
	read from standard input in line-oriented input mode (see note for -L
	option) and only after file names listed on the command line or from
	any file supplied using the -L option. When this option is enabled,
	the options -f, -o, and --totals are ignored. This option is quite
	esoteric and is disabled by default. This option must appear before
	the first file name.

--filter-terminator=string
	Specifies a string to print to standard output following the tags for
	each file name parsed when the --filter option is enabled. This may
	permit an application reading the output of @CTAGS_NAME_EXECUTABLE@
	to determine when the output for each file is finished. Note that if the
	file name read is a directory and --recurse is enabled, this string will
	be printed only once at the end of all tags found for by descending
	the directory. This string will always be separated from the last tag
	line for the file by its terminating newline. This option is quite
	esoteric and is empty by default. This option must appear before
	the first file name.

--format=level
	Change the format of the output tag file. Currently the only valid
	values for level are 1 or 2. Level 1 specifies the original tag file
	format and level 2 specifies a new extended format containing extension
	fields (but in a manner which retains backward-compatibility with
	original vi(1) implementations). The default level is 2. This option
	must appear before the first file name. [Ignored in etags mode]

--help
	Prints to standard output a detailed usage description, and then exits.

--if0[=yes|no]
	Indicates a preference as to whether code within an "#if 0" branch of a
	preprocessor conditional should be examined for non-macro tags (macro
	tags are always included). Because the intent of this construct is to
	disable code, the default value of this option is no. Note that this
	indicates a preference only and does not guarantee skipping code within
	an "#if 0" branch, since the fall-back algorithm used to generate
	tags when preprocessor conditionals are too complex follows all branches
	of a conditional. This option is disabled by default.

--<LANG>-kinds=[+|-]kinds|*
	Specifies a list of language-specific kinds of tags (or kinds) to
	include in the output file for a particular language, where <LANG> is
	case-insensitive and is one of the built-in language names (see the
	--list-languages option for a complete list). The parameter kinds is a group
	of one-letter flags designating kinds of tags (particular to the language)
	to either include or exclude from the output. The specific sets of
	flags recognized for each language, their meanings and defaults may be
	list using the --list-kinds option. Each letter or group of letters
	may be preceded by either '+' to add it to, or '-' to remove it from,
	the default set. In the absence of any preceding '+' or '-' sign, only
	those kinds explicitly listed in kinds will be included in the output
	(i.e. overriding the default for the specified language).

	Specifies '*' as the parameter kinds to include all kinds implemented
	in <LANG> in the output. Further more if '*' is given as <LANG>,
	specification of the parameter kinds affects all languages defined
	in @CTAGS_NAME_EXECUTABLE@.

	As an example for the C language, in order to add prototypes and
	external variable declarations to the default set of tag kinds,
	but exclude macros, use --c-kinds=+px-d; to include only tags for
	functions, use --c-kinds=f.

--langdef=name
	Defines a new user-defined language, name, to be parsed with regular
	expressions. Once defined, name may be used in other options taking
	language names. The typical use of this option is to first define the
	language, then map file names to it using --langmap, then specify regular
	expressions using --regex-<LANG> to define how its tags are found.

--langmap=map[,map[...]]
	Controls how file names are mapped to languages (see the --list-maps
	option). Each comma-separated map consists of the language name (either
	a built-in or user-defined language), a colon, and a list of file
	extensions and/or file name patterns. A file extension is specified by
	preceding the extension with a period (e.g. ".c"). A file name pattern
	is specified by enclosing the pattern in parentheses (e.g.
	"([Mm]akefile)"). If appropriate support is available from the runtime
	library of your C compiler, then the file name pattern may contain the usual
	shell wildcards common on Unix (be sure to quote the option parameter to
	protect the wildcards from being expanded by the shell before being
	passed to @CTAGS_NAME_EXECUTABLE@). You can determine if shell wildcards
	are available on your platform by examining the output of the
	--version option, which will include "+wildcards" in the compiled
	feature list; otherwise, the file name patterns are matched against
	file names using a simple textual comparison. When mapping a file
	extension, it will first be unmapped from any other languages.

	If the first character in a map is a plus sign, then the extensions and
	file name patterns in that map will be appended to the current map
	for that language; otherwise, the map will replace the current map.
	For example, to specify that only files with extensions of .c and .x are
	to be treated as C language files, use "--langmap=c:.c.x"; to also add
	files with extensions of .j as Java language files, specify
	"--langmap=c:.c.x,java:+.j". To map makefiles (e.g. files named either
	"Makefile", "makefile", or having the extension ".mak") to a language
	called "make", specify "--langmap=make:([Mm]akefile).mak". To map files
	having no extension, specify a period not followed by a non-period
	character (e.g. ".", "..x", ".x."). To clear the mapping for a
	particular language (thus inhibiting automatic generation of tags for
	that language), specify an empty extension list (e.g. "--langmap=fortran:").
	To restore the default language mappings for all a particular language,
	supply the keyword "default" for the mapping. To specify restore the
	default language mappings for all languages, specify "--langmap=default".
	Note that file name patterns are tested before file extensions when inferring
	the language of a file. This order of Universal-ctags is different from
	Exuberant-ctags.

--language-force=language
	By default, @CTAGS_NAME_EXECUTABLE@ automatically selects the language
	of a source file, ignoring those files whose language cannot be
	determined (see SOURCE FILES, above). This option forces the specified
	language (case-insensitive; either built-in or user-defined) to be used
	for every supplied file instead of automatically selecting the language
	based upon its extension. In addition, the special value auto indicates
	that the language should be automatically selected (which effectively
	disables this option).

--languages=[+|-]list
	Specifies the languages for which tag generation is enabled, with list
	containing a comma-separated list of language names (case-insensitive;
	either built-in or user-defined). If the first language of list is not
	preceded by either a '+' or '-', the current list will be cleared
	before adding or removing the languages in list. Until a '-' is
	encountered, each language in the list will be added to the current list.
	As either the '+' or '-' is encountered in the list, the languages
	following it are added or removed from the current list, respectively.
	Thus, it becomes simple to replace the current list with a new one, or
	to add or remove languages from the current list. The actual list of
	files for which tags will be generated depends upon the language
	extension mapping in effect (see the --langmap option). Note that all
	languages, including user-defined languages are enabled unless explicitly
	disabled using this option. Language names included in list may be any
	builtin language or one previously defined with --langdef. The default
	is "all", which is also accepted as a valid argument. See the
	--list-languages option for a complete list of the built-in language names.

--license
	Prints a summary of the software license to standard output, and then exits.

--line-directives[=yes|no]
	Specifies whether "#line" directives should be recognized. These are
	present in the output of preprocessors and contain the line number, and
	possibly the file name, of the original source file(s) from which the
	preprocessor output file was generated. When enabled, this option will
	cause @CTAGS_NAME_EXECUTABLE@ to generate tag entries marked with the
	file names and line numbers of their locations original source file(s),
	instead of their actual locations in the preprocessor output. The actual
	file names placed into the tag file will have the same leading path
	components as the preprocessor output file, since it is assumed that
	the original source files are located relative to the preprocessor
	output file (unless, of course, the #line directive specifies an
	absolute path). This option is off by default. Note: This option is generally
	only useful when used together with the --excmd=number (-n) option.
	Also, you may have to use either the --langmap or --language-force option
	if the extension of the preprocessor output file is not known to
	@CTAGS_NAME_EXECUTABLE@.

--links[=yes|no]
	Indicates whether symbolic links (if supported) should be followed.
	When disabled, symbolic links are ignored. This option is on by default.

--list-kinds[=language|all]
	Lists the tag kinds recognized for either the specified language or all
	languages, and then exits. Each kind of tag recorded in the tag file
	is represented by a one-letter flag, which is also used to filter the
	tags placed into the output through use of the --<LANG>-kinds option.
	Note that some languages and/or tag kinds may be implemented using
	regular expressions and may not be available if regex support is not
	compiled into @CTAGS_NAME_EXECUTABLE@ (see the --regex-<LANG> option).
	Each kind listed is enabled unless followed by "[off]".

--list-maps[=language|all]
	Lists the file extensions and file name patterns which associate a file
	name with a language for either the specified language or all
	languages, and then exits. See the --langmap option, and SOURCE FILES, above.

--list-languages
	Lists the names of the languages understood by @CTAGS_NAME_EXECUTABLE@,
	and then exits. These language names are case insensitive and may be
	used in the --language-force, --languages, --<LANG>-kinds,
	and --regex-<LANG> options.

--options=file|directory
	Read additional options from file or directory. If a file is specified,
	it should contain one option per line. If a directory is specified
	(and scandir function is available at build configuration time), files
	suffixed with .ctags or .conf under the directory are read. (On MSDOS or
	MSWindows this directory traverse feature is temporary disable because
	the contributor of this feature has no access to the platforms.
	Volunters are welcome). As a special case, if --options=NONE is
	specified as the first option on the command line, it will disable
	the automatic reading of any configuration options from either a file
	or the environment (see FILES).

 --quiet[=yes|no]
		Write fewer messages(default is no).

--recurse[=yes|no]
	Recurse into directories encountered in the list of supplied files.
	If the list of supplied files is empty and no file list is specified with
	the -L option, then the current directory (i.e. ".") is assumed.
	Symbolic links are followed. If you don't like these behaviors, either
	explicitly specify the files or pipe the output of find(1) into
	@CTAGS_NAME_EXECUTABLE@ -L- instead. Note: This option is not supported on
	all platforms at present. It is available if the output of the --help
	option includes this option. See, also, the --exclude to limit
	recursion.

--regex-<LANG>=/regexp/replacement/[kind-spec/][flags]
	The /regexp/replacement/ pair define a regular expression replacement
	pattern, similar in style to sed substitution commands, with which to
	generate tags from source files mapped to the named language, <LANG>,
	(case-insensitive; either a built-in or user-defined language). The
	regular expression, regexp, defines an extended regular expression
	(roughly that used by egrep(1)), which is used to locate a single source
	line containing a tag and may specify tab characters using \t. When a
	matching line is found, a tag will be generated for the name defined by
	replacement, which generally will contain the special back-references
	\1 through \9 to refer to matching sub-expression groups within regexp.
	The '/' separator characters shown in the parameter to the option can
	actually be replaced by any character. Note that whichever separator
	character is used will have to be escaped with a backslash ('\')
	character wherever it is used in the parameter as something other than a
	separator. The regular expression defined by this option is added to the
	current list of regular expressions for the specified language
	unless the parameter is omitted, in which case the current list is cleared.

	Unless modified by flags, regexp is interpreted as a Posix extended
	regular expression. The replacement should expand for all matching lines
	to a non-empty string of characters, or a warning message will be
	reported. An optional kind specifier for tags matching regexp may follow
	replacement, which will determine what kind of tag is reported in the
	"kind" extension field (see TAG FILE FORMAT, below). The full form of
	kind-spec is in the form of a single letter, a comma, a name (without
	spaces), a comma, a description, followed by a separator, which specify
	the short and long forms of the kind value and its textual description
	(displayed using --list-kinds). Either the kind name and/or the
	description may be omitted. If kind-spec is omitted, it defaults to
	"r,regex". Finally, flags are one or more single-letter characters having
	the following effect upon the interpretation of regexp:

		b	The pattern is interpreted as a Posix basic regular expression.

		e	The pattern is interpreted as a Posix extended regular expression(default).

		i	The regular expression is to be applied in a case-insensitive manner.

	Note that this option is available only if @CTAGS_NAME_EXECUTABLE@ was
	compiled with support for regular expressions, which depends upon your
	platform. You can determine if support for regular expressions is
	compiled in by examining the output of the --version option, which will
	include "+regex" in the compiled feature list.

	For more information on the regular expressions used by
	@CTAGS_NAME_EXECUTABLE@, see either the regex(5,7) man page, or the GNU
	info documentation for regex (e.g. "info regex").

--sort[=yes|no|foldcase]
	Indicates whether the tag file should be sorted on the tag name
	(default is yes). Note that the original vi(1) required sorted tags.
	The foldcase value specifies case insensitive (or case-folded) sorting.
	Fast binary searches of tag files sorted with case-folding will require
	special support from tools using tag files, such as that found in the
	@CTAGS_NAME_EXECUTABLE@ readtags library, or Vim version 6.2 or higher
	(using "set ignorecase"). This option must appear before the first file
	name. [Ignored in etags mode]

--tag-relative[=yes|no]
	Indicates that the file paths recorded in the tag file should be
	relative to the directory containing the tag file, rather than relative
	to the current directory, unless the files supplied on the command line
	are specified with absolute paths. This option must appear before the
	first file name. The default is yes when running in etags mode (see
	the -e option), no otherwise.

--totals[=yes|no]
	Prints statistics about the source files read and the tag file written
	during the current invocation of @CTAGS_NAME_EXECUTABLE@. This option
	is off by default. This option must appear before the first file name.

--undef[=yes|no]
	Specifies whether a macro tag should be generated from an #undef CPP
	directive (in a C/C++ file), as if it were a #define directive. This
	option is enabled by default.

--verbose[=yes|no]
	Enable verbose mode. This prints out information on option processing
	and a brief message describing what action is being taken for each file
	considered by @CTAGS_NAME_EXECUTABLE@. Normally, @CTAGS_NAME_EXECUTABLE@
	does not read command line arguments until after options are read
	from the configuration files (see FILES, below) and the CTAGS
	environment variable. However, if this option is the first argument on
	the command line, it will take effect before any options are read from
	these sources. The default is no.

--version
	Prints a version identifier for @CTAGS_NAME_EXECUTABLE@ to standard
	output, and then exits. This is guaranteed to always contain the string
	"Universal Ctags".


OPERATIONAL DETAILS
-------------------
As @CTAGS_NAME_EXECUTABLE@ considers each file name in turn, it tries to
determine the language of the file by applying the following three tests
in order: if the file extension has been mapped to a language, if the
filename matches a shell pattern mapped to a language, and finally if the
file is executable and its first line specifies an interpreter using the
Unix-style "#!" specification (if supported on the platform). If a
language was identified, the file is opened and then the appropriate
language parser is called to operate on the currently open file. The parser
parses through the file and adds an entry to the tag file for each
language object it is written to handle. See TAG FILE FORMAT, below, for
details on these entries.

This implementation of @CTAGS_NAME_EXECUTABLE@ imposes no formatting
requirements on C code as do legacy implementations. Older implementations
of ctags tended to rely upon certain formatting assumptions in order to
help it resolve coding dilemmas caused by preprocessor conditionals.

In general, @CTAGS_NAME_EXECUTABLE@ tries to be smart about conditional
preprocessor directives. If a preprocessor conditional is encountered
within a statement which defines a tag, @CTAGS_NAME_EXECUTABLE@ follows
only the first branch of that conditional (except in the special case of
"#if 0", in which case it follows only the last branch). The reason for
this is that failing to pursue only one branch can result in ambiguous
syntax, as in the following example:

.. code-block::

	#ifdef TWO_ALTERNATIVES
	struct {
	#else
	union {
	#endif
		short a;
		long b;
	}

Both branches cannot be followed, or braces become unbalanced and
@CTAGS_NAME_EXECUTABLE@ would be unable to make sense of the syntax.

If the application of this heuristic fails to properly parse a file,
generally due to complicated and inconsistent pairing within the
conditionals, @CTAGS_NAME_EXECUTABLE@ will retry the file using a
different heuristic which does not selectively follow conditional
preprocessor branches, but instead falls back to relying upon a closing
brace ("}") in column 1 as indicating the end of a block once any brace
imbalance results from following a #if conditional branch.

@CTAGS_NAME_EXECUTABLE@ will also try to specially handle arguments lists
enclosed in double sets of parentheses in order to accept the following
conditional construct:

	extern void foo __ARGS((int one, char two));

Any name immediately preceding the "((" will be automatically ignored and
the previous name will be used.

C++ operator definitions are specially handled. In order for consistency
with all types of operators (overloaded and conversion), the operator
name in the tag file will always be preceded by the string "operator "
(i.e. even if the actual operator definition was written as "operator<<").

After creating or appending to the tag file, it is sorted by the tag name,
removing identical tag lines.


TAG FILE FORMAT
---------------

When not running in etags mode, each entry in the tag file consists of a
separate line, each looking like this in the most general case:

tag_name<TAB>file_name<TAB>ex_cmd;"<TAB>extension_fields

The fields and separators of these lines are specified as follows:

	1.	tag name
	2.	single tab character
	3.	name of the file in which the object associated with the tag is located
	4.	single tab character
	5.	EX command used to locate the tag within the file; generally a
		search pattern (either /pattern/ or ?pattern?) or line number (see
		--excmd). Tag file format 2 (see --format) extends this EX command
		under certain circumstances to include a set of extension fields
		(described below) embedded in an EX comment immediately appended
		to the EX command, which leaves it backward-compatible with original
		vi(1) implementations.

A few special tags are written into the tag file for internal purposes.
These tags are composed in such a way that they always sort to the top of
the file. Therefore, the first two characters of these tags are used a magic
number to detect a tag file for purposes of determining whether a
valid tag file is being overwritten rather than a source file.

Note that the name of each source file will be recorded in the tag file
exactly as it appears on the command line. Therefore, if the path you
specified on the command line was relative to the current directory, then
it will be recorded in that same manner in the tag file. See, however,
the --tag-relative option for how this behavior can be modified.

Extension fields are tab-separated key-value pairs appended to the end of
the EX command as a comment, as described above. These key value pairs
appear in the general form "key:value". Their presence in the lines of the
tag file are controlled by the --fields option. The possible keys and
the meaning of their values are as follows:

access
	Indicates the visibility of this class member, where value is specific
	to the language.

file
	Indicates that the tag has file-limited visibility. This key has no
	corresponding value.

kind
	Indicates the type, or kind, of tag. Its value is either one of the
	corresponding one-letter flags described under the various
	--<LANG>-kinds options above, or a full name. It is permitted
	(and is, in fact, the default) for the key portion of this field to be
	omitted. The optional behaviors are controlled with the --fields option.

implementation
	When present, this indicates a limited implementation (abstract vs.
	concrete) of a routine or class, where value is specific to the
	language ("virtual" or "pure virtual" for C++; "abstract" for Java).

inherits
	When present, value. is a comma-separated list of classes from which
	this class is derived (i.e. inherits from).

signature
	When present, value is a language-dependent representation of the
	signature of a routine. A routine signature in its complete form
	specifies the return type of a routine and its formal argument list.
	This extension field is presently supported only for C-based
	languages and does not include the return type.

In addition, information on the scope of the tag definition may be
available, with the key portion equal to some language-dependent construct
name and its value the name declared for that construct in the program.
This scope entry indicates the scope in which the tag was found.
For example, a tag generated for a C structure member would have a scope
looking like "struct:myStruct".


HOW TO USE WITH VI
------------------

Vi will, by default, expect a tag file by the name "tags" in the current
directory. Once the tag file is built, the following commands exercise
the tag indexing feature:

vi -t tag
	Start vi and position the cursor at the file and line where "tag"
	is defined.

:ta tag
	Find a tag.

Ctrl-]
	Find the tag under the cursor.

Ctrl-T
	Return to previous location before jump to tag (not widely implemented).


HOW TO USE WITH GNU EMACS
-------------------------

Emacs will, by default, expect a tag file by the name "TAGS" in the
current directory. Once the tag file is built, the following commands
exercise the tag indexing feature:

M-x visit-tags-table <RET> FILE <RET>
	Select the tag file, "FILE", to use.

M-. [TAG] <RET>
	Find the first definition of TAG. The default tag is the identifier
	under the cursor.

M-*
	Pop back to where you previously invoked "M-.".

C-u M-.
	Find the next definition for the last tag.

For more commands, see the Tags topic in the Emacs info document.


HOW TO USE WITH NEDIT
---------------------

NEdit version 5.1 and later can handle the new extended tag file format
(see --format). To make NEdit use the tag file, select "File->Load Tags
File". To jump to the definition for a tag, highlight the word, then press
Ctrl-D. NEdit 5.1 can can read multiple tag files from different
directories. Setting the X resource nedit.tagFile to the name of a tag
file instructs NEdit to automatically load that tag file at startup time.


CAVEATS
-------

Because @CTAGS_NAME_EXECUTABLE@ is neither a preprocessor nor a compiler,
use of preprocessor macros can fool @CTAGS_NAME_EXECUTABLE@ into either
missing tags or improperly generating inappropriate tags. Although
@CTAGS_NAME_EXECUTABLE@ has been designed to handle certain common cases,
this is the single biggest cause of reported problems. In particular,
the use of preprocessor constructs which alter the textual syntax of C
can fool @CTAGS_NAME_EXECUTABLE@. You can work around many such problems
by using the -I option.

Note that since @CTAGS_NAME_EXECUTABLE@ generates patterns for locating
tags (see the --excmd option), it is entirely possible that the wrong line
may be found by your editor if there exists another source line which is
identical to the line containing the tag. The following example
demonstrates this condition:

.. code-block::

	int variable;

	/* ... */
	void foo(variable)
	int variable;
	{
		/* ... */
	}

Depending upon which editor you use and where in the code you happen to be,
it is possible that the search pattern may locate the local parameter
declaration in foo() before it finds the actual global variable definition,
since the lines (and therefore their search patterns are identical).
This can be avoided by use of the --excmd=n option.

BUGS
----

@CTAGS_NAME_EXECUTABLE@ has more options than ls(1).

When parsing a C++ member function definition (e.g. "className::function"),
@CTAGS_NAME_EXECUTABLE@ cannot determine whether the scope specifier
is a class name or a namespace specifier and always lists it as a class name
in the scope portion of the extension fields. Also, if a C++ function
is defined outside of the class declaration (the usual case), the access
specification (i.e. public, protected, or private) and implementation
information (e.g. virtual, pure virtual) contained in the function
declaration are not known when the tag is generated for the function
definition. It will, however be available for prototypes (e.g --c++-kinds=+p).

No qualified tags are generated for language objects inherited into a class.

ENVIRONMENT VARIABLES
---------------------

CTAGS
	If this environment variable exists, it will be expected to contain a
	set of default options which are read when @CTAGS_NAME_EXECUTABLE@
	starts, after the configuration files listed in FILES, below, are read,
	but before any command line options are read. Options appearing on
	the command line will override options specified in this variable.
	Only options will be read from this variable. Note that all white space
	in this variable is considered a separator, making it impossible to pass
	an option parameter containing an embedded space. If this is a problem,
	use a configuration file instead.

ETAGS
	Similar to the CTAGS variable above, this variable, if found, will be
	read when @ETAGS_NAME_EXECUTABLE@ starts. If this variable is not
	found, @ETAGS_NAME_EXECUTABLE@ will try to use CTAGS instead.

TMPDIR
	On Unix-like hosts where mkstemp() is available, the value of this
	variable specifies the directory in which to place temporary files.
	This can be useful if the size of a temporary file becomes too large
	to fit on the partition holding the default temporary directory
	defined at compilation time. @CTAGS_NAME_EXECUTABLE@ creates temporary
	files only if either (1) an emacs-style tag file is being
	generated, (2) the tag file is being sent to standard output, or
	(3) the program was compiled to use an internal sort algorithm to sort
	the tag files instead of the the sort utility of the operating system.
	If the sort utility of the operating system is being used, it will
	generally observe this variable also. Note that if @CTAGS_NAME_EXECUTABLE@
	is setuid, the value of TMPDIR will be ignored.

FILES
-----

/ctags.cnf (on MSDOS, MSWindows only)

/etc/ctags.conf

@prefix@/etc/ctags.conf

$HOME/.ctags

$HOME/ctags.cnf (on MSDOS, MSWindows only)

.ctags

ctags.cnf (on MSDOS, MSWindows only)
	If any of these configuration files exist, each will be expected to
	contain a set of default options which are read in the order listed
	when @CTAGS_NAME_EXECUTABLE@ starts, but before the CTAGS environment
	variable is read or any command line options are read. This makes it
	possible to set up site-wide, personal or project-level defaults. It
	is possible to compile @CTAGS_NAME_EXECUTABLE@ to read an additional
	configuration file before any of those shown above, which will be
	indicated if the output produced by the --version option lists the
	"custom-conf" feature. Options appearing in the CTAGS environment
	variable or on the command line will override options specified in these
	files. Only options will be read from these files. Note that the option
	files are read in line-oriented mode in which spaces are significant
	(since shell quoting is not possible). Each line of the file is read as
	one command line parameter (as if it were quoted with single quotes).
	Therefore, use new lines to indicate separate command-line arguments.

tags
	The default tag file created by @CTAGS_NAME_EXECUTABLE@.

TAGS
	The default tag file created by @ETAGS_NAME_EXECUTABLE@.


SEE ALSO
--------

The official Universal-ctags web site at:

https://ctags.io/

Also ex(1), vi(1), elvis, or, better yet, vim, the official editor of ctags.
For more information on vim, see the VIM Pages web site at:

http://www.vim.org/


AUTHOR
------

Darren Hiebert <dhiebert@users.sourceforge.net>
http://DarrenHiebert.com/


MOTIVATION
----------

"Think ye at all times of rendering some service to every member of the
human race."

"All effort and exertion put forth by man from the fullness of his heart is
worship, if it is prompted by the highest motives and the will to do
service to humanity."

-- From the Baha'i Writings

CREDITS
-------

This version of @CTAGS_NAME_EXECUTABLE@ was originally derived from and
inspired by the ctags program by Steve Kirkendall <kirkenda@cs.pdx.edu>
that comes with the Elvis vi clone (though virtually none of the original
code remains).

Credit is also due Bram Moolenaar <Bram@vim.org>, the author of vim,
who has devoted so much of his time and energy both to developing the editor
as a service to others, and to helping the orphans of Uganda.

The section entitled "HOW TO USE WITH GNU EMACS" was shamelessly stolen
from the info page for GNU etags.
