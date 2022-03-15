======================================================================
Other changes
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

Many changes have been introduced in Universal Ctags. Use git-log to
review changes not enumerated here, especially in language parsers.

New and extended options
---------------------------------------------------------------------

``--exclude-exception``, an option complementing ``--exclude``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See :ref:`option_input_output_file` in :ref:`ctags(1) <ctags(1)>`.

``--maxdepth`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See :ref:`option_input_output_file` in :ref:`ctags(1) <ctags(1)>`.

``--input-encoding=ENCODING`` and ``--output-encoding=ENCODING``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. TODO: Review...

People may use their own native language in source code comments (or
sometimes in identifiers) and in such cases encoding may become an issue.
Nowadays UTF-8 is the most widely used encoding, but some source codes
still use legacy encodings like latin1, cp932 and so on. These options
are useful for such files.

ctags doesn't consider the input encoding; it just reads input as a
sequence of bytes and uses them as is when writing tags entries.

On the other hand Vim does consider input encoding. When loading a
file, Vim converts the file contents into an internal format with one
of the encodings specified in its `fileencodings` option.

As a result of this difference, Vim cannot always move the cursor to
the definition of a tag as users expect when attempting to match the
patterns in a tags file.

The good news is that there is a way to notify Vim of the encoding
used in a tags file with the ``TAG_FILE_ENCODING`` pseudo-tag.

Two new options have been introduced (``--input-encoding=IN`` and
``--output-encoding=OUT``).

Using the encoding specified with these options ctags converts input
from ``IN`` to ``OUT``. ctags uses the converted strings when writing
the pattern parts of each tag line. As a result the tags output is
encoded in ``OUT`` encoding.

In addition ``OUT`` is specified at the top the tags file as the
value for the ``TAG_FILE_ENCODING`` pseudo-tag. The default value of
``OUT`` is UTF-8.

NOTE: Converted input is NOT passed to language parsers.
The parsers still deal with input as a byte sequence.

With ``--input-encoding-<LANG>=IN``, you can specify a specific input
encoding for ``LANG``. It overrides the global default value given
with ``--input-encoding``.

The example usage can be found in *Tmain/{input,output}-encoding-option.d*.

Acceptable ``IN`` and ``OUT`` values can be listed with *iconv -l* or
*iconv --list*. It is platform dependant.

To enable the option, libiconv is needed on your platform.
On Windows mingw (without msys2), you must specify ``WITH_ICONV=yes``
like this::

	C:\dev\ctags>mingw32-make -f mk_mingw.mak WITH_ICONV=yes

``--list-features`` helps you to know whether your ctags executable
links to libiconv or not. You will find ``iconv`` in the output if it
links to.

See also :ref:`option_output_format` in :ref:`ctags(1) <ctags(1)>`.

``--map-<LANG>`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

``--map-<LANG>`` is newly introduced to control the file name
to language mappings (langmap) with finer granularity than
``--langmap`` allows.

A langmap entry is defined as a pair; the name of the language and a
file name extension (or pattern).

Here we use "spec" as a generic term representing both file name
extensions and patterns.

``--langmap`` maps specs to languages exclusively::

  $ ctags --langdef=FOO --langmap=FOO:+.ABC \
	    --langdef=BAR --langmap=BAR:+.ABC  \
	    --list-maps | grep '\*.ABC$'
  BAR      *.ABC

Though language `FOO` is added before `BAR`, only `BAR` is set as a
handler for the spec `*.ABC`.

Universal Ctags enables multiple parsers to be configured for a spec.
The appropriate parser for a given input file can then be chosen by a
variety of internal guessing strategies (see :ref:`Determining file language
<guessing>`).

Let's see how specs can be mapped non-exclusively with
``--map-<LANG>``::

    $ ctags --langdef=FOO --map-FOO=+.ABC \
	      --langdef=BAR --map-BAR=+.ABC \
	      --list-maps | grep '\*.ABC$'
    FOO      *.ABC
    BAR      *.ABC

Both `FOO` and `BAR` are registered as handlers for the spec `*.ABC`.

``--map-<LANG>`` can also be used for removing a langmap entry.::

    $ ctags --langdef=FOO --map-FOO=+.ABC \
	      --langdef=BAR --map-BAR=+.ABC \
	      --map-FOO=-.ABC --list-maps | grep '\*.ABC$'
    BAR      *.ABC

    $ ctags --langdef=FOO --map-FOO=+.ABC \
	      --langdef=BAR --map-BAR=+.ABC \
	      --map-BAR=-.ABC --list-maps | grep '\*.ABC$'
    FOO      *.ABC

    $ ctags --langdef=FOO --map-FOO=+.ABC \
	     --langdef=BAR --map-BAR=+.ABC \
	     --map-BAR=-.ABC --map-FOO=-.ABC  --list-maps | grep '\*.ABC$'
    (NOTHING)

``--langmap`` provides a way to manipulate the langmap in a
spec-centric manner and ``--map-<LANG>`` provides a way to manipulate
the langmap in a parser-centric manner.

See also :ref:`option_lang_mapping` in :ref:`ctags(1) <ctags(1)>`.

Guessing parser from file contents (``-G`` option)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See :ref:`guessing` in :ref:`ctags(1) <ctags(1)>`.

Including line number to pattern field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use ``--excmd=number``.
See :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

Long names in kinds, fields, and extra options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A letter is used for specifying a kind, a field, or an extra entry.
In Universal Ctags a name can also be used.

Surround the name with braces (`{` and `}`) in values assigned to the
options, ``--kind-<LANG>=``, ``--fields=``, or ``--extras=``.

.. code-block:: console

	$ ctags --kinds-C=+L-d ...

This command line uses the letters, `L` for enabling the label kind
and `d` for disabling the macro kind of C. The command line can be
rewritten with the associated names.

.. code-block:: console

	$ ctags --kinds-C='+{label}-{macro}' ...

The quotes are needed because braces are interpreted as meta
characters by the shell.

The available names can be listed with ``--list-kinds-full``,
``--list-fields``, or ``--list-extras``.

See also :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

Wildcard in options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the purpose of gathering as much as information as possible from
source code the "wildcard"(``*``) option value has been introduced.

``--extras=*``
	Enables all extra tags.

``--fields=*``
	Enables all available fields.

``--kinds-<LANG>=*``
	Enables all available kinds for ``LANG``.

``--kinds-all=*``
	Enables all available kinds for all available language parsers.

See also :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

Extra tag entries (``--extras``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
``--extra`` option in Exuberant Ctags is renamed to ``--extras`` (plural) in
Universal Ctags for making consistent with ``--kinds-<LANG>`` and ``--fields``.

These extra tag entries are newly introduced.

``F``
	Replacement for --file-scope.

``p``
	Include pseudo-tags.

..
	NOT REVIEWED YET

See also :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

Kinds synchronization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See the description about ``--kinds-<LANG>`` and ``--list-kinds-full``
option on :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

Enabling/disabling pseudo-tags (``--pseudo-tags`` option)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

See :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>` and
:ref:`ctags-client-tools(7) <ctags-client-tools(7)>` about the option.

``--put-field-prefix`` options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

"always" and "never" as an argument for ``--tag-relative``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``--tag-relative`` option is extend.
See :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>`.

Defining a parser specific extra
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new ``--_extradef-<LANG>=name,description`` option allows you to
defining a parser specific extra which turning on and off can be
referred from a regex based parser for ``<LANG>``.

See :ref:`Conditional tagging with extras <extras>` for more details.

Defining a CPreProcessor macro from command line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Newly introduced ``-D`` option extends the function provided by
``-I`` option.

``-D`` emulates the behaviour of the corresponding gcc option:
it defines a C preprocessor macro.

See :ref:`option_tags_file_contents` in :ref:`ctags(1) <ctags(1)>` and
:ref:`cxx` for more details.

Options for inspecting ctags internals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Exuberant Ctags provides a way to inspect its internals via
``--list-kinds``, ``--list-languages``, and ``--list-maps``.

This idea has been expanded in Universal Ctags with
``--list-kinds-full``, ``--list-map-extensions``,  ``--list-extras``,
``--list-features``, ``--list-fields``, ``--list-map-patterns``, and
``--list-pseudo-tags`` being added.

The original three ``--list-`` options are not changed for
compatibility reasons, however, the newly introduced options are
recommended for all future use.

By default, interactive use is assumed and ctags tries aligning the
list output in columns for easier reading.

When ``--machinable`` is given before a ``--list-`` option, ctags
outputs the list in a format more suitable for processing by scripts.
Tab characters are used as separators between columns. The alignment
of columns is never considered when ``--machinable`` is given.

Currently only ``--list-extras``, ``--list-fields`` and
``--list-kinds-full`` support ``--machinable`` output.

These new ``--list-`` options also print a column header, a line
representing the name of each column. The header may help users and
scripts to understand and recognize the columns. Ignoring the column
header is easy because it starts with a `#` character.

``--with-list-header=no`` suppresses output of the column header.

See also :ref:`option_listing` in :ref:`ctags(1) <ctags(1)>`.

Notice messages and ``--quiet``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There were 3 classes of message in Exuberant Ctags.
In addition to them Universal Ctags introduced a new class of message, *notice*.

*fatal*
	A critical error has occurred and ctags aborts the execution.

*warning*
	An error has occurred but ctags continues the execution.

*notice* (new)
    It is less important than *warning* but more important for users than *verbose*.

*verbose*
	Mainly used for debugging purposes.

Generally the user can ignore *notice* class messages and ``--quiet``
can be used to disable them.

*verbose* class messages are disabled by default, and ``--verbose`` or ``-V``
can be used to enable them.

See also :ref:`option_misc` in :ref:`ctags(1) <ctags(1)>`.

Skipping utf-8 BOM
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The three bytes sequence(``\xEF\xBB\xBF``) at the head of an input
file is skipped when parsing.

TODO:

* Do the same in guessing and selecting parser stage.
* Refect the BOM detection to encoding option

Interactive mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new ``--_interactive`` option launches a JSON based command REPL which
can be used to control ctags generation programmatically.

See :ref:`interactive-mode` for more details.

PCRE2 regular expression
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With ``{pcre2}`` (or ``p``) flag, PCRE2 expressions can be used in
``--regex-<LANG>=``, ``--mline-regex-<LANG>=``, and
``--_mtable-regex-<LANG>=`` if the ctags is built with ``pcre2`` library.


Incompatible changes in command line
---------------------------------------------------------------------

.. NOT REVIEWED YET

``-D`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a ctags binary that had debugging output enabled in the build config
stage, ``-D`` was used for specifying the level of debugging
output. It is changed to ``-d``. This change is not critical because
``-D`` option was not described in ctags.1 man page.

Instead ``-D`` is used for defining a macro in CPreProcessor parser.

Changes imported from Exuberant Ctags
---------------------------------------------------------------------
See "Exuberant Ctags" in "Tracking other projects" for detailed
information regarding imported changes.

Some changes have also been imported from Fedora and Debian.

Parser related changes
---------------------------------------------------------------------

New parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following parsers have been added:

* Abaqus
* Abc
* Ada
* AnsiblePlaybook *libyaml*
* Asciidoc
* Autoconf
* Automake
* AutoIt
* BibTeX
* Clojure
* CMake *optlib*
* CSS
* Ctags option library *optlib*
* CUDA
* D
* DBusIntrospect *libxml*
* Diff
* DTD
* DTS
* Elixir *optlib*
* Elm *optlib*
* Falcon
* FunctionParameters *perl based subparser*
* Gdbinit script *optlib*
* GemSpec *Ruby based subparser*
* GDScript
* Glade *libxml*
* Go
* Haskell
* Haxe
* iPythonCell *optlib*, *pthon based subparser*
* Inko *optlib*
* JavaProperties
* JSON
* Julia
* Kconfig *optlib*
* Kotlin *peg/packcc*
* GNU linker script(LdScript)
* LEX *optlib*
* Man page *optlib*
* Markdown
* Maven2 *libxml*
* MesonBuild (Meson) *optlib*
* MesonOptions *optlib+script*
* Moose *perl based subparser*
* Myrddin
* M4
* NSIS
* ObjectiveC
* Org *optlib*
* OpenAPI (3.x.x / Swagger 2.0) *Yaml based subparser*
* Passwd *optlib*
* PuppetManifest *optlib*
* Perl6
* Pod *optlib*
* PowerShell
* PropertyList(plist) *libxml*
* Protobuf
* PythonLoggingConfig
* QemuHX *optlib*
* QtMoc
* R
* R6Class *R based subparser*
* RelaxNG *libxml*
* ReStructuredText
* RMarkdown *Markdown based subparser*
* Robot
* RpmMacros *optlib*
* RpmSpec
* RSpec *Ruby based subparser*
* Rust
* S4Class *R based subparser*
* SCSS *optlib*
* SystemdUnit
* SystemTap *optlib*
* SystemVerilog
* SVG *libxml*
* TclOO (see :ref:`The new Tcl parser <tcl>`)
* Thrift *peg/packcc*
* TTCN
* Txt2tags
* TypeScript
* Varlink *peg/packcc*
* WindRes
* XSLT v1.0 *libxml*
* Yacc
* Yaml *libyaml*
* YumRepo
* Zephir

See :ref:`optlib` for details on *optlib*.
Libxml2 is required to use the parser(s) marked with *libxml*.
Libyaml is required to use the parser(s) marked with *libyaml*.

TIPS: you can list newly introduced parsers if you also have
Exuberant Ctags installed with following command line:

.. code-block:: console

		$ diff -ruN <(universal-ctags --list-languages) <(exuberant-ctags --list-languages)  | grep '^[-+]'

Fully improved parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* C (see :ref:`The new C/C++ parser <cxx>`)
* C++ (see :ref:`The new C/C++ parser <cxx>`)
* Python (see :ref:`The new Python parser <python>`)
* HTML (see :ref:`The new HTML parser <html>`)
* Tcl (see :ref:`The new Tcl parser <tcl>`)
* ITcl (see :ref:`The new Tcl parser <tcl>`)
* Ant (rewritten with *libxml*)
* PHP
* Verilog/SystemVerilog

Automatically expanding CPreProcessor macros defined in the same input file (HIGHLY EXPERIMENTAL)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See :ref:`The new C/C++ parser <cxx>` for more details.

Readtags
---------------------------------------------------------------------

Printing line numbers with ``-n``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See :ref:`readtags(1) <readtags(1)>`.

Filtering in readtags command
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See :ref:`readtags(1) <readtags(1)>`.

readtags has ability to find tag entries by name.

The concept of filtering is inspired by the display filter of
Wireshark. You can specify more complex conditions for searching.

All symbols starting with `$` represent a field of a tag entry which
is being tested against the S expression. Most will evaluate as a
string or `#f`. It evaluates to `#f` when the field doesn't exist.

The `scope` field holds structured data: the kind and name of the
upper scope combined with `:`. The hold the value is stored to
`$scope`. The kind part is mapped to `$scope-kind`, and the name part
to `$scope-name`.

`$scope-kind` and `$scope-name` can only be used if the input tags
file is generated by ctags with ``--fields=+Z``.

`$` is a generic accessor for accessing extension fields.
`$` takes one argument: the name of an extension field.
It returns the value of the field as a string if a value
is given, or `#f`.

Following examples shows how `prefix?`, `suffix?`, and
`substr?` work.
::

	(prefix? "TARGET" "TA")
	=> #t

	(prefix? "TARGET" "RGET")
	=> #f

	(prefix? "TARGET" "RGE")
	=> #f

	(suffix? "TARGET" "TA")
	=> #f

	(suffix? "TARGET" "RGET")
	=> #t

	(suffix? "TARGET" "RGE")
	=> #f

	(substr? "TARGET" "TA")
	=> #t

	(suffix? "TARGET" "RGET")
	=> #t

	(suffix? "TARGET" "RGE")
	=> #t

	(and (suffix? "TARGET" "TARGET")
	     (prefix? "TARGET" "TARGET")
	     (substr? "TARGET" "TARGET")
	=> #t


Sorting in readtags command
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
readtags can sort the tag entries before printing.
You can specify the way to sort with -S option. Like ``-Q`` option, ``-S``
also takes an S expression.

See :ref:`readtags(1) <readtags(1)>`.


Listing pseudo tags with ``-D``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See :ref:`readtags(1) <readtags(1)>`.
