======================================================================
Introduced changes
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

Many changes have been introduced in Universal-ctags. Use git-log to
review changes not enumerated here, especially in language parsers.

Importing changes from Exuberant-ctags
---------------------------------------------------------------------
See "Exuberant-ctags" in "Tracking other projects" for detailed
information regarding imported changes.

Some changes have also been imported from Fedora and Debian.

Parser related changes
---------------------------------------------------------------------

Fully rewritten parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* C (see :ref:`The new C/C++ parser <cxx>`)
* C++ (see :ref:`The new C/C++ parser <cxx>`)
* Python (see :ref:`The new Python parser <python>`)
* HTML (see :ref:`The new HTML parser <html>`)
* Tcl (see :ref:`The new Tcl parser <tcl>`)
* ITcl (see :ref:`The new Tcl parser <tcl>`)

New parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following parsers have been added:

* Ada
* AnsiblePlaybook *libyaml*
* Autoconf
* Automake
* AutoIt
* Clojure
* CSS
* Ctags option library *optlib*
* CUDA
* D
* DBusIntrospect *libxml*
* Diff
* DTD
* DTS
* Elm *optlib*
* Falcon
* Gdbinit script *optlib*
* Glade *libxml*
* Go
* JavaProperties
* JSON
* GNU linker script(LdScript)
* Man page *optlib*
* Markdown *optlib*
* Maven2 *libxml*
* M4
* ObjectiveC
* Passwd *optlib*
* PuppetManifest *optlib*
* Perl6
* Pod *optlib*
* PropertyList(plist) *libxml*
* Protobuf
* PythonLoggingConfig
* QemuHX *optlib*
* QtMoc
* R
* RelaxNG *libxml*
* ReStructuredText
* Robot
* RpmSpec
* Rust
* SystemdUnit
* SystemVerilog
* SVG *libxml*
* TclOO (see :ref:`The new Tcl parser <tcl>`)
* TTCN
* WindRes
* XSLT v1.0 *libxml*
* Yacc
* Yaml *libyaml*
* YumRepo
* Zephir
* Myrddin
* RSpec *optlib*

See "Option library" for details on *optlib*.
Libxml2 is required to use the parser(s) marked with *libxml*.
Libyaml is required to use the parser(s) marked with *libyaml*.

TIPS: you can list newly introduced parsers if you also have
Exuberant-ctags installed with following command line:

.. code-block:: console

		$ diff -ruN <(universal-ctags --list-languages) <(exuberant-ctags --list-languages)  | grep '^[-+]'


Heavily improved parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Ant (rewritten with *libxml*)
* PHP
* Verilog


`F` kind usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

``F`` is used as a kind letter for file kind in Exuberant-ctags; the
``F`` was hard-coded in ctags internal. However, we found some built-in
parsers including Ruby uses ``F`` for their own purpose. So if you
find a tag having ``F`` as a kind letter, you cannot say what it is
well: a file name or something peculiar in the language. Long kind
description strings may help you but we are not sure all tools
utilizing ``tags`` file refer the long kind description strings.

Universal-ctags disallows parsers to use ``F`` their own purpose
in both built-in and optlib parsers.

``F`` in built-in parsers are replaced as follows:

============  ================  ===========
Language      Long description  Replacement
============  ================  ===========
ObjectiveC    field             E
Ruby          singletonMethod   S
Rust          method            P
SQL           field             E
============  ================  ===========



New and extended options
---------------------------------------------------------------------

Wildcard in options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the purpose of gathering as much as information as possible from
source code the "wildcard"(``*``) option value has been introduced.

``--extras=*``

	Enables all extra tags.

``--fields=*``

	Enables all available fields.

``--<LANG>-kinds=*``

	Enables all available kinds for ``LANG``.

``--kinds-<LANG>=*``

	Alternative representation of ``--<LANG>-kinds=*``.

``--all-kinds=SPEC``

	Applies SPEC as kinds to all available language parsers.

``--all-kinds=*``

	Enables all available kinds for all available language parsers.


Long names in kinds, fields, and extra options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A letter is used for specifying a kind, a field, or an extra entry.
In Universal-ctags a name can also be used.

Surround the name with braces (`{` and `}`) in values assigned to the
options, ``--kind-<LANG>=``, ``--fields=``, or ``--extras=``.

.. code-block:: console

	$ ./ctags --kinds-C=+L-d ...

This command line uses the letters, `L` for enabling the label kind
and `d` for disabling the macro kind of C. The command line can be
rewritten with the associated names.

.. code-block:: console

	$ ./ctags --kinds-C='+{label}-{macro}' ...

The quotes are needed because braces are interpreted as meta
characters by the shell.

The available names can be listed with ``--list-kinds-full``,
``--list-fields``, or ``--list-extras``.



Notice messages and ``--quiet``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There were 3 classes of message in ctags:

*fatal*

	A critical error has occurred and ctags aborts the execution.

*warning*

	An error has occurred but ctags continues the execution.

*verbose*

	Mainly used for debugging purposes.


*notice* is a new class of message. It is less important than
*warning* but more important for users than *verbose*.

Generally the user can ignore *notice* class messages and ``--quiet``
can be used to disable them.

``--input-encoding=ENCODING`` and ``--output-encoding=ENCODING``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. TODO: Review...

Japanese programmers sometimes use the Japanese language in source
code comments. Of course, it is not limited to Japanese. People may
use their own native language and in such cases encoding becomes an
issue.

ctags doesn't consider the input encoding; it just reads input as a
sequence of bytes and uses them as is when writing tags entries.

On the other hand Vim does consider input encoding. When loading a
file, Vim converts the file contents into an internal format with one
of the encodings specified in its `fileencodings` option.

As a result of this difference, Vim cannot always move the cursor to
the definition of a tag as users expect when attempting to match the
patterns in a tags file.

The good news is that there is a way to notify Vim of the encoding
used in a tags file with the ``TAG_FILE_ENCODING`` pseudo tag.

Two new options have been introduced (``--input-encoding=IN`` and
``--output-encoding=OUT``).

Using the encoding specified with these options ctags converts input
from ``IN`` to ``OUT``. ctags uses the converted strings when writing
the pattern parts of each tag line. As a result the tags output is
encoded in ``OUT`` encoding.

In addition ``OUT`` is specified at the top the tags file as the
value for the ``TAG_FILE_ENCODING`` pseudo tag. The default value of
``OUT`` is UTF-8.

NOTE: Converted input is NOT passed to language parsers.
The parsers still deal with input as a byte sequence.

With ``--input-encoding-<LANG>=IN``, you can specify a specific input
encoding for ``LANG``. It overrides the global default value given
with ``--input-encoding``.

The example usage can be found in *Tmain/{input,output}-encoding-option.d*.

Acceptable ``IN`` and ``OUT`` values can be listed with *iconv -l* or
*iconv --list*. It is platform dependant.

To enable the option, libiconv is needed on your platform. In addition
``--enable-iconv`` must be given to configure before making ctags.
On Windows mingw32, you must specify ``WITH_ICONV=yes`` like this::

	C:\dev\ctags>mingw32-make -f mk_mingw.mak WITH_ICONV=yes

``--list-features`` helps you to know whether your ctags executable
links to libiconv or not. You will find ``iconv`` in the output if it
links to.

Extra tag entries (``--extras``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
``--extra`` option in Exuberant-ctags is renamed to ``--extras`` (plural) in
Universal-ctags for making consistent with ``--kinds-<LANG>`` and ``--fields``.

These extra tag entries are newly introduced.

``F``

	Equivalent to --file-scope.

``p``

	Include pseudo tags.


Options for inspecting ctags internals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Exuberant-ctags provides a way to inspect its internals via
``--list-kinds``, ``--list-languages``, and ``--list-maps``.

This idea has been expanded in Universal-ctags with
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

Kinds synchronization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

In Universal-ctags, as in Exuberant-ctags, most kinds are parser
local; enabling (or disabling) a kind in a parser has no effect on
kinds in any other parsers even those with the same name and/or
letter.

However, there are exceptions, such as C and C++ for example. C++ can
be considered a language extended from C. Therefore it is natural
that all kinds defined in the C parser are also defined in the C++
parser. Enabling a kind in the C parser also enables a kind having
the same name in the C++ parser, and vice versa.

A kind group is a group of kinds satisfying the following conditions:

1. Having the same name and letter, and
2. Being synchronized with each other

A master parser manages the synchronization of a kind group. The
`MASTER` column of ``--list-kinds-full`` shows the master parser of
the kind.

Internally, a state change (enabled or disabled with
``--kind-<LANG>=[+|-]...``) of a kind in a kind group is reported to
its master parser as an event. Then the master parser updates the
state of all kinds in the kind group as specified with the option.

.. code-block:: console

    $ ./ctags --list-kinds-full=C++
    #LETTER NAME            ENABLED  REFONLY NROLES MASTER     DESCRIPTION
    d       macro           on       FALSE   1      C          macro definitions
    ...
    $ ./ctags --list-kinds-full=C
    #LETTER NAME            ENABLED  REFONLY NROLES MASTER     DESCRIPTION
    d       macro           on       FALSE   1      C          macro definitions
    ...

The example output indicates that the `d` kinds of both the C++ and C
parsers are in the same group and that the `C` parser manages the
group.

.. code-block:: console

    $ ./ctags --kinds-C++=-d --list-kinds-full=C | head -2
    #LETTER NAME            ENABLED  REFONLY NROLES MASTER     DESCRIPTION
    d       macro           off      FALSE   1      C          macro definitions
    $ ./ctags --kinds-C=-d --list-kinds-full=C | head -2
    #LETTER NAME            ENABLED  REFONLY NROLES MASTER     DESCRIPTION
    d       macro           off      FALSE   1      C          macro definitions
    $ ./ctags --kinds-C++=-d --list-kinds-full=C++ | head -2
    #LETTER NAME            ENABLED  REFONLY NROLES MASTER     DESCRIPTION
    d       macro           off      FALSE   1      C          macro definitions
    $ ./ctags --kinds-C=-d --list-kinds-full=C++ | head -2
    #LETTER NAME            ENABLED  REFONLY NROLES MASTER     DESCRIPTION
    d       macro           off      FALSE   1      C          macro definitions

In the above example, the `d` kind is disabled via C or C++.
Disabling a `d` kind via one language disables the `d` kind for the
other parser, too.


``--put-field-prefix`` options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some fields are newly introduced in Universal-ctags and more will be
introduced in the future. Other tags generators may also introduce
their own fields.

In such a situation there is a concern about conflicting field names;
mixing tags files generated by multiple tags generators including
Universal-ctags is difficult.

``--put-field-prefix`` provides a workaround for this use case. When
``--put-field-prefix`` is given, ctags adds "UCTAGS" as a prefix to
newly introduced fields.

.. code-block:: console

    $ cat /tmp/foo.h
    #include <stdio.h>
    $ ./ctags -o - --extras=+r --fields=+r /tmp/foo.h
    stdio.h	/tmp/foo.h	/^#include <stdio.h>/;"	h	roles:system
    $ ./ctags --put-field-prefix -o - --extras=+r --fields=+r /tmp/foo.h
    stdio.h	/tmp/foo.h	/^#include <stdio.h>/;"	h	UCTAGSroles:system

In this example, ``roles`` is prefixed.

``--maxdepth`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

``--maxdepth`` limits the depth of directory recursion enabled with
the ``-R`` option.

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

  $ ./ctags --langdef=FOO --langmap=FOO:+.ABC \
	    --langdef=BAR --langmap=BAR:+.ABC  \
	    --list-maps | grep '\*.ABC$'
  BAR      *.ABC

Though language `FOO` is added before `BAR`, only `BAR` is set as a
handler for the spec `*.ABC`.

Universal-ctags enables multiple parsers to be configured for a spec.
The appropriate parser for a given input file can then be chosen by a
variety of internal guessing strategies (see "Choosing a proper
parser in ctags").

Let's see how specs can be mapped non-exclusively with
``--map-<LANG>``::

    % ./ctags --langdef=FOO --map-FOO=+.ABC \
	      --langdef=BAR --map-BAR=+.ABC \
	      --list-maps | grep '\*.ABC$'
    FOO      *.ABC
    BAR      *.ABC

Both `FOO` and `BAR` are registered as handlers for the spec `*.ABC`.

``--map-<LANG>`` can also be used for removing a langmap entry.::

    $ ./ctags --langdef=FOO --map-FOO=+.ABC \
	      --langdef=BAR --map-BAR=+.ABC \
	      --map-FOO=-.ABC --list-maps | grep '\*.ABC$'
    BAR      *.ABC

    $ ./ctags --langdef=FOO --map-FOO=+.ABC \
	      --langdef=BAR --map-BAR=+.ABC \
	      --map-BAR=-.ABC --list-maps | grep '\*.ABC$'
    FOO      *.ABC

    $./ctags --langdef=FOO --map-FOO=+.ABC \
	     --langdef=BAR --map-BAR=+.ABC \
	     --map-BAR=-.ABC --map-FOO=-.ABC  --list-maps | grep '\*.ABC$'
    (NOTHING)

``--langmap`` provides a way to manipulate the langmap in a
spec-centric manner and ``--map-<LANG>`` provides a way to manipulate
the langmap in a parser-centric manner.


Guessing parser from file contents (``-G`` option)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

See "Choosing a proper parser in ctags" section.


Enabling/disabling pseudo tags (``--pseudo-tags`` option)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each pseudo tag can be enabled/disabled with ``--pseudo-tags``.
::

	--pseudo-tags=+ptag
	--pseudo-tags=-ptag

When prefixed with `+`, the pseudo tag specified as ``ptag`` is
enabled.  When prefixed with `-`, the pseudo tag is disabled.
``--list-pseudo-tags`` shows all recognized ptag names.

All pseudo tags are enabled if `*` is given as the value of ptag
like::

	--pseudo-tags='*'

All pseudo tags are disabled if no option value is given to
``--pseudo-tags`` like::

	--pseudo-tags=

To specify only a single pseudo tag, omit the sign::

	--pseudo-tags=ptag

JSON output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Experimental JSON output has been added. ``--output-format`` can be
used to enable it.

.. code-block:: console

   $ ./ctags --output-format=json --fields=-s /tmp/foo.py
   {"_type": "tag", "name": "Foo", "path": "/tmp/foo.py", "pattern": "/^class Foo:$/", "kind": "class"}
   {"_type": "tag", "name": "doIt", "path": "/tmp/foo.py", "pattern": "/^    def doIt():$/", "kind": "member"}


See :ref:`JSON output <output-json>` for more details.

"always" and "never" as an argument for --tag-relative
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..
	NOT REVIEWED YET

Even if "yes" is specified as an option argument for --tag-relative,
absolute paths are used in tags output if an input is given as
an absolute path. This behavior is expected in exuberant-ctags
as written in its man-page.

In addition to "yes" and "no", universal-ctags takes "never" and "always".

If "never" is given, absolute paths are used in tags output regardless
of the path representation for input file(s). If "always" is given,
relative paths are used always.


Defining a macro in CPreProcessor input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Newly introduced ``-D`` option extends the function provided by
``-I`` option.

``-D`` emulates the behaviour of the corresponding gcc option:
it defines a C preprocessor macro. All types of macros are supported,
including the ones with parameters and variable arguments.
Stringification, token pasting and recursive macro expansion are also supported.

``-I`` is now simply a backward-compatible syntax to define a
macro with no replacement.

Some examples follow.

.. code-block:: console

	$ ctags ... -D IGNORE_THIS ...

With this commandline the following C/C++ input

.. code-block:: C

	int IGNORE_THIS a;

will be processed as if it was

.. code-block:: C

	int a;

Defining a macro with parameters uses the following syntax:

.. code-block:: console

	$ ctags ... -D "foreach(arg)=for(arg;;)" ...

This example defines `for(arg;;)` as the replacement `foreach(arg)`.
So the following C/C++ input

.. code-block:: C

	foreach(char * p,pointers)
	{

	}

is processed in new C/C++ parser as:

.. code-block:: C

	for(char * p;;)
	{

	}

and the p local variable can be extracted.

The previous commandline includes quotes since the macros generally contain
characters that are treated specially by the shells. You may need some escaping.

Token pasting is performed by the ## operator, just like in the normal
C preprocessor.

.. code-block:: console

	$ ctags ... -D "DECLARE_FUNCTION(prefix)=int prefix ## Call();"

So the following code

.. code-block:: C

	DECLARE_FUNCTION(a)
	DECLARE_FUNCTION(b)

will be processed as

.. code-block:: C

	int aCall();
	int bCall();

Macros with variable arguments use the gcc __VA_ARGS__ syntax.

.. code-block:: console

	$ ctags ... -D "DECLARE_FUNCTION(name,...)=int name(__VA_ARGS__);"

So the following code

.. code-block:: C

	DECLARE_FUNCTION(x,int a,int b)

will be processed as

.. code-block:: C

	int x(int a,int b);

``--_interactive`` Mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new ``--_interactive`` option launches a JSON based command REPL which
can be used to control ctags generation programmatically.

See :ref:`--_interactive Mode <interactive-mode>` for more details.

``--_interactive=sandbox`` adds up seccomp filter. See
:ref:`sandbox submode <sandbox-submode>` for more details.

Defining a kind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

A new ``--kinddef-<LANG>=letter,name,description`` option reduces the
typing defining a regex pattern with ``--regex-<LANG>=``, and keeps
the consistency of dynamically defined kinds in a language.

A kind letter defined with ``--kinddef-<LANG>`` can be referred in
``--kinddef-<LANG>``.

Previously you had to write in your optlib::

    --regex-elm=/^([[:lower:]_][[:alnum:]_]*)[^=]*=$/\1/f,function,Functions/{scope=set}
    --regex-elm=/^[[:blank:]]+([[:lower:]_][[:alnum:]_]*)[^=]*=$/\1/f,function,Functions/{scope=ref}

With new ``--kinddef-<LANG>`` you can write the same things like::

    --kinddef-elm=f,function,Functions
    --regex-elm=/^([[:lower:]_][[:alnum:]_]*)[^=]*=$/\1/f/{scope=set}
    --regex-elm=/^[[:blank:]]+([[:lower:]_][[:alnum:]_]*)[^=]*=$/\1/f/{scope=ref}

We can say now "kind" is a first class object in Universal-ctags.

..
	NOT REVIEWED YET

Defining an extra
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new ``--_extradef-<LANG>=name,description`` option allows you to
defining a parser own extra which turning on and off can be
referred from a regex based parser for ``<LANG>``.

See :ref:`Conditional tagging with extras <extras>` for more details.


..
	NOT REVIEWED YET

.. _defining-subparsers:

Defining a subparser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Basic
......................................................................

About the concept of subparser, see :ref:`Tagging definitions of higher(upper) level language (sub/base) <base-sub-parsers>`.

With ``base`` long flag of `--langdef=<LANG>` option, you can define
a subparser for a specified base parser. Combining with ``--kinddef-<LANG>``
and ``--regex-<KIND>`` options, you can extend an existing parser
without risk of kind confliction.

Let's see an example.

input.c

.. code-block:: C

    static int set_one_prio(struct task_struct *p, int niceval, int error)
    {
    }

    SYSCALL_DEFINE3(setpriority, int, which, int, who, int, niceval)
    {
	    ...;
    }

.. code-block:: console

    $./ctags --options=NONE  -x --_xformat="%20N %10K %10l"  -o - input.c
    ctags: Notice: No options will be read from files or environment
	    set_one_prio   function          C
	 SYSCALL_DEFINE3   function          C

C parser doesn't understand that `SYSCALL_DEFINE3` is a macro for defining an
entry point for a system.

Let's define `linux` subparser which using C parser as a base parser:

.. code-block:: console

    $ cat linux.ctags
    --langdef=linux{base=C}
    --kinddef-linux=s,syscall,system calls
    --regex-linux=/SYSCALL_DEFINE[0-9]\(([^, )]+)[\),]*/\1/s/

The output is change as follows with `linux` parser:

.. code-block:: console

	$ ./ctags --options=NONE --options=./linux.ctags -x --_xformat="%20N %10K %10l"  -o - input.c
	ctags: Notice: No options will be read from files or environment
		 setpriority    syscall      linux
		set_one_prio   function          C
	     SYSCALL_DEFINE3   function          C

`setpriority` is recognized as a `syscall` of `linux`.

Using only `--regex-C=...` you can capture `setpriority`.
However, there were concerns about kind confliction; when introducing
a new kind with `--regex-C=...`, you cannot use a letter and name already
used in C parser and `--regex-C=...` options specified in the other places.

You can use a newly defined subparser as a new namespace of kinds.
In addition you can enable/disable with the subparser usable
`--languages=[+|-]` option:

.. code-block::console

    $ ./ctags --options=NONE --options=./linux.ctags --languages=-linux -x --_xformat="%20N %10K %10l"  -o - input.c
    ctags: Notice: No options will be read from files or environment
	    set_one_prio   function          C
	 SYSCALL_DEFINE3   function          C

Directions
......................................................................

As explained in :ref:`Tagging definitions of higher(upper) level language (sub/base) <base-sub-parsers>`,
you can choose direction(s) how a base parser and a guest parser work together with
long flags putting after `--langdef=Foo{base=Bar}`.

========================  ======================
C level notation          Command line long flag
========================  ======================
SUBPARSER_BASE_RUNS_SUB   shared
SUBPARSER_SUB_RUNS_BASE   dedicated
SUBPARSER_BASE_RUNS_SUB   bidirectional
========================  ======================

Let's see actual difference of behaviors.


The examples are taken from #1409 submitted by @sgraham on github
Universal-ctags repository.

`input.cc` and `input.mojom` are input files, and have the same
contents::

     ABC();
    int main(void)
    {
    }

C++ parser can capture `main` as a function. Mojom subparser defined in the
later runs on C++ parser and is for capturing `ABC`.

shared combination
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
`{shared}` is specified, for `input.cc`, both tags capture by C++ parser
and mojom parser are recorded to tags file. For `input.mojom`, only
tags captured by mojom parser are recorded to tags file.

mojom-shared.ctags:

.. code-block:: ctags

    --langdef=mojom{base=C++}{shared}
    --map-mojom=+.mojom
    --kinddef-mojom=f,function,functions
    --regex-mojom=/^[ ]+([a-zA-Z]+)\(/\1/f/

tags for `input.cc`::

    ABC	input.cc	/^ ABC();$/;"	f	language:mojom
    main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

tags for `input.mojom`::

  ABC	input.mojom	/^ ABC();$/;"	f	language:mojom

Mojom parser uses C++ parser internally but tags captured by C++ parser are
dropped in the output.

`{shared}` is the default behavior. If none of `{shared}`, `{dedicated}`, nor
`{bidirectional}` is specified, it implies `{shared}`.


dedicated combination
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
`{dedicated}` is specified, for `input.cc`, only tags capture by C++
parser are recorded to tags file. For `input.mojom`, both tags capture
by C++ parser and mojom parser are recorded to tags file.

mojom-dedicated.ctags:

.. code-block:: ctags

    --langdef=mojom{base=C++}{dedicated}
    --map-mojom=+.mojom
    --kinddef-mojom=f,function,functions
    --regex-mojom=/^[ ]+([a-zA-Z]+)\(/\1/f/

tags for `input.cc`::

    main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

tags for `input.mojom`::

    ABC	input.mojom	/^ ABC();$/;"	f	language:mojom
    main	input.mojom	/^int main(void)$/;"	f	language:C++	typeref:typename:int

Mojom parser works only when `.mojom` file is given as input.

bidirectional combination
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
`{bidirectional}` is specified, both tags capture by C++ parser and
mojom parser are recorded to tags file for either input `input.cc` and
`input.mojom`.

mojom-bidirectional.ctags:

.. code-block:: ctags

    --langdef=mojom{base=C++}{bidirectional}
    --map-mojom=+.mojom
    --kinddef-mojom=f,function,functions
    --regex-mojom=/^[ ]+([a-zA-Z]+)\(/\1/f/

tags for `input.cc`::

    ABC	input.cc	/^ ABC();$/;"	f	language:mojom
    main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

tags for `input.mojom`::

    ABC	input.cc	/^ ABC();$/;"	f	language:mojom
    main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

Listing subparsers
......................................................................
Subparsers can be listed with ``--list-subparser``:

.. code-block:: console

    $ ./ctags --options=NONE --options=./linux.ctags --list-subparsers=C
    ctags: Notice: No options will be read from files or environment
    #NAME                          BASEPARSER           DIRECTION
    linux                          C                    base => sub {shared}

Changes to the tags file format
---------------------------------------------------------------------


Truncating the pattern for long input lines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To prevent generating overly large tags files, a pattern field is
truncated, by default, when its size exceeds 96 bytes. A different
limit can be specified with ``--pattern-length-limit=N``.

An input source file with long lines and multiple tag matches per
line can generate an excessively large tags file with an
unconstrained pattern length. For example, running ctags on a
minified JavaScript source file often exhibits this behaviour.

Reference tags
---------------------------------------------------------------------

Traditionally ctags collects the information for locating where a
language object is DEFINED.

In addition Universal-ctags supports reference tags. If the extra-tag
``r`` is enabled, Universal-ctags also collects the information for
locating where a language object is REFERENCED. This feature was
proposed by @shigio in #569 for GNU GLOBAL.

Here are some examples. Here is the target input file named reftag.c.

.. code-block:: c

    #include <stdio.h>
    #include "foo.h"
    #define TYPE point
    struct TYPE { int x, y; };
    TYPE p;
    #undef TYPE


Traditional output:

.. code-block:: console

    $ ./ctags -o - reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^struct TYPE { int x, y; };$/;"	s	file:
    p	reftag.c	/^TYPE p;$/;"	v	typeref:typename:TYPE
    x	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:
    y	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:

Output with the extra-tag ``r`` enabled:

.. code-block:: console

    $ ./ctags --list-extras | grep ^r
    r	Include reference tags	off
    $ ./ctags -o - --extras=+r reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^#undef TYPE$/;"	d	file:
    TYPE	reftag.c	/^struct TYPE { int x, y; };$/;"	s	file:
    foo.h	reftag.c	/^#include "foo.h"/;"	h
    p	reftag.c	/^TYPE p;$/;"	v	typeref:typename:TYPE
    stdio.h	reftag.c	/^#include <stdio.h>/;"	h
    x	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:
    y	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:

`#undef X` and two `#include` are newly collected.

"roles" is a newly introduced field in Universal-ctags. The field
named is for recording how a tag is referenced. If a tag is definition
tag, the roles field has "def" as its value.

Universal-ctags prints the role information when the `r`
field is enabled with ``--fields=+r``.

.. code-block:: console

    $  ./ctags -o - --extras=+r --fields=+r reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^#undef TYPE$/;"	d	file:	roles:undef
    TYPE	reftag.c	/^struct TYPE { int x, y; };$/;"	s	file:	roles:def
    foo.h	reftag.c	/^#include "foo.h"/;"	h	roles:local
    p	reftag.c	/^TYPE p;$/;"	v	typeref:typename:TYPE	roles:def
    stdio.h	reftag.c	/^#include <stdio.h>/;"	h	roles:system
    x	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:	roles:def
    y	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:	roles:def

The `Reference tag marker` field, ``R``, is a specialized GNU global
requirement; D is used for the traditional definition tags, and R is
used for the new reference tags. The field can be used only with
``--_xformat``.

.. code-block:: console

    $ ./ctags -x --_xformat="%R %-16N %4n %-16F %C" --extras=+r reftag.c
    D TYPE                3 reftag.c         #define TYPE point
    D TYPE                4 reftag.c         struct TYPE { int x, y; };
    D p                   5 reftag.c         TYPE p;
    D x                   4 reftag.c         struct TYPE { int x, y; };
    D y                   4 reftag.c         struct TYPE { int x, y; };
    R TYPE                6 reftag.c         #undef TYPE
    R foo.h               2 reftag.c         #include "foo.h"
    R stdio.h             1 reftag.c         #include <stdio.h>

See :ref:`Customizing xref output <xformat>` for more details about
this option.

Although the facility for collecting reference tags is implemented,
only a few parsers currently utilize it. All available roles can be
listed with ``--list-roles``:

.. code-block:: console

    $ ./ctags --list-roles
    #LANGUAGE      KIND(L/N)         NAME                ENABLED DESCRIPTION
    SystemdUnit    u/unit            Requires            on      referred in Requires key
    SystemdUnit    u/unit            Wants               on      referred in Wants key
    SystemdUnit    u/unit            After               on      referred in After key
    SystemdUnit    u/unit            Before              on      referred in Before key
    SystemdUnit    u/unit            RequiredBy          on      referred in RequiredBy key
    SystemdUnit    u/unit            WantedBy            on      referred in WantedBy key
    Yaml           a/anchor          alias               on      alias
    DTD            e/element         attOwner            on      attributes owner
    Automake       c/condition       branched            on      used for branching
    Cobol          S/sourcefile      copied              on      copied in source file
    Maven2         g/groupId         dependency          on      dependency
    DTD            p/parameterEntity elementName         on      element names
    DTD            p/parameterEntity condition           on      conditions
    LdScript       s/symbol          entrypoint          on      entry points
    LdScript       i/inputSection    discarded           on      discarded when linking
    ...

The first column shows the name of the parser.
The second column shows the letter/name of the kind.
The third column shows the name of the role.
The fourth column shows whether the role is enabled or not.
The fifth column shows the description of the role.

You can define a role in an optlib parser for capturing reference
tags. See :ref:`Capturing reference tags <capturing_reftag>` for more
details.

Currently ctags doesn't provide the way for disabling a
specified role.


Automatic parser selection
---------------------------------------------------------------------

See "Choosing a proper parser in ctags" section.


Incompatible changes to file name pattern and extension handling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

When guessing a proper parser for a given input file, Exuberant-ctags
tests file name patterns AFTER file extensions (e-order).
Universal-ctags does this differently; it tests file name patterns
BEFORE file extensions (u-order).

This incompatible change is introduced to deal with the following
situation: "build.xml" is an input file. The Ant parser declares it
handles a file name pattern "build.xml" and another parser, Foo,
declares it handles a file extension "xml".

Which parser should be used for parsing the input? The user may want
to use the Ant parser because the pattern it declares is more
specific than the extension Foo declares. However, in e-order, the
other parser, Foo, is chosen.

So Universal-ctags uses the u-order even though it introduces an
incompatibility.


Pseudo tags
---------------------------------------------------------------------

Pseudo tags are used to add meta data to a tags file. Universal-ctags
will utilize pseudo tags aggressively.

Universal-ctags is not mature yet; there is a possibility that
incompatible changes will be introduced. As a result tools reading
a tags file may not work as expected.

To mitigate this issue pseudo tags are employed to make a tags file
more self-descriptive. We hope some of the incompatibilities can be
overcome in client tools by utilizing this approach.

Example output:

.. code-block:: console

    $ ./ctags -o - --extras=p --pseudo-tags='TAG_KIND_DESCRIPTION' foo.c
    !_TAG_KIND_DESCRIPTION!C	L,label	/goto label/
    !_TAG_KIND_DESCRIPTION!C	c,class	/classes/
    !_TAG_KIND_DESCRIPTION!C	d,macro	/macro definitions/
    !_TAG_KIND_DESCRIPTION!C	e,enumerator	/enumerators (values inside an enumeration)/
    !_TAG_KIND_DESCRIPTION!C	f,function	/function definitions/
    !_TAG_KIND_DESCRIPTION!C	g,enum	/enumeration names/
    !_TAG_KIND_DESCRIPTION!C	h,header	/included header files/
    !_TAG_KIND_DESCRIPTION!C	l,local	/local variables/
    !_TAG_KIND_DESCRIPTION!C	m,member	/class, struct, and union members/
    !_TAG_KIND_DESCRIPTION!C	n,namespace	/namespaces/
    !_TAG_KIND_DESCRIPTION!C	p,prototype	/function prototypes/
    !_TAG_KIND_DESCRIPTION!C	s,struct	/structure names/
    !_TAG_KIND_DESCRIPTION!C	t,typedef	/typedefs/
    !_TAG_KIND_DESCRIPTION!C	u,union	/union names/
    !_TAG_KIND_DESCRIPTION!C	v,variable	/variable definitions/
    !_TAG_KIND_DESCRIPTION!C	x,externvar	/external and forward variable declarations/
    foo	foo.c	/^foo (int i, int j)$/;"	f
    main	foo.c	/^main (void)$/;"	f


``TAG_KIND_DESCRIPTION``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a newly introduced pseudo tag. It is not emitted by default.
It is emitted only when ``--pseudo-tags=+TAG_KIND_DESCRIPTION`` is
given.

This is for describing kinds; their letter, name, and description are
enumerated in the tag.

ctags emits ``TAG_KIND_DESCRIPTION`` with following format::

	!_TAG_KIND_SEPARATOR!{parser}	{letter},{name}	/{description}/

A backslash and a slash in {description} is escaped with a backslash.


``TAG_KIND_SEPARATOR``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a newly introduced pseudo tag. It is not emitted by default.
It is emitted only when ``--pseudo-tags=+TAG_KIND_SEPARATOR`` is
given.

This is for describing separators placed between two kinds in a
language.

Tag entries including the separators are emitted when ``--extras=+q``
is given; fully qualified tags contain the separators. The separators
are used in scope information, too.

ctags emits ``TAG_KIND_SEPARATOR`` with following format::

	!_TAG_KIND_SEPARATOR!{parser}	{sep}	/{upper}{lower}/

or ::

	!_TAG_KIND_SEPARATOR!{parser}	{sep}	/{lower}/

Here {parser} is the name of language. e.g. PHP.
{lower} is the letter representing the kind of the lower item.
{upper} is the letter representing the kind of the upper item.
{sep} is the separator placed between the upper item and the lower
item.

The format without {upper} is for representing a root separator. The
root separator is used as prefix for an item which has no upper scope.

`*` given as {upper} is a fallback wild card; if it is given, the
{sep} is used in combination with any upper item and the item
specified with {lower}.

Each backslash character used in {sep} is escaped with an extra
backslash character.

Example output:

.. code-block:: console

    $ ./ctags -o - --extras=+p --pseudo-tags=  --pseudo-tags=+TAG_KIND_SEPARATOR input.php
    !_TAG_KIND_SEPARATOR!PHP	::	/*c/
    ...
    !_TAG_KIND_SEPARATOR!PHP	\\	/c/
    ...
    !_TAG_KIND_SEPARATOR!PHP	\\	/nc/
    ...

The first line means `::` is used when combining something with an
item of the class kind.

The second line means `\\` is used when a class item is at the top
level; no upper item is specified.

The third line means `\\` is used when for combining a namespace item
(upper) and a class item (lower).

Of course, ctags uses the more specific line when choosing a
separator; the third line has higher priority than the first.

``TAG_OUTPUT_MODE``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

This pseudo tag represents output mode: u-ctags or e-ctags.

See also :ref:`Compatible output and weakness <compat-output>`.

.. _parser-own-fields:

Parser own fields
---------------------------------------------------------------------

A tag has a `name`, an `input` file name, and a `pattern` as basic
information. Some fields like `language:`, `signature:`, etc are
attached to the tag as optional information.

In Exuberant-ctags, fields are common to all languages.
Universal-ctags extends the concept of fields; a parser can define
its own field. This extension was proposed by @pragmaware in #857.

For implementing the parser own fields, the options for listing and
enabling/disabling fields are also extended.

In the output of ``--list-fields``, the owner of the field is printed
in the `LANGUAGE` column:

.. code-block:: console

	$ ./ctags --list-fields
	#LETTER NAME            ENABLED LANGUAGE         XFMT  DESCRIPTION
	...
	-       end             off     C                TRUE   end lines of various constructs
	-       properties      off     C                TRUE   properties (static, inline, mutable,...)
	-       end             off     C++              TRUE   end lines of various constructs
	-       template        off     C++              TRUE   template parameters
	-       captures        off     C++              TRUE   lambda capture list
	-       properties      off     C++              TRUE   properties (static, virtual, inline, mutable,...)
	-       sectionMarker   off     reStructuredText TRUE   character used for declaring section
	-       version         off     Maven2           TRUE   version of artifact

e.g. reStructuredText is the owner of the sectionMarker field and
both C and C++ own the end field.

``--list-fields`` takes one optional argument, `LANGUAGE`. If it is
given, ``--list-fields`` prints only the fields for that parser:

.. code-block:: console

	$ ./ctags --list-fields=Maven2
	#LETTER NAME            ENABLED LANGUAGE        XFMT  DESCRIPTION
	-       version         off     Maven2          TRUE  version of artifact

A parser own field only has a long name, no letter. For
enabling/disabling such fields, the name must be passed to
``--fields-<LANG>``.

e.g. for enabling the `sectionMarker` field owned by the
`reStructuredText` parser, use the following command line:

.. code-block:: console

	$ ./ctags --fields-reStructuredText=+{sectionMarker} ...

The wild card notation can be used for enabling/disabling parser own
fields, too. The following example enables all fields owned by the
`C++` parser.

.. code-block:: console

	$ ./ctags --fields-C++='*' ...

`*` can also be used for specifying languages.

The next example is for enabling `end` fields for all languages which
have such a field.

.. code-block:: console

	$ ./ctags --fields-'*'=+'{end}' ...
	...

In this case, using wild card notation to specify the language, not
only fields owned by parsers but also common fields having the name
specified (`end` in this example) are enabled/disabled.

Using the wild card notation to specify the language is helpful to
avoid incompatibilities between versions of Universal-ctags itself
(SELF INCOMPATIBLY).

In Universal-ctags development, a parser developer may add a new
parser own field for a certain language.  Sometimes other developers
then recognize it is meaningful not only for the original language
but also other languages. In this case the field may be promoted to a
common field. Such a promotion will break the command line
compatibility for ``--fields-<LANG>`` usage. The wild card for
`<LANG>` will help in avoiding this unwanted effect of the promotion.

With respect to the tags file format, nothing is changed when
introducing parser own fields; `<fieldname>`:`<value>` is used as
before and the name of field owner is never prefixed. The `language:`
field of the tag identifies the owner.


Parser own extras
---------------------------------------------------------------------

.. NOT REVIEWED YET

As man page of Exuberant-ctags says, ``--extras`` option specifies
whether to include extra tag entries for certain kinds of information.
This option is available in Universal-ctags, too.

In Universal-ctags it is extended; a parser can define its own
extra flags. They can be controlled with ``--extras-<LANG>=[+|-]{...}``.

See some examples:

.. code-block:: console

	$ ./ctags --list-extras
	#LETTER NAME                   ENABLED LANGUAGE         DESCRIPTION
	F       fileScope              TRUE    NONE             Include tags ...
	f       inputFile              FALSE   NONE             Include an entry ...
	p       pseudo                 FALSE   NONE             Include pseudo tags
	q       qualified              FALSE   NONE             Include an extra ...
	r       reference              FALSE   NONE             Include reference tags
	g       guest                  FALSE   NONE             Include tags ...
	-       whitespaceSwapped      TRUE    Robot            Include tags swapping ...

See the `LANGUAGE` column. NONE means the extra flags are language
independent (common). They can be enabled or disabled with `--extras=` as before.

Look at `whitespaceSwapped`. Its language is `Robot`. This flag is enabled
by default but can be disabled with `--extras-Robot=-{whitespaceSwapped}`.

.. code-block:: console

    $ cat input.robot
    *** Keywords ***
    it's ok to be correct
	Python_keyword_2

    $ ./ctags -o - input.robot
    it's ok to be correct	input.robot	/^it's ok to be correct$/;"	k
    it's_ok_to_be_correct	input.robot	/^it's ok to be correct$/;"	k

    $ ./ctags -o - --extras-Robot=-'{whitespaceSwapped}' input.robot
    it's ok to be correct	input.robot	/^it's ok to be correct$/;"	k

When disabled the name `it's_ok_to_be_correct` is not included in the
tags output.  In other words, the name `it's_ok_to_be_correct` is
derived from the name `it's ok to be correct` when the extra flag is
enabled.

Discussion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

(This subsection should move to somewhere for developers.)

The question is what are extra tag entries. As far as I know none has
answered explicitly. I have two ideas in Universal-ctags. I
write "ideas", not "definitions" here because existing parsers don't
follow the ideas. They are kept as is in variety reasons but the
ideas may be good guide for people who wants to write a new parser
or extend an exiting parser.

The first idea is that a tag entry whose name is appeared in the input
file as is, the entry is NOT an extra. (If you want to control the
inclusion of such entries, the classical ``--kind-<LANG>=[+|-]...`` is
what you want.)

Qualified tags, whose inclusion is controlled by ``--extras=+q``, is
explained well with this idea.
Let's see an example:

.. code-block:: console

    $ cat input.py
    class Foo:
	def func (self):
	    pass

    $ ./ctags -o - --extras=+q --fields=+E input.py
    Foo	input.py	/^class Foo:$/;"	c
    Foo.func	input.py	/^    def func (self):$/;"	m	class:Foo	extra:qualified
    func	input.py	/^    def func (self):$/;"	m	class:Foo

`Foo` and `func` are in `input.py`. So they are no extra tags.  In
other hand, `Foo.func` is not in `input.py` as is. The name is
generated by ctags as a qualified extra tag entry.
`whitespaceSwapped` extra flag of  `Robot` parser is also aligned well
on the idea.

I don't say all parsers follows this idea.

.. code-block:: console

    $ cat input.cc
    class A
    {
      A operator+ (int);
    };

    $ ./ctags --kinds-all='*' --fields= -o - input.cc
    A	input.cc	/^class A$/
    operator +	input.cc	/^  A operator+ (int);$/

In this example `operator+` is in `input.cc`.
In other hand, `operator +`  is in the ctags output as non extra tag entry.
See a whitespace between the keyword `operator` and `+` operator.
This is an exception of the first idea.

The second idea is that if the *inclusion* of a tag cannot be
controlled well with ``--kind-<LANG>=[+|-]...``, the tag may be an
extra.

.. code-block:: console

    $ cat input.c
    static int foo (void)
    {
	    return 0;
    }
    int bar (void)
    {
	    return 1;
    }

    $ ./ctags --sort=no -o - --extras=+F input.c
    foo	input.c	/^static int foo (void)$/;"	f	typeref:typename:int	file:
    bar	input.c	/^int bar (void)$/;"	f	typeref:typename:int

    $ ./ctags -o - --extras=-F input.c
    foo	input.c	/^static int foo (void)$/;"	f	typeref:typename:int	file:

    $

Function `foo` of C language is included only when `F` extra flag
is enabled. Both `foo` and `bar` are functions. Their inclusions
can be controlled with `f` kind of C language: ``--kind-C=[+|-]f``.

The difference between static modifier or implicit extern modifier in
a function definition is handled by `F` extra flag.

Basically the concept kind is for handling the kinds of language
objects: functions, variables, macros, types, etc. The concept extra
can handle the other aspects like scope (static or extern).

However, a parser developer can take another approach instead of
introducing parser own extra; one can prepare `staticFunction` and
`exportedFunction` as kinds of one's parser.  The second idea is a
just guide; the parser developer must decide suitable approach for the
target language.

Anyway, in the second idea, ``--extra`` is for controlling inclusion
of tags. If what you want is not about inclusion, ``--param-<LANG>``
can be used as the last resort.


Parser own parameter
---------------------------------------------------------------------

.. NOT REVIEWED YET

To control the detail of a parser, ``--param-<LANG>`` option is introduced.
``--kinds-<LANG>``, ``--fields-<LANG>``, ``--extras-<LANG>``
can be used for customizing the behavior of a parser specified with ``<LANG>``.

``--param-<LANG>`` should be used for aspects of the parser that
the options(kinds, fields, extras) cannot handle well.

A parser defines a set of parameters. Each parameter has name and
takes an argument. A user can set a parameter with following notation
::

   --param-<LANG>:name=arg

An example of specifying a parameter
::

   --param-CPreProcessor:if0=true

Here `if0` is a name of parameter of CPreProcessor parser and
`true` is the value of it.

All available parameters can be listed with ``--list-params`` option.

.. code-block:: console

    $ ./ctags --list-params
    #PARSER         NAME     DESCRIPTION
    CPreProcessor   if0      examine code within "#if 0" branch (true or [false])
    CPreProcessor   ignore   a token to be specially handled

(At this time only CPreProcessor parser has parameters.)


.. _xformat:

Customizing xref output
---------------------------------------------------------------------

``--_xformat`` option allows a user to customize the cross reference
(xref) output enabled with ``-x``.
::

   --_xformat=FORMAT


The notation for FORMAT is similar to that employed by `printf(3)` in
the C language; `%` represents a slot which is substituted with a
field value when printing. You can specify multiple slots in FORMAT.
Here field means an item listed with ``--list-fields`` option.

The notation of a slot::

   %[-][.][WIDTH-AND-ADJUSTMENT]FIELD-SPECIFIER

``FIELD-SPECIFIER`` specifies a field whose value is printed.
Short notation and long notation are available. They can be mixed
in a FORMAT. Specifying a field with either notation, one or more
fields are activated internally.

The short notation is just a letter listed in the LETTER column of
the ``--list-fields`` output.

The long notation is a name string surrounded by braces(`{` and
`}`). The name string is listed in the NAME column of the output of
the same option. To specify a field owned by a parser, prepend
the parser name to the name string with `.` as a separator.

Wild card (`*`) can be used where a parser name is specified. In this
case both common and parser own fields are activated and printed.
If a common field and a parser own field have the same name,
the common field has higher priority.

`WIDTH-AND-ADJUSTMENT` is a positive number.
The value of the number is used as the width of
the column where a field is printed. The printing is
right adjusted by default, and left
adjusted when `-` is given as prefix.
The output is not truncated by default even if its field width is
specified and smaller than width of output value. For truncating
the output to the specified width, use `.` as prefix.

An example of specifying common fields:

.. code-block:: console

    $  ./ctags -x --_xformat="%-20N %4n %-16{input}|" main/main.c | head
    CLOCKS_PER_SEC        360 main/main.c     |
    CLOCKS_PER_SEC        364 main/main.c     |
    CLOCK_AVAILABLE       358 main/main.c     |
    CLOCK_AVAILABLE       363 main/main.c     |
    Totals                 87 main/main.c     |
    __anonae81ef0f0108     87 main/main.c     |
    addTotals             100 main/main.c     |
    batchMakeTags         436 main/main.c     |
    bytes                  87 main/main.c     |
    clock                 365 main/main.c     |

Here `%-20N %4n %-16{input}|` is a format string. Let's look at the
elements of the format.

`%-20N`

	The short notation is used here.
	The element means filling the slot with the name of the tag.
	The width of the column is 20 characters and left adjusted.

`%4n`

	The short notation is used here.
	The element means filling the slot with the line number of
	the tag. The width of the column is 4 characters and right
        adjusted.

`%-16{input}`

	The long notation is used here.
	The element means filling the slot with the input file name
	where the tag is defined. The width of column is 16
        characters and left adjusted.

`|`

	Printed as is.

Another example of specifying parser own fields:

.. code-block:: console

	$  ./ctags -x --_xformat="%-20N [%10{C.properties}]" main/main.c
	CLOCKS_PER_SEC       [          ]
	CLOCK_AVAILABLE      [          ]
	Totals               [          ]
	__anonae81ef0f0108   [          ]
	addTotals            [    extern]
	batchMakeTags        [    static]
	bytes                [          ]
	clock                [          ]
	clock                [    static]
	...

Here `"%-20N [%10{C.properties}]"` is a format string. Let's look at
the elements of the format.

`%-20N`

	Already explained in the first example.

`[` and `]`

	Printed as is.

`%10{C.properties}`

	The long notation is used here.
	The element means filling the slot with the value
	of the properties field of the C parser.
	The width of the column is 10 characters and right adjusted.


.. TODO: An example of using WILDCARD


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


Skipping utf-8 BOM
---------------------------------------------------------------------

The three bytes sequence('\xEF\xBB\xBF') at the head of an input
file is skipped when parsing.

TODO:

* Do the same in guessing and selecting parser stage.
* Refect the BOM detection to encoding option


Readtags
---------------------------------------------------------------------

Printing line numbers with ``-n``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If both ``-e`` and ``-n`` are given, readtags prints the `line:`
field.


Filtering in readtags command
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
readtags has ability to find tag entries by name.

The concept of filtering is inspired by the display filter of
Wireshark. You can specify more complex conditions for searching.
Currently this feature is available only on platforms where
`fmemopen` is available as part of libc. Filtering in readtags is an
experimental feature.

The syntax of filtering rules is based on the Scheme language, a
variant of Lisp. The language has prefix notation and parentheses.

Before printing an entry from the tags file, readtags evaluates an
expression (S expression or sexp) given as an option argument to
``-Q``. As the result of the evaluation, readtags gets a value. false
represented as `#f`, indicates rejection: readtags doesn't print it.

::

   SEXP =
	LIST
	INTEGER
	BOOLEAN
	STRING
	SYMBOL

	LIST = ( SEXP... ) | ()
	INTEGER = [0-9]+
	BOOLEAN = #t | #f
	STRING  = "..."
	SYMBOL  = null?
		    and
		     or
		    not
		    eq?
		      <
		      >
		     <=
		     >=
		prefix?
		suffix?
		substr?
		 member
		      $
		  $name
		 $input
		$access
		  $file
	      $language
	$implementation
		  $line
		  $kind
		  $role
	       $pattern
	      $inherits
	    $scope-kind
	    $scope-name
		   $end

All symbols starting with `$` represent a field of a tag entry which
is being tested against the S expression. Most will evaluate as a
string or `#f`. It evaluates to `#f` when the field doesn't exist.
`$inherits` is evaluated to a list of strings if the entry has an
`inherits` field. The `scope` field holds structured data: the kind
and name of the upper scope combined with `:`. The kind part is
mapped to `$scope-kind`, and the name part to `$scope-name`.

`$scope-kind` and `$scope-name` can only be used if the input tags
file is generated by ctags with ``--fields=+Z``.

All symbols not prefixed with `$` are operators. When using these,
put them at the head(car) of list. The rest(cdr) of the list is
passed to the operator as arguments. Many of them are also available
in the Scheme language; see the other documents.

prefix?, suffix?, and substr? may only be available in this
implementation. All of them take two strings. The first one
is called the target.

The exception in the above naming convention is the `$` operator.
`$` is a generic accessor for accessing extension fields.
`$` takes one argument: the name of an extension field.
It returns the value of the field as a string if a value
is given, or `#f`.

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


Let's see examples.

Examples of input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Create the tags file (*foo.tags*) with following command line

.. code-block:: console

	$ ./ctags --fields='*' --extras='*' -o foo.tags foo.py

for following input (*foo.py*)

.. code-block:: python

    class Foo:
	def aq ():
	    pass
	def aw ():
	    pass
	def ae ():
	    pass
	class A:
	    pass
    class Bar (Foo):
	def bq ():
	    pass
	def bw ():
	    pass
	class B:
	    pass

    class Baz (Foo):
	def bq ():
	    pass
	def bw ():
	    pass
	class C:
	    pass

Examples of filter expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Print entries ending with "q"

  .. code-block:: console

	$ ./readtags -e -t foo.tags -Q '(suffix? $name "q")' -l
	Bar.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Bar	access:public	signature:()
	Baz.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	Foo.aq	foo.py	/^    def aq ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()
	aq	foo.py	/^    def aq ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()
	bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Bar	access:public	signature:()
	bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()

* Print members of Baz

  .. code-block:: console

	$ ./readtags -e -t foo.tags -Q '(and (eq? $kind "member") (eq? "Baz" $scope-name))' -l
	Baz.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	Baz.bw	foo.py	/^    def bw ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	bw	foo.py	/^    def bw ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()

* Print only fully qualified entries (assuming "." is used as the separator)

  .. code-block:: console

	$ ./readtags -e -t foo.tags -Q '(and (eq? $kind "member") (substr? $name "."))' -l
	Bar.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Bar	access:public	signature:()
	Bar.bw	foo.py	/^    def bw ():$/;"	kind:member	language:Python	scope:class:Bar	access:public	signature:()
	Baz.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	Baz.bw	foo.py	/^    def bw ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	Foo.ae	foo.py	/^    def ae ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()
	Foo.aq	foo.py	/^    def aq ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()
	Foo.aw	foo.py	/^    def aw ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()

* Print only classes inheriting Foo

  .. code-block:: console

	$ ./readtags  -e -t foo.tags -Q '(and (member "Foo" $inherits) (eq? $kind "class"))' -l
	Bar	foo.py	/^class Bar (Foo):$/;"	kind:class	language:Python	inherits:Foo	access:public
	Baz	foo.py	/^class Baz (Foo): $/;"	kind:class	language:Python	inherits:Foo	access:public
