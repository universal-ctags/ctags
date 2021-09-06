.. _changes_tags_file:

Changes to the tags file format
---------------------------------------------------------------------

``F`` kind usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You cannot use ``F`` (``file``) kind in your .ctags because Universal Ctags
reserves it. See :ref:`ctags-incompatibilities(7) <ctags-incompatibilities(7)>`.

Reference tags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Traditionally ctags collects the information for locating where a
language object is DEFINED.

In addition Universal Ctags supports reference tags. If the extra-tag
``r`` is enabled, Universal Ctags also collects the information for
locating where a language object is REFERENCED. This feature was
proposed by @shigio in `#569
<https://github.com/universal-ctags/ctags/issues/569>`_ for GNU GLOBAL.

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

    $ ctags -o - reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^struct TYPE { int x, y; };$/;"	s	file:
    p	reftag.c	/^TYPE p;$/;"	v	typeref:typename:TYPE
    x	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:
    y	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:

Output with the extra-tag ``r`` enabled:

.. code-block:: console

    $ ctags --list-extras | grep ^r
    r	Include reference tags	off
    $ ctags -o - --extras=+r reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^#undef TYPE$/;"	d	file:
    TYPE	reftag.c	/^struct TYPE { int x, y; };$/;"	s	file:
    foo.h	reftag.c	/^#include "foo.h"/;"	h
    p	reftag.c	/^TYPE p;$/;"	v	typeref:typename:TYPE
    stdio.h	reftag.c	/^#include <stdio.h>/;"	h
    x	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:
    y	reftag.c	/^struct TYPE { int x, y; };$/;"	m	struct:TYPE	typeref:typename:int	file:

`#undef X` and two `#include` are newly collected.

"roles" is a newly introduced field in Universal Ctags. The field
named is for recording how a tag is referenced. If a tag is definition
tag, the roles field has "def" as its value.

Universal Ctags prints the role information when the `r`
field is enabled with ``--fields=+r``.

.. code-block:: console

    $ ctags -o - --extras=+r --fields=+r reftag.c
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

    $ ctags -x --_xformat="%R %-16N %4n %-16F %C" --extras=+r reftag.c
    D TYPE                3 reftag.c         #define TYPE point
    D TYPE                4 reftag.c         struct TYPE { int x, y; };
    D p                   5 reftag.c         TYPE p;
    D x                   4 reftag.c         struct TYPE { int x, y; };
    D y                   4 reftag.c         struct TYPE { int x, y; };
    R TYPE                6 reftag.c         #undef TYPE
    R foo.h               2 reftag.c         #include "foo.h"
    R stdio.h             1 reftag.c         #include <stdio.h>

See :ref:`Customizing xref output <xformat>` for more details about
``--_xformat``.

Although the facility for collecting reference tags is implemented,
only a few parsers currently utilize it. All available roles can be
listed with ``--list-roles``:

.. code-block:: console

    $ ctags --list-roles
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

.. NOTE: --xformat is the only way to extract referenced tag

The first column shows the name of the parser.
The second column shows the letter/name of the kind.
The third column shows the name of the role.
The fourth column shows whether the role is enabled or not.
The fifth column shows the description of the role.

You can define a role in an optlib parser for capturing reference
tags. See :ref:`Capturing reference tags <roles>` for more
details.

``--roles-<LANG>.<KIND>`` is the option for enabling/disabling
specified roles.

Pseudo-tags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

See :ref:`ctags-client-tools(7) <ctags-client-tools(7)>` about the
concept of the pseudo-tags.

.. TODO move the following contents to ctags-client-tools(7).

``TAG_KIND_DESCRIPTION``
.........................................................................

This is a newly introduced pseudo-tag. It is not emitted by default.
It is emitted only when ``--pseudo-tags=+TAG_KIND_DESCRIPTION`` is
given.

This is for describing kinds; their letter, name, and description are
enumerated in the tag.

ctags emits ``TAG_KIND_DESCRIPTION`` with following format::

	!_TAG_KIND_SEPARATOR!{parser}	{letter},{name}	/{description}/

A backslash and a slash in {description} is escaped with a backslash.


``TAG_KIND_SEPARATOR``
.........................................................................

This is a newly introduced pseudo-tag. It is not emitted by default.
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

    $ ctags -o - --extras=+p --pseudo-tags=  --pseudo-tags=+TAG_KIND_SEPARATOR input.php
    !_TAG_KIND_SEPARATOR!PHP	::	/*c/
    ...
    !_TAG_KIND_SEPARATOR!PHP	\\	/c/
    ...
    !_TAG_KIND_SEPARATOR!PHP	\\	/nc/
    ...

The first line means ``::`` is used when combining something with an
item of the class kind.

The second line means ``\\`` is used when a class item is at the top
level; no upper item is specified.

The third line means ``\\`` is used when for combining a namespace item
(upper) and a class item (lower).

Of course, ctags uses the more specific line when choosing a
separator; the third line has higher priority than the first.

``TAG_OUTPUT_FILESEP``
.........................................................................

This pseudo-tag represents the separator used in file name: slash or
backslash.  This is always 'slash' on Unix-like environments.
This is also 'slash' by default on Windows, however when
``--output-format=e-tags`` or ``--use-slash-as-filename-separator=no``
is specified, it becomes 'backslash'.


``TAG_OUTPUT_MODE``
.........................................................................

.. NOT REVIEWED YET

This pseudo-tag represents output mode: u-ctags or e-ctags.
This is controlled by ``--output-format`` option.

See also :ref:`Compatible output and weakness <compat-output>`.

Truncating the pattern for long input lines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See ``--pattern-length-limit=N`` option in :ref:`ctags(1) <ctags(1)>`.

.. _parser-specific-fields:

Parser specific fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A tag has a `name`, an `input` file name, and a `pattern` as basic
information. Some fields like `language:`, `signature:`, etc are
attached to the tag as optional information.

In Exuberant Ctags, fields are common to all languages.
Universal Ctags extends the concept of fields; a parser can define
its specific field. This extension was proposed by @pragmaware in
`#857 <https://github.com/universal-ctags/ctags/issues/857>`_.

For implementing the parser specific fields, the options for listing and
enabling/disabling fields are also extended.

In the output of ``--list-fields``, the owner of the field is printed
in the `LANGUAGE` column:

.. code-block:: console

	$ ctags --list-fields
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

	$ ctags --list-fields=Maven2
	#LETTER NAME            ENABLED LANGUAGE        XFMT  DESCRIPTION
	-       version         off     Maven2          TRUE  version of artifact

A parser specific field only has a long name, no letter. For
enabling/disabling such fields, the name must be passed to
``--fields-<LANG>``.

e.g. for enabling the `sectionMarker` field owned by the
`reStructuredText` parser, use the following command line:

.. code-block:: console

	$ ctags --fields-reStructuredText=+{sectionMarker} ...

The wild card notation can be used for enabling/disabling parser specific
fields, too. The following example enables all fields owned by the
`C++` parser.

.. code-block:: console

	$ ctags --fields-C++='*' ...

`*` can also be used for specifying languages.

The next example is for enabling `end` fields for all languages which
have such a field.

.. code-block:: console

	$ ctags --fields-'*'=+'{end}' ...
	...

In this case, using wild card notation to specify the language, not
only fields owned by parsers but also common fields having the name
specified (`end` in this example) are enabled/disabled.

Using the wild card notation to specify the language is helpful to
avoid incompatibilities between versions of Universal Ctags itself
(SELF INCOMPATIBLY).

In Universal Ctags development, a parser developer may add a new
parser specific field for a certain language.  Sometimes other developers
then recognize it is meaningful not only for the original language
but also other languages. In this case the field may be promoted to a
common field. Such a promotion will break the command line
compatibility for ``--fields-<LANG>`` usage. The wild card for
`<LANG>` will help in avoiding this unwanted effect of the promotion.

With respect to the tags file format, nothing is changed when
introducing parser specific fields; `<fieldname>`:`<value>` is used as
before and the name of field owner is never prefixed. The `language:`
field of the tag identifies the owner.


Parser specific extras
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

As man page of Exuberant Ctags says, ``--extras`` option specifies
whether to include extra tag entries for certain kinds of information.
This option is available in Universal Ctags, too.

In Universal Ctags it is extended; a parser can define its specific
extra flags. They can be controlled with ``--extras-<LANG>=[+|-]{...}``.

See some examples:

.. code-block:: console

	$ ctags --list-extras
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

    $ ctags -o - input.robot
    it's ok to be correct	input.robot	/^it's ok to be correct$/;"	k
    it's_ok_to_be_correct	input.robot	/^it's ok to be correct$/;"	k

    $ ctags -o - --extras-Robot=-'{whitespaceSwapped}' input.robot
    it's ok to be correct	input.robot	/^it's ok to be correct$/;"	k

When disabled the name `it's_ok_to_be_correct` is not included in the
tags output.  In other words, the name `it's_ok_to_be_correct` is
derived from the name `it's ok to be correct` when the extra flag is
enabled.

Discussion
.........................................................................

.. NOT REVIEWED YET

(This subsection should move to somewhere for developers.)

The question is what are extra tag entries. As far as I know none has
answered explicitly. I have two ideas in Universal Ctags. I
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

    $ ctags -o - --extras=+q --fields=+E input.py
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

    $ ctags --kinds-all='*' --fields= -o - input.cc
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

    $ ctags --sort=no -o - --extras=+F input.c
    foo	input.c	/^static int foo (void)$/;"	f	typeref:typename:int	file:
    bar	input.c	/^int bar (void)$/;"	f	typeref:typename:int

    $ ctags -o - --extras=-F input.c
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
introducing parser specific extra; one can prepare `staticFunction` and
`exportedFunction` as kinds of one's parser.  The second idea is a
just guide; the parser developer must decide suitable approach for the
target language.

Anyway, in the second idea, ``--extras`` is for controlling inclusion
of tags. If what you want is not about inclusion, ``--param-<LANG>``
can be used as the last resort.


Parser specific parameter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

To control the detail of a parser, ``--param-<LANG>`` option is introduced.
``--kinds-<LANG>``, ``--fields-<LANG>``, ``--extras-<LANG>``
can be used for customizing the behavior of a parser specified with ``<LANG>``.

``--param-<LANG>`` should be used for aspects of the parser that
the options(kinds, fields, extras) cannot handle well.

A parser defines a set of parameters. Each parameter has name and
takes an argument. A user can set a parameter with following notation
::

   --param-<LANG>.name=arg

An example of specifying a parameter
::

   --param-CPreProcessor.if0=true

Here `if0` is a name of parameter of CPreProcessor parser and
`true` is the value of it.

All available parameters can be listed with ``--list-params`` option.

.. code-block:: console

    $ ctags --list-params
    #PARSER         NAME     DESCRIPTION
    CPreProcessor   if0      examine code within "#if 0" branch (true or [false])
    CPreProcessor   ignore   a token to be specially handled

(At this time only CPreProcessor parser has parameters.)
