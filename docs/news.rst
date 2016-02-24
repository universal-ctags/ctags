Introduced changes
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

Many changes have been introduced in universal-ctags. Use git-log to
review changes not enumerated here, especially in language parsers.

Importing most of the changes from exuberant-ctags
---------------------------------------------------------------------
See "exuberant-ctags" in "Tracking other projects" about the status of
importing. Some changes in Fedora and Debian are also imported.

Parsers related changes
---------------------------------------------------------------------

New parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following parsers have been added:

* Ada
* Clojure
* CSS
* D
* DBusIntrospect *libxml*
* Diff
* DTS
* Falcon
* Glade *libxml*
* Go
* JSON
* Maven2 *libxml*
* ObjectiveC
* Perl6
* R
* reStructuredText
* Rust
* SystemVerilog
* WindRes
* Zephir
* coffee *xcmd*
* ctags option library *optlib*
* m4 *optlib*

See "Option library" about  *optlib*.
See "External parser command" about *xcmd*.
Libxml2 is needed to use the parser(s) marked with *libxml*.

TIPS: you can list newly introduced parsers if you have
exuberant-ctags with following command line:

.. code-block:: console

		$ diff -ruN <(universal-ctags --list-languages) <(exuberant-ctags --list-languages)  | grep '^[-+]'


Heavily improved language parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* ant *libxml*
* php
* verilog

New options
---------------------------------------------------------------------

Wildcard in options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the purpose gathering as much as possible information from source
code "wildcard"(``*``) in option is introduced.

``--extra=*``

	Enables all extra tags.

``--fields=*``

	Enables all available fields.

``--<LANG>-kinds=*``

	Enables all available kinds for ``LANG``.

``--kinds-<LANG>=*``

	Alternative representation of ``--<LANG>-kinds=*``.

``--*-kinds=SPEC``

	Applies SPEC as kinds to all available language parsers.

``--*-kinds=*``

	Enables all available kinds to all available language parsers.

Notice message and ``--quiet`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There were 3 classes of message in ctags:

*fatal*

	A critical error is occurred. ctags aborts the execution.

*warning*

	An error is occurred but ctags continues the execution.

*verbose*

	Mainly for debugging purpose.


*notice* is a new class of message. It is less important than warning*
*but more important for users than *verbose*. Generally the user can
*ignore *notice*. With ``--quiet`` option can be used to turn off the
printing the *notice* class messages.

``--input-endocing=ENCODING`` and ``--output-endocing=ENCODING`` options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Japanese programmers sometimes use Japanese language in comments in
source code. Of course it is not limited to Japanese. People may use
their own native language in some case. In such case encoding becomes
an issue.

ctags didn't care it. ctags just reads input as just bytes sequence and
use them as is when writing tags entries.

In other hand vim cares it. When loading a file, vim converts the file
content into an internal format with one of encodings specified in
fileencodings variable.

As the result of this difference, vim cannot move the cursor to the
definition of a tag as users expect with pattern matching. ctags
writes patterns in tags file.

Good news is that there is a way to notify vim the encoding used in a
tags file with ``TAG_FILE_ENCODING`` pseudo tag in the tag file.

This feature solves this issue utilizing ``TAG_FILE_ENCODING``
pseudo tag.

This patch introduces two type of options (``--input-encoding=IN``
and ``--output-encoding=OUT``).

As specified encoding with these options ctags converts input from
``IN`` encoding to ``OUT`` encoding. ctags uses the converted strings
when writing pattern parts of tags lines. As the result tags output is
encoded in ``OUT`` encoding.  In addition ``OUT`` is specified in the
top tags file as value for ``TAG_FILE_ENCODING`` pseudo tag.  As
``OUT`` utf-8 is as default.

NOTE: Converted input is NOT passed to language parsers.
The parsers still deal with input as bytes sequence.

With ``--input-encoding-<LANG>=IN``, you can specify ``LANG`` own
input encoding. It overrides the global default value given with
``--input-encoding``.

The example usage can be found in *Tmain/{input,output}-encoding-option.d*.

Acceptable ``IN`` and ``OUT`` can be listed with *iconv -l* or *iconv --list*.
It is up to platform where ctags runs.

To enable the option, libiconv is needed in your platform. In addition
``--enable-iconv`` must be specified to configure before making ctags.
On windows mingw32, you must specify ``WITH_ICONV=yes`` like below::

	C:\dev\ctags>mingw32-make -f mk_mingw.mak WITH_ICONV=yes

Extra tag entries (``--extra``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Following extra tag entries are newly introduced.

``F``

	Equivalent to --file-scope.

``.``

	Do the similar to the ``f`` extra flag but the entry addresses the end line.

``p``

	Include pseudo tags.


``--list-...`` options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``--list-extensions``,  ``--list-extras``, ``--list-features``,
``--list-fields``, ``--list-patterns``, and ``--list-pseudo-tags`` are added.


``--put-field-prefix`` options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some fields are newly introduced in universal-ctags. We will introduce more
in the future. Other tags generators may also introduce for their own fields.

In such situation there is concern about confliction of field names;
mixing tags files generated from multiple tags generator including
universal-ctags is difficult. ``--put-field-prefix`` provides a
workaround for the use case. When ``--put-field-prefix`` is given,
ctags puts "UCTAGS" as prefix for newly introduced field.

.. code-block:: console

    $ cat /tmp/foo.h
    #include <stdio.h>
    $ ./ctags -o - --extra=+r --fields=+r /tmp/foo.h
    stdio.h	/tmp/foo.h	/^#include <stdio.h>/;"	h	role:system
    $ ./ctags --put-field-prefix -o - --extra=+r --fields=+r /tmp/foo.h
    stdio.h	/tmp/foo.h	/^#include <stdio.h>/;"	h	UCTAGSrole:system

In this example, ``role`` is prefixed.

``--maxdepth`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``--maxdepth`` limits the depth of directory recursion enabled with ``-R``
option.


Guessing parser from file contents (``-G`` option)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See "Choosing a proper parser in ctags" section.


Enabling/disabling pseudo tags (``--pseudo-tags`` option)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each pseudo tag can be endabled/disabled with ``--pseudo-tags`` option.
::

	--pseudo-tags=+ptag
	--pseudo-tags=-ptag

With prefixed with `+`, the pseudo tag specified as ``ptag`` is enabled.
With prefixed with `-`, the pseudo tag specified as ``ptag`` is disabled.
``--list-pseudo-tags`` option shows all specifiable ptag names.

All pseudo tags are enabled if `*` is given as the name of ptag like::

	--pseudo-tags=*

All pseudo tags are disabled if no option value is given to
``--pseudo-tags` option like::

	--pseudo-tags=



Changes in tags file format
---------------------------------------------------------------------


Omitting the pattern for too long input line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Not to make too large tags file, a pattern filed of tags file is
omitted when its size goes beyond 96 bytes.

An input source file with single long line causes too large tags file.
Such input files are popular in javascript: tools for size optimizing
generate them.

Reference tags
---------------------------------------------------------------------

Traditionally ctags collects the information for locating where an
object having name is DEFINED.

In addition Universal-ctags supports reference tags. If ``r`` extra
tag is enabled, universal-ctags collects the information for locating
where an object having name is REFERENCED. This feature is proposed
by @shigio on #569 for GNU GLOBAL.

Let me show some examples. Here is the target input file named reftag.c.

.. code-block:: c

    #include <stdio.h>
    #include "foo.h"
    #define TYPE point
    struct TYPE { int x, y };
    TYPE p;
    #undef TYPE


Traditionally output:

.. code-block:: console

    $ ./ctags -o - reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^struct TYPE { int x, y };$/;"	s	file:
    p	reftag.c	/^TYPE p;$/;"	v
    x	reftag.c	/^struct TYPE { int x, y };$/;"	m	struct:TYPE	file:

Output with enabling ``r`` extra tag:

.. code-block:: console

    $ ./ctags --list-extras | grep ^r
    r	Include reference tags	off
    $ ./ctags -o - --extra=+r reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^#undef TYPE$/;"	d	file:
    TYPE	reftag.c	/^struct TYPE { int x, y };$/;"	s	file:
    foo.h	reftag.c	/^#include "foo.h"/;"	h
    p	reftag.c	/^TYPE p;$/;"	v
    stdio.h	reftag.c	/^#include <stdio.h>/;"	h
    x	reftag.c	/^struct TYPE { int x, y };$/;"	m	struct:TYPE	file:

`#undef X` and two `#include` are newly collected. Reference tags may
have "role" information representing how it is
referenced. Universal-ctags print the role information when `r` field
is enabled with ``--fields=+r``. (If a tag doesn't have no specialized
role, `generic` is used as the name of role.)

.. code-block:: console

    $  ./ctags -o - --extra=+r --fields=+r reftag.c
    TYPE	reftag.c	/^#define TYPE /;"	d	file:
    TYPE	reftag.c	/^#undef TYPE$/;"	d	file:	role:undef
    TYPE	reftag.c	/^struct TYPE { int x, y };$/;"	s	file:
    foo.h	reftag.c	/^#include "foo.h"/;"	h	role:local
    p	reftag.c	/^TYPE p;$/;"	v
    stdio.h	reftag.c	/^#include <stdio.h>/;"	h	role:system
    x	reftag.c	/^struct TYPE { int x, y };$/;"	m	struct:TYPE	file:

`Reference tag marker` field is specialized to GNU global requirement; D is used
for the traditional definition tags, and R is used for the new reference tags.
The field can be used only in ``--_xformat`` option.

.. code-block:: console

    $ ./ctags -x --_xformat="%R %-16N %4n %-16F %C" --extra=+r reftag.c
    D TYPE                3 reftag.c         #define TYPE point
    D TYPE                4 reftag.c         struct TYPE { int x, y };
    D p                   5 reftag.c         TYPE p;
    D x                   4 reftag.c         struct TYPE { int x, y };
    R TYPE                6 reftag.c         #undef TYPE
    R foo.h               2 reftag.c         #include "foo.h"
    R stdio.h             1 reftag.c         #include <stdio.h>

Though the facility for collecting reference tags is implemented, only
few parsers utilized it now. All available roles can be listed with
``--list-roles`` option:

.. code-block:: console

    $ ./ctags --_list-roles
    C	d	undef	undefined	on
    C	h	system	system header	on
    C	h	local	local header	on
    C++	d	undef	undefined	on
    C++	h	system	system header	on
    C++	h	local	local header	on
    DTS	d	undef	undefined	on
    DTS	h	system	system header	on
    DTS	h	local	local header	on
    Make	I	generic	non-categorized generic role	on
    Make	I	optional	included as an optional makefile	on
    Sh	s	generic	non-categorized generic role	on
    Vera	d	undef	undefined	on
    Vera	h	system	system header	on
    Vera	h	local	local header	on

The first column shows a name of parser.
The second column shows a name of kind.
The third column shows a name of role.
The fourth column shows description of the role.
The first column shows whether the role is enabled or not.
Currently ctags doesn't provide the way for disabling a
specified role.


Automatic parser selection
---------------------------------------------------------------------

See "Choosing a proper parser in ctags" section.


Incompatible change about file name patterns and file extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When guessing a proper parser for a given input file, exuberant-ctags
tests file name patterns AFTER file
extensions(e-order). universal-ctags does different; it tests file
name patterns BEFORE file extensions(u-order).

This incompatible change is introduced to deal following situation:
"build.xml" is an input file. Ant parser declares it handles
a file name pattern "build.xml". Foo, another parser declares it handles a
file extension "xml".

Which parser does a user want to use for parsing the input?  The user
may want to use Ant parser because the pattern it declares is more
specific than the extension Foo declares.

However, in e-order, the other parser is chosen. So universal-ctags
uses the u-order though it introduces incompatibility.


Pseudo tags
---------------------------------------------------------------------

pseudo tags are meta data of tags file. Universal-ctags will utilize
pseudo tags aggressively.

Universal-ctags is not mature yet; there is possibility that
incompatible changes are introduced. As the result tools reading tags
will not work as expected.

To avoid such cases, we try making tags file more self-descriptive.
The pseudo tags are used for the self description.  We hope some of
incompatibilities can be overcome in upper layer tools with the pseudo
tags.


``TAG_KIND_SEPARATOR``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a newly introduced pseudo tag. It is not emitted by default.
It is emitted only when ``--pseudo-tags=+TAG_KIND_SEPARATOR`` option
is given.

This is for describing separators placed between two kinds in a language.

Tag entries including the separators are emitted when ``--extra=+q``
is given; full qualified tags contain the separators. The separators
are used in scope information, too.

ctags emits ``TAG_KIND_SEPARATOR`` with following format::

	!_TAG_KIND_SEPARATOR!{parser}	{sep}	/{upper}{lower}/

Here {parser} is the name of language. e.g. PHP.
{lower} is the letter representing kind of lower item.
{upper} is the letter representing kind of upper item.
{sep} is the separator placed between the upper item and
the lower item.

`*` given as {upper} is a fallback wild card; if it is given, the
{sep} is used in combination of any upper item and the item specified
with {lower}.

Each backslash characters used in ${sep} is escaped with
an extra backslash character.

Example output:

.. code-block:: console

    $ ./ctags -o - --extra=+p --pseudo-tags=  --pseudo-tags=+TAG_KIND_SEPARATOR input.php
    !_TAG_KIND_SEPARATOR!PHP	::	/*c/
    !_TAG_KIND_SEPARATOR!PHP	::	/*d/
    !_TAG_KIND_SEPARATOR!PHP	::	/*f/
    !_TAG_KIND_SEPARATOR!PHP	::	/*i/
    !_TAG_KIND_SEPARATOR!PHP	::	/*l/
    !_TAG_KIND_SEPARATOR!PHP	::	/*n/
    !_TAG_KIND_SEPARATOR!PHP	::	/*t/
    !_TAG_KIND_SEPARATOR!PHP	::	/*v/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nc/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nd/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nf/
    !_TAG_KIND_SEPARATOR!PHP	\\	/ni/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nl/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nn/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nt/
    !_TAG_KIND_SEPARATOR!PHP	\\	/nv/


Readtags
---------------------------------------------------------------------

Printing line number with ``-n`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If both ``-e`` and ``-n`` options are given, readtags prints `line:`
field.


Filtering in readtags command
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
readtags has ability to find tag entries by name.

The concept filtering is inspired from display filter of wireshark.
You can give more complex condition for searching. Currently this
feature is available only on platforms where `fmemopen` is available
as part of libc. Filtering in readtags command is an
experimental feature.

The syntax of filtering rule is based on scheme language, a variant
of lisp. The language has prefix notation and parenthesis.

Before printing an entry of tags file, readtags evaluates an
expression (S expression or sexp) given as an option argument for
``-Q`` option. As the result of the evaluation, readtags gets
an value. false represented as `#f` in S expression, means
rejection: readtags doesn't print it.

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

All symbols started from `$` represent a field of an entry which is
under judgment with the S expression. Most of all them are evaluated
as a string or `#f`. It is evaluated as `#f` when the field doesn't
exist. `$inherits` is evaluated to a list of strings if the entry has
`inherits` field. `scope` field holds structured data: the kind and
name of upper scope combined with `:`. The kind part goes
`$scope-kind`, and the name part goes `$scope-name`.

`$scope-kind` and `$scope-name` can be used only if the
input tags file is generated by ctags with ``--fields=+Z``.

All symbols not started from `$` are operators. When using, put them
at the head(car) of list. The rest(cdr) of list are passed to the
operator as arguments. Many of them are also available of scheme
language; see the other documents.

prefix?, suffix?, and substr? may be only available in this
implementation. All of them takes two strings. The first one
is called target.

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
Make tags(*foo.tags*) with following command line

.. code-block:: console

	$ ./ctags --fields='*' --extra='*' -o foo.tags foo.py

for following input (*foo.py*)

.. code-block:: Python

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
* Print entries ended with "q"

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

* Print only full qualified entries (assuming "." is used as the separator)

  .. code-block:: console

	$ ./readtags -e -t foo.tags -Q '(and (eq? $kind "member") (substr? $name "."))' -l
	Bar.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Bar	access:public	signature:()
	Bar.bw	foo.py	/^    def bw ():$/;"	kind:member	language:Python	scope:class:Bar	access:public	signature:()
	Baz.bq	foo.py	/^    def bq ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	Baz.bw	foo.py	/^    def bw ():$/;"	kind:member	language:Python	scope:class:Baz	access:public	signature:()
	Foo.ae	foo.py	/^    def ae ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()
	Foo.aq	foo.py	/^    def aq ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()
	Foo.aw	foo.py	/^    def aw ():$/;"	kind:member	language:Python	scope:class:Foo	access:public	signature:()

* Print only inheriting specified classes

  .. code-block:: console

	$ ./readtags  -e -t foo.tags -Q '(and (member "Foo" $inherits) (eq? $kind "class"))' -l
	Bar	foo.py	/^class Bar (Foo):$/;"	kind:class	language:Python	inherits:Foo	access:public
	Baz	foo.py	/^class Baz (Foo): $/;"	kind:class	language:Python	inherits:Foo	access:public


