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
tags file with ``_TAG_FILE_ENCODING`` pseudo tag in the tag file.

This feature solves this issue utilizing ``_TAG_FILE_ENCODING``
pseudo tag.

This patch introduces two type of options (``--input-encoding=IN``
and ``--output-encoding=OUT``).

As specified encoding with these options ctags converts input from
``IN`` encoding to ``OUT`` encoding. ctags uses the converted strings
when writing pattern parts of tags lines. As the result tags output is
encoded in ``OUT`` encoding.  In addition ``OUT`` is specified in the
top tags file as value for ``_TAG_FILE_ENCODING`` pseudo tag.  As
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
``--list-extras``, ``--list-features`` and ``--list-fields`` are added.

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
