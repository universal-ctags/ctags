.. _optlib:


Extending ctags with Regex parser(*optlib*)
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

Writing regex parser and using it as option library(*optlib*)
---------------------------------------------------------------------

exuberant-ctags provides the way to customize ctags with options like
``--langdef=<LANG>`` and ``--regex-<LANG>``. An option file where options are
written can be loaded with ``--options=OPTION_FILE``.

This feature was extended such that ctags treats option files
as libraries. Developers of universal-ctags can maintain option files
as part of universal-ctags, making part of its release. With ``make
install`` they are also installed along with ctags command.

universal-ctags prepares directories where the option files are installed.

Consider a GNU/Linux distribution.
The following directories are searched when loading an option file:

#. *~/.ctags.d/optlib*
#. */etc/ctags/optlib*
#. */usr/share/ctags/optlib*

The name of an option file must have .conf or .ctags as suffix.

If ctags is invoked with following command line::

	$ ctags --options=m4 ...

Following files are searched with following order for finding ``m4``:

#.  *~/.ctags.d/optlib/m4.conf*
#.  *~/.ctags.d/optlib/m4.ctags*
#.  */etc/ctags/optlib/m4.conf*
#.  */etc/ctags/optlib/m4.ctags*
#.  */usr/share/ctags/optlib/m4.conf*
#.  */usr/share/ctags/optlib/m4.ctags*

These are called built-in search paths.

If these search paths are not desired, the full path of the option
file can be directly specified with ``--options``. The parameter must
start with */* (absolute path) or *./* (relative path) like::

	$ ctags --option=/home/user/test/m4.cf
	$ ctags --option=./test/m4.cf

Here the suffix restriction doesn't exist.

On GNU/Linux more directories can be added with the environment variable
``CTAGS_DATA_PATH``.

::

	$ CTAGS_DATA_PATH=A:B ctags --options=m4 ...

The files are searched with the order described below for finding *m4*:

#. *A/optlib/m4.conf*
#. *A/optlib/m4.ctags*
#. *B/optlib/m4.conf*
#. *B/optlib/m4.ctags*
#. *~/.ctags.d/optlib/m4.conf*
#.  ...

Further more ``--data-path=[+]PATH`` can be used for adding more
directories with environment variable::

	$ CTAGS_DATA_PATH=A:B ctags --data-path=+C --options=m4 ...

In this case files are searched with the following order to find
*m4*:

#. *C/optlib/m4.conf*
#. *C/optlib/m4.ctags*
#. *A/optlib/m4.conf*
#. *A/optlib/m4.ctags*
#. *B/optlib/m4.conf*
#. *B/optlib/m4.ctags*
#. *~/.ctags.d/optlib/m4.conf*
#. ...

If *+* is omitted, the directory is set instead of added::

	$ CTAGS_DATA_PATH=A:B ctags --data-path=C --options=m4 ...

In this case files are searched with the following order to find
*m4*:

#. *C/config/m4.conf*
#. *C/config/m4.ctags*

The directory list can be emptied using the reserved file name ``NONE``::

	$ CTAGS_DATA_PATH=A:B ctags --data-path=NONE --options=m4 ...

In this case ctags only tries to load *./m4*.

See also "Loading option recursively".

How a directory is set/added to the search path can be reviewed using
``--verbose`` option. This is useful for debugging this feature.

Pull requests with updated or new option files are welcome by ctags
developers.

NOTE: Although ``--data-path`` has highest priority, ``--data-path`` doesn't
affect a stage of automatic option file loading. Following files are
automatically loaded when ctags starts:

#. */ctags.cnf* (on MSDOS, MSWindows only)
#. */etc/ctags.conf*
#. */usr/local/etc/ctags.conf*
#. *$HOME/.ctags*
#. *$HOME /ctags.cnf* (on MSDOS, MSWindows only)
#. *.ctags*
#. *ctags.cnf* (on MSDOS, MSWindows only)

NOTE: This feature is still experimental. The name of directories,
suffix rules and other conventions may change.

.. TODO
..
.. * Write about MSWindows more(*.cnf*).
.. * ``accept_only_dot_ctags()`` doesn't  check *.cnf*.

See "Contributing an optlib" if you have a good optlib.

Loading option recursively
---------------------------------------------------------------------

The option file loading rules explained in "Option library" is more
complex. If a directory is specified as parameter for ``--option`` instead
of a file, universal-ctags loads option files under the directory
recursively.

Consider the following command line on a GNU/Linux distribution::

	$ ctags --options=bundle ...

The following directories are searched first:

#. *~/.ctags.d/optlib/bundle.d*
#. */etc/ctags/optlib/bundle.d*
#. */usr/share/ctags/optlib/bundle.d*

If *bundle.d* is found and is a directory, files (*\*.ctags*
and *\*.conf*), directories (\*.d) are loaded recursively.

.. TODO

NOTE: If *bundle.d* is not found above list, file
*bundle.ctags* or *bundle.conf* is searched. This rule is a bit
ugly. Following search rules look better.

#. *~/.ctags.d/optlib/bundle.d*
#. *~/.ctags.d/optlib/bundle.ctags*
#. *~/.ctags.d/optlib/bundle.conf*
#. */etc/ctags/optlib/bundle.d*
#. */etc/ctags/optlib/bundle.ctags*
#. */etc/ctags/optlib/bundle.conf*
#. */usr/share/ctags/optlib/bundle.d*
#. */usr/share/ctags/optlib/bundle.ctags*
#. */usr/share/ctags/optlib/bundle.conf*

NOTE: This feature requires ``scandir`` library function. This feature may
be disabled on which platform scandir is not available. Check ``option-directory``
in the supported features::

	$ ./ctags --list-features
	wildcards
	regex
	option-directory


Directories for preloading
---------------------------------------------------------------------

As written in "Option library", option libraries can be loaded with
``--options`` option. However, loading them without explicitly
specifying it may be desired.

Following files can be used for this purpose.

* ~/.ctags
* /ctags.cnf (on MSDOS, MSWindows only)
* /etc/ctags.conf
* /usr/local/etc/ctags.conf

This preloading feature comes from universal-ctags. However, two
weaknesses exist in this implementation.

* The file must be edited when an option library is to be loaded.

  If one wants to add or remove an ``--options=`` in a *ctags.conf*,
  currently one may have to use sed or something tool for adding or
  removing the line for the entry in */usr/local/etc/ctags.conf* (or
  */etc/ctags.conf*).

  There is a discussion about a similar issue in
  *http://marc.info/?t=129794755000003&r=1&w=2* about */etc/exports*
  of NFS.

* The configuration defined by the system administrator cannot be
  overridden.

  A user must accept all configuration including ``--options=``
  in */etc/ctags.conf* and */usr/local/etc/ctags.conf*.

The following directories were introduced for preloading purpose.

#. *~/.ctags.d/preload*
#. */etc/ctags/preload*
#. */usr/share/ctags/preload*

All files and directories under the directories are loaded recursively,
with two restrictions:

* file/directory name

  The same suffix rules written in "Option library" and
  "Loading option recursively" are applied in preloading, too.

* overriding

  The traversing and loading are done in the order listed above.
  Once a file is loaded, another file with the same name is not loaded.
  Once a directory is traversed, another directory with the same name is
  not traversed.

  universal-ctags prepares */usr/share/ctags/preload/default.ctags*.
  If you want ctags not to load it, make an empty file at
  *~/.ctags/default.ctags*. To customize
  */usr/share/ctags/preload/default.ctags*, copy the file to
  *~/.ctags.d/default.ctags* and edit it as desired.

  Assume */usr/share/ctags/preload/something.d* exits.
  Some *.ctags* files are in the directory. With making
  an empty directory at *~/.ctags.d/something.d*, you
  can make ctags not to traverse */usr/share/ctags/preload/something.d*.
  As the result *.ctags* files under */usr/share/ctags/preload/something.d*
  are not loaded.

  To customize one of file under
  */usr/share/ctags/preload/something.d*, copy
  */usr/share/ctags/preload/something.d* to *~/.ctags.d/somethind.d* recursively.
  Symbolic links can also be used. After copying or symbolic linking, edit
  one of the copied file.

This feature is heavily inspired by systemd.


Long regex flag
---------------------------------------------------------------------

Regex parser is made more useful by adding more kinds of flags
to ``--regex-<LANG>`` expression. As explained in
*ctags.1* man page, ``b``, ``e`` and ``i`` are defined as flags in
exuberant-ctags.

Even if more flags are added like ``x``, ``y``, ``z``,..., users
may not utilize them well because it is difficult to memorize them. In
addition, if many "option libraries" are contributed, we have to
maintain them.

For both users and developers the variety of short flags are just
nightmares.

So universal-ctags now includes an API for defining long flags, which can be
used as aliases for short flags. The long flags requires more typing
but are more readable.

Here is the mapping between the standard short flag names and long flag names:

=========== ===========
short flag  long flag
=========== ===========
b           basic
e           extend
i           icase
=========== ===========

Long flags can be specified with surrounding ``{`` and ``}``.
So the following ``--regex-<LANG>`` expression ::

   --m4-regex=/^m4_define\(\[([^]$\(]+).+$/\1/d,definition/x

is the same as ::

   --m4-regex=/^m4_define\(\[([^]$\(]+).+$/\1/d,definition/{extend}

The characters ``{`` and ``}`` may not be suitable for command line
use, but long flags are mostly intended for option libraries.

The notion for the long flag is also introduced in ``--langdef`` option.

Exclusive flag in regex
---------------------------------------------------------------------

A line read from input files was matched with **all** regular expressions
defined with ``--regex-<LANG>``. Each regular
expression matched successfully emits a tag.

In some cases another policy, exclusive-matching, is preferable to the
all-matching policy. Exclusive-matching means the rest of regular
expressions are not tried if one of regular expressions is matched
successfully,

For specifying exclusive-matching the flags ``exclusive`` (long) and
``x`` (short) were introduced. It is used in *data/optlib/m4.ctags*
for ignoring a line::

	--regex-m4=/#.*(define|undefine|s?include)\>//x
	--regex-m4=/\<dnl.*(define|undefine|s?include)\>//x

Comments are started from ``#`` or ``dnl`` in many use case of m4 language.
With above options ctags can ignore ``define`` in comments.

If an empty name pattern(``//``) is found in ``--regex-<LANG>`` option
ctags warns it as wrong usage of the option. However, the flags
``exclusive`` or ``x`` is specified, the warning is suppressed. This
is imperfect approach for ignoring text insides comments but it may
be better than nothing. Ghost kind is assigned to the empty name
pattern. (See "Ghost kind in regex parser".)

Ghost kind in regex parser
---------------------------------------------------------------------

If a whitespace is used as a kind letter, it is never printed when
ctags is called with ``--list-kinds`` option.  This kind is
automatically assigned to an empty name pattern.

Normally you don't need to know this.

Passing parameter for long regex flag
---------------------------------------------------------------------

In the implemented API long-flags can take a parameters.
Conceptual example::

	--regex-<LANG>=/regexp1/replacement/kind-spec/{transformer=uppercase}
	--regex-<LANG>=/regexp2/replacement/kind-spec/{transformer=lowercase}
	--regex-<LANG>=/regexp2/replacement/kind-spec/{transformer=capitalize}

Currently scope long flag taking such parameter.

Scope tracking in a regex parser
---------------------------------------------------------------------

With scope long flag, you can record/track scope context.
A stack is used for tracking the scope context.

`{scope=push}`

	Push the tag captured with a regex pattern to the top of the stack.
	If you don't want to record this tag but just push, use
	`placeholder` long option together.

`{scope=ref}`

	Refer the thing of top of the stack as a scope where
	the tag captured with a regex pattern is.
	The stack is not modified with this specification.
	If the stack is empty, this flag is just ignored.

`{scope=pop}`

	Pop the thing of top of the stack.
	If the stack is empty, this flag is just ignored.

`{scope=clear}`

	Make the stack empty.

`{scope=set}`

	Clear then push.

`{placeholder}`

	Don't print a tag captured with a regex pattern
	to a tag file.
	This is useful when you need to push non-named context
	information to the stack.  Well known non-named scope in C
	language is established with `{`. non-named scope is never
	appeared in tags file as name or scope name.  However, pushing
	it is important to balance `push` and `pop`.

Example 1::

    $ cat /tmp/input.foo
    class foo:
	def bar(baz):
	    print(baz)
    class goo:
	def gar(gaz):
	    print(gaz)

    $ cat /tmp/foo.ctags
    --langdef=foo
	    --map-foo=+.foo
	    --regex-foo=/^class[[:blank:]]+([[:alpha:]]+):/\1/c,class/{scope=set}
	    --regex-foo=/^[[:blank:]]+def[[:blank:]]+([[:alpha:]]+).*:/\1/d,definition/{scope=ref}

    $ ~/var/ctags/ctags --options=/tmp/foo.ctags -o - /tmp/input.foo
    bar	/tmp/input.foo	/^    def bar(baz):$/;"	d	class:foo
    foo	/tmp/input.foo	/^class foo:$/;"	c
    gar	/tmp/input.foo	/^    def gar(gaz):$/;"	d	class:goo
    goo	/tmp/input.foo	/^class goo:$/;"	c


Example 2::

    $ cat /tmp/input.pp
    class foo {
	include bar
    }

    $ cat /tmp/pp.ctags
    --langdef=pp
	    --map-pp=+.pp
	    --regex-pp=/^class[[:blank:]]*([[:alnum:]]+)[[[:blank:]]]*\{/\1/c,class,classes/{scope=push}
	    --regex-pp=/^[[:blank:]]*include[[:blank:]]*([[:alnum:]]+).*/\1/i,include,includes/{scope=ref}
	    --regex-pp=/^[[:blank:]]*\}.*//{scope=pop}{exclusive}

    $ ~/var/ctags/ctags --options=/tmp/pp.ctags -o - /tmp/input.pp
    bar	/tmp/input.pp	/^    include bar$/;"	i	class:foo
    foo	/tmp/input.pp	/^class foo {$/;"	c


Override the letter for file kind
---------------------------------------------------------------------
(See also #317.)

Background
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
``F`` is widely used as a kind letter for file kind. The ``F`` was
hardcoded in ctags internal. However, we found some built-in parsers
including Ruby uses ``F`` for their own kind. So if you find a tag
having ``F`` as a kind letter, you cannot say what it is well: a
file name or something peculiar in a language. Long kind description
strings may help you but I am not sure all tools utilizing ``tags``
file refer the long kind description strings.

To fix the issue for letters for the kind
we modified ctags as follows:
we let the built-in parsers use ``!`` as a letter for file kind
instead of ``F``.

This modification breaks the backward-compatibility of meaning of tags
file. Forcing to use ``F`` for file kinds to the parsers was another
choice but it also breaks the backward-compatibility. We assumed the
impact of using ``!`` for the parsers may be weaker than forcing
t to use ``F``.

For xcmd and regex parsers we prepare the way to override the default
file kind letter, ``F``. Though using this in regex parser is not
recommend.  Try not using ``F`` as a kind letter in your regex
parser. In xcmd parser you may have no choice if the back-end tags file
generator uses ``F`` for its own purpose.

Usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For overriding add ``fileKind`` long flag ``--langdef=LANG`` option.
Following is an example to use ``Z`` as a kind letter in a language named
``foo``::

	$ ctags --langdef=foo'{fileKind=Z}' ...

Single quote is used here to avoid the expansion and evaluate the breaths
by shell.

To know the fileKind of languages, use ``--list-file-kind``::

	$ ctags --list-file-kind 
	Ada F
	Ant F
	Asm F
	...
	Ruby !
	...


Submitting an optlib to universal-ctags project
---------------------------------------------------------------------

You are welcome.

universal-ctags provides a facility for "Option library".
Read "Option library" about the concept and usage first.

Here I will explain how to merge your .ctags into universal-ctags as
part of option library. Here I assume you consider contributing
an option library in which a regex based language parser is defined.
See `How to Add Support for a New Language to Exuberant Ctags (EXTENDING)`_
about the way to how to write a regex based language parser. In this
section I explains the next step.

.. _`How to Add Support for a New Language to Exuberant Ctags (EXTENDING)`: http://ctags.sourceforge.net/EXTENDING.html

I use Swine as the name of programming language which your parser
deals with. Assume source files written in Swine language have a suffix
*.swn*. The file name of option library is *swine.ctags*.


Changes in options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
universal-ctags prepares aliases for options.

========================= ====================
Exuberant                 New aliases
========================= ====================
``--langmap-swine:.swn``  ``--swine-map=.swn``
``--regex-swine=...``     ``--swine-regex=...``
========================= ====================

These are jst aliases. So the original options can be used.
The reason I introduced these aliases, I want option syntax
more language/parser centric. I'm not sure this idea is good
or not.

Copyright notice, contact mail address and license term
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Put these information at the header of *swine.ctags*.

An example taken from *data/optlib/ctags.ctags* ::

    #
    #
    #  Copyright (c) 2014, Red Hat, Inc.
    #  Copyright (c) 2014, Masatake YAMATO
    #
    #  Author: Masatake YAMATO <yamato@redhat.com>
    #
    # This program is free software; you can redistribute it and/or
    # modify it under the terms of the GNU General Public License
    # as published by the Free Software Foundation; either version 2
    # of the License, or (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.
    #
    # You should have received a copy of the GNU General Public License
    # along with this program; if not, write to the Free Software
    # Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
    # USA.
    #
    #
    ...

"GPL version 2 or later version" is needed here.  Option file is not
linked to ctags command. However, I have a plan to write a translator
which generates *.c* file from a given option file. As the result the
*.c* file is built into *ctags* command. In such case "GPL version 2
or later version" may be required.

*Units* test cases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We, universal-ctags developers don't have enough time to learn all
languages supported by ctags. In other word, we cannot review the
code. Only test cases help us to know whether a contributed option
library works well or not. We may reject any contribution without
a test case.

Read "Using *Units*" about how to write *Units* test
cases.  Don't write one big test case. Some smaller cases are helpful
to know about the intent of the contributor.

* *Units/sh-alias.d*
* *Units/sh-comments.d*
* *Units/sh-quotes.d*
* *Units/sh-statements.d*

are good example of small test cases.
Big test cases are good if smaller test cases exist.

See also *parser-m4.r/m4-simple.d* especially *parser-m4.r/m4-simple.d/args.ctags*.
Your test cases need ctags having already loaded your option
library, swine.ctags. You must specify loading it in the 
test case own *args.ctags*.

Assume your test name is *swine-simile.d*. Put ``--option=swine`` in
*Units/swine-simile.d/args.ctags*.

Makefile.in
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Add your optlib file, *swine.ctags* to ``PRELOAD_OPTLIB`` variable of
*Makefile.in*.


If you don't want your optlib loaded automatically when ctags starting up,
put your optlib file to ``OPTLIB`` of *Makefile.in* instead of 
``PRELOAD_OPTLIB``.

Verification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's verify all your work here.

1. Run the tests and check whether your test case is passed or failed::

	$ make units

2. Verify your files are installed as expected::

	$ mkdir /tmp/tmp
	$ ./configure --prefix=/tmp/tmp
	$ make
	$ make install
	$ /tmp/tmp/ctags -o - --option=swine something_input.swn


Pull-request
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Remember your *.ctags* is treasure and can be shared as a first class
software component in universal-ctags.  Again, pull-requests are welcome.
