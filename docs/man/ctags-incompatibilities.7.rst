.. _ctags-incompatibilities(7):

==============================================================
ctags-incompatibilities
==============================================================

Incompatibilities between Universal Ctags and Exuberant Ctags

:Version: 5.9.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** [options] [file(s)]
|	**etags** [options] [file(s)]

DESCRIPTION
-----------

This page describes major incompatible changes introduced to
Universal Ctags forked from Exuberant Ctags.

Option files loading at starting up time (preload files)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Universal Ctags doesn't load ``~/.ctags`` at starting up time.
File paths for preload files are changed.
See "FILES" section of :ref:`ctags(1) <ctags(1)>`.

Environment variables for arranging command lines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Universal Ctags doesn't read ``CTAGS`` and/or ``ETAGS`` environment
variables.

Incompatibilities in command line interface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ordering in a command line
....................................................................................

.. NOTE: #1889

The command line format of Universal Ctags is "``ctags [options]
[source_file(s)]``" following the standard POSIX convention.

Exuberant Ctags accepts a option following a source file.

.. code-block:: console

	$ ctags -o - foo.c --list-kinds=Sh
	f  functions

Universal Ctags warns and ignores the option ``--list-kinds=Sh`` as follows.

.. code-block:: console

	$ ctags -o - foo.c --list-kinds=Sh
	ctags: Warning: cannot open input file "--list-kinds=Sh" : No such file or directory
	a	foo.c	/^void a () {}$/;"	f	typeref:typename:void
	b	foo.c	/^void b () {}$/;"	f	typeref:typename:void

The order of application of patterns and extensions in ``--langmap``
....................................................................................

When applying mappings for a name of given source file,
Exuberant Ctags tests file name patterns *AFTER* file extensions
(*e-map-order*). Universal Ctags does this differently; it tests file
name patterns *BEFORE* file extensions (*u-map-order*).

This incompatible change is introduced to deal with the following
situation:

	* ``build.xml`` as a source file,
	* The Ant parser declares it handles a file name pattern ``build.xml``, and
	* The XML parser declares it handles a file extension ``.xml``.

Which parser should be used for parsing ``build.xml``?  The assumption
of Universal Ctags is the user may want to use the Ant parser; the
file name pattern it declares is more specific than the file extension
that the XML parser declares. However, e-map-order chooses the XML
parser.

So Universal Ctags uses the u-map-order even though it introduces an
incompatibility.

``--list-map-extensions=<language>`` and ``--list-map-patterns=<language>``
options are helpful to verify and the file extensions and the file
name patterns of given *<language>*.

Remove ``--file-tags`` and ``--file-scope`` options
....................................................................................

Even in Exuberant Ctags, ``--file-tags`` is not documented in its man page.
Instead of specifying ``--file-tags`` or ``--file-tags=yes``, use
``--extras=+f`` or ``--extras=+{inputFile}``.

Instead of specifying ``--file-tags=no``, use
``--extras=-f`` or ``--extras=-{inputFile}``.

Universal Ctags introduces ``F/fileScope`` extra as the replacement for
``--file-scope`` option.

Instead of specifying ``--file-tags`` or ``--file-tags=yes``, use
``--extras=+F`` or ``--extras=+{fileScope}``.

Instead of specifying ``--file-tags=no``, use
``--extras=-F`` or ``--extras=-{fileScope}``.

Incompatibilities in language and kind definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Language name defined with ``--langdef=name`` option
....................................................................................

The characters you can use are more restricted than Exuberant Ctags.
For more details, see the description of ``--langdef=name`` in :ref:`ctags-optlib(7) <ctags-optlib(7)>`.

Obsoleting ``--<LANG>-kinds`` option
....................................................................................

Some options have *<LANG>* as parameterized parts in their name like
``--foo-<LANG>=...`` or ``--<LANG>-foo=...``. The most of all such
options in Exuberant Ctags have the former form, ``--foo-<LANG>=...``.
The exception is ``--<LANG>-kinds``.

Universal Ctags uses the former form for all *<LANG>* parameterized
option. Use ``--kinds-<LANG>`` instead of ``--<LANG>-kinds`` in
Universal Ctags. ``--<LANG>-kinds`` still works but it will be
removed in the future.

The former form may be friendly to shell completion engines.

Disallowing to define a kind with ``file`` as name
....................................................................................

The kind name ``file`` is reserved.  Using it as part of kind spec in
``--regex-<LANG>`` option is now disallowed.

Disallowing to define a kind with '``F``' as letter
....................................................................................

The kind letter '``F``' is reserved.  Using it as part of a kind spec in
``--regex-<LANG>`` option is now disallowed.

Disallowing to use other than alphabetical character as kind letter
....................................................................................

Exuberant Ctags accepts a character other than alphabetical character
as kind letter in ``--regex-<LANG>=...`` option.  Universal Ctags
accepts only an alphabetical character.

Acceptable characters as parts of a kind name
....................................................................................

Exuberant Ctags accepts any character as a part of a kind name
defined with ``--regex-<LANG>=/regex/replacement/kind-spec/``.

Universal Ctags accepts only an alphabetical character as
the initial letter of a kind name.
Universal Ctags accepts only an alphabetical character or
numerical character as the rest letters.

An example::

  --regex-Foo=/abstract +class +([a-z]+)/\1/a,abstract class/i

Universal Ctags rejects this because the kind name, ``abstract class``,
includes a whitespace character.

This requirement is for making the output of Universal Ctags follow
the tags file format.

A combination of a kind letter and a kind name
....................................................................................

In Universal Ctags, the combination of
a kind letter and a kind name must be unique in a language.

You cannot define more than one kind reusing a kind letter with
different kind names. You cannot define more than one kind reusing a
kind name with different kind letters.

An example::

  --regex-Foo=/abstract +class +([a-z]+)/\1/a,abstractClass/i
  --regex-Foo=/attribute +([a-z]+)/\1/a,attribute/i

Universal Ctags rejects this because the kind letter, '``a``', used twice
for defining a kind ``abstractClass`` and ``attribute``.


Incompatibilities in tags file format
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using numerical character in the name part of tag tagfield
....................................................................................

The version 2 tags file format, the default output format of
Exuberant Ctags, accepts only alphabetical characters in the name part
of tag tagfield.

Universal Ctags introduces an exception to this specification; it may
use numerical characters in addition to alphabetical characters as the
letters other than initial letter of the name part.

The kinds ``heading1``, ``heading2``, and ``heading3`` in the HTML parser
are the examples.

Truncating the pattern for long input lines
....................................................................................

To prevent generating overly large tags files, a pattern field is
truncated, by default, when its size exceeds 96 bytes. A different
limit can be specified with ``--pattern-length-limit=N``. Specifying
0 as *N* results no truncation as Exuberant Ctags does not.

Kind letters and names
....................................................................................

A kind letter '``F``' and a kind name ``file`` are reserved in the
main part. A parser cannot have a kind conflicting with
these reserved ones. Some incompatible changes are introduced
to follow the above rule.

* Cobol's ``file`` kind is renamed to ``fileDesc`` because the
  kind name ``file`` is reserved.

* Ruby's '``F``' (singletonMethod) is changed to '``S``'.

* SQL's '``F``' (field) is changed to '``E``'.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-optlib(7) <ctags-optlib(7)>`, and :ref:`tags(5) <tags(5)>`.
