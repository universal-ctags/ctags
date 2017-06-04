.. _guessing:

======================================================================
Choosing a proper parser in ctags
======================================================================

.. IN MAN PAGE

.. contents:: `Table of contents`
	:depth: 3
	:local:

.. NOT REVIEWED YET START

Exuberant-ctags uses interpreter lines(`#!` line) of input file,
file name extensions, and file name pattern for choosing proper
parsers for an input file.

Universal-ctags has more ways to guess the language of input files.
.. NOT REVIEWED YET END

Modeline based parser selection
---------------------------------------------------------------------

.. IN MAN PAGE

Exuberant-ctags has the ability to choose a proper parser based on shebang
lines (e.g. *#!/bin/sh*).

Editors like Vim and Emacs recognize special patterns in files called
modelines. These lines are inserted by a user of the text editor and
can be used to set the file type (Vim) or mode (Emacs).

Universal-ctags also recognizes these modelines and selects a language parser
based on them if ``--guess-language-eagerly`` (or ``-G``) is given.


ctags recognizes the following patterns used in Emacs:

  * at the head of the input file or at the line after the shebang line::

      -*- mode: MODE; -*-

    or ::

      -*- MODE -*-

  * at the end of input file::

      Local Variables:
      ...
      mode: MODE
      ...
      End:


ctags recognizes the following patterns used in Vim:

  * at the end of input file::

      vim:set filetype=SYNTAX

    or ::

      ex:se ft=SYNTAX


ctags recognizes the following patterns and matches the
input file as a Z-Shell script:

  * at the head of input file::

      #autoload

    or ::

      #compdef ....


NOTE: This feature comes with a performance hit; it opens the input file
once to detect the file type and a second time to process the file
with the detected parser. For this reason, this feature is enabled
only if the ``--guess-language-eagerly`` option is used. This option
can be placed in the .ctags file to have this feature always enabled.

Better parser selection for template files
---------------------------------------------------------------------

.. IN MAN PAGE

The suffix *.in* is popularly used for template files. A well-known
example is *config.h.in* used in GNU Autotools.

Consider an input file name *foo.c.in*. In this case the suffix
indicates nothing about the language used in the input file. So, in
order to select the appropriate parser, ctags first removes the
suffix *.in* and then matches against the new filename suffix *.c*.

Specialized language selectors
---------------------------------------------------------------------

.. IN MAN PAGE (partially written)

In some cases, a special function may be used to figure out which parser
to choose.  (For example, Perl 6 files may have extension .pm, just like
Perl files do.)  To implement such selector, set ``selectLanguage`` in
each of the possibly conflicting parsers to the same selection function
(which you are to implement).  By convention, the function should begin
with "selectBy".  Then, if there is more than one parser candidate for
a file and they all have ``selectLanguage`` set to the same selector
function, this function will be called to pick the language.

Dry running
---------------------------------------------------------------------

.. IN MAN PAGE

The parser selector of ctags can be tested with ``--print-language``.
e.g.::

	$ ./ctags --print-language main.c
	main.c: C

If no parser is selected, ``NONE`` is printed as the parser name.


Guessing from keywords
---------------------------------------------------------------------

Not implemented.

Some parsers have a keywords table. We can utilize these to guess the
language of input files.
