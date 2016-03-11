=============================================================================
Choosing a proper parser in ctags
=============================================================================

.. contents:: `Table of contents`
	:depth: 3
	:local:

Universal ctags has multiple way to guess the language of input files.

Modeline based parser selection
---------------------------------------------------------------------
exuberant-ctags has the ability to choose a proper parser based on shebang
lines (e.g. *#!/bin/sh*).

Editors like vim and emacs recognize special patterns in files called
modelines. The line is inserted by a user of the text editor and can
be used to set the file type (Vim) or mode (emacs).

universal-ctags also recognizes these modeline and selects a language parser
based on it if ``--guess-language-eagerly`` (or ``-G``) option is given.


ctags recognizes the following patterns used in emacs:

  * at the head of input file or at the line next of shebang line::

      -*- mode: MODE; -*-

    or ::

      -*- MODE -*-

  * at the end of input file::

      Local Variables:
      ...
      mode: MODE
      ...
      End:


ctags recognizes the following patterns used in vim:

  * at the end of input file::

      vim:set filetype=SYNTAX

    or ::

      ex:se ft=SYNTAX


ctags recognizes the following patterns and considers the
input as a zsh script:

  * at the head of input file::

      #autoload

    or ::

      #compdef ....


NOTE: This feature takes a performance hit: it opens the input file
once to detect the file type and a second time to process the file
with the detected parser. For this reason, this feature is enabled
only if the ``--guess-language-eagerly`` option is used. This option
can be placed in the .ctags file to have this feature always enabled.

Better parser selection for template files
---------------------------------------------------------------------
Consider an input file name *foo.c.in*.  Suffix *.in* is popular as a
name for template files.  Well-known one is *config.h.in* used in GNU
autotools.

ctags used suffix here *\*.in* for choosing a parser. *.in* shows
nothing about the language used in the input file. When universal-ctags
finds *.in* as suffix, universal-ctags checks the next suffix, here *.c*.

Specialized language selectors
---------------------------------------------------------------------

In some cases, a special function may be used to figure out which parser
to choose.  (For example, Perl 6 files may have extension .pm, just like
Perl files do.)  To implement such selector, set ``selectLanguage`` in
each of the possibly conflicting parsers to the same selection function
(which you are to implement).  By convention, the function should begin
with "selectBy".  Then, if there are more than one parser candidates for
a file and they all have ``selectLanguage`` set to the same selector
function, this function will be called to pick the language.

Dry running
---------------------------------------------------------------------
With ``--print-language`` option, you can test the parser selector of
ctags. e.g.::

	$ ./ctags --print-language main.c
	main.c: C

If no parser is selected, ``NONE`` is printed as parser name.


Guessing from keywords
---------------------------------------------------------------------

Not implemented.

Some parses have keywords table. We can utilize them to guess
the language of input files.
