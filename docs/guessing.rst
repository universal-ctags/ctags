Choosing a proper parser in ctags
=============================================================================

Universal ctags has multiple way to guess the language of input files.

Automatic parser selection based on corpora
---------------------------------------------------------------------
Ctags has two built-in parsers for suffix *.m*: ``ObjectiveC`` and
``Matlab``. The decision on which parser ctags should use is called
"parser conflict".

Like in ``--language-force`` option, ctags provides some ways to
choose a parser manually. However, it would be nice if ctags could
choose a proper parser without manual instruction.

With ``--corpus-<LANG>=spec:corpusFile`` option you can prepare corpus a
file to make ctags learn lexical tendency of a language. Ctags
learns it as typical input of ``LANG``. Based on this learning ctags
tries to solve the parser conflict. See *Data/optlib/mib.ctags*
and *Data/corpora/RFC1213-MIB.txt* as an example of the usage of
``--corpus-<LANG>``.

For ``ObjectiveC`` and ``Matlab`` parsers, corpus files are embedded
within the parser implementations. See *objc.c* and *matlab.c*.

.. TODO More documentation is needed.


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
nothing about the language used in the input file. When fishman-ctags
finds *.in* as suffix, fishman-ctags checks the next suffix, here *.c*.

Dry running
---------------------------------------------------------------------
With ``--print-language`` option, you can test the parser selector of
ctags. e.g.::

	$ ./ctags --print-language main.c
	main.c: C

If no parser is selected, ``NONE`` is printed as parser name.


Guessing from keywords
---------------------------------------------------------------------

Not implementated.

Some parses have keywords table. We can utilize them to guess
the language of input files.
