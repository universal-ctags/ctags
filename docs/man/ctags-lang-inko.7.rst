.. _ctags_lang-inko(7):

======================================================================
ctags-lang-inko
======================================================================

:Version: 5.9.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Inko ...
|	**ctags** ... --language-force=Inko ...
|	**ctags** ... --map-Inko=+.inko ...

DESCRIPTION
-----------
This man page describes the Inko parser for Universal Ctags.

The input file is expected to be valid Inko source code, otherwise the output of
ctags is undefined.

Tags are generated for objects, traits, methods, attributes, and constants.
String literals are ignored.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
