.. _ctags-lang-lisp(7):

==============================================================
ctags-lang-lisp
==============================================================

Random notes about tagging Lisp source code with Universal Ctags

:Version: 6.1.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Lisp ...
|	**ctags** ... --language-force=Lisp ...
|	**ctags** ... --map-Lisp=+.cl ...
|	**ctags** ... --map-Lisp=+.clisp ...
|	**ctags** ... --map-Lisp=+.l ...
|	**ctags** ... --map-Lisp=+.lisp ...
|	**ctags** ... --map-Lisp=+.lsp ...

DESCRIPTION
-----------
This man page gathers random notes about tagging Lisp source code.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* Add kinds:

  + ``class``,
  + ``generic``,
  + ``method``,
  + ``parameter``,
  + ``struct``, and
  + ``type``.

* Add ``definer`` field.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`
