.. _ctags-lang-c(7):

==============================================================
ctags-lang-c
==============================================================

Random notes about tagging C source code with Universal Ctags

:Version: 6.1.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+C ...
|	**ctags** ... --language-force=C ...
|	**ctags** ... --map-C=+.c ...

DESCRIPTION
-----------
This man page gathers random notes about tagging C source code.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New role ``foreigndecl`` for ``function`` kind

* New role ``foreigndecl`` for ``struct`` kind

* New field ``section``

* New field ``alias``

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`,
:ref:`ctags-lang-ldscript(7) <ctags-lang-ldscript(7)>`,
`The new C/C++ parser <https://docs.ctags.io/en/latest/parser-cxx.html>`_ (https://docs.ctags.io/en/latest/parser-cxx.html)
