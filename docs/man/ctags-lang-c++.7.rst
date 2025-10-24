.. _ctags-lang-c++(7):

==============================================================
ctags-lang-c++
==============================================================

Random notes about tagging C++ source code with Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+C++ ...
|	**ctags** ... --language-force=C++ ...
|	**ctags** ... --map-C++=+.c++  ...
|	**ctags** ... --map-C++=+.cc  ...
|	**ctags** ... --map-C++=+.cpp  ...
|	**ctags** ... --map-C++=+.h  ...
|   ...

DESCRIPTION
-----------
This man page gathers random notes about tagging C++ source code.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New field ``section``

* New field ``alias``

Change since "1.1"
~~~~~~~~~~~~~~~~~~

* New kinds ``module`` and ``partition``

* New roles ``imported`` and ``exported`` for ``header`` kind

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`,
:ref:`ctags-lang-ldscript(7) <ctags-lang-ldscript(7)>`,
`The new C/C++ parser <https://docs.ctags.io/en/latest/parser-cxx.html>`_ (https://docs.ctags.io/en/latest/parser-cxx.html)
