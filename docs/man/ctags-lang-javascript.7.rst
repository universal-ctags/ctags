.. _ctags-lang-javascript(7):

==============================================================
ctags-lang-javascript
==============================================================

Random notes about tagging JavaScript source code with Universal Ctags

:Version: 6.2.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+JavaScript ...
|	**ctags** ... --language-force=JavaScript ...
|	**ctags** ... --map-JavaScript=+.js ...

DESCRIPTION
-----------
This man page gathers random notes about tagging JavaScript source code.

CLASSES
-------

ES6 introduced the ``class`` keyword, but there is still the original method of defining a function and attaching a ``prototype``.  ctags follows the convention that function names that start with a capital letter are class constructors.

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New role ``foreigndecl`` for ``function`` kind

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
