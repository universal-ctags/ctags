.. _ctags-lang-systemtap(7):

==============================================================
ctags-lang-systemtap
==============================================================

Random notes about tagging SystemTap source code with Universal Ctags

:Version: 6.2.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+SystemTap ...
|	**ctags** ... --language-force=SystemTap ...
|	**ctags** ... --map-SystemTap=+.stp ...

DESCRIPTION
-----------
This man page gathers random notes about tagging SystemTap scripts.

Guests
~~~~~~~~~~~
The SystemTap parser runs CPreProcessor as a guest parser on the areas
surrounded by `%{` and `%}`.

"input.stp"

.. code-block:: SystemTap

	%{
	#define X 1
	%}

"output.tags"
with "--options=NONE -o - --sort=no --extras=+{guest} input.stp"

.. code-block:: tags

	X	input.stp	/^#define X /;"	d	file:

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New role ``attached`` for ``probe`` kind

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, `SystemTap Language Reference <https://sourceware.org/systemtap/langref>`_ (https://sourceware.org/systemtap/langref/)
