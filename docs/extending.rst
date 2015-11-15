Extending ctags
=============================================================================

Exuberant-ctags allows a user adding a new parser to ctags with ``--langdef=<LANG>``
and ``--regex-<LANG>=...`` options,

Universal-ctags follows and extends the design of Exuberant-ctags.

Universal-ctags can run an external command as a parser for specified
language.  See :ref:`xcmd <xcmd>` to know how to integrate an external
parser command and universal-ctags.

Universal-ctags encourages users to share the new parsers defined by
the options. See :ref:`optlib <optlib>` to know how you can share your
parser definition with other.

Above new feature is very experimental, especially the directory layout
for `optlib <optlib>` will be changed in the future.

.. 
	revised version of EXTENDING.html should be here.

:ref:`xcmd`
     
:ref:`optlib`

:ref:`internal`
