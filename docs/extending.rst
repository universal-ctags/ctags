=============================================================================
Extending ctags
=============================================================================

Exuberant-ctags allows a user adding a new parser to ctags with ``--langdef=<LANG>``
and ``--regex-<LANG>=...`` options,

Universal-ctags follows and extends the design of Exuberant-ctags.

Universal-ctags encourages users to share the new parsers defined by
the options. See :ref:`optlib <optlib>` to know how you can share your
parser definition with other.

Above new feature is very experimental, especially the directory layout
for :ref:`optlib <optlib>` will be changed in the future.

.. toctree::
	:maxdepth: 2

	optlib.rst
	internal.rst
