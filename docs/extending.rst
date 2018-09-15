=============================================================================
Extending ctags
=============================================================================

Exuberant-ctags allows a user to add a new parser to ctags with ``--langdef=<LANG>``
and ``--regex-<LANG>=...`` options.

Universal-ctags follows and extends the design of Exuberant-ctags in more
powerful ways, as described in the following chapters.

Universal-ctags encourages users to share the new parsers defined by
their options. See :ref:`optlib <optlib>` to know how you can share your
parser definition with others.

Note that some of the new features are experimental, and will be marked as such
in the documentation.

.. toctree::
	:maxdepth: 2

	optlib.rst
	internal.rst
