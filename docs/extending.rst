.. _extending_ctags_in_c:

=============================================================================
Extending ctags with a parser written in C
=============================================================================

This chapter describes how to add a parser in C and the internal API of
Universal Ctags.

Before you start writing a parser in C, consider using *optlib parser*.
Universal Ctags extends the functionality so extensively that it can implement
most of functionality for the parser.
See :ref:`ctags-optlib(7) <ctags-optlib(7)>` and :ref:`optlib` for details.

*optlib parser* is also suitable for prototyping of a parser in C.

.. toctree::
	:maxdepth: 4

	parser-in-c.rst
	internal.rst
