.. _puppetManifest:

======================================================================
puppetManifest parser
======================================================================

.. NOT REVIEWED YET

:Maintainer: Masatake YAMATO <yamato@redhat.com>

puppetManifest is an experimental parser for testing multi tables
regex meta parser defined with ``--_mtable-<LANG>`` option.

The parser has some bugs derived from the limit of the multi tables
regex meta parser.


Here document

	The parser cannot ignore the contents inside the area of
	here document. The end marker of here document is defined
	in the source code. Currently, ctags has no way to add a
	regex pattern for detecting the end maker.
