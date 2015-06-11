Building ctags
=============================================================================

Build system add possibility to change program name
---------------------------------------------------------------------

As on some systems (e.g. BSD) there is a 'ctags' program in the base
system it's somewhat inconvenient to have the same name for universal-ctags
During ``configure`` you can now change the output executable name.

Add a prefix 'ex' which will result in 'ctags' transformed into 'exctags'
::

	$ ./configure --program-prefix=ex

Completely change program name, in this case it's important to remember
there is also 'etags' along 'ctags'
::

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'


Fussy syntax checking
------------------------------------------------------------
If -Wall of gcc is not enough, you may be interested in this.

If SPARSE macro is defined, cgcc is used as CC.  cgcc is part of
sparse, Semantic Parser for C.  It is used in development of Linux
kernel for finding programming error. cgcc acts as a c compiler but
more fussy. -Wsparse-all is used as default option passed to cgcc
but you can change with SPARSEFLAG macro.

::

   $ make SPARSE=1 [SPARSEFLAGS=-Wsparse-all]

.. include:: windows.rst

.. include:: osx.rst

