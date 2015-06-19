Tips for hacking
=============================================================================

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

