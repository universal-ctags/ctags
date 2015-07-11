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


Finding performance bottleneck
------------------------------------------------------------

See https://wiki.geany.org/howtos/profiling/gperftools and #383

Checking coverage
------------------------------------------------------------
After doing ``make clean``, you can build coverage measuring ready
ctags by ``make COVERAGE=1``. At this time *\*.gcno* files are generated
by the compiler. *\*.gcno* files can be removed with ``make clean``.

After building ctags, you can run run-gcov target.  When running
*\*.gcda* files.  The target runs ctags withh all input files under
*Units/\*\*/input.\**; and call gcov. Human readable result is
printed. The detail can be shown in *\*.gcov*. files. *\*.gcda* files
and *\*.gcov* files can be removed with ``make clean-gcov``.
