=============================================================================
Tips for hacking
=============================================================================

Fussy syntax checking
------------------------------------------------------------
If -Wall of gcc is not enough, you may be interested in this.

You can change C compiler warning options with 'WARNING_CFLAGS'
configure arg-var option.

::

   $ ./configure WARNING_CFLAGS='-Wall -Wextra'


If configure option '--with-sparse-cgcc' is specified,
cgcc is used as CC. cgcc is part of Sparse, Semantic Parser for C.
It is used in development of Linux kernel for finding programming error.
cgcc acts as a c compiler but more fussy. '-Wsparse-all' is used as
default option passed to cgcc but you can change with 'CGCC_CFLAGS'
configure arg-var option.

::

   $ ./configure --with-sparse-cgcc [CGCC_CFLAGS='-Wsparse-all']


Finding performance bottleneck
------------------------------------------------------------

See https://wiki.geany.org/howtos/profiling/gperftools and #383

Checking coverage
------------------------------------------------------------
Before starting coverage measuring, you need to specify
'--enable-coverage-gcov' configure option.

::

   $ ./configure --enable-coverage-gcov


After doing ``make clean``, you can build coverage measuring ready
ctags by ``make COVERAGE=1``. At this time *\*.gcno* files are generated
by the compiler. *\*.gcno* files can be removed with ``make clean``.

After building ctags, you can run run-gcov target.  When running
*\*.gcda* files.  The target runs ctags with all input files under
*Units/\*\*/input.\**; and call gcov. Human readable result is
printed. The detail can be shown in *\*.gcov*. files. *\*.gcda* files
and *\*.gcov* files can be removed with ``make clean-gcov``.


Reviewing the result of Units test
------------------------------------------------------------

Try misc/review. [TBW]

Running cppcheck
------------------------------------------------------------

.. NOT REVIEWD YET

cppcheck is a tool for static C/C++ code analysis.

To run it do as following after install cppcheck::

   $ make cppcheck
