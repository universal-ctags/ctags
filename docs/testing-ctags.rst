.. _testing_ctags:

=============================================================================
Testing ctags
=============================================================================

.. contents:: `Table of contents`
	:depth: 1
	:local:

..	tmain.rst

*Tmain*: a facility for testing main part
------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

*Tmain* is introduced to test the area where *Units*
does not cover well.

*Units* works fine for testing parsers. However, it
assumes something input is given to ctags command,
and a `tags` file is generated from ctags command.

Other aspects cannot be tested. Such areas are files
and directories layout after installation, standard
error output, exit status, etc.

You can run test cases with following command line:

::

	$ make tmain

*Tmain* is still under development so I will not write
the details here.


To write a test case, see files under `Tmain/tmain-example.d`.
In the example, *Tmain* does:

1. runs new subshell and change the working directory to `Tmain/tmain-example.d`,
2. runs `run.sh` with `bash`,
3. captures stdout, stderr and exit status, and
4. compares them with `stdout-expected.txt`, `stderr-expected.txt`,
   and `exit-expected.txt`.
5. compares it with `tags-expected.txt` if run.sh generates `tags` file.

`run.sh` is run with following 3 arguments:

1. the path for the target ctags
2. the path for `builddir` directory
3. the path for the target readtags

The path for readtags is not reliable; readtags command is not
available if --disable-readcmd was given in configure time.  A case,
testing the behavior of readtags, must verify the command existence
with `test -x $3` before going into the main part of the test.

When comparing `tags` file with `tags-expected.txt`, you
must specify the path of `tags` explicitly with -o option
in ctags command line like::

	CTAGS=$1
	BUILDDIR=$2
	${CTAGS} ... -o $BUILDDIR/tags ...

This makes it possible to keep the original source directory clean.

See also `tmain_run` and `tmain_compare` functions in `misc/units`.

If run.sh exits with code 77, the test case is skipped.
The output to stdout is captured and printed as the reason
of skipping.

TODO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Run under valgrind

..	tinst.rst

*Tinst*: installation test
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

-----

tinst target is for testing the result of ``make install``.

::

   $ make tinst

Fussy syntax checking
------------------------------------------------------------
If ``-Wall`` of gcc is not enough, you may be interested in this.

You can change C compiler warning options with 'WARNING_CFLAGS'
configure arg-var option.

::

   $ ./configure WARNING_CFLAGS='-Wall -Wextra'


If configure option '--with-sparse-cgcc' is specified,
cgcc is used as CC. cgcc is part of `Sparse, Semantic Parser for C
<https://sparse.docs.kernel.org/en/latest/>`_.
It is used in development of Linux kernel for finding programming error.
cgcc acts as a c compiler but more fussy. '-Wsparse-all' is used as
default option passed to cgcc but you can change with 'CGCC_CFLAGS'
configure arg-var option.

::

   $ ./configure --with-sparse-cgcc [CGCC_CFLAGS='-Wsparse-all']


Finding performance bottleneck
------------------------------------------------------------

See `Profiling with gperftools
<https://wiki.geany.org/howtos/profiling/gperftools>`_ and `#383
<https://github.com/universal-ctags/ctags/issues/383>`_.

See also `codebase <https://github.com/universal-ctags/codebase>`_.

Checking coverage
------------------------------------------------------------
Before starting coverage measuring, you need to specify
'--enable-coverage-gcov' configure option.

::

   $ ./configure --enable-coverage-gcov


After doing ``make clean``, you can build coverage measuring ready
ctags by ``make``. At this time *\*.gcno* files are generated
by the compiler. *\*.gcno* files can be removed with ``make clean``.

After building ctags, you can run run-gcov target.  When running
*\*.gcda* files.  The target runs ctags with all input files under
*Units/\*\*/input.\**; and call ``gcov``. Human readable result is
printed. The detail can be shown in *\*.gcov*. files. *\*.gcda* files
and *\*.gcov* files can be removed with ``make clean-gcov``.

Running cppcheck
------------------------------------------------------------

.. NOT REVIEWED YET

`cppcheck <http://cppcheck.sourceforge.net/>`_ is a tool for static C/C++ code
analysis.

To run it do as following after install cppcheck::

   $ make cppcheck
