*Units* test facility
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

Exuberant ctags has a test facility. The test case were *Test*
directory. So Here I call it *Test*.

Main aim of the facility is detecting regression. All files under Test
directory are given as input for old and new version of ctags
commands.  The output tags files of both versions are compared. If any
difference is found the check fails. *Test* expects the older ctags
binary to be correct.

This expectation is not always met. Consider that a parser for a new
language is added. You may want to add a sample source code for that
language to *Test*. An older ctags version is unable to generate a
tags file for that sample code, but the newer ctags version does. At
this point a difference is found and *Test* reports failure.

The units test facility(*Units*) I describe here takes a different
approach. An input file and an expected output file are given by a
contributor of a language parser. The units test facility runs ctags
command with the input file and compares its output and the expected
output file. The expected output doesn't depend on ctags.

If a contributor sends a patch which may improve a language parser,
and if a reviewer is not familiar with that language, s/he cannot
evaluate it.

*Unit* test files, the pair of input file and expected output file may
be able to explain the intent of patch well; and may help the
reviewer.

How to write a test case
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The test facility recognizes an input file and an expected
output file by patterns of file name. Each test case should
have its own directory under Units directory.

*Units/TEST/input.\** **requisite**

	Input file name must have a *input* as basename. *TEST*
	part should explain the test case well.

*Units/TEST/input-[0-9].\** *Units/TEST/input-[0-9]-*.\** **optional**

	Optional input file names. They are put next to *input.\** in
	testing command line.

*Units/TEST/expected.tags* **optional**

	Expected output file must have a name *expected.tags*. It
	should be the same directory of the input file.

	If this file is not given, the exit status of ctags process
	is just checked; the output is ignored.

	If you want to test etags output (specified with ``-e`` ),
	Use **.tags-e** as suffix instead of **.tags**.
	In such a case you don't have to write ``-e`` to ``args.ctags``.
	The test facility sets ``-e`` automatically.

	If you want to test cross reference output (specified with ``-x`` ),
	Use **.tags-x** as suffix instead of **.tags**.
	In such a case you don't have to write ``-x`` to ``args.ctags``.
	The test facility sets ``-x`` automatically.

	If you want to test json output (specified with ``--output-format=json`` ),
	Use **.tags-json** as suffix instead of **.tags**.
	In such a case you don't have to write ``--output-format=json`` to ``args.ctags``,
	and ``json`` to ``features``.
	The test facility sets the option and the feature automatically.

*Units/TEST/args.ctags* **optional**

	``-o -`` is used as default optional argument when running a
	unit test ctags. If you want to add more options, enumerate
	options in **args.ctags** file. This file is an optional.

	Remember you have to put one option in one line; don't
	put multiple options to one line. Multiple options in
	one line doesn't work.

*Units/TEST/filter-\*.\** **optional**

	You can rearrange the output of ctags with this command
	before comparing with *executed.tags*.
	This command is invoked with no argument. The output
	ctags is given via stdin. Rearrange data should be
	written to stdout.

*Units/TEST/features* **optional**

	If a unit test case requires special features of ctags,
	enumerate them in this file line by line. If a target ctags
	doesn't have one of the features, the test is skipped.

	If a file line is started with ``!``, the effect is inverted;
	if a target ctags has the feature specified with ``!``, the
	test is skipped.

	All features built-in can be listed with passing
	``--list-features`` to ctags.

*Units/TEST/languages* **optional**

	If a unit test case requires that language parsers are enabled/available,
	enumerate them in this file line by line. If one of them is
	disabled/unavailable, the test is skipped.

	language parsers enabled/available can be checked with passing
	``--list-languages`` to ctags.


*Units/TEST/dictfile* **optional**

	Used in spell checking.
	See :ref:`cspell <cspell>` for more details.

Note for importing a test case from Test directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I think all test cases under Test directory should be converted to
Units.

If you convert use following TEST name convention.

* use *.t* instead of *.d* as suffix for the name

Here is an example::

	Test/simple.sh

This should be::

	Units/simple.sh.t

With this name convention we can track which test case is converted or
not.

Example of files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See *Units/c-sample/input.c* and *Units/c-sample/expected*.

How to run unit tests
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*test* make target::

	 $ make units

The result of unit tests is reported by lines. You can specify
test cases with ``UNITS=``.

An example to run *vim-command.d* only::

	$ make units UNITS=vim-command

Another example to run *vim-command.d* and *parser-python.r/bug1856363.py.d*::

	$ make units UNITS=vim-command,bug1856363.py

During testing *OUTPUT.tmp*, *EXPECTED.tmp* and *DIFF.tmp* files are
generated for each test case directory. These are removed when the
unit test is **passed**.  If the result is **FAILED**, it is kept for
debugging. Following command line can clean up these generated files
at once::

	$ make clean-units

Other than **FAILED** and **passed** two types of result are
defined.


**skipped**

	means running the test case is skipped in some reason.

**failed (KNOWN bug)**

	mean the result if failed but the failure is expected.
	See "Gathering test cases for known bugs".

Example of running
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
::

	$ make units
	Category: ROOT
	-------------------------------------------------------------------------
	Testing 1795612.js as JavaScript                            passed
	Testing 1850914.js as JavaScript                            passed
	Testing 1878155.js as JavaScript                            passed
	Testing 1880687.js as JavaScript                            passed
	Testing 2023624.js as JavaScript                            passed
	Testing 3184782.sql as SQL                                  passed
	...

Running unit tests for specific languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can run only the tests for specific languages by setting
``LANGUAGES`` to parsers as reported by
``ctags --list-languages``::

	make units LANGUAGES=PHP,C

Multiple languages can be selected using a comma separated list.

Gathering test cases for known bugs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we met a bug, making a small test case that triggers the bug is
important development activity. Even the bug cannot be fixed in soon,
the test case is an important result of work. Such result should
be merged to the source tree. However, we don't love **FAILED**
message, too. What we should do?

In such a case, merge as usually but use *.b* as suffix for
the directory of test case instead of *.d*.

*Unix/css-singlequote-in-comment-issue2.b* is an example
of *.b* suffix usage.

When you run test.units target, you will see::

    Testing c-sample as C                                 passed
    Testing css-singlequote-in-comment as CSS             failed (KNOWN bug)
    Testing ctags-simple as ctags                         passed

Suffix *.i* is a variant of *.b*. *.i* is for merging/gathering input
which lets ctags process enter an infinite loop. Different from *.b*,
test cases marked as *.i* are never executed. They are just skipped
but reported the skips::

    Testing ada-ads as Ada                                passed
    Testing ada-function as Ada                           skipped (may cause an infinite loop)
    Testing ada-protected as Ada                          passed
    ...

    Summary (see CMDLINE.tmp to reproduce without test harness)
    ------------------------------------------------------------
      #passed:                                347
      #FIXED:                                 0
      #FAILED (unexpected-exit-status):       0
      #FAILED (unexpected-output):            0
      #skipped (features):                    0
      #skipped (languages):                   0
      #skipped (infinite-loop):               1
        ada-protected
      ...

Running under valgrind and timeout
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If ``VG=1`` is given, each test cases are run under valgrind.
If valgrind detects an error, it is reported as::

    $ make units VG=1
    Testing css-singlequote-in-comment as CSS             failed (valgrind-error)
    ...
    Summary (see CMDLINE.tmp to reproduce without test harness)
    ------------------------------------------------------------
    ...
    #valgrind-error:                        1
      css-singlequote-in-comment
    ...

In this case the report of valgrind is recorded to
``Units/css-singlequote-in-comment/VALGRIND-CSS.tmp``.

NOTE: ``/bin/bash`` is needed to report the result. You can specify a shell
running test with SHELL macro like::

    $ make units VG=1 SHELL=/bin/bash


If ``TIMEOUT=N`` is given, each test cases are run under timeout
command. If ctags doesn't stop in ``N`` second, it is stopped
by timeout command and reported as::

    $ make units TIMEOUT=1
    Testing css-singlequote-in-comment as CSS             failed (TIMED OUT)
    ...
    Summary (see CMDLINE.tmp to reproduce without test harness)
    ------------------------------------------------------------
    ...
    #TIMED-OUT:                             1
      css-singlequote-in-comment
    ...

If ``TIMEOUT=N`` is given, *.i* test cases are run. They will be
reported as *TIMED-OUT*.

Categories
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED

With *.r* suffix, you can put test cases under a sub directory
of *Units*. ``Units/parser-ada.r`` is an example. If *misc/units*
test harness, the sub directory is called a category. ``parser-ada.r``
is the name category in the above example.

*CATEGORIES* macro of make is for running units in specified categories.
Following command line is for running units in
``Units/parser-sh.r`` and ``Units/parser-ada.r``::

  $ make units CATEGORIES='parser-sh,parser-ada'


Finding minimal bad input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a test case is failed, the input causing ``FAILED`` result is
passed to *misc/units shrink*.  *misc/units shrink* tries to make the
shortest input which makes ctags exits with non-zero status.  The
result is reported to ``Units/\*/SHRINK-${language}.tmp``.  Maybe
useful to debug.

Acknowledgments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The file name rule is suggested by Maxime Coste <frrrwww@gmail.com>.
