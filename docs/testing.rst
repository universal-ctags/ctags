.. _testing_parser:

=============================================================================
Testing a parser
=============================================================================


.. contents:: `Table of contents`
	:depth: 3
	:local:

It is difficult for us to know syntax of all languages supported in ctags. Test
facility and test cases are quite important for maintaining ctags with limited
resources.

..	_units:

*Units* test facility
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

**Test facility**

Exuberant Ctags has a test facility. The test case were *Test*
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

**Units facility**

The units test facility (*Units*) I describe here takes a different
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

*Units/TEST/input[-_][0-9].\** *Units/TEST/input[-_][0-9][-_]\*.\** **optional**

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
	and add ``json`` to ``features`` as described below.
	The test facility sets the option and the feature automatically.

*Units/TEST/args.ctags* **optional**

	``-o -`` is used as default optional argument when running a
	unit test ctags. If you want to add more options, enumerate
	options in **args.ctags** file.

	Remember you have to put one option in one line; don't
	put multiple options to one line. Multiple options in
	one line doesn't work.

*Units/TEST/filter* **optional**

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

See `Units/parser-c.r/c-sample.d
<https://github.com/universal-ctags/ctags/tree/master/Units/parser-c.r/c-sample.d>`_.

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

	means the result was failed but the failure is expected.
	See ":ref:`gathering_test`".

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

.. _gathering_test:

Gathering test cases for known bugs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we meet a bug, it is an important development activity to make a small test
case that triggers the bug.
Even the bug cannot be fixed in soon,
the test case is an important result of work. Such result should
be merged to the source tree. However, we don't love **FAILED**
message, too. What we should do?

In such a case, merge as usually but use *.b* as suffix for
the directory of test case instead of *.d*.

``parser-autoconf.r/nested-block.ac.b/`` is an example
of ``.b``*`` suffix usage.

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

Reviewing the result of Units test
------------------------------------------------------------

Try misc/review.

.. code-block:: console

    $ misc/review --help
    Usage:
            misc/review          help|--help|-h                 show this message
            misc/review          [list] [-b]                    list failed Units and Tmain
                                 -b                             list .b (known bug) marked cases
            misc/review          inspect [-b]                   inspect difference interactively
                                 -b                             inspect .b (known bug) marked cases
    $

Semi-fuzz(*Fuzz*) testing
---------------------------------------------------------------------

Unexpected input can lead ctags to enter an infinite loop. The fuzz
target tries to identify these conditions by passing
semi-random (semi-broken) input to ctags.

::

	$ make fuzz LANGUAGES=LANG1[,LANG2,...]

With this command line, ctags is run for random variations of all test
inputs under *Units/\*/input.\** of languages defined by ``LANGUAGES``
macro variable. In this target, the output of ctags is ignored and
only the exit status is analyzed. The ctags binary is also run under
timeout command, such that if an infinite loop is found it will exit
with a non-zero status. The timeout will be reported as following::

	[timeout C]                Units/test.vhd.t/input.vhd

This means that if C parser doesn't stop within N seconds when
*Units/test.vhd.t/input.vhd* is given as an input, timeout will
interrupt ctags. The default duration can be changed using
``TIMEOUT=N`` argument in *make* command. If there is no timeout but
the exit status is non-zero, the target reports it as following::

	[unexpected-status(N) C]                Units/test.vhd.t/input.vhd

The list of parsers which can be used as a value for ``LANGUAGES`` can
be obtained with following command line

::

	$ ctags --list-languages

Besides ``LANGUAGES`` and ``TIMEOUT``, fuzz target also takes the
following parameters:

	``VG=1``

		Run ctags under valgrind. If valgrind finds a memory
		error it is reported as::

			[valgrind-error Verilog]                Units/array_spec.f90.t/input.f90

		The valgrind report is recorded at
		``Units/\*/VALGRIND-${language}.tmp``.

As the same as units target, this semi-fuzz test target also calls
*misc/units shrink* when a test case is failed. See "*Units* test facility"
about the shrunk result.

*Noise* testing
---------------------------------------------------------------------

After enjoying developing Semi-fuzz testing, I'm looking for a more unfair
approach. Run

::

	$ make noise LANGUAGES=LANG1[,LANG2,...]

The noise target generates test cases by inserting or deleting one
character to the test cases of *Units*.

It takes a long time, even without ``VG=1``, so this cannot be run
under Travis CI. However, it is a good idea to run it locally.

*Chop* and *slap* testing
---------------------------------------------------------------------

After reviving many bug reports, we recognized some of them spot
unexpected EOF. The chop target was developed based on this recognition.

The chop target generates many input files from an existing input file
under *Units* by truncating the existing input file at variety file
positions.

::

   $ make chop  LANGUAGES=LANG1[,LANG2,...]

It takes a long time, especially with ``VG=1``, so this cannot be run
under Travis CI. However, it is a good idea to run it locally.

slap target is derived from chop target. While chop target truncates
the existing input files from tail, the slap target does the same
from head.

..	_input-validation:

Input validation for *Units*
---------------------------------------------------------------------

We have to maintain parsers for languages that we don't know well.  We
don't have enough time to learn the languages.

*Units* test cases help us not introduce wrong changes to a parser.

However, there is still an issue; a developer who doesn't know a
target language well may write a broken test input file for the
language.  Here comes "Input validation."

How to run an example session of input validation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can validate the test input files of *Units* with *validate-input*
make target if a validator or a language is defined.

Here is an example validating an input file for JSON.

.. code-block:: console

  $ make validate-input VALIDATORS=jq
  ...
  Category: ROOT
  ------------------------------------------------------------
  simple-json.d/input.json with jq                                 valid

  Summary
  ------------------------------------------------------------
    #valid:                                 1
    #invalid:                               0
    #skipped (known invalidation)           0
    #skipped (validator unavailable)        0


This example shows validating *simple-json.d/input.json* as an input
file with *jq* validator. With VALIDATORS variable passed via
command-line, you can specify validators to run. Multiple validators
can be specified using a comma-separated list.  If you don't give
VALIDATORS, the make target tries to use all available validators.

The meanings of "valid" and "invalid" in "Summary" are apparent.  In
two cases, the target skips validating input files:

#skipped (known invalidation)

    A test case specifies KNOWN-INVALIDATION in its *validator* file.

#skipped (validator unavailable)

    A command for a validator is not available.

*validator* file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*validator* file in a *Units* test directory specifies which
validator the make target should use.

.. code-block:: console

  $ cat Units/simple-json.d/validator
  jq

If you put *validator* file to a category directory (a directory
having *.r* suffix), the make target uses the validator specified in
the file as default.  The default validator can be overridden with a
*validator* file in a subdirectory.

.. code-block:: console

  $ cat Units/parser-puppetManifest.r/validator
  puppet
  # cat Units/parser-puppetManifest.r/puppet-append.d/validator
  KNOWN-INVALIDATION

In the example, the make target uses *puppet* validator for validating
the most of all input files under *Units/parser-puppetManifest.r*
directory. An exception is an input file under
*Units/parser-puppetManifest.r/puppet-append.d* directory.  The
directory has its specific *validator* file.

If a *Unit* test case doesn't have *expected.tags* file, the make
target doesn't run the validator on the file even if a default
validator is given in its category directory.

If a *Unit* test case specifies KNOWN-INVALIDATION in its *validator*
file, the make target just increments "#skipped (known invalidation)"
counter. The target reports the counter at the end of execution.

validator command
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A validator specified in a *validator* file is a command file put
under *misc/validators* directory.  The command must have "validator-"
as prefix in its file name. For an example,
*misc/validators/validator-jq* is the command for "jq".

The command file must be an executable. *validate-input* make target
runs the command in two ways.

*is_runnable* method

    Before running the command as a validator, the target runs
    the command with "is_runnable" as the first argument.
    A validator command can let the target know whether the
    validator command is runnable or not with exit status.
    0 means ready to run. Non-zero means not ready to run.

    The make target never runs the validator command for
    validation purpose if the exit status is non-zero.

    For an example, *misc/validators/validator-jq* command uses *jq*
    command as its backend. If *jq* command is not available on a
    system, *validator-jq* can do nothing.  If such case,
    *is_runnable* method of *validator-jq* command should exit with
    non-zero value.

*validate* method

    The make target runs the command with "validate* and an input
    file name for validating the input file.  The command exits
    non-zero if the input file contains invalid syntax. This method
    will never run if *is_runnable* method of the command exits with
    non-zero.


..	_man_test:

Testing examples in language specific man pages
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

`man-test` is a target for testing the examples in the language
specific man pages (``man/ctags-lang-<LANG>.7.rst.in``). The command
line for running the target is:

.. code-block:: console

   $ make man-test

An example for testing must have following form:

.. code-block:: ReStructuredText

  "input.<EXT>"

  .. code-block:: <LANG>

    <INPUT LINES>

  "output.tags"
  with "<OPTIONS FOR CTAGS>"

  .. code-block:: tags

    <TAGS OUTPUT LINES>


The man-test target recognizes the form and does the same as
the following shell code for each example in the man page:

.. code-block:: console

  $ echo <INPUT LINES> > input.<EXT>
  $ echo <TAGS OUTPUT LINES> > output.tags
  $ ctags <OPTIONS FOR CTAGS> > actual.tags
  $ diff output.tags actual.tags

A backslash character at the end of ``<INPUT LINES>`` or
``<TAGS OUTPUT LINES>`` represents the continuation of lines;
a subsequent newline is ignored.

.. code-block:: ReStructuredText

    .. code-block:: tags

       very long\
            line

is read as:

.. code-block:: ReStructuredText

    .. code-block:: tags

       very long     line

Here is an example of a test case taken from
``ctags-lang-python.7.rst.in``:

.. code-block:: ReStructuredText

	"input.py"

	.. code-block:: Python

	   import X0

	"output.tags"
	with "--options=NONE -o - --extras=+r --fields=+rzK input.py"

	.. code-block:: tags

		X0	input.py	/^import X0$/;"	kind:module	roles:imported

``make man-test`` returns 0 if the all test cases in the all language
specific man pages are passed.

Here is an example output of the man-test target.

.. code-block:: console

	$ make man-test
	  RUN      man-test
	# Run test cases in ./man/ctags-lang-julia.7.rst.in
	```
	./man/ctags-lang-julia.7.rst.in[0]:75...passed
	./man/ctags-lang-julia.7.rst.in[1]:93...passed
	```
	# Run test cases in ./man/ctags-lang-python.7.rst.in
	```
	./man/ctags-lang-python.7.rst.in[0]:116...passed
	./man/ctags-lang-python.7.rst.in[1]:133...passed
	./man/ctags-lang-python.7.rst.in[2]:154...passed
	./man/ctags-lang-python.7.rst.in[3]:170...passed
	./man/ctags-lang-python.7.rst.in[4]:187...passed
	./man/ctags-lang-python.7.rst.in[5]:230...passed
	```
	# Run test cases in ./man/ctags-lang-verilog.7.rst.in
	```
	./man/ctags-lang-verilog.7.rst.in[0]:51...passed
	```
	OK

NOTE: keep examples in the man pages simple. If you want to test ctags
complicated (and or subtle) input, use the units target. The main
purpose of the examples is for explaining the parser.
