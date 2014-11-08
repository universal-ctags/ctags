Using *Units*
============================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

Exuberant ctags has a test facility. Main aim of the facility is
detecting regression. All files under Test directory are given as
input for old and new version of ctags commands.  The output tags
files of both versions are compared.

The units test facility I describe here takes a different approach. An
input file and an expected output file are given by a contributor of a
language parser. The units test facility runs ctags command with the
input file and compares its output and the expected output file.

If a contributor sends a patch which may improve a language parser,
and if a reviewer is not familiar with that language, s/he cannot
evaluate it.

*Unit* test files, the pair of input file and expected output file may
be able to explain the intent of patch well; and may help the
reviewer.

How to write a test case
------------------------------------------------------------

The test facility recognizes an input file and an expected
output file by patterns of file name. Each test case should
have its own directory under Units directory.

*Units/TEST/input.\* **requisite**

	Input file name must have a *input* as basename. *TEST*
	part should explain the test case well.

*Units/TEST/expected.tags* **requisite**

	Expected output file must have a name *expected.tags*. It
	should be the same directory of the input file.

*Units/TEST/args.ctags* **optional**

	``-o -`` is used as default optional argument when running a
	unit test ctags. If you want to add more options, enumerate
	options in **args.ctags** file. This file is an optional.

*Units/TEST/filter-\*.\** **optional**

	TBW

*Units/TEST/features* **optional**

	If a unit test case requires special features of ctags,
	enumerate them in this file line by line. If a target ctags
	doesn't have one of the features, the test is skipped.

	All features built-in can be listed with passing
	``--list-features`` to ctags.

*Units/TEST/languages* **optional**

	If a unit test case requires that language parsers are enabled/available,
	enumerate them in this file line by line. If one of them is
	disabled/unavailable, the test is skipped.

	language parsers enabled/available can be checked with passing
	``--list-langauges`` to ctags.

Note for importing a test case from Test directory
------------------------------------------------------------

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
------------------------------------------------------------

See *Units/c-sample/input.c* and *Units/c-sample/expected*.

How to run unit tests
------------------------------------------------------------

*test.units* target is added to testing.mak::

	 $ make -f testing.mak test.units

The result of unit tests is reported by lines. You can specify
a test case with ``UNIT=``. Consider you want to run a test under
*vim-command.d* only. You can do it with following command line::

	$ make -f testing.mak test.units UNIT=vim-command

During testing *OUTPUT*, *EXPECTED* and *DIFF* files are generated for each
test case directory. These are removed when the unit test is **Passed**.
If the result is **Failed**, it is kept for debugging. Following
command line can clean up these generated files at once::

         $ make -f testing.mak clean-test

Other than **Failed** and **Passed** two types of result are
defined.


**Skipped**

	means the ctags command, the testing target, doesn't
	support the feature required by the test case.
	See *Units/TEST/features*.

**failed but KNOWN bug**

	mean the result if failed but the failure is expected.
	See "Gathering test cases for known bugs".
	
Example of running
------------------------------------------------------------
::

	$ make -f testing.mak test.units
	Testing Units/cpp-type-alias-with-using-keyword...Passed
	Testing Units/c-sample...Passed
	...

Runnig unit tests for a specific language
------------------------------------------------------------

You can run only the tests for a specific language by setting
``UNIT_LANGUAGE`` to a specific parser as reported by
``ctags --list-languages``::

	make -f testing.mak test.units UNIT_LANGUAGE=PHP

Gathering test cases for known bugs
------------------------------------------------------------

When we met a bug, making a small test case that triggers the bug is
important develoment activity. Even the bug cannot be fixed in soon,
the test case is an important result of work. Such result should
be merged to the source tree. However, we don't love **FAILED**
message, too. What we should do?

In such case, merge as usually but use *.b* as suffix for
the directory of test case instead of *.d*.

*Unix/css-singlequote-in-comment-issue2.b* is an example
of *.b* suffix usage.

When you run test.units target, you will see::

    Testing Units/c-sample...passed
    Testing Units/css-singlequote-in-comment...failed but KNOWN bug
    Testing Units/ctags-simple...passed

Suffix *.i* is a variant of *.b*. *.i* is for merging/gathering input
which lets ctags process enter an infinite loop. Different from *.b*,
test cases marked as *.i* are never executed. They are just skipped
but reported the skips::

    Testing Units/ada-ads...passed
    Testing Units/ada-function...skipped (infinite loop)
    Testing Units/ada-protected...passed
    ...

      Summary of "Units" test
      -------------------------
	    #passed:  336
	    #failed:  0
	    #skipped(features):  0
	    #skipped(languages):  0
	    #skipped(infinite loop):  1
	    #known-bugs:  2


Acknowledgements
------------------------------------------------------------

The file name rule is suggested by Maxime Coste <frrrwww@gmail.com>.
