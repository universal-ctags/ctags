Input validation for *Units*
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

We have to maintain parsers for languages that we don't know well.  We
don't have enough time to learn the languages.

*Units* test cases help us not introduce wrong changes to a parser.

However, there is still an issue; a developer who doesn't know a
target language well may write a broken test input file for the
language.  Here comes "Input validation."

You can validate the test input files of *Units* with *validate-input*
make target if a validator for a language is defined.

How to run and an example session
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
