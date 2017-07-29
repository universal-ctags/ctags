Semi-fuzz(*Fuzz*) testing
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

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

	$ ./ctags --list-languages

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
