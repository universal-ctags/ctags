*Noise* testing
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

-----

After enjoying developing Semi-fuzz testing, I'm looking for more unfair
approach. Run

::

	$ make noise LANGUAGES=LANG1[,LANG2,...]

It takes long time especially with ``VG=1`` so this one cannot be run
under travis. However, it is worth to run on your local machine.

The noise target generates test cases by inserting or deleting one
character to the test cases of *Units*.


TBW
