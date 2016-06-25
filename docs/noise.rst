*Noise* testing
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

-----

After enjoying developing Semi-fuzz testing, I'm looking for a more unfair
approach. Run

::

	$ make noise LANGUAGES=LANG1[,LANG2,...]

It takes a long time, especially with ``VG=1``, so this cannot be run
under Travis CI. However, it is a good idea to run it locally.

The noise target generates test cases by inserting or deleting one
character to the test cases of *Units*.

TBW
