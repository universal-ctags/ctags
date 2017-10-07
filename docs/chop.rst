*Chop* and *slap* testing
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

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
