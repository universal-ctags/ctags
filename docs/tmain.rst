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

`run.sh` is run with following 4 arguments:

1. the path for the target ctags
2. the path for `builddir` directory
3. the path for the target readtags

The path for readtags is not reliable; readtags command is not
available if --disable-readcmd was given in configure time.  A case,
testing the behavior of readtags, must verify the command existence
with `test -x $4` before going into the main part of the test.

When comparing `tags` file with `tags-expected.txt`, you
must specify the path of `tags` explicitly with -o option
in ctags command line like::

	CTAGS=$1
	BUILDDIR=$3
	${CTAGS} ... -o $BUILDDIR/tags ...

This makes it possible to keep the original source directory clean.

See also `tmain_run` and `tmain_compare` functions in `misc/units`.

If run.sh exits with code 77, the test case is skipped.
The output to stdout is captured and printed as the reason
of skipping.

TODO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Run under valgrind


