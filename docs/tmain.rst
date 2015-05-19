*Tmain*: a facility for testing main part
============================================================

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

1. run `run.sh` with `bash`,
2. capture stdout, stderr and exit status, and
3. compare them with `stdout-expected.txt`, `stderr-expected.txt`,
   and `exit-expected.txt`.

See also `tmain_run` and `tmain_compare` in `misc/units`.


TODO:

* Prepare the way to test the installation.
* Run under travis.

