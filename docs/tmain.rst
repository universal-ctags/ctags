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

See also `tmain_run` and `tmain_compare` functions in `misc/units`.

If run.sh exits with code 77, the test case is skipped.
The output to stdout is captured and printed as the reason
of skipping.

TODO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Run under valgrind


