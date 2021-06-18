.. _testing_readtags:

=============================================================================
Testing readtags
=============================================================================

*roundtrip* target verifies the behavior of readtags command.  It
reuses `expected.tags` files under Units as input files for readtags.
All tags in all `expected.tags` files are searched with readtags.  If a
tag cannot be found with readtags, the roundtrip target reports the readtags
command line that specifies the tag and the path for `expected.tags` file.

Example of running the target

.. code-block:: console

    $ make roundtrip
    FAILED: ./readtags -t Units/sh-statements.d/expected.tags -starting-with-dash
    FAILED: ./readtags -t Units/sh-statements.d/expected.tags -starting-with-dash_2
    FAILED: ./readtags -t Units/sh-statements.d/expected.tags -starting-with-dash_inner
    FAILED: ./readtags -t Units/parser-c.r/backslash-at-the-end-of-pattern.d/expected.tags M
    FAILED: ./readtags -t Units/simple-diff.d/expected.tags -0,0
    FAILED: ./readtags -t Units/simple-diff.d/expected.tags -1,2
    FAILED: ./readtags -t Units/simple-diff.d/expected.tags -28,6
    FAILED: ./readtags -t Units/simple-diff.d/expected.tags -44,6
    FAILED: ./readtags -t Units/review-needed.r/3526726.tex.t/expected.tags -r
	...
    Makefile:498: recipe for target 'roundtrip' failed
    make: *** [roundtrip] Error 1


On CI/CD environments, the target uses only marked `expected.tags`
files instead of using all the `expected.tags` files because the
roundtrip target took too long time.

An empty file named `minitrip` acts as the marker. Before processing
an `expected.tags`, the target looks for `minitrip` file at the
directory where `expected.tags` is. If it finds `minitrip` file, the
target processes the `expected.tags`. If not, the target ignores the
`expected.tags`.
