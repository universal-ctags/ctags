Criteria in patch accepting
=============================================================================

These are what we would like potential contributors to know.

Origin of changes
---------------------------------------------------------------------

Make clear where the patches come from. If you backport patches from
Geany or somewhere other projects, their commit ids should be
logged, too.

Put copyright notice when adding a new ``{parsers,main}/*.[ch]`` file.

Commit log
---------------------------------------------------------------------

Make clear the original motivation and/or impact on tags file.
If you fix a bug reported somewhere on the web, its URL should
logged, too.
If the bug is reported on the tracker of sourceforge web site
of exuberant ctags project, log it as sf-bugs:N, sf-patches:N,
sf-support-requests:N, or sf-feature-requests:N.
docs/tracking.rst also should be updated.

Testing
---------------------------------------------------------------------

If you add a new parser of change a existing parser, run fuzz and
noise test. We want you to run noise with VG=1 if you have time.

Write commit log what kind of tests did you run.

Output
---------------------------------------------------------------------

We will not accept a patch that breaks the format of tags described in
"Proposal for extended Vi tags file format" a.k.a. FORMAT file.

Adding new parser and/or new kind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What should be tagged?
......................................................................

Two classes are considered. The primary class is definition.
If something name is defined in a file, the name and the line and file
where the name is defined should be tagged(recorded.)

TBW.

Disabling/enabling a existing kind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TBW.

Adding a new field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TBW.

Command line options
---------------------------------------------------------------------

Don't introduce --<LANG>-foo=... style option. It will be
unsuitable for zsh/bash completion engine.

Write docs/news.rst if you change the behavior of an option
or introduce a new option.

Write a test case for Tmain or Units.

Don't remove an option, especially if it exits in exuberant
ctags. We want to keep compatibility as much as possible.

Use underscore as prefix for experimental option.
  
Once an option is introduced, it must be maintained.
We don't want to remove it later.
If you are not sure the usefulness of the option, use an underscore
at the start of long option like: `--_echo`.
  
Coding style
---------------------------------------------------------------------

Use underscores in names only in file scope objects.
Don't use it in function declarations, variable declarations
and macro names in header files.

Trim unnecessary white space at the end of line.

If possible, don't use file static variable. Find the
alternative way  to use parameters.

Test cases
---------------------------------------------------------------------

Write a test case of Unit for a parser part modification.

Write a test case of Tmain for a main part modification.

Write a test case of Tinst for modification of install target of
Makefile.
  
If possible, prepare simple one and complex one. The simple one
for making us, the maintainers understanding the intent of
modification.

Squashing commits
---------------------------------------------------------------------

You will go some comments from reviewer; and update your patches.
After updating, we want you to squash your patches before we
merging them to make the our history of repository simple.

Quoted from @steveno in #393:

    You can check out this page for a good example of how to squash
    commits
    http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html

    Once you've squashed all your commits, simply do a git push -f to
    your fork, and github will update the pull request for you
    automatically.
