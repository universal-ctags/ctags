======================================================================
Contributions
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

You are welcome.


These are what we would like potential contributors to know.  In this
section "you" means a contributor, and "we" means reviewers. "I" means
Masatake YAMATO, the author of this section.


General topics
---------------------------------------------------------------------

Origin of changes and license
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Make clear where the patches come from and who wrote them.

If you backport patches from Geany or somewhere other projects, their
commit ids should be logged, too.

Put copyright notice when adding a new ``{parsers,main}/*.[ch]`` file.

About new file, license notice is needed to at the head of source
file.

We expect your change (or new code) is provided under term of General
Public License version 2 or any later version. We would like you to
express "version 2 or any later version".

Commit log
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(About a new parser following criteria is not applicable.)

Make clear the original motivation and/or impact on tags file.
If you fix a bug reported somewhere on the web, its URL should
logged, too.

If the bug is reported on the tracker of sourceforge web site
of exuberant ctags project, log it as sf-bugs:N, sf-patches:N,
sf-support-requests:N, or sf-feature-requests:N.
docs/tracking.rst also should be updated.

NEWS file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Update docs/news.rst especially if you add a new parser.


Testing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add test cases, and run both existing cases and your new cases.

If you add a new parser or modify existing parser, add new test cases
to "Units". If you modify main part, add new test cases to
"Tmain". The way to write and run test cases are written in "Testing
ctags" of this document. Other than tmain test harness, you can
specify VG=1 for running test cases under valgrind memory debugger.

A parse should not enter an infinite loop for bad input.
A parse should not crash for bad input.
A parse should return the control to its caller for bad input.

Write what kind of tests are passed in a commit log.
e.g. ::

  make units LANGUAGES=TTCN VG=1 is passed.
  make fuzz LANGUAGES=TTCN VG=1  is passed.
  make chop LANGUAGES=TTCN VG=1  is passed.  


C language
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Don't forget putting `static` modifiers. Don't introduce unnecessary
global variables.

Remove unused variables and types. If you want to keep them in your
source code, put enough description as comments.

Use available facility that ctags main part provides.  If the
facility is not enough for writing a parser, consider extending the
main part first.

Use underscores in names only in file scope objects.
Don't use it in function declarations, variable declarations
and macro names in header files.

Trim unnecessary white space at the end of line. In emacs `M-x
whitespace-cleanup`. Don't mix `whitespace cleanup` fix and the other
improvements into one commit when changing the existing code. Style,
including `whitespace cleanup` should be in a separate commit.
Mixing makes reviewing harder.

If possible, don't use file static variable. Find the
alternative way to use parameters.

About indentation I'm using linux style in cc-mode of GNU Emacs.

Command line options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Don't introduce `--<LANG>-foo=...` style option. It will be
unsuitable for zsh/bash completion engine. Instead, introduce
`--foo-<LANG>=...` style option

Write docs/news.rst if you change the behavior of an option or
introduce a new option. If you think the option is stable enough,
write it to ctags.1.in, too.

Use underscore as prefix for experimental option. Once an option is
introduced, it must be maintained.  We don't want to remove it later.
If you are not sure the usefulness of the option, use an underscore at
the start of long option like: `--_echo`.

Write a test case for Tmain or Units.

Don't remove an option, especially if it exits in exuberant
ctags. We want to keep compatibility as much as possible.


Test cases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write a test case of Unit for a parser part modification.

Write a test case of Tmain for a main part modification.

Write a test case of Tinst for modification of install target of
Makefile.
  
Compatibility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are trying to keep compatibility in following two aspects.

Tag file compatibility with exuberant ctags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We will not accept a patch that breaks the format of tags described in
"Proposal for extended Vi tags file format" a.k.a. FORMAT file.

TBW.

Command line option with exuberant ctags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TBW.


Specific to add new parser and/or new kind/role
---------------------------------------------------------------------

When working on ctags I take into following purposes of tags into
account:

1. inserting the name with completion,
2. jumping to definition of given name (on an editor or something tool),
3. navigating source code tree,
4. summarizing source code tree, and
5. answering to a query about source code tree.

When I review a new parser code, I expect the parser contributing to
the above purposes.


What should be tagged?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two classes of tags. The primary class is definition tag.
If a name is defined in a file, the name and the line and the file
where the name is defined should be tagged (recorded). However, in
some language, "What is definition" is not so obvious. You may have
to decide what are tagged in your parser thoughtfully. The purposes
listed at the top of this subsection will help you make the decision.

The secondary class is reference tag. This is newly introduced in
Universal-ctags, and is not in Exuberant-ctags. If a name is used
(or referenced) in a file, it can be tagged as a reference tag.

Don't be confused with above two.


Defining kinds and roles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining kinds is the most important task in writing a new parser.
One a kind is introduced, we cannot change because it breaks
tags file compatibility.

If you are not interested in designing kinds because you are
emacs user and use just TAGS output, there are two choices:
TBW.

Scope information and full qualified tags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Optional.
TBW.

Adding a new field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TBW.


Reference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the comment at the head of your source file, put a URL for a web
page that explains the language your parser deals with. Especially if
the language is not popular enough.

Here is an example.

.. code-block:: C

    /*
    *
    *   Copyright (c) 2016, Masatake YAMATO
    *   Copyright (c) 2016, Red Hat, K.K.
    *
    *   This source code is released for free distribution under the terms of the
    *   GNU General Public License version 2 or (at your option) any later version.
    *
    *   This module contains functions for generating tags for property list defined
    *   in http://www.apple.com/DTDs/PropertyList-1.0.dtd.
    */
		
Testing your parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If possible, prepare simple one and complex one. The simple one for
making us, the maintainers understanding the intent of modification.

If there are more than 3 test cases for a parser, parser own
test case directory should be prepared like `Units/parser-c.r`.


Writing parser in regex
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You can write a parser with regex patterns.

`optlib2c`, a part of Universal-ctags build system can translate a
parser written in regex patterns into C source code.

`man` parser is one of example. It is written in regex patterns.
See the output of following command line:

	`git show  0a9e78a8a40e8595b3899e2ad249c8f2c3819c8a^..89aa548`

Translated C code is also put into our git repository. The translated
code is useful for building ctags on the platform where optlib2c doesn't
run.

regex is also suitable for prototyping.

Squashing commits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is about .c file of your parser.
You will go some comments from reviewer; and update your patches.
After updating, we want you to squash your patches for a parser
before we merge them to make the our history of repository simple.

Quoted from @steveno in #393:

    You can check out this page for a good example of how to squash
    commits
    http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html

    Once you've squashed all your commits, simply do a git push -f to
    your fork, and github will update the pull request for you
    automatically.

Build script
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add your `.c` file to somewhere in `source.mak`.

In addition update, `win32/ctags_vs2013.vcxproj` and
`win32/ctags_vs2013.vcxproj.filters`. So our CI process
run on Appveyor will be failed.
