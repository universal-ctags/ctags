======================================================================
Contributions
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

You are welcome.


This is what we would like potential contributors to know. In this
section "you" means a contributor, and "we" means reviewers. "I" means
Masatake YAMATO, the author of this section.


General topics
---------------------------------------------------------------------

Origin of changes and license
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Make clear where the patches come from and who wrote them.

If you backport patches from Geany or some other project, their
commit IDs should be logged, too.

Include a copyright notice when adding a new
``{parsers,main}/*.[ch]`` file.

A new file also requires a license notice at the head of the file.

We expect your change (or new code) to be provided under the terms of
the General Public License version 2 or any later version. We would
like you to express "version 2 or any later version".

Commit log
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(For new parsers the following criteria is not applicable.)

Make clear the original motivation for the change and/or the impact
on the tags file.

If you fix a bug reported somewhere on the web, its URL should be
logged, too.

If the bug is reported in the Exuberant-ctags tracker on the
SourceForge web site, log it as sf-bugs:N, sf-patches:N,
sf-support-requests:N, or sf-feature-requests:N.
docs/tracking.rst also should be updated.

NEWS file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Update docs/news.rst especially if you add a new parser.


Testing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add test cases, and run both existing cases and your new cases.

If you add a new parser or modify an existing parser, add new test
cases to "Units". If you modify the core, add new test cases to
"Tmain". The way to write and run test cases is described in the
"Testing ctags" section of this guide.

With the exception of the tmain test harness, you can specify VG=1
for running test cases under the Valgrind memory debugger.

A parse should not enter an infinite loop for bad input.
A parse should not crash for bad input.
A parse should return control to its caller for bad input.

Describe what kind of tests are passed in the commit message.
e.g. ::

  make units LANGUAGES=TTCN VG=1 is passed.
  make fuzz LANGUAGES=TTCN VG=1  is passed.
  make chop LANGUAGES=TTCN VG=1  is passed.  


C language
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Don't forget to use `static` modifiers. Don't introduce unnecessary
global variables.

Remove unused variables and types. If you want to keep them in your
source code, include a descriptive comment.

Use the available facilities provided by the ctags core. If the
facilities are not enough for writing a parser, consider extending
the core first.

Use underscores in names only in file scope objects.
Don't use them in function declarations, variable declarations or
macro names in header files.

Basic whitespace settings are specified in the `EditorConfig
<http://editorconfig.org/>`_ configuration file (`.editorconfig`).
There are `plugins <http://editorconfig.org/#download>`_ available
for most popular editors to automatically configure these settings.

Style guidelines are largely captured in the `Uncrustify
<http://uncrustify.sourceforge.net/>`_ configuration file
(`.uncrustify.cfg`). Formatting can be checked with:

.. code-block:: console

    $ uncrustify -c .uncrustify.cfg -f parsers/awk.c | diff -u parsers/awk.c -

Don't mix `whitespace cleanup` fixes and other improvements in one
commit when changing the existing code. Style fixes, including
`whitespace cleanup`, should be in a separate commit. Mixing
functional changes with style fixes makes reviewing harder.

If possible, don't use file static variables. Find an alternative way
that uses parameters.

For indentation I'm using the linux style in cc-mode of GNU Emacs.

Command line options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Don't introduce `--<LANG>-foo=...` style options. They are less
suitable for command-line completion by the zsh/bash completion
engines. Instead, introduce `--foo-<LANG>=...` style options.

Add an entry to docs/news.rst if you change the behavior of an option
or introduce a new option. If you think the option is stable enough,
add it to ctags.1.in, too.

Use underscore as a prefix for experimental options. Once an option
is introduced, it must be maintained. We don't want to remove it
later. If you are not sure of the usefulness of the option, use an
underscore at the start of a long option name like: `--_echo`.

Write a test case for Tmain or Units.

Don't remove an option, especially if it exists in Exuberant-ctags.
We want to maintain compatibility as much as possible.


Test cases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add a test case to Unit when creating or modifying a parser.

Add a test case to Tmain when modifying the core.

Add a test case to Tinst when modifying the install target in the
Makefile.
  
Compatibility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are trying to maintain compatibility with Exuberant-ctags in the
following two areas.

Tag file compatibility with Exuberant-ctags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We will not accept a patch that breaks the tags file format described
in "Proposal for extended Vi tags file format" a.k.a. FORMAT file.

TBW.

Command line option with Exuberant-ctags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TBW.


Specific to add new parser and/or new kind/role
---------------------------------------------------------------------

When working on ctags I take into account the following uses for
tags:

1. inserting the name with completion,
2. jumping to the definition of the name (in an editor or similar tool),
3. navigating the source code tree,
4. summarizing the source code tree, and
5. answering a query about the source code tree.

When I review new parser code, I expect the parser to contribute to
these purposes.


What should be tagged?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two classes of tags. The primary class is a definition tag.
If a name is defined in a file, the name and the line and the file
where the name is defined should be tagged (recorded). However, in
some languages answering, "What is a definition?" is not so obvious.
You may have to decide what is tagged in your parser thoughtfully.
The purposes listed at the top of this subsection should help you
decide.

The secondary class is a reference tag. This is newly introduced in
Universal-ctags and is not available in Exuberant-ctags. If a name is
used (or referenced) in a file, it can be tagged as a reference tag.

Don't be confused by the two tag classes.


Defining kinds and roles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining kinds is the most important task in writing a new parser.
Once a kind is introduced, we cannot change it because it breaks
tags file compatibility.

If you are not interested in designing kinds because you are an
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

In the comment at the head of your source file, include a URL for a
web page that explains the language your parser deals with.
Especially if the language is not well known.

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

If possible, prepare a simple test and a complex one. The simple one
for helping us, the maintainers, understand the intent of the
modification.

If there are more than 3 test cases for a parser, a parser specific
test case directory should be prepared like `Units/parser-c.r`.


Writing parser in regex
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You can write a parser with regex patterns.

`optlib2c`, a part of the Universal-ctags build system can translate
a parser written in regex patterns into C source code.

The `man` parser is one example described in regex patterns.
See the output of the following command line for details:

	`git show  0a9e78a8a40e8595b3899e2ad249c8f2c3819c8a^..89aa548`

Translated C code is also committed to our git repository. The
translated code is useful for building ctags on the platforms where
optlib2c doesn't run.

The regex approach is also suitable for prototyping.

Squashing commits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you submit a pull request you might receive some comments from a
reviewer and, in response, update your patches. After updating, we
would like you to squash your patches into logical units of work
before we merge them to keep the repository history as simple as
possible.

Quoted from @steveno in #393:

    You can check out this page for a good example of how to squash
    commits
    http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html

    Once you've squashed all your commits, simply do a git push -f to
    your fork, and GitHub will update the pull request for you
    automatically.

Build script
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add your `.c` file to `source.mak`.

In addition, update `win32/ctags_vs2013.vcxproj` and
`win32/ctags_vs2013.vcxproj.filters`. Otherwise our CI process run on
Appveyor will fail.
