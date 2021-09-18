.. _contributions:

======================================================================
Contributions
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

You are welcome.

Supporting many parsers with few developers is impossible.  We invite
the person who contributes a parser to u-ctags team, especially if the
target language is updated frequently. TypeScript is a typical
frequently updated language.

This is what we would like potential contributors to know. In this
section "you" means a contributor, and "we" means reviewers. "I" means
Masatake YAMATO, the author of this section.

This page gathers random notes for newly joined members.

Basic rules
---------------------------------------------------------------------

You are the maintainer of your parser, of course.

You may update your parser as you want under the rules described
later.

You may review pull requests changing your parser.

A parser exists and is maintained independently form other
parsers. However, considering the consistency between parsers are not
bad.

You can put your name to docs/developers.rst.

Read docs.ctags.io.

Before You Start
---------------------------------------------------------------------

 .. Specific to add new parser and/or new kind/role

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

There are two classes of tags. The primary class is a *definition tag*.
If a name is defined in a file, the name and the line and the file
where the name is defined should be tagged (recorded). However, in
some languages answering, "What is a definition?" is not so obvious.
You may have to decide what is tagged in your parser thoughtfully.
The purposes listed at the top of this subsection should help you
decide.

The secondary class is a *reference tag*. This is newly introduced in
Universal Ctags and is not available in Exuberant Ctags. If a name is
used (or referenced) in a file, it can be tagged as a reference tag.

Don't be confused by the two tag classes.

Defining kinds and roles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining kinds is the most important task in writing a new parser.
Once a kind is introduced, we cannot change it because it breaks
tags file compatibility.

See :ref:`tag_entries` in :ref:`ctags(1) <ctags(1)>` for more details of kinds
and roles.

Scope information and full qualified tags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Optional.
TBW.

Developing a parser
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
<https://editorconfig.org/>`_ configuration file (`.editorconfig`).
There are `plugins <https://editorconfig.org/#download>`_ available
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


.. NOT REVIEWED YET

Notes for GNU emacs users
.........................................................................

If you use GNU emacs, utilize the `.editorconfig` configuration based
on non-GNU C style. Here non-GNU C style means
"align a keyword for control flow and `{` of the block start".

GNU style:

.. code-block:: C

	if (...)
	    {
		...

non-GNU style:

.. code-block:: C

	if (...)
	{
		...

For combining the style and `.editorconfig` configuration, put
following code snippet to your .emacs:

.. code-block:: emacs

	(add-hook 'hack-local-variables-hook
		(lambda () (editorconfig-apply)))

`.dir-locals.el` in ctags source tree applies "linux" style of `cc-mode`.
Above code snippet applies the `.editorconfig` configuration AFTER
installing the "linux" style to the current buffer.

I like GNU style, but for keeping consistency in existing code of
Exuberant Ctags, the origin of Universal Ctags, I introduced the style
and configuration to my .emacs.  Please, do the same.

Compatibility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are trying to maintain compatibility with Exuberant-ctags in the
following two areas.

* Command line option
* Tag file compatibility

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

Don't remove an option, especially if it exists in Exuberant Ctags.

Writing parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two ways to write a parser, writing in C and using *optlib parser*.

Universal Ctags extends the *optlib parser* feature so extensively that it can
implement most of functions of a parser.
*optlib parser* is also suitable for prototyping.

See :ref:`ctags-optlib(7) <ctags-optlib(7)>` and :ref:`optlib` for details.
See :ref:`optlib2c` how to add a optlib parser on ``ctags``.

For writing a parser in C see :ref:`writing_parser_in_c`.

Build script
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To add your optlib parser, ``foo.ctags``, into ``ctags`` do the following steps;

* put ``foo.ctags`` file on ``optlib/`` directory
* add ``foo.ctags`` on ``OPTLIB2C_INPUT`` variable in ``source.mak``
* add ``fooParser`` on ``PARSER_LIST`` macro variable in ``main/parser_p.h``
* add ``foo`` on the list in the section "New parsers" in ``docs/news.rst``
* add ``"..\optlib\foo.c"`` in ``win32/ctags_vs2013.vcxproj``
* add ``"..\optlib\foo.c"`` in  ``win32/ctags_vs2013.vcxproj.filters``

Translated C code is also committed to our git repository. The translated code
is useful for building ctags on the platforms where optlib2c doesn't run.

To add your parser file, ``foo.c``, into ``ctags`` do the following steps;

* put ``foo.c`` file on ``parsers/`` directory
* add ``foo.c`` on ``PARSER_SRCS`` variable in ``sources.mak``
* add ``foo`` on the list in the section "New parsers" in ``docs/news.rst``
* add ``"..\parsers\foo.c"`` in ``win32/ctags_vs2013.vcxproj``
* add ``"..\parsers\foo.c"`` in  ``win32/ctags_vs2013.vcxproj.filters``

If you have GNU make, ``make -C win32`` updates the win32 files described above
from ``makefile.mak``.
Without updating win32 files our CI process run on
Appveyor will fail.

See `this pull request <https://github.com/universal-ctags/ctags/pull/2765>`_
for the `Meson` parser as an example of optlib parser.

Testing
---------------------------------------------------------------------

Add test cases, and run both existing cases and your new cases.

If you add a new parser or modify an existing parser, add new test
cases to "Units". If you modify the core, add new test cases to
"Tmain". The way to write and run test cases is described in
:ref:`testing_ctags` and :ref:`testing_parser` section of this guide.

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

Test cases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add a test case to Unit when creating or modifying a parser.

Add a test case to Tmain when modifying the core.

Add a test case to Tinst when modifying the install target in the
Makefile.

Testing your parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If possible, prepare a simple test and a complex one. The simple one
for helping us, the maintainers, understand the intent of the
modification.

If there are more than 3 test cases for a parser, a parser specific
test case directory should be prepared like `Units/parser-c.r`.

Add realistic examples for you parser to *codebase*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At `codebase <https://github.com/universal-ctags/codebase>`_, we
collect realistic examples that can be used for evaluating your parser
especially about its performance aspect. Consider contributing to the
repository when adding a new parser to Universal Ctags.

Writing Documents
---------------------------------------------------------------------

* ``man/*.rst`` files are the source files of our man pages.
  The man pages are for users. See "`How to add a new man page for your parser`_".

* ``docs/*.rst`` files explain experimental
  new features. The files are for developers. The parts of contents
  of ``docs/*.rst`` should be moved to ``man/*.rst`` in the future.

* Update ``docs/news.rst`` especially if you add a new parser.

* Write ``docs/parser-<NAME-OF-YOUR-PARSER>.rst`` as you want.
  A FAQ and the design or your parser are common topics.
  Consider the maintenance of your parser after you left the
  project for some reason.

How to add a new man page for your parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. write what the users of your parser may want to (or should) know to ``man/ctags-lang-LANGUAGE.7.rst.in``
2. add ``man/ctags-lang-LANGUAGE.7`` to ``GEN_IN_MAN_FILES`` of ``man/Makefile.am``.
3. run ``make -C man update-docs``. This step generates the rst file at ``docs/man/ctags-lang-LANGUAGE.7.rst``.
4. add ``ctags-lang-LANGUAGE(7)`` to (toctree of) ``docs/man-pages.rst``.
5. do ``git add`` for
    * ``man/ctags-lang-LANGUAGE.7.rst.in``
    * ``docs/man/ctags-lang-LANGUAGE.7.rst``
6. git commit with a log header: "``docs(man): add a man page for LANGUAGE``".
7. make a pull request

.. TODO: Why do we have to git add ``docs/man/ctags-lang-LANGUAGE.7.rst``?
	A system which uses it has Sphinx. It should be able to generate
	``docs/man/ctags-lang-LANGUAGE.7.rst``.

Committing and submitting a pull request
---------------------------------------------------------------------

* Make a pull request even if the change is small enough.

* Wait for one day till merging even if the change is small enough.

* Wait for 3 days at least for non-small change to your parser.

* Wait for 7 days at least and get an LGTM (Looks Good To Me) comment from a
  member of the team if your commit changes the other parts than your parser and
  the changes are not obvious.

* Add a test case to your pull request. To make git-bisect happy,
  don't add a test case for a feature or a bugfix before adding the
  code for the feature or the bugfix.

* Even if a pull request includes multiple commits, each commit must
  be semantically well separated. Sometimes you may want to adjust
  whitespaces in the code. Adjusting whitespaces is o.k., but don't
  mix the other change with it. Make a commit just for the whitespaces
  adjustment.

Title of commit log and pull request
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* "Misc Fixes" is allowed as far as each commit in a pull request is
  semantically well separated. Sometimes, you may fix various minor
  things randomly. Making pull requests for each of them is
  boring. You may want to make "mix fixes" pull request especially if
  your code is young.

* Use [WIP] (Work In Progress) prefix as the title of your pull request, if you don't
  want people to take time for reviewing your code. Removing [WIP]
  implies "ready to be reviewed."

* Use [FYI] (For Your Information) prefix as the title to show your idea or sketch represented
  in C language.

* Use the name of your parser as the prefix of a commit log.

  .. code-block:: git

        C++: record template type parameters to detect the end of template prefix

        If we know Foo is a name of type, it becomes easier to detect whether
        ">>" in "Foo>>" is a shift operator or the end marker of the template
        prefix.

  In the above example, "C++: " is the prefix.

* Use the name of your parser as the prefix of a pull request if your
  change is about a parser.

* Use following prefixes for the changes other than parsers.

  main:
    Changes for files under ``main/`` directory

  Units:
    Changes for the test cases under ``Units/`` directory

  Tmain
    Changes for the test cases under ``Tmain/`` directory

  docs(web)
    Changes for the ``docs/*.rst``

  docs(man)
    Changes for the ``man/*.rst``

  See also the output of ``git log`` command.

* Combine prefixes with a comma if a change modifies multiple parts of our source tree

  Here is an example.

  .. code-block:: git


        commit 64a05963c108af4b7832a2215006ff5cafcaaebb
        Author: Masatake YAMATO <yamato@redhat.com>
        Date:   Tue Mar 19 12:19:37 2019 +0900

        main,Flex,JavaScript,SQL,refactor: introduce a helper function to skip two character sequence

        ...

* Use following prefixes if the change as no run-time impact.

  cosmetic
    - Remove whitespaces at the end of lines
    - Adjust indentation
    - Remove an empty line
    - ...

  style
    - Rename symbol names
    - ...

  refactor
    - Code transformation that doesn't intent changing run-time behavior

  These prefixes reduce the load of reviewers.

* Use [INCOMPATIBLE] as a prefix for both pull request and commit log
  if the change breaks the compatibility with Exuberant Ctags. Write
  an explanation in ``man/ctags-incompatibilities.7.rst.in`` about the
  detail of breakage.

* Use [SELF-INCOMPATIBLE] as a prefix for both pull request and commit
  log if the change breaks the compatibility with Universal Ctags
  itself.

Commit log
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(For new parsers the following criteria is not applicable.)

Make clear the original motivation for the change and/or the impact
on the tags file.

If you fix a bug reported somewhere on the web, its URL should be
logged, too.

If the bug is reported in the Exuberant Ctags tracker on the
SourceForge web site, log it as ``sf-bugs:N``, ``sf-patches:N``,
``sf-support-requests:N``, or ``sf-feature-requests:N``.
``docs/tracking.rst`` also should be updated.

Squashing commits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you submit a pull request you might receive some comments from a
reviewer and, in response, update your patches. After updating, we
would like you to squash your patches into logical units of work
before we merge them to keep the repository history as simple as
possible.

* Use ``git rebase -i`` and ``git push --force`` to refine your change in
  the meaning of "semantically well separated."  "semantically well
  separated" is important than "recording the history of your try and
  error."

Quoted from @steveno in `#393
<https://github.com/universal-ctags/ctags/issues/393>`_ :

    You can check out this page for a good example of how to squash
    commits
    http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html

    Once you've squashed all your commits, simply do a git push -f to
    your fork, and GitHub will update the pull request for you
    automatically.

Rules for reviewing a pull request
---------------------------------------------------------------------

* Put your rough schedule as a comment if you don't have time, but you
  want to review.

Testing a PR locally before being merged
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may want to test a PR locally before it is merged into the master
repository.

If you want to test a PR #2234 on a repository `upstream` as branch name
`BRANCHNAME`;::

  $ git fetch upstream pull/2234/head:BRANCHNAME
  $ git checkout BRANCHNAME

This creates a branch `BRANCHNAME`, and pulls the PR into the branch, and
switches to the branch. The branch name `BRANCHNAME` does not have to be the
same as the branch name of the PR.

Alternatively suppose you want to test ``USERNAME``'s PR with branch
name ``main-fix-foo``;::

  git checkout -b tmp-main-fix-foo master
  git pull https://github.com/USERNAME/ctags.git main-fix-foo

This creates a branch ``tmp-main-fix-fix-foo`` from a branch ``master`` and
switches to it, then pulls the branch ``main-fix-foo`` from
``https://github.com/USERNAME/ctags.git``.
