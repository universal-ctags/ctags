======================================================================
The development guideline
======================================================================

Supporting many parsers with few developers is impossible.  We invite
the person who contributes a parser to u-ctags team, especially if the
target language is updated frequently. TypeScript is a typical
frequently updated language.

This page gathers random notes for newly joined members.


Common topics
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


Rules for reviewing a pull request
---------------------------------------------------------------------

* Put your rough schedule as a comment if you don't have time, but you
  want to review.


Rules for modifying our repository
---------------------------------------------------------------------

* Make a pull request even if the change is small enough.

* Wait for one day till merging even if the change is small enough.

* Wait for 3 days at least for non-small change to your parser.

* Wait for 7 days at least and get an LTGM comment from a member of the
  team if your commit changes the other parts than your parser.

* Add a test case to your pull request. To make git-bisect happy,
  don't add a test case for a feature or a bugfix before adding the
  code for the feature or the bugfix.

* Even if a pull request includes multiple commits, each commit must
  be semantically well separated. Sometimes you may want to adjust
  whitespaces in the code. Adjusting whitespaces is o.k., but don't
  mix the other change with it. Make a commit just for the whitespaces
  adjustment.

* "Misc Fixes" is allowed as far as each commit in a pull request is
  semantically well separated. Sometimes, you may fix various minor
  things randomly. Making pull requests for each of them is
  boring. You may want to make "mix fixes" pull request especially if
  your code is young.

* Use [WIP] prefix as the title of your pull request, if you don't
  want people to take time for reviewing your code. Removing [WIP]
  implies "ready to be reviewed."

* Use [FYI] prefix as the title to show your idea or sketch represented
  in C language.

* Use the name of your parser as the prefix of a pull request if your
  change is about a parser.

* Use the name of your parser as the prefix of a commit log.

  .. code-block:: git


        C++: record template type parameters to detect the end of template prefix


        If we know Foo is a name of type, it becomes easier to detect whether
        ">>" in "Foo>>" is a shift operator or the end marker of the template
        prefix.


   In the above example, "C++: " is the prefix.


* Use following prefixes for the changes other than parsers.

  main:

    Changes for files under main/ directory

  Units:

    Changes for the test cases under Units directory

  Tmain

    Changes for the test cases under Tmain directory

  docs(web)

    Changes for the docs/\*.rst.

  docs(man)

    Changes for the man/\*.rst.

  See also the output of git-log.


* Combine prefixes with a comma if a change modifies multiple parts of our source tree

  Here is an example.

  .. code-block:: git


        commit 64a05963c108af4b7832a2215006ff5cafcaaebb
        Author: Masatake YAMATO <yamato@redhat.com>
        Date:   Tue Mar 19 12:19:37 2019 +0900

        main,Flex,JavaScriupt,SQL,refactor: introduce a helper function to skip two character sequence

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

    Code transformation that doesn't intent changing run-time behavior

  These prefixes reduce the load of reviewers.

* Use "git rebase -i" and "git push --force" to refine your change in
  the meaning of "semantically well separated."  "semantically well
  separated" is important than "recording the history of your try and
  error."

* Use [INCOMPATIBLE] as a prefix for both pull request and commit log
  if the change breaks the compatibility with Exuberant-ctags. Write
  an explanation in man/ctags-incompatibilities.7.rst.in about the
  detail of breakage.

* Use [SELF-INCOMPATIBLE] as a prefix for both pull request and commit
  log if the change breaks the compatibility with Universal-ctags
  itself.

About documentation
---------------------------------------------------------------------

* Update documents. man/\*.rst files are the source files of our man pages.
  The man pages are for users. docs/\*.rst files explain experimental
  new features. The files are for developers. The parts of contents
  of docs/\*.rst should be moved to man/\*.rst in the future.

* Write docs/parser-<NAME-OF-YOUR-PARSER>.rst as you want.
  A FAQ and the design or your parser are common topics.
  Consider the maintenance of your parser after you left the
  project for some reason.
