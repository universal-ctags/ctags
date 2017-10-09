.. _cspell:

*Cspell* spell checking
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

-----

* ``make cspell`` reports unknown words.
  After verifying the reported words are correct you should
  add them to files under *dictfiles* directory.

* ``cspell`` target assumes the names used in ctags source code are
  correctly spelled. Such names can be added semi-automatically; use
  ``make dicts`` targets. It updates files prefixed with *GENERATED-*
  under *dictfiles*.

* Either semi-automatically generated or adding by manually, files
  under *dictfiles* directory should be installed to Universal-ctags
  git repository.

* ``make cspell`` makes and users *SPELL_CHECKING.TMP* at the top of source
  code directory as temporary working space.

* ``cspell`` target depends on GNU aspell library. If the library is linked
  to, ``ctags --list-features`` prints ``aspell``.

An example session:

.. code-block:: console

    $ make cspell
    ./misc/gen-repoinfo > main/repoinfo.h
      CC       main/ctags-repoinfo.o
      CCLD     ctags
    /bin/sh misc/cspell
    checking bda36b226cfc0f492908bc5779268c353e615623...unknown words
	    orignal
	    dictfile
    checking 61a71ef37df9fd9ebb0d3e8a941effd643662cda...ok
    checking b08c956e155d47a8e689cfede2501ab48890c889...ok
    checking c07a6e94140e1d5cfeaf3cb42d2fc28bd0e92e51...ok
    ...
    checking 201e5e774fef84527802113969998bd71b14466d...ok
    Makefile:6510: recipe for target 'cspell' failed
    make: *** [cspell] Error 1

Here cspell reports "orignal" and "dictfile" as unknown words.

"orignal" should be "original". So you should make a fixup commit
for bda36b with "git commit --fixup=bda36b". Then you may want to
do "git rebase -i --autosquash master".

"dictfile" may be a name used in source code files. Ideally
``make dicts`` picks up the name and puts to one of *GENERATED-*
dictionaries. However, it is not implemented yet. What you can
do now is adding it to one of dictionaries under *dictfiles*
directory. After do "git add" the directory file and make
a fixup commit.

There are cases that you want to add a misspelled word intentionally
to source tree: to test cases(Units and Tmain) and to documentations.

About test cases, make a file named *dictfile* under the directory of
target test case, and put the words line by line. You can find an
example in *Units/simple-ctags-aspell.d/dictfile* of Universal-ctags
source tree.

For documentations, there is no good way. Suggestions are welcome.
"CSPELL:" prefix line is a temporary solution.  A line starting from
"CSPELL:" in a commit log is treated specially by ``make cspell`` when
spell-checking the commit; whitespace separated words in the line are
added to a temporary dictionary.

An example

.. code-block:: git-commit

	commit 8efb57fa1c9d7b9b7ba01f49963d7d7779609f21
	Author: Masatake YAMATO <yamato@redhat.com>
	Date:   Mon Jun 5 23:08:51 2017 +0900

	docs(man): fix styles of definition list

	CSPELL: xno xyes

	Signed-off-by: Masatake YAMATO <yamato@redhat.com>

Here "xno" and "xyes" are added to a dictionary temporary used during
spell-checking the commit, "8efb57"; "xno" and "xyes" are never
reported as unknown words. The temporary dictionary is used only for
this commit.
