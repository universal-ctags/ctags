.. _releasing:

======================================================================
How to release a new version
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

#. Update the version numbers (e.g. "5.9.0", "5,9,0") embedded in the following files:

   + ``configure.ac``
   + ``main/ctags.h``
   + ``win32/ctags.rc``
   + ``win32/ctags.exe.manifest``
   + ``win32/config_mvc.h``
   + ``win32/config_mingw.h``
   + ``misc/git-tag-maybe.sh``

#. Regenerate rst files under ``docs/man``.

#. Put a version tag and push it to ``github.com/universal-ctags/ctags``

   .. code-block:: console

				   $ git tag v5.9.0
				   $ git push upstream --tags

#. Make tarbarll

   .. code-block:: console

				   $ ./configure
				   $ make clean
				   $ make dist

   You will get a file like universal-ctags-5.9.0.tar.gz.

#. Make a release on the GitHub page

   Don't forget to upload the tar.gz file.
