.. _releasing:

======================================================================
How to release a new version
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

#. Update NEWS.rst

   + New parsers
   + Changes about ctags' CLI and OUTPUT
   + Changes about parsers' kinds, roles, fields, extras, and parameters

#. Update the version numbers (e.g. "5.9.0", "5,9,0") embedded in the following files:

   + ``configure.ac``

	 - AC_INIT

   + ``main/ctags.h``

	 - PROGRAM_VERSION
	 - PROGRAM_COPYRIGHT
	 - OUTPUT_VERSION_CURRENT
	 - OUTPUT_VERSION_AGE

   + ``win32/ctags.rc``

	 - FILEVERSION
	 - PRODUCTVERSION
	 - VALUE "FileVersion"
	 - VALUE "LegalCopyright"
	 - VALUE "ProductVersion"

   + ``win32/ctags.exe.manifest``

	 - assemblyIdentity

   + ``win32/config_mvc.h``

	 - PACKAGE_STRING
	 - PACKAGE_VERSION

   + ``win32/config_mingw.h``

	 - PACKAGE_STRING
	 - PACKAGE_VERSION
	 - VERSION

   + ``misc/git-tag-maybe.sh``

	 - BASE

#. Revise the version numbers in each parsers and their man pages

#. Regenerate rst files under ``docs/man``.

#. Put an annotation tag for the version and push it to ``github.com/universal-ctags/ctags``

   .. code-block:: console

				   $ git tag -a v5.9.0 -m 'Version 5.9.0'
				   $ git push upstream --tags

#. Make tarbarll

   .. code-block:: console

				   $ ./configure
				   $ make clean
				   $ make dist

   You will get a file like universal-ctags-5.9.0.tar.gz.

#. Make a release on the GitHub page

   Don't forget to upload the tar.gz file.
