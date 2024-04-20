.. _releasing:

======================================================================
How to release a new version
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

----

#. Revise misc/visit-version-info.bash, run it, verify the output

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

   + ``NEWS.rst``

	 - Changes in ...

#. Revise the version numbers in each parsers and their man pages

#. Revise the version number in writer-json.c

#. Regenerate rst files under ``docs/man``.

#. Add a entry for the new version to ``docs/news.rst``.

#. Put an annotation tag for the version and push it to ``github.com/universal-ctags/ctags``

   .. code-block:: console

				   $ git tag -a v5.9.0 -m 'Version 5.9.0'
				   $ git push upstream --tags

#. Make tarball in a freshly cloned directory

   .. code-block:: console

				   $ ./configure
				   $ make distcheck
				   $ make clean
				   $ make dist

   You will get a file like universal-ctags-5.9.0.tar.gz.

#. Make a release on the GitHub page

   Don't forget to upload the tar.gz file.
