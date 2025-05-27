======================================================================
Changes in 6.?.0
======================================================================

New and extended options and their flags
---------------------------------------------------------------------

``--list-output-formats`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See :ref:`option_listing` in :ref:`ctags(1) <ctags(1)>`.

``nulltag``/``z`` extra
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Universal Ctags now supports tags (*null tags*) having empty strings as their names.
See :ref:`extras` in :ref:`ctags(1) <ctags(1)>`.

.. note::

   * ``libreadtags`` and ``readtags`` do not support the null tags yet.
   * ``json`` and ``xref`` output formats support the null tags.

Incompatible changes
---------------------------------------------------------------------

* [readtags] make -Q,--filter not work on ptags when -P,--with-pseudo-tags is specified together

  With this version, ``-Q,--filter`` option doesn't affect the pseudo tags listed
  with ``-P,--with-pseudo-tags`` option.  ``-Q,--filter`` option specified wth
  ``-P,--with-pseudo-tags`` option affect only regular tags.

  To extract speicifed pseudo tags, use ``-Q,--filter`` option with
  ``-D,--list-pseudo`` action.

Parser related changes
---------------------------------------------------------------------

#4026
   Integrate `pegof <https://github.com/dolik-rce/pegof>`_ to our build process.

New parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* SELinuxIntefae *M4 based subparser*
* SELinuxTypeEnforcement *optlib*
* PythonEntryPoints *subparser*
* Scdoc *optlib*
* JNI *subparser*
* TypeSpec *parser*

.. note:: We added a TOML as a new parser in this version. However,
		  after adding it, we learned its implementation didn't work
		  entirely. So we deleted the TOML parser, and Cargo subparser
		  runs on the TOML parser from this "New parsers" list.
		  See `TOML: infinite loop <https://github.com/universal-ctags/ctags/issues/4096>`__
		  about how it doesn't work.

Changes about parser specific kinds, roles, fields, and extras
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Readtags
---------------------------------------------------------------------

* make formatter work with -D,--list-pseudo-tags option

  An example extracting the value of ``!_TAG_PROC_CWD``:

  .. code-block:: console

	 $ ./readtags -t podman.tags -Q '(#/.*CWD.*/ $name)' -F '(list $input #t)' -D
	 /home/yamato/var/ctags-github/

* make -Q,--filter not work on ptags when -P,--with-pseudo-tags is specified together

Merged pull requests
---------------------------------------------------------------------

.. note::

   This list is imperfect. masatake cleaned up some pull requests before
   merging. Though his names is used in "... by ...", his is not the
   primary contributor of the pull requests. See git log for more
   defatils.

Issues close or partially closed via above pull requests
---------------------------------------------------------------------
