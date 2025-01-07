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

Parser related changes
---------------------------------------------------------------------

#4026
   Integrate `pegof <https://github.com/dolik-rce/pegof>`_ to our build process.

New parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* TOML *peg/packcc*
* Cargo *TOML based subparser*
* SELinuxIntefae *M4 based subparser*

Changes about parser specific kinds, roles, fields, and extras
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Readtags
---------------------------------------------------------------------

Merged pull requests
---------------------------------------------------------------------

.. note::

   This list is imperfect. masatake cleaned up some pull requests before
   merging. Though his names is used in "... by ...", his is not the
   primary contributor of the pull requests. See git log for more
   defatils.

Issues close or partially closed via above pull requests
---------------------------------------------------------------------
