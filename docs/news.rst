Introduced changes
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

Many changes have been introduced in universal-ctags. Use git-log to
review changes not enumerated here, especially in language parsers.

Importing most of the changes from exuberant-ctags
---------------------------------------------------------------------
See "exuberant-ctags" in "Tracking other projects" about the status of
importing. Some changes in Fedora and Debian are also imported.

New parsers
---------------------------------------------------------------------
The following parsers have been added:

* ada
* clojure
* coffee *xcmd*
* css
* d
* ctags option library *optlib*
* falcon
* go
* json
* m4 *optlib*
* mib *optlib*
* rust
* windres
* SystemVerilog

See "Option library" about  *optlib*.
See "External parser command" about *xcmd*.


Heavily improved language parsers
---------------------------------------------------------------------
* php
* verilog

Miscellaneous new options
---------------------------------------------------------------------

Tagging #undef
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``--undef[=yes|no]``
    Allows disabling the generation of macro tags from ``#undef``
    directives.

Wildcard in options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the purpose gathering as mach as possible information from source
code "wildcard"(``*``) in option is introduced.

``--fields=*``

	Enables all available fields.

``--<LANG>-kinds=*``

	Enables all available kinds for ``LANG``.

``--kinds-<LANG>=*``

	Alternative representation of ``--<LANG>-kinds=*``.

``--*-kinds=SPEC``

	Applies SPEC as kinds to all available language parsers.

``--*-kinds=*``

	Enables all available kinds to all available language parsers.

Notice message and ``--quiet`` option
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There were 3 classes of message in ctags:

*fatal*

	A ciritical error is occured. ctags aborts the execution.

*warning*

	An error is occured but ctags continues the execution.

*verbose*

	Mainly for debugging prupose.


*notice* is a new class of message. It is less important than warning*
*but more important for users than *verbose*. Generally the user can
*ignore *notice*. With ``--quiet`` option can be used to turn off the
priting the *notice* class messages.
