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

For the purpose gathering as much as possible information from source
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

	A critical error is occurred. ctags aborts the execution.

*warning*

	An error is occurred but ctags continues the execution.

*verbose*

	Mainly for debugging purpose.


*notice* is a new class of message. It is less important than warning*
*but more important for users than *verbose*. Generally the user can
*ignore *notice*. With ``--quiet`` option can be used to turn off the
printing the *notice* class messages.

``--input-endocing=ENCODING`` and ``--output-endocing=ENCODING`` options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Japanese programmers sometimes use Japanese language in comments in
source code. Of course it is not limited to Japanese. People may use
their own native language in some case. In such case encoding becomes
an issue.

ctags didn't care it. ctags just reads input as just bytes sequence and
use them as is when writing tags entries.

In other hand vim cares it. When loading a file, vim converts the file
content into an internal format with one of encodings specified in
fileencodings variable.

As the result of this difference, vim cannot move the cursor to the
definition of a tag as users expect with pattern matching. ctags
writes patterns in tags file.

Good news is that there is a way to notify vim the encoding used in a
tags file with ``_TAG_FILE_ENCODING`` pseudo tag in the tag file.

This feature solves this issue utilizing ``_TAG_FILE_ENCODING``
pseudo tag.

This patch introduces two type of options (``--input-encoding=IN``
and ``--output-encoding=OUT``).

As specified encoding with these options ctags converts input from
``IN`` encoding to ``OUT`` encoding. ctags uses the converted strings
when writing pattern parts of tags lines. As the result tags output is
encoded in ``OUT`` encoding.  In addition ``OUT`` is specified in the
top tags file as value for ``_TAG_FILE_ENCODING`` pseudo tag.  As
``OUT`` utf-8 is as default.

NOTE: Converted input is NOT passed to language parsers.
The parsers still deal with input as bytes sequence.

With ``--input-encoding-<LANG>=IN``, you can specify ``LANG`` own
input encoding. It overrides the global default value given with
``--input-encoding``.

The example usage can be found in *Tmain/{input,output}-encoding-option.d*.

Acceptable ``IN`` and ``OUT`` can be listed with *iconv -l* or *iconv --list*.
It is up to platform where ctags runs.

To enable the option, libiconv is needed in your platform. In addition
``--enable-iconv`` must be specified to configure before making ctags.
On windows mingw32, you must specify ``WITH_ICONV=yes`` like below::

	C:\dev\ctags>mingw32-make -f mk_mingw.mak WITH_ICONV=yes
