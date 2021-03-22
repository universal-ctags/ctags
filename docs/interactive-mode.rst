.. _interactive-mode:

======================================================================
Interactive mode
======================================================================

Universal Ctags can be run with ``--_interactive``, which enters a REPL that
can be used programmatically to control ctags generation. In this mode, json
commands are received over stdin, and corresponding responses are emitted over
stdout.

This feature needs ctags to be built with json support and this requires libjansson to be installed
at build-time. If it's supported it will be listed in the output of ``--list-features``:

.. code-block:: console

	$ ctags --list-features | grep json
	json

Communication with Universal Ctags over stdio uses the `json lines`_ format, where each
json object appears on a single line and is terminated with a newline.

When ``ctags --_interactive`` is invoked, it will emit a single json object to stdout announcing
its name and version. This signals the start of the interactive loop, and the user can begin sending
commands over stdin.

.. code-block:: console

    $ ctags --_interactive
    {"_type": "program", "name": "Universal Ctags", "version": "0.0.0"}

The following commands are currently supported in interactive mode:

- generate-tags_

generate-tags
-------------

The ``generate-tags`` command takes two arguments:

- ``filename``: name of the file to generate tags for (required)
- ``size``: size in bytes of the file, if the contents will be received over stdin (optional)

The simplest way to generate tags for a file is by passing its path on filesystem(``file request``). The response will include
one json object per line representing each tag, followed by a single json object with the ``completed``
field emitted once the file has been fully processed.

.. code-block:: console

    $ echo '{"command":"generate-tags", "filename":"test.rb"}' | ctags --_interactive
    {"_type": "program", "name": "Universal Ctags", "version": "0.0.0"}
    {"_type": "tag", "name": "foobar", "path": "test.rb", "pattern": "/^  def foobar$/", "kind": "method", "scope": "Test", "scopeKind": "class"}
    {"_type":"completed", "command": "generate-tags"}

The ``generate-tags`` command can also be used to generate tags for code which is not present on filesystem(``inline request``). For example,
an IDE might want to generate ctags for an unsaved buffer while the user is editing code. When ``size`` is specified,
the corresponding number of bytes are read over stdin after the json object and newline.

.. code-block:: console

    $ (
      echo '{"command":"generate-tags", "filename":"test.rb", "size": 17}'
      echo 'def foobaz() end'
    ) | ctags --_interactive
    {"_type": "program", "name": "Universal Ctags", "version": "0.0.0"}
    {"_type": "tag", "name": "foobaz", "path": "test.rb", "pattern": "/^def foobaz() end$/", "kind": "method"}
    {"_type": "completed", "command": "generate-tags"}

.. _json lines: http://jsonlines.org/

.. _sandbox-submode:

sandbox submode
--------------------------

``sandbox`` submode can be used with ``--_interactive=sandbox``.  This
submode will activate a sandbox, to this limits the damage that the
can be achieved when exploiting a buffer overflow in Universal Ctags.

In the sandbox submode ctags can generate tags only for inline
requests because ctags has to use open system call to handle file
requests. The open system call is not allowed in the sandbox.

This feature uses `Seccomp BPF (SECure COMPuting with filters)
<https://www.kernel.org/doc/html/latest/userspace-api/seccomp_filter.html>`_,
and is only supported on Linux. To use the sandbox submode `libseccomp
<https://github.com/seccomp/libseccomp>`_ is needed at build-time. If ctags was
built with seccomp support, ``sandbox`` is listed in the output of
``--list-features`` option.

.. code-block:: console

	$ ctags --list-features | grep sandbox
	sandbox

.. code-block:: console

    $ (
      echo '{"command":"generate-tags", "filename":"test.rb", "size": 17}'
      echo 'def foobaz() end'
    ) | ctags --_interactive=sandbox
    {"_type": "program", "name": "Universal Ctags", "version": "0.0.0"}
    {"_type": "tag", "name": "foobaz", "path": "test.rb", "pattern": "/^def foobaz() end$/", "kind": "method"}
    {"_type": "completed", "command": "generate-tags"}
