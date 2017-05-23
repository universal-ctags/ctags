.. _interactive-mode:

======================================================================
``--_interactive`` Mode
======================================================================

Universal ctags can be run with ``--_interactive``, which enters a REPL that
can be used programatically to control ctags generation. In this mode, json
commands are received over stdin, and corresponding responses are emitted over
stdout.

Communication with Universal ctags over stdio uses the `json lines`_ format, where each
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

The simplest way to generate tags for a file is by passing its path on disk. The response will include
one json object per line representing each tag, followed by a single json object with the ``completed``
field emitted once the file has been fully processed.

.. code-block:: console

    $ echo '{"command":"generate-tags", "filename":"test.rb"}' | ctags --_interactive
    {"_type": "program", "name": "Universal Ctags", "version": "0.0.0"}
    {"_type": "tag", "name": "foobar", "path": "test.rb", "pattern": "/^  def foobar$/", "kind": "method", "scope": "Test", "scopeKind": "class"}
    {"_type":"completed", "command": "generate-tags"}

The ``generate-tags`` command can also be used to generate tags for code which is not present on disk. For example,
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
