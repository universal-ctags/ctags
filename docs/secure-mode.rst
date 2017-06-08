.. _secure-mode:

======================================================================
``--_secure`` Mode
======================================================================

Universal-ctags can be run with ``--_secure``. This is similar to
``--_interactive``, this will activate a sandbox, to this limits the
damage that the can be achieved when exploiting a buffer overflow in
Ctags.

The ``--_secure`` mode must be used with inline file contents, eg.

.. code-block:: console

    $ (
      echo '{"command":"generate-tags", "filename":"test.rb", "size": 17}'
      echo 'def foobaz() end'
    ) | ctags --_secure
    {"_type": "program", "name": "Universal Ctags", "version": "0.0.0"}
    {"_type": "tag", "name": "foobaz", "path": "test.rb", "pattern": "/^def foobaz() end$/", "kind": "method"}
    {"_type": "completed", "command": "generate-tags"}

.. _json lines: http://jsonlines.org/

This feature uses seccomp-bpf, and is only supported on Linux.
