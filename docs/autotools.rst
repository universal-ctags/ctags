Building with configure (\*nix including GNU/Linux)
---------------------------------------------------------------------
Like most Autotools-based projects, you need to do::

    $ ./autogen.sh
    $ ./configure --prefix=/where/you/want # defaults to /usr/local
    $ make
    $ make install # may require extra privileges depending on where to install

After installing the `ctags` executable will be in `$prefix/bin/`.

`autogen.sh` runs `autoreconf` internally.
If you use (binary oriented) GNU/Linux distribution, `autoreconf` may
be part of `autoconf` package. In additional you may have to install
`automake` and/or `pkg-config`, too.

Build system add possibility to change program name
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

On some systems, like certain BSDs, there is already a 'ctags' program in the base
system, so it is somewhat inconvenient to have the same name for
universal-ctags. During the ``configure`` stage you can now change
the output executable name.

To add a prefix 'ex' which will result in 'ctags' being transformed into 'exctags'

.. code-block:: bash

	$ ./configure --program-prefix=ex

To completely change the program's name run the following. Please remember there is also an 'etags' along 'ctags'

.. code-block:: bash

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'
