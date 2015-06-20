Building with configure
---------------------------------------------------------------------

Like most Autotools-based projects, you need to do::

    $ autoreconf -vfi
    $ ./configure --prefix=/where/you/want # defaults to /usr/local
    $ make
    $ make install # may require extra privileges depending on where to install

After installing the `ctags` executable will be in `$prefix/bin/`.

