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
`automake`, too.
