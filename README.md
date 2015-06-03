[![Build Status](https://travis-ci.org/universal-ctags/ctags.svg?branch=master)](https://travis-ci.org/universal-ctags/ctags)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/4355/badge.svg)](https://scan.coverity.com/projects/4355)

This project continues the development of exuberant-ctags, but with the work
hosted at github.

The purpose of the project is preparing and maintaining common/unified working
space where people interested in making universal-ctags better can work
together.

Pull-requests are welcome!

## How to build and install ##

Like most Autotools-based projects, you need to do:

```bash
$ autoreconf -vfi
$ ./configure --prefix=/where/you/want # defaults to /usr/local
$ make
$ make install # may require extra privileges depending on where to install
```

After installing the `ctags` executable will be in `$prefix/bin/`.

To build on Windows see `docs/windows.rst` for more information.
