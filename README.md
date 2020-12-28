# Universal Ctags

[![Build Status](https://travis-ci.com/universal-ctags/ctags.svg?branch=master)](https://travis-ci.com/universal-ctags/ctags)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/4355/badge.svg)](https://scan.coverity.com/projects/4355)
[![Coverage Status](https://coveralls.io/repos/universal-ctags/ctags/badge.svg?branch=master&service=github)](https://coveralls.io/github/universal-ctags/ctags?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/6hk2p5lv6jsrd9o7/branch/master?svg=true)](https://ci.appveyor.com/project/universalctags/ctags/branch/master)
[![RTD build status](https://readthedocs.org/projects/ctags/badge)](https://docs.ctags.io)
[![CircleCI Build Status](https://circleci.com/gh/universal-ctags/ctags.svg?style=shield&circle-token=2e582261da84ebc6d21725b05381f410bc5de29d)](https://circleci.com/gh/universal-ctags)
![GitHub Actions/VALGRIND](https://github.com/universal-ctags/ctags/workflows/run%20units%20target%20under%20VALGRIND/badge.svg)

Universal Ctags generates an index (or tag) file of language objects found in source files for many popular programming languages. This index makes it easy for text editors and other tools to locate the indexed items. Universal Ctags improves on traditional ctags because of its multilanguage support, its ability for the user to define new languages searched by regular expressions, and its ability to generate emacs-style TAGS files.

universal-ctags has the objective of continuing the development from
what existed in the Sourceforge area. Github exuberant-ctags
repository was started by Reza Jelveh and was later moved to the
universal-ctags organization.

The goal of the project is preparing and maintaining common/unified working
space where people interested in making ctags better can work
together.

## The latest build and package ##

If you want to try the latest universal-ctags without building it yourself...

### Windows
Daily builds are available at the [ctags-win32](https://github.com/universal-ctags/ctags-win32) project.
Go to the [releases](https://github.com/universal-ctags/ctags-win32/releases) page to download zip packages.

### Mac
See [Homebrew Tap for Universal Ctags](https://github.com/universal-ctags/homebrew-universal-ctags)

### Snap
Go to [ctags-snap](https://github.com/universal-ctags/ctags-snap) and
clone the `ctags-snap` repo. Then, follow instructions to build the
snap package of ctags. Snapcraft will automatically fetch the source
code from GitHub.

## How to build and install ##

To build with Autotools (Autoconf, Automake, and Libtool) on GNU/Linux, OSX, or Windows 10 WSL,
```
    $ git clone https://github.com/universal-ctags/ctags.git
    $ cd ctags
    $ ./autogen.sh
    $ ./configure --prefix=/where/you/want # defaults to /usr/local
    $ make
    $ make install # may require extra privileges depending on where to install
```

See
[`docs/autotools.rst`](https://github.com/universal-ctags/ctags/blob/master/docs/autotools.rst)
for more information.

To build on Windows, see
[`docs/windows.rst`](https://github.com/universal-ctags/ctags/blob/master/docs/windows.rst)
for more information.

To build on OSX, see
[`docs/osx.rst`](https://github.com/universal-ctags/ctags/blob/master/docs/osx.rst)
for more information.

## Manual ##
Go to https://docs.ctags.io for the preformatted documentations
of the latest development version.
See also `*/README.md` on this repository.

## Differences from exuberant-ctags ##

You may be interested in how universal-ctags is different from
exuberant-ctags. See [Introduced changes](https://docs.ctags.io/en/latest/news.html)
on https://docs.ctags.io/ for details.

The most significant incompatible changes:

* Universal-ctags doesn't load
`~/.ctags` and `./.ctags` at starting up time. Instead, it loads
`~/.ctags.d/*.ctags` and `./.ctags.d/*.ctags`. See the above web
site and man pages
(man/ctags.1.rst.in and man/ctags-incompatibilities.7.in in the
source tree).

* Universal-ctags is more strict about characters that can be
  used in kind letters and kind names than Exuberant-ctags.

  - The letter must be an alphabetical character (`[a-zA-EG-Z]`).
    `F` is reserved for `file` kind.

  - The first character of the name must be alphabetic, and
    the rest characters must be alphanumeric (`[a-zA-Z][a-zA-Z0-9]*`).

  See the web site and man pages. The detailed background is explained
  in [#1737](https://github.com/universal-ctags/ctags/pull/1737).

  If you want to reuse your .ctags written for Exuberant-ctags,
  you must review kind letters and names defined with `--regex-<LANG>=...`
  options. When updating the definitions, using `--kinddef-<LANG>=...` option
  is appreciated.

Pull-requests are welcome!
