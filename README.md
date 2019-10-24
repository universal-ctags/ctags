# Universal Ctags

[![Build Status](https://travis-ci.org/universal-ctags/ctags.svg?branch=master)](https://travis-ci.org/universal-ctags/ctags)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/4355/badge.svg)](https://scan.coverity.com/projects/4355)
[![Coverage Status](https://coveralls.io/repos/universal-ctags/ctags/badge.svg?branch=master&service=github)](https://coveralls.io/github/universal-ctags/ctags?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/6hk2p5lv6jsrd9o7/branch/master?svg=true)](https://ci.appveyor.com/project/universalctags/ctags/branch/master)
[![RTD build status](https://readthedocs.org/projects/ctags/badge)](https://docs.ctags.io)
[![CircleCI Build Status](https://circleci.com/gh/universal-ctags/ctags.svg?style=shield&circle-token=2e582261da84ebc6d21725b05381f410bc5de29d)](https://circleci.com/gh/universal-ctags)

Universal Ctags generates an index (or tag) file of language objects found in source files for many popular programming languages. This index makes it easy for text editors and other tools to locate the indexed items. Universal Ctags improves on traditional ctags because of its multilanguage support, its ability for the user to define new languages searched by regular expressions, and its ability to generate emacs-style TAGS files.

universal-ctags has the objective of continuing the development from
what existed in the Sourceforge area. Github exuberant-ctags
repository was started by Reza Jelveh and was later moved to the
universal-ctags organization.

The goal of the project is preparing and maintaining common/unified working
space where people interested in making ctags better can work
together.

## Getting PACKCC compiler-compiler ##

Packcc is a compiler-compiler; it translates .peg grammar file to .c
file.  packcc was originally written by Arihiro Yoshida. Its source
repository is at sourceforge. It seems that packcc at sourceforge is
not actively maintained. Some derived repositories are at
github. Currently, our choice is
https://github.com/enechaev/packcc. It is the most active one in the
derived repositories.

The source tree of packcc is grafted at misc/packcc directory.
Building packcc and ctags are integrated in the build-scripts of
Universal-ctags.

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

To build with Autotools, see `docs/autotools.rst` for more information.
(To build on GNU/Linux, Autotools is your choice.)
To build on Windows, see `docs/windows.rst` for more information.
To build on OSX, see `docs/osx.rst` for more information.

## Manual ##
Man page (ctags.1) is generated only in Autotools based building process.
In addition rst2man command is needed.

rst2man is part of the python-docutils package on Ubuntu.

## Differences ##

You may be interested in how universal-ctags is different from
exuberant-ctags. The critical and attractive changes are explained
in docs/\*.rst. The preformatted version is available on line,
https://docs.ctags.io/.

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
  options. When updating the definitions, using `--kind-<LANG>` option
  is appreciated.

Pull-requests are welcome!
