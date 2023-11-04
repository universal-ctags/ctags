# Universal Ctags

[![Coverity Scan Build Status](https://scan.coverity.com/projects/4355/badge.svg)](https://scan.coverity.com/projects/4355)
[![Coverage Status](https://coveralls.io/repos/universal-ctags/ctags/badge.svg?branch=master&service=github)](https://coveralls.io/github/universal-ctags/ctags?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/6hk2p5lv6jsrd9o7/branch/master?svg=true)](https://ci.appveyor.com/project/universalctags/ctags/branch/master)
[![RTD build status](https://readthedocs.org/projects/ctags/badge)](https://docs.ctags.io)
[![CircleCI Build Status](https://dl.circleci.com/status-badge/img/gh/universal-ctags/ctags/tree/master.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/universal-ctags/ctags/tree/master)
![GitHub Actions/VALGRIND](https://github.com/universal-ctags/ctags/workflows/run%20units%20target%20under%20VALGRIND/badge.svg)

[Universal Ctags](https://ctags.io/) (abbreviated as u-ctags) is a *maintained*
implementation of `ctags`.
`ctags` generates an index (or tag) file of language objects found in source
files for programming languages.
This index makes it easy for text editors and other tools to locate the indexed
items.

[Exuberant Ctags](http://ctags.sourceforge.net/) (e-ctags) maintained by Darren
Hiebert, the ancestor of Universal Ctags, improved traditional `ctags` with
multi-language support, the ability for the user to define new languages
searched by regular expressions (called optlib in Universal Ctags), and the
ability to generate emacs-style TAGS files.
But the activity of the project unfortunately stalled.

Universal Ctags has the objective of continuing the development of Exuberant
Ctags.
Reza Jelveh <reza.jelveh@gmail.com> initially created a personal fork of
Exuberant Ctags on GitHub.
As interest and participation grew, it was decided to move development to a
dedicated project as Universal Ctags.
The goal of this project is to maintain a common/unified working space where
people interested in making ctags better can work together.

Some of the major features of Universal Ctags are:

* more numbers of improved language support
    * new extended C/C++ language parser, etc.
* fully extended optlib (a feature to define a new language parser from a
  command line)
* interactive mode (experimental)

## The latest build and package ##

If you want to try the latest Universal Ctags without building it yourself...

### Windows
Daily builds are available at the [ctags-win32](https://github.com/universal-ctags/ctags-win32) project.
Go to the [releases](https://github.com/universal-ctags/ctags-win32/releases) page to download zip packages.

### Unix-like
Nightly builds are available at the [ctags-nightly-build](https://github.com/universal-ctags/ctags-nightly-build) project.
Go to the [releases](https://github.com/universal-ctags/ctags-nightly-build/releases) page to download tarball archives.

### Mac
Recent builds are available via the [`universal-ctags` Homebrew formula](https://formulae.brew.sh/formula/universal-ctags).

### Snap
Go to [ctags-snap](https://github.com/universal-ctags/ctags-snap) and
clone the `ctags-snap` repo. Then, follow instructions to build the
snap package of Universal Ctags. Snapcraft will automatically fetch the source
code from GitHub.

## How to build and install ##

To build with Autotools (Autoconf and Automake) on GNU/Linux, OSX, or Windows 10 WSL,
```
    $ git clone https://github.com/universal-ctags/ctags.git
    $ cd ctags
    $ ./autogen.sh
    $ ./configure  # use --prefix=/where/you/want to override installation directory, defaults to /usr/local
    $ make
    $ make install # may require extra privileges depending on where to install
```

GNU make is assumed as the `make` command.

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
The primary documents of Universal Ctags are man pages.
Users should first consult the
[ctags(1)](https://docs.ctags.io/en/latest/man/ctags.1.html), and [other man
pages](https://docs.ctags.io/en/latest/man-pages.html) if necessary.

[Universal Ctags Hacking Guide](https://docs.ctags.io), which also includes the
man pages, is primarily for developers and provides additional information to
the man pages, including experimental features.

See also `*/README.md` on this repository.

## Differences from exuberant-ctags ##

You may be interested in how Universal Ctags is different from Exuberant Ctags.
See
[ctags-incompatibilities(7)](https://docs.ctags.io/en/latest/man/ctags-incompatibilities.7.html)
and [Introduced changes](https://docs.ctags.io/en/latest/news.html) for details.

The most significant incompatible changes:

* Universal Ctags doesn't load `~/.ctags` and `./.ctags` at starting up time.
  Instead, it loads `~/.ctags.d/*.ctags` and `./.ctags.d/*.ctags`.

* Universal Ctags is more strict about characters that can be
  used in kind letters and kind names than Exuberant-ctags.

  - The letter must be an alphabetical character (`[a-zA-EG-Z]`).
    `F` is reserved for `file` kind.

  - The first character of the name must be alphabetic, and
    the rest characters must be alphanumeric (`[a-zA-Z][a-zA-Z0-9]*`).

  The detailed background is explained in
  [#1737](https://github.com/universal-ctags/ctags/pull/1737).

  If you want to reuse your `.ctags` written for Exuberant-ctags,
  you must review kind letters and names defined with `--regex-<LANG>=...`
  options. When updating the definitions, using `--kinddef-<LANG>=...` option
  is appreciated.

## [CVE-2022-4515](https://access.redhat.com/security/cve/CVE-2022-4515) ##
It is not affected to Universal Ctags.
It was fixed in [e00c55d7a0204dc1d0ae316141323959e1e16162](https://github.com/universal-ctags/ctags/commit/e00c55d7a0204dc1d0ae316141323959e1e16162) in 2016. Thanks to the reporter.

Pull-requests are welcome!
