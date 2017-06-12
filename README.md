[![Build Status](https://travis-ci.org/universal-ctags/ctags.svg?branch=master)](https://travis-ci.org/universal-ctags/ctags)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/4355/badge.svg)](https://scan.coverity.com/projects/4355)
[![Coverage Status](https://coveralls.io/repos/universal-ctags/ctags/badge.svg?branch=master&service=github)](https://coveralls.io/github/universal-ctags/ctags?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/6hk2p5lv6jsrd9o7/branch/master?svg=true)](https://ci.appveyor.com/project/universalctags/ctags/branch/master)
[![RTD build status](https://readthedocs.org/projects/ctags/badge)](http://docs.ctags.io)

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
- Go to https://ci.appveyor.com/project/universalctags/ctags/history
  - Select one of the builds named ```Daily build: YYYY-MM-DD```.
  - Click the ```compiler=msys2, ARCH=x64, ...``` (or ```compiler=msys2, ARCH=x86, ...```) job.
  - View the *Artifacts* tab and download ```ctags-XXXXXX-x64.zip``` (or ```ctags-XXXXXX-x86.zip```). (```XXXXXX``` is a version number or a commit ID.)
  - Add the binary folder to your PATH.
  - If you need unstripped binaries for debugging, download ```ctags-XXXXXX-x64.debug.zip``` (or ```ctags-XXXXXX-x86.debug.zip```).

### Mac
See [Homebrew Tap for Universal Ctags](https://github.com/universal-ctags/homebrew-universal-ctags)

## How to build and install ##

To build with Autotools, see `docs/autotools.rst` for more information.
(To build on GNU/Linux, Autotools is your choice.)
To build on Windows, see `docs/windows.rst` for more information.
To build on OSX, see `docs/osx.rst` for more information.

## Manual ##
Man page (ctags.1) is generated only in Autotools based building process.
In addition rst2man command is needed.

rst2man is part of the python-docutils package on Ubuntu.

## Difference ##

You may be interested in how universal-ctags is different from
exuberant-ctags. The critical and attractive changes are explained
in docs/\*.rst. The preformatted version is available on line,
http://docs.ctags.io/.

Pull-requests are welcome!
