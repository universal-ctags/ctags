[![Build Status](https://travis-ci.org/universal-ctags/ctags.svg?branch=master)](https://travis-ci.org/universal-ctags/ctags)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/4355/badge.svg)](https://scan.coverity.com/projects/4355)
[![Coverage Status](https://coveralls.io/repos/universal-ctags/ctags/badge.svg?branch=master&service=github)](https://coveralls.io/github/universal-ctags/ctags?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/6hk2p5lv6jsrd9o7/branch/master?svg=true)](https://ci.appveyor.com/project/universalctags/ctags/branch/master)
[![RTD build status](https://readthedocs.org/projects/ctags/badge)](http://docs.ctags.io)
[![CircleCI Build Status](https://circleci.com/gh/universal-ctags/ctags.svg?style=shield&circle-token=2e582261da84ebc6d21725b05381f410bc5de29d)](https://circleci.com/gh/universal-ctags)

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

If misc/packcc directory is empty, run following command to get
source code before building ctags:
```
$ git submodule init misc/packcc
$ git submodule update misc/packcc
```

## The latest build and package ##

If you want to try the latest universal-ctags without building it yourself...

### Windows
Daily builds are available at the [ctags-win32](https://github.com/universal-ctags/ctags-win32) project.
Go to the [releases](https://github.com/universal-ctags/ctags-win32/releases) page to download zip packages.

### Mac
See [Homebrew Tap for Universal Ctags](https://github.com/universal-ctags/homebrew-universal-ctags)

### Docker
Go to [ctags-docker](https://github.com/universal-ctags/ctags-docker) and follow
instructions to download the `uctags` script. Docker will automatically fetch
the image from Docker Hub.

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

## Difference ##

You may be interested in how universal-ctags is different from
exuberant-ctags. The critical and attractive changes are explained
in docs/\*.rst. The preformatted version is available on line,
http://docs.ctags.io/.

The most significant one is that Universal-ctags doesn't load
`~/.ctags` and `./.ctags` at starting up time. Instead, it loads
`~/.ctags.d/*.ctags` and `./.ctags.d/*.ctags`. See the above web
site and man pages
(man/ctags.1.rst.in and man/ctags-incompatibilities.7.in in the
source tree).

Pull-requests are welcome!

## peg-based-java-parser branch ##

This is an experimental branch for testing "packcc" parser generator
and peg based "NewJava" parser.  Only building with Autotools is
supported. Volunteers for the other build-system are welcome.

packcc is registred as a git submodule. It is taken from https://github.com/enechaev/packcc.
The peg definition for Java 1.7 is taken from https://github.com/pointlander/peg/blob/master/grammars/java/java_1_7.peg.

git submodule handling is done in ./autogen.sh.

How to switch the branch:

```
[yamato@master]/tmp% git clone https://github.com/masatake/ctags.git
Cloning into 'ctags'...
remote: Counting objects: 38867, done.
remote: Compressing objects: 100% (48/48), done.
remote: Total 38867 (delta 18), reused 43 (delta 16), pack-reused 38802
Receiving objects: 100% (38867/38867), 11.80 MiB | 4.41 MiB/s, done.
Resolving deltas: 100% (24486/24486), done.
[yamato@master]/tmp% cd ctags
[yamato@master]/tmp/ctags% git checkout peg-based-java-parser
Branch 'peg-based-java-parser' set up to track remote branch 'peg-based-java-parser' from 'origin'.
Switched to a new branch 'peg-based-java-parser'
```

How to try the new java parser:
```
[yamato@master]~/var/ctags-peg% ./ctags --fields=+l -o - --languages=+NewJava --language-force=NewJava foo.java
A	foo.java	/^    A;$/;"	e	language:NewJava	enum:e
e	foo.java	/^public enum e {$/;"	g	language:NewJava
getString	foo.java	/^    public String getString() {$/;"	m	language:NewJava	enum:e
```

TODO:

* Improve or modify packcc to make ctags pass the test case parser-java.r/accented-latin1-identifiers.java.d,
* Build a ctags executable on non-Autotools platform,
* Support Java-1.9 (See http://www.romanredz.se/Mouse/index.htm#parsing), and
* Add a parser for peg itself.
