Building on Mac OS
-----------------------------------------------------------------------------

:Maintainer: Cameron Eagans <me@cweagans.net>

----

This part of the documentation is written by Cameron Eagans, a co-maintainer of Universal Ctags and the maintainer of
the OSX packaging of this project.


Build Prerequisites
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Building ctags on OSX should be no different than building on GNU/Linux. The same toolchains are used, and the Mac OS
packaging scripts use autotools and make (as you'd expect).

You may need to install the xcode command line tools. You can install the entire xcode distribution from the App Store,
or for a lighter install, you can simply run ``xcode-select --install`` to *only* install the compilers and such. See
https://stackoverflow.com/a/9329325 for more information. Once your build toolchain is installed, proceed to the next
section.

At this point, if you'd like to build from an IDE, you'll have to figure it out. Building ctags is a pretty straightforward
process that matches many other projects and most decent IDEs should be able to handle it.

Building Manually (i.e. for development)
.............................................................................

You can simply run the build instructions in README.md.

Building with Homebrew
.............................................................................

Homebrew (https://brew.sh/) is the preferred method for installing Universal Ctags for end users. Currently, the process
for installing with Homebrew looks like this::

        brew tap universal-ctags/universal-ctags
        brew install --HEAD universal-ctags

Eventually, we hope to move the Universal-ctags formula to the main Homebrew repository, but since we don't have any
tagged releases at this point, it's a head-only formula and wouldn't be accepted. When we have a tagged release, we'll
submit a PR to Homebrew.

If you'd like to help with the Homebrew formula, you can find the repository here:
https://github.com/universal-ctags/homebrew-universal-ctags


Differences between OSX and GNU/Linux
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There other things where building ctags on OSX differs from building on GNU/Linux.

- Filenames on HFS+ (the Mac OS filesystem) are case-preserving, but not case-sensitive in 99% of configurations. If a
  user manually formats their disk with a case sensitive version of HFS+, then the filesystem will behave like normal
  GNU/Linux systems. Depending on users doing this is not a good thing.

Contributing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This documentation is very much a work in progress. If you'd like to contribute, submit a PR and mention @cweagans for
review.

