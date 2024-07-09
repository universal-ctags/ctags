Building with configure (\*nix including GNU/Linux)
---------------------------------------------------------------------

If you are going to build Universal Ctags on a popular GNU/Linux
distribution, you can install the tools and libraries that Universal Ctags
requires (or may use) as packages. See `GNU/Linux distributions`_ about
the packages.

Like most Autotools-based projects, you need to do:

.. code-block:: console

    $ git clone https://github.com/universal-ctags/ctags.git
    $ cd ctags
    $ ./autogen.sh
    $ ./configure --prefix=/where/you/want # defaults to /usr/local
    $ make
    $ make install # may require extra privileges depending on where to install

After installation the `ctags` executable will be in `$prefix/bin/`.

`autogen.sh` runs `autoreconf` internally.
If you use a (binary oriented) GNU/Linux distribution, `autoreconf` may
be part of the `autoconf` package. In addition you may have to install
`automake` and/or `pkg-config`, too.

GNU/Linux distributions
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Before running ./autogen.sh, install some packages.

On Debian-based systems (including Ubuntu), do:

.. code-block:: console

    $ sudo apt install \
        gcc make \
        pkg-config autoconf automake \
        python3-docutils \
        libseccomp-dev \
        libjansson-dev \
        libyaml-dev \
        libxml2-dev

On Fedora systems, do:

.. code-block:: console

    $ sudo dnf install \
        gcc make \
        pkgconfig autoconf automake \
        python3-docutils \
        libseccomp-devel \
        jansson-devel \
        libyaml-devel \
        libxml2-devel

Changing the executable's name
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

On some systems, like certain BSDs, there is already a 'ctags' program in the base
system, so it is somewhat inconvenient to have the same name for
Universal Ctags. During the ``configure`` stage you can now change
the name of the created executable.

To add a prefix 'ex' which will result in 'ctags' being renamed to 'exctags':

.. code-block:: console

	$ ./configure --program-prefix=ex

To completely change the program's name run the following:

.. code-block:: console

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'

Please remember there is also an 'etags' installed alongside 'ctags' which you may also want to rename as shown above.

Link time optimization
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Link-time optimization (LTO) is an interprocedural optimization (IPO) across
translation units (module files) for languages that compile on a file-by-file
basis. This optimization is supported by the compilers, like gcc and clang,
etc.

LTO is usually beneficial to improving program performance (here refers to ctags).
We provide the `--enable-lto` option to enable it, as in the following example:

.. code-block:: console

	$ ./configure --enable-lto

But note that we do not enable LTO by default (for stability reasons), and there
are certain requirements for using LTO. First, the compiler itself must support
LTO optimization, and second, it cannot be cross-compilation (like below). When
the above requirements are met, you need to actively use the `--enable-lto` option
to truly enable LTO optimization for ctags.

For example:

.. code-block:: console

	$ ./configure

is equivalent to:

.. code-block:: console

	$ ./configure --disable-lto

Cross-compilation
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

The way of cross-compilation is a bit complicated because the
build-system of ctags uses `packcc`, a code generator written in C
language. It means that two C compilers should be installed on you build machine;
one for compiling `packcc`, another for compiling `ctags`.

We provide two sets of configure variables to affect these two C compilers:
`CC`, `CFLAGS`, `CPPFLAGS`, `LDFLAGS` variables affect the compiler who compiles `ctags`.
`CC_FOR_BUILD`, `CPPFLAGS_FOR_BUILD`, `CPPFLAGS_FOR_BUILD`, `LDFLAGS_FOR_BUILD` variables
affect the compiler who compiles `packcc`.

When native-compiling, `FOO_FOR_BUILD` is the same as `FOO`.

Here is an example show you how to use these configure variables:

.. code-block:: console

       $ mkdir ./out
       $ configure \
               --host=armv7a-linux-androideabi \
               --prefix=`pwd`/out \
               --enable-static \
               --disable-seccomp \
               CC=/usr/local/opt/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/darwin-x86_64/bin/armv7a-linux-androideabi21-clang \
               CFLAGS='-v' \
               CPP='/usr/local/opt/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/darwin-x86_64/bin/armv7a-linux-androideabi21-clang -E' \
               CPPFLAGS='-I/Users/leleliu008/.ndk-pkg/pkg/jansson/armeabi-v7a/include -I/Users/leleliu008/.ndk-pkg/pkg/libyaml/armeabi-v7a/include -I/Users/leleliu008/.ndk-pkg/pkg/libxml2/armeabi-v7a/include -I/Users/leleliu008/.ndk-pkg/pkg/libiconv/armeabi-v7a/include --sysroot /usr/local/opt/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/darwin-x86_64/sysroot -Qunused-arguments -Dftello=ftell -Dfseeko=fseek' \
               LDFLAGS='-L/Users/leleliu008/.ndk-pkg/pkg/jansson/armeabi-v7a/lib -L/Users/leleliu008/.ndk-pkg/pkg/libyaml/armeabi-v7a/lib -L/Users/leleliu008/.ndk-pkg/pkg/libxml2/armeabi-v7a/lib -L/Users/leleliu008/.ndk-pkg/pkg/libiconv/armeabi-v7a/lib --sysroot /usr/local/opt/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/darwin-x86_64/sysroot' \
               AR=/usr/local/opt/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/darwin-x86_64/bin/arm-linux-androideabi-ar \
               RANLIB=/usr/local/opt/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/darwin-x86_64/bin/arm-linux-androideabi-ranlib \
               CC_FOR_BUILD=/usr/bin/cc \
               CFLAGS_FOR_BUILD='-v' \
               PKG_CONFIG_PATH=/Users/leleliu008/.ndk-pkg/pkg/libiconv/armeabi-v7a/lib/pkgconfig:/Users/leleliu008/.ndk-pkg/pkg/libxml2/armeabi-v7a/lib/pkgconfig:/Users/leleliu008/.ndk-pkg/pkg/libyaml/armeabi-v7a/lib/pkgconfig:/Users/leleliu008/.ndk-pkg/pkg/jansson/armeabi-v7a/lib/pkgconfig \
               PKG_CONFIG_LIBDIR=/Users/leleliu008/.ndk-pkg/pkg
       ...
       $ make
       ...
       $ make install
       ...
       $ ls out/bin
       ctags readtags

Simpler example for `aarch64-linux-gnu` can be found in `circle.yml` in the source tree.

PEG optimization
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Some parsers of Universal Ctags are written in PEG grammar.  The build
system of Universal Ctags uses `packcc
<https://github.com/arithy/packcc>`_ for translating the PEG source
files to C source files. A snapshot version of `packcc` is in the
source tree of Universal Ctags. The in-tree `packcc` is used when
building `ctags`. So you don't have to install `packcc`.

`pegof <https://github.com/dolik-rce/pegof>`_ is a PEG grammar
optimizer (and formatter). You can use `pegof` to build `ctags` though the
developer says it is not yet stable enough (https://github.com/universal-ctags/ctags/pull/4023).

You may have to build `pegof` first.
See `Building <https://github.com/dolik-rce/pegof/blob/master/README.md#building>`_ about
how to build `pegof`. Following the instructions on the page, you may
see the executable is at `./build/pegof`.

When building ctags, specify the executable to `--with-pegof` option of `./configure`
script:

.. code-block:: console

   $ ls -d ctags pegof
   ctags  pegof
   $ ls -l pegof/build/pegof
   -rwxr-xr-x. 1 yamato yamato 1894560 Jun 30 03:01 pegof/build/pegof
   $ cd ctags
   $ ./configure --with-pegof=../pegof/build/pegof
   ...
   $ make
   ...

If your `ctags` is built with `pegof`, you can verify it with `--list-features` option:

.. code-block:: console

   $ ./ctags --list-features | grep pegof
   pegof             makes peg based parser(s) optimized (experimental)
