Building with configure (\*nix including GNU/Linux)
---------------------------------------------------------------------

If you are going to build Universal Ctags on a popular GNU/Linux
distribution, you can install the tools and libraries that Universal Ctags
requires (or may use) as packages. See `GNU/Linux distributions`_ about
the packages.

Like most Autotools-based projects, you need to do::

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

On Debian-based systems (including Ubuntu), do::

    $ sudo apt install \
        gcc make \
        pkg-config autoconf automake \
        python3-docutils \
        libseccomp-dev \
        libjansson-dev \
        libyaml-dev \
        libxml2-dev

On Fedora systems, do::

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

.. code-block:: bash

	$ ./configure --program-prefix=ex

To completely change the program's name run the following:

.. code-block:: bash

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'

Please remember there is also an 'etags' installed alongside 'ctags' which you may also want to rename as shown above.

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

::

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
