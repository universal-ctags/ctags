Building/hacking/using on MS-Windows
=============================================================================

:Maintainer: Frank Fesevur <ffes@users.sourceforge.net>

----

This part of the documentation is written by Frank Fesevur, co-maintainer of exuberant-ctags and the maintainer of the Windows port of this project. It is still very much work in progress. Things still need to be written down, tested or even investigated. When building for Windows you need to know there are many compilers and many building environments. This is a summary of available options and things that have been tested so far.


Compilers
-----------------------------------------------------------------------------

There are many compilers for Windows. Apart from the ones mentioned here others do exist, but not used by me.


Microsoft Visual Studio
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
http://www.visualstudio.com/

Obviously there is Microsoft Visual Studio 2013. Many professional developers targeting Windows use Visual Studio. Visual Studio comes in a couple of different editions. Their Express Edition is free to use, but a Microsoft-account is required to download the .iso and when you want to continue using it after a 30-days trial period. All other editions you need to be paid for.

Installing Visual Studio will give you the IDE, the command line compilers and the MS-version of make named nmake.

Note that ctags can not be build with Visual Studio older then 2013 anymore. There is C99 (or C11) coding used that generate syntax errors with VS2010 and older. This could effect compilers from other vendors as well.


GCC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are three flavors of GCC for Windows:

- MinGW http://www.mingw.org
- MinGW-w64 http://mingw-w64.sourceforge.net
- TDM-GCC http://tdm-gcc.tdragon.net

MinGW started it all, but development stalled a while and no x64 was available. Then the MinGW-w64 fork emerged. It started as a 64-bit compiler, but soon they included both a 32-bit and a 64-bit compiler. But the name remained, a bit confusing. Another fork of MinGW is TDM-GCC. It also provides both 32-bit and 64-bit compilers. All have at least GCC 4.8. MinGW-w64 appears to be the most used flavor of MinGW at this moment. Many well known program the originate from GNU/Linux use MinGW-w64 to compile their Windows port.

Building ctags from the command line
-----------------------------------------------------------------------------

Microsoft Visual Studio
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most users of Visual Studio will use the IDE and not the command line to compile a project. But by default a shortcut to the command prompt that sets the proper path is installed in the Start Menu. When this command prompt is used ``nmake -f mk_mvc.mak`` will compile ctags. You can also go into the ``win32`` subdirectory and run ``msbuild ctags_vs2013.sln`` for the default build. Use ``msbuild ctags_vs2013.sln /p:Configuration=Release`` to specifically build a release build. MSBuild is what the IDE uses internally and therefore will product the same files as the IDE.

GCC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**General**

All the GCC's come with installers or with zipped archives. Install or extract them in a directory without spaces.
GNU Make builds for Win32 are available as well, and sometimes are included with the compilers. Make sure it is in your path, for instance by copying the make.exe in the bin directory of your compiler.
Native win32 version of the GNU/Linux command cp, rm and mv can be useful. rm is almost always used in by the ``clean`` target of a makefile.

Note that ctags builds with MinGW and TDM-GCC but does not build with MinGW-w64 at the moment. To fix the MinGW-w64 build a small patch is required for general.h. I have to test if this patch does not effects the other GCCs.


**CMD**

Any Windows includes a command prompt. Not the most advanced, but it is enough to do the build tasks. Make sure the path is set properly and ``make -f mk_mingw.mak`` should so the trick.

**MSYS**

From their site: MSYS is a collection of GNU utilities such as bash, make, gawk and grep to allow building of applications and programs which depend on traditionally UNIX tools to be present. It is intended to supplement MinGW and the deficiencies of the cmd shell.

MSYS comes in two flavors. The original from MinGW and a MSYS2 http://sourceforge.net/projects/msys2/

MSYS is old but still works. You can build ctags with it using ``make -f mk_mingw.mak``. You can run ``autoheader``, ``autoconf``, ``./configure`` and ``make`` but that way regex and fnmatch are not included in the build process and these functionalities are lost. This is likely fixable by someone who knows autoconf better.

MSYS2 is a more maintained version of MSYS, but specially geared towards MinGW-w64. Still to be investigated.

**Cygwin**

Cygwin provides ports of many GNU/Linux tools and a POSIX API layer. This is the most complete way to get the GNU/Linux terminal feel under Windows. Cygwin has a setup that helps you install all the tools you need. Drawback of Cygwin is that the POSIX API layer (cygwin1.dll) is not fast.

It is easy to build a Cygwin version of ctags using the normal GNU/Linux build steps. This ctags.exe will depend on cygwin1.dll and should only be used within the Cygwin ecosystem.

Cygwin has packages with a recent version of MinGW-w64 as well. This way it is easy to cross-compile a native Windows application. With ``make -f mk_mingw.mak  CC=i686-w64-mingw32-gcc`` a native Windows application can be compiled.

You can also build a native Windows version when using ``./configure --host=i686-w64-mingw32`` but this also doesn't add the regex and fnmatch to the program. Similar problem as with MSYS.

**Cross-compile from GNU/Linux**

All major distros have both MinGW and MinGW-w64 packages. Compiling works the same way as with MSYS and Cygwin. A Windows application can be cross-compiled, with the same limitation when autoconf is used. Just like with Cygwin, with ``make -f mk_mingw.mak CC=i686-w64-mingw32-gcc`` a native Windows application can be compiled.


Building ctags with IDEs
-----------------------------------------------------------------------------

I have no idea how things work for most GNU/Linux developers, but most Windows developers are used to IDEs. Not many use a command prompt and running gdb from the command line is not a thing a Windows developers would normally do. Many IDEs exist for Windows, I use the two below.

Microsoft Visual Studio
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As already mentioned Microsoft Visual Studio 2013 has the free Express edition. For ctags the Windows Desktop Express Edition is enough to get the job done. The IDE has a proper debugger. Project files for VS2013 can be found in the win32 directory.

Please know that when files are added to the sources.mak, these files need to be added to the .vcproj and .vcproj.filters files as well. The XML of these files should not be a problem.

Code::Blocks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
http://www.codeblocks.org/

Code::Blocks is a decent GPL-licensed IDE that has good gcc and gdb integration. The TDM-GCC that can be installed together with Code::Blocks works fine and I can provide a project file. This is an easy way to have a free - free as in beer as well as in speech - solution and to have the debugger within the GUI as well.


Other differences between Microsoft Windows and GNU/Linux
-----------------------------------------------------------------------------

There other things where building ctags on Microsoft Windows differs from building on GNU/Linux.

- Filenames on Windows file systems are case-preserving, but not case-sensitive.
- Windows file systems use backslashes "\\" as path separators, but paths with forward slashes "/" are no problem for a Windows program to recognize, even when a full path (include drive letter) is used.
- The default line-ending on Windows is CRLF. A tags file generated by the Windows build of ctags will contain CRLF.
- The tools used to build ctags do understand Unix-line endings without problems. There is no need to convert the line-ending of existing files.
- Due to the differences between the GNU/Linux and Windows C runtime library there are some things that need to be added to ctags to make make the program as powerful as it is on GNU/Linux. At this moment regex and fnmatch are borrowed from glibc.
- Because there is no default scandir() for Windows, the optlib feature is not yet available for Windows. Various implementations of scandir() for Windows do exist, but still have to be investigated.
- The xcmd feature is not yet available for Windows. This needs to be investigated.
- Units testing needs a decent bash-like shell. It is only tested using Cygwin.
