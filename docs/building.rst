Building ctags
=============================================================================

`Building with configure <autotools.rst>`_

`Building/hacking/using on MS-Windows <windows.rst>`_

`Building on Mac OS <osx.rst>`_

Build system add possibility to change program name
---------------------------------------------------------------------

As on some systems (e.g. BSD), there is alreayd a 'ctags' program in the base
system. Since it's somewhat inconvenient to have the same name for 
universal-ctags, during the ``configure`` phase you can now change the output
of the executable name.

Add a prefix 'ex' which will result in 'ctags' being transformed into 'exctags'
::

	$ ./configure --program-prefix=ex

If you would like to completely change program name please remember in this case
there is also an 'etags' along with a 'ctags'
::

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'
