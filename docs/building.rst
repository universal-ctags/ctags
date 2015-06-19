Building ctags
=============================================================================

.. include:: windows.rst

.. include:: osx.rst

Build system add possibility to change program name
---------------------------------------------------------------------

As on some systems (e.g. BSD) there is a 'ctags' program in the base
system it's somewhat inconvenient to have the same name for universal-ctags
During ``configure`` you can now change the output executable name.

Add a prefix 'ex' which will result in 'ctags' transformed into 'exctags'
::

	$ ./configure --program-prefix=ex

Completely change program name, in this case it's important to remember
there is also 'etags' along 'ctags'
::

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'


