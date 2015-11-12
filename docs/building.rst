Building ctags
=============================================================================


.. contents:: `Table of contents`
	:depth: 3
	:local:


.. include:: autotools.rst


Build system add possibility to change program name
---------------------------------------------------------------------

On some systems, like certain BSDs, there is already a 'ctags' program in the base
system, so it is somewhat inconvenient to have the same name for
universal-ctags. During the ``configure`` stage you can now change
the output executable name.

To add a prefix 'ex' which will result in 'ctags' being transformed into 'exctags'

.. code-block:: bash

	$ ./configure --program-prefix=ex

To completely change the program's name run the following. Please remember there is also an 'etags' along 'ctags'

.. code-block:: bash

	$ ./configure --program-transform-name='s/ctags/my_ctags/; s/etags/myemacs_tags/'


.. include:: windows.rst
.. include:: osx.rst
