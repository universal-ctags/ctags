Nested quoted block
==========================================================

Various Autoconf language objects inside m4 quote quote blocks are not
tagged well.


For example `USE_STDBOOL_H` in the following code snippet is not tagged.

.. code-block:: m4

    AC_CHECK_HEADERS([stdbool.h],
    [
	    AC_DEFINE([USE_STDBOOL_H])
    ])
