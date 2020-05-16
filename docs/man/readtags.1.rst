.. _readtags(1):

==============================================================
readtags
==============================================================
--------------------------------------------------------------
Find tag file entries matching specified names
--------------------------------------------------------------
:Version: 0.0.0
:Manual group: Universal-ctags
:Manual section: 1

SYNOPSIS
--------
|	**readtags** -h | --help
|	**readtags** (-H | --help-expression) (filter|sorter)
|	**readtags** [OPTION]... ACTION

DESCRIPTION
-----------

readtags lists or finds tag entries in a tags file.
...

ACTIONS
-------

``-l``
	Equivalent to ``--list``.

``--list``
	List regular tags.

``[-] NAME``
	List regular tags matching NAME.
	"-" as NAME indicates arguments after this as NAME even if they start with -.

``-D``
	Equivalent to ``--list-pseudo-tags``.

``--list-pseudo-tags``
	List pseudo tags.

OPTIONS
-------

...

EXPRESSION
----------

...

Filtering
~~~~~~~~~

...

Sorting
~~~~~~~

...

BUGS
----

...


SEE ALSO
--------
See :ref:`tags(5) <tags(5)>` for the details of tags file format.

See :ref:`ctags-client-tools(7) <ctags-client-tools(7)>` for the tips writing a
tool utilizing tags file.

The official Universal-ctags web site at:

https://ctags.io/


The git repository for the library used in readtags command:

https://github.com/universal-ctags/libreadtags

AUTHOR
------

YOUR NAME HERE


CREDITS
-------

Universal-ctags project
https://ctags.io/

Darren Hiebert <dhiebert@users.sourceforge.net>
http://DarrenHiebert.com/

The readtags command and libreadtags maintained at Universal-ctags
are derrived from readtags.c and readtags.h developd at
http://ctags.sourceforge.net.
