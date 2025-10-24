.. _ctags-lang-r(7):

==============================================================
ctags-lang-r
==============================================================

Random notes about tagging R source code with Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+R ...
|	**ctags** ... --language-force=R ...
|	**ctags** ... --map-R=+.r ...

DESCRIPTION
-----------
This man page gathers random notes about tagging R source code
with Universal Ctags.

Kinds
-----------
If a variable gets a value returned from a *well-known constructor*
and the variable appears for the first time in the current input file,
the R parser makes a tag for the variable and attaches a kind
associated with the constructor to the tag regardless of whether
the variable appears in the top-level context or a function.

Well-known constructor and kind mapping

	============  ==================
	Constructor   kind
	============  ==================
	function()    function
	c()           vector
	list()        list
	data.frame()  dataframe
	============  ==================

If a variable doesn't get a value returned from one of well-known
constructors, the R parser attaches ``globalVar`` or ``functionVar`` kind
to the tag for the variable depending on the context.

Here is an example demonstrating the usage of the kinds:

"input.r"

.. code-block:: R

	G <- 1
	v <- c(1, 2)
	l <- list(3, 4)
	d <- data.frame(n = v)
	f <- function(a) {
		g <- function (b) a + b
		w <- c(1, 2)
		m <- list (3, 4)
		e <- data.frame(n = w)
		L <- 2
	}

"output.tags"
with "--options=NONE --sort=no --fields=+KZ -o - input.r"

.. code-block:: tags

	G	input.r	/^G <- 1$/;"	globalVar
	v	input.r	/^v <- c(1, 2)$/;"	vector
	l	input.r	/^l <- list(3, 4)$/;"	list
	d	input.r	/^d <- data.frame(n = v)$/;"	dataframe
	n	input.r	/^d <- data.frame(n = v)$/;"	nameattr	scope:dataframe:d
	f	input.r	/^f <- function(a) {$/;"	function
	g	input.r	/^	g <- function (b) a + b$/;"	function	scope:function:f
	w	input.r	/^	w <- c(1, 2)$/;"	vector	scope:function:f
	m	input.r	/^	m <- list (3, 4)$/;"	list	scope:function:f
	e	input.r	/^	e <- data.frame(n = w)$/;"	dataframe	scope:function:f
	n	input.r	/^	e <- data.frame(n = w)$/;"	nameattr	scope:dataframe:f.e
	L	input.r	/^	L <- 2$/;"	functionVar	scope:function:f

.. TODO:

   - other kinds
   - operators for assignment, <-, <<-, ->>, ->, =
   - illuminating duplicated tags
   - fields (constructor, assignmentop)
   - sub parsers

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`
