.. _ctags_lang-rmarkdown(7):

======================================================================
ctags-lang-rmarkdown
======================================================================

Random notes about tagging R Markdown source code with Universal Ctags

:Version: 5.9.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ...--extras=+{subparser}{guest} --languages=+RMarkdown ...
|	**ctags** ...--extras=+{subparser}{guest} --language-force=RMarkdown ...
|	**ctags** ...--extras=+{subparser}{guest} --map-RMarkdown=+.rmd ...

DESCRIPTION
-----------
RMarkdown parser is an exclusive subparser stacked on top of the Markdown parser.
It works when:

* the Markdown parser is enabled,
* the ``subparser`` extra is enabled, and
* the RMarkdown parser itself is enabled.

The RMarkdown parser extends the way of detecting **codeblocks** from the
Markdown parser for running guest parsers on **code chunks**.

The Markdown parser expects the following syntax for codeblocks

.. code-block::

	```language-name
		...
	```

For an example

.. code-block::

	```r
		...
	```

The RMarkdown parser accepts the following syntax for code chunks
as the markdown parser accepts codeblocks

.. code-block::

	```{language-name chunk-label, ...}
		...
	```

For an example

.. code-block::

	```{r precalc fig.height=4}
		...
	```

Give `--extras=+{guest}` for enabling ``guest`` to command line if you
want to run proper parsers on inside code chunks.

The parser extrats chunk labels coming after `language-name` as
`chunklabel` kind objcts. The kind is enabled by default.

EXAMPLES
--------
"input.rmd"

.. code-block:: RMarkdown

	# Section 1

	```{r myblock}
		zero_fun <- function () {
			return 0
		}
	```

	# Section 2

"output.tags"
with "--options=NONE --extras=+{guest} --fields=+KZln -o - input.rmd"

.. code-block:: tags

	Section 1	input.rmd	/^# Section 1$/;"	chapter	line:1	language:Markdown
	Section 2	input.rmd	/^# Section 2$/;"	chapter	line:9	language:Markdown
	myblock	input.rmd	/^```{r myblock}$/;"	chunklabel	line:3	language:RMarkdown
	zero_fun	input.rmd	/^	zero_fun <- function () {$/;"	function	line:4	language:R

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`, `R Markdown: The Definitive Guide <https://bookdown.org/yihui/rmarkdown/>`_
