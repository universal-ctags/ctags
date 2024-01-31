.. _ctags-lang-i18nrubgem(7):

==============================================================
ctags-lang-i18nrubgem
==============================================================

Random notes about tagging input for I18n Ruby Gem with Universal Ctags

:Version: 6.1.0
:Manual group: Universal Ctags
:Manual section: 7
:Expected feature: yaml

SYNOPSIS
--------
|	**ctags** ... --extras=+s --languages=+I18nRubyGem ...
|	**ctags** ... --extras=+s --language-force=I18nRubyGem ...
|	**ctags** ... --extras=+s --map-I18nRubyGem=+.yaml ...
|	**ctags** ... --extras=+s --map-I18nRubyGem=+.yml ...

DESCRIPTION
-----------
This man page gathers random notes about tagging input for I18n Ruby
Gem [RUBYI18N]_. I18nRubyGem is a subparser running on Yaml parser. If a
top-level scalar token of a given Yaml source input is a local name [ISOCODES]_,
I18nRubyGem activates itself. If not, I18nRubyGem does nothing.

"input.yaml"

.. code-block:: YAML

	---
	ja:
	  msg:
	    error: エラー
	    function: 関数

"output.tags"
with "--options=NONE -o - --fields=+E input.yaml"

.. code-block:: tags

	error	input.yaml	/^    error: エラー$/;"	k	keyInMiddle:ja.msg	extras:subparser
	function	input.yaml	/^    function: 関数$/;"	k	keyInMiddle:ja.msg	extras:subparser
	ja.msg.error	input.yaml	/^    error: エラー$/;"	k	keyInMiddle:ja.msg	extras:subparser,localeful
	ja.msg.function	input.yaml	/^    function: 関数$/;"	k	keyInMiddle:ja.msg	extras:subparser,localeful
	msg.error	input.yaml	/^    error: エラー$/;"	k	locale:ja	extras:subparser,localeless
	msg.function	input.yaml	/^    function: 関数$/;"	k	locale:ja	extras:subparser,localeless


With the options, the parser emits three tag entries for a key:
"error", "js.msg.error", and "msg.error" for the key "error", The
parser emits "error" at the first line always.

To emit "ja.msg.error", a ``localeful`` extra tag, at the fifth line
always. If you specify ``--extra=+q`` or ``--extra=+{qualified}``,
ctags emits the same tag twice: a ``localeful`` tag, and a ``qualified`` tag.
You can suppress the ``localeful`` extra tags., specify
``--extras-I18nRubyGem=-{localeful}``.

To emit "msg.error", a ``localeless`` extra tag, at the fifth line
always. The parser creates a ``localeless`` extra tag by truncating
the first component (e.g. "ja.") from the associated full qualified
extra tag (e.g. "ja.msg.error").  ``localeless`` extra is enabled by
default. If you don't need ``localeless`` extra tags, specify
``--extras-I18nRubyGem=-{localeless}``.

The parser doesn't make tag entries for top-level and mid-level components like
``ja`` and ``msg`` by default. If you need them, specify
``--kinds-I18nRubyGem=+{locale}`` and/or
``--kinds-I18nRubyGem=+{keyInMiddle}``.

KNOWN BUGS
----------
The mechanism activating I18nRubyGem subparser doesn't work well for YAML
source input containing multiple documents.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`

REFERENCES
----------
.. [RUBYI18N] Ruby I18n, https://github.com/ruby-i18n/i18n
.. [ISOCODES] iso-codes, https://salsa.debian.org/iso-codes-team/iso-codes
