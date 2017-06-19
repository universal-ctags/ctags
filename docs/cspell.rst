*Cspell* spell checking
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

-----

* ``make cspell`` reports unknown words.
  After verifying the reported words are correct you should
  add them to files under *dictfiles* directory.

* ``cspell`` target assumes the names used in ctags source code are
  correctly spelled. Such names can be added semi-automatically; use
  ``make dicts`` targets. It updates files prefixed with *GENERATED-*
  under *dictfiles*.

* Either semi-automatically generated or adding by manually, files
  under *dictfiles* directory should be installed to Universal-ctags
  git repository.

* ``make cspell`` makes and users *SPELL_CHECKING.TMP* at the top of source
  code directory as temporary working space.

* ``cspell`` target depends on GNU aspell library. If the library is linked
  to, ``ctags --list-features`` prints ``aspell``.

