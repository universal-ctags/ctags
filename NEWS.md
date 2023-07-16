# v6.1.0 (WORK IN PROGRESS)

Many *compatible* changes are added to parsers. See "Merged pull
requests" section.

## Highlight of changes in readtags command
* Enhancement: add `-A, --absolute-input` and `-C, --canonicalize-input` options. 
  See readtags(1) for more details.
* Bugfix: make --formater options works.

## New parsers
* Quarto: new parser by masatake · Pull Request #3677
	+  Quarto by asshah4 · Pull Request #3643
* Terraform: new parser by masatake · Pull Request #3684 
	+ Terraform (HCL) (*.tf): new parser by Haggus · Pull Request #3683
	+ Terraform (HCL) (*.tf): new parser by antonysouthworth-halter · Pull Request #2952
  
## Changes about parser specific kinds, roles, fields, and extras
* AutoIt
	+ Drop `$` from tags for variables names.
* Fortran
	+ New extra `linkName`.
* Kconfig
	+ New kind `variable`
* Markdown
	+ New kind  `hashtag`
* SystemTap
	+ New role `attached` for `probe` kind
* SystemVerilog
	+ New kind `define`

## Merged pull requests

> This list is imperfect. masatake cleaned up some pull requests before
> merging. Though his names is used in "... by ...", his is not the
> primary contributor of the pull requests. See git log for more
> defatils.

* readtags: canonicalize the input file name based on CWD ptag by masatake · Pull Request #3304
* Perl: skip string literals when collecting heredoc markers by masatake · Pull Request #3592
* HTML: introduce a specialized tokenizer for script areas by masatake · Pull Request #3598
* readtags: unescape input field (a.k.a {tagfile}) only if TAG_OUTPUT_MODE is u-ctags and TAG_OUTPUT_FILESEP is slash by masatake · Pull Request #3599
* C++,C: record consteval, constinit, thread_local, and __thread to properties: field by masatake · Pull Request #3602
* Systemtap: add new role "attached" for "probe" kind , and run CPreProcessor as a guest parser  by masatake · Pull Request #3607
* GitHub Actions: fix testing-mac.yml Homebrew upgrade python@3.10 and python@3.11 failed problem by leleliu008 · Pull Request #3610
* Org: optimize by masatake · Pull Request #3611
* main: introduce --_paramdef-<LANG>=<NAME>,<DESCRIPTION> option by masatake · Pull Request #3613
* Various preparations by masatake · Pull Request #3617
* Cxx: support typeof and __typeof__ keywords of the gcc extension by masatake · Pull Request #3621
* C++,ObjectiveC,C,main: fix  the broken selector for .h by masatake · Pull Request #3622
* C,Asm,LdScript: minor fixes by masatake · Pull Request #3623
* Markdown: fix the condition to detect code blocks by masatake · Pull Request #3626
* CPreProcessor:  don't include the newline after a backslash in string or char literals by masatake · Pull Request #3629
* Ldscript:  improve tagging versions in VERSION commands by masatake · Pull Request #3631
* Kconfig: support the  macro language by masatake · Pull Request #3632
* Ruby:  handle curly bracket by masatake · Pull Request #3633
* Tcl: don't include '"' char as a part of identifiers by masatake · Pull Request #3639
* GitHubActions: fix testing-openbsd.yml automake version broken issue by leleliu008 · Pull Request #3640
* Cxx: scan the cork queue instead of the symtab to fill nth fields by masatake · Pull Request #3642
* Fix typo in conditional in C++ parser by al42and · Pull Request #3646
* Verilog: all text macro map to new kindDefinition:define  by my2817 · Pull Request #3653
* SQL: Skip PL/SQL selection directives and add sanity check for inquiry directive size by techee · Pull Request #3654
* Powershell: fix string escape issue by iaalm · Pull Request #3661
* main: add quick path for looking up too long strings in the keyword table by techee · Pull Request #3664
* main,cosmetic: fix misspelling by jafl · Pull Request #3667
* Fortran: add "linkName" extra by masatake · Pull Request #3671
* misc/review: add "accept" command to the Tmain inspector by masatake · Pull Request #3672
* docs(web): add ctags-lang-kconfig.7.rst by masatake · Pull Request #3673
* SQL: handle "DATABASE" and "SCHEMA" keywords specially only when they come after "CREATE" by masatake · Pull Request #3674
* Ruby: skip if __DATA__ is found by masatake · Pull Request #3676
* Quarto: new parser by masatake · Pull Request #3677
* main: Don't strdup the inputFileName when storing a tag to the corkQueue by masatake · Pull Request #3682
* Terraform: new parser by masatake · Pull Request #3684
* Optscript: add _foreignreftag operator by masatake · Pull Request #3686
* main,refactor: delete 'inCorkQueue' parameter from attachParserField() by masatake · Pull Request #3687
* AutoIt: Slightly optimize parsing #region by b4n · Pull Request #3689
* vstring: Avoid int -> char truncation warnings by b4n · Pull Request #3690
* C++: accept prototypes starting from :: operator by masatake · Pull Request #3694
* main: revise bit fields in tagEntryInfo by masatake · Pull Request #3695
* autoit: Drop $ from variable names by techee · Pull Request #3697
* main: add missing const modifiers by masatake · Pull Request #3699
* D: parse user-defined attributes by ntrel · Pull Request #3701
* Circleci: add  fedora 38 by masatake · Pull Request #3705
* C-based parsers,style: adjust placements of "{" after if by masatake · Pull Request #3706
* D: set template members parent name by ntrel · Pull Request #3707
* D: parse contract expressions by ntrel · Pull Request #3708
* D: parse const(T), immutable, inout and shared type qualifiers by ntrel · Pull Request #3709
* D: remove `overload`, not a keyword by ntrel · Pull Request #3710
* Suppress warning by masatake · Pull Request #3714
* D: fix parsing parameter with pointer by ntrel · Pull Request #3715
* D: parse template instance types by ntrel · Pull Request #3716
* GDScript : mark xtag bit for implicitClass xtags by masatake · Pull Request #3717
* Fortran: fix wrongly specified xtag type by masatake · Pull Request #3718
* docs(web): sphinx minor fixes by masatake · Pull Request #3719
* Verilog: support virtual interface variables by hirooih · Pull Request #3720
* units.py: don't use color if NO_COLOR is specified by masatake · Pull Request #3721
* verilog: treat a text-macro as an identifier by hirooih · Pull Request #3722
* readtags: fix a bug compiling a formatter wrongly if giving --formater long option by masatake · Pull Request #3723
* erlang: Fix crash parsing directives longer than 31 characters by b4n · Pull Request #3726
* lregex: optimize substitute() by masatake · Pull Request #3728
* Ruby: don't make a scope for "Class.new..."  with no block by masatake · Pull Request #3733
* Fix many calls to ctype functions by b4n · Pull Request #3734
* libreadtags: pull the latest version by masatake · Pull Request #3735
* LdScript: support SORT keyword by masatake · Pull Request #3743
* Markdown: add hashtags functionality by jiangyinzuo · Pull Request #3747
* Markdown: accept sections in the line started from spaces by masatake · Pull Request #3750
* Markdown: set the parser-version 1.1 by masatake · Pull Request #3752
* docs(web),cosmetic: update optlib.rst typo removed by artemnovichenko · Pull Request #3753
* main: make --verison and --help options work even if a broken .ctags is given by masatake · Pull Request #3756
* Fix typo in asm.c by eltociear · Pull Request #3757
* JavaScript: move unit test from review-needed to parser-javascript by jafl · Pull Request #3760
* JavaScript: treat "get" and "set" as function names… by jafl · Pull Request #3761
* JavaScript: allow array index after 'this' keyword by jafl · Pull Request #3762
* JavaScript: report object fields specified via shortcut syntax by jafl · Pull Request #3763
* Fix compiler warnings by jafl · Pull Request #3764
* JavaScript: additional fix for get & set, when specified in prototype by jafl · Pull Request #3765
* dsl: use strtol instead of es_read_from_string by masatake · Pull Request #3769
* Javascript: multiple prototype assignments by jafl · Pull Request #3770

## Issues close or partially closed via above pull requests
* main: ctags option processing fails if "Language already defined", including ctags --help · Issue #2935
* Markdown: tagging a chapter unexpectedly · Issue #3748
* units.py: support NO_COLOR (https://no-color.org/) · Issue #3688
* C++: fully qualified return type breaks parsing prototypes · Issue #3693
* SQL:  Warning: ignoring null tag in ... /src/test/regress/sql/collate.icu.utf8.sql(line: 412) · Issue #3636
* Fortran: Improve handling of case insensitivity · Issue #3668
* C/C++: Endless parse large file · Issue #3634
* Tcl parser - Ctags can not generating tags for some proc · Issue #3638
* Markdown: comments within shell code of markdown fiiles are recognized as chapters · Issue #3625
* C: support typeof gcc extention · Issue #3620
* main: use escape sequences when printing pseudo tags as explainged in tags(5) · Issue #3577
* readtags: improper handling of escape sequences in input field · Issue #3559
* Perl: Incorrectly sees << inside a string as start of a heredoc · Issue #3588
* C++: Output information on `constexpr` and `consteval` functions · Issue #3539
* html: apostrophe in JavaScript comment breaks guest parser · Issue #3581
* HTML: HTML comment starter in JavaScript area · Issue #3597
* html: apostrophe in JavaScript comment breaks guest parser · Issue #3581
* HTML: HTML comment starter in JavaScript area · Issue #3597
