# Version ???

- read input fields, values at the second column in a tag file, with
  unescaping if !_TAG_OUTPUT_MODE is "u-ctags" and
  !_TAG_OUTPUT_FILESEP is "slash" in the tag file.

- LT_VERSION ?:?:?

# Version 0.2.1

- use "m" mode flag of fopen only when compiling with glibc 2.3 or higher.

- LT_VERSION 2:1:1

	- no change in public interface

# Version 0.2.0

- delete debug output automatically printed when DEBUG is defiend in
  build-time.

- fix potential crashes trigged when passing NULL as `file` parameter
  to the API functions. Provided by rootkea (GitHub account).

- add a new error constant `TagErrnoFileMaybeTooBig` to represent
  the case that the given tags file is too large for the platform APIs
  (ftell and fseek) used in libreadtags.
  See https://github.com/universal-ctags/libreadtags/issues/36 about the
  background of this change.

- allow the library to read larger (> 2G) tag files on Win32 platform.
  The tag file size was limited to 2G on the platform because the library
  used fseek and ftell. In this version, they are replaced with _fseeki64 and
  _ftelli64.

- add a new API function (tagsFindPseudoTag) for finding a pseudo tag for
  given name.

- Use mmap(2) when opening a tags file if fopen() supports "m" mode flag.

- LT_VERSION 2:0:1

	- extend the API

		- add a constant: TagErrnoFileMaybeTooBig
		- add a function: tagsFindPseudoTag

# Version 0.1.0

- propagate internal errors to caller

- LT_VERSION 1:0:0

	- extend the API for the error propagation

		- add tagsGetErrno function
		- add tagErrno eum type

	- break the API

		- rename sortType to tagSortType for avoiding name conflictions
		  However, sortType is still defined as a macro.
		  See readtags.h
