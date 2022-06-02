# Version ???

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
