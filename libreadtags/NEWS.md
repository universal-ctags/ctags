# Version ???

- delete debug output automatically printed when DEBUG is defiend in
  build-time.

- fix potential crashes trigged when passing NULL as `file` parameter
  to the API functions. Provided by rootkea (GitHub account).

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
