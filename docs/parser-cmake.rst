.. _cmake:

======================================================================
CMake parser
======================================================================

The CMake parser is used for ``.cmake`` and ``CMakeLists.txt`` files.
It generates tags for the following items:

- User-defined functions
- User-defined macros
- User-defined options created by ``option()``
- Variables defined by ``set()``
- Targets created by ``add_custom_target()``, ``add_executable()`` and ``add_library()``

The parser uses the experimental multi-table regex ``ctags`` options to
perform the parsing and tag generation.

Caveats:

	Names that are ``${}`` references to variables are not tagged.

	For example, given the following::

	    set(PROJECT_NAME_STR ${PROJECT_NAME})
	    add_executable( ${PROJECT_NAME_STR} ... )
	    add_custom_target( ${PROJECT_NAME_STR}_tests ... )
	    add_library( sharedlib ... )

	...the variable ``PROJECT_NAME_STR`` and target ``sharedlib`` will both be tagged,
	but the other targets will not be.


References:

	https://cmake.org/cmake/help/latest/manual/cmake-language.7.html
