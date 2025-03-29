/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2014-2016, Colomban Wendling <ban@herbesfolles.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef CTAGS_PARSER_PYTHON_H
#define CTAGS_PARSER_PYTHON_H

typedef enum {
	PYTHON_CLASS_KIND,
	PYTHON_FUNCTION_KIND,
	PYTHON_METHOD_KIND,
	PYTHON_VARIABLE_KIND,
	PYTHON_NAMESPACE_KIND,
	PYTHON_MODULE_KIND,
	PYTHON_UNKNOWN_KIND,
	PYTHON_PARAMETER_KIND,
	PYTHON_LOCAL_VARIABLE_KIND,
	PYTHON_COUNT_KIND
} pythonKind;

typedef enum {
	PYTHON_MODULE_IMPORTED,
	PYTHON_MODULE_NAMESPACE,
	PYTHON_MODULE_INDIRECTLY_IMPORTED,
	PYTHON_MODULE_ENTRY_POINT,
} pythonModuleRole;

typedef enum {
	PYTHON_FUNCTION_ENTRY_POINT,
} pythonFunctionRole;

#endif	/* CTAGS_PARSER_PYTHON_H */
