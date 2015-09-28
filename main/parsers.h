/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to all language parsing modules.
*
*   To add a new language parser, you need only modify this single source
*   file to add the name of the parser definition function.
*/
#ifndef _PARSERS_H
#define _PARSERS_H

/* Add the name of any new parser definition function here */
#define PARSER_LIST \
	AdaParser, \
	AntParser, \
	AsmParser, \
	AspParser, \
	AwkParser, \
	BasicParser, \
	BetaParser, \
	ClojureParser, \
	CParser, \
	CppParser, \
	CssParser, \
	CsharpParser, \
	CobolParser, \
	DParser, \
	DiffParser, \
	DTSParser, \
	DosBatchParser, \
	EiffelParser, \
	ErlangParser, \
	FalconParser, \
	FlexParser, \
	FortranParser, \
	GoParser, \
	HtmlParser, \
	JavaParser, \
	JavaScriptParser, \
	JsonParser, \
	LispParser, \
	LuaParser, \
	MakefileParser, \
	MatLabParser, \
	ObjcParser, \
	OcamlParser, \
	PascalParser, \
	PerlParser, \
	Perl6Parser, \
	PhpParser, \
	PythonParser, \
	RParser, \
	RexxParser, \
	RstParser, \
	RubyParser, \
	RustParser, \
	SchemeParser, \
	ShParser, \
	SlangParser, \
	SmlParser, \
	SqlParser, \
	TclParser, \
	TexParser, \
	VeraParser, \
	VerilogParser, \
	SystemVerilogParser, \
	VhdlParser, \
	VimParser, \
	WindResParser, \
	YaccParser, \
	ZephirParser

#endif  /* _PARSERS_H */

/* vi:set tabstop=4 shiftwidth=4: */
