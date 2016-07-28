/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to all language parsing modules.
*
*   To add a new language parser, you need only modify this single input
*   file to add the name of the parser definition function.
*/
#ifndef CTAGS_MAIN_PARSERS_H
#define CTAGS_MAIN_PARSERS_H

#ifdef HAVE_LIBXML
#define XML_PARSER_LIST \
	DbusIntrospectParser, \
	GladeParser,  \
	Maven2Parser, \
	PlistXMLParser, \
	RelaxNGParser, \
	SvgParser, \
	XsltParser
#else
#define XML_PARSER_LIST
#endif

/* Add the name of any new parser definition function here */
#define PARSER_LIST \
	AdaParser, \
	AntParser, \
	AsmParser, \
	AspParser, \
	AutoconfParser, \
	AutomakeParser, \
	AwkParser, \
	BasicParser, \
	BetaParser, \
	ClojureParser, \
	CoffeeScriptParser, \
	CParser, \
	CppParser, \
	CssParser, \
	CsharpParser, \
	CtagsParser, \
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
	GdbinitParser, \
	GoParser, \
	HtmlParser, \
        IniconfParser, \
	JavaParser, \
	JavaPropertiesParser, \
	JavaScriptParser, \
	JsonParser, \
	LispParser, \
	LuaParser, \
	M4Parser, \
	ManParser, \
	MakefileParser, \
	MatLabParser, \
	MyrddinParser, \
	ObjcParser, \
	OldCppParser, \
	OldCParser, \
	OcamlParser, \
	PascalParser, \
	PerlParser, \
	Perl6Parser, \
	PhpParser, \
	PodParser, \
	ProtobufParser, \
	PythonParser, \
	PythonLoggingConfigParser, \
	RParser, \
	RexxParser, \
	RpmSpecParser, \
	RstParser, \
	RubyParser, \
	RustParser, \
	SchemeParser, \
	ShParser, \
	SlangParser, \
	SmlParser, \
	SqlParser, \
	SystemdUnitParser, \
	TclParser, \
	TexParser, \
	TTCNParser, \
	VeraParser, \
	VerilogParser, \
	SystemVerilogParser, \
	VhdlParser, \
	VimParser, \
	WindResParser, \
	YaccParser, \
	YumRepoParser, \
	ZephirParser

#endif  /* CTAGS_MAIN_PARSERS_H */
