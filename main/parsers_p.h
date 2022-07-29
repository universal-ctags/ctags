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
	XmlParser, \
	XsltParser
#else
#define XML_PARSER_LIST
#endif

#ifdef HAVE_LIBYAML
#define YAML_PARSER_LIST						\
	YamlParser,									\
	AnsiblePlaybookParser, \
	OpenAPIParser, \
	YamlFrontMatter
#else
#define YAML_PARSER_LIST
#endif

#ifdef HAVE_PACKCC
#define PEG_PARSER_LIST						\
	VarlinkParser, \
	KotlinParser,  \
	ThriftParser,  \
	ElmParser
#else
#define PEG_PARSER_LIST
#endif

#ifdef HAVE_PCRE2
#define OPTLIB2C_PCRE2_PARSER_LIST             \
   RDocParser
#else
#define OPTLIB2C_PCRE2_PARSER_LIST
#endif

/* Add the name of any new parser definition function here */
#define PARSER_LIST \
	AbaqusParser, \
	AbcParser, \
	AdaParser, \
	AntParser, \
	AsciidocParser, \
	AsmParser, \
	AspParser, \
	AutoconfParser, \
	AutoItParser, \
	AutomakeParser, \
	AwkParser, \
	BasicParser, \
	BetaParser, \
  	BibtexParser, \
	ClojureParser, \
	CMakeParser, \
	CParser, \
	CppParser, \
	CPreProParser, \
	CssParser, \
	CsharpParser, \
	CtagsParser, \
	CobolParser, \
	CobolFreeParser, \
	CobolVariableParser, \
	CUDAParser, \
	DParser, \
	DiffParser, \
	DtdParser, \
	DTSParser, \
	DosBatchParser, \
	EiffelParser, \
	ElixirParser, \
	EmacsLispParser, \
	ErlangParser, \
	FalconParser, \
	FlexParser, \
	FortranParser, \
	FrontMatterParser, \
	FunctionParametersParser, \
	FyppParser,	   \
	GdbinitParser, \
	GDScriptParser, \
	GemSpecParser, \
	GoParser, \
	GPerfParser, \
	HaskellParser, \
	HaxeParser, \
	HtmlParser, \
	IniconfParser, \
	InkoParser, \
	IPythonCellParser, \
	ITclParser, \
	JavaParser, \
	JavaPropertiesParser, \
	JavaScriptParser, \
	JsonParser, \
	JuliaParser, \
	KconfigParser, \
	LdScriptParser, \
	LEXParser, \
	LispParser, \
	LiterateHaskellParser, \
	LuaParser, \
	M4Parser, \
	ManParser, \
	MakefileParser, \
	MarkdownParser, \
	MatLabParser, \
	MesonParser, \
	MesonOptionsParser, \
	MooseParser, \
	MyrddinParser, \
	NsisParser, \
	ObjcParser, \
	OldCppParser, \
	OldCParser, \
	OcamlParser, \
	OrgParser, \
	PasswdParser, \
	PascalParser, \
	PerlParser, \
	Perl6Parser, \
	PhpParser, \
	PodParser, \
	PowerShellParser, \
	ProtobufParser, \
	PuppetManifestParser, \
	PythonParser, \
	PythonLoggingConfigParser, \
	QemuHXParser, \
	QtMocParser, \
	RMarkdownParser, \
	RParser, \
	RakeParser, \
	R6ClassParser, \
	RSpecParser, \
	RexxParser, \
	RobotParser, \
	RpmMacrosParser, \
	RpmSpecParser, \
	RstParser, \
	RubyParser, \
	RustParser, \
	S4ClassParser, \
	SchemeParser, \
	SCSSParser, \
	ShParser, \
	SlangParser, \
	SmlParser, \
	SqlParser, \
	SystemdUnitParser, \
	SystemTapParser, \
	TclParser, \
	TclOOParser, \
	TexParser, \
	TexBeamerParser, \
	TTCNParser, \
	Txt2tagsParser, \
	TypeScriptParser, \
	VeraParser, \
	VerilogParser, \
	SystemVerilogParser, \
	VhdlParser, \
	VimParser, \
	WindResParser, \
	YACCParser, \
	YumRepoParser, \
	ZephirParser

#endif  /* CTAGS_MAIN_PARSERS_H */
