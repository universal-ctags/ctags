/*
 * Generated by ./misc/optlib2c from optlib/puppetManifest.ctags, Don't edit this manually.
 */
#include "general.h"
#include "parse.h"
#include "routines.h"


static void initializePuppetManifestParser (const langType language CTAGS_ATTR_UNUSED)
{

	addLanguageRegexTable (language, "main");
	addLanguageRegexTable (language, "separator");
	addLanguageRegexTable (language, "any");
	addLanguageRegexTable (language, "ignoreWhiteSpace");
	addLanguageRegexTable (language, "end");
	addLanguageRegexTable (language, "endWithPop");
	addLanguageRegexTable (language, "ssliteral");
	addLanguageRegexTable (language, "dsliteral");
	addLanguageRegexTable (language, "comment");
	addLanguageRegexTable (language, "blockStart");
	addLanguageRegexTable (language, "blockHead");
	addLanguageRegexTable (language, "blockHeadPopAtLast");
	addLanguageRegexTable (language, "block");
	addLanguageRegexTable (language, "classStart");
	addLanguageRegexTable (language, "resourceBlock");
	addLanguageRegexTable (language, "skipLiteral");
	addLanguageRegexTable (language, "skipBlock");
	addLanguageRegexTable (language, "skipArray");
	addLanguageRegexTable (language, "skipArgs");
	addLanguageRegexTable (language, "skipCollector");
	addLanguageRegexTable (language, "var");
	addLanguageRegexTable (language, "defineStart");
	addLanguageRegexTable (language, "caseStart");
	addLanguageRegexTable (language, "ifStart");
	addLanguageRegexTable (language, "nodeStart");
	addLanguageRegexTable (language, "comment_multiline");
	addLanguageRegexTable (language, "comment_oneline");
	addLanguageRegexTable (language, "resourceName");
	addLanguageRegexTable (language, "resourceNameInArray");
	addLanguageRegexTable (language, "resourceBody");
	addLanguageRegexTable (language, "resourceArray");
	addLanguageRegexTable (language, "resourceCollector");
	addLanguageRegexTable (language, "varexpr");
	addLanguageRegexTable (language, "caseBlock");

	addLanguageTagMultiTableRegex (language, "main",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^<<?\\|",
	                               "", "", "{tenter=skipCollector}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\$",
	                               "", "", "{tenter=var}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^@?::[a-zA-Z0-9:]+[ \t\n]*\\{",
	                               "", "", "{tenter=resourceBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^@?[a-zA-Z][a-zA-Z0-9:]*[ \t\n]*\\{",
	                               "", "", "{tenter=resourceBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^class[ \t\n]+",
	                               "", "", "{tenter=classStart}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^define[ \t\n]+",
	                               "", "", "{tenter=defineStart}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^case[ \t\n]+",
	                               "", "", "{tenter=caseStart}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^(if|elsif|else|unless)[ \t\n]+",
	                               "", "", "{tenter=ifStart}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^node[ \t\n]+",
	                               "", "", "{tenter=nodeStart}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\(",
	                               "", "", "{tenter=skipArgs}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\{",
	                               "", "", "{tenter=skipBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^.",
	                               "", "", "{tenter=separator}", NULL);
	addLanguageTagMultiTableRegex (language, "separator",
	                               "^[a-zA-Z0-9]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "separator",
	                               "^",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "any",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "ignoreWhiteSpace",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "end",
	                               "^",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "endWithPop",
	                               "^",
	                               "", "", "{tleave}{scope=pop}", NULL);
	addLanguageTagMultiTableRegex (language, "ssliteral",
	                               "^'",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "ssliteral",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "dsliteral",
	                               "^\"",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "dsliteral",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "comment",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "comment",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^@?::[a-zA-Z0-9:]+[ \t\n]*\\{",
	                               "", "", "{tenter=resourceBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^@?[a-zA-Z][a-zA-Z0-9:]*[ \t\n]*\\{",
	                               "", "", "{tenter=resourceBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^class[ \t\n]+",
	                               "", "", "{tenter=classStart}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^define[ \t\n]+",
	                               "", "", "{tenter=defineStart}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^case[ \t\n]+",
	                               "", "", "{tenter=caseStart}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^(if|elsif|else|unless)[ \t\n]+",
	                               "", "", "{tenter=ifStart}", NULL);
	addLanguageTagMultiTableRegex (language, "blockStart",
	                               "^node[ \t\n]+",
	                               "", "", "{tenter=nodeStart}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHead",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHead",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHead",
	                               "^\\{",
	                               "", "", "{tenter=block,end}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHead",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "blockHeadPopAtLast",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHeadPopAtLast",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHeadPopAtLast",
	                               "^\\{",
	                               "", "", "{tenter=block,endWithPop}", NULL);
	addLanguageTagMultiTableRegex (language, "blockHeadPopAtLast",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^<<?\\|",
	                               "", "", "{tenter=skipCollector}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^@?::[a-zA-Z0-9:]+[ \t\n]*\\{",
	                               "", "", "{tenter=resourceBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^@?[a-zA-Z][a-zA-Z0-9:]*[ \t\n]*\\{",
	                               "", "", "{tenter=resourceBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^class[ \t\n]+",
	                               "", "", "{tenter=classStart}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^define[ \t\n]+",
	                               "", "", "{tenter=defineStart}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^case[ \t\n]+",
	                               "", "", "{tenter=caseStart}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^(if|elsif|else|unless)[ \t\n]+",
	                               "", "", "{tenter=ifStart}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^node[ \t\n]+",
	                               "", "", "{tenter=nodeStart}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^;?[ \t\n]*\\}",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^;",
	                               "", "", "{tjump=resourceBlock}{scope=pop}", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^:",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "block",
	                               "^.",
	                               "", "", "{tenter=separator}", NULL);
	addLanguageTagMultiTableRegex (language, "classStart",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "classStart",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "classStart",
	                               "^(::[a-zA-Z0-9:]+)",
	                               "\\1", "c", "{tenter=blockHead,endWithPop}{scope=push}", NULL);
	addLanguageTagMultiTableRegex (language, "classStart",
	                               "^([a-zA-Z][a-zA-Z0-9:]*)",
	                               "\\1", "c", "{tenter=blockHead,endWithPop}{scope=push}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^'",
	                               "", "", "{tenter=resourceName}{_advanceTo=0start}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^\"",
	                               "", "", "{tenter=resourceName}{_advanceTo=0start}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^\\[",
	                               "", "", "{tenter=resourceArray}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^\\}",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBlock",
	                               "^.",
	                               "", "", "{tenter=resourceBody}{scope=push}{placeholder}", NULL);
	addLanguageTagMultiTableRegex (language, "skipLiteral",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipLiteral",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^\\{",
	                               "", "", "{tenter=skipBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^\\}",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "skipBlock",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^\\[",
	                               "", "", "{tenter=skipArray}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^\\]",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArray",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^\\(",
	                               "", "", "{tenter=skipArgs}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^\\)",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "skipArgs",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^<<?\\|",
	                               "", "", "{tenter=skipCollector}", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^\\|>>?",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "skipCollector",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "var",
	                               "^(::[a-zA-Z0-9_:]+)[ \t\n]*=",
	                               "\\1", "v", "{tenter=varexpr,end}", NULL);
	addLanguageTagMultiTableRegex (language, "var",
	                               "^([a-zA-Z_][a-zA-Z0-9_:]*)[ \t\n]*=",
	                               "\\1", "v", "{tenter=varexpr,end}", NULL);
	addLanguageTagMultiTableRegex (language, "defineStart",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "defineStart",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "defineStart",
	                               "^([a-zA-Z:][a-zA-Z0-9:]*)[ \n\t]*\\(",
	                               "\\1", "d", "{tenter=skipArgs,blockHeadPopAtLast}{scope=push}", NULL);
	addLanguageTagMultiTableRegex (language, "defineStart",
	                               "^([a-zA-Z:][a-zA-Z0-9:]*)[ \n\t]*\\{",
	                               "\\1", "d", "{tenter=block,endWithPop}{scope=push}", NULL);
	addLanguageTagMultiTableRegex (language, "caseStart",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "caseStart",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "caseStart",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "caseStart",
	                               "^\\{",
	                               "", "", "{tenter=caseBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "caseStart",
	                               "^}",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "caseStart",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^\\{",
	                               "", "", "{tenter=block,end}", NULL);
	addLanguageTagMultiTableRegex (language, "ifStart",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^'([^']+)'",
	                               "\\1", "n", "", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^\"([^\"]+)\"",
	                               "\\1", "n", "", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^\\{",
	                               "", "", "{tenter=block,end}", NULL);
	addLanguageTagMultiTableRegex (language, "nodeStart",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "comment_multiline",
	                               "^\\*/",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "comment_multiline",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "comment_oneline",
	                               "^\n",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "comment_oneline",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "resourceName",
	                               "^'([^']+)'",
	                               "\\1", "r", "{tenter=resourceBody,end}{scope=push}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceName",
	                               "^\"([^\"]+)\"",
	                               "\\1", "r", "{tenter=resourceBody,end}{scope=push}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceName",
	                               "^",
	                               "", "", "{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceNameInArray",
	                               "^'([^']+)'",
	                               "\\1", "r", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceNameInArray",
	                               "^\"([^\"]+)\"",
	                               "\\1", "r", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceNameInArray",
	                               "^",
	                               "", "", "{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^\\{",
	                               "", "", "{tenter=skipBlock}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^;",
	                               "", "", "{tleave}{scope=pop}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^\\}",
	                               "", "", "{tleave}{_advanceTo=0start}{scope=pop}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^<<?\\|",
	                               "", "", "{tenter=skipCollector}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceBody",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^'",
	                               "", "", "{tenter=resourceNameInArray}{_advanceTo=0start}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^\"",
	                               "", "", "{tenter=resourceNameInArray}{_advanceTo=0start}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^\\]",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "resourceArray",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^'",
	                               "", "", "{tenter=ssliteral,end}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\"",
	                               "", "", "{tenter=dsliteral,end}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\\[",
	                               "", "", "{tenter=skipArray,end}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\\{",
	                               "", "", "{tenter=skipBlock,end}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\\(",
	                               "", "", "{tenter=skipArgs,end}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\\$[a-zA-Z:][0-9a-zA-Z\"]*",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^[0-9]+(\\.[0-9]+(e([+-][0-9]+)))?",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^[a-zA-Z0-9:][0-9a-zA-Z:]*",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^[ \t]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^\n",
	                               "", "", "{tleave}", NULL);
	addLanguageTagMultiTableRegex (language, "varexpr",
	                               "^.",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^/\\*",
	                               "", "", "{tenter=comment_multiline}", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^\\#",
	                               "", "", "{tenter=comment_oneline}", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^[ \t\n]",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^'",
	                               "", "", "{tenter=ssliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^\"",
	                               "", "", "{tenter=dsliteral}", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^:",
	                               "", "", "{tenter=block}", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^}",
	                               "", "", "{tleave}{_advanceTo=0start}", NULL);
	addLanguageTagMultiTableRegex (language, "caseBlock",
	                               "^.",
	                               "", "", "", NULL);
}

extern parserDefinition* PuppetManifestParser (void)
{
	static const char *const extensions [] = {
		"pp",
		NULL
	};

	static const char *const aliases [] = {
		NULL
	};

	static const char *const patterns [] = {
		NULL
	};

	static kindDefinition PuppetManifestKindTable [] = {
		{
		  true, 'c', "class", "classes",
		},
		{
		  true, 'd', "definition", "definitions",
		},
		{
		  true, 'n', "node", "nodes",
		},
		{
		  true, 'r', "resource", "resources",
		},
		{
		  true, 'v', "variable", "variables",
		},
	};

	parserDefinition* const def = parserNew ("PuppetManifest");

	def->enabled       = true;
	def->extensions    = extensions;
	def->patterns      = patterns;
	def->aliases       = aliases;
	def->method        = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->useCork       = 1;
	def->kindTable = PuppetManifestKindTable;
	def->kindCount = ARRAY_SIZE(PuppetManifestKindTable);
	def->initialize    = initializePuppetManifestParser;

	return def;
}
