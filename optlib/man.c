/*
 * Generated by ./misc/optlib2c from optlib/man.ctags, Don't edit this manually.
 */
#include "general.h"
#include "parse.h"
#include "routines.h"
#include "field.h"
#include "xtag.h"


typedef enum {
	K_TITLE,
	K_SECTION,
	K_SUBSECTION,
} ManKind;


static void initializeManParser (const langType language)
{
	addLanguageOptscriptToHook (language, SCRIPT_HOOK_PRELUDE,
		"{{    % adjustment:int FILL-END-OF-SCOPE -\n"
		"    %\n"
		"    % Note: The group 1 of a regex matching is assumed.\n"
		"    %       An entry on the scope stack is assumed.\n"
		"    /fill-end-of-scope {\n"
		"         _scopetop pop exch\n"
		"         % scope-top:int adjustment:int\n"
		"         @1 _matchloc2line exch\n"
		"         % scope-top:int line adjustment:int\n"
		"         2 copy gt {\n"
		"             sub end:\n"
		"         } {\n"
		"             pop pop pop\n"
		"         } ifelse\n"
		"    } def\n"
		"    % adjustment:int /replace HEADING-ACTION -\n"
		"    % /push HEADING-ACTION -\n"
		"    /heading-action {\n"
		"        /replace eq {\n"
		"             %\n"
		"             % Before removing the tag at the top of the scope stack,\n"
		"             % fill the end field of the tag.\n"
		"             %\n"
		"             % input0.man\n"
		"             % ----------------------------------------------------\n"
		"             %   .SH SEC1\n"
		"             %   ...\n"
		"             %E: This is the end of the SEC1.\n"
		"             %   .SH\n"
		"             %C: SEC2\n"
		"             %\n"
		"             % ----------------------------------------------------\n"
		"             %\n"
		"             % C represents the current input line. The parser must\n"
		"             % fill the end field of \"SEC1\", the tag at the top,\n"
		"             % with the line number of E.\n"
		"             %\n"
		"             %    E = C - 2\n"
		"             %\n"
		"             % input1.man\n"
		"             % ----------------------------------------------------\n"
		"             %   .SH SEC3\n"
		"             %   ...\n"
		"             %E: This is the end of the SEC1.\n"
		"             %C:  .SH SEC4\n"
		"             %\n"
		"             % ----------------------------------------------------\n"
		"             %\n"
		"             % In this case\n"
		"             %\n"
		"             %    E = C - 1\n"
		"             %\n"
		"             % The offset for the adjustment depends on the conctxt.\n"
		"             %\n"
		"             fill-end-of-scope\n"
		"             _scopepop\n"
		"        } if\n"
		"\n"
		"        _scopetop {\n"
		"            . exch scope:\n"
		"        } if\n"
		"        . _scopepush\n"
		"    } def\n"
		"}}");

	addLanguageRegexTable (language, "main");
	addLanguageRegexTable (language, "section");
	addLanguageRegexTable (language, "sectionheading");
	addLanguageRegexTable (language, "subsection");
	addLanguageRegexTable (language, "subsectionheading");
	addLanguageRegexTable (language, "EOF");
	addLanguageRegexTable (language, "SKIP");
	addLanguageRegexTable (language, "REST");
	addLanguageRegexTable (language, "GUARD");

	addLanguageTagMultiTableRegex (language, "main",
	                               "^([^\n.]|\\.[^\nst])[^\n]*\n",
	                               "", "", "{icase}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\.TH[\t ]+\"([^\"]+)\"[^\n]*\n",
	                               "\\1", "t", "{icase}{scope=set}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\.TH[\t ]+([^\t \n]+)[^\n]*\n",
	                               "\\1", "t", "{icase}{scope=set}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\.SH[\t ]+\"([^\"\n]+)\"[^\n]*\n",
	                               "\\1", "s", "{icase}{scope=push}{tenter=section}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\.SH[\t ]+([^\n]+)\n",
	                               "\\1", "s", "{icase}{scope=push}{tenter=section}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^\\.SH[\t ]*\n",
	                               "", "", "{icase}{tenter=sectionheading}"
		"{{\n"
		"    /push\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "main",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^([^\n.]|\\.[^\nst])[^\n]*\n",
	                               "", "", "{icase}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^\\.SH[\t ]+\"([^\"\n]+)\"[^\n]*\n",
	                               "\\1", "s", "{icase}{scope=replace}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^\\.SH[\t ]+([^\n]+)\n",
	                               "\\1", "s", "{icase}{scope=replace}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^(\\.SH)[\t ]*\n",
	                               "", "", "{icase}{tjump=sectionheading}"
		"{{\n"
		"    2 /replace\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^\\.SS[\t ]+\"([^\"\n]+)\"[^\n]*\n",
	                               "\\1", "S", "{icase}{scope=push}{tenter=subsection}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^\\.SS[\t ]+([^\n]+)\n",
	                               "\\1", "S", "{icase}{scope=push}{tenter=subsection}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^\\.SS[\t ]*\n",
	                               "", "", "{icase}{tenter=subsectionheading}"
		"{{\n"
		"    /push\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "section",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "sectionheading",
	                               "^[ \t]*([^\n]+)\n",
	                               "\\1", "s", "{tjump=section}"
		"{{\n"
		"    heading-action\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "sectionheading",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "sectionheading",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^([^\n.]|\\.[^\nst])[^\n]*\n",
	                               "", "", "{icase}", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^\\.SS[\t ]+\"([^\"\n]+)\"[^\n]*\n",
	                               "\\1", "S", "{icase}{scope=replace}", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^\\.SS[\t ]+([^\n]+)\n",
	                               "\\1", "S", "{icase}{scope=replace}", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^\\.SS[\t ]*\n",
	                               "", "", "{icase}{tjump=subsectionheading}"
		"{{\n"
		"    2 /replace\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^(\\.SH)",
	                               "", "", "{icase}{_advanceTo=0start}{tleave}"
		"{{\n"
		"    1 fill-end-of-scope\n"
		"    _scopepop\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "subsection",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "subsectionheading",
	                               "^[ \t]*([^\n]+)\n",
	                               "\\1", "S", "{tjump=subsection}"
		"{{\n"
		"    heading-action\n"
		"}}", NULL);
	addLanguageTagMultiTableRegex (language, "subsectionheading",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "subsectionheading",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "EOF",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "SKIP",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "REST",
	                               "^[^\n]*\n|[^\n]+",
	                               "", "", "", NULL);
	addLanguageTagMultiTableRegex (language, "REST",
	                               "^",
	                               "", "", "{scope=clear}{tquit}", NULL);
	addLanguageTagMultiTableRegex (language, "GUARD",
	                               "^([^\n.]|\\.[^\nst])[^\n]*\n",
	                               "", "", "{icase}", NULL);
}

extern parserDefinition* ManParser (void)
{
	static const char *const extensions [] = {
		"man",
		"1",
		"2",
		"3",
		"4",
		"5",
		"6",
		"7",
		"8",
		"9",
		"3pm",
		"3stap",
		"7stap",
		NULL
	};

	static const char *const aliases [] = {
		NULL
	};

	static const char *const patterns [] = {
		NULL
	};

	static kindDefinition ManKindTable [] = {
		{
		  true, 't', "title", "titles",
		},
		{
		  true, 's', "section", "sections",
		},
		{
		  true, 'S', "subsection", "sub sections",
		},
	};

	parserDefinition* const def = parserNew ("Man");

	def->versionCurrent= 0;
	def->versionAge    = 0;
	def->enabled       = true;
	def->extensions    = extensions;
	def->patterns      = patterns;
	def->aliases       = aliases;
	def->method        = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->useCork       = CORK_QUEUE;
	def->kindTable     = ManKindTable;
	def->kindCount     = ARRAY_SIZE(ManKindTable);
	def->defaultScopeSeparator = "\"\"";
	def->initialize    = initializeManParser;

	return def;
}
