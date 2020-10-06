/*
 *   Copyright (c) 2017, Daniel Riechers
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   Parser from Robot Framework http://robotframework.org
 */

#include "general.h"

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "vstring.h"
#include "routines.h"

typedef enum {
        K_TESTCASE,
        K_KEYWORD,
        K_VARIABLE,
        COUNT_KIND
} RobotKind;

static RobotKind section = -1;

static kindDefinition RobotKinds[COUNT_KIND] = {
	{true, 't', "testcase",   "testcases"},
	{true, 'k', "keyword",    "keywords"},
	{true, 'v', "variable",   "variables"},
};

typedef enum {
	X_WHITESPACE_SWAPPED,
} robotXtag;

static xtagDefinition RobotXtags [] = {
	{
		.enabled = true,
		.name = "whitespaceSwapped",
		.description = "Include tags swapping whitespace and underscore chars",
	},
};


static void findRobotTags (void)
{
	findRegexTags ();
}

static bool whitespaceSwap (vString *const s)
{
        char replaceWith = '_';
        char toReplace = ' ';
		char changed = false;

        if(strchr(vStringValue (s), '_'))
        {
            replaceWith = ' ';
            toReplace = '_';
        }

        for(unsigned int i=0; i < vStringLength(s); i++)
            if(vStringChar(s, i) == toReplace)
			{
                vStringChar (s, i) = replaceWith;
				changed = true;
			}

		return changed;
}

static bool changeSection (const char *const line, const regexMatch *const matches,
                               const unsigned int count CTAGS_ATTR_UNUSED, void *data CTAGS_ATTR_UNUSED)
{
    const char * const matchedSection = line + matches[1].start;

    if(strncasecmp(matchedSection, "test cases", matches[1].length) == 0)
    {
        section = K_TESTCASE;
    }
    else if(strncasecmp(matchedSection, "keywords", matches[1].length) == 0)
    {
        section = K_KEYWORD;
    }
    else if(strncasecmp(matchedSection, "variables", matches[1].length) == 0)
    {
        section = K_VARIABLE;
    }
	return true;
}

static void makeSimpleXTag (const vString* const name, const int kind,
							unsigned int xtagType)
{
	tagEntryInfo e;

	initTagEntry (&e, vStringValue(name), kind);
	markTagExtraBit (&e, xtagType);
	makeTagEntry (&e);
}

static bool tagKeywordsAndTestCases (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data CTAGS_ATTR_UNUSED)
{
    if (count > 1 && ( section == K_KEYWORD || section == K_TESTCASE) )
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, section);
        if (isXtagEnabled (RobotXtags[X_WHITESPACE_SWAPPED].xtype)
			&& whitespaceSwap(name))
			makeSimpleXTag (name, section,
							RobotXtags[X_WHITESPACE_SWAPPED].xtype);
        vStringDelete (name);
		return true;
    }
	return false;
}

static bool tagVariables (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data CTAGS_ATTR_UNUSED)
{
    if (count > 1 && section == K_VARIABLE)
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, K_VARIABLE);
        if (isXtagEnabled (RobotXtags[X_WHITESPACE_SWAPPED].xtype)
			&& whitespaceSwap(name))
			makeSimpleXTag (name, K_VARIABLE,
							RobotXtags[X_WHITESPACE_SWAPPED].xtype);
        vStringDelete (name);
		return true;
    }
	return false;
}

static void initialize (const langType language)
{
    addLanguageCallbackRegex (language, "^\\*+ *([^* ].+[^* ]) *\\*+$",
            "{exclusive}", changeSection, NULL, NULL);

    addLanguageCallbackRegex (
		language,
		"(^([A-Za-z0-9]+|\\$\\{[_A-Za-z0-9][' _A-Za-z0-9]*(:([^}]|\\\\)+)*\\})([${}' _]([-_$A-Za-z0-9]+|\\{[_A-Za-z0-9][' _A-Za-z0-9]*(:([^}]|\\\\)+)*\\})+)*)",
		"{exclusive}", tagKeywordsAndTestCases, NULL, NULL);

    addLanguageCallbackRegex (language, "^[$@]\\{([_A-Za-z0-9][' _A-Za-z0-9]+)\\}  [ ]*.+",
            "{exclusive}", tagVariables, NULL, NULL);
}

extern parserDefinition* RobotParser (void)
{
	static const char *const extensions[] = { "robot", NULL };
	parserDefinition *def = parserNew ("Robot");
    def->kindTable      = RobotKinds;
    def->kindCount  = COUNT_KIND;
	def->extensions = extensions;
	def->initialize = initialize;
    def->parser     = findRobotTags;
	def->xtagTable = RobotXtags;
	def->xtagCount = ARRAY_SIZE (RobotXtags);
	return def;
}
