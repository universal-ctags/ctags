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

static kindOption RobotKinds[COUNT_KIND] = {
	{true, 't', "testcase",   "testcases"},
	{true, 'k', "keyword",    "keywords"},
	{true, 'v', "variable",   "variables"},
};

static void findRobotTags (void)
{
	findRegexTags ();
}

static void whitespaceSwap (vString *const s)
{
        char replaceWith = '_';
        char toReplace = ' ';

        if(strchr(s->buffer, '_'))
        {
            replaceWith = ' ';
            toReplace = '_';
        }

        for(int i=0; i < vStringLength(s); i++)
            if(s->buffer[i] == toReplace)
                s->buffer[i] = replaceWith;
}

static void changeSection (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    vString *const sectionName = vStringNew ();
    vStringNCopyS (sectionName, line + matches [1].start, matches [1].length);
    vStringCopyToLower(sectionName, sectionName);

    if(strcmp(sectionName->buffer, "test cases") == 0)
    {
        section = K_TESTCASE;
    }
    else if(strcmp(sectionName->buffer, "keywords") == 0)
    {
        section = K_KEYWORD;
    }
    else if(strcmp(sectionName->buffer, "variables") == 0)
    {
        section = K_VARIABLE;
    }

    vStringDelete(sectionName);
}

static void tagKeywordsAndTestCases (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    if (count > 1 && ( section == K_KEYWORD || section == K_TESTCASE) )
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, RobotKinds, section);
        whitespaceSwap(name);
        makeSimpleTag (name, RobotKinds, section);
        vStringDelete (name);
    }
}

static void tagVariables (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    if (count > 1 && section == K_VARIABLE)
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, RobotKinds, K_VARIABLE);
        whitespaceSwap(name);
        makeSimpleTag (name, RobotKinds, K_VARIABLE);
        vStringDelete (name);
    }
}

static void initialize (const langType language)
{
    addCallbackRegex (language, "^\\*+ *([^* ].+[^* ]) *\\*+$",
            "{exclusive}", changeSection, NULL, NULL);
    addCallbackRegex (language, "(^[A-Za-z0-9]+([' _][A-Za-z0-9]+)*)($|[ ]*[$@])",
            "{exclusive}", tagKeywordsAndTestCases, NULL, NULL);
    addCallbackRegex (language, "^[$@]\\{([_A-Za-z0-9][' _A-Za-z0-9]+)\\}  [ ]*.+",
            "{exclusive}", tagVariables, NULL, NULL);
}

extern parserDefinition* RobotParser (void)
{
	static const char *const extensions[] = { "robot", NULL };
	parserDefinition *def = parserNew ("Robot");
    def->kinds      = RobotKinds;
    def->kindCount  = COUNT_KIND;
	def->extensions = extensions;
	def->initialize = initialize;
    def->parser     = findRobotTags;
	return def;
}
