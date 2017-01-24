/*
 *   Copyright (c) 2017, Daniel Riechers
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2.
 *
 *   Parser from Robot Framework http://robotframework.org
 */

#include "general.h"

#include <string.h>

#include "parse.h"
#include "vstring.h"
#include "routines.h"

typedef enum {
        K_KEYWORD,
        K_VARIABLE,
        COUNT_KIND
} RobotKind;

static kindOption RobotKinds[COUNT_KIND] = {
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

static void tagKeywords (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    if (count > 1)
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, RobotKinds, K_KEYWORD);
        whitespaceSwap(name);
        makeSimpleTag (name, RobotKinds, K_KEYWORD);
        vStringDelete (name);
    }
}

static void tagVariables (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    if (count > 1)
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
    addCallbackRegex (language, "(^[A-Za-z0-9]+([' _][A-Za-z0-9]+)*)($|[ ]*[$@])",
            "{exclusive}", tagKeywords, NULL, NULL);
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
