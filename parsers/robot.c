/*
 * robot.c
 * parser for robot framewor
 * Author - Daniel Riechers
 * License GPL-2
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

        for(int i=0; i < s->length; i++)
            if(s->buffer[i] == toReplace)
                s->buffer[i] = replaceWith;
}

static void keywords (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    if (count > 1)
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, RobotKinds, K_KEYWORD);
        whitespaceSwap(name);
        makeSimpleTag (name, RobotKinds, K_KEYWORD);
    }
}

static void variables (const char *const line, const regexMatch *const matches,
                               const unsigned int count, void *data)
{
    if (count > 1)
    {
        vString *const name = vStringNew ();
        vStringNCopyS (name, line + matches [1].start, matches [1].length);
        makeSimpleTag (name, RobotKinds, K_VARIABLE);
        whitespaceSwap(name);
        makeSimpleTag (name, RobotKinds, K_VARIABLE);
    }
}

static void initialize (const langType language)
{
    addCallbackRegex (language, "(^[A-Za-z0-9]+([ _][A-Za-z0-9]+)*)",
            "{exclusive}", keywords, NULL, NULL);
    addCallbackRegex (language, "^[$@]\\{([_A-Za-z0-9][ _A-Za-z0-9]+)\\}  [ ]*.+",
            "{exclusive}", variables, NULL, NULL);
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
