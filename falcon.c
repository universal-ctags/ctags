/*
 * $Id$
 * 
 * Copyright (c) 2011 Steven Oliver <oliver.steven@gmail.com>
 * 
 * This source code is released for free distribution under the terms of the
 * GNU General Public License.
 * 
 * This module contains functions for generating tags for Falcon language
 * files.
 */


/* 
 * INCLUDE FILES 
 */
#include "general.h"

#include <string.h>
#include <ctype.h>   

#include "parse.h"
#include "read.h"  

/*
 * Data Definitions
 */
typedef enum eFalconKinds {
    K_CLASS,
    K_FUNCTION,
    K_MEMBER,
    K_VARIABLE,
    K_NAMESPACE
} falconKind;

static kindOption FalconKinds [] = {
    {TRUE, 'c', "class",     "classes" },
    {TRUE, 'f', "function",  "functions"},
    {TRUE, 'm', "member",    "class members"},
    {TRUE, 'v', "variable",  "variables"},
    {TRUE, 'i', "namespace", "imports"}
};

/* 
 * Function Definitions
 */
static void findFalconTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
        const unsigned char *cp = line;

        if (*cp == '#')
            continue;

        if (strncmp ((const char*) cp, "function", (size_t) 8) == 0)
        {
            cp += 8;
            while (isspace ((int) *cp))
                ++cp;
            while (isalnum ((int) *cp)  ||  *cp == '_')
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            vStringTerminate (name);
            makeSimpleTag (name, FalconKinds, K_FUNCTION);
            vStringClear (name);
        }
        else if (strncmp ((const char*) cp, "class", (size_t) 5) == 0)
        {
            cp += 5;
            while (isspace ((int) *cp))
                ++cp;
            while (isalnum ((int) *cp)  ||  *cp == '_')
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            vStringTerminate (name);
            makeSimpleTag (name, FalconKinds, K_CLASS);
            vStringClear (name);
        }
        else if (strncmp ((const char*) cp, "load", (size_t) 4) == 0)
        {
            cp += 4;
            while (isspace ((int) *cp))
                ++cp;
            while (isalnum ((int) *cp)  ||  *cp == '_')
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            vStringTerminate (name);
            makeSimpleTag (name, FalconKinds, K_NAMESPACE);
            vStringClear (name);
        }
    }
    vStringDelete (name);
}

/* 
 * Parser definition structure
 */
extern parserDefinition* FalconParser (void)
{
    static const char *const extensions [] = { "fal", NULL };
    parserDefinition *def = parserNew ("Falcon");
    def->kinds      = FalconKinds;
    def->kindCount  = KIND_COUNT (FalconKinds);
    def->extensions = extensions;
    def->parser     = findFalconTags;
    return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
