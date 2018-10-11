/*
 * Copyright (c) 2011, 2012 Steven Oliver <oliver.steven@gmail.com>
 *
 * This source code is released for free distribution under the terms of the
 * GNU General Public License version 2 or (at your option) any later version.
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

#include "read.h"
#include "kind.h"
#include "parse.h"
#include "routines.h"

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

static kindDefinition FalconKinds [] = {
    {true, 'c', "class",     "classes" },
    {true, 'f', "function",  "functions"},
    {true, 'm', "member",    "class members"},
    {true, 'v', "variable",  "variables"},
    {true, 'i', "namespace", "imports"}
};

/*
 * Function Definitions
 */

static bool isIdentifierChar (int c)
{
    return (bool) (isalnum (c));
}

static const unsigned char *skipSpace (const unsigned char *cp)
{
    while (isspace ((int) *cp))
        ++cp;

    return cp;
}

/*
 * Main parsing function
 */

static void findFalconTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = readLineFromInputFile ()) != NULL)
    {
        const unsigned char *cp = line;

        // Skip lines starting with # which in falcon
        // would only be the "crunch bang" statement
        if (*cp == '#')
            continue;

        if (strncmp ((const char*) cp, "function", (size_t) 8) == 0)
        {
            cp += 8;
            cp = skipSpace (cp);

            while (isIdentifierChar ((int) *cp))
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            makeSimpleTag (name, K_FUNCTION);
            vStringClear (name);
        }
        else if (strncmp ((const char*) cp, "class", (size_t) 5) == 0)
        {
            cp += 5;
            cp = skipSpace (cp);

            while (isIdentifierChar ((int) *cp))
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            makeSimpleTag (name, K_CLASS);
            vStringClear (name);
        }
        else if (strncmp ((const char*) cp, "load", (size_t) 4) == 0)
        {
            cp += 4;
            cp = skipSpace (cp);

            while (isIdentifierChar ((int) *cp))
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            makeSimpleTag (name, K_NAMESPACE);
            vStringClear (name);
        }
        else if (strncmp ((const char*) cp, "import from", (size_t) 11) == 0)
        {
            cp += 12;
            cp = skipSpace (cp);

            while (isIdentifierChar ((int) *cp))
            {
                vStringPut (name, (int) *cp);
                ++cp;
            }
            makeSimpleTag (name, K_NAMESPACE);
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
    static const char *const extensions [] = { "fal", "ftd", NULL };
    parserDefinition *def = parserNew ("Falcon");
    def->kindTable  = FalconKinds;
    def->kindCount  = ARRAY_SIZE (FalconKinds);
    def->extensions = extensions;
    def->parser     = findFalconTags;
    return def;
}
