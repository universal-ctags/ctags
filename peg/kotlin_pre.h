/*
 *   Copyright (c) 2021 Jan Dolinár
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains macros, data decls and prototypes to generate tags for Kotlin.
 */

/*
*   INCLUDE FILES
*/

#include "kind.h"
#include "peg_common.h"


/*
*   MACROS
*/
#undef PCC_GETCHAR
#define PCC_GETCHAR(auxil) getcFromKotlinFile(auxil)

/*
*   DATA DECLARATIONS
*/
typedef enum {
    K_PACKAGE,
    K_INTERFACE,
    K_CLASS,
    K_OBJECT,
    K_METHOD,
    K_TYPEALIAS,
    K_CONSTANT,
    K_VARIABLE,
    K_IGNORE
} KotlinKind;

static kindDefinition KotlinKinds [] = {
    { true, 'p', "package", "packages", },
    { true, 'i', "interface", "interfaces", },
    { true, 'c', "class", "classes", },
    { true, 'o', "object", "objects", },
    { true, 'm', "method", "methods", },
    { true, 'T', "typealias", "typealiases", },
    { true, 'C', "constant", "constants", },
    { true, 'v', "variable", "variables", },
};

struct parserCtx {
    struct parserBaseCtx base;
    int parenthesis_level;
    #ifdef DEBUG
    long fail_offset;
    #endif
};

/*
*   FUNCTION PROTOTYPES
*/
static int getcFromKotlinFile(struct parserCtx *auxil);
static void makeKotlinTag (struct parserCtx *auxil, const char *name, long offset, bool pushScope);
#ifdef DEBUG
static void reportFailure(struct parserCtx *auxil, long offset);
static void resetFailure(struct parserCtx *auxil, long offset);
#else
    #define reportFailure(AUXIL, OFFSET)
    #define resetFailure(AUXIL, OFFSET)
#endif
