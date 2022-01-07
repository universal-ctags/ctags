/*
 *   Copyright (c) 2021 Jan Dolinár
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Kotlin.
 */

/*
*   INCLUDE FILES
*/
#include "parse.h"
#include "trace.h"

/*
* FUNCTION DEFINITIONS
*/
static int getcFromKotlinFile(struct parserCtx *auxil)
{
    int c = getcFromInputFile();
    if (auxil->parenthesis_level > 0 && (c == '\r' || c == '\n'))
    {
        return ' ';
    }
    return c;
}

static void makeKotlinTag (struct parserCtx *auxil, const char *name, long offset, bool pushScope)
{
    int k = PEEK_KIND(auxil);
    if (k == K_IGNORE) return;
    tagEntryInfo e;
    char *stripped = NULL;
    if (*name != '`')
    {
        initTagEntry(&e, name, k);
    } else
    {
        size_t len = strlen(name);
        Assert(len >= 2);
        len -= 2;
        stripped = eStrndup (name + 1, len);
        initTagEntry(&e, stripped, k);
    }
    e.lineNumber = getInputLineNumberForFileOffset (offset);
    e.filePosition = getInputFilePositionForLine (e.lineNumber);
    e.extensionFields.scopeIndex = BASE_SCOPE(auxil);
    int scope_index = makeTagEntry (&e);
    if (pushScope)
    {
        SET_SCOPE(auxil, scope_index);
    }
    if (stripped)
        eFree (stripped);
}

#ifdef DEBUG
static void reportFailure(struct parserCtx *auxil, long offset)
{
    if(auxil->fail_offset < 0)
    {
        auxil->fail_offset = offset;
    }
}

static void resetFailure(struct parserCtx *auxil, long offset)
{
    if(auxil->fail_offset >= 0)
    {
        unsigned long startLine = getInputLineNumberForFileOffset(auxil->fail_offset);
        unsigned long endLine = getInputLineNumberForFileOffset(offset-1);
        if (startLine == endLine)
        {
            TRACE_PRINT("Failed to parse '%s' at line %lu!\n", getInputFileName(), startLine);
        } else
        {
            TRACE_PRINT("Failed to parse '%s' from line %lu to line %lu!\n", getInputFileName(), startLine, endLine);
        }
    }
    auxil->fail_offset = -1;
}
#endif

static void ctxInit (struct parserCtx *auxil)
{
    BASE_INIT(auxil, K_INTERFACE);
    auxil->parenthesis_level = 0;
    #ifdef DEBUG
    auxil->fail_offset = -1;
    #endif
}

static void ctxFini (struct parserCtx *auxil)
{
    BASE_FINI(auxil);
}

static void findKotlinTags (void)
{
    struct parserCtx auxil;

    ctxInit (&auxil);
    pkotlin_context_t *pctx = pkotlin_create(&auxil);

    while (pkotlin_parse(pctx, NULL) && (!BASE_ERROR(&auxil)) );

    pkotlin_destroy(pctx);
    ctxFini (&auxil);
}

extern parserDefinition* KotlinParser (void)
{
    static const char *const extensions [] = { "kt", "kts", NULL };
    parserDefinition* def = parserNew ("Kotlin");
    def->kindTable = KotlinKinds;
    def->kindCount = ARRAY_SIZE (KotlinKinds);
    def->extensions = extensions;
    def->parser = findKotlinTags;
    def->useCork = true;
    def->requestAutomaticFQTag = true;
    def->defaultScopeSeparator = ".";
    def->enabled = true;
    return def;
}
