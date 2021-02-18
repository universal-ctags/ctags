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


static void pushKind (struct parserCtx *auxil, int kind)
{
    intArrayAdd (auxil->kind_stack, kind);
}

static void popScope(struct parserCtx *auxil)
{
    tagEntryInfo *e = getEntryInCorkQueue (auxil->scope_cork_index);
    if (e)
        auxil->scope_cork_index = e->extensionFields.scopeIndex;
}

static void popKind (struct parserCtx *auxil, bool popScopeToo)
{
    intArrayRemoveLast (auxil->kind_stack);

    if (popScopeToo)
    {
        popScope(auxil);
    }
}

static int peekKind (struct parserCtx *auxil)
{
    return intArrayLast (auxil->kind_stack);
}

static void reportError (struct parserCtx *auxil)
{
    auxil->found_syntax_error = true;
    fprintf(stderr, "%s: syntax error in \"%s\"\n", getLanguageName (getInputLanguage ()), getInputFileName());
}

static void makeKotlinTag (struct parserCtx *auxil, const char *name, long offset, bool pushScope)
{
    int k = peekKind (auxil);
    if (k == K_IGNORE) return;
    tagEntryInfo e;
    if (*name != '`')
    {
        initTagEntry(&e, name, k);
    } else
    {
        size_t len = strlen(name) - 2;
        char *stripped = (char *)eMalloc(len+1);
        memcpy(stripped, name + 1, len);
        stripped[len] = '\0';
        initTagEntry(&e, stripped, k);
        eFree(stripped);
    }
    e.lineNumber = getInputLineNumberForFileOffset (offset);
    e.filePosition = getInputFilePositionForLine (e.lineNumber);
    e.extensionFields.scopeIndex = auxil->scope_cork_index;
    int scope_index = makeTagEntry (&e);
    if (pushScope)
    {
        auxil->scope_cork_index = scope_index;
    }
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
    auxil->kind_stack = intArrayNew ();
    pushKind (auxil, K_INTERFACE);
    auxil->scope_cork_index = CORK_NIL;
    auxil->found_syntax_error = false;
    auxil->parenthesis_level = 0;
    #ifdef DEBUG
    auxil->fail_offset = -1;
    #endif
}

static void ctxFini (struct parserCtx *auxil)
{
    popKind (auxil, false);
    intArrayDelete (auxil->kind_stack);
}

static void findKotlinTags (void)
{
    struct parserCtx auxil;

    ctxInit (&auxil);
    pkotlin_context_t *pctx = pkotlin_create(&auxil);

    while (pkotlin_parse(pctx, NULL) && (!auxil.found_syntax_error) );

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
