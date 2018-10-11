/*
 * perl6.c -- Perl6 parser.
 * Author: Dmitri Tikhonov <dmitri@cpan.org>
 *
 * This is a very basic Perl 6 parser.  It does not know how to:
 *   - skip POD;
 *   - skip multiline comments;
 *   - skip quoted strings;
 *   - generate fully-qualified tags.
 *
 * This source code is released for free distribution under the terms of
 * the GNU General Public License version 2 or (at your option) any later version.
 */

#include "general.h"    /* must always come first */

#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "vstring.h"

enum perl6Kind {
    K_NONE = -1,
    K_CLASS,
    K_GRAMMAR,
    K_METHOD,
    K_MODULE,
    K_PACKAGE,
    K_ROLE,
    K_RULE,
    K_SUBMETHOD,
    K_SUBROUTINE,
    K_TOKEN,
};

static kindDefinition perl6Kinds[] = {
    [K_CLASS]       = { true,  'c', "class",      "classes" },
    [K_GRAMMAR]     = { true,  'g', "grammar",    "grammars" },
    [K_METHOD]      = { true,  'm', "method",     "methods" },
    [K_MODULE]      = { true,  'o', "module",     "modules" },
    [K_PACKAGE]     = { true,  'p', "package",    "packages" },
    [K_ROLE]        = { true,  'r', "role",       "roles" },
    [K_RULE]        = { true,  'u', "rule",       "rules" },
    [K_SUBMETHOD]   = { true,  'b', "submethod",  "submethods" },
    [K_SUBROUTINE]  = { true,  's', "subroutine", "subroutines" },
    [K_TOKEN]       = { true,  't', "token",      "tokens" },
};

enum token {
    T_CLASS,
    T_GRAMMAR,
    T_METHOD,
    T_MODULE,
    T_MULTI,
    T_MY,
    T_OUR,
    T_PACKAGE,
    T_PROTO,
    T_ROLE,
    T_RULE,
    T_SUB,
    T_SUBMETHOD,
    T_UNIT,
    T_TOKEN,
};

static const enum perl6Kind token2kind[] = {
    [T_CLASS]       = K_CLASS,
    [T_GRAMMAR]     = K_GRAMMAR,
    [T_METHOD]      = K_METHOD,
    [T_MODULE]      = K_MODULE,
    [T_MULTI]       = K_SUBROUTINE,
    [T_MY]          = K_NONE,
    [T_OUR]         = K_NONE,
    [T_PACKAGE]     = K_PACKAGE,
    [T_PROTO]       = K_NONE,
    [T_ROLE]        = K_ROLE,
    [T_RULE]        = K_RULE,
    [T_SUB]         = K_SUBROUTINE,
    [T_SUBMETHOD]   = K_SUBMETHOD,
    [T_UNIT]        = K_NONE,
    [T_TOKEN]       = K_TOKEN,
};

#define STRLEN(s) (sizeof(s) - 1)
#define STREQN(s, token) (0 == strncmp(s, token, STRLEN(token)))

static enum token
matchToken (const char *s, int len)
{
    switch (len) {
        case 2:
            if (STREQN(s, "my"))                return T_MY;
            break;
        case 3:
            switch (s[0]) {
                case 'o':
                    if (STREQN(s, "our"))       return T_OUR;
                    break;
                case 's':
                    if (STREQN(s, "sub"))       return T_SUB;
                    break;
            }
            break;
        case 4:
            switch (s[1]) {
                case 'o':
                    if (STREQN(s, "role"))      return T_ROLE;
                    break;
                case 'u':
                    if (STREQN(s, "rule"))      return T_RULE;
                    break;
                case 'n':
                    if (STREQN(s, "unit"))      return T_UNIT;
                    break;
            }
            break;
        case 5:
            switch (s[0]) {
                case 'c':
                    if (STREQN(s, "class"))     return T_CLASS;
                    break;
                case 'm':
                    if (STREQN(s, "multi"))     return T_MULTI;
                    break;
                case 'p':
                    if (STREQN(s, "proto"))     return T_PROTO;
                    break;
                case 't':
                    if (STREQN(s, "token"))     return T_TOKEN;
                    break;
            }
            break;
        case 6:
            switch (s[1]) {
                case 'e':
                    if (STREQN(s, "method"))    return T_METHOD;
                    break;
                case 'o':
                    if (STREQN(s, "module"))    return T_MODULE;
                    break;
            }
            break;
        case 7:
            switch (s[0]) {
                case 'g':
                    if (STREQN(s, "grammar"))   return T_GRAMMAR;
                    break;
                case 'p':
                    if (STREQN(s, "package"))   return T_PACKAGE;
                    break;
            }
            break;
        case 9:
            if (STREQN(s, "submethod"))         return T_SUBMETHOD;
            break;
    }
    return -1;
}

static const int validPerl6Identifier[0x100] = {
/* r!perl -e "print qq([(int)'\$_'] = 1,\n)for a..z,A..Z,0..9,':','-','_'"|fmt
 */
    [(int)'a'] = 1, [(int)'b'] = 1, [(int)'c'] = 1, [(int)'d'] = 1,
    [(int)'e'] = 1, [(int)'f'] = 1, [(int)'g'] = 1, [(int)'h'] = 1,
    [(int)'i'] = 1, [(int)'j'] = 1, [(int)'k'] = 1, [(int)'l'] = 1,
    [(int)'m'] = 1, [(int)'n'] = 1, [(int)'o'] = 1, [(int)'p'] = 1,
    [(int)'q'] = 1, [(int)'r'] = 1, [(int)'s'] = 1, [(int)'t'] = 1,
    [(int)'u'] = 1, [(int)'v'] = 1, [(int)'w'] = 1, [(int)'x'] = 1,
    [(int)'y'] = 1, [(int)'z'] = 1, [(int)'A'] = 1, [(int)'B'] = 1,
    [(int)'C'] = 1, [(int)'D'] = 1, [(int)'E'] = 1, [(int)'F'] = 1,
    [(int)'G'] = 1, [(int)'H'] = 1, [(int)'I'] = 1, [(int)'J'] = 1,
    [(int)'K'] = 1, [(int)'L'] = 1, [(int)'M'] = 1, [(int)'N'] = 1,
    [(int)'O'] = 1, [(int)'P'] = 1, [(int)'Q'] = 1, [(int)'R'] = 1,
    [(int)'S'] = 1, [(int)'T'] = 1, [(int)'U'] = 1, [(int)'V'] = 1,
    [(int)'W'] = 1, [(int)'X'] = 1, [(int)'Y'] = 1, [(int)'Z'] = 1,
    [(int)'0'] = 1, [(int)'1'] = 1, [(int)'2'] = 1, [(int)'3'] = 1,
    [(int)'4'] = 1, [(int)'5'] = 1, [(int)'6'] = 1, [(int)'7'] = 1,
    [(int)'8'] = 1, [(int)'9'] = 1, [(int)':'] = 1, [(int)'-'] = 1,
    [(int)'_'] = 1,
};

static const int validMethodPrefix[0x100] = {
    [(int)'!'] = 1, [(int)'^'] = 1,
};

static const int kindMayHaveMethodPrefix = (1 << K_SUBMETHOD) |
                                           (1 << K_METHOD)    ;

/* Trim identifier pointed to by ps, possibly advancing it, and return
 * the length of the valid portion.  If the returned value is zero, the
 * identifier is invalid.
 */
static int
trimIdentifier (enum perl6Kind kind, const char **ps, int len)
{
    Assert(len > 0);
    const char *const end = *ps + len;
    const char *s = *ps;
    /* Trim the front if possible: */
    s += (kindMayHaveMethodPrefix & (1 << kind)) &&
         validMethodPrefix[(int)*s];
    /* Record the start of identifier: */
    *ps = s;
    /* Continuous string of valid characters: */
    while (s < end && validPerl6Identifier[(int)*s])
        ++s;
    /* sub multi infix:<...>        -- we want the "infix" only */
    while (s - *ps > 0 && ':' == s[-1])
        --s;
    /* It's ok if this is zero: */
    return s - *ps;
}

struct p6Ctx {
    enum token  tokens[128 /* unlikely to need more than this */];
    unsigned int n_tokens;
    vString    *name;
    const char *line;      /* Saved from readLineFromInputFile() */
};

static void
makeTag (struct p6Ctx *ctx, int kind, const char *name, int len)
{
    tagEntryInfo entry;
    vStringNCopyS(ctx->name, name, len);
    initTagEntry(&entry, vStringValue(ctx->name), kind);
    makeTagEntry(&entry);
}

static void
possiblyMakeTag (struct p6Ctx *ctx, const char *s, int len)
{
    Assert(ctx->n_tokens > 0);
    enum perl6Kind kind = token2kind[ ctx->tokens[ctx->n_tokens - 1] ];
    if (K_NONE != kind && perl6Kinds[kind].enabled
                       && (len = trimIdentifier(kind, &s, len)) > 0)
        makeTag(ctx, kind, s, len);
}

static void
initP6Ctx (struct p6Ctx *ctx)
{
    ctx->n_tokens = 0;
    ctx->name = vStringNew();
    ctx->line = NULL;
}

static void
deinitP6Ctx (struct p6Ctx *ctx)
{
    vStringDelete(ctx->name);
}

/* Read next contiguous sequence of non-whitespace characters, store
 * the address in `ptok', and return its length.  Return value of zero
 * means EOF.
 *
 * TODO: Currently, POD and multi-line comments are not handled.
 */
static int
getNonSpaceStr (struct p6Ctx *ctx, const char **ptok)
{
    const char *s = ctx->line;
    if (!s) {
next_line:
        s = (const char *) readLineFromInputFile();
        if (!s)
            return 0;                           /* EOF */
    }
    while (*s && isspace(*s))                   /* Skip whitespace */
        ++s;
    if ('#' == *s)
        goto next_line;
    int non_white_len = strcspn(s, ",; \t");
    if (non_white_len) {
        ctx->line = s + non_white_len;          /* Save state */
        *ptok = s;
        return non_white_len;
    } else
        goto next_line;
}

static void
findPerl6Tags (void)
{
    struct p6Ctx ctx;

#define RESET_TOKENS() do { ctx.n_tokens = 0; } while (0)

#define PUSH_TOKEN(_t_) do {                                            \
    if (ctx.n_tokens < ARRAY_SIZE(ctx.tokens)) {			\
        ctx.tokens[ ctx.n_tokens ] = _t_;                               \
        ++ctx.n_tokens;                                                 \
    } else {                                                            \
        Assert(!"Token stack overflown: this is quite odd");            \
        RESET_TOKENS();                                                 \
    }                                                                   \
} while (0)

    initP6Ctx(&ctx);

    const char *s;
    int len;

    while ((len = getNonSpaceStr(&ctx, &s)) > 0) {
        enum token token = matchToken(s, len);
        if ((int) token >= 0) {
            PUSH_TOKEN(token);
        } else if (ctx.n_tokens > 0) {
            possiblyMakeTag(&ctx, s, len);
            RESET_TOKENS();
        }
    }

    deinitP6Ctx(&ctx);
}

parserDefinition *
Perl6Parser (void)
{
    static const char *const extensions[] = { "p6", "pm6", "pm", "pl6", NULL };
    static selectLanguage selectors [] = { selectByPickingPerlVersion,
					   NULL };
    parserDefinition* def = parserNew("Perl6");
    def->kindTable      = perl6Kinds;
    def->kindCount  = ARRAY_SIZE(perl6Kinds);
    def->extensions = extensions;
    def->parser     = findPerl6Tags;
    def->selectLanguage = selectors;
    return def;
}
