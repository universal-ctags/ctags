/*
*   Copyright (c) 2020-2021, getzze <getzze@gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Julia files.
*
*   Documented 'kinds':
*       https://docs.julialang.org/en/v1/manual/documentation/#Syntax-Guide
*   Language parser in Scheme:
*       https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm
*/

/*
*   INCLUDE FILES
*/
#include "general.h"    /* must always come first */

#include <string.h>

#include "keyword.h"
#include "parse.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"

/*
*   MACROS
*/
#define MAX_STRING_LENGTH 256

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_CONSTANT,
    K_FUNCTION,
    K_FIELD,
    K_MACRO,
    K_MODULE,
    K_STRUCT,
    K_TYPE,
    K_UNKNOWN,
    K_NONE
} JuliaKind;

typedef enum {
    JULIA_MODULE_IMPORTED,
    JULIA_MODULE_USED,
    JULIA_MODULE_NAMESPACE,
} juliaModuleRole;

typedef enum {
    JULIA_UNKNOWN_IMPORTED,
    JULIA_UNKNOWN_USED,
} juliaUnknownRole;

/*
*  using X               X = (kind:module, role:used)
*
*  using X: a, b         X = (kind:module, role:namespace)
*                     a, b = (kind:unknown, role:used, scope:module:X)
*
*  import X              X = (kind:module, role:imported)
*
*  import X.a, Y.b    X, Y = (kind:module, role:namespace)
*                     a, b = (kind:unknown, role:imported, scope:module:X)
*
*  import X: a, b     Same as the above one
*/
static roleDefinition JuliaModuleRoles [] = {
    { true, "imported", "loaded by \"import\"" },
    { true, "used", "loaded by \"using\"" },
    { true, "namespace", "only some symbols in it are imported" },
};

static roleDefinition JuliaUnknownRoles [] = {
    { true, "imported", "loaded by \"import\"" },
    { true, "used", "loaded by \"using\""},
};

static kindDefinition JuliaKinds [] = {
    { true, 'c', "constant", "Constants"    },
    { true, 'f', "function", "Functions"    },
    { true, 'g', "field",    "Fields"       },
    { true, 'm', "macro",    "Macros"       },
    { true, 'n', "module",   "Modules",
      ATTACH_ROLES(JuliaModuleRoles) },
    { true, 's', "struct",   "Structures"   },
    { true, 't', "type",     "Types"        },
    { true, 'x', "unknown", "name defined in other modules",
      .referenceOnly = true, ATTACH_ROLES(JuliaUnknownRoles) },
};

typedef enum {
    TOKEN_NONE=0,         /* none */
    TOKEN_WHITESPACE,
    TOKEN_PAREN_BLOCK,
    TOKEN_BRACKET_BLOCK,
    TOKEN_CURLY_BLOCK,
    TOKEN_OPEN_BLOCK,
    TOKEN_CLOSE_BLOCK,
    TOKEN_TYPE_ANNOTATION,
    TOKEN_TYPE_WHERE,
    TOKEN_CONST,
    TOKEN_STRING,         /*  = 10 */
    TOKEN_COMMAND,
    TOKEN_MACROCALL,
    TOKEN_IDENTIFIER,
    TOKEN_MODULE,
    TOKEN_MACRO,
    TOKEN_FUNCTION,
    TOKEN_STRUCT,
    TOKEN_ENUM,
    TOKEN_TYPE,
    TOKEN_IMPORT,         /*  = 20 */
    TOKEN_USING,
    TOKEN_EXPORT,
    TOKEN_NEWLINE,
    TOKEN_SEMICOLON,
    TOKEN_COMPOSER_KWD,   /* KEYWORD only */
    TOKEN_EOF,
    TOKEN_COUNT
} tokenType;

static const keywordTable JuliaKeywordTable [] = {
    /* TODO: Sort by keys. */
    { "mutable",   TOKEN_COMPOSER_KWD },
    { "primitive", TOKEN_COMPOSER_KWD },
    { "abstract",  TOKEN_COMPOSER_KWD },

    { "if",        TOKEN_OPEN_BLOCK   },
    { "for",       TOKEN_OPEN_BLOCK   },
    { "while",     TOKEN_OPEN_BLOCK   },
    { "try",       TOKEN_OPEN_BLOCK   },
    { "do",        TOKEN_OPEN_BLOCK   },
    { "begin",     TOKEN_OPEN_BLOCK   },
    { "let",       TOKEN_OPEN_BLOCK   },
    { "quote",     TOKEN_OPEN_BLOCK   },

    { "module",    TOKEN_MODULE       },
    { "baremodule",TOKEN_MODULE       },

    { "using",     TOKEN_USING        },
    { "import",    TOKEN_IMPORT       },

    { "export",    TOKEN_EXPORT       },
    { "const",     TOKEN_CONST        },
    { "macro",     TOKEN_MACRO        },
    { "function",  TOKEN_FUNCTION     },
    { "struct",    TOKEN_STRUCT       },
    { "type",      TOKEN_TYPE         },
    { "where",     TOKEN_TYPE_WHERE   },
    { "end",       TOKEN_CLOSE_BLOCK  },
};

typedef struct {
    /* Characters */
    int prev_c;
    int cur_c;
    int next_c;

    /* Tokens */
    bool first_token;
    int cur_token;
    vString* token_str;
    unsigned long line;
    MIOPos pos;
} lexerState;

/*
*   FUNCTION PROTOTYPES
*/

static void parseExpr (lexerState *lexer, bool delim, int kind, vString *scope);

static void scanParenBlock (lexerState *lexer);

/*
*   FUNCTION DEFINITIONS
*/

static int endswith(const char* what, const char* withwhat)
{
    int l1 = strlen(what);
    int l2 = strlen(withwhat);
    if (l2 > l1)
    {
        return 0;
    }

    return strcmp(withwhat, what + (l1 - l2)) == 0;
}

/* Resets the scope string to the old length */
static void resetScope (vString *scope, size_t old_len)
{
    vStringTruncate (scope, old_len);
}

/* Adds a name to the end of the scope string */
static void addToScope (vString *scope, vString *name)
{
    if (vStringLength(scope) > 0)
    {
        vStringPut(scope, '.');
    }
    vStringCat(scope, name);
}

/* Reads a character from the file */
static void advanceChar (lexerState *lexer)
{
    lexer->prev_c = lexer->cur_c;
    lexer->cur_c  = lexer->next_c;
    lexer->next_c = getcFromInputFile();
}

/* Reads N characters from the file */
static void advanceNChar (lexerState *lexer, int n)
{
    while (n--)
    {
        advanceChar(lexer);
    }
}

/* Store the current character in lexerState::token_str if there is space
 * (set by MAX_STRING_LENGTH), and then read the next character from the file */
static void advanceAndStoreChar (lexerState *lexer)
{
    if (vStringLength(lexer->token_str) < MAX_STRING_LENGTH)
    {
        vStringPut(lexer->token_str, (char) lexer->cur_c);
    }
    advanceChar(lexer);
}

static bool isWhitespace (int c, bool newline)
{
    if (newline)
    {
        return c == ' ' || c == '\t' || c == '\r' || c == '\n';
    }
    return c == ' ' || c == '\t';
}

static bool isAscii (int c)
{
    return (c >= 0) && (c < 0x80);
}

static bool isOperator (int c)
{
    if (c == '%' || c == '^' || c == '&' || c == '|' ||
        c == '*' || c == '-' || c == '+' || c == '~' ||
        c == '<' || c == '>' || c == ',' || c == '/' ||
        c == '?' || c == '=' || c == ':' )
    {
        return true;
    }
    return false;
}

/* This does not distinguish Unicode letters from operators... */
static bool isIdentifierFirstCharacter (int c)
{
    return (bool) ((isAscii(c) && (isalpha (c) || c == '_')) || c >= 0xC0);
}

/* This does not distinguish Unicode letters from operators... */
static bool isIdentifierCharacter (int c)
{
    return (bool) (isIdentifierFirstCharacter(c) || (isAscii(c) && (isdigit(c) || c == '!')) || c >= 0x80);
}

static void skipWhitespace (lexerState *lexer, bool newline)
{
    while (isWhitespace(lexer->cur_c, newline))
    {
        advanceChar(lexer);
    }
}

/* The transpose operator is only allowed after an identifier, a number, an expression inside parenthesis or an index */
static bool isTranspose (int c)
{
    return (isIdentifierCharacter(c) || c == ')' || c == ']');
}


/*
 *  Lexer functions
 * */

/* Check that the current character sequence is a type declaration or inheritance */
static bool isTypeDecl (lexerState *lexer)
{
    if ((lexer->prev_c != '.' && lexer->cur_c == '<' && lexer->next_c == ':') ||
        (lexer->prev_c != '.' && lexer->cur_c == '>' && lexer->next_c == ':') ||
        (lexer->cur_c == ':' && lexer->next_c == ':') )
    {
        return true;
    }
    return false;
}

/* Check if the current char is a new line */
static bool isNewLine (lexerState *lexer)
{
    return (lexer->cur_c == '\n')? true: false;
}

/* Check if the current char is a new line.
 * If it is, skip the newline and return true */
static bool skipNewLine (lexerState *lexer)
{
    if (isNewLine(lexer))
    {
        advanceChar(lexer);
        return true;
    }
    return false;
}

/* Skip a single comment or multiline comment
 * A single line comment starts with #
 * A multi-line comment is encapsulated in #=...=# and they are nesting
 * */
static void skipComment (lexerState *lexer)
{
    /* # */
    if (lexer->next_c != '=')
    {
        advanceNChar(lexer, 1);
        while (lexer->cur_c != EOF && lexer->cur_c != '\n')
        {
            advanceChar(lexer);
        }
    }
    /* block comment */
    else /* if (lexer->next_c == '=') */
    {
        int level = 1;
        advanceNChar(lexer, 2);
        while (lexer->cur_c != EOF && level > 0)
        {
            if (lexer->cur_c == '=' && lexer->next_c == '#')
            {
                level--;
                advanceNChar(lexer, 2);
            }
            else if (lexer->cur_c == '#' && lexer->next_c == '=')
            {
                level++;
                advanceNChar(lexer, 2);
            }
            else
            {
                advanceChar(lexer);
            }
        }
    }
}

static void scanIdentifier (lexerState *lexer, bool clear)
{
    if (clear)
    {
        vStringClear(lexer->token_str);
    }

    do
    {
        advanceAndStoreChar(lexer);
    } while(lexer->cur_c != EOF && isIdentifierCharacter(lexer->cur_c));
}

/* Scan a quote-like expression.
 * Allow for triple-character variand and interpolation with `$`.
 * These last past the end of the line, so be careful
 * not to store too much of them (see MAX_STRING_LENGTH). */
static void scanStringOrCommand (lexerState *lexer, int c)
{
    bool istriple = false;

    /* Pass the first "quote"-character */
    advanceAndStoreChar(lexer);

    /* Check for triple "quote"-character */
    if (lexer->cur_c == c && lexer->next_c == c)
    {
        istriple = true;
        advanceAndStoreChar(lexer);
        advanceAndStoreChar(lexer);

        /* Cancel up to 2 "quote"-characters after opening the triple */
        if (lexer->cur_c == c)
        {
            advanceAndStoreChar(lexer);
            if (lexer->cur_c == c)
            {
                advanceAndStoreChar(lexer);
            }
        }
    }

    while (lexer->cur_c != EOF && lexer->cur_c != c)
    {
        /* Check for interpolation before checking for end of "quote" */
        if (lexer->cur_c == '$' && lexer->next_c == '(')
        {
            advanceAndStoreChar(lexer);
            scanParenBlock(lexer);
            /* continue to avoid advance character again. Correct bug
             * with "quote"-character just after closing parenthesis */
            continue;
        }

        if (lexer->cur_c == '\\' &&
            (lexer->next_c == c || lexer->next_c == '\\'))
        {
            advanceAndStoreChar(lexer);
        }
        advanceAndStoreChar(lexer);

        /* Cancel up to 2 "quote"-characters if triple string */
        if (istriple && lexer->cur_c == c)
        {
            advanceAndStoreChar(lexer);
            if (lexer->cur_c == c)
            {
                advanceAndStoreChar(lexer);
            }
        }
    }
    /* Pass the last "quote"-character */
    advanceAndStoreChar(lexer);
}


/* Scan commands surrounded by backticks,
 * possibly triple backticks */
static void scanCommand (lexerState *lexer)
{
    scanStringOrCommand(lexer, '`');
}

/* Double-quoted strings,
 * possibly triple doublequotes */
static void scanString (lexerState *lexer)
{
    scanStringOrCommand(lexer, '"');
}


/* This deals with character literals: 'n', '\n', '\uFFFF';
 * and matrix transpose: A'.
 * We'll use this approximate regexp for the literals:
 * \' [^'] \' or \' \\ [^']+ \' or \' \\ \' \'
 * Either way, we'll treat this token as a string, so it gets preserved */
static bool scanCharacterOrTranspose (lexerState *lexer)
{
    if (isTranspose(lexer->prev_c))
    {
        /* deal with untranspose/transpose sequence */
        while (lexer->cur_c != EOF && lexer->cur_c == '\'')
        {
            advanceAndStoreChar(lexer);
        }
        return false;
    }

    //vStringClear(lexer->token_str);
    advanceAndStoreChar(lexer);

    if (lexer->cur_c == '\\')
    {
        advanceAndStoreChar(lexer);
        /* The \' \\ \' \' (literally '\'') case */
        if (lexer->cur_c == '\'' && lexer->next_c == '\'')
        {
            advanceAndStoreChar(lexer);
            advanceAndStoreChar(lexer);
        }
        /* The \' \\ [^']+ \' case */
        else
        {
            while (lexer->cur_c != EOF && lexer->cur_c != '\'')
            {
                advanceAndStoreChar(lexer);
            }
        }
    }
    /* The \' [^'] \' and  \' \' \' cases */
    else if (lexer->next_c == '\'')
    {
        advanceAndStoreChar(lexer);
        advanceAndStoreChar(lexer);
    }
    /* Otherwise it is malformed */
    return true;
}

/* Parse a block with opening and closing character */
static void scanBlock (lexerState *lexer, int open, int close, bool convert_newline)
{
    /* Assume the current char is `open` */
    int level = 1;

    /* Pass the first opening */
    advanceAndStoreChar(lexer);

    while (lexer->cur_c != EOF && level > 0)
    {
        /* Parse everything */
        if (lexer->cur_c == ' ' || lexer->cur_c == '\t')
        {
            skipWhitespace(lexer, false);
            vStringPut(lexer->token_str, ' ');
        }
        if (lexer->cur_c == '#')
        {
            skipComment(lexer);
        }
        else if (lexer->cur_c == '\"')
        {
            scanString(lexer);
        }
        else if (lexer->cur_c == '\'')
        {
            scanCharacterOrTranspose(lexer);
        }

        /* Parse opening/closing */
        if (lexer->cur_c == open)
        {
            level++;
        }
        else if (lexer->cur_c == close)
        {
            level--;
        }

        if (convert_newline && skipNewLine(lexer))
        {
            vStringPut(lexer->token_str, ' ');
        }
        else
        {
            advanceAndStoreChar(lexer);
        }

    }
    /* Lexer position is just after `close` */
}


/* Parse a block inside parenthesis, for example a function argument list */
static void scanParenBlock (lexerState *lexer)
{
    scanBlock(lexer, '(', ')', true);
}

/* Indexing block with bracket.
 * Some keywords have a special meaning in this environment:
 * end, begin, for and if */
static void scanIndexBlock (lexerState *lexer)
{
    scanBlock(lexer, '[', ']', false);

}

/* Parse a block inside curly brackets, for type parametrization */
static void scanCurlyBlock (lexerState *lexer)
{
    scanBlock(lexer, '{', '}', true);
}

/* Scan type annotation like
 * `::Type`, `::Type{T}`
 */
static void scanTypeAnnotation (lexerState *lexer)
{
    /* assume that current char is '<', '>' or ':', followed by ':' */
    advanceAndStoreChar(lexer);
    advanceAndStoreChar(lexer);

    skipWhitespace(lexer, true);
    scanIdentifier(lexer, false);
    if (lexer->cur_c == '{')
    {
        scanCurlyBlock(lexer);
    }
}

/* Scan type annotation like
 * `where Int<:T<:Real`, `where S<:Array{Real}` or `where {S, T}`
 */
static void scanTypeWhere (lexerState *lexer)
{
    /* assume that current token is 'where'
     * allow line continuation */
    vStringPut(lexer->token_str, ' ');
    skipWhitespace(lexer, true);

    while (lexer->cur_c != EOF)
    {

        if (lexer->cur_c == '{')
        {
            scanCurlyBlock(lexer);
        }
        else if (isIdentifierFirstCharacter(lexer->cur_c))
        {
            scanIdentifier(lexer, false);
            if (endswith(vStringValue(lexer->token_str), "where"))
            {
                /* allow line continuation */
                vStringPut(lexer->token_str, ' ');
                skipWhitespace(lexer, true);
            }
        }
        else if (isTypeDecl(lexer))
        {
            scanTypeAnnotation(lexer);
            //skipWhitespace(lexer, false);
        }
        else if (lexer->cur_c == '#')
        {
            skipComment(lexer);
            /* allow line continuation */
            if (endswith(vStringValue(lexer->token_str), "where "))
            {
                skipWhitespace(lexer, true);
            }
        }
        else if (isWhitespace(lexer->cur_c, false))
        {
            while (isWhitespace(lexer->cur_c, false))
            {
                advanceChar(lexer);
            }
            /* Add a space, if it is not a trailing space */
            if (!(isNewLine(lexer)))
            {
                vStringPut(lexer->token_str, ' ');
            }
        }
        else
        {
            break;
        }
    }
}


static int parseIdentifier (lexerState *lexer)
{
    langType julia = getInputLanguage ();
    scanIdentifier(lexer, true);

    int k = lookupKeyword (vStringValue(lexer->token_str), julia);
    /* First part of a composed identifier */
    if (k == TOKEN_COMPOSER_KWD)
    {
        skipWhitespace(lexer, false);
        scanIdentifier(lexer, true);
        k = lookupKeyword (vStringValue(lexer->token_str), julia);
    }

    if ((k == TOKEN_OPEN_BLOCK)
        || (k == TOKEN_MODULE)
        || (k == TOKEN_IMPORT)
        || (k == TOKEN_USING)
        || (k == TOKEN_EXPORT)
        || (k == TOKEN_CONST)
        || (k == TOKEN_MACRO)
        || (k == TOKEN_FUNCTION)
        || (k == TOKEN_STRUCT)
        || (k == TOKEN_TYPE)
        || (k == TOKEN_TYPE_WHERE)
        || (k == TOKEN_CLOSE_BLOCK))
    {
        if (k == TOKEN_TYPE_WHERE)
        {
            scanTypeWhere(lexer);
        }
        return lexer->cur_token = k;
    }
    return lexer->cur_token = TOKEN_IDENTIFIER;
}


/* Advances the parser one token, optionally skipping whitespace
 * (otherwise it is concatenated and returned as a single whitespace token).
 * Whitespace is needed to properly render function signatures. Unrecognized
 * token starts are stored literally, e.g. token may equal to a character '#'. */
static int advanceToken (lexerState *lexer, bool skip_whitespace, bool propagate_first)
{
    bool have_whitespace = false;
    bool newline = false;
    lexer->line = getInputLineNumber();
    lexer->pos = getInputFilePosition();

    /* the next token is the first token of the line */
    if (!propagate_first)
    {
        if (lexer->cur_token == TOKEN_NEWLINE ||
            lexer->cur_token == TOKEN_SEMICOLON ||
            lexer->cur_token == TOKEN_NONE ||
            (lexer->first_token && lexer->cur_token == TOKEN_MACROCALL))
        {
            lexer->first_token = true;
        }
        else
        {
            lexer->first_token = false;
        }
    }

    while (lexer->cur_c != EOF)
    {
        /* skip whitespaces but not newlines */
        if (isWhitespace(lexer->cur_c, newline))
        {
            skipWhitespace(lexer, newline);
            have_whitespace = true;
        }
        else if (lexer->cur_c == '#')
        {
            skipComment(lexer);
            have_whitespace = true;
        }
        else
        {
            if (have_whitespace && !skip_whitespace)
            {
                return lexer->cur_token = TOKEN_WHITESPACE;
            }
            break;
        }
    }
    lexer->line = getInputLineNumber();
    lexer->pos = getInputFilePosition();
    while (lexer->cur_c != EOF)
    {
        if (lexer->cur_c == '"')
        {
            vStringClear(lexer->token_str);
            scanString(lexer);
            return lexer->cur_token = TOKEN_STRING;
        }
        else if (lexer->cur_c == '\'')
        {
            vStringClear(lexer->token_str);
            if (scanCharacterOrTranspose(lexer))
            {
                return lexer->cur_token = TOKEN_STRING;
            }
            else
            {
                return lexer->cur_token = '\'';
            }
        }
        else if (lexer->cur_c == '`')
        {
            vStringClear(lexer->token_str);
            scanCommand(lexer);
            return lexer->cur_token = TOKEN_COMMAND;
        }
        else if (isIdentifierFirstCharacter(lexer->cur_c))
        {
            return parseIdentifier(lexer);
        }
        else if (lexer->cur_c == '@')
        {
            vStringClear(lexer->token_str);
            advanceAndStoreChar(lexer);
            do
            {
                advanceAndStoreChar(lexer);
            } while(lexer->cur_c != EOF && isIdentifierCharacter(lexer->cur_c));
            return lexer->cur_token = TOKEN_MACROCALL;
        }
        else if (lexer->cur_c == '(')
        {
            vStringClear(lexer->token_str);
            scanParenBlock(lexer);
            return lexer->cur_token = TOKEN_PAREN_BLOCK;
        }
        else if (lexer->cur_c == '[')
        {
            vStringClear(lexer->token_str);
            scanIndexBlock(lexer);
            return lexer->cur_token = TOKEN_BRACKET_BLOCK;
        }
        else if (lexer->cur_c == '{')
        {
            vStringClear(lexer->token_str);
            scanCurlyBlock(lexer);
            return lexer->cur_token = TOKEN_CURLY_BLOCK;
        }
        else if (isTypeDecl(lexer))
        {
            vStringClear(lexer->token_str);
            scanTypeAnnotation(lexer);
            return lexer->cur_token = TOKEN_TYPE_ANNOTATION;
        }
        else if (skipNewLine(lexer))
        {
            /* allow line continuation */
            if (isOperator(lexer->cur_token))
            {
                return lexer->cur_token;
            }
            return lexer->cur_token = TOKEN_NEWLINE;
        }
        else if (lexer->cur_c == ';')
        {
            advanceChar(lexer);
            return lexer->cur_token = TOKEN_SEMICOLON;
        }
        else
        {
            int c = lexer->cur_c;
            advanceChar(lexer);
            return lexer->cur_token = c;
        }
    }
    return lexer->cur_token = TOKEN_EOF;
}

static void initLexer (lexerState *lexer)
{
    advanceNChar(lexer, 2);
    lexer->token_str = vStringNew();
    lexer->first_token = true;
    lexer->cur_token = TOKEN_NONE;
    lexer->prev_c = '\0';

    if (lexer->cur_c == '#' && lexer->next_c == '!')
    {
        skipComment(lexer);
    }
    advanceToken(lexer, true, false);
}

static void deInitLexer (lexerState *lexer)
{
    vStringDelete(lexer->token_str);
    lexer->token_str = NULL;
}

#if 0
static void debugLexer (lexerState *lexer)
{
    printf("Current lexer state: line %d, token (%lu), cur char `%c`, token str:\n\t`", lexer->line, lexer->cur_token, lexer->cur_c);
    printf(vStringValue(lexer->token_str));
    printf("`\n");
}
#endif

static void addTag (vString* ident, const char* type, const char* arg_list, int kind, unsigned long line, MIOPos pos, vString *scope, int parent_kind)
{
    if (kind == K_NONE)
    {
        return;
    }
    tagEntryInfo tag;
    initTagEntry(&tag, vStringValue(ident), kind);

    tag.lineNumber = line;
    tag.filePosition = pos;
    tag.sourceFileName = getInputFileName();

    tag.extensionFields.signature = arg_list;
    /* tag.extensionFields.varType = type; */  /* Needs a workaround */
    if (parent_kind != K_NONE)
    {
        tag.extensionFields.scopeKindIndex = parent_kind;
        tag.extensionFields.scopeName = vStringValue(scope);
    }
    makeTagEntry(&tag);
}

static void addReferenceTag (vString* ident, int kind, int role, unsigned long line, MIOPos pos, vString* scope, int parent_kind)
{
    if (kind == K_NONE)
    {
        return;
    }
    tagEntryInfo tag;
    initRefTagEntry(&tag, vStringValue(ident), kind, role);
    tag.lineNumber = line;
    tag.filePosition = pos;
    if (parent_kind != K_NONE)
    {
        tag.extensionFields.scopeKindIndex = parent_kind;
        tag.extensionFields.scopeName = vStringValue(scope);
    }
    makeTagEntry(&tag);
}

/* Skip tokens until one of the goal tokens is hit. Escapes when level = 0 if there are no goal tokens.
 * Keeps track of balanced ()'s, []'s, and {}'s and ignores the goal tokens within those pairings */
static void skipUntil (lexerState *lexer, int goal_tokens[], int num_goal_tokens)
{
    int block_level = 0;

    while (lexer->cur_token != TOKEN_EOF)
    {
        /* check if the keyword is reached, only if outside a block */
        if (block_level == 0)
        {
            int ii = 0;
            for(ii = 0; ii < num_goal_tokens; ii++)
            {
                if (lexer->cur_token == goal_tokens[ii])
                {
                    break;
                }
            }
            if (ii < num_goal_tokens)
            {
                /* parse the next token */
                advanceToken(lexer, true, false);
                break;
            }
        }

        /* take into account nested blocks */
        switch (lexer->cur_token)
        {
            case TOKEN_OPEN_BLOCK:
                block_level++;
                break;
            case TOKEN_CLOSE_BLOCK:
                block_level--;
                break;
            default:
                break;
        }

        /* Has to be after the token switch to catch the case when we start with the initial level token */
        if (num_goal_tokens == 0 && block_level == 0)
        {
            break;
        }

        advanceToken(lexer, true, false);
    }
}

/* Skip until the end of the block */
static void skipUntilEnd (lexerState *lexer)
{
    int goal_tokens[] = { TOKEN_CLOSE_BLOCK };

    skipUntil(lexer, goal_tokens, 1);
}

/* Skip a function body after assignment operator '='
 * Beware of continuation lines after operators
 *  */
static void skipBody (lexerState *lexer)
{
    /* assume position just after '=' */
    while (lexer->cur_token != TOKEN_EOF && lexer->cur_token != TOKEN_NEWLINE)
    {
        advanceToken(lexer, true, false);

        if (lexer->cur_token == TOKEN_OPEN_BLOCK)
        {
            /* pass the keyword */
            advanceToken(lexer, true, false);
            skipUntilEnd(lexer);
            /* the next token is already selected */
        }
    }
}

/* Short function format:
 * <ident> ( [<args>] ) [::<type>] [<where>] = [begin] <body> [end]
 * */
static void parseShortFunction (lexerState *lexer, vString *scope, int parent_kind)
{
    /* assume the current char is just after identifier */
    vString *name;
    vString *arg_list;
    unsigned long line;
    MIOPos pos;

    /* should be an open parenthesis after identifier
     * with potentially parametric type */
    skipWhitespace(lexer, false);
    if (lexer->cur_c == '{')
    {
        scanCurlyBlock(lexer);
        skipWhitespace(lexer, false);
    }

    if (lexer->cur_c != '(')
    {
        advanceToken(lexer, true, false);
        return;
    }

    name = vStringNewCopy(lexer->token_str);
    line = lexer->line;
    pos = lexer->pos;

    /* scan argument list */
    advanceToken(lexer, true, false);
    arg_list = vStringNewCopy(lexer->token_str);

    /* scan potential type casting */
    advanceToken(lexer, true, false);
    if (lexer->cur_token == TOKEN_TYPE_ANNOTATION)
    {
        vStringCat(arg_list, lexer->token_str);
        advanceToken(lexer, true, false);
    }
    /* scan potential type union with 'where' */
    if (lexer->cur_token == TOKEN_TYPE_WHERE)
    {
        vStringPut(arg_list, ' ');
        vStringCat(arg_list, lexer->token_str);
        advanceToken(lexer, true, false);
    }

    /* scan equal sign, ignore `==` and `=>` */
    if (!(lexer->cur_token == '=' &&
          lexer->cur_c != '=' &&
          lexer->cur_c != '>'))
    {
        vStringDelete(name);
        vStringDelete(arg_list);
        return;
    }

    addTag(name, NULL, vStringValue(arg_list), K_FUNCTION, line, pos, scope, parent_kind);

    /* scan until end of function definition */
    skipBody(lexer);

    /* Should end on a new line, parse next token */
    advanceToken(lexer, true, false);
    lexer->first_token = true;

    vStringDelete(name);
    vStringDelete(arg_list);
}

/* Function format:
 * function <ident> ( [<args>] ) [::<type>] [<where>] [<body>] end
 * */
static void parseFunction (lexerState *lexer, vString *scope, int parent_kind)
{
    vString *name;
    vString *arg_list;
    vString *local_scope;
    int local_parent_kind;
    unsigned long line;
    MIOPos pos;

    advanceToken(lexer, true, false);
    if (lexer->cur_token != TOKEN_IDENTIFIER)
    {
        return;
    }
    else if (lexer->cur_c == '.')
    {
        local_scope = vStringNewCopy(lexer->token_str);
        local_parent_kind = K_MODULE;
        advanceChar(lexer);
        advanceToken(lexer, true, false);
    }
    else
    {
        local_scope = vStringNewCopy(scope);
        local_parent_kind = parent_kind;
    }

    /* Scan for parametric type constructor */
    skipWhitespace(lexer, false);
    if (lexer->cur_c == '{')
    {
        scanCurlyBlock(lexer);
        skipWhitespace(lexer, false);
    }

    name = vStringNewCopy(lexer->token_str);
    arg_list = vStringNew();
    line = lexer->line;
    pos = lexer->pos;

    advanceToken(lexer, true, false);
    if (lexer->cur_token == TOKEN_PAREN_BLOCK)
    {
        vStringCopy(arg_list, lexer->token_str);

        /* scan potential type casting */
        advanceToken(lexer, true, false);
        if (lexer->cur_token == TOKEN_TYPE_ANNOTATION)
        {
            vStringCat(arg_list, lexer->token_str);
            advanceToken(lexer, true, false);
        }
        /* scan potential type union with 'where' */
        if (lexer->cur_token == TOKEN_TYPE_WHERE)
        {
            vStringPut(arg_list, ' ');
            vStringCat(arg_list, lexer->token_str);
            advanceToken(lexer, true, false);
        }

        addTag(name, NULL, vStringValue(arg_list), K_FUNCTION, line, pos, local_scope, local_parent_kind);
        addToScope(scope, name);
        parseExpr(lexer, true, K_FUNCTION, scope);
    }
    else if (lexer->cur_token == TOKEN_CLOSE_BLOCK)
    {
        /* Function without method */
        addTag(name, NULL, NULL, K_FUNCTION, line, pos, local_scope, local_parent_kind);
        /* Go to the closing 'end' keyword */
        skipUntilEnd(lexer);
    }

    vStringDelete(name);
    vStringDelete(arg_list);
    vStringDelete(local_scope);
}

/* Macro format:
 * "macro" <ident>()
 */
static void parseMacro (lexerState *lexer, vString *scope, int parent_kind)
{
    vString *name;
    unsigned long line;
    MIOPos pos;

    advanceToken(lexer, true, false);
    if (lexer->cur_token != TOKEN_IDENTIFIER)
    {
        return;
    }

    name = vStringNewCopy(lexer->token_str);
    line = lexer->line;
    pos = lexer->pos;

    advanceToken(lexer, true, false);
    if (lexer->cur_token == TOKEN_PAREN_BLOCK)
    {
        addTag(name, NULL, vStringValue(lexer->token_str), K_MACRO, line, pos, scope, parent_kind);
    }

    skipUntilEnd(lexer);
    vStringDelete(name);
}

/* Const format:
 * "const" <ident>
 */
static void parseConst (lexerState *lexer, vString *scope, int parent_kind)
{
    vString *name;

    advanceToken(lexer, true, false);
    if (lexer->cur_token != TOKEN_IDENTIFIER)
    {
        return;
    }

    name = vStringNewCopy(lexer->token_str);

    advanceToken(lexer, true, false);
    if (lexer->cur_token == TOKEN_TYPE_ANNOTATION)
    {
        addTag(name, "const", vStringValue(lexer->token_str), K_CONSTANT, lexer->line, lexer->pos, scope, parent_kind);
        advanceToken(lexer, true, false);
    }
    else
    {
        addTag(name, "const", NULL, K_CONSTANT, lexer->line, lexer->pos, scope, parent_kind);
    }

    vStringDelete(name);
}

/* Type format:
 * [ "abstract" | "primitive" ] "type" <ident>
 */
static void parseType (lexerState *lexer, vString *scope, int parent_kind)
{
    advanceToken(lexer, true, false);
    if (lexer->cur_token != TOKEN_IDENTIFIER)
    {
        return;
    }

    addTag(lexer->token_str, NULL, NULL, K_TYPE, lexer->line, lexer->pos, scope, parent_kind);

    skipUntilEnd(lexer);
}

/* Module format:
 * [ "baremodule" | "module" ] <ident>
 */
static void parseModule (lexerState *lexer, vString *scope, int parent_kind)
{
    advanceToken(lexer, true, false);
    if (lexer->cur_token != TOKEN_IDENTIFIER)
    {
        return;
    }

    addTag(lexer->token_str, NULL, NULL, K_MODULE, lexer->line, lexer->pos, scope, parent_kind);
    addToScope(scope, lexer->token_str);
    advanceToken(lexer, true, false);
    parseExpr(lexer, true, K_MODULE, scope);
}

/*
 * Parse comma separated entity in import/using expressions. An entity could be
 * in the form of "Module" or "Module.symbol". The lexer should be at the end
 * of "Module", and this function will take it to the end of the entity
 * (whitespaces also skipped).
 */
static void parseImportEntity (lexerState *lexer, vString *scope, int token_type, int parent_kind)
{
    if (lexer->cur_c == '.')
    {
        if (token_type == TOKEN_IMPORT)
        {
            vString *module_name = vStringNewCopy(lexer->token_str);
            addReferenceTag(module_name, K_MODULE, JULIA_MODULE_NAMESPACE, lexer->line, lexer->pos, scope, parent_kind);
            advanceChar(lexer);
            advanceToken(lexer, true, false);
            addReferenceTag(lexer->token_str, K_UNKNOWN, JULIA_UNKNOWN_IMPORTED, lexer->line, lexer->pos, module_name, K_MODULE);
            vStringDelete(module_name);
        }
        else /* if (token_type == TOKEN_USING) */
        {
            /* using Module.symbol is invalid, so we advance the lexer but don't tag it. */
            advanceChar(lexer);
            advanceToken(lexer, true, false);
        }
    }
    else
    {
        if (token_type == TOKEN_IMPORT)
        {
            addReferenceTag(lexer->token_str, K_MODULE, JULIA_MODULE_IMPORTED, lexer->line, lexer->pos, scope, parent_kind);
        }
        else /* if (token_type == TOKEN_USING) */
        {
            addReferenceTag(lexer->token_str, K_MODULE, JULIA_MODULE_USED, lexer->line, lexer->pos, scope, parent_kind);
        }
    }
}

/* Parse import/using expressions with a colon, like: */
/* import Module: symbol1, symbol2 */
/* using Module: symbol1, symbol2 */
/* The lexer should be at the end of "Module", and this function will take it
 * to the end of the token after this expression (whitespaces also skipped). */
static void parseColonImportExpr (lexerState *lexer, vString *scope, int token_type, int parent_kind)
{
    int symbol_role;
    if (token_type == TOKEN_IMPORT)
    {
        symbol_role = JULIA_UNKNOWN_IMPORTED;
    }
    else /* if (token_type == TOKEN_USING) */
    {
        symbol_role = JULIA_UNKNOWN_USED;
    }
    vString *name = vStringNewCopy(lexer->token_str);
    addReferenceTag(name, K_MODULE, JULIA_MODULE_NAMESPACE, lexer->line, lexer->pos, scope, parent_kind);
    advanceChar(lexer);
    advanceToken(lexer, true, false);
    if (lexer->cur_token == TOKEN_NEWLINE)
    {
        advanceToken(lexer, true, false);
    }
    while (lexer->cur_token == TOKEN_IDENTIFIER || lexer->cur_token == TOKEN_MACROCALL)
    {
        addReferenceTag(lexer->token_str, K_UNKNOWN, symbol_role, lexer->line, lexer->pos, name, K_MODULE);
        if (lexer->cur_c == ',')
        {
            advanceChar(lexer);
            advanceToken(lexer, true, false);
            if (lexer->cur_token == TOKEN_NEWLINE)
            {
                advanceToken(lexer, true, false);
            }
        }
        else
        {
            advanceToken(lexer, true, false);
        }
    }
    vStringDelete(name);
}

/* Import format:
 * [ "import" | "using" ] <ident> [: <name>]
 */
static void parseImport (lexerState *lexer, vString *scope, int token_type, int parent_kind)
{
    /* capture the imported name */
    advanceToken(lexer, true, false);
    /* import Mod1: symbol1, symbol2 */
    /* using Mod1: symbol1, symbol2 */
    if (lexer->cur_c == ':')
    {
        parseColonImportExpr(lexer, scope, token_type, parent_kind);
    }
    /* All other situations, like import/using Mod1, Mod2.symbol1, Mod3... */
    else
    {
        while (lexer->cur_token == TOKEN_IDENTIFIER || lexer->cur_token == TOKEN_MACROCALL)
        {
            parseImportEntity(lexer, scope, token_type, parent_kind);
            if (lexer->cur_c == ',')
            {
                advanceChar(lexer);
                advanceToken(lexer, true, false);
                if (lexer->cur_token == TOKEN_NEWLINE)
                {
                    advanceToken(lexer, true, false);
                }
            }
            else
            {
                advanceToken(lexer, true, false);
            }
        }
    }
}

/* Structs format:
 * "struct" <ident>[{<param>}] [<:<type>]; <fields> <inner constructor> end
 * */
static void parseStruct (lexerState *lexer, vString *scope, int parent_kind)
{
    vString *name;
    vString *field;
    size_t old_scope_len;
    unsigned long line;
    MIOPos pos;

    advanceToken(lexer, true, false);
    if (lexer->cur_token != TOKEN_IDENTIFIER)
    {
        return;
    }

    name = vStringNewCopy(lexer->token_str);
    field = vStringNew();
    line = lexer->line;
    pos = lexer->pos;

    /* scan parametrization */
    advanceToken(lexer, true, false);
    if (lexer->cur_token == TOKEN_CURLY_BLOCK)
    {
        addTag(name, NULL, vStringValue(lexer->token_str), K_STRUCT, line, pos, scope, parent_kind);
        advanceToken(lexer, true, false);
    }
    else
    {
        addTag(name, NULL, NULL, K_STRUCT, line, pos, scope, parent_kind);
    }
    addToScope(scope, name);

    /* skip inheritance */
    if (lexer->cur_token == TOKEN_TYPE_ANNOTATION)
    {
        advanceToken(lexer, true, false);
    }

    /* keep the struct scope in memory to reset it after parsing constructors */
    old_scope_len = vStringLength(scope);
    /* Parse fields and inner constructors */
    while (lexer->cur_token != TOKEN_EOF && lexer->cur_token != TOKEN_CLOSE_BLOCK)
    {
        if (lexer->cur_token == TOKEN_IDENTIFIER && lexer->first_token)
        {
            if (strcmp(vStringValue(lexer->token_str), vStringValue(name)) == 0)
            {
                /* inner constructor */
                parseShortFunction(lexer, scope, K_STRUCT);
                continue;
            }

            vStringCopy(field, lexer->token_str);

            /* parse type annotation */
            advanceToken(lexer, true, false);
            if (lexer->cur_token == TOKEN_TYPE_ANNOTATION)
            {
                addTag(field, NULL, vStringValue(lexer->token_str), K_FIELD, lexer->line, lexer->pos, scope, K_STRUCT);
                advanceToken(lexer, true, false);
            }
            else
            {
                addTag(field, NULL, NULL, K_FIELD, lexer->line, lexer->pos, scope, K_STRUCT);
            }
        }
        else if (lexer->cur_token == TOKEN_FUNCTION)
        {
            /* inner constructor */
            parseFunction(lexer, scope, K_STRUCT);
        }
        else
        {
            /* Get next token */
            advanceToken(lexer, true, false);
        }
        resetScope(scope, old_scope_len);
    }

    vStringDelete(name);
    vStringDelete(field);
}


static void parseExpr (lexerState *lexer, bool delim, int kind, vString *scope)
{
    int level = 1;
    size_t old_scope_len;
    vString *local_scope = NULL;

    while (lexer->cur_token != TOKEN_EOF)
    {
        old_scope_len = vStringLength(scope);
        /* Advance token and update if this is a new line */
        while (lexer->cur_token == TOKEN_NEWLINE ||
               lexer->cur_token == TOKEN_SEMICOLON ||
               lexer->cur_token == TOKEN_NONE )
        {
            advanceToken(lexer, true, false);
        }

        /* Make sure every case advances the token
         * otherwise we can be stuck in infinite loop */
        switch (lexer->cur_token)
        {
            case TOKEN_CONST:
                parseConst(lexer, scope, kind);
                break;
            case TOKEN_FUNCTION:
                parseFunction(lexer, scope, kind);
                break;
            case TOKEN_MACRO:
                parseMacro(lexer, scope, kind);
                break;
            case TOKEN_MODULE:
                parseModule(lexer, scope, kind);
                break;
            case TOKEN_STRUCT:
                parseStruct(lexer, scope, kind);
                break;
            case TOKEN_TYPE:
                parseType(lexer, scope, kind);
                break;
            case TOKEN_IMPORT:
                parseImport(lexer, scope, TOKEN_IMPORT, kind);
                break;
            case TOKEN_USING:
                parseImport(lexer, scope, TOKEN_USING, kind);
            case TOKEN_IDENTIFIER:
                if (lexer->first_token && lexer->cur_c == '.')
                {
                    if (local_scope == NULL)
                    {
                        local_scope = vStringNew();
                    }
                    vStringCopy(local_scope, lexer->token_str);
                    advanceChar(lexer);
                    // next token, but keep the first_token value
                    advanceToken(lexer, true, true);
                    skipWhitespace(lexer, false);
                    if (lexer->cur_c == '(')
                    {
                        parseShortFunction(lexer, local_scope, K_MODULE);
                    }
                }
                else
                {
                    skipWhitespace(lexer, false);
                    if (lexer->first_token && (lexer->cur_c == '(' || lexer->cur_c == '{'))
                    {
                        parseShortFunction(lexer, scope, kind);
                    }
                    else
                    {
                        advanceToken(lexer, true, false);
                    }
                }
                break;
            case TOKEN_OPEN_BLOCK:
                level++;
                advanceToken(lexer, true, false);
                break;
            case TOKEN_CLOSE_BLOCK:
                level--;
                advanceToken(lexer, true, false);
                break;
            default:
                advanceToken(lexer, true, false);
                break;
        }
        resetScope(scope, old_scope_len);
        if (delim && level <= 0)
        {
            break;
        }
    }
    vStringDelete(local_scope);
}

static void findJuliaTags (void)
{
    lexerState lexer;
    vString* scope = vStringNew();
    initLexer(&lexer);

    parseExpr(&lexer, false, K_NONE, scope);
    vStringDelete(scope);

    deInitLexer(&lexer);
}

extern parserDefinition* JuliaParser (void)
{
    static const char *const extensions [] = { "jl", NULL };
    parserDefinition* def = parserNew ("Julia");
    def->kindTable  = JuliaKinds;
    def->kindCount  = ARRAY_SIZE (JuliaKinds);
    def->extensions = extensions;
    def->parser     = findJuliaTags;
    def->keywordTable = JuliaKeywordTable;
    def->keywordCount = ARRAY_SIZE (JuliaKeywordTable);
    return def;
}
