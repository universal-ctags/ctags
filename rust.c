/*
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Rust files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */
#include "main.h"

#include <string.h>

#include "keyword.h"
#include "parse.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "vstring.h"

/*
*   MACROS
*/
#define MAX_STRING_LENGTH 256

/*
*   DATA DECLARATIONS
*/

typedef enum {
	K_MOD,
	K_STRUCT,
	K_TRAIT,
	K_IMPL,
	K_FN,
	K_ENUM,
	K_TYPE,
	K_STATIC,
	K_MACRO,
	K_FIELD,
	K_VARIANT,
	K_METHOD,
	K_NONE
} RustKind;

static kindOption rustKinds[] = {
	{TRUE, 'n', "namespace", "module"},
	{TRUE, 's', "struct", "structural type"},
	{TRUE, 'i', "interface", "trait interface"},
	{TRUE, 'c', "class", "implementation"},
	{TRUE, 'f', "function", "Function"},
	{TRUE, 'g', "enum", "Enum"},
	{TRUE, 't', "typedef", "Type Alias"},
	{TRUE, 'v', "variable", "Global variable"},
	{TRUE, 'M', "macro", "Macro Definition"},
	{TRUE, 'm', "field", "A struct field"},
	{TRUE, 'e', "enumerator", "An enum variant"},
	{TRUE, 'F', "method", "A method"},
};

typedef enum {
	TOKEN_WHITESPACE,
	TOKEN_STRING,
	TOKEN_IDENT,
	TOKEN_LSHIFT,
	TOKEN_RSHIFT,
	TOKEN_RARROW,
	TOKEN_EOF
} tokenType;

typedef struct {
	/* Characters */
	int cur_c;
	int next_c;

	/* Tokens */
	int cur_token;
	vString* token_str;
	unsigned long line;
	fpos_t pos;
} lexerState;

/*
*   FUNCTION PROTOTYPES
*/

static void parseBlock (lexerState *lexer, boolean delim, int kind, vString *scope);

/*
*   FUNCTION DEFINITIONS
*/

/* Resets the scope string to the old length */
static void resetScope (vString *scope, size_t old_len)
{
	scope->length = old_len;
	scope->buffer[old_len] = '\0';
}

/* Adds a name to the end of the scope string */
static void addToScope (vString *scope, vString *name)
{
	if (vStringLength(scope) > 0)
		vStringCatS(scope, "::");
	vStringCat(scope, name);
}

/* Write the lexer's current token to string, taking care of special tokens */
static void writeCurTokenToStr (lexerState *lexer, vString *out_str)
{
	switch (lexer->cur_token)
	{
		case TOKEN_IDENT:
			vStringCat(out_str, lexer->token_str);
			break;
		case TOKEN_STRING:
			vStringCat(out_str, lexer->token_str);
			break;
		case TOKEN_WHITESPACE:
			vStringPut(out_str, ' ');
			break;
		case TOKEN_LSHIFT:
			vStringCatS(out_str, "<<");
			break;
		case TOKEN_RSHIFT:
			vStringCatS(out_str, ">>");
			break;
		case TOKEN_RARROW:
			vStringCatS(out_str, "->");
			break;
		default:
			vStringPut(out_str, (char) lexer->cur_token);
	}
}

/* Reads a character from the file */
static void advanceChar (lexerState *lexer)
{
	lexer->cur_c = lexer->next_c;
	lexer->next_c = fileGetc();
}

/* Reads N characters from the file */
static void advanceNChar (lexerState *lexer, int n)
{
	while (n--)
		advanceChar(lexer);
}

/* Store the current character in lexerState::token_str if there is space
 * (set by MAX_STRING_LENGTH), and then read the next character from the file */
static void advanceAndStoreChar (lexerState *lexer)
{
	if (vStringLength(lexer->token_str) < MAX_STRING_LENGTH)
		vStringPut(lexer->token_str, (char) lexer->cur_c);
	advanceChar(lexer);
}

static boolean isWhitespace (int c)
{
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

static boolean isAscii (int c)
{
	return (c >= 0) && (c < 0x80);
}

/* This isn't quite right for Unicode identifiers */
static boolean isIdentifierStart (int c)
{
	return (isAscii(c) && (isalpha(c) || c == '_')) || !isAscii(c);
}

/* This isn't quite right for Unicode identifiers */
static boolean isIdentifierContinue (int c)
{
	return (isAscii(c) && (isalnum(c) || c == '_')) || !isAscii(c);
}

static void scanWhitespace (lexerState *lexer)
{
	while (isWhitespace(lexer->cur_c))
		advanceChar(lexer);
}

/* Normal line comments start with two /'s and continue until the next \n
 * (NOT any other newline character!). Additionally, a shebang in the beginning
 * of the file also counts as a line comment.
 * Block comments start with / followed by a * and end with a * followed by a /.
 * Unlike in C/C++ they nest. */
static void scanComments (lexerState *lexer)
{
	/* // or #! */
	if (lexer->next_c == '/' || lexer->next_c == '!')
	{
		advanceNChar(lexer, 2);
		while (lexer->cur_c != EOF && lexer->cur_c != '\n')
			advanceChar(lexer);
	}
	else if (lexer->next_c == '*')
	{
		int level = 1;
		advanceNChar(lexer, 2);
		while (lexer->cur_c != EOF && level > 0)
		{
			if (lexer->cur_c == '*' && lexer->next_c == '/')
			{
				level--;
				advanceNChar(lexer, 2);
			}
			else if (lexer->cur_c == '/' && lexer->next_c == '*')
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

static void scanIdentifier (lexerState *lexer)
{
	vStringClear(lexer->token_str);
	do
	{
		advanceAndStoreChar(lexer);
	} while(lexer->cur_c != EOF && isIdentifierContinue(lexer->cur_c));
}

/* Double-quoted strings, we only care about the \" escape. These
 * last past the end of the line, so be careful not too store too much
 * of them (see MAX_STRING_LENGTH). The only place we look at their
 * contents is in the function definitions, and there the valid strings are
 * things like "C" and "Rust" */
static void scanString (lexerState *lexer)
{
	vStringClear(lexer->token_str);
	advanceAndStoreChar(lexer);
	while (lexer->cur_c != EOF && lexer->cur_c != '"')
	{
		if (lexer->cur_c == '\\' && lexer->next_c == '"')
			advanceAndStoreChar(lexer);
		advanceAndStoreChar(lexer);
	}
	advanceAndStoreChar(lexer);
}

/* Raw strings look like this: r"" or r##""## where the number of
 * hashes must match */
static void scanRawString (lexerState *lexer)
{
	size_t num_initial_hashes = 0;
	vStringClear(lexer->token_str);
	advanceAndStoreChar(lexer);
	/* Count how many leading hashes there are */
	while (lexer->cur_c == '#')
	{
		num_initial_hashes++;
		advanceAndStoreChar(lexer);
	}
	if (lexer->cur_c != '"')
		return;
	advanceAndStoreChar(lexer);
	while (lexer->cur_c != EOF)
	{
		/* Count how many trailing hashes there are. If the number is equal or more
		 * than the number of leading hashes, break. */
		if (lexer->cur_c == '"')
		{
			size_t num_trailing_hashes = 0;
			advanceAndStoreChar(lexer);
			while (lexer->cur_c == '#' && num_trailing_hashes < num_initial_hashes)
			{
				num_trailing_hashes++;

				advanceAndStoreChar(lexer);
			}
			if (num_trailing_hashes == num_initial_hashes)
				break;
		}
		else
		{
			advanceAndStoreChar(lexer);
		}
	}
}

/* This deals with character literals: 'n', '\n', '\uFFFF'; and lifetimes:
 * 'lifetime. We'll use this approximate regexp for the literals:
 * \' \\ [^']+ \' or \' [^'] \' or \' \\ \' \'. Either way, we'll treat this
 * token as a string, so it gets preserved as is for function signatures with
 * lifetimes. */
static void scanCharacterOrLifetime (lexerState *lexer)
{
	vStringClear(lexer->token_str);
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
				advanceAndStoreChar(lexer);
		}
	}
	/* The \' [^'] \' case */
	else if (lexer->cur_c != '\'' && lexer->next_c == '\'')
	{
		advanceAndStoreChar(lexer);
		advanceAndStoreChar(lexer);
	}
	/* Otherwise it is malformed, or a lifetime */
}

/* Advances the parser one token, optionally skipping whitespace
 * (otherwise it is concatenated and returned as a single whitespace token).
 * Whitespace is needed to properly render function signatures. Unrecognized
 * token starts are stored literally, e.g. token may equal to a character '#'. */
static int advanceToken (lexerState *lexer, boolean skip_whitspace)
{
	boolean have_whitespace = FALSE;
	lexer->line = getSourceLineNumber();
	lexer->pos = getInputFilePosition();
	while (lexer->cur_c != EOF)
	{
		if (isWhitespace(lexer->cur_c))
		{
			scanWhitespace(lexer);
			have_whitespace = TRUE;
		}
		else if (lexer->cur_c == '/' && (lexer->next_c == '/' || lexer->next_c == '*'))
		{
			scanComments(lexer);
			have_whitespace = TRUE;
		}
		else
		{
			if (have_whitespace && !skip_whitspace)
				return lexer->cur_token = TOKEN_WHITESPACE;
			break;
		}
	}
	lexer->line = getSourceLineNumber();
	lexer->pos = getInputFilePosition();
	while (lexer->cur_c != EOF)
	{
		if (lexer->cur_c == '"')
		{
			scanString(lexer);
			return lexer->cur_token = TOKEN_STRING;
		}
		else if (lexer->cur_c == 'r' && (lexer->next_c == '#' || lexer->next_c == '"'))
		{
			scanRawString(lexer);
			return lexer->cur_token = TOKEN_STRING;
		}
		else if (lexer->cur_c == '\'')
		{
			scanCharacterOrLifetime(lexer);
			return lexer->cur_token = TOKEN_STRING;
		}
		else if (isIdentifierStart(lexer->cur_c))
		{
			scanIdentifier(lexer);
			return lexer->cur_token = TOKEN_IDENT;
		}
		/* These shift tokens aren't too important for tag-generation per se,
		 * but they confuse the skipUntil code which tracks the <> pairs. */
		else if (lexer->cur_c == '>' && lexer->next_c == '>')
		{
			advanceNChar(lexer, 2);
			return lexer->cur_token = TOKEN_RSHIFT;
		}
		else if (lexer->cur_c == '<' && lexer->next_c == '<')
		{
			advanceNChar(lexer, 2);
			return lexer->cur_token = TOKEN_LSHIFT;
		}
		else if (lexer->cur_c == '-' && lexer->next_c == '>')
		{
			advanceNChar(lexer, 2);
			return lexer->cur_token = TOKEN_RARROW;
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

	if (lexer->cur_c == '#' && lexer->next_c == '!')
		scanComments(lexer);
	advanceToken(lexer, TRUE);
}

static void deInitLexer (lexerState *lexer)
{
	vStringDelete(lexer->token_str);
	lexer->token_str = NULL;
}

static void addTag (vString* ident, const char* type, const char* arg_list, int kind, unsigned long line, fpos_t pos, vString *scope, int parent_kind)
{
	if (kind == K_NONE)
		return;
	tagEntryInfo tag;
	initTagEntry(&tag, ident->buffer);

	tag.lineNumber = line;
	tag.filePosition = pos;
	tag.sourceFileName = getSourceFileName();

	tag.kindName = rustKinds[kind].name;
	tag.kind = rustKinds[kind].letter;

	tag.extensionFields.signature = arg_list;
	/*tag.extensionFields.varType = type;*/ /* FIXME: map to typeRef[1]? */
	if (parent_kind != K_NONE)
	{
		tag.extensionFields.scope[0] = rustKinds[parent_kind].name;
		tag.extensionFields.scope[1] = scope->buffer;
	}
	makeTagEntry(&tag);
}

/* Skip tokens until one of the goal tokens is hit. Escapes when level = 0 if there are no goal tokens.
 * Keeps track of balanced <>'s, ()'s, []'s, and {}'s and ignores the goal tokens within those pairings */
static void skipUntil (lexerState *lexer, int goal_tokens[], int num_goal_tokens)
{
	int angle_level = 0;
	int paren_level = 0;
	int brace_level = 0;
	int bracket_level = 0;
	while (lexer->cur_token != TOKEN_EOF)
	{
		if (angle_level == 0 && paren_level == 0 && brace_level == 0
		    && bracket_level == 0)
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
				break;
		}
		switch (lexer->cur_token)
		{
			case '<':
				angle_level++;
				break;
			case '(':
				paren_level++;
				break;
			case '{':
				brace_level++;
				break;
			case '[':
				bracket_level++;
				break;
			case '>':
				angle_level--;
				break;
			case ')':
				paren_level--;
				break;
			case '}':
				brace_level--;
				break;
			case ']':
				bracket_level--;
				break;
			case TOKEN_RSHIFT:
				if (angle_level >= 2)
					angle_level -= 2;
				break;
			/* TOKEN_LSHIFT is never interpreted as two <'s in valid Rust code */
			default:
				break;
		}
		/* Has to be after the token switch to catch the case when we start with the initial level token */
		if (num_goal_tokens == 0 && angle_level == 0 && paren_level == 0 && brace_level == 0
		    && bracket_level == 0)
			break;
		advanceToken(lexer, TRUE);
	}
}

/* Function format:
 * "fn" <ident>[<type_bounds>] "(" [<args>] ")" ["->" <ret_type>] "{" [<body>] "}"*/
static void parseFn (lexerState *lexer, vString *scope, int parent_kind)
{
	int kind = (parent_kind == K_TRAIT || parent_kind == K_IMPL) ? K_METHOD : K_FN;
	vString *name;
	vString *arg_list;
	unsigned long line;
	fpos_t pos;
	int paren_level = 0;
	boolean found_paren = FALSE;
	boolean valid_signature = TRUE;

	advanceToken(lexer, TRUE);
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	name = vStringNewCopy(lexer->token_str);
	arg_list = vStringNew();

	line = lexer->line;
	pos = lexer->pos;

	advanceToken(lexer, TRUE);

	/* HACK: This is a bit coarse as far as what tag entry means by
	 * 'arglist'... */
	while (lexer->cur_token != '{' && lexer->cur_token != ';')
	{
		if (lexer->cur_token == '}')
		{
			valid_signature = FALSE;
			break;
		}
		else if (lexer->cur_token == '(')
		{
			found_paren = TRUE;
			paren_level++;
		}
		else if (lexer->cur_token == ')')
		{
			paren_level--;
			if (paren_level < 0)
			{
				valid_signature = FALSE;
				break;
			}
		}
		else if (lexer->cur_token == TOKEN_EOF)
		{
			valid_signature = FALSE;
			break;
		}
		writeCurTokenToStr(lexer, arg_list);
		advanceToken(lexer, FALSE);
	}
	if (!found_paren || paren_level != 0)
		valid_signature = FALSE;

	if (valid_signature)
	{
		vStringStripTrailing(arg_list);
		addTag(name, NULL, arg_list->buffer, kind, line, pos, scope, parent_kind);
		addToScope(scope, name);
		parseBlock(lexer, TRUE, kind, scope);
	}

	vStringDelete(name);
	vStringDelete(arg_list);
}

/* Mod format:
 * "mod" <ident> "{" [<body>] "}"
 * "mod" <ident> ";"*/
static void parseMod (lexerState *lexer, vString *scope, int parent_kind)
{
	advanceToken(lexer, TRUE);
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, NULL, K_MOD, lexer->line, lexer->pos, scope, parent_kind);
	addToScope(scope, lexer->token_str);

	advanceToken(lexer, TRUE);

	parseBlock(lexer, TRUE, K_MOD, scope);
}

/* Trait format:
 * "trait" <ident> [<type_bounds>] "{" [<body>] "}"
 */
static void parseTrait (lexerState *lexer, vString *scope, int parent_kind)
{
	int goal_tokens[] = {'{'};

	advanceToken(lexer, TRUE);
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, NULL, K_TRAIT, lexer->line, lexer->pos, scope, parent_kind);
	addToScope(scope, lexer->token_str);

	advanceToken(lexer, TRUE);

	skipUntil(lexer, goal_tokens, 1);

	parseBlock(lexer, TRUE, K_TRAIT, scope);
}

/* Skips type blocks of the form <T:T<T>, ...> */
static void skipTypeBlock (lexerState *lexer)
{
	if (lexer->cur_token == '<')
	{
		skipUntil(lexer, NULL, 0);
		advanceToken(lexer, TRUE);
	}
}

/* Essentially grabs the last ident before 'for', '<' and '{', which
 * tends to correspond to what we want as the impl tag entry name */
static void parseQualifiedType (lexerState *lexer, vString* name)
{
	while (lexer->cur_token != TOKEN_EOF)
	{
		if (lexer->cur_token == TOKEN_IDENT)
		{
			if (strcmp(lexer->token_str->buffer, "for") == 0)
				break;
			vStringClear(name);
			vStringCat(name, lexer->token_str);
		}
		else if (lexer->cur_token == '<' || lexer->cur_token == '{')
		{
			break;
		}
		advanceToken(lexer, TRUE);
	}
	skipTypeBlock(lexer);
}

/* Impl format:
 * "impl" [<type_bounds>] <qualified_ident>[<type_bounds>] ["for" <qualified_ident>[<type_bounds>]] "{" [<body>] "}"
 */
static void parseImpl (lexerState *lexer, vString *scope, int parent_kind)
{
	unsigned long line;
	fpos_t pos;
	vString *name;

	advanceToken(lexer, TRUE);

	line = lexer->line;
	pos = lexer->pos;

	skipTypeBlock(lexer);

	name = vStringNew();

	parseQualifiedType(lexer, name);

	if (lexer->cur_token == TOKEN_IDENT && strcmp(lexer->token_str->buffer, "for") == 0)
	{
		advanceToken(lexer, TRUE);
		parseQualifiedType(lexer, name);
	}

	addTag(name, NULL, NULL, K_IMPL, line, pos, scope, parent_kind);
	addToScope(scope, name);

	parseBlock(lexer, TRUE, K_IMPL, scope);

	vStringDelete(name);
}

/* Static format:
 * "static" ["mut"] <ident>
 */
static void parseStatic (lexerState *lexer, vString *scope, int parent_kind)
{
	advanceToken(lexer, TRUE);
	if (lexer->cur_token != TOKEN_IDENT)
		return;
	if (strcmp(lexer->token_str->buffer, "mut") == 0)
	{
		advanceToken(lexer, TRUE);
	}
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, NULL, K_STATIC, lexer->line, lexer->pos, scope, parent_kind);
}

/* Type format:
 * "type" <ident>
 */
static void parseType (lexerState *lexer, vString *scope, int parent_kind)
{
	advanceToken(lexer, TRUE);
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, NULL, K_TYPE, lexer->line, lexer->pos, scope, parent_kind);
}

/* Structs and enums are very similar syntax-wise.
 * It is possible to parse variants a bit more cleverly (e.g. make tuple variants functions and
 * struct variants structs) but it'd be too clever and the signature wouldn't make too much sense without
 * the enum's definition (e.g. for the type bounds)
 *
 * Struct/Enum format:
 * "struct/enum" <ident>[<type_bounds>] "{" [<ident>,]+ "}"
 * "struct/enum" <ident>[<type_bounds>] ";"
 * */
static void parseStructOrEnum (lexerState *lexer, vString *scope, int parent_kind, boolean is_struct)
{
	int kind = is_struct ? K_STRUCT : K_ENUM;
	int field_kind = is_struct ? K_FIELD : K_VARIANT;
	int goal_tokens1[] = {';', '{'};

	advanceToken(lexer, TRUE);
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, NULL, kind, lexer->line, lexer->pos, scope, parent_kind);
	addToScope(scope, lexer->token_str);

	skipUntil(lexer, goal_tokens1, 2);

	if (lexer->cur_token == '{')
	{
		vString *field_name = vStringNew();
		while (lexer->cur_token != TOKEN_EOF)
		{
			int goal_tokens2[] = {'}', ','};
			/* Skip attributes. Format:
			 * #[..] or #![..]
			 * */
			if (lexer->cur_token == '#')
			{
				advanceToken(lexer, TRUE);
				if (lexer->cur_token == '!')
					advanceToken(lexer, TRUE);
				if (lexer->cur_token == '[')
				{
					/* It's an attribute, skip it. */
					skipUntil(lexer, NULL, 0);
				}
				else
				{
					/* Something's up with this field, skip to the next one */
					skipUntil(lexer, goal_tokens2, 2);
					continue;
				}
			}
			if (lexer->cur_token == TOKEN_IDENT)
			{
				if (strcmp(lexer->token_str->buffer, "priv") == 0
				    || strcmp(lexer->token_str->buffer, "pub") == 0)
				{
					advanceToken(lexer, TRUE);
					if (lexer->cur_token != TOKEN_IDENT)
					{
						/* Something's up with this field, skip to the next one */
						skipUntil(lexer, goal_tokens2, 2);
						continue;
					}
				}

				vStringClear(field_name);
				vStringCat(field_name, lexer->token_str);
				addTag(field_name, NULL, NULL, field_kind, lexer->line, lexer->pos, scope, kind);
				skipUntil(lexer, goal_tokens2, 2);
			}
			if (lexer->cur_token == '}')
			{
				advanceToken(lexer, TRUE);
				break;
			}
			advanceToken(lexer, TRUE);
		}
		vStringDelete(field_name);
	}
}

/* Skip the body of the macro. Can't use skipUntil here as
 * the body of the macro may have arbitrary code which confuses it (e.g.
 * bitshift operators/function return arrows) */
static void skipMacro (lexerState *lexer)
{
	int level = 0;
	int plus_token = 0;
	int minus_token = 0;

	advanceToken(lexer, TRUE);
	switch (lexer->cur_token)
	{
		case '(':
			plus_token = '(';
			minus_token = ')';
			break;
		case '{':
			plus_token = '{';
			minus_token = '}';
			break;
		case '[':
			plus_token = '[';
			minus_token = ']';
			break;
		default:
			return;
	}

	while (lexer->cur_token != TOKEN_EOF)
	{
		if (lexer->cur_token == plus_token)
			level++;
		else if (lexer->cur_token == minus_token)
			level--;
		if (level == 0)
			break;
		advanceToken(lexer, TRUE);
	}
	advanceToken(lexer, TRUE);
}

/*
 * Macro rules format:
 * "macro_rules" "!" <ident> <macro_body>
 */
static void parseMacroRules (lexerState *lexer, vString *scope, int parent_kind)
{
	advanceToken(lexer, TRUE);

	if (lexer->cur_token != '!')
		return;

	advanceToken(lexer, TRUE);

	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, NULL, K_MACRO, lexer->line, lexer->pos, scope, parent_kind);

	skipMacro(lexer);
}

/*
 * Rust is very liberal with nesting, so this function is used pretty much for any block
 */
static void parseBlock (lexerState *lexer, boolean delim, int kind, vString *scope)
{
	int level = 1;
	if (delim)
	{
		if (lexer->cur_token != '{')
			return;
		advanceToken(lexer, TRUE);
	}
	while (lexer->cur_token != TOKEN_EOF)
	{
		if (lexer->cur_token == TOKEN_IDENT)
		{
			size_t old_scope_len = vStringLength(scope);
			if (strcmp(lexer->token_str->buffer, "fn") == 0)
			{
				parseFn(lexer, scope, kind);
			}
			else if(strcmp(lexer->token_str->buffer, "mod") == 0)
			{
				parseMod(lexer, scope, kind);
			}
			else if(strcmp(lexer->token_str->buffer, "static") == 0)
			{
				parseStatic(lexer, scope, kind);
			}
			else if(strcmp(lexer->token_str->buffer, "trait") == 0)
			{
				parseTrait(lexer, scope, kind);
			}
			else if(strcmp(lexer->token_str->buffer, "type") == 0)
			{
				parseType(lexer, scope, kind);
			}
			else if(strcmp(lexer->token_str->buffer, "impl") == 0)
			{
				parseImpl(lexer, scope, kind);
			}
			else if(strcmp(lexer->token_str->buffer, "struct") == 0)
			{
				parseStructOrEnum(lexer, scope, kind, TRUE);
			}
			else if(strcmp(lexer->token_str->buffer, "enum") == 0)
			{
				parseStructOrEnum(lexer, scope, kind, FALSE);
			}
			else if(strcmp(lexer->token_str->buffer, "macro_rules") == 0)
			{
				parseMacroRules(lexer, scope, kind);
			}
			else
			{
				advanceToken(lexer, TRUE);
				if (lexer->cur_token == '!')
				{
					skipMacro(lexer);
				}
			}
			resetScope(scope, old_scope_len);
		}
		else if (lexer->cur_token == '{')
		{
			level++;
			advanceToken(lexer, TRUE);
		}
		else if (lexer->cur_token == '}')
		{
			level--;
			advanceToken(lexer, TRUE);
		}
		else if (lexer->cur_token == '\'')
		{
			/* Skip over the 'static lifetime, as it confuses the static parser above */
			advanceToken(lexer, TRUE);
			if (lexer->cur_token == TOKEN_IDENT && strcmp(lexer->token_str->buffer, "static") == 0)
				advanceToken(lexer, TRUE);
		}
		else
		{
			advanceToken(lexer, TRUE);
		}
		if (delim && level <= 0)
			break;
	}
}

static void findRustTags (void)
{
	lexerState lexer;
	vString* scope = vStringNew();
	initLexer(&lexer);

	parseBlock(&lexer, FALSE, K_NONE, scope);
	vStringDelete(scope);

	deInitLexer(&lexer);
}

extern parserDefinition *RustParser (void)
{
	static const char *const extensions[] = { "rs", NULL };
	parserDefinition *def = parserNew ("Rust");
	def->kinds = rustKinds;
	def->kindCount = KIND_COUNT (rustKinds);
	def->extensions = extensions;
	def->parser = findRustTags;

	return def;
}
