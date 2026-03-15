/*
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Rust files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "keyword.h"
#include "parse.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#include "trace.h"

/*
*   MACROS
*/
#define MAX_STRING_LENGTH 256

#define tokenLitEq(LEXER,CSTRLIT) (vStringEqC((LEXER)->token_str,CSTRLIT))

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
	K_CONST,
	K_NONE
} RustKind;

typedef enum {
	F_MACRO,
} RustField;

static kindDefinition rustKinds[] = {
	{true, 'n', "module", "module"},
	{true, 's', "struct", "structural type"},
	{true, 'i', "interface", "trait interface"},
	{true, 'c', "implementation", "implementation"},
	{true, 'f', "function", "Function"},
	{true, 'g', "enum", "Enum"},
	{true, 't', "typedef", "Type Alias"},
	{true, 'v', "variable", "Global variable"},
	{true, 'M', "macro", "Macro Definition"},
	{true, 'm', "field", "A struct field"},
	{true, 'e', "enumerator", "An enum variant"},
	{true, 'P', "method", "A method"},
	{true, 'C', "constant", "A constant"},
};

static fieldDefinition rustFields[] = {
	{
		.name = "macro",
		.description = "macro invocation containing the definition",
		.enabled = true,
		.version = 1,
	},
};

typedef enum eTokenType {
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
	MIOPos pos;

	const char *macro;
} lexerState;

/*
*   FUNCTION PROTOTYPES
*/

static void parseBlock (lexerState *lexer, bool delim, int kind, vString *scope,
						bool as_token_tree);

/*
*   FUNCTION DEFINITIONS
*/
#include "d-rust.h"

/* Resets the scope string to the old length */
static void resetScope (vString *scope, size_t old_len)
{
	vStringTruncate (scope, old_len);
}

/* Adds a name to the end of the scope string */
static void addToScope (vString *scope, vString *name)
{
	if (vStringLength(scope) > 0)
		vStringCatS(scope, "::");
	vStringCat(scope, name);
}

static const char* swapMacroInvocation (lexerState *lexer, const char *macro)
{
	const char *tmp = lexer->macro;
	lexer->macro = macro;
	return tmp;
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
			vStringPut(out_str, lexer->cur_token);
	}
}

/* Reads a character from the file */
static void advanceChar (lexerState *lexer)
{
	lexer->cur_c = lexer->next_c;
	lexer->next_c = getcFromInputFile();
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
		vStringPut(lexer->token_str, lexer->cur_c);
	advanceChar(lexer);
}

static bool isWhitespace (int c)
{
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

static bool isAscii (int c)
{
	return (c >= 0) && (c < 0x80);
}

/* This isn't quite right for Unicode identifiers */
static bool isIdentifierStart (int c)
{
	return (isAscii(c) && (isalpha(c) || c == '_')) || !isAscii(c);
}

/* This isn't quite right for Unicode identifiers */
static bool isIdentifierContinue (int c)
{
	return (isAscii(c) && (isalnum(c) || c == '_')) || !isAscii(c);
}

static void scanWhitespace (lexerState *lexer)
{
	while (isWhitespace(lexer->cur_c))
		advanceChar(lexer);
}

/* Normal line comments start with two /'s and continue until the next \n
 * (potentially after a \r). Additionally, a shebang in the beginning of the
 * file also counts as a line comment as long as it is not this sequence: #![ .
 * Block comments start with / followed by a * and end with a * followed by a /.
 * Unlike in C/C++ they nest. */
static void scanComments (lexerState *lexer)
{
	/* // */
	if (lexer->next_c == '/')
	{
		advanceNChar(lexer, 2);
		while (lexer->cur_c != EOF && lexer->cur_c != '\n')
			advanceChar(lexer);
	}
	/* #! */
	else if (lexer->next_c == '!')
	{
		advanceNChar(lexer, 2);
		/* If it is exactly #![ then it is not a comment, but an attribute */
		if (lexer->cur_c == '[')
			return;
		while (lexer->cur_c != EOF && lexer->cur_c != '\n')
			advanceChar(lexer);
	}
	/* block comment */
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
	if (lexer->cur_c != EOF)
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
static int advanceToken0 (lexerState *lexer, bool skip_whitspace)
{
	bool have_whitespace = false;
	lexer->line = getInputLineNumber();
	lexer->pos = getInputFilePosition();
	while (lexer->cur_c != EOF)
	{
		if (isWhitespace(lexer->cur_c))
		{
			scanWhitespace(lexer);
			have_whitespace = true;
		}
		else if (lexer->cur_c == '/' && (lexer->next_c == '/' || lexer->next_c == '*'))
		{
			scanComments(lexer);
			have_whitespace = true;
		}
		else
		{
			if (have_whitespace && !skip_whitspace)
				return lexer->cur_token = TOKEN_WHITESPACE;
			break;
		}
	}
	lexer->line = getInputLineNumber();
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

static int advanceToken (lexerState *lexer, bool skip_whitspace)
{
	int r = advanceToken0(lexer, skip_whitspace);
	TRACE_PRINT("token: %s", decodeTokenType(r));
	return r;
}

static void initLexer (lexerState *lexer)
{
	lexer->macro = NULL;
	advanceNChar(lexer, 2);
	lexer->token_str = vStringNew();

	if (lexer->cur_c == '#' && lexer->next_c == '!')
		scanComments(lexer);
	advanceToken(lexer, true);
}

static void deInitLexer (lexerState *lexer)
{
	vStringDelete(lexer->token_str);
	lexer->token_str = NULL;
}

static void addTag (vString* ident, const char* arg_list, int kind, unsigned long line, MIOPos pos, vString *scope, int parent_kind,
					const char* macro)
{
	TRACE_PRINT("tag: %s", vStringValue (ident));

	if (kind == K_NONE || ! rustKinds[kind].enabled)
		return;
	tagEntryInfo tag;
	initTagEntry(&tag, vStringValue(ident), kind);

	updateTagLine (&tag, line, pos);

	tag.extensionFields.signature = arg_list;
	/*tag.extensionFields.varType = type;*/ /* FIXME: map to typeRef[1]? */
	if (parent_kind != K_NONE)
	{
		tag.extensionFields.scopeKindIndex = parent_kind;
		tag.extensionFields.scopeName = vStringValue(scope);
	}

	if (macro)
		attachParserField (&tag, rustFields[F_MACRO].ftype, macro);

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
		advanceToken(lexer, true);
	}
}

/* Function format:
 * "fn" <ident>[<type_bounds>] "(" [<args>] ")" ["->" <ret_type>] "{" [<body>] "}"*/
static void parseFn (lexerState *lexer, vString *scope, int parent_kind)
{
	TRACE_ENTER();

	int kind = (parent_kind == K_TRAIT || parent_kind == K_IMPL) ? K_METHOD : K_FN;
	vString *name;
	vString *arg_list;
	unsigned long line;
	MIOPos pos;
	int paren_level = 0;
	int bracket_level = 0;
	bool found_paren = false;
	bool valid_signature = true;

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier");
		return;
	}

	name = vStringNewCopy(lexer->token_str);
	arg_list = vStringNew();

	line = lexer->line;
	pos = lexer->pos;

	advanceToken(lexer, true);

	/* HACK: This is a bit coarse as far as what tag entry means by
	 * 'arglist'... */
	while (lexer->cur_token != '{')
	{
		if (lexer->cur_token == ';' && bracket_level == 0)
		{
			break;
		}
		else if (lexer->cur_token == '}')
		{
			valid_signature = false;
			break;
		}
		else if (lexer->cur_token == '(')
		{
			found_paren = true;
			paren_level++;
		}
		else if (lexer->cur_token == ')')
		{
			paren_level--;
			if (paren_level < 0)
			{
				valid_signature = false;
				break;
			}
		}
		else if (lexer->cur_token == '[')
		{
			bracket_level++;
		}
		else if (lexer->cur_token == ']')
		{
			bracket_level--;
		}
		else if (lexer->cur_token == TOKEN_EOF)
		{
			valid_signature = false;
			break;
		}
		writeCurTokenToStr(lexer, arg_list);
		advanceToken(lexer, false);
	}
	if (!found_paren || paren_level != 0 || bracket_level != 0)
		valid_signature = false;

	if (valid_signature)
	{
		vStringStripTrailing(arg_list);
		addTag(name, vStringValue(arg_list), kind, line, pos, scope, parent_kind,
			   lexer->macro);
		addToScope(scope, name);
		parseBlock(lexer, true, kind, scope, false);
	}

	vStringDelete(name);
	vStringDelete(arg_list);

	TRACE_LEAVE();
}

/* Mod format:
 * "mod" <ident> "{" [<body>] "}"
 * "mod" <ident> ";"*/
static void parseMod (lexerState *lexer, vString *scope, int parent_kind)
{
	TRACE_ENTER();

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier");
		return;
	}

	addTag(lexer->token_str, NULL, K_MOD, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);
	addToScope(scope, lexer->token_str);

	advanceToken(lexer, true);

	parseBlock(lexer, true, K_MOD, scope, false);

	TRACE_LEAVE();
}

/* Trait format:
 * "trait" <ident> [<type_bounds>] "{" [<body>] "}"
 */
static void parseTrait (lexerState *lexer, vString *scope, int parent_kind)
{
	int goal_tokens[] = {'{'};

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
		return;

	addTag(lexer->token_str, NULL, K_TRAIT, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);
	addToScope(scope, lexer->token_str);

	advanceToken(lexer, true);

	skipUntil(lexer, goal_tokens, 1);

	parseBlock(lexer, true, K_TRAIT, scope, false);
}

/* Skips type blocks of the form <T:T<T>, ...> */
static void skipTypeBlock (lexerState *lexer)
{
	if (lexer->cur_token == '<')
	{
		skipUntil(lexer, NULL, 0);
		advanceToken(lexer, true);
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
			if (tokenLitEq (lexer, "for")
				|| tokenLitEq (lexer, "where"))
				break;
			vStringClear(name);
			vStringCat(name, lexer->token_str);
		}
		else if (lexer->cur_token == '<' || lexer->cur_token == '{')
		{
			break;
		}
		advanceToken(lexer, true);
	}
	skipTypeBlock(lexer);
}

/* Impl format:
 * "impl" [<type_bounds>] <qualified_ident>[<type_bounds>] ["for" <qualified_ident>[<type_bounds>]] "{" [<body>] "}"
 */
static void parseImpl (lexerState *lexer, vString *scope, int parent_kind)
{
	unsigned long line;
	MIOPos pos;
	vString *name;

	advanceToken(lexer, true);

	line = lexer->line;
	pos = lexer->pos;

	skipTypeBlock(lexer);

	name = vStringNew();

	parseQualifiedType(lexer, name);

	if (lexer->cur_token == TOKEN_IDENT && tokenLitEq (lexer, "for"))
	{
		advanceToken(lexer, true);
		parseQualifiedType(lexer, name);
	}

	addTag(name, NULL, K_IMPL, line, pos, scope, parent_kind,
		   lexer->macro);
	addToScope(scope, name);

	parseBlock(lexer, true, K_IMPL, scope, false);

	vStringDelete(name);
}

/* Static format:
 * "static" ["mut"] <ident>
 */
static void parseStatic (lexerState *lexer, vString *scope, int parent_kind)
{
	TRACE_ENTER();

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier (0)");
		return;
	}
	if (tokenLitEq (lexer, "mut"))
	{
		advanceToken(lexer, true);
	}
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier (1)");
		return;
	}

	addTag(lexer->token_str, NULL, K_STATIC, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);

	TRACE_LEAVE();
}

/* Const format:
 * "const" <ident>
 */
static void parseConst (lexerState *lexer, vString *scope, int parent_kind)
{
	TRACE_ENTER();

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier");
		return;
	}
	if (tokenLitEq (lexer, "fn"))
	{
		TRACE_LEAVE_TEXT("fn follows");
		return;
	}

	addTag(lexer->token_str, NULL, K_CONST, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);

	TRACE_LEAVE();
}

/* Type format:
 * "type" <ident>
 */
static void parseType (lexerState *lexer, vString *scope, int parent_kind)
{
	TRACE_ENTER();

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier");
		return;
	}

	addTag(lexer->token_str, NULL, K_TYPE, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);

	TRACE_LEAVE();
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
static void parseStructOrEnum (lexerState *lexer, vString *scope, int parent_kind, bool is_struct)
{
	TRACE_ENTER();

	int kind = is_struct ? K_STRUCT : K_ENUM;
	int field_kind = is_struct ? K_FIELD : K_VARIANT;
	int goal_tokens1[] = {';', '{'};

	advanceToken(lexer, true);
	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier");
		return;
	}

	addTag(lexer->token_str, NULL, kind, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);
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
				advanceToken(lexer, true);
				if (lexer->cur_token == '!')
					advanceToken(lexer, true);
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
				if (tokenLitEq (lexer, "priv")
				    || tokenLitEq (lexer, "pub"))
				{
					advanceToken(lexer, true);

					/* Skip thevisibility specificaions.
					 * https://doc.rust-lang.org/reference/visibility-and-privacy.html */
					if (lexer->cur_token == '(')
					{
						advanceToken(lexer, true);
						skipUntil (lexer, (int []){')'}, 1);
						advanceToken(lexer, true);
					}

					if (lexer->cur_token != TOKEN_IDENT)
					{
						/* Something's up with this field, skip to the next one */
						skipUntil(lexer, goal_tokens2, 2);
						continue;
					}
				}

				vStringClear(field_name);
				vStringCat(field_name, lexer->token_str);
				addTag(field_name, NULL, field_kind, lexer->line, lexer->pos, scope, kind,
					   lexer->macro);
				skipUntil(lexer, goal_tokens2, 2);
			}
			if (lexer->cur_token == '}')
			{
				advanceToken(lexer, true);
				break;
			}
			advanceToken(lexer, true);
		}
		vStringDelete(field_name);
	}

	TRACE_LEAVE();
}

/* Skip the body of the macro. Can't use skipUntil here as
 * the body of the macro may have arbitrary code which confuses it (e.g.
 * bitshift operators/function return arrows) */
static void skipMacro (lexerState *lexer)
{
	TRACE_ENTER();

	int level = 0;
	int plus_token = 0;
	int minus_token = 0;

	advanceToken(lexer, true);
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
			TRACE_LEAVE_TEXT("unexpected cur_token: %d", lexer->cur_token);
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
		advanceToken(lexer, true);
	}
	advanceToken(lexer, true);

	TRACE_LEAVE();
}

/*
 * Macro rules format:
 * "macro_rules" "!" <ident> <macro_body>
 */
static void parseMacroRules (lexerState *lexer, vString *scope, int parent_kind)
{
	TRACE_ENTER();

	advanceToken(lexer, true);

	if (lexer->cur_token != '!')
	{
		TRACE_LEAVE_TEXT("no bang '!'");
		return;
	}

	advanceToken(lexer, true);

	if (lexer->cur_token != TOKEN_IDENT)
	{
		TRACE_LEAVE_TEXT("no identifier");
		return;
	}

	addTag(lexer->token_str, NULL, K_MACRO, lexer->line, lexer->pos, scope, parent_kind,
		   lexer->macro);

	skipMacro(lexer);

	TRACE_LEAVE();
}

/*
 * Rust is very liberal with nesting, so this function is used pretty much for any block
 */
static void parseBlock (lexerState *lexer, bool delim, int kind, vString *scope,
						bool as_token_tree)
{
	TRACE_ENTER();

	int level = 1;
	if (delim)
	{
		if (as_token_tree)
		{
			if (lexer->cur_token != '{'
				&& lexer->cur_token != '('
				&& lexer->cur_token != '[')
			{
				TRACE_LEAVE_TEXT("no opening characters '{[('");
				return;
			}
		}
		else if (lexer->cur_token != '{')
		{
			TRACE_LEAVE_TEXT("no opening curly bracket '{'");
			return;
		}
		advanceToken(lexer, true);
	}
	while (lexer->cur_token != TOKEN_EOF)
	{
		if (lexer->cur_token == TOKEN_IDENT)
		{
			size_t old_scope_len = vStringLength(scope);
			if (tokenLitEq (lexer, "fn"))
			{
				parseFn(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "mod"))
			{
				parseMod(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "static"))
			{
				parseStatic(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "const"))
			{
				parseConst(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "trait"))
			{
				parseTrait(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "type"))
			{
				parseType(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "impl"))
			{
				parseImpl(lexer, scope, kind);
			}
			else if (tokenLitEq (lexer, "struct"))
			{
				parseStructOrEnum(lexer, scope, kind, true);
			}
			else if (tokenLitEq (lexer, "enum"))
			{
				parseStructOrEnum(lexer, scope, kind, false);
			}
			else if (tokenLitEq (lexer, "macro_rules"))
			{
				parseMacroRules(lexer, scope, kind);
			}
			else
			{
				vString* macro = vStringNewCopy (lexer->token_str);

				advanceToken(lexer, true);
				if (lexer->cur_token == '!')
				{
					const char *last_macro = swapMacroInvocation (lexer,
																  vStringValue (macro));
					advanceToken(lexer, true);
					parseBlock(lexer, true, kind, scope, true);
					swapMacroInvocation (lexer, last_macro);
				}
				vStringDelete (macro);
			}
			resetScope(scope, old_scope_len);
		}
		else if (lexer->cur_token == '{'
				 || (as_token_tree
					 && (lexer->cur_token == '('
						 || lexer->cur_token == '[')))
		{
			level++;
			advanceToken(lexer, true);
		}
		else if (lexer->cur_token == '}'
				 || (as_token_tree
					 && (lexer->cur_token == ')'
						 || lexer->cur_token == ']')))
		{
			level--;
			advanceToken(lexer, true);
		}
		else if (lexer->cur_token == '\'')
		{
			/* Skip over the 'static lifetime, as it confuses the static parser above */
			advanceToken(lexer, true);
			if (lexer->cur_token == TOKEN_IDENT && tokenLitEq (lexer, "static"))
				advanceToken(lexer, true);
		}
		else
		{
			advanceToken(lexer, true);
		}
		if (delim && level <= 0)
			break;
	}

	TRACE_LEAVE();
}

static void findRustTags (void)
{
	TRACE_ENTER();

	lexerState lexer = {0};
	vString* scope = vStringNew();
	initLexer(&lexer);

	parseBlock(&lexer, false, K_NONE, scope, false);
	vStringDelete(scope);

	deInitLexer(&lexer);

	TRACE_LEAVE();
}

extern parserDefinition *RustParser (void)
{
	static const char *const extensions[] = { "rs", NULL };
	parserDefinition *def = parserNew ("Rust");
	def->kindTable = rustKinds;
	def->kindCount = ARRAY_SIZE (rustKinds);
	def->extensions = extensions;
	def->parser = findRustTags;
	def->fieldTable = rustFields;
	def->fieldCount = ARRAY_SIZE (rustFields);

	def->versionCurrent = 1;
	def->versionAge = 1;

	return def;
}
