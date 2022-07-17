/*
 *	 Copyright (c) 2003, Darren Hiebert
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating tags for JavaScript language
 *	 files.
 *
 *	 Reference: http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf
 *
 *	 This is a good reference for different forms of the function statement:
 *		 http://www.permadi.com/tutorial/jsFunc/
 *   Another good reference:
 *       http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include <ctype.h>	/* to define isalpha () */
#ifdef DEBUG
#include <stdio.h>
#endif

#ifdef HAVE_ICONV
#include <iconv.h>
#include <errno.h>
#	ifdef WORDS_BIGENDIAN
#		define INTERNAL_ENCODING "UTF-32BE"
#	else
#		define INTERNAL_ENCODING "UTF-32LE"
#	endif /* WORDS_BIGENDIAN */
#endif

#include <string.h>
#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "numarray.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "objpool.h"
#include "options.h"
#include "mbcs.h"
#include "trace.h"

/*
 *	 MACROS
 */
#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '$' || \
		(c) == '@' || (c) == '_' || (c) == '#' || \
		(c) >= 0x80)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))

/*
 *	 DATA DECLARATIONS
 */

/*	Used to specify type of keyword.
*/
enum eKeywordId {
	KEYWORD_function,
	KEYWORD_capital_function,
	KEYWORD_capital_object,
	KEYWORD_prototype,
	KEYWORD_var,
	KEYWORD_let,
	KEYWORD_const,
	KEYWORD_new,
	KEYWORD_this,
	KEYWORD_for,
	KEYWORD_while,
	KEYWORD_do,
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_switch,
	KEYWORD_try,
	KEYWORD_catch,
	KEYWORD_finally,
	KEYWORD_sap,
	KEYWORD_return,
	KEYWORD_class,
	KEYWORD_extends,
	KEYWORD_static,
	KEYWORD_default,
	KEYWORD_export,
	KEYWORD_async,
	KEYWORD_get,
	KEYWORD_set,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_CHARACTER,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_TEMPLATE_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_EQUAL_SIGN,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_REGEXP,
	TOKEN_POSTFIX_OPERATOR,
	TOKEN_STAR,
	/* To handle Babel's decorators.
	 * Used only in readTokenFull or lower functions. */
	TOKEN_ATMARK,
	TOKEN_BINARY_OPERATOR,
	TOKEN_ARROW,
	TOKEN_DOTS,					/* ... */
} tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	int				scope;
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
	int				nestLevel;
	bool			dynamicProp;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static tokenType LastTokenType;
static tokenInfo *NextToken;

static langType Lang_js;

static objPool *TokenPool = NULL;

#ifdef HAVE_ICONV
static iconv_t JSUnicodeConverter = (iconv_t) -2;
#endif

typedef enum {
	JSTAG_FUNCTION,
	JSTAG_CLASS,
	JSTAG_METHOD,
	JSTAG_PROPERTY,
	JSTAG_CONSTANT,
	JSTAG_VARIABLE,
	JSTAG_GENERATOR,
	JSTAG_GETTER,
	JSTAG_SETTER,
	JSTAG_FIELD,
	JSTAG_COUNT
} jsKind;

/*
 * "chain element" role is introduced when adapting the JavaScript parser
 * to corkAPI.
 *
 * In the corkAPI, a cork index returned from makeTagEntry() can
 * represent a scope of another tag. Let's think about `input-0.js' that
 * the node command accepts as an input for ctags.
 *
 +---+ input-0.js ------------------------------------------------------
 | 1 | class A {
 | 2 |    f = function(x) {
 | 3 |       return x
 | 4 |    }
 | 5 | }
 +---+------------------------------------------------------------------
 *
 * The following pseudo C code illustrate the code for
 * tagging `A' and `f' in input-0.js:
 +---+------------------------------------------------------------------
 |   |...
 |   | tagEntryFor e_for_A, e_for_f;
 |   | ...
 |   | int index_for_A = makeTagEntry (&e_for_A);
 |   | ...
 |>>>| e_for_f.extensionFields.scopeIndex = index_for_A;
 |   | ...
 |   | makeTagEntry (&e_for_f);
 |   | ...
 +---+------------------------------------------------------------------
 *
 * `index_for_A' represents "A" in "class A".
 * `f' is defined in `A'. To fill the scope field of the tag for `f',
 * `scopeIndex' member of the tag is filled with `index_for_A' at line |>>>|.
 *
 * If `A' is defined in the input source file, this technique based on
 * the cork API works fine. However, if `A' is not defined in the input
 * source file, the technique doesn't work well.
 +---+ input-1.js -------------------------------------------------------
 | 1 | import {A} from 'input-0.js';
 | 2 | A.g = function(x) {
 | 3 |    return x
 | 4 | }
 +---+------------------------------------------------------------------
 *
 * In this case, `A' may be defined in input-0.js.
 * The current implementation of ctags processes file by file; it doesn't
 * use the knowledge in other input source files than current input source
 * file. ctags processing input-1.js doesn't know the cork index for `A'.
 *
 * When tagging `g' with "function" kind, how can we fill the scope field
 * of the tag for `g'?
 *
 * Here the "chain element" role comes.
 * This role is used for tagging `z' in "x.y.z" in the case when ctags
 * doesn't see the definitions for `x' and `y'.
 * The JavaScript parser makes reference tags for `x' and `'y' with
 * "chain element" role. makeTagEntry() returns a cork index regardless the
 * type of tags (definition or reference).
 * The index for the reference tag for `y' can be used to fill the scope
 * field of the tag for `z'. The index for `x' can be used to fill the
 * field for `y'.
 *
 * With these trick and technique, the scope field for `g' is filled:
 +---+ tags for input-1.js ---------------------------------------------
 | 1 | A	input-1.js	/^A.g = function(x) {$/;"	f	roles:chainElt	extras:reference
 | 2 | g	input-1.js	/^A.g = function(x) {$/;"	f	scope:function:A	signature:(x)	roles:def
 +---+------------------------------------------------------------------
 *
 * By default, reference tags are not emitted. So non-ctags-expert users may
 * not see the tag entry for `A'.
 *
 * makeJsRefTagsForNameChain() and makeJsTagCommon() implement the trick
 * and technique.
 *
 * Arguable points:
 *
 * Is "chain element(chainElt)" suitable name for people familier with JavaScript?
 *
 * Kinds assigned to the tag having chainElt role must revised. Eventually
 * we may need to introduce "unknown" kind like the Python parser. Assigning
 * "function" kind to `A' in input-1.js is obviously wrong.
 */

typedef enum {
	JS_VARIABLE_CHAINELT,
} jsVariableRole;

typedef enum {
	JS_CLASS_CHAINELT,
} jsClassRole;

static roleDefinition JsVariableRoles [] = {
	{ false, "chainElt", "(EXPERIMENTAL)used as an element in a name chain like a.b.c" },
};

static roleDefinition JsClassRoles [] = {
	{ false, "chainElt", "(EXPERIMENTAL)used as an element in a name chain like a.b.c" },
};

static kindDefinition JsKinds [] = {
	{ true,  'f', "function",	  "functions"        },
	{ true,  'c', "class",		  "classes",
	  .referenceOnly = false, ATTACH_ROLES(JsClassRoles)    },
	{ true,  'm', "method",		  "methods"          },
	{ true,  'p', "property",	  "properties"       },
	{ true,  'C', "constant",	  "constants"        },
	{ true,  'v', "variable",	  "global variables",
	  .referenceOnly = false, ATTACH_ROLES(JsVariableRoles) },
	{ true,  'g', "generator",	  "generators"       },
	{ true,  'G', "getter",		  "getters"          },
	{ true,  'S', "setter",		  "setters"          },
	{ true,  'M', "field",		  "fields"           },
};

static const keywordTable JsKeywordTable [] = {
	/* keyword		keyword ID */
	{ "function",	KEYWORD_function			},
	{ "Function",	KEYWORD_capital_function	},
	{ "Object",		KEYWORD_capital_object		},
	{ "prototype",	KEYWORD_prototype			},
	{ "var",		KEYWORD_var					},
	{ "let",		KEYWORD_let					},
	{ "const",		KEYWORD_const				},
	{ "new",		KEYWORD_new					},
	{ "this",		KEYWORD_this				},
	{ "for",		KEYWORD_for					},
	{ "while",		KEYWORD_while				},
	{ "do",			KEYWORD_do					},
	{ "if",			KEYWORD_if					},
	{ "else",		KEYWORD_else				},
	{ "switch",		KEYWORD_switch				},
	{ "try",		KEYWORD_try					},
	{ "catch",		KEYWORD_catch				},
	{ "finally",	KEYWORD_finally				},
	{ "sap",	    KEYWORD_sap    				},
	{ "return",		KEYWORD_return				},
	{ "class",		KEYWORD_class				},
	{ "extends",	KEYWORD_extends				},
	{ "static",		KEYWORD_static				},
	{ "default",	KEYWORD_default				},
	{ "export",		KEYWORD_export				},
	{ "async",		KEYWORD_async				},
	{ "get",		KEYWORD_get					},
	{ "set",		KEYWORD_set					},
};

/*
 *	 FUNCTION DEFINITIONS
 */

/* Recursive functions */
static void readTokenFull (tokenInfo *const token, bool include_newlines, vString *const repr);
static void skipArgumentList (tokenInfo *const token, bool include_newlines, vString *const repr);
static void parseFunction (tokenInfo *const token);
static bool parseBlock (tokenInfo *const token, int parentScope);
static bool parseMethods (tokenInfo *const token, int classIndex, const bool is_es6_class);
static bool parseLine (tokenInfo *const token, bool is_inside_class);
static void parseUI5 (tokenInfo *const token);

#ifdef DO_TRACING
static const char *tokenTypeName(enum eTokenType e);
static const char* getNameStringForCorkIndex(int index);
static const char* getKindStringForCorkIndex(int index);
static const char *kindName(jsKind kind);
// #define DO_TRACING_USE_DUMP_TOKEN
#ifdef DO_TRACING_USE_DUMP_TOKEN
static void dumpToken (const tokenInfo *const token);
#endif
#endif

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);

	token->string		= vStringNew ();
	token->scope		= CORK_NIL;

	return token;
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->nestLevel	= 0;
	token->dynamicProp  = false;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
	token->scope 		= CORK_NIL;
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	vStringDelete (token->string);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src,
                       bool const include_non_read_info)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	dest->dynamicProp = src->dynamicProp;
	vStringCopy(dest->string, src->string);
	if (include_non_read_info)
	{
		dest->nestLevel = src->nestLevel;
		dest->scope = src->scope;
	}
}

static void injectDynamicName (tokenInfo *const token, vString *newName)
{
	token->dynamicProp = true;
	vStringDelete (token->string);
	token->string = newName;
}

/*
 *	 Tag generation functions
 */

struct  bestJSEntryInScopeData {
	int index;
};

static bool findBestJSEntry (int corkIndex, tagEntryInfo *entry, void *cbData)
{
	struct  bestJSEntryInScopeData *data = cbData;

	if (isRoleAssigned (entry, ROLE_DEFINITION_INDEX))
	{
		data->index = corkIndex;
		return false;
	}

	if (data->index == CORK_NIL || data->index > corkIndex)
		data->index = corkIndex;
	return true;
}

static int bestJSEntryInScope(int scope, const char *name)
{
	/* If the SCOPE has a tag entry having NAME, the tag is the best
	 * even if there are reference tag entries having NAME.
	 * If the scope has only reference tag entries having NAME, the
	 * tag having smallest cork index is the best.
	 */

	struct  bestJSEntryInScopeData data = {
		.index = CORK_NIL,
	};
	foreachEntriesInScope (scope, name,  findBestJSEntry, &data);
	return data.index;
}

static int makeJsRefTagsForNameChain (char *name_chain, const tokenInfo *token, int leaf_kind, int scope)
{
	/* To fill the scope field of "d" of "a.b.c.d",
	 * "c" must be tagged if the cork API is used.
	 * ----------------------------------------------------------
	 * How the fields for "a", "b", and "c" are filled.
	 *   a  kind:class	scope:<given by SCOPE> roles:chainElt
	 *   b  kind:class	scope:class:a	roles:chainElt
	 *
	 *   The fields of c depends on LEAF_KIND that is passed to this functions.
	 *
	 *   if (LEAF_KIND == FUNCTION)
	 *       c  kind:function	scope:class:b	roles:chainElt
	 *   else
	 *       c  kind:class	scope:class:b	roles:chainElt
	 */

	const char *name = name_chain;
	char *next = strchr(name_chain, '.');
	if (next)
		*next = '\0';
	int index = bestJSEntryInScope (scope, name);

	if (index == CORK_NIL)
	{
		tagEntryInfo e;
		int kind = JSTAG_CLASS;
		int role = JS_CLASS_CHAINELT;
		if (next == NULL && leaf_kind == JSTAG_FUNCTION)
		{
			/*
			 * If we're creating a function (and not a method),
			 * assume the parent is a plain variable.
			 */
			kind = JSTAG_VARIABLE;
			role = JS_VARIABLE_CHAINELT;
		}

		initRefTagEntry (&e, name, kind, role);
		e.lineNumber = token->lineNumber;
		e.filePosition = token->filePosition;
		e.extensionFields.scopeIndex = scope;

		index = makeTagEntry (&e);
		/* We shold remove This condition. We should fix the callers passing
		 * an empty name instead. makeTagEntry() returns CORK_NIL if the tag
		 * name is empty. */
		if (index != CORK_NIL)
			registerEntry (index);
	}

	return next
		? makeJsRefTagsForNameChain (next + 1, token, leaf_kind, index)
		: index;
}

static int makeJsTagCommon (const tokenInfo *const token, const jsKind kind,
							vString *const signature, vString *const inheritance,
							bool anonymous)
{
	int index = CORK_NIL;
	const char *name = vStringValue (token->string);

	const char *p;
	char *name_chain = NULL;
	if (!token->dynamicProp && kind != JSTAG_PROPERTY &&  (p = strrchr (name, '.')) != NULL )
	{
		if ((p - name) != 0)
			name_chain = eStrndup (name, (size_t) (p - name));
		name = p + 1;
		if (name[0] == '\0')
			return CORK_NIL;
	}

	int scope = token->scope;
	if (name_chain)
	{
		scope = makeJsRefTagsForNameChain (name_chain, token, kind, scope);
		eFree (name_chain);
	}

	/*
	 * Check whether NAME is already defined in SCOPE.
	 * If the NAME is already defined, return the cork index for the NAME.
	 */
	if (kind == JSTAG_FUNCTION || kind == JSTAG_CLASS)
	{
		index = anyKindEntryInScope (scope, name, kind, true);
		if (index != CORK_NIL)
			return index;
	}

	tagEntryInfo e;
	initTagEntry (&e, name, kind);
	e.lineNumber   = token->lineNumber;
	e.filePosition = token->filePosition;
	e.extensionFields.scopeIndex = scope;

#ifdef DO_TRACING
	{
		const char *scopeStr = getNameStringForCorkIndex (scope);
		const char *scopeKindStr = getKindStringForCorkIndex (scope);
		TRACE_PRINT("Emitting tag for symbol '%s' of kind %s with scope '%s:%s'", name, kindName(kind), scopeKindStr, scopeStr);
	}
#endif

	if (signature && vStringLength(signature))
	{
		size_t i;
		/* sanitize signature by replacing all control characters with a
		 * space (because it's simple).
		 * there should never be any junk in a valid signature, but who
		 * knows what the user wrote and CTags doesn't cope well with weird
		 * characters. */
		for (i = 0; i < signature->length; i++)
		{
			unsigned char c = (unsigned char) vStringChar (signature, i);
			if (c < 0x20 /* below space */ || c == 0x7F /* DEL */)
				vStringChar (signature, i) = ' ';
		}
		e.extensionFields.signature = vStringValue(signature);
	}

	if (inheritance)
		e.extensionFields.inheritance = vStringValue(inheritance);

	if (anonymous)
		markTagExtraBit (&e, XTAG_ANONYMOUS);

	index = makeTagEntry (&e);
	/* We shold remove This condition. We should fix the callers passing
	 * an empty name instead. makeTagEntry() returns CORK_NIL if the tag
	 * name is empty. */
	if (index != CORK_NIL)
		registerEntry (index);

	return index;
}

static int makeJsTag (const tokenInfo *const token, const jsKind kind,
					   vString *const signature, vString *const inheritance)
{
	return makeJsTagCommon (token, kind, signature, inheritance, false);
}

static int makeClassTagCommon (tokenInfo *const token, vString *const signature,
                          vString *const inheritance, bool anonymous)
{
	return makeJsTagCommon (token, JSTAG_CLASS, signature, inheritance, anonymous);
}

static int makeClassTag (tokenInfo *const token, vString *const signature,
						  vString *const inheritance)
{
	return makeClassTagCommon (token, signature, inheritance, false);
}

static int makeFunctionTagCommon (tokenInfo *const token, vString *const signature, bool generator,
								   bool anonymous)
{
	return makeJsTagCommon (token, generator ? JSTAG_GENERATOR : JSTAG_FUNCTION, signature, NULL,
							anonymous);
}

static int makeFunctionTag (tokenInfo *const token, vString *const signature, bool generator)
{
	return makeFunctionTagCommon (token, signature, generator, false);
}

static bool isClassName (tokenInfo *const name)
{
	char * p = strrchr(vStringValue (name->string), '.');
	if (p == NULL)
		p = vStringValue (name->string);
	else
		p++;

	return isupper((unsigned char) *p);
}

/*
 *	 Parsing functions
 */

/* given @p point, returns the first byte of the encoded output sequence, and
 * make sure the next ones will be returned by calls to getcFromInputFile()
 * as if the code point was simply written in the input file. */
static int handleUnicodeCodePoint (uint32_t point)
{
	int c = (int) point;

	Assert (point < 0x110000);

#ifdef HAVE_ICONV
	/* if we do have iconv and the encodings are specified, use this */
	if (isConverting () && JSUnicodeConverter == (iconv_t) -2)
	{
		/* if we didn't try creating the converter yet, try and do so */
		JSUnicodeConverter = iconv_open (getLanguageEncoding (Lang_js), INTERNAL_ENCODING);
	}
	if (isConverting () && JSUnicodeConverter != (iconv_t) -1)
	{
		char *input_ptr = (char *) &point;
		size_t input_left = sizeof point;
		/* 4 bytes should be enough for any encoding (it's how much UTF-32
		 * would need). */
		/* FIXME: actually iconv has a tendency to output a BOM for Unicode
		 * encodings where it matters when the endianness is not specified in
		 * the target encoding name.  E.g., if the target encoding is "UTF-32"
		 * or "UTF-16" it will output 2 code points, the BOM (U+FEFF) and the
		 * one we expect. This does not happen if the endianness is specified
		 * explicitly, e.g. with "UTF-32LE", or "UTF-16BE".
		 * However, it's not very relevant for the moment as nothing in CTags
		 * cope well (if at all) with non-ASCII-compatible encodings like
		 * UTF-32 or UTF-16 anyway. */
		char output[4] = { 0 };
		char *output_ptr = output;
		size_t output_left = ARRAY_SIZE (output);

		if (iconv (JSUnicodeConverter, &input_ptr, &input_left, &output_ptr, &output_left) == (size_t) -1)
		{
			/* something went wrong, which probably means the output encoding
			 * cannot represent the character.  Use a placeholder likely to be
			 * supported instead, that's also valid in an identifier */
			verbose ("JavaScript: Encoding: %s\n", strerror (errno));
			c = '_';
		}
		else
		{
			const size_t output_len = ARRAY_SIZE (output) - output_left;

			/* put all but the first byte back so that getcFromInputFile() will
			 * return them in the right order */
			for (unsigned int i = 1; i < output_len; i++)
				ungetcToInputFile ((unsigned char) output[output_len - i]);
			c = (unsigned char) output[0];
		}

		iconv (JSUnicodeConverter, NULL, NULL, NULL, NULL);
	}
	else
#endif
	{
		/* when no encoding is specified (or no iconv), assume UTF-8 is good.
		 * Why UTF-8?  Because it's an ASCII-compatible common Unicode encoding. */
		if (point < 0x80)
			c = (unsigned char) point;
		else if (point < 0x800)
		{
			c = (unsigned char) (0xc0 | ((point >> 6) & 0x1f));
			ungetcToInputFile ((unsigned char) (0x80 | (point & 0x3f)));
		}
		else if (point < 0x10000)
		{
			c = (unsigned char) (0xe0 | ((point >> 12) & 0x0f));
			ungetcToInputFile ((unsigned char) (0x80 | ((point >>  0) & 0x3f)));
			ungetcToInputFile ((unsigned char) (0x80 | ((point >>  6) & 0x3f)));
		}
		else if (point < 0x110000)
		{
			c = (unsigned char) (0xf0 | ((point >> 18) & 0x07));
			ungetcToInputFile ((unsigned char) (0x80 | ((point >>  0) & 0x3f)));
			ungetcToInputFile ((unsigned char) (0x80 | ((point >>  6) & 0x3f)));
			ungetcToInputFile ((unsigned char) (0x80 | ((point >> 12) & 0x3f)));
		}
	}

	return c;
}

/* reads a Unicode escape sequence after the "\" prefix.
 * @param value Location to store the escape sequence value.
 * @param isUTF16 Location to store whether @param value is an UTF-16 word.
 * @returns Whether a valid sequence was read. */
static bool readUnicodeEscapeSequenceValue (uint32_t *const value,
                                            bool *const isUTF16)
{
	bool valid = false;
	int d = getcFromInputFile ();

	if (d != 'u')
		ungetcToInputFile (d);
	else
	{
		int e = getcFromInputFile ();
		char cp[6 + 1]; /* up to 6 hex + possible closing '}' or invalid char */
		unsigned int cp_len = 0;

		*isUTF16 = (e != '{');
		if (e == '{')
		{	/* Handles Unicode code point escapes: \u{ HexDigits }
			 * We skip the leading 0s because there can be any number of them
			 * and they don't change any meaning. */
			bool has_leading_zero = false;

			while ((cp[cp_len] = (char) getcFromInputFile ()) == '0')
				has_leading_zero = true;

			while (isxdigit (cp[cp_len]) && ++cp_len < ARRAY_SIZE (cp))
				cp[cp_len] = (char) getcFromInputFile ();
			valid = ((cp_len > 0 || has_leading_zero) &&
					 cp_len < ARRAY_SIZE (cp) && cp[cp_len] == '}' &&
					 /* also check if it's a valid Unicode code point */
					 (cp_len < 6 ||
					  (cp_len == 6 && strncmp (cp, "110000", 6) < 0)));
			if (! valid) /* put back the last (likely invalid) character */
				ungetcToInputFile (cp[cp_len]);
		}
		else
		{	/* Handles Unicode escape sequences: \u Hex4Digits */
			do
				cp[cp_len] = (char) ((cp_len == 0) ? e : getcFromInputFile ());
			while (isxdigit (cp[cp_len]) && ++cp_len < 4);
			valid = (cp_len == 4);
		}

		if (! valid)
		{
			/* we don't get every character back, but it would require to
			 * be able to put up to 9 characters back (in the worst case
			 * for handling invalid \u{10FFFFx}), and here we're recovering
			 * from invalid syntax anyway. */
			ungetcToInputFile (e);
			ungetcToInputFile (d);
		}
		else
		{
			*value = 0;
			for (unsigned int i = 0; i < cp_len; i++)
			{
				*value *= 16;

				/* we know it's a hex digit, no need to double check */
				if (cp[i] < 'A')
					*value += (unsigned int) cp[i] - '0';
				else if (cp[i] < 'a')
					*value += 10 + (unsigned int) cp[i] - 'A';
				else
					*value += 10 + (unsigned int) cp[i] - 'a';
			}
		}
	}

	return valid;
}

static int valueToXDigit (unsigned char v)
{
	Assert (v <= 0xF);

	if (v >= 0xA)
		return 'A' + (v - 0xA);
	else
		return '0' + v;
}

/* Reads and expands a Unicode escape sequence after the "\" prefix.  If the
 * escape sequence is a UTF16 high surrogate, also try and read the low
 * surrogate to emit the proper code point.
 * @param fallback The character to return if the sequence is invalid. Usually
 *                 this would be the '\' character starting the sequence.
 * @returns The first byte of the sequence, or @param fallback if the sequence
 *          is invalid. On success, next calls to getcFromInputFile() will
 *          return subsequent bytes (if any). */
static int readUnicodeEscapeSequence (const int fallback)
{
	int c;
	uint32_t value;
	bool isUTF16;

	if (! readUnicodeEscapeSequenceValue (&value, &isUTF16))
		c = fallback;
	else
	{
		if (isUTF16 && (value & 0xfc00) == 0xd800)
		{	/* this is a high surrogate, try and read its low surrogate and
			 * emit the resulting code point */
			uint32_t low;
			int d = getcFromInputFile ();

			if (d != '\\' || ! readUnicodeEscapeSequenceValue (&low, &isUTF16))
				ungetcToInputFile (d);
			else if (! isUTF16)
			{	/* not UTF-16 low surrogate but a plain code point */
				d = handleUnicodeCodePoint (low);
				ungetcToInputFile (d);
			}
			else if ((low & 0xfc00) != 0xdc00)
			{	/* not a low surrogate, so put back the escaped representation
				 * in case it was another high surrogate we should read as part
				 * of another pair. */
				ungetcToInputFile (valueToXDigit ((unsigned char) ((low & 0x000f) >>  0)));
				ungetcToInputFile (valueToXDigit ((unsigned char) ((low & 0x00f0) >>  4)));
				ungetcToInputFile (valueToXDigit ((unsigned char) ((low & 0x0f00) >>  8)));
				ungetcToInputFile (valueToXDigit ((unsigned char) ((low & 0xf000) >> 12)));
				ungetcToInputFile ('u');
				ungetcToInputFile ('\\');
			}
			else
				value = 0x010000 + ((value & 0x03ff) << 10) + (low & 0x03ff);
		}
		c = handleUnicodeCodePoint (value);
	}

	return c;
}

static void parseString (vString *const string, const int delimiter)
{
	bool end = false;
	while (! end)
	{
		int c = getcFromInputFile ();
		if (c == EOF)
			end = true;
		else if (c == '\\')
		{
			/* Eat the escape sequence (\", \', etc).  We properly handle
			 * <LineContinuation> by eating a whole \<CR><LF> not to see <LF>
			 * as an unescaped character, which is invalid and handled below.
			 * Also, handle the fact that <LineContinuation> produces an empty
			 * sequence.
			 * See ECMA-262 7.8.4 */
			c = getcFromInputFile ();
			if (c == 'u')
			{
				ungetcToInputFile (c);
				c = readUnicodeEscapeSequence ('\\');
				vStringPut (string, c);
			}
			else if (c != '\r' && c != '\n')
				vStringPut(string, c);
			else if (c == '\r')
			{
				c = getcFromInputFile();
				if (c != '\n')
					ungetcToInputFile (c);
			}
		}
		else if (c == delimiter)
			end = true;
		else if (c == '\r' || c == '\n')
		{
			/* those are invalid when not escaped */
			end = true;
			/* we don't want to eat the newline itself to let the automatic
			 * semicolon insertion code kick in */
			ungetcToInputFile (c);
		}
		else
			vStringPut (string, c);
	}
}

static void parseRegExp (void)
{
	int c;
	bool in_range = false;

	do
	{
		c = getcFromInputFile ();
		if (! in_range && c == '/')
		{
			do /* skip flags */
			{
				c = getcFromInputFile ();
			} while (isalpha (c));
			ungetcToInputFile (c);
			break;
		}
		else if (c == '\n' || c == '\r')
		{
			/* invalid in a regex */
			ungetcToInputFile (c);
			break;
		}
		else if (c == '\\')
			c = getcFromInputFile (); /* skip next character */
		else if (c == '[')
			in_range = true;
		else if (c == ']')
			in_range = false;
	} while (c != EOF);
}

/*	Read a C identifier beginning with "firstChar" and places it into
 *	"name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar (c));
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
		if (c == '\\')
			c = readUnicodeEscapeSequence (c);
	} while (isIdentChar (c));
	/* if readUnicodeEscapeSequence() read an escape sequence this is incorrect,
	 * as we should actually put back the whole escape sequence and not the
	 * decoded character.  However, it's not really worth the hassle as it can
	 * only happen if the input has an invalid escape sequence. */
	ungetcToInputFile (c);		/* unget non-identifier character */
}

static void parseTemplateString (vString *const string)
{
	int c;
	do
	{
		c = getcFromInputFile ();
		if (c == '`' || c == EOF)
			break;

		vStringPut (string, c);

		if (c == '\\')
		{
			c = getcFromInputFile();
			if (c != EOF)
				vStringPut(string, c);
		}
		else if (c == '$')
		{
			c = getcFromInputFile ();
			if (c != '{')
				ungetcToInputFile (c);
			else
			{
				int depth = 1;
				/* we need to use the real token machinery to handle strings,
				 * comments, regexes and whatnot */
				tokenInfo *token = newToken ();
				LastTokenType = TOKEN_UNDEFINED;
				vStringPut(string, c);
				do
				{
					readTokenFull (token, false, string);
					if (isType (token, TOKEN_OPEN_CURLY))
						depth++;
					else if (isType (token, TOKEN_CLOSE_CURLY))
						depth--;
				}
				while (! isType (token, TOKEN_EOF) && depth > 0);
				deleteToken (token);
			}
		}
	}
	while (c != EOF);
}

static void readTokenFullRaw (tokenInfo *const token, bool include_newlines, vString *const repr)
{
	int c;
	int i;
	bool newline_encountered = false;

	/* if we've got a token held back, emit it */
	if (NextToken)
	{
		copyToken (token, NextToken, false);
		deleteToken (NextToken);
		NextToken = NULL;
		return;
	}

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:
	i = 0;
	do
	{
		c = getcFromInputFile ();
		if (include_newlines && (c == '\r' || c == '\n'))
			newline_encountered = true;
		i++;
	}
	while (c == '\t' || c == ' ' || c == '\r' || c == '\n');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	if (repr && c != EOF)
	{
		if (i > 1)
			vStringPut (repr, ' ');
		vStringPut (repr, c);
	}

	switch (c)
	{
		case EOF: token->type = TOKEN_EOF;					break;
		case '(': token->type = TOKEN_OPEN_PAREN;			break;
		case ')': token->type = TOKEN_CLOSE_PAREN;			break;
		case ';': token->type = TOKEN_SEMICOLON;			break;
		case ',': token->type = TOKEN_COMMA;				break;
		case '.':
			{
				token->type = TOKEN_PERIOD;

				int d = getcFromInputFile ();
				if (d != '.')
				{
					ungetcToInputFile (d);
					break;
				}

				d = getcFromInputFile ();
				if (d != '.')
				{
					ungetcToInputFile (d);
					ungetcToInputFile ('.');
					break;
				}

				token->type = TOKEN_DOTS;
				if (repr)
				{
					/* Adding two dots is enough here.
					 * The first one is already added with
					 * vStringPut (repr, c).
					 */
					vStringCatS (repr, "..");
				}
				break;
			}
		case ':': token->type = TOKEN_COLON;				break;
		case '{': token->type = TOKEN_OPEN_CURLY;			break;
		case '}': token->type = TOKEN_CLOSE_CURLY;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;			break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;			break;

		case '=':
			{
				int d = getcFromInputFile ();
				if (d == '>')
					token->type = TOKEN_ARROW;
				else
				{
					ungetcToInputFile (d);
					token->type = TOKEN_EQUAL_SIGN;
				}
				break;
			}

		case '+':
		case '-':
			{
				int d = getcFromInputFile ();
				if (d == c) /* ++ or -- */
					token->type = TOKEN_POSTFIX_OPERATOR;
				else
				{
					ungetcToInputFile (d);
					token->type = TOKEN_BINARY_OPERATOR;
				}
				break;
			}

		case '*':
			token->type = TOKEN_STAR;
			break;
		case '%':
		case '?':
		case '>':
		case '<':
		case '^':
		case '|':
		case '&':
			token->type = TOKEN_BINARY_OPERATOR;
			break;

		case '\'':
		case '"':
				  token->type = TOKEN_STRING;
				  parseString (token->string, c);
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  if (repr)
				  {
					  vStringCat (repr, token->string);
					  vStringPut (repr, c);
				  }
				  break;

		case '`':
				  token->type = TOKEN_TEMPLATE_STRING;
				  parseTemplateString (token->string);
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  if (repr)
				  {
					  vStringCat (repr, token->string);
					  vStringPut (repr, c);
				  }
				  break;

		case '/':
				  {
					  int d = getcFromInputFile ();
					  if ( (d != '*') &&		/* is this the start of a comment? */
							  (d != '/') )		/* is a one line comment? */
					  {
						  ungetcToInputFile (d);
						  switch (LastTokenType)
						  {
							  case TOKEN_CHARACTER:
							  case TOKEN_IDENTIFIER:
							  case TOKEN_STRING:
							  case TOKEN_TEMPLATE_STRING:
							  case TOKEN_CLOSE_CURLY:
							  case TOKEN_CLOSE_PAREN:
							  case TOKEN_CLOSE_SQUARE:
								  token->type = TOKEN_BINARY_OPERATOR;
								  break;

							  default:
								  token->type = TOKEN_REGEXP;
								  parseRegExp ();
								  token->lineNumber = getInputLineNumber ();
								  token->filePosition = getInputFilePosition ();
								  break;
						  }
					  }
					  else
					  {
						  if (repr) /* remove the / we added */
							  vStringChop(repr);
						  if (d == '*')
						  {
							  skipToCharacterInInputFile2('*', '/');
							  goto getNextChar;
						  }
						  else if (d == '/')	/* is this the start of a comment?  */
						  {
							  skipToCharacterInInputFile ('\n');
							  /* if we care about newlines, put it back so it is seen */
							  if (include_newlines)
								  ungetcToInputFile ('\n');
							  goto getNextChar;
						  }
					  }
					  break;
				  }

		case '#':
				  /* skip shebang in case of e.g. Node.js scripts */
				  if (token->lineNumber > 1)
					  token->type = TOKEN_UNDEFINED;
				  else if ((c = getcFromInputFile ()) != '!')
				  {
					  ungetcToInputFile (c);
					  token->type = TOKEN_UNDEFINED;
				  }
				  else
				  {
					  skipToCharacterInInputFile ('\n');
					  goto getNextChar;
				  }
				  break;

		case '@':
				  token->type = TOKEN_ATMARK;
				  break;

		case '\\':
				  c = readUnicodeEscapeSequence (c);
				  /* fallthrough */
		default:
				  if (! isIdentChar (c))
					  token->type = TOKEN_UNDEFINED;
				  else
				  {
					  parseIdentifier (token->string, c);
					  token->lineNumber = getInputLineNumber ();
					  token->filePosition = getInputFilePosition ();
					  token->keyword = lookupKeyword (vStringValue (token->string), Lang_js);
					  if (isKeyword (token, KEYWORD_NONE))
						  token->type = TOKEN_IDENTIFIER;
					  else
						  token->type = TOKEN_KEYWORD;
					  if (repr && vStringLength (token->string) > 1)
						  vStringCatS (repr, vStringValue (token->string) + 1);
				  }
				  break;
	}

	if (include_newlines && newline_encountered)
	{
		/* This isn't strictly correct per the standard, but following the
		 * real rules means understanding all statements, and that's not
		 * what the parser currently does.  What we do here is a guess, by
		 * avoiding inserting semicolons that would make the statement on
		 * the left or right obviously invalid.  Hopefully this should not
		 * have false negatives (e.g. should not miss insertion of a semicolon)
		 * but might have false positives (e.g. it will wrongfully emit a
		 * semicolon sometimes, i.e. for the newline in "foo\n(bar)").
		 * This should however be mostly harmless as we only deal with
		 * newlines in specific situations where we know a false positive
		 * wouldn't hurt too bad. */

		/* these already end a statement, so no need to duplicate it */
		#define IS_STMT_SEPARATOR(t) ((t) == TOKEN_SEMICOLON    || \
		                              (t) == TOKEN_EOF          || \
		                              (t) == TOKEN_COMMA        || \
		                              (t) == TOKEN_OPEN_CURLY)
		/* these cannot be the start or end of a statement */
		#define IS_BINARY_OPERATOR(t) ((t) == TOKEN_EQUAL_SIGN      || \
		                               (t) == TOKEN_ARROW           || \
		                               (t) == TOKEN_COLON           || \
		                               (t) == TOKEN_PERIOD          || \
		                               (t) == TOKEN_STAR            || \
		                               (t) == TOKEN_BINARY_OPERATOR)

		if (! IS_STMT_SEPARATOR(LastTokenType) &&
		    ! IS_STMT_SEPARATOR(token->type) &&
		    ! IS_BINARY_OPERATOR(LastTokenType) &&
		    ! IS_BINARY_OPERATOR(token->type) &&
		    /* these cannot be followed by a semicolon */
		    ! (LastTokenType == TOKEN_OPEN_PAREN ||
		       LastTokenType == TOKEN_OPEN_SQUARE))
		{
			/* hold the token... */
			Assert (NextToken == NULL);
			NextToken = newToken ();
			copyToken (NextToken, token, false);

			/* ...and emit a semicolon instead */
			token->type		= TOKEN_SEMICOLON;
			token->keyword	= KEYWORD_NONE;
			vStringClear (token->string);
			if (repr)
				vStringPut (token->string, '\n');
		}

		#undef IS_STMT_SEPARATOR
		#undef IS_BINARY_OPERATOR
	}

	LastTokenType = token->type;
}

/* See https://babeljs.io/blog/2018/09/17/decorators */
static void skipBabelDecorator (tokenInfo *token, bool include_newlines, vString *const repr)
{
	readTokenFullRaw (token, include_newlines, repr);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		/*  @(complex ? dec1 : dec2) */
		skipArgumentList (token, include_newlines, repr);
		TRACE_PRINT ("found @(...) style decorator");
	}
	else if (isType (token, TOKEN_IDENTIFIER))
	{
		/*  @namespace.foo (...) */
		bool found_period = false;
		while (1)
		{
			readTokenFullRaw (token, include_newlines, repr);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				if (!found_period)
				{
					TRACE_PRINT("found @namespace.bar style decorator");
					break;
				}
				found_period = false;
			}
			else if (isType (token, TOKEN_PERIOD))
				found_period = true;
			else if (isType (token, TOKEN_OPEN_PAREN))
			{
				skipArgumentList (token, include_newlines, repr);
				TRACE_PRINT("found @foo(...) style decorator");
				break;
			}
			else
			{
				TRACE_PRINT("found @foo style decorator");
				break;
			}
		}
	}
	else
		/* Unexpected token after @ */
		TRACE_PRINT("found unexpected token during skipping a decorator");
}

static void readTokenFull (tokenInfo *const token, bool include_newlines, vString *const repr)
{
	readTokenFullRaw (token, include_newlines, repr);

	while (1)
	{
		if (!isType (token, TOKEN_ATMARK))
			break;
		skipBabelDecorator (token, include_newlines, repr);
		/* @decorator0 @decorator1 ... There can be more than one decorator. */
	}
}

#ifdef DO_TRACING_USE_DUMP_TOKEN
/* trace readTokenFull() */
static void readTokenFullDebug (tokenInfo *const token, bool include_newlines, vString *const repr)
{
	readTokenFull (token, include_newlines, repr);
	dumpToken (token);
}
# define readTokenFull readTokenFullDebug
#endif

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, false, NULL);
}

/*
 *	 Token parsing functions
 */

static int parseMethodsInAnonymousObject (tokenInfo *const token)
{
	int index = CORK_NIL;

	tokenInfo *const anon_object = newToken ();
	copyToken (anon_object, token, true);
	anonGenerate (anon_object->string, "anonymousObject", JSTAG_VARIABLE);
	anon_object->type = TOKEN_IDENTIFIER;

	index = makeJsTagCommon (anon_object, JSTAG_VARIABLE, NULL, NULL, true);
	if (! parseMethods (token, index, false))
	{
		/* If no method is found, the anonymous object
		 * should not be tagged.
		 */
		tagEntryInfo *e = getEntryInCorkQueue (index);
		if (e)
			markTagPlaceholder (e, true);
		index = CORK_NIL;
	}

	deleteToken (anon_object);

	return index;
}

static void skipArgumentList (tokenInfo *const token, bool include_newlines, vString *const repr)
{
	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	{
		int nest_level = 1;
		if (repr)
			vStringPut (repr, '(');

		tokenType prev_token_type = token->type;
		while (nest_level > 0 && ! isType (token, TOKEN_EOF))
		{
			readTokenFull (token, false, repr);
			if (isType (token, TOKEN_OPEN_PAREN))
				nest_level++;
			else if (isType (token, TOKEN_CLOSE_PAREN))
				nest_level--;
			else if (isType (token, TOKEN_OPEN_CURLY))
			{
				if (prev_token_type == TOKEN_ARROW)
					parseBlock (token, CORK_NIL);
				else
					parseMethodsInAnonymousObject (token);
			}
			else if (isKeyword (token, KEYWORD_function))
				parseFunction (token);

			prev_token_type = token->type;
		}
		readTokenFull (token, include_newlines, NULL);
	}
}

static void skipArrayList (tokenInfo *const token, bool include_newlines)
{
	/*
	 * Handle square brackets
	 *	 var name[1]
	 * So we must check for nested open and closing square brackets
	 */

	if (isType (token, TOKEN_OPEN_SQUARE))	/* arguments? */
	{
		int nest_level = 1;
		tokenType prev_token_type = token->type;
		while (nest_level > 0 && ! isType (token, TOKEN_EOF))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				nest_level++;
			else if (isType (token, TOKEN_CLOSE_SQUARE))
				nest_level--;
			else if (isType (token, TOKEN_OPEN_CURLY))
			{
				if (prev_token_type == TOKEN_ARROW)
					parseBlock (token, CORK_NIL);
				else
					parseMethodsInAnonymousObject (token);
			}

			prev_token_type = token->type;
		}
		readTokenFull (token, include_newlines, NULL);
	}
}

static void skipQualifiedIdentifier (tokenInfo *const token)
{
	/* Skip foo.bar.baz */
	while (isType (token, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
			readToken (token);
		else
			break;
	}
}

static void addContext (tokenInfo* const parent, const tokenInfo* const child)
{
	if (vStringLength (parent->string) > 0)
	{
		vStringPut (parent->string, '.');
	}
	vStringCat (parent->string, child->string);
}

/*
 *	 Scanning functions
 */

static bool findCmdTerm (tokenInfo *const token, bool include_newlines,
                            bool include_commas)
{
	/*
	 * Read until we find either a semicolon or closing brace.
	 * Any nested braces will be handled within.
	 */
	while (! isType (token, TOKEN_SEMICOLON) &&
		   ! isType (token, TOKEN_CLOSE_CURLY) &&
		   ! (include_commas && isType (token, TOKEN_COMMA)) &&
		   ! isType (token, TOKEN_EOF))
	{
		/* Handle nested blocks */
		if ( isType (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, CORK_NIL);
			readTokenFull (token, include_newlines, NULL);
		}
		else if ( isType (token, TOKEN_OPEN_PAREN) )
			skipArgumentList(token, include_newlines, NULL);
		else if ( isType (token, TOKEN_OPEN_SQUARE) )
			skipArrayList(token, include_newlines);
		else
			readTokenFull (token, include_newlines, NULL);
	}

	return isType (token, TOKEN_SEMICOLON);
}

static void parseSwitch (tokenInfo *const token)
{
	/*
	 * switch (expression) {
	 * case value1:
	 *	   statement;
	 *	   break;
	 * case value2:
	 *	   statement;
	 *	   break;
	 * default : statement;
	 * }
	 */

	readToken (token);

	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipArgumentList(token, false, NULL);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, CORK_NIL);
	}
}

static bool parseLoop (tokenInfo *const token)
{
	/*
	 * Handles these statements
	 *	   for (x=0; x<3; x++)
	 *		   document.write("This text is repeated three times<br>");
	 *
	 *	   for (x=0; x<3; x++)
	 *	   {
	 *		   document.write("This text is repeated three times<br>");
	 *	   }
	 *
	 *	   while (number<5){
	 *		   document.write(number+"<br>");
	 *		   number++;
	 *	   }
	 *
	 *	   do{
	 *		   document.write(number+"<br>");
	 *		   number++;
	 *	   }
	 *	   while (number<5);
	 */
	bool is_terminated = true;

	if (isKeyword (token, KEYWORD_for) || isKeyword (token, KEYWORD_while))
	{
		readToken(token);

		if (isType (token, TOKEN_OPEN_PAREN))
			skipArgumentList(token, false, NULL);

		if (isType (token, TOKEN_OPEN_CURLY))
			parseBlock (token, CORK_NIL);
		else
			is_terminated = parseLine(token, false);
	}
	else if (isKeyword (token, KEYWORD_do))
	{
		readToken(token);

		if (isType (token, TOKEN_OPEN_CURLY))
			parseBlock (token, CORK_NIL);
		else
			is_terminated = parseLine(token, false);

		if (is_terminated)
			readToken(token);

		if (isKeyword (token, KEYWORD_while))
		{
			readToken(token);

			if (isType (token, TOKEN_OPEN_PAREN))
				skipArgumentList(token, true, NULL);

			if (! isType (token, TOKEN_SEMICOLON))
			{
				/* oddly enough, `do {} while (0) var foo = 42` is perfectly
				 * valid JS, so explicitly handle the remaining of the line
				 * for the sake of the root scope handling (as parseJsFile()
				 * always advances a token not to ever get stuck) */
				is_terminated = parseLine(token, false);
			}
		}
	}

	return is_terminated;
}

static bool parseIf (tokenInfo *const token)
{
	bool read_next_token = true;
	/*
	 * If statements have two forms
	 *	   if ( ... )
	 *		   one line;
	 *
	 *	   if ( ... )
	 *		  statement;
	 *	   else
	 *		  statement
	 *
	 *	   if ( ... ) {
	 *		  multiple;
	 *		  statements;
	 *	   }
	 *
	 *
	 *	   if ( ... ) {
	 *		  return elem
	 *	   }
	 *
	 *     This example if correctly written, but the
	 *     else contains only 1 statement without a terminator
	 *     since the function finishes with the closing brace.
	 *
     *     function a(flag){
     *         if(flag)
     *             test(1);
     *         else
     *             test(2)
     *     }
	 *
	 * TODO:  Deal with statements that can optional end
	 *		  without a semi-colon.  Currently this messes up
	 *		  the parsing of blocks.
	 *		  Need to somehow detect this has happened, and either
	 *		  backup a token, or skip reading the next token if
	 *		  that is possible from all code locations.
	 *
	 */

	readToken (token);

	if (isKeyword (token, KEYWORD_if))
	{
		/*
		 * Check for an "else if" and consume the "if"
		 */
		readToken (token);
	}

	if (isType (token, TOKEN_OPEN_PAREN))
		skipArgumentList(token, false, NULL);

	if (isType (token, TOKEN_OPEN_CURLY))
		parseBlock (token, CORK_NIL);
	else
	{
		/* The next token should only be read if this statement had its own
		 * terminator */
		read_next_token = findCmdTerm (token, true, false);
	}
	return read_next_token;
}

static bool collectChildren (int corkIndex, tagEntryInfo *entry, void *data)
{
	intArray *children = (intArray *)data;

	Assert (entry->extensionFields.scopeIndex != CORK_NIL);
	intArrayAdd (children, corkIndex);

	return true;
}

/* During parsing, there is a case that a language object (parent)
 * should be tagged only when there are language objects (children)
 * are defined in the parent; if the parent has no child, the parser
 * should not make a tag for the parent.
 *
 * Handling the this case was not easy because the parser must fill
 * the scope field of children with the cork index of parent.
 * However, the parser can decide whether the parent should be tagged
 * or not after parsing inside the parent where the children are
 * defined.
 *
 * "class" is an example of the language object of the parent.
 * "methods" are examples of the language object of the children.
 * "class" is tagged as a class only when methods are found in it.
 *
 *
 * The parser handles this case with the following steps:
 *
 * 1.  make a dummy tag entry for the candidate of parent with
 *
 * >       int dummyIndex = makeSimplePlaceholder().
 *
 *     ctags doesn't emit this dummy tag entry.
 *
 * 2.  parse inside the candidate of parent and count children.
 *     If a child is found, make a tag for it with filling its
 *     scope field with the dummyIndex.
 *
 * 3. make a true tag entry for the parent if a child is found:
 *
 * >       int trueIdex = makeTagEntry (...);
 *
 * 4. update the scope fields of children with the trueIdex.
 *
 *         moveChildren (dummyIndex, trueIdex);
 *
 */
static void moveChildren (int oldParent, int newParent)
{
	intArray *children = intArrayNew ();
	foreachEntriesInScope (oldParent, NULL, collectChildren, children);
	for (unsigned int i = 0; i < intArrayCount (children); i++)
	{
		int c = intArrayItem (children, i);

		unregisterEntry (c);
		tagEntryInfo *e = getEntryInCorkQueue (c);
		Assert (e);
		e->extensionFields.scopeIndex = newParent;
		registerEntry (c);
	}
	intArrayDelete (children);
}

static void parseFunction (tokenInfo *const token)
{
#ifdef DO_TRACING
	{
		const char *scopeStr = getNameStringForCorkIndex (token->scope);
		const char *scopeKindStr = getKindStringForCorkIndex (token->scope);
		TRACE_ENTER_TEXT("token has scope '%s' of kind %s", scopeStr, scopeKindStr);
	}
#endif

	tokenInfo *const name = newToken ();
	vString *const signature = vStringNew ();
	bool is_generator = false;
	bool is_anonymous = false;
	/*
	 * This deals with these formats
	 *	   function validFunctionTwo(a,b) {}
	 *	   function * generator(a,b) {}
	 */

	copyToken (name, token, true);
	readToken (name);
	if (isType (name, TOKEN_STAR))
	{
		is_generator = true;
		readToken (name);
	}
	if (isType (name, TOKEN_OPEN_PAREN))
	{
		/* anonymous function */
		copyToken (token, name, false);
		anonGenerate (name->string, "anonymousFunction", JSTAG_FUNCTION);
		is_anonymous = true;
	}
	else if (!isType (name, TOKEN_IDENTIFIER))
		goto cleanUp;
	else
		readToken (token);

	while (isType (token, TOKEN_PERIOD))
	{
		readToken (token);
		if (! isType(token, TOKEN_KEYWORD))
		{
			addContext (name, token);
			readToken (token);
		}
	}

	if ( isType (token, TOKEN_OPEN_PAREN) )
		skipArgumentList(token, false, signature);

	if ( isType (token, TOKEN_OPEN_CURLY) )
	{
		const int p = isClassName (name) ?
			makeClassTagCommon (name, signature, NULL, is_anonymous) :
			makeFunctionTagCommon (name, signature, is_generator, is_anonymous);

		parseBlock (token, p);
	}

	findCmdTerm (token, false, false);

 cleanUp:
	vStringDelete (signature);
	deleteToken (name);

	TRACE_LEAVE();
}

/* Parses a block surrounded by curly braces.
 * @p parentScope is the scope name for this block, or NULL for unnamed scopes */
static bool parseBlock (tokenInfo *const token, int parentScope)
{
	TRACE_ENTER();

	bool is_class = false;
	bool read_next_token = true;
	int saveScope = token->scope;

	if (parentScope != CORK_NIL)
	{
		token->scope = parentScope;
		token->nestLevel++;
	}

	/*
	 * Make this routine a bit more forgiving.
	 * If called on an open_curly advance it
	 */
	if (isType (token, TOKEN_OPEN_CURLY))
		readToken(token);

	if (! isType (token, TOKEN_CLOSE_CURLY))
	{
		/*
		 * Read until we find the closing brace,
		 * any nested braces will be handled within
		 */
		do
		{
			read_next_token = true;
			if (isKeyword (token, KEYWORD_this))
			{
				/*
				 * Means we are inside a class and have found
				 * a class, not a function
				 */
				is_class = true;

				/*
				 * Ignore the remainder of the line
				 * findCmdTerm(token);
				 */
				read_next_token = parseLine (token, is_class);
			}
			else if (isKeyword (token, KEYWORD_var) ||
					 isKeyword (token, KEYWORD_let) ||
					 isKeyword (token, KEYWORD_const))
			{
				/*
				 * Potentially we have found an inner function.
				 * Set something to indicate the scope
				 */
				read_next_token = parseLine (token, is_class);
			}
			else if (isType (token, TOKEN_OPEN_CURLY))
			{
				/* Handle nested blocks */
				parseBlock (token, CORK_NIL);
			}
			else
			{
				/*
				 * It is possible for a line to have no terminator
				 * if the following line is a closing brace.
				 * parseLine will detect this case and indicate
				 * whether we should read an additional token.
				 */
				read_next_token = parseLine (token, is_class);
			}

			/*
			 * Always read a new token unless we find a statement without
			 * a ending terminator
			 */
			if( read_next_token )
				readToken(token);

			/*
			 * If we find a statement without a terminator consider the
			 * block finished, otherwise the stack will be off by one.
			 */
		} while (! isType (token, TOKEN_EOF) &&
				 ! isType (token, TOKEN_CLOSE_CURLY) && read_next_token);
	}

	token->scope = saveScope;
	if (parentScope != CORK_NIL)
		token->nestLevel--;

	TRACE_LEAVE();

	return is_class;
}

static bool parseMethods (tokenInfo *const token, int classIndex,
                          const bool is_es6_class)
{
	TRACE_ENTER_TEXT("token is '%s' of type %s in parentToken '%s' of kind %s (es6: %s)",
					 vStringValue(token->string), tokenTypeName (token->type),
					 classIndex == CORK_NIL ? "none" : getNameStringForCorkIndex (classIndex),
					 classIndex == CORK_NIL ? "none" : getKindStringForCorkIndex (classIndex),
					 is_es6_class? "yes": "no");

	/*
	 * When making a tag for `name', its core index is stored to
	 * `indexForName'. The value stored to `indexForName' is valid
	 * till the value for `name' is updated. If the value for `name'
	 * is changed, `indexForName' is reset to CORK_NIL.
	 */
	tokenInfo *const name = newToken ();
	int indexForName = CORK_NIL;
	bool has_methods = false;
	int saveScope = token->scope;

	if (classIndex != CORK_NIL)
		token->scope = classIndex;

	/*
	 * This deals with these formats
	 *	   validProperty  : 2,
	 *	   validMethod    : function(a,b) {}
	 *	   'validMethod2' : function(a,b) {}
     *     container.dirtyTab = {'url': false, 'title':false, 'snapshot':false, '*': false}
	 *     get prop() {}
	 *     set prop(val) {}
	 *     get(...) {}
	 *     set(...) {}
     *
     * ES6 methods:
     *     property(...) {}
     *     *generator() {}
     *
     * ES6 computed name:
     *     [property]() {}
     *     get [property]() {}
     *     set [property]() {}
     *     *[generator]() {}
	 *
	 * tc39/proposal-class-fields
	 *     field0 = function(a,b) {}
	 *     field1 = 1
	 * The parser extracts field0 as a method because the left value
	 * is a function (kind propagation), and field1 as a field.
	 */

	bool dont_read = false;
	do
	{
		bool is_setter = false;
		bool is_getter = false;

		if (!dont_read)
			readToken (token);
		dont_read = false;

		if (isType (token, TOKEN_CLOSE_CURLY))
		{
			goto cleanUp;
		}

		if (isKeyword (token, KEYWORD_async))
			readToken (token);
		else if (isType (token, TOKEN_KEYWORD) &&
				 (isKeyword (token, KEYWORD_get) || isKeyword (token, KEYWORD_set)))
		{
			tokenInfo *saved_token = newToken ();
			copyToken (saved_token, token, true);
			readToken (token);
			if (isType(token, TOKEN_OPEN_PAREN) || isType(token, TOKEN_COLON))
			{
				Assert (NextToken == NULL);
				NextToken = newToken ();
				copyToken (NextToken, token, false);	/* save token for next read */
				copyToken (token, saved_token, true);	/* restore token to process */
				token->type = TOKEN_IDENTIFIER;			/* process as identifier */
				token->keyword = KEYWORD_NONE;
			}
			else if (isKeyword (saved_token, KEYWORD_get))
				is_getter = true;
			else
				is_setter = true;

			deleteToken (saved_token);
		}

		if (! isType (token, TOKEN_KEYWORD) &&
		    ! isType (token, TOKEN_SEMICOLON))
		{
			bool is_generator = false;
			bool is_shorthand = false; /* ES6 shorthand syntax */
			bool is_computed_name = false; /* ES6 computed property name */
			bool is_dynamic_prop = false;
			vString *dprop = NULL; /* is_computed_name is true but
									* the name is not represented in
									* a string literal. The expressions
									* go this string. */

			if (isType (token, TOKEN_STAR)) /* shorthand generator */
			{
				is_generator = true;
				readToken (token);
			}

			if (isType (token, TOKEN_OPEN_SQUARE))
			{
				is_computed_name = true;
				dprop = vStringNewInit ("[");
				readTokenFull (token, false, dprop);
			}

			copyToken(name, token, true);
			indexForName = CORK_NIL;
			if (is_computed_name && ! isType (token, TOKEN_STRING))
				is_dynamic_prop = true;

			readTokenFull (token, false, dprop);

			if (is_computed_name)
			{
				int depth = 1;
				do
				{
					if (isType (token, TOKEN_CLOSE_SQUARE))
						depth--;
					else
					{
						is_dynamic_prop = true;
						if (isType (token, TOKEN_OPEN_SQUARE))
							depth++;
					}
					readTokenFull (token, false, (is_dynamic_prop && depth != 0)? dprop: NULL);
				} while (! isType (token, TOKEN_EOF) && depth > 0);
			}

			if (is_dynamic_prop)
			{
				injectDynamicName (name, dprop);
				indexForName = CORK_NIL;
				dprop = NULL;
			}
			else
				vStringDelete (dprop);

			is_shorthand = isType (token, TOKEN_OPEN_PAREN);
			bool can_be_field = isType (token, TOKEN_EQUAL_SIGN);
			if ( isType (token, TOKEN_COLON) || can_be_field || is_shorthand )
			{
				if (! is_shorthand)
				{
					readToken (token);
					if (isKeyword (token, KEYWORD_async))
						readToken (token);
				}

				vString * signature = vStringNew ();
				if ( is_shorthand || isKeyword (token, KEYWORD_function) )
				{
					TRACE_PRINT("Seems to be a function or shorthand");

					if (! is_shorthand)
					{
						readToken (token);
						if (isType (token, TOKEN_STAR))
						{
							/* generator: 'function' '*' '(' ... ')' '{' ... '}' */
							is_generator = true;
							readToken (token);
						}
					}
					if ( isType (token, TOKEN_OPEN_PAREN) )
					{
						skipArgumentList(token, false, signature);
					}

function:
					if (isType (token, TOKEN_OPEN_CURLY))
					{
						has_methods = true;

						int kind = JSTAG_METHOD;
						if (is_generator)
							kind = JSTAG_GENERATOR;
						else if (is_getter)
							kind = JSTAG_GETTER;
						else if (is_setter)
							kind = JSTAG_SETTER;

						indexForName = makeJsTag (name, kind, signature, NULL);
						parseBlock (token, indexForName);

						/*
						 * If we aren't parsing an ES6 class (for which there
						 * is no mandatory separators), read to the closing
						 * curly, check next token, if a comma, we must loop
						 * again.
						 */
						if (! is_es6_class)
							readToken (token);
					}
				}
				else if (! is_es6_class)
				{
					int p = CORK_NIL;
					tokenInfo *saved_token = newToken ();

					/* skip whatever is the value */
					while (! isType (token, TOKEN_COMMA) &&
					       ! isType (token, TOKEN_CLOSE_CURLY) &&
					       ! isType (token, TOKEN_EOF))
					{
						if (isType (token, TOKEN_OPEN_CURLY))
						{
							/* Recurse to find child properties/methods */
							p = makeSimplePlaceholder (name->string);
							parseMethods (token, p, false);
							readToken (token);
						}
						else if (isType (token, TOKEN_OPEN_PAREN))
						{
							vStringClear (signature);
							skipArgumentList (token, false, signature);
						}
						else if (isType (token, TOKEN_OPEN_SQUARE))
						{
							skipArrayList (token, false);
						}
						else if (isType (token, TOKEN_ARROW))
						{
							TRACE_PRINT("Seems to be an anonymous function");
							if (vStringIsEmpty (signature) &&
								isType (saved_token, TOKEN_IDENTIFIER))
							{
								vStringPut (signature, '(');
								vStringCat (signature, saved_token->string);
								vStringPut (signature, ')');
							}
							readToken (token);
							deleteToken (saved_token);
							goto function;
						}
						else
						{
							copyToken (saved_token, token, true);
							readToken (token);
						}
					}
					deleteToken (saved_token);

					has_methods = true;
					indexForName = makeJsTag (name, JSTAG_PROPERTY, NULL, NULL);
					if (p != CORK_NIL)
						moveChildren (p, indexForName);
				}
				else if (can_be_field)
				{
					makeJsTag (name, JSTAG_FIELD, NULL, NULL);
					parseLine (token, true);
				}

				vStringDelete (signature);
			}
			else
			{
				makeJsTag (name, JSTAG_FIELD, NULL, NULL);
				if (!isType (token, TOKEN_SEMICOLON))
					dont_read = true;
			}
		}
	} while ( isType(token, TOKEN_COMMA) ||
	          ( is_es6_class && ! isType(token, TOKEN_EOF) ) );

	TRACE_PRINT("Finished parsing methods");

	findCmdTerm (token, false, false);

cleanUp:
	token->scope = saveScope;
	deleteToken (name);

	TRACE_LEAVE_TEXT("found method(s): %s", has_methods? "yes": "no");

	return has_methods;
}

static bool parseES6Class (tokenInfo *const token, const tokenInfo *targetName)
{
	TRACE_ENTER();

	tokenInfo * className = newToken ();
	vString *inheritance = NULL;
	bool is_anonymous = true;

	copyToken (className, token, true);
	readToken (className);

	/* optional name */
	if (isType (className, TOKEN_IDENTIFIER))
	{
		readToken (token);
		is_anonymous = false;
	}
	else
	{
		copyToken (token, className, true);
		/* We create a fake name so we have a scope for the members */
		if (! targetName)
			anonGenerate (className->string, "AnonymousClass", JSTAG_CLASS);
	}

	if (! targetName)
		targetName = className;

	if (isKeyword (token, KEYWORD_extends))
		inheritance = vStringNew ();

	/* skip inheritance info */
	while (! isType (token, TOKEN_OPEN_CURLY) &&
	       ! isType (token, TOKEN_EOF) &&
	       ! isType (token, TOKEN_SEMICOLON))
		readTokenFull (token, false, inheritance);

	/* remove the last added token (here we assume it's one char, "{" or ";" */
	if (inheritance && vStringLength (inheritance) > 0 &&
	    ! isType (token, TOKEN_EOF))
	{
		vStringChop (inheritance);
		vStringStripTrailing (inheritance);
		vStringStripLeading (inheritance);
	}

	TRACE_PRINT("Emitting tag for class '%s'", vStringValue(targetName->string));

	int r = makeJsTagCommon (targetName, JSTAG_CLASS, NULL, inheritance,
							 (is_anonymous && (targetName == className)));

	if (! is_anonymous && targetName != className)
	{
		/* FIXME: what to do with the secondary name?  It's local to the
		 *        class itself, so not very useful... let's hope people
		 *        don't give it another name than the target in case of
		 *        	var MyClass = class MyClassSecondaryName { ... }
		 *        I guess it could be an alias to MyClass, or duplicate it
		 *        altogether, not sure. */
		makeJsTag (className, JSTAG_CLASS, NULL, inheritance);
	}

	if (inheritance)
		vStringDelete (inheritance);

	if (isType (token, TOKEN_OPEN_CURLY))
		parseMethods (token, r, true);

	deleteToken (className);

	TRACE_LEAVE();
	return true;
}

static void convertToFunction (int index, const char *signature)
{
	tagEntryInfo *e = getEntryInCorkQueue(index);
	if (e && e->kindIndex != JSTAG_FUNCTION
		&& ( signature == NULL || e->extensionFields.signature == NULL))
	{
		e->kindIndex = JSTAG_FUNCTION;
		if (signature)
			e->extensionFields.signature = eStrdup (signature);
	}
}

static vString *trimGarbageInSignature (vString *sig)
{
	/* Drop "=>" at the end. */
	const char *sigstr = vStringValue (sig);
	char *last = strrchr (sigstr, ')');
	Assert (last);
	vStringTruncate (sig, last - sigstr + 1);
	return sig;
}

static vString *makeVStringForSignature (tokenInfo *const token)
{
	vString * sig = vStringNewInit ("(");

	if (isType (token, TOKEN_IDENTIFIER))
		vStringCat (sig, token->string);
	else if (isType (token, TOKEN_CLOSE_PAREN))
		vStringPut (sig, ')');
	else if (isType (token, TOKEN_DOTS))
		vStringCatS (sig, "...");

	return sig;
}

static bool parseStatement (tokenInfo *const token, bool is_inside_class)
{
	TRACE_ENTER_TEXT("is_inside_class: %s", is_inside_class? "yes": "no");

	/*
	 * When making a tag for `name', its core index is stored to
	 * `indexForName'. The value stored to `indexForName' is valid
	 * till the value for `name' is updated. If the value for `name'
	 * is changed, `indexForName' is reset to CORK_NIL.
	 */
	tokenInfo *const name = newToken ();
	int indexForName = CORK_NIL;
	tokenInfo *const secondary_name = newToken ();
	tokenInfo *const method_body_token = newToken ();
	int saveScope = token->scope;
	bool is_class = false;
	bool is_var = false;
	bool is_const = false;
	bool is_terminated = true;
	bool is_global = false;
	bool has_methods = false;
	bool found_this = false;

	/*
	 * Functions can be named or unnamed.
	 * This deals with these formats:
	 * Function
	 *	   validFunctionOne = function(a,b) {}
	 *	   testlib.validFunctionFive = function(a,b) {}
	 *	   var innerThree = function(a,b) {}
	 *	   var innerFour = (a,b) {}
	 *	   var D2 = secondary_fcn_name(a,b) {}
	 *	   var D3 = new Function("a", "b", "return a+b;");
	 * Class
	 *	   testlib.extras.ValidClassOne = function(a,b) {
	 *		   this.a = a;
	 *	   }
	 * Class Methods
	 *	   testlib.extras.ValidClassOne.prototype = {
	 *		   'validMethodOne' : function(a,b) {},
	 *		   'validMethodTwo' : function(a,b) {}
	 *	   }
     *     ValidClassTwo = function ()
     *     {
     *         this.validMethodThree = function() {}
     *         // unnamed method
     *         this.validMethodFour = () {}
     *     }
	 *	   Database.prototype.validMethodThree = Database_getTodaysDate;
	 */

	if ( is_inside_class )
		is_class = true;
	/*
	 * var can precede an inner function
	 */
	if ( isKeyword(token, KEYWORD_var) ||
		 isKeyword(token, KEYWORD_let) ||
		 isKeyword(token, KEYWORD_const) )
	{
		TRACE_PRINT("var/let/const case");
		is_const = isKeyword(token, KEYWORD_const);
		/*
		 * Only create variables for global scope
		 */
		if ( token->nestLevel == 0 )
		{
			is_global = true;
		}
		readToken(token);
	}

nextVar:
	found_this = false;
	if ( isKeyword(token, KEYWORD_this) )
	{
		TRACE_PRINT("found 'this' keyword");
		found_this = true;

		readToken(token);
		if (isType (token, TOKEN_PERIOD))
		{
			readToken(token);
		}
	}

	copyToken(name, token, true);
	indexForName = CORK_NIL;
	TRACE_PRINT("name becomes '%s' of type %s",
				vStringValue(token->string), tokenTypeName (token->type));

	while (! isType (token, TOKEN_CLOSE_CURLY) &&
	       ! isType (token, TOKEN_SEMICOLON)   &&
	       ! isType (token, TOKEN_EQUAL_SIGN)  &&
	       ! isType (token, TOKEN_COMMA)       &&
	       ! isType (token, TOKEN_EOF))
	{
		if (isType (token, TOKEN_OPEN_CURLY))
			parseBlock (token, CORK_NIL);
		else if (isKeyword (token, KEYWORD_function))
			parseFunction (token);

		/* Potentially the name of the function */
		if (isType (token, TOKEN_PERIOD))
		{
			/*
			 * Cannot be a global variable is it has dot references in the name
			 */
			is_global = false;
			/* Assume it's an assignment to a global name (e.g. a class) using
			 * its fully qualified name, so strip the scope.
			 * FIXME: resolve the scope so we can make more than an assumption. */
			token->scope = CORK_NIL;
			name->scope = CORK_NIL;
			do
			{
				readToken (token);
				if (! isType(token, TOKEN_KEYWORD))
				{
					if ( is_class )
						token->scope = indexForName;
					else
					{
						addContext (name, token);
						indexForName = CORK_NIL;
					}

					readToken (token);
				}
				else if ( isKeyword(token, KEYWORD_prototype) )
				{
					/*
					 * When we reach the "prototype" tag, we infer:
					 *     "BindAgent" is a class
					 *     "build"     is a method
					 *
					 * function BindAgent( repeatableIdName, newParentIdName ) {
					 * }
					 *
					 * CASE 1
					 * Specified function name: "build"
					 *     BindAgent.prototype.build = function( mode ) {
					 *     	  maybe parse nested functions
					 *     }
					 *
					 * CASE 2
					 * Prototype listing
					 *     ValidClassOne.prototype = {
					 *         'validMethodOne' : function(a,b) {},
					 *         'validMethodTwo' : function(a,b) {}
					 *     }
					 *
					 */
					if (! ( isType (name, TOKEN_IDENTIFIER)
						|| isType (name, TOKEN_STRING) ) )
						/*
						 * Unexpected input. Try to reset the parsing.
						 *
						 * TOKEN_STRING is acceptable. e.g.:
						 * -----------------------------------
						 * "a".prototype = function( mode ) {}
						 */
						goto cleanUp;

					indexForName = makeClassTag (name, NULL, NULL);
					is_class = true;

					/*
					 * There should a ".function_name" next.
					 */
					readToken (token);
					if (isType (token, TOKEN_PERIOD))
					{
						/*
						 * Handle CASE 1
						 */
						readToken (token);
						if (! isType(token, TOKEN_KEYWORD))
						{
							vString *const signature = vStringNew ();

							token->scope = indexForName;

							copyToken (method_body_token, token, true);
							readToken (method_body_token);

							while (! isType (method_body_token, TOKEN_SEMICOLON) &&
							       ! isType (method_body_token, TOKEN_CLOSE_CURLY) &&
							       ! isType (method_body_token, TOKEN_OPEN_CURLY) &&
							       ! isType (method_body_token, TOKEN_EOF))
							{
								if ( isType (method_body_token, TOKEN_OPEN_PAREN) )
									skipArgumentList(method_body_token, false,
													 vStringLength (signature) == 0 ? signature : NULL);
								else
									readToken (method_body_token);
							}

							int index = makeJsTag (token, JSTAG_METHOD, signature, NULL);
							vStringDelete (signature);

							if ( isType (method_body_token, TOKEN_OPEN_CURLY))
							{
								parseBlock (method_body_token, index);
								is_terminated = true;
							}
							else
								is_terminated = isType (method_body_token, TOKEN_SEMICOLON);
							goto cleanUp;
						}
					}
					else if (isType (token, TOKEN_EQUAL_SIGN))
					{
						readToken (token);
						if (isType (token, TOKEN_OPEN_CURLY))
						{
							/*
							 * Handle CASE 2
							 *
							 * Creates tags for each of these class methods
							 *     ValidClassOne.prototype = {
							 *         'validMethodOne' : function(a,b) {},
							 *         'validMethodTwo' : function(a,b) {}
							 *     }
							 */
							parseMethods(token, indexForName, false);
							/*
							 * Find to the end of the statement
							 */
							findCmdTerm (token, false, false);
							is_terminated = true;
							goto cleanUp;
						}
					}
				}
				else
					readToken (token);
			} while (isType (token, TOKEN_PERIOD));
		}
		else
			readTokenFull (token, true, NULL);

		if ( isType (token, TOKEN_OPEN_PAREN) )
			skipArgumentList(token, false, NULL);

		if ( isType (token, TOKEN_OPEN_SQUARE) )
			skipArrayList(token, false);

		/*
		if ( isType (token, TOKEN_OPEN_CURLY) )
		{
			is_class = parseBlock (token, name->string);
		}
		*/
	}

	if ( isType (token, TOKEN_CLOSE_CURLY) )
	{
		/*
		 * Reaching this section without having
		 * processed an open curly brace indicates
		 * the statement is most likely not terminated.
		 */
		is_terminated = false;
		goto cleanUp;
	}

	if ( isType (token, TOKEN_SEMICOLON) ||
	     isType (token, TOKEN_EOF) ||
	     isType (token, TOKEN_COMMA) )
	{
		/*
		 * Only create variables for global scope
		 */
		if ( token->nestLevel == 0 && is_global )
		{
			/*
			 * Handles this syntax:
			 *	   var g_var2;
			 */
			indexForName = makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
		}
		/*
		 * Statement has ended.
		 * This deals with calls to functions, like:
		 *     alert(..);
		 */
		if (isType (token, TOKEN_COMMA))
		{
			readToken (token);
			goto nextVar;
		}
		goto cleanUp;
	}

	if ( isType (token, TOKEN_EQUAL_SIGN) )
	{
		int parenDepth = 0;
		int arrowfun_paren_depth = 0;
		bool canbe_arrowfun = false;

		readToken (token);

		/* rvalue might be surrounded with parentheses */
		while (isType (token, TOKEN_OPEN_PAREN))
		{
			parenDepth++;
			arrowfun_paren_depth++;
			readToken (token);
		}

		if (isKeyword (token, KEYWORD_async))
		{
			arrowfun_paren_depth = 0;
			readToken (token);

			/* check for function signature */
			while (isType (token, TOKEN_OPEN_PAREN))
			{
				parenDepth++;
				arrowfun_paren_depth++;
				readToken (token);
			}
		}

		if ( isKeyword (token, KEYWORD_function) )
		{
			vString *const signature = vStringNew ();
			bool is_generator = false;

			readToken (token);
			if (isType (token, TOKEN_STAR))
			{
				is_generator = true;
				readToken (token);
			}

			if (! isType (token, TOKEN_KEYWORD) &&
			    ! isType (token, TOKEN_OPEN_PAREN))
			{
				/*
				 * Functions of this format:
				 *	   var D2A = function theAdd(a, b)
				 *	   {
				 *		  return a+b;
				 *	   }
				 * Are really two separate defined functions and
				 * can be referenced in two ways:
				 *	   alert( D2A(1,2) );			  // produces 3
				 *	   alert( theAdd(1,2) );		  // also produces 3
				 * So it must have two tags:
				 *	   D2A
				 *	   theAdd
				 * Save the reference to the name for later use, once
				 * we have established this is a valid function we will
				 * create the secondary reference to it.
				 */
				copyToken(secondary_name, token, true);
				readToken (token);
			}

			if ( isType (token, TOKEN_OPEN_PAREN) )
				skipArgumentList(token, false, signature);

			if (isType (token, TOKEN_OPEN_CURLY))
			{
				// This will be either a function or a class.
				if ( is_inside_class )
				{
					indexForName = makeJsTag (name, is_generator ? JSTAG_GENERATOR : JSTAG_METHOD, signature, NULL);
					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name, signature, is_generator);
				}
				else
				{
					if (! ( isType (name, TOKEN_IDENTIFIER)
					     || isType (name, TOKEN_STRING)
					     || isType (name, TOKEN_KEYWORD) ) )
					{
						/* Unexpected input. Try to reset the parsing. */
						TRACE_PRINT("Unexpected input, trying to reset");
						vStringDelete (signature);
						goto cleanUp;
					}

					indexForName = isClassName (name) ?
						makeClassTag (name, signature, NULL):
						makeFunctionTag (name, signature, is_generator);

					parseBlock (token, indexForName);

					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name, signature, is_generator);
				}
				parseBlock (token, indexForName);
			}

			vStringDelete (signature);
		}
		else if (isKeyword (token, KEYWORD_class))
		{
			is_terminated = parseES6Class (token, name);
		}
		else if (isType (token, TOKEN_OPEN_CURLY))
		{
			/*
			 * Creates tags for each of these class methods
			 *     objectOne = {
			 *         'validMethodOne' : function(a,b) {},
			 *         'validMethodTwo' : function(a,b) {}
			 *     }
			 * Or checks if this is a hash variable.
			 *     var z = {};
			 */
			bool anonObject = vStringIsEmpty (name->string);
			if (anonObject)
			{
				anonGenerate (name->string, "anonymousObject", JSTAG_VARIABLE);
				indexForName = CORK_NIL;
			}
			int p = makeSimplePlaceholder (name->string);
			has_methods = parseMethods(token, p, false);
			if (has_methods)
			{
				jsKind kind = found_this || strchr (vStringValue(name->string), '.') != NULL ? JSTAG_PROPERTY : JSTAG_VARIABLE;
				indexForName = makeJsTagCommon (name, kind, NULL, NULL, anonObject);
				moveChildren (p, indexForName);
			}
			else if ( token->nestLevel == 0 && is_global )
			{
				/*
				 * Only create variables for global scope
				 *
				 * A pointer can be created to the function.
				 * If we recognize the function/class name ignore the variable.
				 * This format looks identical to a variable definition.
				 * A variable defined outside of a block is considered
				 * a global variable:
				 *	   var g_var1 = 1;
				 *	   var g_var2;
				 * This is not a global variable:
				 *	   var g_var = function;
				 * This is a global variable:
				 *	   var g_var = different_var_name;
				 */
				indexForName = anyKindsEntryInScope (name->scope, vStringValue (name->string),
													 (int[]){JSTAG_VARIABLE, JSTAG_FUNCTION, JSTAG_CLASS}, 3, true);

				if (indexForName == CORK_NIL)
					indexForName = makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
			}
			/* Here we should be at the end of the block, on the close curly.
			 * If so, read the next token not to confuse that close curly with
			 * the end of the current statement. */
			if (isType (token, TOKEN_CLOSE_CURLY))
			{
				readTokenFull(token, true, NULL);
				is_terminated = isType (token, TOKEN_SEMICOLON);
			}
		}
		else if (isType (token, TOKEN_OPEN_SQUARE) && !vStringIsEmpty (name->string))
		{
			/*
			 * Creates tag for an array
			 */
			skipArrayList(token, true);
			jsKind kind = found_this || strchr (vStringValue(name->string), '.') != NULL ? JSTAG_PROPERTY : JSTAG_VARIABLE;
			/*
			 * Only create variables for global scope or class/object properties
			 */
			if ( ( token->nestLevel == 0 && is_global ) || kind == JSTAG_PROPERTY )
			{
				indexForName = makeJsTagCommon (name, kind, NULL, NULL, false);
			}
		}
		else if (isKeyword (token, KEYWORD_new))
		{
			readToken (token);
			is_var = isType (token, TOKEN_IDENTIFIER) || isKeyword (token, KEYWORD_capital_object);
			if ( isKeyword (token, KEYWORD_function) ||
					isKeyword (token, KEYWORD_capital_function) ||
					is_var )
			{
				if ( isKeyword (token, KEYWORD_capital_function) && isClassName (name) )
					is_class = true;

				if ( isType (token, TOKEN_IDENTIFIER) )
					skipQualifiedIdentifier (token);
				else
					readToken (token);

				if ( isType (token, TOKEN_OPEN_PAREN) )
					skipArgumentList(token, true, NULL);

				if (isType (token, TOKEN_SEMICOLON) && token->nestLevel == 0)
				{
					if ( is_var )
						indexForName = makeJsTag (name, is_const ? JSTAG_CONSTANT : found_this ? JSTAG_PROPERTY : JSTAG_VARIABLE, NULL, NULL);
					else if ( is_class )
						indexForName = makeClassTag (name, NULL, NULL);
					else
					{
						/* FIXME: we cannot really get a meaningful
						 * signature from a `new Function()` call,
						 * so for now just don't set any */
						indexForName = makeFunctionTag (name, NULL, false);
					}
				}
				else if (isType (token, TOKEN_CLOSE_CURLY))
					is_terminated = false;
			}
		}
		else if (! isType (token, TOKEN_KEYWORD) &&
				 token->nestLevel == 0 && is_global )
		{
			/*
			 * Only create variables for global scope
			 *
			 * A pointer can be created to the function.
			 * If we recognize the function/class name ignore the variable.
			 * This format looks identical to a variable definition.
			 * A variable defined outside of a block is considered
			 * a global variable:
			 *	   var g_var1 = 1;
			 *	   var g_var2;
			 * This is not a global variable:
			 *	   var g_var = function;
			 * This is a global variable:
			 *	   var g_var = different_var_name;
			 */
			indexForName = anyKindsEntryInScope (name->scope, vStringValue (name->string),
												 (int[]){JSTAG_VARIABLE, JSTAG_FUNCTION, JSTAG_CLASS}, 3, true);

			if (indexForName == CORK_NIL)
			{
				indexForName = makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
				if (isType (token, TOKEN_IDENTIFIER))
					canbe_arrowfun = true;
			}
		}
		else if ( isType (token, TOKEN_IDENTIFIER) )
		{
			canbe_arrowfun = true;
		}

		if (arrowfun_paren_depth == 0 && canbe_arrowfun)
		{
			/* var v = a => { ... } */
			vString *sig = vStringNewInit ("(");
			vStringCat (sig, token->string);
			vStringPut (sig, ')');
			readTokenFull (token, true, NULL);
			if (isType (token, TOKEN_ARROW))
			{
				if (indexForName == CORK_NIL)	// was not a global variable
					indexForName = makeFunctionTag (name, sig, false);
				else
					convertToFunction (indexForName, vStringValue (sig));
			}
			vStringDelete (sig);
		}

		if (parenDepth > 0)
		{
			/* Collect parameters for arrow function. */
			vString *sig = (arrowfun_paren_depth == 1)? makeVStringForSignature (token): NULL;

			while (parenDepth > 0 && ! isType (token, TOKEN_EOF))
			{
				if (isType (token, TOKEN_OPEN_PAREN))
				{
					parenDepth++;
					arrowfun_paren_depth++;
				}
				else if (isType (token, TOKEN_CLOSE_PAREN))
				{
					parenDepth--;
					arrowfun_paren_depth--;
				}
				readTokenFull (token, true, sig);

				/* var f = (a, b) => { ... } */
				if (arrowfun_paren_depth == 0 && isType (token, TOKEN_ARROW) && sig)
				{
					if (indexForName == CORK_NIL)	// was not a global variable
						indexForName = makeFunctionTag (name, trimGarbageInSignature (sig), false);
					else
						convertToFunction (indexForName,
										   vStringValue (trimGarbageInSignature (sig)));

					vStringDelete (sig);
					sig = NULL;
				}
			}
			if (isType (token, TOKEN_CLOSE_CURLY))
				is_terminated = false;

			vStringDelete (sig); /* NULL is acceptable. */
		}
	}
	/* if we aren't already at the cmd end, advance to it and check whether
	 * the statement was terminated */
	if (! isType (token, TOKEN_CLOSE_CURLY) &&
	    ! isType (token, TOKEN_SEMICOLON))
	{
		/*
		 * Statements can be optionally terminated in the case of
		 * statement prior to a close curly brace as in the
		 * document.write line below:
		 *
		 * function checkForUpdate() {
		 *	   if( 1==1 ) {
		 *		   document.write("hello from checkForUpdate<br>")
		 *	   }
		 *	   return 1;
		 * }
		 */
		is_terminated = findCmdTerm (token, true, true);
		/* if we're at a comma, try and read a second var */
		if (isType (token, TOKEN_COMMA))
		{
			readToken (token);
			goto nextVar;
		}
	}

cleanUp:
	token->scope = saveScope;
	deleteToken (name);
	deleteToken (secondary_name);
	deleteToken (method_body_token);

	TRACE_LEAVE();

	return is_terminated;
}

static void parseUI5 (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	/*
	 * SAPUI5 is built on top of jQuery.
	 * It follows a standard format:
	 *     sap.ui.controller("id.of.controller", {
	 *         method_name : function... {
	 *         },
	 *
	 *         method_name : function ... {
	 *         }
	 *     }
	 *
	 * Handle the parsing of the initial controller (and the
	 * same for "view") and then allow the methods to be
	 * parsed as usual.
	 */

	readToken (token);

	if (isType (token, TOKEN_PERIOD))
	{
		int r = CORK_NIL;

		readToken (token);
		while (! isType (token, TOKEN_OPEN_PAREN) &&
			   ! isType (token, TOKEN_EOF))
		{
			readToken (token);
		}
		readToken (token);

		if (isType (token, TOKEN_STRING))
		{
			copyToken(name, token, true);
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
			readToken (token);

		if (isType(name, TOKEN_STRING))
		{
			/*
			 * `name' can include '.'.
			 * Setting dynamicProp to true can prohibit
			 * that makeClassTag ispects the inside
			 * of `name'.
			 */
			name->dynamicProp = true;
			r = makeClassTag (name, NULL, NULL);
			/*
			 * TODO
			 * `name' specifies a class of OpenUI5.
			 * So tagging it as a language object of
			 * JavaScript is incorrect. We have to introduce
			 * OpenUI5 language as a subparser of JavaScript
			 * to fix this situation.
			 */
		}

		do
		{
			parseMethods (token, r, false);
		} while (! isType (token, TOKEN_CLOSE_CURLY) &&
				 ! isType (token, TOKEN_EOF));
	}

	deleteToken (name);
}

static bool parseLine (tokenInfo *const token, bool is_inside_class)
{
	TRACE_ENTER_TEXT("token is '%s' of type %s",
					 vStringValue(token->string), tokenTypeName (token->type));

	bool is_terminated = true;
	/*
	 * Detect the common statements, if, while, for, do, ...
	 * This is necessary since the last statement within a block "{}"
	 * can be optionally terminated.
	 *
	 * If the statement is not terminated, we need to tell
	 * the calling routine to prevent reading an additional token
	 * looking for the end of the statement.
	 */

	if (isType(token, TOKEN_KEYWORD))
	{
		switch (token->keyword)
		{
			case KEYWORD_for:
			case KEYWORD_while:
			case KEYWORD_do:
				is_terminated = parseLoop (token);
				break;
			case KEYWORD_if:
			case KEYWORD_else:
			case KEYWORD_try:
			case KEYWORD_catch:
			case KEYWORD_finally:
				/* Common semantics */
				is_terminated = parseIf (token);
				break;
			case KEYWORD_switch:
				parseSwitch (token);
				break;
			case KEYWORD_return:
			case KEYWORD_async:
				readToken (token);
				is_terminated = parseLine (token, is_inside_class);
				break;
			case KEYWORD_function:
				parseFunction (token);
				break;
			case KEYWORD_class:
				is_terminated = parseES6Class (token, NULL);
				break;
			default:
				is_terminated = parseStatement (token, is_inside_class);
				break;
		}
	}
	else
	{
		/*
		 * Special case where single line statements may not be
		 * SEMICOLON terminated.  parseBlock needs to know this
		 * so that it does not read the next token.
		 */
		is_terminated = parseStatement (token, is_inside_class);
	}

	TRACE_LEAVE();

	return is_terminated;
}

static void parseJsFile (tokenInfo *const token)
{
	TRACE_ENTER();

	do
	{
		readToken (token);

		if (isType (token, TOKEN_KEYWORD) && token->keyword == KEYWORD_sap)
			parseUI5 (token);
		else if (isType (token, TOKEN_KEYWORD) && (token->keyword == KEYWORD_export ||
		                                           token->keyword == KEYWORD_default))
			/* skip those at top-level */;
		else
			parseLine (token, false);
	} while (! isType (token, TOKEN_EOF));

	TRACE_LEAVE();
}

#ifdef DO_TRACING
#ifdef DO_TRACING_USE_DUMP_TOKEN
static void dumpToken (const tokenInfo *const token)
{
	const char *scopeStr = getNameStringForCorkIndex (token->scope);
	const char *scopeKindStr = getKindStringForCorkIndex (token->scope);

	if (strcmp(scopeStr, "placeholder") == 0)
	{
		TRACE_PRINT("%s: %s",
			tokenTypeName (token->type),
			vStringValue (token->string));
	}
	else
	{
		TRACE_PRINT("%s: %s (scope '%s' of kind %s)",
			tokenTypeName (token->type),
			vStringValue (token->string),
			scopeStr, scopeKindStr);
	}
}
#endif

static const char*
getNameStringForCorkIndex(int index)
{
	if (index == CORK_NIL)
		return "none";
	tagEntryInfo *e = getEntryInCorkQueue (index);
	if (e == NULL)
		return "ghost";			/* Can this happen? */

	if (e->placeholder)
		return "placeholder";

	return e->name;
}

static const char*
getKindStringForCorkIndex(int index)
{
	if (index == CORK_NIL)
		return "none";
	tagEntryInfo *e = getEntryInCorkQueue (index);
	if (e == NULL)
		return "ghost";			/* Can this happen? */

	if (e->placeholder)
		return "placeholder";

	if (e->kindIndex == KIND_GHOST_INDEX)
		return "ghost";

	return JsKinds [e->kindIndex].name;
}

static const char *kindName(jsKind kind)
{
	return ((int)kind) >= 0 ? JsKinds[kind].name : "none";
}

static const char *tokenTypeName(enum eTokenType e)
{ /* Generated by misc/enumstr.sh with cmdline:
     parsers/jscript.c eTokenType tokenTypeName */
	switch (e)
	{
		case      TOKEN_UNDEFINED: return "TOKEN_UNDEFINED";
		case            TOKEN_EOF: return "TOKEN_EOF";
		case      TOKEN_CHARACTER: return "TOKEN_CHARACTER";
		case    TOKEN_CLOSE_PAREN: return "TOKEN_CLOSE_PAREN";
		case      TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
		case          TOKEN_COLON: return "TOKEN_COLON";
		case          TOKEN_COMMA: return "TOKEN_COMMA";
		case        TOKEN_KEYWORD: return "TOKEN_KEYWORD";
		case     TOKEN_OPEN_PAREN: return "TOKEN_OPEN_PAREN";
		case     TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
		case         TOKEN_STRING: return "TOKEN_STRING";
		case TOKEN_TEMPLATE_STRING: return "TOKEN_TEMPLATE_STRING";
		case         TOKEN_PERIOD: return "TOKEN_PERIOD";
		case     TOKEN_OPEN_CURLY: return "TOKEN_OPEN_CURLY";
		case    TOKEN_CLOSE_CURLY: return "TOKEN_CLOSE_CURLY";
		case     TOKEN_EQUAL_SIGN: return "TOKEN_EQUAL_SIGN";
		case    TOKEN_OPEN_SQUARE: return "TOKEN_OPEN_SQUARE";
		case   TOKEN_CLOSE_SQUARE: return "TOKEN_CLOSE_SQUARE";
		case         TOKEN_REGEXP: return "TOKEN_REGEXP";
		case TOKEN_POSTFIX_OPERATOR: return "TOKEN_POSTFIX_OPERATOR";
		case           TOKEN_STAR: return "TOKEN_STAR";
		case         TOKEN_ATMARK: return "TOKEN_ATMARK";
		case TOKEN_BINARY_OPERATOR: return "TOKEN_BINARY_OPERATOR";
		case          TOKEN_ARROW: return "TOKEN_ARROW";
		case           TOKEN_DOTS: return "TOKEN_DOTS";
		default:                   return "UNKNOWN";
	}
}
#endif

static void initialize (const langType language)
{
	Assert (ARRAY_SIZE (JsKinds) == JSTAG_COUNT);
	Lang_js = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

static void findJsTags (void)
{
	tokenInfo *const token = newToken ();

	NextToken = NULL;
	LastTokenType = TOKEN_UNDEFINED;

	parseJsFile (token);

	deleteToken (token);

#ifdef HAVE_ICONV
	if (JSUnicodeConverter != (iconv_t) -2 && /* not created */
	    JSUnicodeConverter != (iconv_t) -1 /* creation failed */)
	{
		iconv_close (JSUnicodeConverter);
		JSUnicodeConverter = (iconv_t) -2;
	}
#endif

	Assert (NextToken == NULL);
}

/* Create parser definition structure */
extern parserDefinition* JavaScriptParser (void)
{
	// .jsx files are JSX: https://facebook.github.io/jsx/
	// which have JS function definitions, so we just use the JS parser
	static const char *const extensions [] = { "js", "jsx", "mjs", NULL };
	static const char *const aliases [] = { "js", "node", "nodejs",
	                                        "seed", "gjs",
											/* Used in PostgreSQL
											 * https://github.com/plv8/plv8 */
											"v8",
											NULL };
	parserDefinition *const def = parserNew ("JavaScript");
	def->extensions = extensions;
	def->aliases = aliases;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kindTable	= JsKinds;
	def->kindCount	= ARRAY_SIZE (JsKinds);
	def->parser		= findJsTags;
	def->initialize = initialize;
	def->finalize   = finalize;
	def->keywordTable = JsKeywordTable;
	def->keywordCount = ARRAY_SIZE (JsKeywordTable);
	def->useCork	= CORK_QUEUE|CORK_SYMTAB;
	def->requestAutomaticFQTag = true;

	return def;
}
