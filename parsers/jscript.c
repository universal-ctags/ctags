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

#include <string.h>
#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "objpool.h"
#include "trace.h"

/*
 *	 MACROS
 */
#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '$' || \
		(c) == '@' || (c) == '_' || (c) == '#')
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))

/*
 * Debugging
 *
 * Uncomment this to enable extensive debugging to stderr in jscript code.
 * Please note that TRACING_ENABLED should be #defined in main/trace.h
 * for this to work.
 *
 */
//#define JSCRIPT_DEBUGGING_ENABLED 1

#if defined(DO_TRACING) && defined(JSCRIPT_DEBUGGING_ENABLED)
	#define JSCRIPT_DO_DEBUGGING
#endif

#ifdef JSCRIPT_DO_DEBUGGING

#define JSCRIPT_DEBUG_ENTER() TRACE_ENTER()
#define JSCRIPT_DEBUG_LEAVE() TRACE_LEAVE()

#define JSCRIPT_DEBUG_ENTER_TEXT(_szFormat,...) \
	TRACE_ENTER_TEXT(_szFormat,## __VA_ARGS__)

#define JSCRIPT_DEBUG_LEAVE_TEXT(_szFormat,...) \
	TRACE_LEAVE_TEXT(_szFormat,## __VA_ARGS__)

#define JSCRIPT_DEBUG_PRINT(_szFormat,...) \
	TRACE_PRINT(_szFormat,## __VA_ARGS__)

#define JSCRIPT_DEBUG_ASSERT(_condition,_szFormat,...) \
	TRACE_ASSERT(_condition,_szFormat,## __VA_ARGS__)

#else //!JSCRIPT_DO_DEBUGGING

#define JSCRIPT_DEBUG_ENTER() do { } while(0)
#define JSCRIPT_DEBUG_LEAVE() do { } while(0)

#define JSCRIPT_DEBUG_ENTER_TEXT(_szFormat,...) do { } while(0)
#define JSCRIPT_DEBUG_LEAVE_TEXT(_szFormat,...) do { } while(0)

#define JSCRIPT_DEBUG_PRINT(_szFormat,...) do { } while(0)

#define JSCRIPT_DEBUG_ASSERT(_condition,_szFormat,...) do { } while(0)

#endif //!JSCRIPT_DO_DEBUGGING


/*
 *	 DATA DECLARATIONS
 */

/*
 * Tracks class and function names already created
 */
static stringList *ClassNames;
static stringList *FunctionNames;

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
	TOKEN_BINARY_OPERATOR
} tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
	int				nestLevel;
	bool			ignoreTag;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static tokenType LastTokenType;
static tokenInfo *NextToken;

static langType Lang_js;

static objPool *TokenPool = NULL;

typedef enum {
	JSTAG_FUNCTION,
	JSTAG_CLASS,
	JSTAG_METHOD,
	JSTAG_PROPERTY,
	JSTAG_CONSTANT,
	JSTAG_VARIABLE,
	JSTAG_GENERATOR,
	JSTAG_COUNT
} jsKind;

static kindDefinition JsKinds [] = {
	{ true,  'f', "function",	  "functions"		   },
	{ true,  'c', "class",		  "classes"			   },
	{ true,  'm', "method",		  "methods"			   },
	{ true,  'p', "property",	  "properties"		   },
	{ true,  'C', "constant",	  "constants"		   },
	{ true,  'v', "variable",	  "global variables"   },
	{ true,  'g', "generator",	  "generators"		   }
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
	{ "default",		KEYWORD_default				},
	{ "export",		KEYWORD_export				},
};

/*
 *	 FUNCTION DEFINITIONS
 */

/* Recursive functions */
static void readTokenFull (tokenInfo *const token, bool include_newlines, vString *const repr);
static void parseFunction (tokenInfo *const token);
static bool parseBlock (tokenInfo *const token, tokenInfo *const orig_parent);
static bool parseLine (tokenInfo *const token, tokenInfo *const parent, bool is_inside_class);
static void parseUI5 (tokenInfo *const token);

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);

	token->string		= vStringNew ();
	token->scope		= vStringNew ();

	return token;
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->nestLevel	= 0;
	token->ignoreTag	= false;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
	vStringClear (token->scope);
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src,
                       bool const include_non_read_info)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy(dest->string, src->string);
	if (include_non_read_info)
	{
		dest->nestLevel = src->nestLevel;
		vStringCopy(dest->scope, src->scope);
	}
}

/*
 *	 Tag generation functions
 */

static void makeJsTag (const tokenInfo *const token, const jsKind kind,
                       vString *const signature, vString *const inheritance)
{
	if (JsKinds [kind].enabled && ! token->ignoreTag )
	{
		const char *name = vStringValue (token->string);
		vString *fullscope = vStringNewCopy (token->scope);
		const char *p;
		tagEntryInfo e;

		if (kind != JSTAG_PROPERTY &&  (p = strrchr (name, '.')) != NULL )
		{
			if (vStringLength (fullscope) > 0)
				vStringPut (fullscope, '.');
			vStringNCatS (fullscope, name, (size_t) (p - name));
			name = p + 1;
		}

		initTagEntry (&e, name, &(JsKinds [kind]));

		JSCRIPT_DEBUG_PRINT("Emitting tag for symbol '%s' of kind %02x",name,kind);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;

		if ( vStringLength(fullscope) > 0 )
		{
			/* FIXME: proper parent type */
			jsKind parent_kind = JSTAG_CLASS;

			/*
			 * If we're creating a function (and not a method),
			 * guess we're inside another function
			 */
			if (kind == JSTAG_FUNCTION)
				parent_kind = JSTAG_FUNCTION;

			e.extensionFields.scopeKind = &(JsKinds [parent_kind]);
			e.extensionFields.scopeName = vStringValue (fullscope);
		}

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
				unsigned char c = (unsigned char) signature->buffer[i];
				if (c < 0x20 /* below space */ || c == 0x7F /* DEL */)
					signature->buffer[i] = ' ';
			}
			e.extensionFields.signature = vStringValue(signature);
		}

		if (inheritance)
			e.extensionFields.inheritance = vStringValue(inheritance);

		makeTagEntry (&e);
		vStringDelete (fullscope);
	}
}

static void makeClassTag (tokenInfo *const token, vString *const signature,
                          vString *const inheritance)
{
	vString *	fulltag;

	if ( ! token->ignoreTag )
	{
		fulltag = vStringNew ();
		if (vStringLength (token->scope) > 0)
		{
			vStringCopy(fulltag, token->scope);
			vStringPut (fulltag, '.');
			vStringCatS (fulltag, vStringValue(token->string));
		}
		else
		{
			vStringCopy(fulltag, token->string);
		}
		if ( ! stringListHas(ClassNames, vStringValue (fulltag)) )
		{
			stringListAdd (ClassNames, vStringNewCopy (fulltag));
			makeJsTag (token, JSTAG_CLASS, signature, inheritance);
		}
		vStringDelete (fulltag);
	}
}

static void makeFunctionTag (tokenInfo *const token, vString *const signature, bool generator)
{
	vString *	fulltag;

	if ( ! token->ignoreTag )
	{
		fulltag = vStringNew ();
		if (vStringLength (token->scope) > 0)
		{
			vStringCopy(fulltag, token->scope);
			vStringPut (fulltag, '.');
			vStringCatS (fulltag, vStringValue(token->string));
		}
		else
		{
			vStringCopy(fulltag, token->string);
		}
		if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) )
		{
			stringListAdd (FunctionNames, vStringNewCopy (fulltag));
			makeJsTag (token, generator ? JSTAG_GENERATOR : JSTAG_FUNCTION, signature, NULL);
		}
		vStringDelete (fulltag);
	}
}

/*
 *	 Parsing functions
 */

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
			if (c != '\r' && c != '\n')
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
	} while (isIdentChar (c));
	ungetcToInputFile (c);		/* unget non-identifier character */
}

static void parseTemplateString (vString *const string)
{
	int c;
	do
	{
		c = getcFromInputFile ();
		if (c == '`')
			break;
		vStringPut (string, c);
		if (c == '\\')
		{
			c = getcFromInputFile();
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

static void readTokenFull (tokenInfo *const token, bool include_newlines, vString *const repr)
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

	if (repr)
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
		case '.': token->type = TOKEN_PERIOD;				break;
		case ':': token->type = TOKEN_COLON;				break;
		case '{': token->type = TOKEN_OPEN_CURLY;			break;
		case '}': token->type = TOKEN_CLOSE_CURLY;			break;
		case '=': token->type = TOKEN_EQUAL_SIGN;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;			break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;			break;

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

		case '\\':
				  c = getcFromInputFile ();
				  if (c != '\\'  && c != '"'  &&  !isspace (c))
					  ungetcToInputFile (c);
				  token->type = TOKEN_CHARACTER;
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
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
							  repr->buffer[--repr->length] = 0;
						  if (d == '*')
						  {
							  do
							  {
								  skipToCharacterInInputFile ('*');
								  c = getcFromInputFile ();
								  if (c == '/')
									  break;
								  else
									  ungetcToInputFile (c);
							  } while (c != EOF && c != '\0');
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
		                              (t) == TOKEN_CLOSE_CURLY  || \
		                              (t) == TOKEN_OPEN_CURLY)
		/* these cannot be the start or end of a statement */
		#define IS_BINARY_OPERATOR(t) ((t) == TOKEN_EQUAL_SIGN      || \
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

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, false, NULL);
	JSCRIPT_DEBUG_PRINT("token '%s' of type %02x",vStringValue(token->string),token->type);
}

/*
 *	 Token parsing functions
 */

static void skipArgumentList (tokenInfo *const token, bool include_newlines, vString *const repr)
{
	int nest_level = 0;

	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	{
		nest_level++;
		if (repr)
			vStringPut (repr, '(');
		while (nest_level > 0 && ! isType (token, TOKEN_EOF))
		{
			readTokenFull (token, false, repr);
			if (isType (token, TOKEN_OPEN_PAREN))
				nest_level++;
			else if (isType (token, TOKEN_CLOSE_PAREN))
				nest_level--;
			else if (isKeyword (token, KEYWORD_function))
				parseFunction (token);
		}
		readTokenFull (token, include_newlines, NULL);
	}
}

static void skipArrayList (tokenInfo *const token, bool include_newlines)
{
	int nest_level = 0;

	/*
	 * Handle square brackets
	 *	 var name[1]
	 * So we must check for nested open and closing square brackets
	 */

	if (isType (token, TOKEN_OPEN_SQUARE))	/* arguments? */
	{
		nest_level++;
		while (nest_level > 0 && ! isType (token, TOKEN_EOF))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
				nest_level++;
			else if (isType (token, TOKEN_CLOSE_SQUARE))
				nest_level--;
		}
		readTokenFull (token, include_newlines, NULL);
	}
}

static void addContext (tokenInfo* const parent, const tokenInfo* const child)
{
	if (vStringLength (parent->string) > 0)
	{
		vStringPut (parent->string, '.');
	}
	vStringCatS (parent->string, vStringValue(child->string));
}

static void addToScope (tokenInfo* const token, vString* const extra)
{
	if (vStringLength (token->scope) > 0)
	{
		vStringPut (token->scope, '.');
	}
	vStringCatS (token->scope, vStringValue(extra));
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
			parseBlock (token, token);
			readTokenFull (token, include_newlines, NULL);
		}
		else if ( isType (token, TOKEN_OPEN_PAREN) )
		{
			skipArgumentList(token, include_newlines, NULL);
		}
		else if ( isType (token, TOKEN_OPEN_SQUARE) )
		{
			skipArrayList(token, include_newlines);
		}
		else
		{
			readTokenFull (token, include_newlines, NULL);
		}
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
		parseBlock (token, token);
	}
}

static bool parseLoop (tokenInfo *const token, tokenInfo *const parent)
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
		{
			skipArgumentList(token, false, NULL);
		}

		if (isType (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, parent);
		}
		else
		{
			is_terminated = parseLine(token, parent, false);
		}
	}
	else if (isKeyword (token, KEYWORD_do))
	{
		readToken(token);

		if (isType (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, parent);
		}
		else
		{
			is_terminated = parseLine(token, parent, false);
		}

		if (is_terminated)
			readToken(token);

		if (isKeyword (token, KEYWORD_while))
		{
			readToken(token);

			if (isType (token, TOKEN_OPEN_PAREN))
			{
				skipArgumentList(token, true, NULL);
			}
			if (! isType (token, TOKEN_SEMICOLON))
				is_terminated = false;
		}
	}

	return is_terminated;
}

static bool parseIf (tokenInfo *const token, tokenInfo *const parent)
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
	{
		skipArgumentList(token, false, NULL);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, parent);
	}
	else
	{
		/* The next token should only be read if this statement had its own
		 * terminator */
		read_next_token = findCmdTerm (token, true, false);
	}
	return read_next_token;
}

static void parseFunction (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	vString *const signature = vStringNew ();
	bool is_class = false;
	bool is_generator = false;

	/*
	 * This deals with these formats
	 *	   function validFunctionTwo(a,b) {}
	 *	   function * generator(a,b) {}
	 */

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
		anonGenerate (name->string, "AnonymousFunction", JSTAG_FUNCTION);
	}
	else if (!isType (name, TOKEN_IDENTIFIER))
		goto cleanUp;
	else
		readToken (token);

	/* Add scope in case this is an INNER function */
	addToScope(name, token->scope);

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
		is_class = parseBlock (token, name);
		if ( is_class )
			makeClassTag (name, signature, NULL);
		else
			makeFunctionTag (name, signature, is_generator);
	}

	findCmdTerm (token, false, false);

 cleanUp:
	vStringDelete (signature);
	deleteToken (name);
}

static bool parseBlock (tokenInfo *const token, tokenInfo *const orig_parent)
{
	JSCRIPT_DEBUG_ENTER();

	bool is_class = false;
	bool read_next_token = true;
	vString * saveScope = vStringNew ();
	tokenInfo *const parent = newToken ();

	/* backup the parent token to allow calls like parseBlock(token, token) */
	if (orig_parent)
		copyToken (parent, orig_parent, true);

	token->nestLevel++;
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
				vStringCopy(saveScope, token->scope);
				addToScope (token, parent->string);

				/*
				 * Ignore the remainder of the line
				 * findCmdTerm(token);
				 */
				read_next_token = parseLine (token, parent, is_class);

				vStringCopy(token->scope, saveScope);
			}
			else if (isKeyword (token, KEYWORD_var) ||
					 isKeyword (token, KEYWORD_let) ||
					 isKeyword (token, KEYWORD_const))
			{
				/*
				 * Potentially we have found an inner function.
				 * Set something to indicate the scope
				 */
				vStringCopy(saveScope, token->scope);
				addToScope (token, parent->string);
				read_next_token = parseLine (token, parent, is_class);
				vStringCopy(token->scope, saveScope);
			}
			else if (isType (token, TOKEN_OPEN_CURLY))
			{
				/* Handle nested blocks */
				parseBlock (token, parent);
			}
			else
			{
				/*
				 * It is possible for a line to have no terminator
				 * if the following line is a closing brace.
				 * parseLine will detect this case and indicate
				 * whether we should read an additional token.
				 */
				read_next_token = parseLine (token, parent, is_class);
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

	deleteToken (parent);
	vStringDelete(saveScope);
	token->nestLevel--;

	JSCRIPT_DEBUG_LEAVE();

	return is_class;
}

static bool parseMethods (tokenInfo *const token, const tokenInfo *const class,
                          const bool is_es6_class)
{
	JSCRIPT_DEBUG_ENTER();

	tokenInfo *const name = newToken ();
	bool has_methods = false;

	/*
	 * This deals with these formats
	 *	   validProperty  : 2,
	 *	   validMethod    : function(a,b) {}
	 *	   'validMethod2' : function(a,b) {}
     *     container.dirtyTab = {'url': false, 'title':false, 'snapshot':false, '*': false}
     *
     * ES6 methods:
     *     property(...) {}
     *     *generator() {}
     * FIXME: what to do with computed names?
     *     [property]() {}
     *     *[generator]() {}
	 */

	do
	{
		readToken (token);
		if (isType (token, TOKEN_CLOSE_CURLY))
		{
			goto cleanUp;
		}

		if (! isType (token, TOKEN_KEYWORD) &&
		    ! isType (token, TOKEN_SEMICOLON))
		{
			bool is_generator = false;
			bool is_shorthand = false; /* ES6 shorthand syntax */

			if (isType (token, TOKEN_STAR)) /* shorthand generator */
			{
				is_generator = true;
				readToken (token);
			}

			copyToken(name, token, true);

			readToken (token);
			is_shorthand = isType (token, TOKEN_OPEN_PAREN);
			if ( isType (token, TOKEN_COLON) || is_shorthand )
			{
				if (! is_shorthand)
					readToken (token);
				if ( is_shorthand || isKeyword (token, KEYWORD_function) )
				{
					JSCRIPT_DEBUG_PRINT("Seems to be a function or shorthand");
					vString *const signature = vStringNew ();

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

					if (isType (token, TOKEN_OPEN_CURLY))
					{
						has_methods = true;
						addToScope (name, class->string);
						makeJsTag (name, is_generator ? JSTAG_GENERATOR : JSTAG_METHOD, signature, NULL);
						parseBlock (token, name);

						/*
						 * If we aren't parsing an ES6 class (for which there
						 * is no mandatory separators), read to the closing
						 * curly, check next token, if a comma, we must loop
						 * again.
						 */
						if (! is_es6_class)
							readToken (token);
					}

					vStringDelete (signature);
				}
				else if (! is_es6_class)
				{
						vString * saveScope = vStringNew ();
						bool has_child_methods = false;

						/* skip whatever is the value */
						while (! isType (token, TOKEN_COMMA) &&
						       ! isType (token, TOKEN_CLOSE_CURLY) &&
						       ! isType (token, TOKEN_EOF))
						{
							if (isType (token, TOKEN_OPEN_CURLY))
							{
								/* Recurse to find child properties/methods */
								vStringCopy (saveScope, token->scope);
								addToScope (token, class->string);
								has_child_methods = parseMethods (token, name, false);
								vStringCopy (token->scope, saveScope);
								readToken (token);
							}
							else if (isType (token, TOKEN_OPEN_PAREN))
							{
								skipArgumentList (token, false, NULL);
							}
							else if (isType (token, TOKEN_OPEN_SQUARE))
							{
								skipArrayList (token, false);
							}
							else
							{
								readToken (token);
							}
						}
						vStringDelete (saveScope);

						has_methods = true;
						addToScope (name, class->string);
						if (has_child_methods)
							makeJsTag (name, JSTAG_CLASS, NULL, NULL);
						else
							makeJsTag (name, JSTAG_PROPERTY, NULL, NULL);
				}
			}
		}
	} while ( isType(token, TOKEN_COMMA) ||
	          ( is_es6_class && ! isType(token, TOKEN_EOF) ) );

	JSCRIPT_DEBUG_PRINT("Finished parsing methods");

	findCmdTerm (token, false, false);

cleanUp:
	deleteToken (name);

	JSCRIPT_DEBUG_LEAVE();

	return has_methods;
}

static bool parseES6Class (tokenInfo *const token, const tokenInfo *const parent,
                           const tokenInfo *targetName)
{
	JSCRIPT_DEBUG_ENTER();

	tokenInfo * className = newToken ();
	vString *inheritance = NULL;
	bool is_anonymous = true;

	copyToken (className, token, true);
	readToken (className);
	if (parent && ! targetName)
		addToScope (className, parent->string);

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

	JSCRIPT_DEBUG_PRINT("Emitting tag for class '%s'", vStringValue(targetName->string));

	makeJsTag (targetName, JSTAG_CLASS, NULL, inheritance);

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
		parseMethods (token, targetName, true);

	deleteToken (className);

	JSCRIPT_DEBUG_LEAVE();
	return true;
}

static bool parseStatement (tokenInfo *const token, tokenInfo *const parent, bool is_inside_class)
{
	JSCRIPT_DEBUG_ENTER();

	tokenInfo *const name = newToken ();
	tokenInfo *const secondary_name = newToken ();
	tokenInfo *const method_body_token = newToken ();
	vString * saveScope = vStringNew ();
	bool is_class = false;
	bool is_var = false;
	bool is_const = false;
	bool is_terminated = true;
	bool is_global = false;
	bool has_methods = false;
	vString *	fulltag;

	vStringClear(saveScope);
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
		JSCRIPT_DEBUG_PRINT("var/let/const case");
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
	if ( isKeyword(token, KEYWORD_this) )
	{
		JSCRIPT_DEBUG_PRINT("found 'this' keyword");

		readToken(token);
		if (isType (token, TOKEN_PERIOD))
		{
			readToken(token);
		}
	}

	copyToken(name, token, true);
	JSCRIPT_DEBUG_PRINT("name becomes '%s'",vStringValue(name->string));

	while (! isType (token, TOKEN_CLOSE_CURLY) &&
	       ! isType (token, TOKEN_SEMICOLON)   &&
	       ! isType (token, TOKEN_EQUAL_SIGN)  &&
	       ! isType (token, TOKEN_COMMA)       &&
	       ! isType (token, TOKEN_EOF))
	{
		if (isType (token, TOKEN_OPEN_CURLY))
			parseBlock (token, parent);

		/* Potentially the name of the function */
		if (isType (token, TOKEN_PERIOD))
		{
			/*
			 * Cannot be a global variable is it has dot references in the name
			 */
			is_global = false;
			do
			{
				readToken (token);
				if (! isType(token, TOKEN_KEYWORD))
				{
					if ( is_class )
					{
						addToScope(token, name->string);
					}
					else
						addContext (name, token);

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

					makeClassTag (name, NULL, NULL);
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

							vStringCopy(saveScope, token->scope);
							addToScope(token, name->string);

							readToken (method_body_token);
							vStringCopy (method_body_token->scope, token->scope);

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

							makeJsTag (token, JSTAG_METHOD, signature, NULL);
							vStringDelete (signature);

							if ( isType (method_body_token, TOKEN_OPEN_CURLY))
							{
								parseBlock (method_body_token, token);
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
							parseMethods(token, name, false);
							/*
							 * Find to the end of the statement
							 */
							findCmdTerm (token, false, false);
							token->ignoreTag = false;
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
			readToken (token);

		if ( isType (token, TOKEN_OPEN_PAREN) )
			skipArgumentList(token, false, NULL);

		if ( isType (token, TOKEN_OPEN_SQUARE) )
			skipArrayList(token, false);

		/*
		if ( isType (token, TOKEN_OPEN_CURLY) )
		{
			is_class = parseBlock (token, name);
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
			makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
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

		readToken (token);

		/* rvalue might be surrounded with parentheses */
		while (isType (token, TOKEN_OPEN_PAREN))
		{
			parenDepth++;
			readToken (token);
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
				/*
				 * This will be either a function or a class.
				 * We can only determine this by checking the body
				 * of the function.  If we find a "this." we know
				 * it is a class, otherwise it is a function.
				 */
				if ( is_inside_class )
				{
					makeJsTag (name, is_generator ? JSTAG_GENERATOR : JSTAG_METHOD, signature, NULL);
					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name, signature, is_generator);
					parseBlock (token, name);
				}
				else
				{
					if (! ( isType (name, TOKEN_IDENTIFIER)
					     || isType (name, TOKEN_STRING)
					     || isType (name, TOKEN_KEYWORD) ) )
					{
						/* Unexpected input. Try to reset the parsing. */
						JSCRIPT_DEBUG_PRINT("Unexpected input, trying to reset");
						vStringDelete (signature);
						goto cleanUp;
					}

					is_class = parseBlock (token, name);
					if ( is_class )
						makeClassTag (name, signature, NULL);
					else
						makeFunctionTag (name, signature, is_generator);

					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name, signature, is_generator);
				}
			}

			vStringDelete (signature);
		}
		else if (isKeyword (token, KEYWORD_class))
		{
			is_terminated = parseES6Class (token, name, name);
		}
		else if (isType (token, TOKEN_OPEN_CURLY))
		{
			/*
			 * Creates tags for each of these class methods
			 *     ValidClassOne.prototype = {
			 *         'validMethodOne' : function(a,b) {},
			 *         'validMethodTwo' : function(a,b) {}
			 *     }
			 * Or checks if this is a hash variable.
			 *     var z = {};
			 */
			has_methods = parseMethods(token, name, false);
			if (has_methods)
				makeJsTag (name, JSTAG_CLASS, NULL, NULL);
			else
			{
				/*
				 * Only create variables for global scope
				 */
				if ( token->nestLevel == 0 && is_global )
				{
					/*
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
					fulltag = vStringNew ();
					if (vStringLength (token->scope) > 0)
					{
						vStringCopy(fulltag, token->scope);
						vStringPut (fulltag, '.');
						vStringCatS (fulltag, vStringValue(token->string));
					}
					else
					{
						vStringCopy(fulltag, token->string);
					}
					if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) &&
							! stringListHas(ClassNames, vStringValue (fulltag)) )
					{
						makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
					}
					vStringDelete (fulltag);
				}
			}
			if (isType (token, TOKEN_CLOSE_CURLY))
			{
				/*
				 * Assume the closing parentheses terminates
				 * this statements.
				 */
				is_terminated = true;
			}
		}
		else if (isKeyword (token, KEYWORD_new))
		{
			readToken (token);
			is_var = isType (token, TOKEN_IDENTIFIER);
			if ( isKeyword (token, KEYWORD_function) ||
					isKeyword (token, KEYWORD_capital_function) ||
					isKeyword (token, KEYWORD_capital_object) ||
					is_var )
			{
				if ( isKeyword (token, KEYWORD_capital_object) )
					is_class = true;

				readToken (token);
				if ( isType (token, TOKEN_OPEN_PAREN) )
					skipArgumentList(token, true, NULL);

				if (isType (token, TOKEN_SEMICOLON))
				{
					if ( token->nestLevel == 0 )
					{
						if ( is_var )
						{
							makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
						}
						else
						{
							if ( is_class )
							{
								makeClassTag (name, NULL, NULL);
							} else {
								/* FIXME: we cannot really get a meaningful
								 * signature from a `new Function()` call,
								 * so for now just don't set any */
								makeFunctionTag (name, NULL, false);
							}
						}
					}
				}
				else if (isType (token, TOKEN_CLOSE_CURLY))
					is_terminated = false;
			}
		}
		else if (! isType (token, TOKEN_KEYWORD))
		{
			/*
			 * Only create variables for global scope
			 */
			if ( token->nestLevel == 0 && is_global )
			{
				/*
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
				fulltag = vStringNew ();
				if (vStringLength (token->scope) > 0)
				{
					vStringCopy(fulltag, token->scope);
					vStringPut (fulltag, '.');
					vStringCatS (fulltag, vStringValue(token->string));
				}
				else
				{
					vStringCopy(fulltag, token->string);
				}
				if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) &&
						! stringListHas(ClassNames, vStringValue (fulltag)) )
				{
					makeJsTag (name, is_const ? JSTAG_CONSTANT : JSTAG_VARIABLE, NULL, NULL);
				}
				vStringDelete (fulltag);
			}
		}

		if (parenDepth > 0)
		{
			while (parenDepth > 0 && ! isType (token, TOKEN_EOF))
			{
				if (isType (token, TOKEN_OPEN_PAREN))
					parenDepth++;
				else if (isType (token, TOKEN_CLOSE_PAREN))
					parenDepth--;
				readTokenFull (token, true, NULL);
			}
			if (isType (token, TOKEN_CLOSE_CURLY))
				is_terminated = false;
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
	vStringCopy(token->scope, saveScope);
	deleteToken (name);
	deleteToken (secondary_name);
	deleteToken (method_body_token);
	vStringDelete(saveScope);

	JSCRIPT_DEBUG_LEAVE();

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

		do
		{
			parseMethods (token, name, false);
		} while (! isType (token, TOKEN_CLOSE_CURLY) &&
				 ! isType (token, TOKEN_EOF));
	}

	deleteToken (name);
}

static bool parseLine (tokenInfo *const token, tokenInfo *const parent, bool is_inside_class)
{
	JSCRIPT_DEBUG_ENTER_TEXT("token is %s",vStringValue(token->string));

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
				is_terminated = parseLoop (token, parent);
				break;
			case KEYWORD_if:
			case KEYWORD_else:
			case KEYWORD_try:
			case KEYWORD_catch:
			case KEYWORD_finally:
				/* Common semantics */
				is_terminated = parseIf (token, parent);
				break;
			case KEYWORD_switch:
				parseSwitch (token);
				break;
			case KEYWORD_return:
				readToken (token);
				is_terminated = parseLine (token, parent, is_inside_class);
				break;
			case KEYWORD_function:
			{
				vString *saveScope = vStringNewCopy (token->scope);
				if (parent)
					addToScope (token, parent->string);
				parseFunction (token);
				vStringCopy (token->scope, saveScope);
				vStringDelete (saveScope);
				break;
			}
			case KEYWORD_class:
				is_terminated = parseES6Class (token, parent, NULL);
				break;
			default:
				is_terminated = parseStatement (token, parent, is_inside_class);
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
		is_terminated = parseStatement (token, parent, is_inside_class);
	}

	JSCRIPT_DEBUG_LEAVE();

	return is_terminated;
}

static void parseJsFile (tokenInfo *const token)
{
	JSCRIPT_DEBUG_ENTER();

	do
	{
		readToken (token);

		if (isType (token, TOKEN_KEYWORD) && token->keyword == KEYWORD_sap)
			parseUI5 (token);
		else if (isType (token, TOKEN_KEYWORD) && (token->keyword == KEYWORD_export ||
		                                           token->keyword == KEYWORD_default))
			/* skip those at top-level */;
		else
			parseLine (token, NULL, false);
	} while (! isType (token, TOKEN_EOF));

	JSCRIPT_DEBUG_LEAVE();
}

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
	ClassNames = stringListNew ();
	FunctionNames = stringListNew ();
	LastTokenType = TOKEN_UNDEFINED;

	parseJsFile (token);

	stringListDelete (ClassNames);
	stringListDelete (FunctionNames);
	ClassNames = NULL;
	FunctionNames = NULL;
	deleteToken (token);

	Assert (NextToken == NULL);
}

/* Create parser definition structure */
extern parserDefinition* JavaScriptParser (void)
{
	// .jsx files are JSX: https://facebook.github.io/jsx/
	// which have JS function definitions, so we just use the JS parser
	static const char *const extensions [] = { "js", "jsx", NULL };
	static const char *const aliases [] = { "js", "node", "nodejs",
	                                        "seed", "gjs", NULL };
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

	return def;
}
