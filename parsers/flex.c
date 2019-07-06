/*
 *	 Copyright (c) 2008, David Fishburn
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating tags for Adobe languages.
 *	 There are a number of different ones, but this will begin with:
 *	     Flex
 *	         MXML files (*.mMacromedia XML)
 *	         ActionScript files (*.as)
 *
 *	 The ActionScript code was copied from the JavaScript parser, with some
 *	 adaptations e.g. for classes and type specifiers.
 *
 *	 Flex 3 language reference
 *		 http://livedocs.adobe.com/flex/3/langref/index.html
 * 		 https://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/language-elements.html
 * 		 https://www.adobe.com/devnet/actionscript/learning/as3-fundamentals/packages.html
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include <ctype.h>	/* to define isalpha () */
#ifdef DEBUG
#include <stdio.h>
#endif

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "strlist.h"

/*
 *	 MACROS
 */
#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define isEOF(token) (isType ((token), TOKEN_EOF))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '$' || \
		(c) == '@' || (c) == '_' || (c) == '#' || \
		(c) >= 0x80)

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
	KEYWORD_object,
	KEYWORD_capital_object,
	KEYWORD_prototype,
	KEYWORD_var,
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
	KEYWORD_return,
	KEYWORD_public,
	KEYWORD_private,
	KEYWORD_protected,
	KEYWORD_internal,
	KEYWORD_final,
	KEYWORD_native,
	KEYWORD_dynamic,
	KEYWORD_class,
	KEYWORD_interface,
	KEYWORD_package,
	KEYWORD_extends,
	KEYWORD_static,
	KEYWORD_implements,
	KEYWORD_get,
	KEYWORD_set,
	KEYWORD_import,
	KEYWORD_id,
	KEYWORD_name,
	KEYWORD_script,
	KEYWORD_cdata,
	KEYWORD_mx,
	KEYWORD_fx,
	KEYWORD_override
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
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_EQUAL_SIGN,
	TOKEN_EXCLAMATION,
	TOKEN_FORWARD_SLASH,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_OPEN_MXML,
	TOKEN_CLOSE_MXML,
	TOKEN_CLOSE_SGML,
	TOKEN_LESS_THAN,
	TOKEN_GREATER_THAN,
	TOKEN_QUESTION_MARK,
	TOKEN_OPEN_NAMESPACE,
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
	bool			isClass;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */
static tokenType LastTokenType;
static tokenInfo *NextToken;

static langType Lang_flex;

typedef enum {
	FLEXTAG_FUNCTION,
	FLEXTAG_CLASS,
	FLEXTAG_INTERFACE,
	FLEXTAG_PACKAGE,
	FLEXTAG_METHOD,
	FLEXTAG_PROPERTY,
	FLEXTAG_VARIABLE,
	FLEXTAG_LOCALVAR,
	FLEXTAG_CONST,
	FLEXTAG_IMPORT,
	FLEXTAG_MXTAG,
	FLEXTAG_COUNT
} flexKind;

typedef enum {
	FLEX_IMPORT_ROLE_IMPORTED,
} flexImportRole;

static roleDefinition FlexImportRoles [] = {
	{ true, "import", "imports" },
};

static kindDefinition FlexKinds [] = {
	{ true,  'f', "function",	  "functions"		   },
	{ true,  'c', "class",		  "classes"			   },
	{ true,  'i', "interface",	  "interfaces"		   },
	{ true,  'P', "package",	  "packages"		   },
	{ true,  'm', "method",		  "methods"			   },
	{ true,  'p', "property",	  "properties"		   },
	{ true,  'v', "variable",	  "global variables"   },
	{ false, 'l', "localvar",	  "local variables"   },
	{ true,  'C', "constant",	  "constants"		   },
	{ true,  'I', "import",		  "imports",
	  .referenceOnly = true, ATTACH_ROLES (FlexImportRoles) },
	{ true,  'x', "mxtag",		  "mxtags" 			   }
};

/*	Used to determine whether keyword is valid for the token language and
 *	what its ID is.
 */
static const keywordTable FlexKeywordTable [] = {
	/* keyword		keyword ID */
	{ "function",	KEYWORD_function			},
	{ "Function",	KEYWORD_capital_function	},
	{ "object",		KEYWORD_object				},
	{ "Object",		KEYWORD_capital_object		},
	{ "prototype",	KEYWORD_prototype			},
	{ "var",		KEYWORD_var					},
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
	{ "return",		KEYWORD_return				},
	{ "public",		KEYWORD_public				},
	{ "private",	KEYWORD_private				},
	{ "protected",	KEYWORD_protected			},
	{ "internal",	KEYWORD_internal			},
	{ "final",		KEYWORD_final				},
	{ "native",		KEYWORD_native				},
	{ "dynamic",	KEYWORD_dynamic				},
	{ "class",		KEYWORD_class				},
	{ "interface",	KEYWORD_interface			},
	{ "package",	KEYWORD_package				},
	{ "extends",	KEYWORD_extends				},
	{ "static",		KEYWORD_static				},
	{ "implements",	KEYWORD_implements			},
	{ "get",		KEYWORD_get					},
	{ "set",		KEYWORD_set					},
	{ "import",		KEYWORD_import				},
	{ "id",			KEYWORD_id					},
	{ "name",		KEYWORD_name				},
	{ "script",		KEYWORD_script				},
	{ "cdata",		KEYWORD_cdata				},
	{ "mx",			KEYWORD_mx					},
	{ "fx",			KEYWORD_fx					},
	{ "override",	KEYWORD_override			}
};

/*
 *	 FUNCTION DEFINITIONS
 */

/* Recursive functions */
static void parseFunction (tokenInfo *const token);
static bool parseBlock (tokenInfo *const token, const vString *const parentScope);
static bool parseLine (tokenInfo *const token);
static bool parseActionScript (tokenInfo *const token, bool readNext);
static bool parseMXML (tokenInfo *const token);

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= vStringNew ();
	token->nestLevel	= 0;
	token->isClass		= false;
	token->ignoreTag	= false;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src,
                       bool const include_non_read_info)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	dest->isClass = src->isClass;
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

static vString *buildQualifiedName (const tokenInfo *const token)
{
	vString *qualified = vStringNew ();

	if (vStringLength (token->scope) > 0)
	{
		vStringCopy (qualified, token->scope);
		vStringPut (qualified, '.');
	}
	vStringCat (qualified, token->string);

	return qualified;
}

static void makeConstTag (tokenInfo *const token, const flexKind kind)
{
	if (FlexKinds [kind].enabled && ! token->ignoreTag )
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		int role = ROLE_DEFINITION_INDEX;

		if (kind == FLEXTAG_IMPORT)
			role = FLEX_IMPORT_ROLE_IMPORTED;

		initRefTagEntry (&e, name, kind, role);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;

		if ( vStringLength(token->scope) > 0 )
		{
			/* FIXME: proper parent type */
			flexKind parent_kind = FLEXTAG_CLASS;

			/*
			 * If we're creating a function (and not a method),
			 * guess we're inside another function
			 */
			if (kind == FLEXTAG_FUNCTION)
				parent_kind = FLEXTAG_FUNCTION;
			/* mxtags can only be nested inside other mxtags */
			else if (kind == FLEXTAG_MXTAG)
				parent_kind = kind;

			e.extensionFields.scopeKindIndex = parent_kind;
			e.extensionFields.scopeName = vStringValue (token->scope);
		}

		makeTagEntry (&e);

		/* make qualified tags for compatibility if requested */
		if (isXtagEnabled (XTAG_QUALIFIED_TAGS))
		{
			vString *qualified = buildQualifiedName (token);

			markTagExtraBit (&e, XTAG_QUALIFIED_TAGS);
			e.name = vStringValue (qualified);
			makeTagEntry (&e);
			vStringDelete (qualified);
		}
	}
}

static void makeFlexTag (tokenInfo *const token, flexKind kind)
{
	if (FlexKinds [kind].enabled && ! token->ignoreTag )
	{
	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n makeFlexTag start: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
		if (kind == FLEXTAG_FUNCTION && token->isClass )
		{
			kind = FLEXTAG_METHOD;
		}
		makeConstTag (token, kind);
	}
}

static void makeClassTag (tokenInfo *const token)
{
	if ( ! token->ignoreTag )
	{
		vString *fulltag = buildQualifiedName (token);

		if ( ! stringListHas(ClassNames, vStringValue (fulltag)) )
		{
			stringListAdd (ClassNames, vStringNewCopy (fulltag));
			makeFlexTag (token, FLEXTAG_CLASS);
		}
		vStringDelete (fulltag);
	}
}

static void makeMXTag (tokenInfo *const token)
{
	if ( ! token->ignoreTag )
	{
		makeFlexTag (token, FLEXTAG_MXTAG);
	}
}

static void makeFunctionTag (tokenInfo *const token)
{
	if ( ! token->ignoreTag )
	{
		vString *fulltag = buildQualifiedName (token);

		if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) )
		{
			stringListAdd (FunctionNames, vStringNewCopy (fulltag));
			makeFlexTag (token, FLEXTAG_FUNCTION);
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
			c = getcFromInputFile(); /* This maybe a ' or ". */
			vStringPut(string, c);
		}
		else if (c == delimiter)
			end = true;
		else
			vStringPut (string, c);
	}
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

static void readTokenFull (tokenInfo *const token, bool include_newlines)
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
		case '?': token->type = TOKEN_QUESTION_MARK;		break;

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
							  (d != '/') &&		/* is a one line comment? */
							  (d != '>') )		/* is this a close XML tag? */
					  {
						  ungetcToInputFile (d);
						  token->type = TOKEN_FORWARD_SLASH;
						  token->lineNumber = getInputLineNumber ();
						  token->filePosition = getInputFilePosition ();
					  }
					  else
					  {
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
						  else if (d == '>')	/* is this the start of a comment?  */
						  {
							  token->type = TOKEN_CLOSE_SGML;
							  token->lineNumber = getInputLineNumber ();
							  token->filePosition = getInputFilePosition ();
						  }
					  }
					  break;
				  }

		case '<':
				  {
					  /*
					   * An XML comment looks like this
					   *   <!-- anything over multiple lines -->
					   */
					  int d = getcFromInputFile ();

					  if ( (d != '!' )  && 		/* is this the start of a comment? */
					       (d != '/' )  &&	 	/* is this the start of a closing mx tag */
					       (d != 'm' )  &&  	/* is this the start of a mx tag */
					       (d != 'f' )  &&  	/* is this the start of a fx tag */
					       (d != 's' )    ) 	/* is this the start of a spark tag */
					  {
						  ungetcToInputFile (d);
						  token->type = TOKEN_LESS_THAN;
						  token->lineNumber = getInputLineNumber ();
						  token->filePosition = getInputFilePosition ();
						  break;
					  }
					  else
					  {
						  if (d == '!')
						  {
							  int e = getcFromInputFile ();
							  if ( e != '-' ) 		/* is this the start of a comment? */
							  {
								  ungetcToInputFile (e);
								  ungetcToInputFile (d);
								  token->type = TOKEN_LESS_THAN;
								  token->lineNumber = getInputLineNumber ();
								  token->filePosition = getInputFilePosition ();
							  }
							  else
							  {
								  if (e == '-')
								  {
									  int f = getcFromInputFile ();
									  if ( f != '-' ) 		/* is this the start of a comment? */
									  {
										  ungetcToInputFile (f);
										  ungetcToInputFile (e);
										  ungetcToInputFile (d);
										  token->type = TOKEN_LESS_THAN;
										  token->lineNumber = getInputLineNumber ();
										  token->filePosition = getInputFilePosition ();
									  }
									  else
									  {
										  if (f == '-')
										  {
											  do
											  {
												  skipToCharacterInInputFile ('-');
												  c = getcFromInputFile ();
												  if (c == '-')
												  {
													  d = getcFromInputFile ();
													  if (d == '>')
														  break;
													  else
													  {
														  ungetcToInputFile (d);
														  ungetcToInputFile (c);
													  }
													  break;
												  }
												  else
													  ungetcToInputFile (c);
											  } while (c != EOF && c != '\0');
											  goto getNextChar;
										  }
									  }
								  }
							  }
						  }
						  else if (d == 'm' || d == 'f' || d == 's' )
						  {
							  int e = getcFromInputFile ();
							  if ( (d == 'm' || d == 'f') && e != 'x' ) 		/* continuing an mx or fx tag */
							  {
								  ungetcToInputFile (e);
								  ungetcToInputFile (d);
								  token->type = TOKEN_LESS_THAN;
								  token->lineNumber = getInputLineNumber ();
								  token->filePosition = getInputFilePosition ();
								  break;
							  }
							  else
							  {
								  if ( (d == 'm' || d == 'f') && e == 'x' )
								  {
									  int f = getcFromInputFile ();
									  if ( f != ':' ) 		/* start of the tag */
									  {
										  ungetcToInputFile (f);
										  ungetcToInputFile (e);
										  ungetcToInputFile (d);
										  token->type = TOKEN_LESS_THAN;
										  token->lineNumber = getInputLineNumber ();
										  token->filePosition = getInputFilePosition ();
										  break;
									  }
									  else
									  {
										  token->type = TOKEN_OPEN_MXML;
										  token->lineNumber = getInputLineNumber ();
										  token->filePosition = getInputFilePosition ();
										  break;
									  }
								  }
								  if ( d == 's' && e == ':')    /* continuing a spark tag */
								  {
									  token->type = TOKEN_OPEN_MXML;
									  token->lineNumber = getInputLineNumber ();
									  token->filePosition = getInputFilePosition ();
									  break;
								  }
								  else
								  {
									  ungetcToInputFile (e);
									  ungetcToInputFile (d);
									  token->type = TOKEN_LESS_THAN;
									  token->lineNumber = getInputLineNumber ();
									  token->filePosition = getInputFilePosition ();
									  break;
								  }
							  }
						  }
						  else if (d == '/')
						  {
							  int e = getcFromInputFile ();
							  if ( !(e == 'm' || e == 'f' || e == 's' ))
							  {
								  ungetcToInputFile (e);
								  ungetcToInputFile (d);
								  token->type = TOKEN_LESS_THAN;
								  token->lineNumber = getInputLineNumber ();
								  token->filePosition = getInputFilePosition ();
								  break;
							  }
							  else
							  {
								  int f = getcFromInputFile ();
								  if ( (e == 'm' || e == 'f') && f != 'x' ) 		/* continuing an mx or fx tag */
								  {
									  ungetcToInputFile (f);
									  ungetcToInputFile (e);
									  token->type = TOKEN_LESS_THAN;
									  token->lineNumber = getInputLineNumber ();
									  token->filePosition = getInputFilePosition ();
									  break;
								  }
								  else
								  {
									  if (f == 'x')
									  {
										  int g = getcFromInputFile ();
										  if ( g != ':' ) 		/* is this the start of a comment? */
										  {
											  ungetcToInputFile (g);
											  ungetcToInputFile (f);
											  ungetcToInputFile (e);
											  token->type = TOKEN_LESS_THAN;
											  token->lineNumber = getInputLineNumber ();
											  token->filePosition = getInputFilePosition ();
											  break;
										  }
										  else
										  {
											  token->type = TOKEN_CLOSE_MXML;
											  token->lineNumber = getInputLineNumber ();
											  token->filePosition = getInputFilePosition ();
											  break;
										  }
									  }
									  if ( e == 's' && f == ':')    /* continuing a spark tag */
									  {
										  token->type = TOKEN_CLOSE_MXML;
										  token->lineNumber = getInputLineNumber ();
										  token->filePosition = getInputFilePosition ();
										  break;
									  }
									  else
									  {
										  ungetcToInputFile (f);
										  ungetcToInputFile (e);
										  token->type = TOKEN_LESS_THAN;
										  token->lineNumber = getInputLineNumber ();
										  token->filePosition = getInputFilePosition ();
										  break;
									  }
								  }
							  }
						  }
					  }
					  break;
				  }

		case '>':
				  token->type = TOKEN_GREATER_THAN;
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '!':
				  token->type = TOKEN_EXCLAMATION;
				  /*token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();*/
				  break;

		default:
				  if (! isIdentChar (c))
					  token->type = TOKEN_UNDEFINED;
				  else
				  {
					  parseIdentifier (token->string, c);
					  token->lineNumber = getInputLineNumber ();
					  token->filePosition = getInputFilePosition ();
					  token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_flex);
					  if (isKeyword (token, KEYWORD_NONE))
						  token->type = TOKEN_IDENTIFIER;
					  else
						  token->type = TOKEN_KEYWORD;
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
		                               (t) == TOKEN_COLON           || \
		                               (t) == TOKEN_PERIOD          || \
		                               (t) == TOKEN_STAR            || \
		                               (t) == TOKEN_FORWARD_SLASH   || \
		                               (t) == TOKEN_QUESTION_MARK   || \
		                               (t) == TOKEN_LESS_THAN       || \
		                               (t) == TOKEN_GREATER_THAN    || \
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
		}

		#undef IS_STMT_SEPARATOR
		#undef IS_BINARY_OPERATOR
	}

	LastTokenType = token->type;
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, false);
}

/*
 *	 Token parsing functions
 */

static void skipArgumentList (tokenInfo *const token, bool include_newlines)
{
	int nest_level = 0;

	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	{
		nest_level++;
		while (nest_level > 0 && ! isType (token, TOKEN_EOF))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
				nest_level++;
			else if (isType (token, TOKEN_CLOSE_PAREN))
				nest_level--;
		}
		readTokenFull (token, include_newlines);
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
		readTokenFull (token, include_newlines);
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

static void addToScope (tokenInfo* const token, const vString* const extra)
{
	if (vStringLength (token->scope) > 0)
	{
		vStringPut (token->scope, '.');
	}
	vStringCat (token->scope, extra);
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
			parseBlock (token, NULL);
			readTokenFull (token, include_newlines);
		}
		else if ( isType (token, TOKEN_OPEN_PAREN) )
		{
			skipArgumentList(token, include_newlines);
		}
		else if ( isType (token, TOKEN_OPEN_SQUARE) )
		{
			skipArrayList(token, include_newlines);
		}
		else
		{
			readTokenFull (token, include_newlines);
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
		skipArgumentList(token, false);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, NULL);
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
		{
			skipArgumentList(token, false);
		}

		if (isType (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, NULL);
		}
		else
		{
			is_terminated = parseLine(token);
		}
	}
	else if (isKeyword (token, KEYWORD_do))
	{
		readToken(token);

		if (isType (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, NULL);
		}
		else
		{
			is_terminated = parseLine(token);
		}

		if (is_terminated)
			readToken(token);

		if (isKeyword (token, KEYWORD_while))
		{
			readToken(token);

			if (isType (token, TOKEN_OPEN_PAREN))
			{
				skipArgumentList(token, true);
			}
			if (! isType (token, TOKEN_SEMICOLON))
			{
				/* oddly enough, `do {} while (0) var foo = 42` is perfectly
				 * valid AS, so explicitly handle the remaining of the line
				 * for the sake of the root scope handling (as parseActionScript()
				 * always advances a token not to ever get stuck) */
				is_terminated = parseLine(token);
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
	{
		skipArgumentList(token, false);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		parseBlock (token, NULL);
	}
	else
	{
		/* The next token should only be read if this statement had its own
		 * terminator */
		read_next_token = findCmdTerm (token, true, false);
	}
	return read_next_token;
}

static bool parseImport (tokenInfo *const token)
{
	if (! isKeyword (token, KEYWORD_import))
		return false;

	readToken (token);

	if (isType (token, TOKEN_IDENTIFIER))
	{
		tokenInfo *const name = newToken ();

		copyToken (name, token, true);
		readToken (token);
		while (isType (token, TOKEN_PERIOD))
		{
			vStringPut (name->string, '.');
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				vStringCat (name->string, token->string);
			else if (isType (token, TOKEN_STAR))
				vStringPut (name->string, '*');
			if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_STAR))
				readToken (token);
		}

		makeFlexTag (name, FLEXTAG_IMPORT);
		deleteToken (name);
	}

	return isType (token, TOKEN_SEMICOLON);
}

static void parseFunction (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	flexKind kind = FLEXTAG_FUNCTION;

	/*
	 * This deals with these formats
     *     private static function ioErrorHandler( event:IOErrorEvent ):void {
     *     public function get prop():String {}
     *     public function set prop(param:String):void {}
	 */

	if ( isKeyword(token, KEYWORD_function) )
	{
		readToken (token);
	}

	/* getter and setter */
	if (isKeyword (token, KEYWORD_get) ||
		isKeyword (token, KEYWORD_set))
	{
		kind = FLEXTAG_PROPERTY;
		readToken (token);
	}

	copyToken (name, token, true);
	/* Add scope in case this is an INNER function
	addToScope(name, token->scope);
	*/

	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseFunction: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseFunction: name isClass:%d  scope:%s  name:%s\n"
				, name->isClass
				, vStringValue(name->scope)
				, vStringValue(name->string)
				);
			);

	readToken (token);

	if ( isType (token, TOKEN_OPEN_PAREN) )
		skipArgumentList(token, false);

	if ( isType (token, TOKEN_COLON) )
	{
		/*
		 *   function fname ():ReturnType
		 */
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			readToken (token);
	}

	if ( isType (token, TOKEN_OPEN_CURLY) )
	{
		DebugStatement (
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end: name isClass:%d  scope:%s  name:%s\n"
					, name->isClass
					, vStringValue(name->scope)
					, vStringValue(name->string)
					);
				);
		parseBlock (token, name->string);
		DebugStatement (
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end2: token isClass:%d  scope:%s  name:%s\n"
					, token->isClass
					, vStringValue(token->scope)
					, vStringValue(token->string)
					);
				);
		DebugStatement (
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end2: token isClass:%d  scope:%s  name:%s\n"
					, token->isClass
					, vStringValue(token->scope)
					, vStringValue(token->string)
					);
				);
		DebugStatement (
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end3: name isClass:%d  scope:%s  name:%s\n"
					, name->isClass
					, vStringValue(name->scope)
					, vStringValue(name->string)
					);
				);
		if (kind == FLEXTAG_FUNCTION)
			makeFunctionTag (name);
		else
			makeFlexTag (name, kind);
	}

	findCmdTerm (token, false, false);

	deleteToken (name);
}

/* Parses a block surrounded by curly braces.
 * @p parentScope is the scope name for this block, or NULL for unnamed scopes */
static bool parseBlock (tokenInfo *const token, const vString *const parentScope)
{
	bool read_next_token = true;
	vString * saveScope = vStringNew ();

	vStringCopy (saveScope, token->scope);
	if (parentScope)
	{
		addToScope (token, parentScope);
		token->nestLevel++;
	}

	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseBlock start: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
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
			if (isType (token, TOKEN_OPEN_CURLY))
			{
				/* Handle nested blocks */
				parseBlock (token, NULL);
			}
			else
			{
				/*
				 * It is possible for a line to have no terminator
				 * if the following line is a closing brace.
				 * parseLine will detect this case and indicate
				 * whether we should read an additional token.
				 */
				read_next_token = parseLine (token);
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

	vStringCopy(token->scope, saveScope);
	vStringDelete(saveScope);
	if (parentScope)
		token->nestLevel--;

	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseBlock end: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	return false;
}

static void parseMethods (tokenInfo *const token, const tokenInfo *const class)
{
	tokenInfo *const name = newToken ();
	vString *saveScope = vStringNew ();

	vStringCopy (saveScope, token->scope);
	addToScope (token, class->string);

	/*
	 * This deals with these formats
	 *	   validProperty  : 2,
	 *	   validMethod    : function(a,b) {}
	 *	   'validMethod2' : function(a,b) {}
     *     container.dirtyTab = {'url': false, 'title':false, 'snapshot':false, '*': false}
	 */

	do
	{
		readToken (token);
		if (isType (token, TOKEN_CLOSE_CURLY))
		{
			goto cleanUp;
		}

		if (isType (token, TOKEN_STRING) || isKeyword(token, KEYWORD_NONE))
		{
			copyToken (name, token, true);

			readToken (token);
			if ( isType (token, TOKEN_COLON) )
			{
				readToken (token);
				if ( isKeyword (token, KEYWORD_function) )
				{
					readToken (token);
					if ( isType (token, TOKEN_OPEN_PAREN) )
					{
						skipArgumentList(token, false);
					}

					if (isType (token, TOKEN_OPEN_CURLY))
					{
						makeFlexTag (name, FLEXTAG_METHOD);
						parseBlock (token, name->string);

						/*
						 * Read to the closing curly, check next
						 * token, if a comma, we must loop again
						 */
						readToken (token);
					}
				}
				else
				{
						makeFlexTag (name, FLEXTAG_PROPERTY);

						/*
						 * Read the next token, if a comma
						 * we must loop again
						 */
						readToken (token);
				}
			}
		}
	} while ( isType(token, TOKEN_COMMA));

	findCmdTerm (token, false, false);

cleanUp:
	vStringCopy (token->scope, saveScope);
	vStringDelete (saveScope);
	deleteToken (name);
}

static bool parseVar (tokenInfo *const token, bool is_public)
{
	tokenInfo *const name = newToken ();
	bool is_terminated = true;
	flexKind kind = is_public ? FLEXTAG_VARIABLE : FLEXTAG_LOCALVAR;

	/*
	 * Variables are defined as:
	 *     private static var lastFaultMessage:Date = new Date( 0 );
	 *     private static var webRequests:ArrayCollection = new ArrayCollection();
	 */

	if ( isKeyword(token, KEYWORD_var) )
	{
		readToken(token);
	}
	else if (isKeyword(token, KEYWORD_const))
	{
		kind = FLEXTAG_CONST;
		readToken(token);
	}

	/* Variable name */
	copyToken (name, token, true);
	readToken(token);

	if ( isType (token, TOKEN_COLON) )
	{
		/*
		 *   var vname ():DataType = new Date();
		 *   var vname ():DataType;
		 */
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			readToken (token);
	}

	is_terminated = findCmdTerm (token, true, false);

	if ( isType (token, TOKEN_SEMICOLON) )
	{
		makeFlexTag (name, kind);
	}

	deleteToken (name);

	return is_terminated;
}

static void parsePackage (tokenInfo *const token)
{
	tokenInfo *name = NULL;

	if (isKeyword (token, KEYWORD_package))
		readToken(token);

	/* name is optional and can be qualified */
	if (isType (token, TOKEN_IDENTIFIER))
	{
		name = newToken ();
		copyToken (name, token, true);
		readToken (token);

		while (isType (token, TOKEN_PERIOD))
		{
			vStringPut (name->string, '.');
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				vStringCat (name->string, token->string);
				readToken (token);
			}
		}
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		if (name)
			makeFlexTag (name, FLEXTAG_PACKAGE);
		parseBlock (token, name ? name->string : NULL);
	}

	if (name)
		deleteToken (name);
}

static bool parseClass (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	bool saveIsClass = token->isClass;

	/*
	 * Variables are defined as:
	 *     private static var lastFaultMessage:Date = new Date( 0 );
	 *     private static var webRequests:ArrayCollection = new ArrayCollection();
	 */

	if ( isKeyword(token, KEYWORD_class) )
	{
		readToken(token);
	}

	token->isClass = true;
	/* Class name */
	copyToken (name, token, true);
	readToken(token);

	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseClass start: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);

	if (isKeyword (token, KEYWORD_extends))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			readToken (token);
	}

	if (isKeyword (token, KEYWORD_implements))
	{
		do
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}
		while (isType (token, TOKEN_COMMA));
	}

	if ( isType (token, TOKEN_OPEN_CURLY) )
	{
		makeClassTag (name);
		parseBlock (token, name->string);
	}

	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseClass end: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	token->isClass = saveIsClass;
	deleteToken (name);

	return true;
}

static void parseInterface (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	bool saveIsClass = token->isClass;

	if (isKeyword(token, KEYWORD_interface))
		readToken(token);

	token->isClass = true;
	/* interface name */
	copyToken (name, token, true);
	readToken (token);

	/* interfaces can extend multiple interfaces */
	if (isKeyword (token, KEYWORD_extends))
	{
		do
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}
		while (isType (token, TOKEN_COMMA));
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		makeFlexTag (name, FLEXTAG_INTERFACE);
		parseBlock (token, name->string);
	}

	token->isClass = saveIsClass;
	deleteToken (name);
}

static bool parseStatement (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	tokenInfo *const secondary_name = newToken ();
	vString * saveScope = vStringNew ();
	bool is_public = false;
	bool is_class = false;
	bool is_terminated = true;
	bool is_global = false;
	/* bool is_prototype = false; */
	vString *	fulltag;

	vStringCopy (saveScope, token->scope);
	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n parseStatement: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
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

	/* skip attributes */
	while (isKeyword (token, KEYWORD_public) ||
	       isKeyword (token, KEYWORD_protected) ||
	       isKeyword (token, KEYWORD_private) ||
	       isKeyword (token, KEYWORD_override) ||
	       isKeyword (token, KEYWORD_static) ||
	       isKeyword (token, KEYWORD_internal) ||
	       isKeyword (token, KEYWORD_native) ||
	       isKeyword (token, KEYWORD_dynamic) ||
	       isKeyword (token, KEYWORD_final))
	{
		if (isKeyword(token, KEYWORD_public))
			is_public = true;

		readToken (token);
	}

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
			case KEYWORD_package:
				parsePackage (token);
				goto cleanUp;
				break;
			case KEYWORD_class:
				parseClass (token);
				goto cleanUp;
				break;
			case KEYWORD_interface:
				parseInterface (token);
				goto cleanUp;
				break;
			case KEYWORD_function:
				parseFunction (token);
				goto cleanUp;
				break;
			case KEYWORD_var:
			case KEYWORD_const:
				is_terminated = parseVar (token, is_public);
				goto cleanUp;
				break;
			default:
				readToken(token);
				break;
		}
	}

nextVar:
	copyToken (name, token, true);

	while (! isType (token, TOKEN_CLOSE_CURLY) &&
	       ! isType (token, TOKEN_SEMICOLON)   &&
	       ! isType (token, TOKEN_EQUAL_SIGN)  &&
	       ! isType (token, TOKEN_COMMA)       &&
	       ! isType (token, TOKEN_EOF))
	{
		if (isType (token, TOKEN_OPEN_CURLY))
			parseBlock (token, NULL);

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
			vStringClear (token->scope);
			vStringClear (name->scope);
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
					 *     	  ignore everything within this function
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
					makeClassTag (name);
					is_class = true;
					/* is_prototype = true; */

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
							addToScope(token, name->string);

							makeFlexTag (token, FLEXTAG_METHOD);
							/*
							 * We can read until the end of the block / statement.
							 * We need to correctly parse any nested blocks, but
							 * we do NOT want to create any tags based on what is
							 * within the blocks.
							 */
							token->ignoreTag = true;
							/*
							 * Find to the end of the statement
							 */
							findCmdTerm (token, false, false);
							token->ignoreTag = false;
							is_terminated = true;
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
							parseMethods(token, name);
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
			readTokenFull (token, true);

		if ( isType (token, TOKEN_OPEN_PAREN) )
			skipArgumentList(token, false);

		if ( isType (token, TOKEN_COLON) )
		{
			/*
			 * Functions are of this form:
			 *   function fname ():ReturnType {
			 */
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
				readToken (token);
		}

		if ( isType (token, TOKEN_OPEN_SQUARE) )
			skipArrayList(token, false);
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
			makeFlexTag (name, FLEXTAG_VARIABLE);
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
		readToken (token);

		if ( isKeyword (token, KEYWORD_function) )
		{
			readToken (token);

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
				copyToken (secondary_name, token, true);
				readToken (token);
			}

			if ( isType (token, TOKEN_OPEN_PAREN) )
				skipArgumentList(token, false);

			if (isType (token, TOKEN_OPEN_CURLY))
			{
				/*
				 * This will be either a function or a class.
				 * We can only determine this by checking the body
				 * of the function.  If we find a "this." we know
				 * it is a class, otherwise it is a function.
				 */
				if ( token->isClass )
				{
					makeFlexTag (name, FLEXTAG_METHOD);
					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name);
					parseBlock (token, name->string);
				}
				else
				{
					parseBlock (token, name->string);
					makeFunctionTag (name);

					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name);
				}
			}
		}
		else if (isType (token, TOKEN_OPEN_PAREN))
		{
			/*
			 * Handle nameless functions
			 *     this.method_name = () {}
			 */
			skipArgumentList(token, false);

			if (isType (token, TOKEN_OPEN_CURLY))
			{
				/*
				 * Nameless functions are only setup as methods.
				 */
				makeFlexTag (name, FLEXTAG_METHOD);
				parseBlock (token, name->string);
			}
		}
		else if (isType (token, TOKEN_OPEN_CURLY))
		{
			/*
			 * Creates tags for each of these class methods
			 *     ValidClassOne.prototype = {
			 *         'validMethodOne' : function(a,b) {},
			 *         'validMethodTwo' : function(a,b) {}
			 *     }
			 */
			parseMethods(token, name);
			/* Here we should be at the end of the block, on the close curly.
			 * If so, read the next token not to confuse that close curly with
			 * the end of the current statement. */
			if (isType (token, TOKEN_CLOSE_CURLY))
			{
				readTokenFull(token, true);
				is_terminated = isType (token, TOKEN_SEMICOLON);
			}
		}
		else if (isKeyword (token, KEYWORD_new))
		{
			readToken (token);
			if ( isKeyword (token, KEYWORD_function) ||
					isKeyword (token, KEYWORD_capital_function) ||
					isKeyword (token, KEYWORD_object) ||
					isKeyword (token, KEYWORD_capital_object) )
			{
				if ( isKeyword (token, KEYWORD_object) ||
						isKeyword (token, KEYWORD_capital_object) )
					is_class = true;

				readToken (token);
				if ( isType (token, TOKEN_OPEN_PAREN) )
					skipArgumentList(token, true);

				if (isType (token, TOKEN_SEMICOLON))
				{
					if ( token->nestLevel == 0 )
					{
						if ( is_class )
						{
							makeClassTag (name);
						} else {
							makeFunctionTag (name);
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
					vStringCat (fulltag, token->string);
				}
				else
				{
					vStringCopy(fulltag, token->string);
				}
				if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) &&
						! stringListHas(ClassNames, vStringValue (fulltag)) )
				{
					makeFlexTag (name, FLEXTAG_VARIABLE);
				}
				vStringDelete (fulltag);
			}
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
	vStringDelete(saveScope);

	return is_terminated;
}

static bool parseLine (tokenInfo *const token)
{
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
				readToken (token);
				is_terminated = parseLine (token);
				break;
			case KEYWORD_function:
				parseFunction (token);
				break;
			case KEYWORD_import:
				is_terminated = parseImport (token);
				/* to properly support unterminated imports at top level,
				 * recurse here because parseActionScript() will *always*
				 * advance to avoid ever getting stuck. */
				if (! is_terminated)
					return parseLine (token);
				break;
			default:
				is_terminated = parseStatement (token);
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
		is_terminated = parseStatement (token);
	}
	return is_terminated;
}

static bool parseCDATA (tokenInfo *const token)
{
	if (isType (token, TOKEN_LESS_THAN))
	{
		/*
		 * Handle these tags
		 * <![CDATA[
		 *    ...
		 * ]]>
		 */
		readToken (token);
		if (isType (token, TOKEN_EXCLAMATION))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
			{
				readToken (token);
				if (isKeyword (token, KEYWORD_cdata))
				{
					readToken (token);
					if (isType (token, TOKEN_OPEN_SQUARE))
					{
						parseActionScript (token, true);
						if (isType (token, TOKEN_CLOSE_SQUARE))
						{
							readToken (token);
							if (isType (token, TOKEN_CLOSE_SQUARE))
							{
								readToken (token);
							}
						}
					}
				}
			}
		}
	}
	else
	{
		parseActionScript (token, false);
	}
	return true;
}

static bool parseNamespace (tokenInfo *const token)
{
	/*
	 * If we have found a <, we know it is not a TOKEN_OPEN_MXML
	 * but it could potentially be a different namespace.
	 * This means it will also have a closing tag, which will
	 * mess up the parser if we do not properly recurse
	 * through these tags.
	 */

	if (isType (token, TOKEN_LESS_THAN))
	{
		readToken (token);
	}

	/*
	 * Check if we have reached a other namespace tag
	 *   <views:Object ... />
	 * or
	 *   <views:Object ... >
	 *   </views:Object>
	 */
	if (isType (token, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isType (token, TOKEN_COLON))
		{
			readToken (token);
			if ( ! isType (token, TOKEN_IDENTIFIER))
			{
				return true;
			}
		}
		else
		{
			return true;
		}
	}
	else
	{
		return true;
	}

	/*
	 * Confirmed we are inside a namespace tag, so
	 * process it until the close tag.
	 *
	 * But also check for new tags, which will either
	 * be recursive namespaces or MXML tags
	 */
	do
	{
		if (isType (token, TOKEN_LESS_THAN))
		{
			parseNamespace (token);
			readToken (token);
		}
		if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
		}
		else
		{
			readToken (token);
		}
	} while (! (isType (token, TOKEN_CLOSE_SGML) ||
		    isType (token, TOKEN_CLOSE_MXML) ||
		    isEOF (token)) );
	return true;
}

static bool parseMXML (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	tokenInfo *const type = newToken ();
	bool inside_attributes = true;
	/*
	 * Detect the common statements, if, while, for, do, ...
	 * This is necessary since the last statement within a block "{}"
	 * can be optionally terminated.
	 *
	 * If the statement is not terminated, we need to tell
	 * the calling routine to prevent reading an additional token
	 * looking for the end of the statement.
	 */

	readToken (token);

	if (isKeyword (token, KEYWORD_script))
	{
		/*
		 * These tags can be of this form:
		 * <mx:Script src="filename.as" />
		 */
		do
		{
			readToken (token);
		} while (! (isType (token, TOKEN_CLOSE_SGML)   ||
			    isType (token, TOKEN_CLOSE_MXML)   ||
			    isType (token, TOKEN_GREATER_THAN) ||
			    isEOF (token)) );

		if (isType (token, TOKEN_CLOSE_MXML))
		{
			/*
			 * We have found a </mx:type> tag
			 * Finish reading the "type" and ">"
			 */
			readToken (token);
			readToken (token);
			goto cleanUp;
		}
		if (isType (token, TOKEN_CLOSE_SGML))
		{
			/*
			 * We have found a <mx:Script src="filename.as" />
			 */
			goto cleanUp;
		}

		/*
		 * This is a beginning of an embedded script.
		 * These typically are of this format:
		 *    <mx:Script>
		 *        <![CDATA[
		 *        ... ActionScript ...
		 *        ]]>
		 *    </mx:Script>
		 */
		readToken (token);
		parseCDATA (token);

		readToken (token);
		if (isType (token, TOKEN_CLOSE_MXML))
		{
			/*
			 * We have found a </mx:type> tag
			 * Finish reading the "type" and ">"
			 */
			readToken (token);
			readToken (token);
		}
		goto cleanUp;
	}

	copyToken (type, token, true);

	readToken (token);
	do
	{
		if (isType (token, TOKEN_GREATER_THAN))
		{
			inside_attributes = false;
		}
		if (isType (token, TOKEN_LESS_THAN))
		{
			parseNamespace (token);
			readToken (token);
		}
		else if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
			readToken (token);
		}
		else if (inside_attributes && (isKeyword (token, KEYWORD_id) || isKeyword (token, KEYWORD_name)))
		{
			if (vStringLength(name->string) == 0 )
			{
				/*
				 * If we have already created the tag based on either "name"
				 * or "id" do not do it again.
				 */
				readToken (token);
				readToken (token);

				copyToken (name, token, true);
				addToScope (name, type->string);
				makeMXTag (name);
			}
			else
			{
				readToken (token);
			}
		}
		else
		{
			readToken (token);
		}
	} while (! (isType (token, TOKEN_CLOSE_SGML) ||
		    isType (token, TOKEN_CLOSE_MXML) ||
		    isEOF (token)) );

	if (isType (token, TOKEN_CLOSE_MXML))
	{
		/*
		 * We have found a </mx:type> tag
		 * Finish reading the "type" and ">"
		 */
		readToken (token);
		readToken (token);
	}

cleanUp:
	deleteToken (name);
	deleteToken (type);
	return true;
}

static bool parseActionScript (tokenInfo *const token, bool readNext)
{
	LastTokenType = TOKEN_UNDEFINED;

	do
	{
		if (! readNext)
			readNext = true;
		else
			readToken (token);

		if (isType (token, TOKEN_LESS_THAN))
		{
			/*
			 * Handle these tags
			 * <![CDATA[
			 *    ...
			 * ]]>
			 */
			readToken (token);
			if (isType (token, TOKEN_EQUAL_SIGN))
			{
				if (isType (token, TOKEN_OPEN_SQUARE))
				{
					readToken (token);
					if (isKeyword (token, KEYWORD_cdata))
					{
						readToken (token);
					}
				}
			}
		}
		if (isType (token, TOKEN_CLOSE_SQUARE))
		{
			/*
			 * Handle these tags
			 * <![CDATA[
			 *    ...
			 * ]]>
			 */
			readToken (token);
			if (isType (token, TOKEN_CLOSE_SQUARE))
			{
				readToken (token);
				if (isType (token, TOKEN_GREATER_THAN))
				{
					return true;
				}
			}
		}
		else if (isType (token, TOKEN_CLOSE_MXML))
		{
			/*
			 * Read the Script> tags
			 */
			readToken (token);
			readToken (token);
			return true;
		}
		else if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
		}
		else
		{
			parseLine (token);
		}
	} while (!isEOF (token));
	return true;
}

static void parseFlexFile (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
		}
		else if (isType (token, TOKEN_LESS_THAN))
		{
			readToken (token);
			if (isType (token, TOKEN_QUESTION_MARK))
			{
				/*
				 * <?xml version="1.0" encoding="utf-8"?>
				 */
				readToken (token);
				while (! (isType (token, TOKEN_QUESTION_MARK) || isEOF (token)))
				{
					readToken (token);
				}
				readToken (token);
			}
			else if (isKeyword (token, KEYWORD_NONE))
			{
				/*
				 * This is a simple XML tag, read until the closing statement
				 * <something .... >
				 * </something>
				 */
				readToken (token);
				while (! (isType (token, TOKEN_GREATER_THAN) || isEOF (token)))
				{
					readToken (token);
				}
			}
		}
		else
		{
			parseActionScript (token, false);
		}
	} while (!isEOF (token));
}

static void initialize (const langType language)
{
	Assert (ARRAY_SIZE (FlexKinds) == FLEXTAG_COUNT);
	Lang_flex = language;
}

static void findFlexTags (void)
{
	tokenInfo *const token = newToken ();

	NextToken = NULL;
	ClassNames = stringListNew ();
	FunctionNames = stringListNew ();

	parseFlexFile (token);

	stringListDelete (ClassNames);
	stringListDelete (FunctionNames);
	ClassNames = NULL;
	FunctionNames = NULL;
	deleteToken (token);
}

/* Create parser definition structure */
extern parserDefinition* FlexParser (void)
{
	static const char *const extensions [] = { "as", "mxml", NULL };
	parserDefinition *const def = parserNew ("Flex");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kindTable	= FlexKinds;
	def->kindCount	= ARRAY_SIZE (FlexKinds);
	def->parser		= findFlexTags;
	def->initialize = initialize;
	def->keywordTable = FlexKeywordTable;
	def->keywordCount = ARRAY_SIZE (FlexKeywordTable);

	return def;
}
