/*
 *	 Copyright (c) 2019, Karol Samborski
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating tags for TypeScript language
 *	 files.
 *
 *	 Reference: https://github.com/Microsoft/TypeScript/blob/master/doc/TypeScript%20Language%20Specification.pdf
 *
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include "parse.h"
#include "objpool.h"
#include "keyword.h"
#include "read.h"

/*
 *	DATA DEFINITIONS
 */
static langType Lang_ts;
static objPool *TokenPool = NULL;

/*	Used to specify type of keyword.
*/
enum eKeywordId {
	KEYWORD_break,
	KEYWORD_case,
	KEYWORD_catch,
	KEYWORD_class,
	KEYWORD_const,
	KEYWORD_continue,
	KEYWORD_debugger,
	KEYWORD_default,
	KEYWORD_delete,
	KEYWORD_do,
	KEYWORD_else,
	KEYWORD_enum,
	KEYWORD_export,
	KEYWORD_extends,
	KEYWORD_false,
	KEYWORD_finally,
	KEYWORD_for,
	KEYWORD_function,
	KEYWORD_if,
	KEYWORD_implements,
	KEYWORD_import,
	KEYWORD_in,
	KEYWORD_instanceof,
	KEYWORD_interface,
	KEYWORD_let,
	KEYWORD_module,
	KEYWORD_namespace,
	KEYWORD_new,
	KEYWORD_null,
	KEYWORD_package,
	KEYWORD_private,
	KEYWORD_protected,
	KEYWORD_public,
	KEYWORD_return,
	KEYWORD_static,
	KEYWORD_super,
	KEYWORD_switch,
	KEYWORD_this,
	KEYWORD_throw,
	KEYWORD_true,
	KEYWORD_try,
	KEYWORD_type,
	KEYWORD_typeof,
	KEYWORD_var,
	KEYWORD_void,
	KEYWORD_while,
	KEYWORD_with,
	KEYWORD_yield
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
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
} tokenInfo;

static const keywordTable TsKeywordTable [] = {
	/* keyword		keyword ID */
	{ "break"      , KEYWORD_break      },
	{ "case"       , KEYWORD_case       },
	{ "catch"      , KEYWORD_catch      },
	{ "class"      , KEYWORD_class      },
	{ "const"      , KEYWORD_const      },
	{ "continue"   , KEYWORD_continue   },
	{ "debugger"   , KEYWORD_debugger   },
	{ "default"    , KEYWORD_default    },
	{ "delete"     , KEYWORD_delete     },
	{ "do"         , KEYWORD_do         },
	{ "else"       , KEYWORD_else       },
	{ "enum"       , KEYWORD_enum       },
	{ "export"     , KEYWORD_export     },
	{ "extends"    , KEYWORD_extends    },
	{ "false"      , KEYWORD_false      },
	{ "finally"    , KEYWORD_finally    },
	{ "for"        , KEYWORD_for        },
	{ "function"   , KEYWORD_function   },
	{ "if"         , KEYWORD_if         },
	{ "implements" , KEYWORD_implements },
	{ "import"     , KEYWORD_import     },
	{ "in"         , KEYWORD_in         },
	{ "instanceof" , KEYWORD_instanceof },
	{ "interface"  , KEYWORD_interface  },
	{ "let"        , KEYWORD_let        },
	{ "module"     , KEYWORD_module     },
	{ "namespace"  , KEYWORD_namespace  },
	{ "new"        , KEYWORD_new        },
	{ "null"       , KEYWORD_null       },
	{ "package"    , KEYWORD_package    },
	{ "private"    , KEYWORD_private    },
	{ "protected"  , KEYWORD_protected  },
	{ "public"     , KEYWORD_public     },
	{ "return"     , KEYWORD_return     },
	{ "static"     , KEYWORD_static     },
	{ "super"      , KEYWORD_super      },
	{ "switch"     , KEYWORD_switch     },
	{ "this"       , KEYWORD_this       },
	{ "throw"      , KEYWORD_throw      },
	{ "true"       , KEYWORD_true       },
	{ "try"        , KEYWORD_try        },
	{ "type"       , KEYWORD_type       },
	{ "typeof"     , KEYWORD_typeof     },
	{ "var"        , KEYWORD_var        },
	{ "void"       , KEYWORD_void       },
	{ "while"      , KEYWORD_while      },
	{ "with"       , KEYWORD_with       },
	{ "yield"      , KEYWORD_yield      }
};

static kindDefinition TsKinds [] = {
	{ true,  'f', "function",	  "functions"		   },
	{ true,  'c', "class",		  "classes"			   },
	{ true,  'i', "interface",	  "interfaces"		   },
	{ true,  'e', "enum",		  "enums"			   },
	{ true,  'm', "method",		  "methods"			   },
	{ true,  'p', "property",	  "properties"		   },
	{ true,  'C', "constant",	  "constants"		   },
	{ true,  'v', "variable",	  "global variables"   },
	{ true,  'g', "generator",	  "generators"		   },
	{ true,	 'a', "alias",		  "aliases",		   }
};

static void findTsTags (void)
{
}

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->string = vStringNew ();

	return token;
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	vStringDelete (token->string);
	eFree (token);
}

static void initialize (const langType language)
{
	Lang_ts = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

/* Create parser definition structure */
extern parserDefinition* TypeScriptParser (void)
{
	static const char *const extensions [] = { "ts", NULL };
	parserDefinition *const def = parserNew ("TypeScript");
	def->extensions = extensions;
	def->kindTable	= TsKinds;
	def->kindCount	= ARRAY_SIZE (TsKinds);
	def->parser		= findTsTags;
	def->initialize = initialize;
	def->finalize   = finalize;
	def->keywordTable = TsKeywordTable;
	def->keywordCount = ARRAY_SIZE (TsKeywordTable);

	return def;
}
