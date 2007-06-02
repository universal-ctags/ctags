/*
 *	$Id$
 *
 *	Copyright (c) 2002-2003, Darren Hiebert
 *
 *	This source code is released for free distribution under the terms of the
 *	GNU General Public License.
 *
 *	This module contains functions for generating tags for PL/SQL language
 *	files.
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include <ctype.h>	/* to define isalpha () */
#include <setjmp.h>
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

/*
 *	On-line PL/SQL Reference Guide:
 *	http://info-it.umsystem.edu/oradocs/doc/server/doc/PLS23/toc.htm
 *
 *	Sample PL/SQL code is available from:
 *	http://www.orafaq.com/faqscrpt.htm#GENPLSQL
 *
 *	On-line SQL Anywhere Documentation
 *	http://www.ianywhere.com/developer/product_manuals/sqlanywhere/index.html
 */

/*
 *	 MACROS
 */
#define isType(token,t)		(boolean) ((token)->type == (t))
#define isKeyword(token,k)	(boolean) ((token)->keyword == (k))

/*
 *	 DATA DECLARATIONS
 */

typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

/*
 * Used to specify type of keyword.
 */
typedef enum eKeywordId {
	KEYWORD_NONE = -1,
	KEYWORD_is,
	KEYWORD_begin,
	KEYWORD_body,
	KEYWORD_cursor,
	KEYWORD_declare,
	KEYWORD_end,
	KEYWORD_function,
	KEYWORD_if,
	KEYWORD_loop,
	KEYWORD_case,
	KEYWORD_for,
	KEYWORD_call,
	KEYWORD_package,
	KEYWORD_pragma,
	KEYWORD_procedure,
	KEYWORD_record,
	KEYWORD_object,
	KEYWORD_ref,
	KEYWORD_rem,
	KEYWORD_return,
	KEYWORD_returns,
	KEYWORD_subtype,
	KEYWORD_table,
	KEYWORD_trigger,
	KEYWORD_type,
	KEYWORD_index,
	KEYWORD_event,
	KEYWORD_publication,
	KEYWORD_service,
	KEYWORD_domain,
	KEYWORD_datatype,
	KEYWORD_result,
	KEYWORD_when,
	KEYWORD_then,
	KEYWORD_variable,
	KEYWORD_exception,
	KEYWORD_at,
	KEYWORD_on,
	KEYWORD_primary,
	KEYWORD_references,
	KEYWORD_unique,
	KEYWORD_check,
	KEYWORD_constraint,
	KEYWORD_foreign,
	KEYWORD_ml_table,
	KEYWORD_ml_table_lang,
	KEYWORD_ml_conn,
	KEYWORD_ml_conn_lang,
	KEYWORD_local,
	KEYWORD_temporary,
	KEYWORD_drop,
	KEYWORD_view,
	KEYWORD_synonym,
	KEYWORD_handler,
	KEYWORD_comment,
	KEYWORD_go
} keywordId;

/*
 * Used to determine whether keyword is valid for the token language and
 *	what its ID is.
 */
typedef struct sKeywordDesc {
	const char *name;
	keywordId id;
} keywordDesc;

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_BLOCK_LABEL_BEGIN,
	TOKEN_BLOCK_LABEL_END,
	TOKEN_CHARACTER,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COMMA,
	TOKEN_IDENTIFIER,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_OPERATOR,
	TOKEN_OTHER,
	TOKEN_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_TILDE,
	TOKEN_FORWARD_SLASH
} tokenType;

typedef struct sTokenInfo {
	tokenType	type;
	keywordId	keyword;
	vString *	string;
	vString *	scope;
	unsigned long lineNumber;
	fpos_t filePosition;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_sql;

static jmp_buf Exception;

typedef enum {
	SQLTAG_CURSOR,
	SQLTAG_PROTOTYPE,
	SQLTAG_FUNCTION,
	SQLTAG_FIELD,
	SQLTAG_LOCAL_VARIABLE,
	SQLTAG_BLOCK_LABEL,
	SQLTAG_PACKAGE,
	SQLTAG_PROCEDURE,
	SQLTAG_RECORD,
	SQLTAG_SUBTYPE,
	SQLTAG_TABLE,
	SQLTAG_TRIGGER,
	SQLTAG_VARIABLE,
	SQLTAG_INDEX,
	SQLTAG_EVENT,
	SQLTAG_PUBLICATION,
	SQLTAG_SERVICE,
	SQLTAG_DOMAIN,
	SQLTAG_VIEW,
	SQLTAG_SYNONYM,
	SQLTAG_MLTABLE,
	SQLTAG_MLCONN,
	SQLTAG_COUNT
} sqlKind;

static kindOption SqlKinds [] = {
	{ TRUE,  'c', "cursor",		  "cursors"				   },
	{ FALSE, 'd', "prototype",	  "prototypes"			   },
	{ TRUE,  'f', "function",	  "functions"			   },
	{ TRUE,  'F', "field",		  "record fields"		   },
	{ FALSE, 'l', "local",		  "local variables"		   },
	{ TRUE,  'L', "label",		  "block label"			   },
	{ TRUE,  'P', "package",	  "packages"			   },
	{ TRUE,  'p', "procedure",	  "procedures"			   },
	{ FALSE, 'r', "record",		  "records"				   },
	{ TRUE,  's', "subtype",	  "subtypes"			   },
	{ TRUE,  't', "table",		  "tables"				   },
	{ TRUE,  'T', "trigger",	  "triggers"			   },
	{ TRUE,  'v', "variable",	  "variables"			   },
	{ TRUE,  'i', "index",		  "indexes"				   },
	{ TRUE,  'e', "event",		  "events"				   },
	{ TRUE,  'U', "publication",  "publications"		   },
	{ TRUE,  'R', "service",	  "services"			   },
	{ TRUE,  'D', "domain",		  "domains"				   },
	{ TRUE,  'V', "view",		  "views"				   },
	{ TRUE,  'n', "synonym",	  "synonyms"			   },
	{ TRUE,  'x', "mltable",	  "MobiLink Table Scripts" },
	{ TRUE,  'y', "mlconn",		  "MobiLink Conn Scripts"  }
};

static const keywordDesc SqlKeywordTable [] = {
	/* keyword		keyword ID */
	{ "as",								KEYWORD_is				},
	{ "begin",							KEYWORD_begin			},
	{ "body",							KEYWORD_body			},
	{ "cursor",							KEYWORD_cursor			},
	{ "declare",						KEYWORD_declare			},
	{ "end",							KEYWORD_end				},
	{ "function",						KEYWORD_function		},
	{ "if",								KEYWORD_if				},
	{ "is",								KEYWORD_is				},
	{ "loop",							KEYWORD_loop			},
	{ "case",							KEYWORD_case			},
	{ "for",							KEYWORD_for				},
	{ "call",							KEYWORD_call			},
	{ "package",						KEYWORD_package			},
	{ "pragma",							KEYWORD_pragma			},
	{ "procedure",						KEYWORD_procedure		},
	{ "record",							KEYWORD_record			},
	{ "object",							KEYWORD_object			},
	{ "ref",							KEYWORD_ref				},
	{ "rem",							KEYWORD_rem				},
	{ "return",							KEYWORD_return			},
	{ "returns",						KEYWORD_returns			},
	{ "subtype",						KEYWORD_subtype			},
	{ "table",							KEYWORD_table			},
	{ "trigger",						KEYWORD_trigger			},
	{ "type",							KEYWORD_type			},
	{ "index",							KEYWORD_index			},
	{ "event",							KEYWORD_event			},
	{ "publication",					KEYWORD_publication		},
	{ "service",						KEYWORD_service			},
	{ "result",							KEYWORD_result			},
	{ "when",							KEYWORD_when			},
	{ "then",							KEYWORD_then			},
	{ "variable",						KEYWORD_variable		},
	{ "exception",						KEYWORD_exception		},
	{ "at",								KEYWORD_at				},
	{ "on",								KEYWORD_on				},
	{ "primary",						KEYWORD_primary			},
	{ "references",						KEYWORD_references		},
	{ "unique",							KEYWORD_unique			},
	{ "check",							KEYWORD_check			},
	{ "constraint",						KEYWORD_constraint		},
	{ "foreign",						KEYWORD_foreign			},
	{ "ml_add_table_script",			KEYWORD_ml_table		},
	{ "ml_add_lang_table_script",		KEYWORD_ml_table_lang	},
	{ "ml_add_connection_script",		KEYWORD_ml_conn			},
	{ "ml_add_lang_connection_script",	KEYWORD_ml_conn_lang	},
	{ "local",							KEYWORD_local			},
	{ "temporary",						KEYWORD_temporary		},
	{ "drop",							KEYWORD_drop			},
	{ "view",							KEYWORD_view			},
	{ "synonym",						KEYWORD_synonym			},
	{ "handler",						KEYWORD_handler			},
	{ "comment",						KEYWORD_comment			},
	{ "go",								KEYWORD_go				}
};

/*
 *	 FUNCTION DECLARATIONS
 */

/* Recursive calls */
static void parseBlock (tokenInfo *const token, const boolean local);

/*
 *	 FUNCTION DEFINITIONS
 */

static boolean isIdentChar1 (const int c)
{
	/*
	 * Other databases are less restrictive on the first character of
	 * an identifier.
	 * isIdentChar1 is used to identify the first character of an 
	 * identifier, so we are removing some restrictions.
	 */
	return (boolean)
		(isalpha (c) || c == '@' || c == '_' );
}

static boolean isIdentChar (const int c)
{
	return (boolean)
		(isalpha (c) || isdigit (c) || c == '$' || 
		 c == '@' || c == '_' || c == '#');
}

static boolean isCmdTerm (tokenInfo *const token)
{
#ifdef DEBUGed
	printf( "\n isCmdTerm: token same  tt:%d  tk:%d\n"
			, token->type
			, token->keyword
		  );
#endif

	/*
	 * Based on the various customer sites I have been at
	 * the most common command delimiters are
	 *	   ;
	 *	   ~
	 *	   /
	 *	   go
	 * This routine will check for any of these, more
	 * can easily be added by modifying readToken and
	 * either adding the character to:
	 *	   enum eTokenType
	 *	   enum eTokenType
	 */
	return ( isType (token, TOKEN_SEMICOLON) || 
			isType (token, TOKEN_TILDE) || 
			isType (token, TOKEN_FORWARD_SLASH) || 
			isKeyword (token, KEYWORD_go) );
}

static void buildSqlKeywordHash (void)
{
	const size_t count = sizeof (SqlKeywordTable) /
		sizeof (SqlKeywordTable [0]);
	size_t i;
	for (i = 0	;  i < count  ;  ++i)
	{
		const keywordDesc* const p = &SqlKeywordTable [i];
		addKeyword (p->name, Lang_sql, (int) p->id);
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= vStringNew ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

/*
 *	 Tag generation functions
 */

static void makeConstTag (tokenInfo *const token, const sqlKind kind)
{
	if (SqlKinds [kind].enabled)
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;
		e.kindName	   = SqlKinds [kind].name;
		e.kind		   = SqlKinds [kind].letter;

		makeTagEntry (&e);
	}
}

static void makeSqlTag (tokenInfo *const token, const sqlKind kind)
{
	vString *	fulltag;

	if (SqlKinds [kind].enabled)
	{
		/*
		 * If a scope has been added to the token, change the token
		 * string to include the scope when making the tag.
		 */
		if ( vStringLength(token->scope) > 0 )
		{
			fulltag = vStringNew ();
			vStringCopy(fulltag, token->scope);
			vStringCatS (fulltag, ".");
			vStringCatS (fulltag, vStringValue(token->string));
			vStringTerminate(fulltag);
			vStringCopy(token->string, fulltag);
			vStringDelete (fulltag);
		}
		makeConstTag (token, kind);
	}
}

/*
 *	 Parsing functions
 */

static int skipToCharacter (const int c)
{
	int d;
	do
	{
		d = fileGetc ();
	} while (d != EOF  &&  d != c);
	return d;
}

static void parseString (vString *const string, const int delimiter)
{
	boolean end = FALSE;
	int c;
	while (! end)
	{
		c = fileGetc ();
		if (c == EOF)
			end = TRUE;
		else if (c == delimiter)
			end = TRUE;
		else
			vStringPut (string, c);
	}
	vStringTerminate (string);
}

/*	Read a C identifier beginning with "firstChar" and places it into "name".
*/
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar1 (c));
	do
	{
		vStringPut (string, c);
		c = fileGetc ();
	} while (isIdentChar (c));
	vStringTerminate (string);
	if (!isspace (c))
		fileUngetc (c);		/* unget non-identifier character */
}

static keywordId analyzeToken (vString *const name)
{
	vString *keyword = vStringNew ();
	keywordId result;
	vStringCopyToLower (keyword, name);
	result = (keywordId) lookupKeyword (vStringValue (keyword), Lang_sql);
	vStringDelete (keyword);
	return result;
}

static void readToken (tokenInfo *const token)
{
	int c;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:
	do
	{
		c = fileGetc ();
		/* 
		 * Added " to the list of ignores, not sure what this 
		 * might break but it gets by this issue:
		 *	  create table "t1" (...)
		 *
		 * Darren, the code passes all my tests for both 
		 * Oracle and SQL Anywhere, but maybe you can tell me
		 * what this may effect.
		 */
	}
	while (c == '\t'  ||  c == ' ' ||  c == '\n');

	switch (c)
	{
		case EOF: longjmp (Exception, (int)ExceptionEOF);	break;
		case '(': token->type = TOKEN_OPEN_PAREN;		break;
		case ')': token->type = TOKEN_CLOSE_PAREN;		break;
		case ';': token->type = TOKEN_SEMICOLON;		break;
		case '.': token->type = TOKEN_PERIOD;				break;
		case ',': token->type = TOKEN_COMMA;			break;
		case '{': token->type = TOKEN_OPEN_CURLY;		break;
		case '}': token->type = TOKEN_CLOSE_CURLY;		break;
		case '~': token->type = TOKEN_TILDE;			break;

		case '\'':
		case '"':
				  token->type = TOKEN_STRING;
				  parseString (token->string, c);
				  token->lineNumber = getSourceLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '-':
				  c = fileGetc ();
				  if (c == '-')		/* -- is this the start of a comment? */
				  {
					  skipToCharacter ('\n');
					  goto getNextChar;
				  }
				  else
				  {
					  if (!isspace (c))
						  fileUngetc (c);
					  token->type = TOKEN_OPERATOR;
				  }
				  break;

		case '<':
		case '>':
				  {
					  const int initial = c;
					  int d = fileGetc ();
					  if (d == initial)
					  {
						  if (initial == '<')
							  token->type = TOKEN_BLOCK_LABEL_BEGIN;
						  else
							  token->type = TOKEN_BLOCK_LABEL_END;
					  }
					  else
					  {
						  fileUngetc (d);
						  token->type = TOKEN_UNDEFINED;
					  }
					  break;
				  }

		case '/':
				  {
					  int d = fileGetc ();
					  if ( (d != '*') &&		/* is this the start of a comment? */
							  (d != '/') )			/* is a one line comment? */
					  {
						  token->type = TOKEN_FORWARD_SLASH;
						  fileUngetc (d);
					  }
					  else
					  {
						  if (d == '*')
						  {
							  do
							  {
								  skipToCharacter ('*');
								  c = fileGetc ();
								  if (c == '/')
									  break;
								  else
									  fileUngetc (c);
							  } while (c != EOF && c != '\0');
							  goto getNextChar;
						  }
						  else if (d == '/')	/* is this the start of a comment?  */
						  {
							  skipToCharacter ('\n');
							  goto getNextChar;
						  }
					  }
					  break;
				  }

		default:
				  if (! isIdentChar1 (c))
					  token->type = TOKEN_UNDEFINED;
				  else
				  {
					  parseIdentifier (token->string, c);
					  token->lineNumber = getSourceLineNumber ();
					  token->filePosition = getInputFilePosition ();
					  token->keyword = analyzeToken (token->string);
					  if (isKeyword (token, KEYWORD_rem))
					  {
						  vStringClear (token->string);
						  skipToCharacter ('\n');
						  goto getNextChar;
					  }
					  else if (isKeyword (token, KEYWORD_NONE))
						  token->type = TOKEN_IDENTIFIER;
					  else
						  token->type = TOKEN_KEYWORD;
				  }
				  break;
	}
}

/*
 *	 Token parsing functions
 */

/*
 * static void addContext (tokenInfo* const parent, const tokenInfo* const child)
 * {
 *	   if (vStringLength (parent->string) > 0)
 *	   {
 *		   vStringCatS (parent->string, ".");
 *	   }
 *	   vStringCatS (parent->string, vStringValue(child->string));
 *	   vStringTerminate(parent->string);
 * }
 */

static void addToScope (tokenInfo* const token, vString* const extra)
{
	if (vStringLength (token->scope) > 0)
	{
		vStringCatS (token->scope, ".");
	}
	vStringCatS (token->scope, vStringValue(extra));
	vStringTerminate(token->scope);
}

/*
 *	 Scanning functions
 */

static void findToken (tokenInfo *const token, const tokenType type)
{
	while (! isType (token, type))
	{
		readToken (token);
	}
}

static void findCmdTerm (tokenInfo *const token, const boolean check_first)
{
	if ( check_first ) 
	{
		if ( isCmdTerm(token) )
			return;
	}
	do
	{
		readToken (token);
	} while ( !isCmdTerm(token) );
}

static void skipArgumentList (tokenInfo *const token)
{
	int nest_level = 0;

	/*
	 * Other databases can have arguments with fully declared
	 * datatypes:
	 *	 (	name varchar(30), text binary(10)  )
	 * So we must check for nested open and closing parantheses
	 */

	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	{
		nest_level++;
		while (! (isType (token, TOKEN_CLOSE_PAREN) && (nest_level == 0)))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
			{
				nest_level++;
			}
			if (isType (token, TOKEN_CLOSE_PAREN))
			{
				if (nest_level > 0)
				{
					nest_level--;
				}
			}
		} 
		readToken (token);
	}
}

static void parseSubProgram (tokenInfo *const token)
{
	tokenInfo *const name  = newToken ();

	/*
	 * This must handle both prototypes and the body of
	 * the procedures.
	 *
	 * Prototype:
	 *	   FUNCTION func_name RETURN integer;
	 *	   PROCEDURE proc_name( parameters );
	 * Procedure
	 *	   FUNCTION GET_ML_USERNAME RETURN VARCHAR2
	 *	   IS
	 *	   BEGIN
	 *		   RETURN v_sync_user_id;
	 *	   END GET_ML_USERNAME;
	 *
	 *	   PROCEDURE proc_name( parameters )
	 *		   IS
	 *		   BEGIN
	 *		   END;
	 *	   CREATE PROCEDURE proc_name( parameters )
	 *		   EXTERNAL NAME ... ;
	 *	   CREATE PROCEDURE proc_name( parameters )
	 *		   BEGIN
	 *		   END;
	 *
	 *	   CREATE FUNCTION f_GetClassName(
	 *		   IN @object VARCHAR(128)
	 *		  ,IN @code   VARCHAR(128)
	 *	   )
	 *	   RETURNS VARCHAR(200)
	 *	   DETERMINISTIC
	 *	   BEGIN
	 *	   
	 *		   IF( @object = 'user_state' ) THEN
	 *			   SET something = something;
	 *		   END IF;
	 *	   
	 *		   RETURN @name;
	 *	   END;
	 */
	const sqlKind kind = isKeyword (token, KEYWORD_function) ?
		SQLTAG_FUNCTION : SQLTAG_PROCEDURE;
	Assert (isKeyword (token, KEYWORD_function) ||
			isKeyword (token, KEYWORD_procedure));
	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
		readToken (token);
	}
	skipArgumentList (token);

	if (kind == SQLTAG_FUNCTION)
	{
		if (isKeyword (token, KEYWORD_return))
		{
			/* Read datatype */
			readToken (token);
			/*
			 * Read token after which could be the
			 * command terminator if a prototype
			 */
			readToken (token);
		}
	}
	if( isCmdTerm (token) )
	{
		makeSqlTag (name, SQLTAG_PROTOTYPE);
	} 
	else 
	{
		while (!(isKeyword (token, KEYWORD_is) ||
					isKeyword (token, KEYWORD_begin) ||
					isCmdTerm (token)
				)
			  )
		{
			/* read return type */
			readToken (token);
		}
		if (isKeyword (token, KEYWORD_is) || 
				isKeyword (token, KEYWORD_begin) )
		{
			addToScope(token, name->string);
			if (isType (name, TOKEN_IDENTIFIER) ||
					isType (name, TOKEN_STRING) ||
					!isKeyword (token, KEYWORD_NONE)
			   )
				makeSqlTag (name, kind);

			parseBlock (token, TRUE);
			vStringClear (token->scope);
		} 
	}
	deleteToken (name);
}

static void parseRecord (tokenInfo *const token)
{
	/*
	 * Make it a bit forgiving, this is called from
	 * multiple functions, parseTable, parseType
	 */
	if (!isType (token, TOKEN_OPEN_PAREN))
		readToken (token);

	Assert (isType (token, TOKEN_OPEN_PAREN));
	do
	{
		if ( isType (token, TOKEN_COMMA) || isType (token, TOKEN_OPEN_PAREN) )
			readToken (token);

		/*
		 * Create table statements can end with various constraints
		 * which must be excluded from the SQLTAG_FIELD.
		 *	  create table t1 (
		 *		  c1 integer,
		 *		  c2 char(30),
		 *		  c3 numeric(10,5),
		 *		  c4 integer,
		 *		  constraint whatever,
		 *		  primary key(c1),
		 *		  foreign key (), 
		 *		  check ()
		 *	  )
		 */
		if (! (isKeyword(token, KEYWORD_primary) ||
					isKeyword(token, KEYWORD_references) ||
					isKeyword(token, KEYWORD_unique) ||
					isKeyword(token, KEYWORD_check) ||
					isKeyword(token, KEYWORD_constraint) ||
					isKeyword(token, KEYWORD_foreign) ) )
		{
			if (isType (token, TOKEN_IDENTIFIER) ||
					isType (token, TOKEN_STRING))
				makeSqlTag (token, SQLTAG_FIELD);
		}

		while (!(isType (token, TOKEN_COMMA) ||
					isType (token, TOKEN_CLOSE_PAREN) ||
					isType (token, TOKEN_OPEN_PAREN) 
				))
		{
			readToken (token);
			/* 
			 * A table structure can look like this:
			 *	  create table t1 (
			 *		  c1 integer,
			 *		  c2 char(30),
			 *		  c3 numeric(10,5),
			 *		  c4 integer
			 *	  )
			 * We can't just look for a COMMA or CLOSE_PAREN
			 * since that will not deal with the numeric(10,5)
			 * case.  So we need to skip the argument list 
			 * when we find an open paren.
			 */
			if (isType (token, TOKEN_OPEN_PAREN))
			{
				/* Reads to the next token after the TOKEN_CLOSE_PAREN */
				skipArgumentList(token);
			}
		}
	} while (! isType (token, TOKEN_CLOSE_PAREN));
}

static void parseType (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	vString * saveScope = vStringNew ();

	vStringCopy(saveScope, token->scope);
	/* If a scope has been set, add it to the name */
	addToScope (name, token->scope);
	readToken (name);
	if (isType (name, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_is))
		{
			readToken (token);
			addToScope (token, name->string);
			switch (token->keyword)
			{
				case KEYWORD_record:
				case KEYWORD_object:
					makeSqlTag (name, SQLTAG_RECORD);
					parseRecord (token);
					break;

				case KEYWORD_table:
					makeSqlTag (name, SQLTAG_TABLE);
					break;

				case KEYWORD_ref:
					readToken (token);
					if (isKeyword (token, KEYWORD_cursor))
						makeSqlTag (name, SQLTAG_CURSOR);
					break;

				default: break;
			}
			vStringClear (token->scope);
		}
	}
	vStringCopy(token->scope, saveScope);
	deleteToken (name);
	vStringDelete(saveScope);
}

static void parseSimple (tokenInfo *const token, const sqlKind kind)
{
	/* This will simply make the tagname from the first word found */
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) ||
			isType (token, TOKEN_STRING))
		makeSqlTag (token, kind);
}

static void parseDeclare (tokenInfo *const token, const boolean local)
{
	/*
	 * PL/SQL declares are of this format:
	 *	  IS|AS
	 *	  [declare]
	 *		 CURSOR curname ...
	 *		 varname1 datatype;
	 *		 varname2 datatype;
	 *		 varname3 datatype;
	 *	  begin
	 */

	if (isKeyword (token, KEYWORD_declare))
		readToken (token);
	while (! isKeyword (token, KEYWORD_begin) && ! isKeyword (token, KEYWORD_end))
	{
		switch (token->keyword)
		{
			case KEYWORD_cursor:	parseSimple (token, SQLTAG_CURSOR); break;
			case KEYWORD_function:	parseSubProgram (token); break;
			case KEYWORD_procedure: parseSubProgram (token); break;
			case KEYWORD_subtype:	parseSimple (token, SQLTAG_SUBTYPE); break;
			case KEYWORD_trigger:	parseSimple (token, SQLTAG_TRIGGER); break;
			case KEYWORD_type:		parseType (token); break;

			default:
									if (isType (token, TOKEN_IDENTIFIER))
									{
										if (local)
										{
											makeSqlTag (token, SQLTAG_LOCAL_VARIABLE);
										} 
										else 
										{
											makeSqlTag (token, SQLTAG_VARIABLE);
										}
									}
									break;
		}
		findToken (token, TOKEN_SEMICOLON);
		readToken (token);
	}
}

static void parseDeclareANSI (tokenInfo *const token, const boolean local)
{
	tokenInfo *const type = newToken ();
	/*
	 * ANSI declares are of this format:
	 *	 BEGIN
	 *		 DECLARE varname1 datatype;
	 *		 DECLARE varname2 datatype;
	 *		 ...
	 *
	 * This differ from PL/SQL where DECLARE preceeds the BEGIN block
	 * and the DECLARE keyword is not repeated.
	 */
	while (isKeyword (token, KEYWORD_declare))
	{
		readToken (token);
		readToken (type);

		if (isKeyword (type, KEYWORD_cursor))
			makeSqlTag (token, SQLTAG_CURSOR);
		else if (isKeyword (token, KEYWORD_local) &&
				isKeyword (type, KEYWORD_temporary))
		{
			/*
			 * DECLARE LOCAL TEMPORARY TABLE table_name (
			 *	  c1 int,
			 *	  c2 int
			 * );
			 */
			readToken (token);
			if (isKeyword (token, KEYWORD_table))
			{
				readToken (token);
				if (isType(token, TOKEN_IDENTIFIER) || 
						isType(token, TOKEN_STRING) )
				{
					makeSqlTag (token, SQLTAG_TABLE);
				}
			}
		}
		else if (isType (token, TOKEN_IDENTIFIER) || 
				isType (token, TOKEN_STRING))
		{
			if (local)
				makeSqlTag (token, SQLTAG_LOCAL_VARIABLE);
			else
				makeSqlTag (token, SQLTAG_VARIABLE);
		}
		findToken (token, TOKEN_SEMICOLON);
		readToken (token);
	}
	deleteToken (type);
}

static void parseLabel (tokenInfo *const token)
{
	/*
	 * A label has this format:
	 *	   <<tobacco_dependency>>
	 *	   DECLARE
	 *		  v_senator VARCHAR2(100) := 'THURMOND, JESSE';
	 *	   BEGIN
	 *		  IF total_contributions (v_senator, 'TOBACCO') > 25000
	 *		  THEN
	 *			 <<alochol_dependency>>
	 *			 DECLARE
	 *				v_senator VARCHAR2(100) := 'WHATEVERIT, TAKES';
	 *			 BEGIN
	 *				...
	 */

	Assert (isType (token, TOKEN_BLOCK_LABEL_BEGIN));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		makeSqlTag (token, SQLTAG_BLOCK_LABEL);
		readToken (token);		  /* read end of label */
	}
}

static void parseStatements (tokenInfo *const token)
{
	do
	{
		if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
			parseLabel (token);
		else
		{
			switch (token->keyword)
			{
				case KEYWORD_exception:
					/*
					 * EXCEPTION
					 *	 <exception handler>;
					 *
					 * Where an exception handler could be:
					 *	 BEGIN
					 *		WHEN OTHERS THEN
					 *			x := x + 3;
					 *	 END;
					 * In this case we need to skip this keyword and 
					 * move on to the next token without reading until
					 * TOKEN_SEMICOLON;
					 */
					readToken (token);
					continue;

				case KEYWORD_when:
					/*
					 * WHEN statements can be used in exception clauses
					 * and CASE statements.  The CASE statement should skip
					 * these given below we skip over to an END statement.
					 * But for an exception clause, we can have:
					 *	   EXCEPTION
					 *		   WHEN OTHERS THEN
					 *		   BEGIN
					 *				  x := x + 3;
					 *		   END;
					 * If we skip to the TOKEN_SEMICOLON, we miss the begin
					 * of a nested BEGIN END block.  So read the next token
					 * after the THEN and restart the LOOP.
					 */
					while (! isKeyword (token, KEYWORD_then))
						readToken (token);
					readToken (token);
					continue;

				case KEYWORD_if:
					/*
					 * We do not want to look for a ; since for an empty
					 * IF block, it would skip over the END.
					 *	IF...THEN
					 *	END IF;
					 */
					while (! isKeyword (token, KEYWORD_then))
						readToken (token);

					parseStatements (token);
					/* 
					 * parseStatements returns when it finds an END, an IF
					 * statement should be followed by an IF (ANSI anyway)
					 * so read the following IF as well
					 */
					readToken (token);
					break;

				case KEYWORD_loop:
				case KEYWORD_case:
				case KEYWORD_for:
					/*
					 *	LOOP...
					 *	END LOOP;
					 *	
					 *	FOR loop_name AS cursor_name CURSOR FOR ...
					 *	END FOR;
					 */
					readToken (token);
					parseStatements (token);
					break;

				case KEYWORD_declare:
				case KEYWORD_begin:
					parseBlock (token, TRUE);
					break;

				default:
					readToken (token);
					break;
			}
			/*
			 * Not all statements must end in a semi-colon 
			 *	   begin
			 *		   if current publisher <> 'publish' then
			 *			 signal UE_FailStatement
			 *		   end if
			 *	   end;
			 * The last statement prior to an end ("signal" above) does
			 * not need a semi-colon, nor does the end if, since it is 
			 * also the last statement prior to the end of the block.
			 *
			 * So we must read to the first semi-colon or an END block
			 */
			while (! (isKeyword (token, KEYWORD_end) ||
						(isType (token, TOKEN_SEMICOLON)))	  )
			{
				readToken (token);
			}
		}
		/*
		 * We assumed earlier all statements ended with a semi-colon,
		 * see comment above, now, only read if the current token 
		 * is not a semi-colon
		 */
		if ( isType (token, TOKEN_SEMICOLON) )
		{
			readToken (token);
		}
	} while (! isKeyword (token, KEYWORD_end));
}

static void parseBlock (tokenInfo *const token, const boolean local)
{
	if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
	{
		parseLabel (token);
		readToken (token);
	}
	if (! isKeyword (token, KEYWORD_begin))
	{
		readToken (token);
		/*
		 * These are Oracle style declares which generally come
		 * between an IS/AS and BEGIN block.
		 */
		parseDeclare (token, local);
	}
	if (isKeyword (token, KEYWORD_begin))
	{
		readToken (token);
		/*
		 * Check for ANSI declarations which always follow
		 * a BEGIN statement.  This routine will not advance
		 * the token if none are found.
		 */
		parseDeclareANSI (token, local);
		while (! isKeyword (token, KEYWORD_end))
		{
			parseStatements (token);
		}
		findCmdTerm (token, FALSE);
	}
}

static void parsePackage (tokenInfo *const token)
{
	/* 
	 * Packages can be specified in a number of ways:
	 *		CREATE OR REPLACE PACKAGE pkg_name AS
	 * or
	 *		CREATE OR REPLACE PACKAGE owner.pkg_name AS
	 * or by specifying a package body
	 *	   CREATE OR REPLACE PACKAGE BODY pkg_name AS
	 *	   CREATE OR REPLACE PACKAGE BODY owner.pkg_name AS
	 */
	tokenInfo *const name = newToken ();
	readToken (name);
	if (isKeyword (name, KEYWORD_body))
	{
		/*
		 * Ignore the BODY tag since we will process
		 * the body or prototypes in the same manner
		 */
		readToken (name);
	}
	/* Check for owner.pkg_name */
	while (! isKeyword (token, KEYWORD_is))
	{
		readToken (token);
		if ( isType(token, TOKEN_PERIOD) )
		{
			readToken (name);
		}
	}
	if (isKeyword (token, KEYWORD_is))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
				isType (name, TOKEN_STRING))
			makeSqlTag (name, SQLTAG_PACKAGE);
		parseBlock (token, FALSE);
	}
	findCmdTerm (token, FALSE);
	deleteToken (name);
}

static void parseTable (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats:
	 *	   create table t1 (c1 int);
	 *	   create global tempoary table t2 (c1 int);
	 *	   create table "t3" (c1 int);
	 *	   create table bob.t4 (c1 int);
	 *	   create table bob."t5" (c1 int);
	 *	   create table "bob"."t6" (c1 int);
	 *	   create table bob."t7" (c1 int);
	 * Proxy tables use this format:
	 *	   create existing table bob."t7" AT '...';
	 */

	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
		readToken (token);
	}
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
				isType (name, TOKEN_STRING))
		{
			makeSqlTag (name, SQLTAG_TABLE);
			vStringCopy(token->scope, name->string);
			parseRecord (token);
			vStringClear (token->scope);
		}
	} 
	else if (isKeyword (token, KEYWORD_at))
	{
		if (isType (name, TOKEN_IDENTIFIER))
		{
			makeSqlTag (name, SQLTAG_TABLE);
		}
	}
	findCmdTerm (token, FALSE);
	deleteToken (name);
}

static void parseIndex (tokenInfo *const token)
{
	tokenInfo *const name  = newToken ();
	tokenInfo *const owner = newToken ();

	/*
	 * This deals with these formats
	 *	   create index i1 on t1(c1) create index "i2" on t1(c1) 
	 *	   create virtual unique clustered index "i3" on t1(c1) 
	 *	   create unique clustered index "i4" on t1(c1) 
	 *	   create clustered index "i5" on t1(c1) 
	 *	   create bitmap index "i6" on t1(c1)
	 */

	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
		readToken (token);
	}
	if ( isKeyword (token, KEYWORD_on) &&
			(isType (name, TOKEN_IDENTIFIER) || isType (name, TOKEN_STRING) ) )
	{
		readToken (owner);
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
		{
			readToken (owner);
			readToken (token);
		}
		addToScope(name, owner->string);
		makeSqlTag (name, SQLTAG_INDEX);
	}
	findCmdTerm (token, FALSE);
	deleteToken (name);
	deleteToken (owner);
}

static void parseEvent (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create event e1 handler begin end;
	 *	   create event "e2" handler begin end;
	 *	   create event dba."e3" handler begin end;
	 *	   create event "dba"."e4" handler begin end;
	 */

	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
	}
	while (! (isKeyword (token, KEYWORD_handler) ||
				(isType (token, TOKEN_SEMICOLON)))	  )
	{
		readToken (token);
	}

	if ( isKeyword (token, KEYWORD_handler) ||
			isType (token, TOKEN_SEMICOLON)   )
	{
		makeSqlTag (name, SQLTAG_EVENT);
	}

	if (isKeyword (token, KEYWORD_handler))
	{
		readToken (token);
		if ( isKeyword (token, KEYWORD_begin) )
		{
			parseBlock (token, TRUE);
		}
		findCmdTerm (token, FALSE);
	}
	deleteToken (name);
}

static void parseTrigger (tokenInfo *const token)
{
	tokenInfo *const name  = newToken ();
	tokenInfo *const table = newToken ();

	/*
	 * This deals with these formats
	 *	   create or replace trigger tr1 begin end;
	 *	   create trigger "tr2" begin end;
	 *	   drop trigger "droptr1";
	 *	   create trigger "tr3" CALL sp_something();
	 *	   create trigger "owner"."tr4" begin end;
	 *	   create trigger "tr5" not valid;
	 *	   create trigger "tr6" begin end;
	 */

	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
		readToken (token);
	}

	while ( !isKeyword (token, KEYWORD_on) &&
			!isCmdTerm (token)		)
	{
		readToken (token);
	}

	/*if (! isType (token, TOKEN_SEMICOLON) ) */
	if (! isCmdTerm (token) )
	{
		readToken (table);
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
		{
			readToken (table);
			readToken (token);
		}

		while (! (isKeyword (token, KEYWORD_begin) ||
					(isKeyword (token, KEYWORD_call)) ||
					(	isCmdTerm (token)))    )
		{
			readToken (token);
			if ( isKeyword (token, KEYWORD_declare) )
			{
				addToScope(token, name->string);
				parseDeclare(token, TRUE);
				vStringClear(token->scope);
			}
		}

		if ( isKeyword (token, KEYWORD_begin) || 
				isKeyword (token, KEYWORD_call)   )
		{
			addToScope(name, table->string);
			makeSqlTag (name, SQLTAG_TRIGGER);
			addToScope(token, table->string);
			if ( isKeyword (token, KEYWORD_begin) )
			{
				parseBlock (token, TRUE);
			}
			vStringClear(token->scope);
		}
	}

	findCmdTerm (token, TRUE);
	deleteToken (name);
	deleteToken (table);
}

static void parsePublication (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create or replace publication pu1 ()
	 *	   create publication "pu2" ()
	 *	   create publication dba."pu3" ()
	 *	   create publication "dba"."pu4" ()
	 */

	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
		readToken (token);
	}
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
				isType (name, TOKEN_STRING))
		{
			makeSqlTag (name, SQLTAG_PUBLICATION);
		}
	}
	findCmdTerm (token, FALSE);
	deleteToken (name);
}

static void parseService (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   CREATE SERVICE s1 TYPE 'HTML' 
	 *		   AUTHORIZATION OFF USER DBA AS 
	 *		   SELECT * 
	 *			 FROM SYS.SYSTABLE;
	 *	   CREATE SERVICE "s2" TYPE 'HTML'
	 *		   AUTHORIZATION OFF USER DBA AS 
	 *		   CALL sp_Something();
	 */

	readToken (name);
	readToken (token);
	if (isKeyword (token, KEYWORD_type))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
				isType (name, TOKEN_STRING))
		{
			makeSqlTag (name, SQLTAG_SERVICE);
		}
	}
	findCmdTerm (token, FALSE);
	deleteToken (name);
}

static void parseDomain (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   CREATE DOMAIN|DATATYPE [AS] your_name ...;
	 */

	readToken (name);
	if (isKeyword (name, KEYWORD_is))
	{
		readToken (name);
	}
	readToken (token);
	if (isType (name, TOKEN_IDENTIFIER) ||
			isType (name, TOKEN_STRING))
	{
		makeSqlTag (name, SQLTAG_DOMAIN);
	}
	findCmdTerm (token, FALSE);
	deleteToken (name);
}

static void parseDrop (tokenInfo *const token)
{
	/*
	 * This deals with these formats
	 *	   DROP TABLE|PROCEDURE|DOMAIN|DATATYPE name;
	 *
	 * Just simply skip over these statements.
	 * They are often confused with PROCEDURE prototypes
	 * since the syntax is similar, this effectively deals with
	 * the issue for all types.
	 */

	findCmdTerm (token, FALSE);
}

static void parseVariable (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create variable varname1 integer;
	 *	   create variable @varname2 integer;
	 *	   create variable "varname3" integer;
	 *	   drop   variable @varname3;
	 */

	readToken (name);
	readToken (token);
	if ( (isType (name, TOKEN_IDENTIFIER) || isType (name, TOKEN_STRING))
			&& !isType (token, TOKEN_SEMICOLON) )
	{
		makeSqlTag (name, SQLTAG_VARIABLE);
	}
	findCmdTerm (token, TRUE);

	deleteToken (name);
}

static void parseSynonym (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create variable varname1 integer;
	 *	   create variable @varname2 integer;
	 *	   create variable "varname3" integer;
	 *	   drop   variable @varname3;
	 */

	readToken (name);
	readToken (token);
	if ( (isType (name, TOKEN_IDENTIFIER) || isType (name, TOKEN_STRING))
			&& isKeyword (token, KEYWORD_for) )
	{
		makeSqlTag (name, SQLTAG_SYNONYM);
	}
	findCmdTerm (token, TRUE);

	deleteToken (name);
}

static void parseView (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create variable varname1 integer;
	 *	   create variable @varname2 integer;
	 *	   create variable "varname3" integer;
	 *	   drop   variable @varname3;
	 */

	readToken (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readToken (name);
		readToken (token);
	}
	if ( isType (token, TOKEN_OPEN_PAREN) )
	{
		skipArgumentList(token);

	}

	while (!(isKeyword (token, KEYWORD_is) ||
				isType (token, TOKEN_SEMICOLON)
			))
	{
		readToken (token);
	}

	if ( (isType (name, TOKEN_IDENTIFIER) || isType (name, TOKEN_STRING))
			&& isKeyword (token, KEYWORD_is) )
	{
		makeSqlTag (name, SQLTAG_VIEW);
	}

	findCmdTerm (token, TRUE);

	deleteToken (name);
}

static void parseMLTable (tokenInfo *const token)
{
	tokenInfo *const version = newToken ();
	tokenInfo *const table	 = newToken ();
	tokenInfo *const event	 = newToken ();

	/*
	 * This deals with these formats
	 *	  call dbo.ml_add_table_script( 'version', 'table_name', 'event',
	 *		   'some SQL statement'
	 *		   );
	 */

	readToken (token);
	if ( isType (token, TOKEN_OPEN_PAREN) )
	{
		readToken (version);
		readToken (token);
		while (!(isType (token, TOKEN_COMMA) ||
					isType (token, TOKEN_CLOSE_PAREN)
				))
		{
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
		{
			readToken (table);
			readToken (token);
			while (!(isType (token, TOKEN_COMMA) ||
						isType (token, TOKEN_CLOSE_PAREN)
					))
			{
				readToken (token);
			}

			if (isType (token, TOKEN_COMMA))
			{
				readToken (event);

				if (isType (version, TOKEN_STRING) && 
						isType (table, TOKEN_STRING) && 
						isType (event, TOKEN_STRING) )
				{
					addToScope(version, table->string);
					addToScope(version, event->string);
					makeSqlTag (version, SQLTAG_MLTABLE);
				}
			} 
			if( !isType (token, TOKEN_CLOSE_PAREN) )
				findToken (token, TOKEN_CLOSE_PAREN);
		} 
	}

	findCmdTerm (token, TRUE);

	deleteToken (version);
	deleteToken (table);
	deleteToken (event);
}

static void parseMLConn (tokenInfo *const token)
{
	tokenInfo *const version = newToken ();
	tokenInfo *const event	 = newToken ();

	/*
	 * This deals with these formats
	 *	  call ml_add_connection_script( 'version', 'event',
	 *		   'some SQL statement'
	 *		   );
	 */

	readToken (token);
	if ( isType (token, TOKEN_OPEN_PAREN) )
	{
		readToken (version);
		readToken (token);
		while (!(isType (token, TOKEN_COMMA) ||
					isType (token, TOKEN_CLOSE_PAREN)
				))
		{
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
		{
			readToken (event);

			if (isType (version, TOKEN_STRING) && 
					isType (event, TOKEN_STRING) )
			{
				addToScope(version, event->string);
				makeSqlTag (version, SQLTAG_MLCONN);
			}
		} 
		if( !isType (token, TOKEN_CLOSE_PAREN) )
			findToken (token, TOKEN_CLOSE_PAREN);

	}

	findCmdTerm (token, TRUE);

	deleteToken (version);
	deleteToken (event);
}

static void parseComment (tokenInfo *const token)
{
	/*
	 * This deals with this statement:
	 *	   COMMENT TO PRESERVE FORMAT ON PROCEDURE "DBA"."test" IS 
	 *	   {create PROCEDURE DBA."test"()
	 *	   BEGIN
	 *		signal dave;
	 *	   END
	 *	   }
	 *	   ;
	 * The comment can contain anything between the CURLY
	 * braces
	 *	   COMMENT ON USER "admin" IS
	 *			'Administration Group'
	 *			;
	 * Or it could be a simple string with no curly braces
	 */
	while (! isKeyword (token, KEYWORD_is))
	{
		readToken (token);
	}
	readToken (token);
	if ( isType(token, TOKEN_OPEN_CURLY) )
	{
		findToken (token, TOKEN_CLOSE_CURLY);
	}

	findCmdTerm (token, TRUE);
}


static void parseSqlFile (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
			parseLabel (token);
		else switch (token->keyword)
		{
			case KEYWORD_begin:			parseBlock (token, FALSE); break;
			case KEYWORD_comment:		parseComment (token); break;
			case KEYWORD_cursor:		parseSimple (token, SQLTAG_CURSOR); break;
			case KEYWORD_datatype:		parseDomain (token); break;
			case KEYWORD_declare:		parseBlock (token, FALSE); break;
			case KEYWORD_domain:		parseDomain (token); break;
			case KEYWORD_drop:			parseDrop (token); break;
			case KEYWORD_event:			parseEvent (token); break;
			case KEYWORD_function:		parseSubProgram (token); break;
			case KEYWORD_index:			parseIndex (token); break;
			case KEYWORD_ml_table:		parseMLTable (token); break;
			case KEYWORD_ml_table_lang: parseMLTable (token); break;
			case KEYWORD_ml_conn:		parseMLConn (token); break;
			case KEYWORD_ml_conn_lang:	parseMLConn (token); break;
			case KEYWORD_package:		parsePackage (token); break;
			case KEYWORD_procedure:		parseSubProgram (token); break;
			case KEYWORD_publication:	parsePublication (token); break;
			case KEYWORD_service:		parseService (token); break;
			case KEYWORD_subtype:		parseSimple (token, SQLTAG_SUBTYPE); break;
			case KEYWORD_synonym:		parseSynonym (token); break;
			case KEYWORD_table:			parseTable (token); break;
			case KEYWORD_trigger:		parseTrigger (token); break;
			case KEYWORD_type:			parseType (token); break;
			case KEYWORD_variable:		parseVariable (token); break;
			case KEYWORD_view:			parseView (token); break;
			default:				break;
		}
	} while (! isKeyword (token, KEYWORD_end));
}

static void initialize (const langType language)
{
	Assert (sizeof (SqlKinds) / sizeof (SqlKinds [0]) == SQLTAG_COUNT);
	Lang_sql = language;
	buildSqlKeywordHash ();
}

static void findSqlTags (void)
{
	tokenInfo *const token = newToken ();
	exception_t exception = (exception_t) (setjmp (Exception));

	while (exception == ExceptionNone)
		parseSqlFile (token);

	deleteToken (token);
}

extern parserDefinition* SqlParser (void)
{
	static const char *const extensions [] = { "sql", NULL };
	parserDefinition* def = parserNew ("SQL");
	def->kinds		= SqlKinds;
	def->kindCount	= KIND_COUNT (SqlKinds);
	def->extensions = extensions;
	def->parser		= findSqlTags;
	def->initialize = initialize;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
